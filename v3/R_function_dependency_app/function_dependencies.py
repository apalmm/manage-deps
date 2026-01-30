# function_dependencies.py
#
# Function-level dependency inference for R packages.
#
# What I’m trying to compute:
#   - Given a function name in a package, I want the set of packages it *actually touches*
#     by following call edges in source code, not DESCRIPTION metadata.
#
# How I do it (static-ish):
#   1) Pull the package source from GitHub (best-effort search + caching).
#   2) Extract function bodies (regex + brace matching) + the roxygen block directly above each
#      function definition (so @seealso links count as dependency signals).
#   3) For a target function, collect dependency signals from:
#        - explicit namespace calls: pkg::fun / pkg:::fun
#        - namespace loaders: library/require/requireNamespace/loadNamespace
#        - getExportedValue("pkg", "fun")
#        - NAMESPACE imports:
#            * importFrom(pkg, sym) gives exact symbol -> package mapping
#            * import(pkg) is coarse, so I try to resolve bare calls by checking exported symbols
#              of imported packages (best-effort)
#        - roxygen references:
#            * @seealso [pkg::fun()] or anything similar is treated as a dependency signal
#              because it’s usually the “real implementation this wraps”
#   4) Recurse on:
#        - internal/local calls inside the same package
#        - cross-package calls (pkg::fun) and imported-symbol calls (depth-limited)
#
# Limits:
#   - This won’t be “perfect runtime invokes” because R is dynamic (S3 dispatch, NSE,
#     get/do.call/eval/parse, etc.). I can optionally flag “dynamic-ish” patterns later.
#   - Regex parsing of R is inherently brittle. Brace matching helps, but an AST pass
#     (in R or tree-sitter) is the endgame if I need maximal correctness.
#
# Design choice I’m making here:
#   - When resolution is ambiguous (multiple imported packages export the same symbol),
#     I include all candidates rather than guessing. That biases toward slight overcounting
#     instead of silently missing real deps.

import os
import re
import json
import time
import hashlib
import requests
from pathlib import Path

CACHE_DIR = Path("cache/github_repos")
CACHE_DIR.mkdir(parents=True, exist_ok=True)

BASE_PACKAGES = {
    "base",
    "compiler",
    "datasets",
    "graphics",
    "grDevices",
    "grid",
    "methods",
    "parallel",
    "splines",
    "stats",
    "stats4",
    "tcltk",
    "tools",
    "utils",
}

GITHUB_API_SEARCH = (
    "https://api.github.com/search/repositories?q={pkg}+language:R&sort=stars"
)
GITHUB_API_REPO = "https://api.github.com/repos/{full_name}"
GITHUB_API_TREE = "https://api.github.com/repos/{full_name}/git/trees/{sha}?recursive=1"

USER_AGENT = os.getenv("DEEPDEPBOT_UA", "DeepDepBot")
GITHUB_TOKEN = os.getenv("GITHUB_TOKEN")  # set this or GitHub will throttle you fast

DEFAULT_TIMEOUT = 20
MAX_RETRIES = 4

_session = requests.Session()
_headers = {
    "Accept": "application/vnd.github+json",
    "User-Agent": USER_AGENT,
}
if GITHUB_TOKEN:
    _headers["Authorization"] = f"Bearer {GITHUB_TOKEN}"


def _cache_path(url):
    h = hashlib.sha1(url.encode("utf-8")).hexdigest()
    return CACHE_DIR / f"{h}.json"


def cached_get(url):
    """
    GET with disk cache + retries.

    The point is to make recursion cheap:
      - if it’s cached, use cache
      - if GitHub rate limits (403/429), back off and retry
      - cache raw text even for JSON so we can replay without network
    """
    cache_file = _cache_path(url)
    if cache_file.exists():
        try:
            return json.loads(cache_file.read_text(encoding="utf-8"))
        except Exception:
            try:
                return cache_file.read_text(encoding="utf-8")
            except Exception:
                return None

    for attempt in range(MAX_RETRIES):
        try:
            r = _session.get(url, headers=_headers, timeout=DEFAULT_TIMEOUT)

            # rate limits / abuse protection show up as 403/429
            if r.status_code in (403, 429):
                time.sleep(min(2**attempt, 30))
                continue

            if not r.ok:
                time.sleep(min(2**attempt, 10))
                continue

            text = r.text
            try:
                cache_file.write_text(text, encoding="utf-8")
            except Exception:
                pass

            if "application/json" in r.headers.get("Content-Type", ""):
                return r.json()
            return text
        except Exception:
            time.sleep(min(2**attempt, 10))

    return None


# ---- in-memory caches (keeps recursion from becoming a network benchmark) ----
_REPO_CACHE = {}  # pkg -> "user/repo"
_BRANCH_CACHE = {}  # "user/repo" -> default branch
_TREE_CACHE = {}  # "user/repo" -> [raw file urls]
_SOURCE_CACHE = {}  # "user/repo" -> combined source string

_DEFS_CACHE = {}  # pkg -> {func_name -> body}
_DOCS_CACHE = {}  # pkg -> {func_name -> roxygen text}

_NAMESPACE_CACHE = {}  # "user/repo" -> NAMESPACE text (only stored if non-empty)
_IMPORT_MAP_CACHE = {}  # pkg -> {symbol: package} from importFrom
_IMPORT_PKG_CACHE = {}  # pkg -> set(packages) from import(pkg)

_EXPORT_SYMS_CACHE = {}  # pkg -> set(exported symbols)


def find_github_repo(pkg):
    """
    Best-effort repo discovery via GitHub search.
    This can pick the wrong repo; the thesis-safe path is CRAN metadata -> repo URL.
    """
    key = pkg.lower().strip()
    if key in _REPO_CACHE:
        return _REPO_CACHE[key]

    data = cached_get(GITHUB_API_SEARCH.format(pkg=pkg))
    if not data or "items" not in data or not data["items"]:
        _REPO_CACHE[key] = None
        return None

    # prefer exact repo name == package name if it exists
    for repo in data["items"]:
        if repo.get("name", "").lower() == key:
            full = repo.get("full_name")
            _REPO_CACHE[key] = full
            return full

    # otherwise just take the top result
    full = data["items"][0].get("full_name")
    _REPO_CACHE[key] = full
    return full


def _repo_default_branch_tree_sha(full_name):
    """
    Get git tree SHA for default branch and remember the branch name.
    """
    data = cached_get(GITHUB_API_REPO.format(full_name=full_name))
    if not isinstance(data, dict):
        return None

    default_branch = data.get("default_branch")
    if not default_branch:
        return None

    _BRANCH_CACHE[full_name] = default_branch

    branch_url = f"https://api.github.com/repos/{full_name}/branches/{default_branch}"
    b = cached_get(branch_url)
    if not isinstance(b, dict):
        return None

    try:
        return b["commit"]["commit"]["tree"]["sha"]
    except Exception:
        return None


def _raw_url(full_name, path):
    """
    Prefer explicit default branch for raw file fetches; fall back to HEAD.
    """
    user, repo = full_name.split("/", 1)
    branch = _BRANCH_CACHE.get(full_name)
    if branch:
        return f"https://raw.githubusercontent.com/{user}/{repo}/{branch}/{path.lstrip('/')}"
    return f"https://raw.githubusercontent.com/{user}/{repo}/HEAD/{path.lstrip('/')}"


def get_repo_r_files(full_name):
    """
    List all .R files using the recursive git tree API.
    """
    if full_name in _TREE_CACHE and _TREE_CACHE[full_name] is not None:
        return _TREE_CACHE[full_name]

    tree_sha = _repo_default_branch_tree_sha(full_name)
    if not tree_sha:
        _TREE_CACHE[full_name] = []
        return []

    tree = cached_get(GITHUB_API_TREE.format(full_name=full_name, sha=tree_sha))
    if not isinstance(tree, dict) or "tree" not in tree:
        _TREE_CACHE[full_name] = []
        return []

    urls = []
    for item in tree.get("tree", []):
        if item.get("type") != "blob":
            continue
        path = item.get("path", "")
        if path.endswith(".R"):
            urls.append(_raw_url(full_name, path))

    _TREE_CACHE[full_name] = urls
    return urls


def load_all_source(full_name):
    """
    Combine all .R files into one big string (cached).
    """
    if full_name in _SOURCE_CACHE:
        return _SOURCE_CACHE[full_name]

    repo_file = CACHE_DIR / f"{full_name.replace('/', '_')}_source.R"
    if repo_file.exists():
        try:
            src = repo_file.read_text(encoding="utf-8", errors="ignore")
            _SOURCE_CACHE[full_name] = src
            return src
        except Exception:
            pass

    urls = get_repo_r_files(full_name)
    if not urls:
        _SOURCE_CACHE[full_name] = None
        return None

    chunks = []
    for u in urls:
        txt = cached_get(u)
        if not txt or isinstance(txt, dict):
            continue
        chunks.append("\n\n# --- FILE: " + u + " ---\n")
        chunks.append(str(txt))

    src = "".join(chunks) if chunks else None
    if src:
        try:
            repo_file.write_text(src, encoding="utf-8")
        except Exception:
            pass

    _SOURCE_CACHE[full_name] = src
    return src


# ----------------------------
# NAMESPACE handling
# ----------------------------


def load_namespace_file(full_name):
    """
    Fetch raw NAMESPACE.

    Important detail:
      - I don’t permanently cache empty string on failure
        because that makes import/export resolution silently broken forever.
    """
    if full_name in _NAMESPACE_CACHE and _NAMESPACE_CACHE[full_name]:
        return _NAMESPACE_CACHE[full_name]

    if full_name not in _BRANCH_CACHE:
        _repo_default_branch_tree_sha(full_name)

    txt = cached_get(_raw_url(full_name, "NAMESPACE"))
    if not txt or isinstance(txt, dict):
        return ""

    txt = str(txt)
    if txt.strip():
        _NAMESPACE_CACHE[full_name] = txt
    return txt


def _scan_calls(ns_text, call_name):
    """
    Small call-scanner for NAMESPACE.
    Finds occurrences like importFrom(...) and returns the arg string inside the parens.

    This avoids a bunch of regex footguns with multiline formatting.
    """
    out = []
    s = re.sub(r"#.*", "", ns_text)  # strip comments
    i = 0
    needle = call_name + "("

    while True:
        j = s.find(needle, i)
        if j < 0:
            break
        k = j + len(needle)
        depth = 1
        buf = []
        while k < len(s) and depth > 0:
            ch = s[k]
            if ch == "(":
                depth += 1
            elif ch == ")":
                depth -= 1
                if depth == 0:
                    break
            buf.append(ch)
            k += 1
        out.append("".join(buf).strip())
        i = k + 1

    return out


def parse_namespace_imports(namespace_text):
    """
    Return:
      - import_map: symbol -> package  (from importFrom)
      - import_pkgs: set(packages)      (from import)
    """
    import_map = {}
    import_pkgs = set()

    # importFrom(pkg, sym1, sym2, ...)
    for arg_blob in _scan_calls(namespace_text, "importFrom"):
        parts = arg_blob.split(",", 1)
        if not parts:
            continue

        pkg = parts[0].strip().strip("'\"")
        if not pkg:
            continue

        if len(parts) == 1:
            continue

        syms_blob = parts[1]
        syms = [p.strip().strip("'\"") for p in syms_blob.split(",")]
        syms = [s for s in syms if re.match(r"^[A-Za-z][A-Za-z0-9_.]*$", s)]

        for sym in syms:
            import_map[sym] = pkg

    # import(pkg1, pkg2, ...)
    for arg_blob in _scan_calls(namespace_text, "import"):
        pkgs = [p.strip().strip("'\"") for p in arg_blob.split(",")]
        for p in pkgs:
            if re.match(r"^[A-Za-z][A-Za-z0-9_.]*$", p):
                import_pkgs.add(p)

    return import_map, import_pkgs


def parse_namespace_exports(namespace_text):
    """
    Return:
      - export_syms: set(symbols) from export(sym1, sym2, ...)
      - export_patterns: list(regex strings) from exportPattern("regex")

    I only need this for one thing:
      - resolving import(pkg) cases by asking “does pkg export symbol X”
    """
    export_syms = set()
    export_patterns = []

    for arg_blob in _scan_calls(namespace_text, "export"):
        syms = [p.strip().strip("'\"") for p in arg_blob.split(",")]
        for s in syms:
            if re.match(r"^[A-Za-z][A-Za-z0-9_.]*$", s):
                export_syms.add(s)

    for arg_blob in _scan_calls(namespace_text, "exportPattern"):
        raw = arg_blob.strip().strip("'\"")
        if raw:
            export_patterns.append(raw)

    return export_syms, export_patterns


def get_import_info_for_pkg(pkg, debug=False):
    """
    Memoized + disk cached:
      - import_map: symbol -> package  (from importFrom)
      - import_pkgs: set(packages)     (from import)

    Disk cache keeps repeated runs from re-fetching/re-parsing NAMESPACE constantly.
    """
    key = pkg.lower().strip()
    if key in _IMPORT_MAP_CACHE and key in _IMPORT_PKG_CACHE:
        return _IMPORT_MAP_CACHE[key], _IMPORT_PKG_CACHE[key]

    disk_file = CACHE_DIR / f"{key}__namespace_imports.json"
    if disk_file.exists():
        try:
            blob = json.loads(disk_file.read_text(encoding="utf-8"))
            mp = blob.get("import_map", {})
            ip = set(blob.get("import_pkgs", []))
            if isinstance(mp, dict):
                _IMPORT_MAP_CACHE[key] = mp
                _IMPORT_PKG_CACHE[key] = ip
                return mp, ip
        except Exception:
            pass

    repo = find_github_repo(pkg)
    if not repo:
        _IMPORT_MAP_CACHE[key] = {}
        _IMPORT_PKG_CACHE[key] = set()
        return {}, set()

    ns_text = load_namespace_file(repo)
    if debug:
        print(f"[NS] pkg={pkg} repo={repo} ns_len={len(ns_text)}", flush=True)

    if not ns_text.strip():
        _IMPORT_MAP_CACHE[key] = {}
        _IMPORT_PKG_CACHE[key] = set()
        return {}, set()

    mp, ip = parse_namespace_imports(ns_text)

    if debug:
        print(
            f"[NS] pkg={pkg} importFrom_syms={len(mp)} import_pkgs={len(ip)}",
            flush=True,
        )

    _IMPORT_MAP_CACHE[key] = mp
    _IMPORT_PKG_CACHE[key] = ip

    try:
        disk_file.write_text(
            json.dumps({"import_map": mp, "import_pkgs": sorted(list(ip))}),
            encoding="utf-8",
        )
    except Exception:
        pass

    return mp, ip


def get_exported_symbols(pkg, debug=False):
    """
    Build a set of exported symbols for a package, for resolving import(pkg) calls.

    Strategy:
      - Parse that package's NAMESPACE:
          export(a, b, c) -> explicit exports
          exportPattern("regex") -> apply regex against function defs we extracted
      - Cache it in memory + on disk because recursion calls this a lot.

    It’s best-effort, but it works well enough to map “bare call name(…)” -> package.
    """
    key = pkg.lower().strip()
    if key in _EXPORT_SYMS_CACHE:
        return _EXPORT_SYMS_CACHE[key]

    disk_file = CACHE_DIR / f"{key}__namespace_exports.json"
    if disk_file.exists():
        try:
            blob = json.loads(disk_file.read_text(encoding="utf-8"))
            syms = set(blob.get("export_syms", []))
            _EXPORT_SYMS_CACHE[key] = syms
            return syms
        except Exception:
            pass

    repo = find_github_repo(pkg)
    if not repo:
        _EXPORT_SYMS_CACHE[key] = set()
        return set()

    ns_text = load_namespace_file(repo)
    if not ns_text.strip():
        _EXPORT_SYMS_CACHE[key] = set()
        return set()

    export_syms, export_patterns = parse_namespace_exports(ns_text)

    # exportPattern("...") means “export everything matching regex”
    # I apply it to the function defs I can see in source (best-effort).
    if export_patterns:
        _, defs, _docs = _get_defs_for_pkg(pkg)
        if defs:
            for pat in export_patterns:
                try:
                    rx = re.compile(pat)
                except Exception:
                    continue
                for name in defs.keys():
                    if rx.search(name):
                        export_syms.add(name)

    if debug:
        print(
            f"[EXP] pkg={pkg} export_syms={len(export_syms)} export_patterns={len(export_patterns)}",
            flush=True,
        )

    _EXPORT_SYMS_CACHE[key] = export_syms
    try:
        disk_file.write_text(
            json.dumps({"export_syms": sorted(list(export_syms))}),
            encoding="utf-8",
        )
    except Exception:
        pass

    return export_syms


# ----------------------------
# R source parsing helpers (best-effort)
# ----------------------------


def extract_function_defs(source):
    """
    Extract function bodies + roxygen blocks directly above each definition.

    Returns:
      defs: {func_name -> body_text}
      docs: {func_name -> roxygen_text}  (joined "#'" lines, with "#'" stripped)

    The roxygen capture is important because stuff like:
      #' @seealso [stringi::stri_count()] which this wraps
    is a real dependency signal even if the body calls stringi functions indirectly.
    """
    pat = re.compile(
        r"(?P<name>[A-Za-z][A-Za-z0-9_.]*)\s*(?:<-|=)\s*function\s*\((?P<args>.*?)\)\s*\{",
        re.DOTALL,
    )

    defs = {}
    docs = {}

    for m in pat.finditer(source):
        name = m.group("name")

        # body
        start = m.end()
        end = _find_matching_brace(source, start - 1)
        if end is None:
            continue
        defs[name] = source[start:end].strip()

        # roxygen: contiguous "#'" lines immediately above definition
        roxy = []
        line_start = source.rfind("\n", 0, m.start())
        i = line_start if line_start != -1 else 0

        while i > 0:
            prev_nl = source.rfind("\n", 0, i)
            line = source[prev_nl + 1 : i].rstrip("\r")

            if line.strip().startswith("#'"):
                roxy.append(line.strip()[2:].lstrip())
                i = prev_nl
                continue

            break

        if roxy:
            docs[name] = "\n".join(reversed(roxy)).strip()
        else:
            docs[name] = ""

    return defs, docs


def _find_matching_brace(text, open_brace_pos):
    """
    Given index of '{', find the matching '}'.
    Tries to ignore braces in strings/comments so it doesn’t miscount instantly.
    """
    if open_brace_pos < 0 or open_brace_pos >= len(text) or text[open_brace_pos] != "{":
        return None

    depth = 0
    i = open_brace_pos
    in_sq = False
    in_dq = False
    in_comment = False
    esc = False

    while i < len(text):
        ch = text[i]

        if in_comment:
            if ch == "\n":
                in_comment = False
            i += 1
            continue

        if esc:
            esc = False
            i += 1
            continue

        if ch == "\\":
            esc = True
            i += 1
            continue

        if not in_sq and not in_dq and ch == "#":
            in_comment = True
            i += 1
            continue

        if ch == "'" and not in_dq:
            in_sq = not in_sq
            i += 1
            continue

        if ch == '"' and not in_sq:
            in_dq = not in_dq
            i += 1
            continue

        if in_sq or in_dq:
            i += 1
            continue

        if ch == "{":
            depth += 1
        elif ch == "}":
            depth -= 1
            if depth == 0:
                return i
        i += 1

    return None


def extract_pkg_calls(source):
    """
    Extract package names from obvious namespace usage / loaders.

    This is intentionally simple:
      - pkg::fun or pkg:::fun
      - library(pkg), require(pkg)
      - requireNamespace("pkg"), loadNamespace("pkg")
      - getExportedValue("pkg", "fun")

    This also works on roxygen text (e.g. @seealso [pkg::fun()]).
    """
    deps = set()

    deps |= set(
        re.findall(
            r"\b([A-Za-z][A-Za-z0-9_.]*):::{0,2}[A-Za-z][A-Za-z0-9_.]*\b", source
        )
    )

    lib_matches = re.findall(
        r"\b(?:library|require)\(\s*([A-Za-z][A-Za-z0-9_.]*|\"[A-Za-z][A-Za-z0-9_.]*\"|'[A-Za-z][A-Za-z0-9_.]*')\s*\)",
        source,
    )
    for x in lib_matches:
        deps.add(x.strip("'\""))

    deps |= set(
        re.findall(
            r"\b(?:requireNamespace|loadNamespace)\(\s*[\"']([A-Za-z][A-Za-z0-9_.]*)[\"']",
            source,
        )
    )

    deps |= set(
        re.findall(
            r"\bgetExportedValue\(\s*[\"']([A-Za-z][A-Za-z0-9_.]*)[\"']\s*,",
            source,
        )
    )

    return deps


def extract_func_calls(source):
    """
    Grab bare calls like foo(...).

    This is noisy by nature, so I don’t treat it as dependencies by itself.
    I only use it after I can resolve a name as:
      - local function definition in this pkg, or
      - imported symbol (via NAMESPACE)
    """
    calls = re.findall(r"\b([A-Za-z][A-Za-z0-9_.]*)\s*\(", source)

    skip = {
        "if",
        "for",
        "while",
        "function",
        "return",
        "switch",
        "repeat",
        "in",
        "next",
        "break",
        "else",
    }
    return [c for c in calls if c not in skip]


# ----------------------------
# Dependency tracing
# ----------------------------


def _get_defs_for_pkg(pkg):
    """
    Memoized: return (repo, defs, docs)
      - defs: {func -> body}
      - docs: {func -> roxygen text}
    """
    key = pkg.lower().strip()
    if key in _DEFS_CACHE and key in _DOCS_CACHE:
        return _REPO_CACHE.get(key), _DEFS_CACHE[key], _DOCS_CACHE[key]

    repo = find_github_repo(pkg)
    _REPO_CACHE[key] = repo
    if not repo:
        _DEFS_CACHE[key] = {}
        _DOCS_CACHE[key] = {}
        return None, {}, {}

    src = load_all_source(repo)
    if not src:
        _DEFS_CACHE[key] = {}
        _DOCS_CACHE[key] = {}
        return repo, {}, {}

    defs, docs = extract_function_defs(src)
    _DEFS_CACHE[key] = defs
    _DOCS_CACHE[key] = docs
    return repo, defs, docs


def trace_dependencies(
    pkg,
    func,
    *,
    depth=3,
    seen=None,
    include_base_stdlib=False,
    debug=False,
):
    """
    Trace dependency packages reachable from (pkg, func).

    Mental model:
      - DFS over a call graph where nodes are (pkg, func)
      - edges come from:
          (a) local calls resolved against defs in same pkg
          (b) explicit pkg::fun calls (from body or roxygen)
          (c) imported-symbol calls resolved via NAMESPACE
    """
    if seen is None:
        seen = set()

    pkg = pkg.strip()
    func = func.strip()
    node = (pkg.lower(), func)

    if depth <= 0 or node in seen:
        return set()
    seen.add(node)

    _repo, defs, docs = _get_defs_for_pkg(pkg)
    if not defs:
        return set()

    body = defs.get(func) or ""
    roxy = docs.get(func) or ""

    if not body and not roxy:
        return set()

    if debug:
        print(f"[TRACE] {pkg}::{func} depth={depth}", flush=True)

    # For dependency signals I analyze roxygen + body together.
    # For bare function call parsing I stick to body only (roxygen is too noisy).
    text = roxy + "\n\n" + body

    deps = set()

    # 1) obvious external signals (pkg::, library(), requireNamespace(), etc.)
    deps |= extract_pkg_calls(text)

    # 2) bare calls list
    bare_calls = extract_func_calls(body)

    # 3) local expansion: bare call matches local function definition
    for name in bare_calls:
        if name in defs:
            deps |= trace_dependencies(
                pkg,
                name,
                depth=depth - 1,
                seen=seen,
                include_base_stdlib=include_base_stdlib,
                debug=debug,
            )

    # 4) explicit pkg::fun cross-package expansion (scan roxygen + body)
    for sub_pkg, sub_func in re.findall(
        r"\b([A-Za-z][A-Za-z0-9_.]*):::{0,2}([A-Za-z][A-Za-z0-9_.]*)\b",
        text,
    ):
        deps.add(sub_pkg)
        if sub_pkg in BASE_PACKAGES and not include_base_stdlib:
            continue
        deps |= trace_dependencies(
            sub_pkg,
            sub_func,
            depth=depth - 1,
            seen=seen,
            include_base_stdlib=include_base_stdlib,
            debug=debug,
        )

    # 5) NAMESPACE import resolution
    import_map, import_pkgs = get_import_info_for_pkg(pkg, debug=debug)

    # exact symbol mappings from importFrom(pkg, sym)
    for name in bare_calls:
        if name in defs:
            continue
        sub_pkg = import_map.get(name)
        if not sub_pkg:
            continue

        deps.add(sub_pkg)
        if sub_pkg in BASE_PACKAGES and not include_base_stdlib:
            continue

        deps |= trace_dependencies(
            sub_pkg,
            name,
            depth=depth - 1,
            seen=seen,
            include_base_stdlib=include_base_stdlib,
            debug=debug,
        )

    # coarse import(pkg) resolution:
    # if this package imports packages wholesale, resolve bare calls by checking which
    # imported packages actually export that symbol.
    if import_pkgs:
        unresolved = [n for n in bare_calls if (n not in defs and n not in import_map)]
        if unresolved:
            export_sets = {}
            for p in import_pkgs:
                if p in BASE_PACKAGES and not include_base_stdlib:
                    continue
                export_sets[p] = get_exported_symbols(p, debug=False)

            for name in unresolved:
                candidates = []
                for p, esyms in export_sets.items():
                    if name in esyms:
                        candidates.append(p)

                if not candidates:
                    continue

                if debug and candidates:
                    print(
                        f"[IMPORT] {pkg} bare_call={name} candidates={candidates}",
                        flush=True,
                    )

                for sub_pkg in candidates:
                    deps.add(sub_pkg)
                    deps |= trace_dependencies(
                        sub_pkg,
                        name,
                        depth=depth - 1,
                        seen=seen,
                        include_base_stdlib=include_base_stdlib,
                        debug=debug,
                    )

    if not include_base_stdlib:
        deps = {d for d in deps if d not in BASE_PACKAGES}

    return deps


def function_dependencies(
    func,
    pkgs,
    *,
    depth=4,
    include_base_stdlib=False,
    include_root_pkg=True,
    debug=False,
):
    """
    Entry point.
    """
    if isinstance(pkgs, str):
        pkgs = [pkgs]

    roots = {p.strip() for p in pkgs if p and p.strip()}
    deps = set()

    for pkg in roots:
        deps |= trace_dependencies(
            pkg,
            func,
            depth=depth,
            seen=set(),
            include_base_stdlib=include_base_stdlib,
            debug=debug,
        )

    if include_root_pkg:
        deps |= roots

    if not include_base_stdlib:
        deps = {d for d in deps if d not in BASE_PACKAGES} | (
            roots if include_root_pkg else set()
        )

    return sorted(deps)
