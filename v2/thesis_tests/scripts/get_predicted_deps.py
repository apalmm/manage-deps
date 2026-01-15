# thesis_tests/scripts/get_predicted_deps.py
#
# Fetch the predicted dependency set for one (package, function) pair from my Flask backend
# and print it as JSON to stdout. The batch runner redirects this into predicted_deps.json.
#
# Usage:
#   python scripts/get_predicted_deps.py stringr str_count
#
# Notes:
#   - This intentionally does not try to be clever about environment detection.
#   - Set ANALYZER_URL if your Flask app isn't on localhost:5000.
#     Example: export ANALYZER_URL="http://127.0.0.1:5000"

import json
import os
import sys
import urllib.request

ANALYZER_URL = os.getenv("ANALYZER_URL", "http://127.0.0.1:5000")

def die(msg, code=1):
    print(msg, file=sys.stderr)
    sys.exit(code)

def post_json(url, payload, timeout=60):
    data = json.dumps(payload).encode("utf-8")
    req = urllib.request.Request(
        url,
        data=data,
        headers={"Content-Type": "application/json"},
        method="POST",
    )
    with urllib.request.urlopen(req, timeout=timeout) as resp:
        body = resp.read().decode("utf-8")
        return resp.status, body


def main():
    if len(sys.argv) < 3:
        die("Usage: get_predicted_deps.py <package> <function>")

    pkg = sys.argv[1].strip()
    func = sys.argv[2].strip()

    if not pkg or not func:
        die("Error: package and function must be non-empty")

    url = f"{ANALYZER_URL}/analyze"
    payload = {"function": func, "packages": pkg}

    try:
        status, body = post_json(url, payload)
    except Exception as e:
        die(f"Error: failed to reach analyzer at {ANALYZER_URL}: {e}")

    if status != 200:
        die(f"Error: analyzer returned HTTP {status}: {body}")

    try:
        data = json.loads(body)
    except Exception as e:
        die(f"Error: analyzer returned non-JSON: {e}\nBody:\n{body}")

    deps = data.get("required_packages", [])
    if deps is None:
        deps = []

    # normalize + dedupe while preserving order
    seen = set()
    out = []
    for d in deps:
        d = str(d).strip()
        if not d or d in seen:
            continue
        seen.add(d)
        out.append(d)

    # Always include the root package, because the runtime test needs it installed too.
    # This matches how you'd actually run "library(pkg); pkg::fun()".
    if pkg not in seen:
        out.insert(0, pkg)

    print(json.dumps(out))


if __name__ == "__main__":
    main()