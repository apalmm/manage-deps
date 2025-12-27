# File for fetching function descriptions from online sources, multiple strategies
# File for fetching package descriptions from online sources, multiple strategies
import re
import json
import hashlib
import os
from pathlib import Path

import requests
from bs4 import BeautifulSoup

DESC_CACHE_DIR = Path("cache/descriptions")
DESC_CACHE_DIR.mkdir(parents=True, exist_ok=True)

DESC_UA = os.getenv("DEEPDEPBOT_UA", "DeepDepBot")
DESC_TIMEOUT = 12

_desc_session = requests.Session()
_desc_headers = {"User-Agent": DESC_UA}


def _desc_cache_path(key: str) -> Path:
    h = hashlib.sha1(key.encode("utf-8")).hexdigest()
    return DESC_CACHE_DIR / f"{h}.json"


def _cache_get_json(key: str):
    p = _desc_cache_path(key)
    if not p.exists():
        return None
    try:
        return json.loads(p.read_text(encoding="utf-8"))
    except Exception:
        return None


def _cache_set_json(key: str, obj):
    p = _desc_cache_path(key)
    try:
        p.write_text(json.dumps(obj), encoding="utf-8")
    except Exception:
        pass


def _fetch_html(url: str):
    r = _desc_session.get(url, headers=_desc_headers, timeout=DESC_TIMEOUT)
    if not r.ok:
        return None
    return r.text


def _clean_text(s: str) -> str:
    s = re.sub(r"\s+", " ", s or "").strip()
    return s


def scrape_package_description(pkg: str) -> str:
    """
    Try to fetch a short human-readable package description.

    Priority:
      1) rdrr.io package page
      2) rdocumentation.org package page
      3) CRAN package page (Description: ...)
    """
    pkg = (pkg or "").strip()
    if not pkg:
        return ""

    cache_key = f"pkg::{pkg}"
    cached = _cache_get_json(cache_key)
    if cached and "description" in cached:
        return cached["description"] or ""

    # --- 1) rdrr.io ---
    # Example: https://rdrr.io/cran/stringr/
    url = f"https://rdrr.io/cran/{pkg}/"
    html = None
    try:
        html = _fetch_html(url)
    except Exception:
        html = None

    if html:
        soup = BeautifulSoup(html, "html.parser")

        # rdrr has a short lead paragraph near the top; this selector is intentionally loose
        # so it doesn't break if they tweak classnames.
        lead = soup.find("meta", attrs={"name": "description"})
        if lead and lead.get("content"):
            desc = _clean_text(lead["content"])
            if desc:
                _cache_set_json(cache_key, {"description": desc, "source": "rdrr"})
                return desc

        # fallback: try first reasonably-sized paragraph in main content
        main = soup.find("div", {"id": "readme"}) or soup.find("div", {"class": "container"})
        if main:
            p = main.find("p")
            if p:
                desc = _clean_text(p.get_text(" ", strip=True))
                if desc:
                    _cache_set_json(cache_key, {"description": desc, "source": "rdrr_fallback"})
                    return desc

    # --- 2) rdocumentation.org ---
    # Example: https://www.rdocumentation.org/packages/stringr
    url = f"https://www.rdocumentation.org/packages/{pkg}"
    html = None
    try:
        html = _fetch_html(url)
    except Exception:
        html = None

    if html:
        soup = BeautifulSoup(html, "html.parser")

        # try meta description first (usually good enough)
        lead = soup.find("meta", attrs={"name": "description"})
        if lead and lead.get("content"):
            desc = _clean_text(lead["content"])
            if desc:
                _cache_set_json(cache_key, {"description": desc, "source": "rdocumentation"})
                return desc

        # fallback: some pages have a short summary blurb
        summary = soup.find("div", {"class": "package-description"}) or soup.find("div", {"class": "description"})
        if summary:
            desc = _clean_text(summary.get_text(" ", strip=True))
            if desc:
                _cache_set_json(cache_key, {"description": desc, "source": "rdocumentation_fallback"})
                return desc

    # --- 3) CRAN package page ---
    # Example: https://cran.r-project.org/package=stringr
    url = f"https://cran.r-project.org/package={pkg}"
    html = None
    try:
        html = _fetch_html(url)
    except Exception:
        html = None

    if html:
        soup = BeautifulSoup(html, "html.parser")

        # CRAN page has a table; "Description:" is a <tr><td> label in many packages.
        # We'll just brute-force search for the text "Description:" and use sibling cell.
        desc = ""
        for tr in soup.find_all("tr"):
            tds = tr.find_all("td")
            if len(tds) >= 2:
                label = _clean_text(tds[0].get_text(" ", strip=True)).lower()
                if label.startswith("description"):
                    desc = _clean_text(tds[1].get_text(" ", strip=True))
                    break

        if desc:
            _cache_set_json(cache_key, {"description": desc, "source": "cran"})
            return desc

        # last-ditch: meta description
        lead = soup.find("meta", attrs={"name": "description"})
        if lead and lead.get("content"):
            desc = _clean_text(lead["content"])
            if desc:
                _cache_set_json(cache_key, {"description": desc, "source": "cran_meta"})
                return desc

    _cache_set_json(cache_key, {"description": "", "source": "none"})
    return ""


def scrape_function_description(pkg: str, func: str) -> str:
    """
    Try to fetch a short function description (one-liner-ish).

    Priority:
      1) rdrr.io function page (usually clean + stable)
      2) rdocumentation.org function page
      3) CRAN manual HTML (very optional; brittle, so only as fallback)
    """
    pkg = (pkg or "").strip()
    func = (func or "").strip()
    if not pkg or not func:
        return ""

    cache_key = f"fn::{pkg}::{func}"
    cached = _cache_get_json(cache_key)
    if cached and "description" in cached:
        return cached["description"] or ""

    # --- 1) rdrr.io ---
    # Example: https://rdrr.io/cran/stringr/man/str_count.html
    url = f"https://rdrr.io/cran/{pkg}/man/{func}.html"
    html = None
    try:
        html = _fetch_html(url)
    except Exception:
        html = None

    if html:
        soup = BeautifulSoup(html, "html.parser")

        # rdrr often has a concise "Description" section; try to find it by heading text.
        # look for <h2> or <h3> named "Description"
        desc = ""
        for hx in soup.find_all(["h2", "h3"]):
            if _clean_text(hx.get_text()).lower() == "description":
                # next sibling block usually contains text
                nxt = hx.find_next_sibling()
                if nxt:
                    desc = _clean_text(nxt.get_text(" ", strip=True))
                break

        if not desc:
            # fallback: meta description often contains good summary
            lead = soup.find("meta", attrs={"name": "description"})
            if lead and lead.get("content"):
                desc = _clean_text(lead["content"])

        if desc:
            _cache_set_json(cache_key, {"description": desc, "source": "rdrr"})
            return desc

    # --- 2) rdocumentation.org ---
    # Example: https://www.rdocumentation.org/packages/stringr/versions/1.6.0/topics/str_count
    # Version is optional; without it, it redirects to latest.
    url = f"https://www.rdocumentation.org/packages/{pkg}/topics/{func}"
    html = None
    try:
        html = _fetch_html(url)
    except Exception:
        html = None

    if html:
        soup = BeautifulSoup(html, "html.parser")

        # meta description first
        lead = soup.find("meta", attrs={"name": "description"})
        if lead and lead.get("content"):
            desc = _clean_text(lead["content"])
            if desc:
                _cache_set_json(cache_key, {"description": desc, "source": "rdocumentation"})
                return desc

        # fallback: the first paragraph in the topic content
        main = soup.find("div", {"class": "topic-content"}) or soup.find("div", {"class": "documentation"})
        if main:
            p = main.find("p")
            if p:
                desc = _clean_text(p.get_text(" ", strip=True))
                if desc:
                    _cache_set_json(cache_key, {"description": desc, "source": "rdocumentation_fallback"})
                    return desc

    # --- 3) CRAN manual HTML (last resort; can be ugly) ---
    # This is not always present in a stable way, but sometimes works:
    # https://cran.r-project.org/web/packages/<pkg>/man/<func>.html
    url = f"https://cran.r-project.org/web/packages/{pkg}/man/{func}.html"
    html = None
    try:
        html = _fetch_html(url)
    except Exception:
        html = None

    if html:
        soup = BeautifulSoup(html, "html.parser")
        # try to take the first non-empty paragraph
        p = soup.find("p")
        if p:
            desc = _clean_text(p.get_text(" ", strip=True))
            if desc:
                _cache_set_json(cache_key, {"description": desc, "source": "cran_manual"})
                return desc

    _cache_set_json(cache_key, {"description": "", "source": "none"})
    return ""
