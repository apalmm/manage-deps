import os, json, requests

def fetch_cran_metadata(pkg, outdir="data/raw"):
    #function for fetching and caching CRAN package metadata
    os.makedirs(outdir, exist_ok=True)
    path = os.path.join(outdir, f"{pkg}.json")
    if os.path.exists(path):
        with open(path, "r") as f:
            return json.load(f)
        
    url = f"https://crandb.r-pkg.org/{pkg}"
    r = requests.get(url)

    if r.status_code != 200:
        raise ValueError(f"Failed to fetch metadata for package {pkg}")
    
    data = r.json()
    with open(path, "w") as f:
        json.dump(data, f, indent=2)

    return data