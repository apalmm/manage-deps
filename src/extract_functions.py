from rpy2 import robjects
import json

def export_package_functions(packages, out_path="data/package_functions.json"):
    result = {}
    r = robjects.r

    for pkg in packages:
        try:
            # Check if package is installed
            installed = r(f'"{pkg}" %in% rownames(installed.packages())')[0]
            if not installed:
                result[pkg] = []
                continue

            # Get exported object names
            func_names = list(r(f"getNamespaceExports('{pkg}')"))
            result[pkg] = func_names
        except Exception as e:
            print(f"Could not fetch functions for {pkg}: {e}")
            result[pkg] = []

    with open(out_path, "w", encoding="utf-8") as f:
        json.dump(result, f, indent=2)

    print(f"Saved R function metadata to {out_path}")
    return out_path
