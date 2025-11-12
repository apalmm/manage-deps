from rpy2.robjects import r
from rpy2.robjects.packages import importr

codetools = importr('codetools')
tools = importr('tools')
r('library(tidyverse)')

def function_dependencies(func_names):
    deps_pkgs = set()
    for func in func_names:
        try:
            # Find the package it lives in
            pkg = r(f'find("{func}")')[0].replace("package:", "")
            deps_pkgs.add(pkg)

            # Inspect what globals it calls
            globals_ = codetools.findGlobals(r(func))
            for sym in globals_:
                try:
                    owner = r(f'find("{sym}")')[0].replace("package:", "")
                    deps_pkgs.add(owner)
                except Exception:
                    pass
        except Exception as e:
            print(f"Skipping {func}: {e}")
    return deps_pkgs

# Example: only using filter() and geom_point()
needed = function_dependencies(["filter"])

# Expand to recursive package dependencies
deps_full = tools.package_dependencies(list(needed), recursive=True)
print("Required package tree:", deps_full)
