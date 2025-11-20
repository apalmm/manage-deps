from rpy2.robjects import r
import rpy2.robjects as ro
from rpy2.robjects.conversion import localconverter, converter_ctx
from rpy2.robjects import default_converter


def convert_r_to_python(obj):
    if isinstance(obj, ro.vectors.StrVector):
        return list(obj)
    elif isinstance(obj, ro.vectors.ListVector):
        return {name: convert_r_to_python(value) for name, value in zip(obj.names, obj)}
    return obj


def function_dependencies(func, pkgs=None, depth=3):
    converter_ctx.set(default_converter)

    # Convert Python -> R string/vector
    if pkgs:
        if isinstance(pkgs, str):
            pkgs_r = f'"{pkgs}"'
        else:
            pkgs_r = "c(" + ",".join([f'"{p}"' for p in pkgs]) + ")"
    else:
        pkgs_r = "character()"

    with localconverter(default_converter):
        r('options(repos = c(CRAN = "https://cloud.r-project.org"))')

        res = r(f"""
            suppressPackageStartupMessages({{
                if (!requireNamespace("codetools", quietly = TRUE))
                    stop("codetools required")
                if (!requireNamespace("tools", quietly = TRUE))
                    stop("tools required")

                extra_pkgs <- {pkgs_r}
                for (pkg in extra_pkgs) {{
                    if (!requireNamespace(pkg, quietly = TRUE)) {{
                        install.packages(pkg)
                    }}
                    library(pkg, character.only = TRUE)
                }}

                # load recursive dependencies too
                all_deps <- unique(unlist(tools::package_dependencies(extra_pkgs, recursive=TRUE)))
                for (dep in all_deps) {{
                    if (!requireNamespace(dep, quietly = TRUE))
                        next
                    suppressWarnings(library(dep, character.only = TRUE))
                }}
            }})

            library(codetools)

            find_function_packages <- function(fn_name, depth = {depth}, seen = character()) {{
                if (depth <= 0 || fn_name %in% seen)
                    return(character())

                seen <- c(seen, fn_name)
                pkgs_found <- character()

                tryCatch({{
                    fn_obj <- get(fn_name, mode = "function")
                    pkg <- sub("^package:", "", find(fn_name)[1])
                    if (!is.na(pkg) && nzchar(pkg))
                        pkgs_found <- unique(c(pkgs_found, pkg))

                    globals <- codetools::findGlobals(fn_obj)

                    skip_syms <- c(".GlobalEnv", ".Call", ".Internal", ".Primitive", ".External", ".External2", "NULL")
                    globals <- setdiff(globals, skip_syms)

                    for (sym in globals) {{
                        owner <- tryCatch(sub("^package:", "", find(sym)[1]), error = function(e) NA)
                        if (!is.na(owner) && nzchar(owner)) {{
                            pkgs_found <- unique(c(pkgs_found, owner))
                            if (exists(sym, mode = "function", inherits = TRUE)) {{
                                deeper <- tryCatch(find_function_packages(sym, depth - 1, seen),
                                    error = function(e) character())
                                pkgs_found <- unique(c(pkgs_found, deeper))
                            }}
                        }}
                    }}
                }}, error = function(e) {{
                    message(sprintf("Skipping %s: %s", fn_name, e$message))
                }})

                unique(pkgs_found)
            }}

            find_function_packages("{func}")
        """)

    # Convert to Python
    result = convert_r_to_python(res)

    # Filter out unwanted or meaningless results
    # ignore_patterns = {
    #     "base", "methods", "utils", "stats", "graphics", "grDevices",
    #     "datasets", "tools", "compiler", "codetools"
    # }

    # Keep only meaningful, user-installed or high-level packages
    filtered = [
        pkg for pkg in result
        if isinstance(pkg, str)
        # and pkg not in ignore_patterns
        and not pkg.startswith("package:")
        and pkg.strip() != ""
    ]

    return filtered
