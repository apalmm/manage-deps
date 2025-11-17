from rpy2.robjects import r
import rpy2.robjects as ro
from rpy2.robjects.conversion import localconverter, converter_ctx
from rpy2.robjects import default_converter, pandas2ri


def convert_r_to_python(obj):
    if isinstance(
        obj,
        (
            ro.vectors.IntVector,
            ro.vectors.FloatVector,
            ro.vectors.StrVector,
            ro.vectors.BoolVector,
        ),
    ):
        return list(obj)

    elif isinstance(obj, ro.vectors.DataFrame):
        with localconverter(default_converter + pandas2ri.converter):
            return {col: list(obj.rx2(col)) for col in obj.colnames}

    elif isinstance(obj, ro.vectors.ListVector):
        return {name: convert_r_to_python(value) for name, value in zip(obj.names, obj)}

    elif isinstance(obj, ro.vectors.Matrix):
        return [list(row) for row in obj]

    return obj


def function_dependencies(func, pkgs=None):
    converter_ctx.set(default_converter)

    # Convert Python list → R vector syntax
    if pkgs:
        if isinstance(pkgs, str):
            pkgs_r = f'"{pkgs}"'
        else:
            pkgs_r = "c(" + ",".join([f'"{p}"' for p in pkgs]) + ")"
    else:
        pkgs_r = "character()"

    with localconverter(default_converter):
        r('options(repos = c(CRAN = "https://cloud.r-project.org"))')

        deps_pkgs = r(f"""
            suppressPackageStartupMessages({{
                if (!requireNamespace("codetools", quietly = TRUE))
                    stop("codetools must be installed. Please install it in R.")
                if (!requireNamespace("tools", quietly = TRUE))
                    stop("tools package is missing (base R should have it).")

                extra_pkgs <- {pkgs_r}

                for (pkg in extra_pkgs) {{
                    if (!requireNamespace(pkg, quietly = TRUE)) {{
                        install.packages(pkg)
                    }}
                    library(pkg, character.only = TRUE)
                }}

                library(codetools)
                library(tools)
            }})

            analyze_function_deps <- function(func_name) {{
                deps_pkgs <- character()

                tryCatch({{
                    pkg <- sub("^package:", "", find(func_name)[1])
                    deps_pkgs <- unique(c(deps_pkgs, pkg))

                    fn_obj <- get(func_name, mode = "function")
                    globals_ <- codetools::findGlobals(fn_obj)

                    for (sym in globals_) {{
                        owner <- find(sym)[1]
                        if (!is.na(owner) && nzchar(owner)) {{
                            owner <- sub("^package:", "", owner)
                            deps_pkgs <- unique(c(deps_pkgs, owner))
                        }}
                    }}

                }}, error = function(e) {{
                    message(sprintf("Skipping %s: %s", func_name, e$message))
                }})

                return(deps_pkgs)
            }}

            analyze_function_deps("{func}")
        """)

    return convert_r_to_python(deps_pkgs)
