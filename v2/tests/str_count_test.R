# str_count_runtime_validation_strict.R
#
# This checks "runtime sufficiency" in a clean library, without pretending R starts empty.
# R always has base/recommended stuff around (datasets is common), so we treat those as allowed.

options(repos = c(CRAN = "https://cloud.r-project.org"))

predicted <- c("stringr", "stringi", "vctrs", "rlang")

# Base + recommended packages that can appear without you doing anything.
# (R startup differs by install; don't fight it.)
allowed_system <- c(
  "base", "compiler", "datasets", "graphics", "grDevices",
  "grid", "methods", "parallel", "splines", "stats", "stats4",
  "tcltk", "tools", "utils"
)

cat("=== Minimal runtime dependency validation for stringr::str_count ===\n\n")
cat("R version:\n")
print(R.version.string)

# ---- clean isolated library ----
lib <- tempfile("rdeps_lib_")
dir.create(lib, recursive = TRUE, showWarnings = FALSE)

cat("\nIsolated lib:\n")
cat(lib, "\n")

orig_libpaths <- .libPaths()
.libPaths(c(lib, .Library))

cat("\n.libPaths() (forced):\n")
print(.libPaths())

# ---- install only the predicted packages into the isolated lib ----
cat("\nInstalling predicted packages (dependencies=FALSE):\n")
print(predicted)
install.packages(predicted, lib = lib, dependencies = FALSE)

installed <- rownames(installed.packages(lib.loc = lib))
cat("\nInstalled in isolated lib:\n")
print(sort(installed))

missing <- setdiff(predicted, installed)
if (length(missing)) stop("FAIL: missing installs: ", paste(missing, collapse = ", "))

extras <- setdiff(installed, predicted)
if (length(extras)) stop("FAIL: extra packages installed into isolated lib: ", paste(extras, collapse = ", "))

cat("\nOK: isolated lib contains exactly the predicted packages.\n")

# ---- baseline namespaces ----
baseline <- loadedNamespaces()
cat("\nLoaded namespaces BEFORE loading stringr:\n")
print(sort(baseline))

# ---- run the actual function tests ----
cat("\nLoading stringr from isolated lib...\n")
suppressPackageStartupMessages(library(stringr, lib.loc = lib))

cat("Running tests...\n")

stopifnot(stringr::str_count("banana", "a") == 3L)
stopifnot(identical(stringr::str_count(c("aa", "aba", NA, ""), "a"),
                    c(2L, 2L, NA_integer_, 0L)))
stopifnot(stringr::str_count("abc123", "[a-z]") == 3L)
stopifnot(stringr::str_count("", "a") == 0L)
stopifnot(stringr::str_count("öööö", "ö") == 4L)

cat("PASS: functional checks\n")

# ---- what got loaded as a result of using str_count ----
after <- loadedNamespaces()
cat("\nLoaded namespaces AFTER loading stringr + running str_count:\n")
print(sort(after))

# focus on what *new* things appeared
newly_loaded <- setdiff(after, baseline)
cat("\nNew namespaces loaded during this run:\n")
print(sort(newly_loaded))

# fail only if something outside the union of:
#   - allowed system
#   - predicted set
# gets loaded
allowed_total <- unique(c(allowed_system, predicted))

unexpected <- setdiff(after, allowed_total)
if (length(unexpected)) {
  cat("\nUnexpected namespaces:\n")
  print(sort(unexpected))
  stop("FAIL: unexpected namespaces were loaded (not in system or predicted set).")
}

cat("\n✅ FINAL PASS: str_count works, and only system + predicted namespaces were loaded.\n")

# cleanup
.libPaths(orig_libpaths)
