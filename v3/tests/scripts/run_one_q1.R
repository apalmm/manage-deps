#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 3) {
  stop("Usage: run_one_q1.R <package> <function> <fixture_json>")
}

pkg <- args[[1]]
fun <- args[[2]]
json_file <- args[[3]]

suppressPackageStartupMessages(library(jsonlite))

# CRAN mirror
options(repos = c(CRAN = readLines("thesis_tests/config/cran_mirror.txt")))

# predicted deps path
deps_path <- file.path(
  "thesis_tests", "results", "q1",
  paste0(pkg, "__", fun, "_deps.json")
)

if (!file.exists(deps_path)) {
  stop(
    paste0(
      "Missing deps file: ", deps_path, "\n",
      "Run: python thesis_tests/scripts/get_predicted_deps.py ",
      pkg, " ", fun, " > ", deps_path
    )
  )
}

minimal_package_set <- jsonlite::fromJSON(deps_path)

# isolated library (avoid runif() to prevent NA weirdness)
lib <- file.path(tempdir(), paste0("rdeps_lib_", as.integer(Sys.time()), "_", Sys.getpid()))
dir.create(lib, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(lib, .libPaths()))

install.packages(minimal_package_set, dependencies = FALSE, quiet = TRUE)

before <- loadedNamespaces()

suppressPackageStartupMessages(library(pkg, character.only = TRUE))
f <- get(fun, envir = asNamespace(pkg))

cases <- jsonlite::fromJSON(json_file)

# fixtures must be a list of objects like:
# { "id": "...", "input": { ... }, "expected": ... }
if (!is.list(cases) || is.data.frame(cases)) {
  stop("Fixture JSON must be a list of case objects (not a data.frame)")
}

normalize_input_scalar <- function(input) {
  if (!is.list(input) || is.null(names(input))) {
    stop("Fixture 'input' must be a named list of arguments")
  }

  # enforce scalar-only for this script
  if (!is.null(input$string)) {
    s <- unlist(input$string, use.names = FALSE)
    if (length(s) != 1) stop("Scalar-only: input$string must be length 1")
    input$string <- as.character(s[[1]])
  }
  if (!is.null(input$pattern)) {
    p <- unlist(input$pattern, use.names = FALSE)
    if (length(p) != 1) stop("Scalar-only: input$pattern must be length 1")
    input$pattern <- as.character(p[[1]])
  }

  input
}

normalize_expected_scalar <- function(expected) {
  if (is.list(expected)) {
    e <- unlist(expected, recursive = TRUE, use.names = FALSE)
  } else {
    e <- expected
  }
  if (length(e) != 1) stop("Scalar-only: expected must be length 1")
  e
}

for (case in cases) {
  if (!is.list(case)) stop("Each test case must be an object")
  if (is.null(case$id) || is.null(case$input)) stop("Each case needs 'id' and 'input'")

  input <- normalize_input_scalar(case$input)
  expected <- normalize_expected_scalar(case$expected)

  result <- do.call(f, input)
  if (is.list(result)) result <- unlist(result, recursive = TRUE, use.names = FALSE)
  if (length(result) != 1) stop(paste0("Scalar-only: result not length 1 for case ", case$id))

  if (!identical(result, expected)) {
    stop(paste0("FAIL: ", case$id))
  }
}

after <- loadedNamespaces()
new <- setdiff(after, before)

writeLines(c(
  paste0("pkg=", pkg),
  paste0("fun=", fun),
  paste0("deps_predicted=", paste(minimal_package_set, collapse = ",")),
  paste0("namespaces_new=", paste(new, collapse = ","))
))
