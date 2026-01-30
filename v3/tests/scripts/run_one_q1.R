#!/usr/bin/env Rscript

cat("1: Minimal runtime dependency validation\n\n")

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 3) {
  stop("Usage: run_one_q1.R <package> <function> <fixture_json>")
}
pkg  <- args[[1]]
fun  <- args[[2]]
json_file <- args[[3]]

cat("Package:", pkg, "\n")
cat("Function:", fun, "\n")
cat("Fixture:", json_file, "\n\n")

suppressPackageStartupMessages(library(jsonlite))

# --- create isolated library ---
lib <- file.path(tempdir(), paste0("rdeps_lib_",
                                   as.integer(Sys.time()), "_", Sys.getpid()))
dir.create(lib, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(lib, .libPaths()))

# --- CRAN mirror ---
options(repos = c(CRAN = readLines("thesis_tests/config/cran_mirror.txt")))

# --- load predicted dependencies ---
deps_path <- file.path("thesis_tests", "results", "q1",
                       paste0(pkg, "__", fun, "_deps.json"))

if (!file.exists(deps_path)) {
  stop(paste0("Missing deps file: ", deps_path,
              "\nRun: python thesis_tests/scripts/get_predicted_deps.py ",
              pkg, " ", fun,
              " > ", deps_path))
}

hypothesis <- jsonlite::fromJSON(deps_path)

cat("Hypothesized packages (dependencies=FALSE):\n")
print(hypothesis)

install.packages(hypothesis, dependencies = FALSE, quiet = TRUE)

installed <- rownames(installed.packages(lib.loc = lib))
cat("\nInstalled in isolated lib:\n")
print(installed)

# --- snapshot namespaces before ---
before <- loadedNamespaces()

# --- load target function ---
library(pkg, character.only = TRUE)

# --- load fixtures ---
cases <- jsonlite::fromJSON(json_file)

# normalize into a list of case objects
if (is.data.frame(cases)) {
  cases_list <- lapply(seq_len(nrow(cases)), function(i) cases[i, , drop = FALSE])
} else if (is.list(cases)) {
  cases_list <- cases
} else {
  stop("Fixture JSON parsed into an unexpected type")
}

f <- get(fun, envir = asNamespace(pkg))

normalize_input <- function(input) {
  # jsonlite data.frame rows give input as a list-column wrapper: list(<named list>)
  if (is.list(input) && length(input) == 1 && is.list(input[[1]]) && !is.null(names(input[[1]]))) {
    input <- input[[1]]
  }

  # force typical atomic types (stringi wants atomic vectors)
  if (!is.list(input) || is.null(names(input))) {
    stop("Fixture 'input' must be a named list of arguments")
  }

  # specifically coerce string/pattern if present (keeps fixtures simple)
  if (!is.null(input$string))  input$string  <- as.character(unlist(input$string,  use.names = FALSE))
  if (!is.null(input$pattern)) input$pattern <- as.character(unlist(input$pattern, use.names = FALSE))

  input
}

normalize_expected <- function(expected) {
  # jsonlite data.frame rows can make expected a 1-element list-column
  if (is.list(expected) && length(expected) == 1) expected <- expected[[1]]

  # if expected is a list of scalars, flatten it
  if (is.list(expected)) expected <- unlist(expected, recursive = TRUE, use.names = FALSE)

  expected
}

for (case in cases_list) {
  id <- if (!is.null(case$id)) case$id else case[["id"]]

  input <- if (!is.null(case$input)) case$input else case[["input"]]
  expected <- if (!is.null(case$expected)) case$expected else case[["expected"]]

  input <- normalize_input(input)
  expected <- normalize_expected(expected)

  result <- do.call(f, input)

  # for consistency, flatten scalar-ish results
  if (is.list(result)) result <- unlist(result, recursive = TRUE, use.names = FALSE)

  if (!identical(result, expected)) {
    cat("FAIL:", id, "\n")
    cat("Expected:\n")
    print(expected)
    cat("Got:\n")
    print(result)
    stop("Test failed")
  } else {
    cat("PASS:", id, "\n")
  }
}

# --- snapshot namespaces after ---
after <- loadedNamespaces()
new <- setdiff(after, before)

cat("\nNew namespaces loaded:\n")
print(new)

cat("\nPASS: function runs correctly with hypothesized dependencies\n")