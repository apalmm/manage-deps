

# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/.github/workflows/dep-suggests-matrix/action.R ---
# FIXME: Dynamic lookup by parsing https://svn.r-project.org/R/tags/
get_deps <- function() {
  # Determine package dependencies
  if (!requireNamespace("desc", quietly = TRUE)) {
    install.packages("desc")
  }

  deps_df <- desc::desc_get_deps()
  deps_df_optional <- deps_df$package[deps_df$type %in% c("Suggests", "Enhances")]
  deps_df_hard <- deps_df$package[deps_df$type %in% c("Depends", "Imports", "LinkingTo")]
  deps_df_base <- unlist(tools::standard_package_names(), use.names = FALSE)

  packages <- sort(deps_df_optional)
  packages <- intersect(packages, rownames(available.packages()))

  # Too big to fail, or can't be avoided:
  off_limits <- c("testthat", "rmarkdown", "rcmdcheck", deps_df_hard, deps_df_base)
  off_limits_dep <- unlist(tools::package_dependencies(off_limits, recursive = TRUE, which = "strong"))
  setdiff(packages, c(off_limits, off_limits_dep))
}

if (Sys.getenv("GITHUB_BASE_REF") != "") {
  print(Sys.getenv("GITHUB_BASE_REF"))
  system("git fetch origin ${GITHUB_BASE_REF}")
  # Use .. to avoid having to fetch the entire history
  # https://github.com/krlmlr/actions-sync/issues/45
  diff_cmd <- "git diff origin/${GITHUB_BASE_REF}.. -- R/ tests/ | egrep '^[+][^+]' | grep -q ::"
  diff_lines <- system(diff_cmd, intern = TRUE)
  if (length(diff_lines) > 0) {
    writeLines("Changes using :: in R/ or tests/:")
    writeLines(diff_lines)
    packages <- get_deps()
  } else {
    writeLines("No changes using :: found in R/ or tests/, not checking without suggested packages")
    packages <- character()
  }
} else {
  writeLines("No GITHUB_BASE_REF, checking without suggested packages")
  packages <- get_deps()
}

if (length(packages) > 0) {
  json <- paste0(
    '{"package":[',
    paste0('"', packages, '"', collapse = ","),
    "]}"
  )
  writeLines(paste0("matrix=", json), Sys.getenv("GITHUB_OUTPUT"))
  writeLines(json)
} else {
  writeLines("No suggested packages found.")
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/.github/workflows/versions-matrix/action.R ---
# Determine active versions of R to test against
tags <- xml2::read_html("https://svn.r-project.org/R/tags/")

bullets <-
  tags |>
  xml2::xml_find_all("//li") |>
  xml2::xml_text()

version_bullets <- grep("^R-([0-9]+-[0-9]+-[0-9]+)/$", bullets, value = TRUE)
versions <- unique(gsub("^R-([0-9]+)-([0-9]+)-[0-9]+/$", "\\1.\\2", version_bullets))

r_release <- head(sort(as.package_version(versions), decreasing = TRUE), 5)

deps <- desc::desc_get_deps()
r_crit <- deps$version[deps$package == "R"]
if (length(r_crit) == 1) {
  min_r <- as.package_version(gsub("^>= ([0-9]+[.][0-9]+)(?:.*)$", "\\1", r_crit))
  r_release <- r_release[r_release >= min_r]
}

r_versions <- c("devel", as.character(r_release))

macos <- data.frame(os = "macos-latest", r = r_versions[2:3])
windows <- data.frame(os = "windows-latest", r = r_versions[1:3])
linux_devel <- data.frame(os = "ubuntu-22.04", r = r_versions[1], `http-user-agent` = "release", check.names = FALSE)
linux <- data.frame(os = "ubuntu-22.04", r = r_versions[-1])
covr <- data.frame(os = "ubuntu-22.04", r = r_versions[2], covr = "true", desc = "with covr")

include_list <- list(macos, windows, linux_devel, linux, covr)

if (file.exists(".github/versions-matrix.R")) {
  custom <- source(".github/versions-matrix.R")$value
  if (is.data.frame(custom)) {
    custom <- list(custom)
  }
  include_list <- c(include_list, custom)
}

print(include_list)

filter <- read.dcf("DESCRIPTION")[1, ]["Config/gha/filter"]
if (!is.na(filter)) {
  filter_expr <- parse(text = filter)[[1]]
  subset_fun_expr <- bquote(function(x) subset(x, .(filter_expr)))
  subset_fun <- eval(subset_fun_expr)
  include_list <- lapply(include_list, subset_fun)
  print(include_list)
}

to_json <- function(x) {
  if (nrow(x) == 0) return(character())
  parallel <- vector("list", length(x))
  for (i in seq_along(x)) {
    parallel[[i]] <- paste0('"', names(x)[[i]], '":"', x[[i]], '"')
  }
  paste0("{", do.call(paste, c(parallel, sep = ",")), "}")
}

configs <- unlist(lapply(include_list, to_json))
json <- paste0('{"include":[', paste(configs, collapse = ","), "]}")

if (Sys.getenv("GITHUB_OUTPUT") != "") {
  writeLines(paste0("matrix=", json), Sys.getenv("GITHUB_OUTPUT"))
}
writeLines(json)


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/R/aaa-options.R ---
make_option_impl <- function(getter, option_name = NULL, env = caller_env()) {
  getter_body <- enexpr(getter)

  if (is.null(option_name)) {
    # Assuming that the call is getOption()
    option_name <- getter_body[[2]]
    stopifnot(is.character(option_name))
  }
  name <- sub(
    paste0(utils::packageName(env), "."),
    "",
    option_name,
    fixed = TRUE
  )
  getter_name <- paste0("get_", utils::packageName(env), "_option_", name)
  local_setter_name <- paste0(
    "local_",
    utils::packageName(env),
    "_option_",
    name
  )
  setter_name <- paste0("set_", utils::packageName(env), "_option_", name)

  local_setter_body <- expr({
    out <- !!call2(
      "local_options",
      !!option_name := sym("value"),
      .frame = sym("env")
    )
    !!call2(getter_name)
    invisible(out[[1]])
  })

  setter_body <- expr({
    out <- !!call2("options", !!option_name := sym("value"))
    !!call2(getter_name)
    invisible(out[[1]])
  })

  body <- expr({
    if (missing(!!sym("value"))) {
      if (!missing(local)) {
        abort("Can't pass `local` argument if `value` is missing.")
      }
      !!getter_body
    } else if (local) {
      !!local_setter_body
    } else {
      !!setter_body
    }
  })

  args <- pairlist2(value = , local = FALSE, env = quote(caller_env()))

  assign(getter_name, new_function(list(), getter_body, env = env), env)
  assign(
    local_setter_name,
    new_function(args[c(1, 3)], local_setter_body, env = env),
    env
  )
  assign(setter_name, new_function(args[1], setter_body, env = env), env)

  new_function(args, body, env = env)
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/R/aaa-r-version.R ---
# Detect mismatched R versions and clean object files if needed
#
# Loading a package compiled with a different R version is a bad idea.
# Switching R versions is useful: LLDB debugging only works with non-notarized
# versions of R (e.g. R-devel) on macOS, binary packages (e.g. arrow)
# are available only for stable versions.
# This code cleans up if a mismatch in R versions is detected,
# via a hidden src/version.txt file.
#
# This code is run directly, not in a function, to support both R CMD INSTALL
# and pkgload::load_all().
# Running this in .onLoad() is too late.
#
# This file is sourced manually in ./configure,
# and also sourced when the package is loaded.
# - For a package installed via R CMD build, nothing happens,
#   because this file is part of .Rbuildignore .
# - For a package installed via R CMD INSTALL, the file is sourced manually
#   in ./configure (which is always run) and does the right thing
# - For a package loaded via pkgload::load_all(), ./configure isn't always run.
#   In this case, this code serves as an emergency brake to clean up and fail,
#   so that the next attempt has a chance to succeed.
local({
  is_configure <- is.null(utils::packageName())

  if (is_configure) {
    pkg_path <- "src"
  } else {
    pkg_path <- system.file("src", package = utils::packageName())
  }

  if (dir.exists(pkg_path)) {
    version_file <- file.path(pkg_path, "version.txt")
    if (file.exists(version_file)) {
      binary_version <- readLines(version_file)[[1]]
    } else {
      binary_version <- "<unknown>"
    }
    if (R.version.string != binary_version) {
      files <- dir(pkg_path, pattern = "[.]o$|[.]so$", full.names = TRUE)
      unlink(files)
      writeLines(R.version.string, version_file)

      if (!is_configure) {
        stop(
          "Package was previously compiled with ",
          binary_version,
          ", ",
          "current is ",
          R.version.string,
          ". ",
          "Object files deleted, please try again.",
          call. = FALSE
        )
      }
    }
  }
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/R/add.R ---
#' Add rows to a data frame
#'
#' @description
#' This is a convenient way to add one or more rows of data to an existing data
#' frame. See [tribble()] for an easy way to create an complete
#' data frame row-by-row. Use [tibble_row()] to ensure that the new data
#' has only one row.
#'
#' `add_case()` is an alias of `add_row()`.
#'
#' @param .data Data frame to append to.
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]>
#'   Name-value pairs, passed on to [tibble()]. Values can be defined
#'   only for columns that already exist in `.data` and unset columns will get an
#'   `NA` value.
#' @param .before,.after One-based row index where to add the new rows,
#'   default: after last row.
#' @family addition
#' @examples
#' # add_row ---------------------------------
#' df <- tibble(x = 1:3, y = 3:1)
#'
#' df %>% add_row(x = 4, y = 0)
#'
#' # You can specify where to add the new rows
#' df %>% add_row(x = 4, y = 0, .before = 2)
#'
#' # You can supply vectors, to add multiple rows (this isn't
#' # recommended because it's a bit hard to read)
#' df %>% add_row(x = 4:5, y = 0:-1)
#'
#' # Use tibble_row() to add one row only
#' df %>% add_row(tibble_row(x = 4, y = 0))
#' try(df %>% add_row(tibble_row(x = 4:5, y = 0:-1)))
#'
#' # Absent variables get missing values
#' df %>% add_row(x = 4)
#'
#' # You can't create new variables
#' try(df %>% add_row(z = 10))
#' @export
add_row <- function(.data, ..., .before = NULL, .after = NULL) {
  if (inherits(.data, "grouped_df")) {
    abort_add_rows_to_grouped_df()
  }

  if (!is.data.frame(.data)) {
    deprecate_stop("2.1.1", "add_row(.data = 'must be a data frame')")
  }

  if (dots_n(...) == 0L) {
    # A single row of missing values is added if no input is supplied
    df <- new_tibble(list(), nrow = 1L)
  } else {
    df <- tibble(...)
  }

  extra_vars <- setdiff(names(df), names(.data))
  if (has_length(extra_vars)) {
    abort_incompatible_new_rows(extra_vars)
  }

  pos <- pos_from_before_after(.before, .after, nrow(.data))
  out <- rbind_at(.data, df, pos)

  vectbl_restore(out, .data)
}

#' @export
#' @rdname add_row
#' @usage NULL
add_case <- add_row

na_value <- function(boilerplate) {
  if (is.list(boilerplate)) {
    list(NULL)
  } else {
    NA
  }
}

rbind_at <- function(old, new, pos) {
  out <- vec_rbind(old, new)

  # Append at end: Nothing more to do.
  if (pos >= nrow(old)) {
    return(out)
  }

  # Splice: Construct index vector
  pos <- max(pos, 0L)
  idx <- c(
    seq2(1L, pos),
    seq2(nrow(old) + 1L, nrow(old) + nrow(new)),
    seq2(pos + 1L, nrow(old))
  )
  vec_slice(out, idx)
}

#' Add columns to a data frame
#'
#' This is a convenient way to add one or more columns to an existing data
#' frame.
#'
#' @param .data Data frame to append to.
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]>
#'   Name-value pairs, passed on to [tibble()]. All values must have
#'   the same size of `.data` or size 1.
#' @param .before,.after One-based column index or column name where to add the
#'   new columns, default: after last column.
#' @inheritParams tibble
#' @family addition
#' @examples
#' # add_column ---------------------------------
#' df <- tibble(x = 1:3, y = 3:1)
#'
#' df %>% add_column(z = -1:1, w = 0)
#' df %>% add_column(z = -1:1, .before = "y")
#'
#' # You can't overwrite existing columns
#' try(df %>% add_column(x = 4:6))
#'
#' # You can't create new observations
#' try(df %>% add_column(z = 1:5))
#'
#' @export
add_column <- function(
  .data,
  ...,
  .before = NULL,
  .after = NULL,
  .name_repair = c(
    "check_unique",
    "unique",
    "universal",
    "minimal",
    "unique_quiet",
    "universal_quiet"
  )
) {
  if (!is.data.frame(.data)) {
    deprecate_stop("2.1.1", "add_column(.data = 'must be a data frame')")
  }

  if (
    has_length(.data) &&
      (!is_named(.data) || anyDuplicated(names2(.data))) &&
      missing(.name_repair)
  ) {
    deprecate_stop(
      "3.0.0",
      "add_column(.data = 'must have unique names')",
      details = 'Use `.name_repair = "minimal"`.'
    )
  }

  df <- tibble(..., .name_repair = .name_repair)

  if (ncol(df) == 0L) {
    return(.data)
  }

  if (nrow(df) != nrow(.data)) {
    if (nrow(df) == 1) {
      df <- df[rep(1L, nrow(.data)), ]
    } else {
      abort_incompatible_new_cols(nrow(.data), df)
    }
  }

  pos <- pos_from_before_after_names(.before, .after, colnames(.data))

  end_pos <- ncol(.data) + seq_len(ncol(df))

  indexes_before <- rlang::seq2(1L, pos)
  indexes_after <- rlang::seq2(pos + 1L, ncol(.data))
  indexes <- c(indexes_before, end_pos, indexes_after)

  new_data <- .data
  new_data[end_pos] <- df

  out <- new_data[indexes]

  out <- set_repaired_names(out, repair_hint = TRUE, .name_repair)
  vectbl_restore(out, .data)
}


# helpers -----------------------------------------------------------------

pos_from_before_after_names <- function(
  before,
  after,
  names,
  call = caller_env()
) {
  before <- check_names_before_after(before, names)
  after <- check_names_before_after(after, names)

  pos_from_before_after(before, after, length(names), call)
}

pos_from_before_after <- function(before, after, len, call = caller_env()) {
  if (is.null(before)) {
    if (is.null(after)) {
      len
    } else {
      limit_pos_range(after, len)
    }
  } else {
    if (is.null(after)) {
      limit_pos_range(before - 1L, len)
    } else {
      abort_both_before_after(call)
    }
  }
}

limit_pos_range <- function(pos, len) {
  max(0L, min(len, pos))
}

# check_names_before_after ------------------------------------------------

check_names_before_after <- function(j, x) {
  if (!is_bare_character(j)) {
    return(j)
  }

  check_needs_no_dim(j)
  check_names_before_after_character(j, x)
}

check_needs_no_dim <- function(j) {
  if (needs_dim(j)) {
    abort_dim_column_index(j)
  }
}

check_names_before_after_character <- function(j, names) {
  pos <- safe_match(j, names)
  if (anyNA(pos)) {
    unknown_names <- j[is.na(pos)]
    abort_unknown_column_names(unknown_names)
  }
  pos
}

# Errors ------------------------------------------------------------------

msg_unknown_column_names <- function(names) {
  pluralise_commas("Can't find column(s) ", tick(names), " in `.data`.")
}

abort_add_rows_to_grouped_df <- function(call = caller_env()) {
  tibble_abort(call = call, "Can't add rows to grouped data frames.")
}

abort_incompatible_new_rows <- function(names, call = caller_env()) {
  tibble_abort(
    call = call,
    problems(
      "New rows can't add columns:",
      msg_unknown_column_names(names)
    ),
    names = names
  )
}

abort_both_before_after <- function(call = caller_env()) {
  tibble_abort(call = call, "Can't specify both `.before` and `.after`.")
}

abort_unknown_column_names <- function(j, parent = NULL, call = caller_env()) {
  tibble_abort(
    call = call,
    pluralise_commas("Can't find column(s) ", tick(j), " in `.data`."),
    j = j,
    parent = parent
  )
}

abort_incompatible_new_cols <- function(n, df, call = caller_env()) {
  tibble_abort(
    call = call,
    bullets(
      "New columns must be compatible with `.data`:",
      x = paste0(
        pluralise_n("New column(s) ha[s](ve)", ncol(df)),
        " ",
        nrow(df),
        " rows"
      ),
      i = pluralise_count("`.data` has ", n, " row(s)")
    ),
    expected = n,
    actual = nrow(df)
  )
}

abort_dim_column_index <- function(j, call = caller_env()) {
  # friendly_type_of() doesn't distinguish between matrices and arrays
  tibble_abort(
    call = call,
    paste0(
      "Must use a vector in `[`, not an object of class ",
      class(j)[[1]],
      "."
    )
  )
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/R/ansi.R ---
# nocov start - https://github.com/tidyverse/tibble/blob/main/R/ansi.R
set_fansi_hooks <- function() {
  knitr::opts_chunk$set(collapse = TRUE)

  if (Sys.getenv("IN_GALLEY") != "") {
    knitr::opts_chunk$set(comment = "#>")
    return()
  }

  knitr::opts_chunk$set(comment = pillar::style_subtle("#>"))

  options(cli.num_colors = 256)
  options(pillar.bold = TRUE)

  knitr::knit_hooks$set(
    output = colourise_chunk("output"),
    message = colourise_chunk("message"),
    warning = colourise_chunk("warning"),
    error = colourise_chunk("error")
  )

  invisible()
}

colourise_chunk <- function(type) {
  function(x, options) {
    # lines <- strsplit(x, "\\n")[[1]]
    lines <- x
    if (type != "output") {
      lines <- cli::col_red(lines)
    }
    paste0(
      '<div class="sourceCode"><pre class="sourceCode"><code class="sourceCode">',
      paste0(
        cli::ansi_html(htmltools::htmlEscape(lines)),
        collapse = "\n"
      ),
      "</code></pre></div>"
    )
  }
}
# nocov end


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/R/as_tibble.R ---
#' Coerce lists, matrices, and more to data frames
#'
#' @description
#' `as_tibble()` turns an existing object, such as a data frame or
#' matrix, into a so-called tibble, a data frame with class [`tbl_df`]. This is
#' in contrast with [tibble()], which builds a tibble from individual columns.
#' `as_tibble()` is to [`tibble()`] as [base::as.data.frame()] is to
#' [base::data.frame()].
#'
#' `as_tibble()` is an S3 generic, with methods for:
#' * [`data.frame`][base::data.frame()]: Thin wrapper around the `list` method
#'   that implements tibble's treatment of [rownames].
#' * [`matrix`][base::matrix()], [`poly`][stats::poly()],
#'   [`ts`][stats::ts()], [`table`][base::table()]
#' * Default: Other inputs are first coerced with [base::as.data.frame()].
#'
#' @section Row names:
#' The default behavior is to silently remove row names.
#'
#' New code should explicitly convert row names to a new column using the
#' `rownames` argument.
#'
#' For existing code that relies on the retention of row names, call
#' `pkgconfig::set_config("tibble::rownames" = NA)` in your script or in your
#' package's [.onLoad()]  function.
#'
#' @section Life cycle:
#' Using `as_tibble()` for vectors is superseded as of version 3.0.0,
#' prefer the more expressive `as_tibble_row()` and
#' `as_tibble_col()` variants for new code.
#'
#' @seealso [tibble()] constructs a tibble from individual columns. [enframe()]
#'   converts a named vector to a tibble with a column of names and column of
#'   values. Name repair is implemented using [vctrs::vec_as_names()].
#'
#' @param x A data frame, list, matrix, or other object that could reasonably be
#'   coerced to a tibble.
#' @param ... Unused, for extensibility.
#' @inheritParams tibble
#' @param rownames How to treat existing row names of a data frame or matrix:
#'   * `NULL`: remove row names. This is the default.
#'   * `NA`: keep row names.
#'   * A string: the name of a new column. Existing rownames are transferred
#'     into this column and the `row.names` attribute is deleted.
#'     No name repair is applied to the new column name, even if `x` already contains
#'     a column of that name.
#'     Use `as_tibble(rownames_to_column(...))` to safeguard against this case.
#'
#'  Read more in [rownames].

#' @param _n,validate
#'   `r lifecycle::badge("soft-deprecated")`
#'
#'   For compatibility only, do not use for new code.
#' @export
#' @examples
#' m <- matrix(rnorm(50), ncol = 5)
#' colnames(m) <- c("a", "b", "c", "d", "e")
#' df <- as_tibble(m)
as_tibble <- function(
  x,
  ...,
  .rows = NULL,
  .name_repair = c(
    "check_unique",
    "unique",
    "universal",
    "minimal",
    "unique_quiet",
    "universal_quiet"
  ),
  rownames = pkgconfig::get_config("tibble::rownames", NULL)
) {
  UseMethod("as_tibble")
}

#' @export
#' @rdname as_tibble
as_tibble.data.frame <- function(
  x,
  validate = NULL,
  ...,
  .rows = NULL,
  .name_repair = c(
    "check_unique",
    "unique",
    "universal",
    "minimal",
    "unique_quiet",
    "universal_quiet"
  ),
  rownames = pkgconfig::get_config("tibble::rownames", NULL)
) {
  if (!is.null(validate)) {
    deprecate_stop(
      "2.0.0",
      "tibble::as_tibble(validate = )",
      "as_tibble(.name_repair =)"
    )
  }

  if (!identical(class(x), "data.frame") && !inherits(x, "tbl_df")) {
    x <- as.data.frame(x)
  }

  old_rownames <- raw_rownames(x)
  if (is.null(.rows)) {
    .rows <- nrow(x)
  }

  result <- lst_to_tibble(unclass(x), .rows, .name_repair)

  if (is.null(rownames)) {
    result
  } else if (is.na(rownames)) {
    attr(result, "row.names") <- old_rownames
    result
  } else {
    if (length(old_rownames) > 0 && is.na(old_rownames[1L])) {
      # if implicit rownames
      old_rownames <- seq_len(abs(old_rownames[2L]))
    }
    old_rownames <- as.character(old_rownames)
    add_column(
      result,
      !!rownames := old_rownames,
      .before = 1L,
      .name_repair = "minimal"
    )
  }
}

#' @export
#' @rdname as_tibble
as_tibble.list <- function(
  x,
  validate = NULL,
  ...,
  .rows = NULL,
  .name_repair = c(
    "check_unique",
    "unique",
    "universal",
    "minimal",
    "unique_quiet",
    "universal_quiet"
  )
) {
  if (!is.null(validate)) {
    deprecate_stop(
      "2.0.0",
      "tibble::as_tibble(validate = )",
      "as_tibble(.name_repair =)"
    )
  }

  lst_to_tibble(x, .rows, .name_repair, col_lengths(x))
}

lst_to_tibble <- function(
  x,
  .rows,
  .name_repair,
  lengths = NULL,
  call = caller_env()
) {
  x <- unclass(x)
  x <- set_repaired_names(x, repair_hint = TRUE, .name_repair, call = call)
  x <- check_valid_cols(x, call = call)
  x <- recycle_columns(x, .rows, lengths)
  x
}

check_valid_cols <- function(x, pos = NULL, call = caller_env()) {
  names_x <- names2(x)

  is_xd <- which(!map_lgl(x, is_valid_col))
  if (has_length(is_xd)) {
    classes <- map_chr(x[is_xd], friendly_type_of)
    abort_column_scalar_type(names_x[is_xd], pos[is_xd], classes, call)
  }

  # 657
  x[] <- map(x, make_valid_col)
  invisible(x)
}

make_valid_col <- function(x) {
  if (is.expression(x)) {
    x <- as.list(x)
  }
  x
}

is_valid_col <- function(x) {
  # 657
  vec_is(x) || is.expression(x)
}

recycle_columns <- function(x, .rows, lengths) {
  nrow <- guess_nrow(lengths, .rows)

  # Shortcut if all columns have the requested or implied length
  different_len <- which(lengths != nrow)
  if (is_empty(different_len)) {
    return(new_tibble(x, nrow = nrow, subclass = NULL))
  }

  if (any(lengths[different_len] != 1)) {
    abort_incompatible_size(
      .rows,
      names(x),
      lengths,
      "Requested with `.rows` argument"
    )
  }

  if (nrow != 1L) {
    short <- which(lengths == 1L)
    if (has_length(short)) {
      x[short] <- map(x[short], vec_recycle, nrow)
    }
  }

  new_tibble(x, nrow = nrow, subclass = NULL)
}

guess_nrow <- function(lengths, .rows) {
  if (!is.null(.rows)) {
    return(.rows)
  }
  if (is_empty(lengths)) {
    return(0)
  }

  nontrivial_lengths <- lengths[lengths != 1L]
  if (is_empty(nontrivial_lengths)) {
    return(1)
  }

  max(nontrivial_lengths)
}

#' @export
#' @rdname as_tibble
as_tibble.matrix <- function(x, ..., validate = NULL, .name_repair = NULL) {
  m <- matrixToDataFrame(x)
  names <- colnames(x)
  if (is.null(.name_repair)) {
    if (
      (is.null(names) || any(bad_names <- duplicated(names) | names == "")) &&
        has_length(x)
    ) {
      deprecate_warn(
        "2.0.0",
        "as_tibble.matrix(x = 'must have unique column names if `.name_repair` is omitted')",
        details = "Using compatibility `.name_repair`."
      )
      compat_names <- paste0("V", seq_along(m))
      if (is.null(names)) {
        names <- compat_names
      } else {
        names[bad_names] <- compat_names[bad_names]
      }
      .name_repair <- function(x) names
    } else {
      .name_repair <- "check_unique"
    }
    validate <- NULL
  }

  colnames(m) <- names
  as_tibble(m, ..., validate = validate, .name_repair = .name_repair)
}

#' @export
as_tibble.poly <- function(x, ...) {
  m <- matrixToDataFrame(unclass(x))
  colnames(m) <- colnames(x)
  as_tibble(m, ...)
}

#' @export
as_tibble.ts <- function(x, ..., .name_repair = "minimal") {
  df <- as.data.frame(x)
  if (length(dim(x)) == 2) {
    colnames(df) <- colnames(x)
  }
  as_tibble(df, ..., .name_repair = .name_repair)
}

#' @export
#' @param n Name for count column, default: `"n"`.
#' @rdname as_tibble
as_tibble.table <- function(
  x,
  `_n` = "n",
  ...,
  n = `_n`,
  .name_repair = "check_unique"
) {
  if (!missing(`_n`)) {
    warn("Please pass `n` as a named argument to `as_tibble.table()`.")
  }

  df <- as.data.frame(x, stringsAsFactors = FALSE)

  names(df) <- repaired_names(
    c(names2(dimnames(x)), n),
    repair_hint = TRUE,
    .name_repair = .name_repair
  )

  # Names already repaired:
  as_tibble(df, ..., .name_repair = "minimal")
}

#' @export
#' @rdname as_tibble
as_tibble.NULL <- function(x, ...) {
  if (missing(x)) {
    deprecate_stop("3.0.0", "as_tibble(x = 'can\\'t be missing')")
  }

  new_tibble(list(), nrow = 0)
}

#' @export
#' @rdname as_tibble
as_tibble.default <- function(x, ...) {
  value <- x
  if (is_atomic(value)) {
    signal_superseded(
      "3.0.0",
      "as_tibble(x = 'can\\'t be an atomic vector')",
      "as_tibble_col()"
    )
  }
  as_tibble(as.data.frame(value, stringsAsFactors = FALSE), ...)
}

#' @description
#' `as_tibble_row()` converts a vector to a tibble with one row.
#' If the input is a list, all elements must have size one.
#'
#' @rdname as_tibble
#' @export
#' @examples
#'
#' as_tibble_row(c(a = 1, b = 2))
#' as_tibble_row(list(c = "three", d = list(4:5)))
#' as_tibble_row(1:3, .name_repair = "unique")
as_tibble_row <- function(
  x,
  .name_repair = c(
    "check_unique",
    "unique",
    "universal",
    "minimal",
    "unique_quiet",
    "universal_quiet"
  )
) {
  if (!vec_is(x)) {
    abort_as_tibble_row_vector(x)
  }

  names <- vectbl_names2(x, .name_repair = .name_repair)

  # FIXME: Use vec_chop2() when https://github.com/r-lib/vctrs/pull/1226 is in
  if (is_bare_list(x)) {
    slices <- x
  } else {
    x <- vectbl_set_names(x, NULL)
    slices <- lapply(seq_len(vec_size(x)), vec_slice, x = x)
    names(slices) <- names
  }

  check_all_lengths_one(slices)

  new_tibble(slices, nrow = 1)
}

check_all_lengths_one <- function(x, call = caller_env()) {
  sizes <- col_lengths(x)

  bad_lengths <- which(sizes != 1)
  if (!is_empty(bad_lengths)) {
    abort_as_tibble_row_size_one(
      seq_along(x)[bad_lengths],
      names2(x)[bad_lengths],
      sizes[bad_lengths],
      call
    )
  }
}


#' @description
#' `as_tibble_col()` converts a vector to a tibble with one column.
#'
#' @param column_name Name of the column.
#'
#' @rdname as_tibble
#' @export
#' @examples
#'
#' as_tibble_col(1:3)
#' as_tibble_col(
#'   list(c = "three", d = list(4:5)),
#'   column_name = "data"
#' )
as_tibble_col <- function(x, column_name = "value") {
  # Side effect: checking that x is a vector
  tibble(!!column_name := x)
}

# External ----------------------------------------------------------------

matrixToDataFrame <- function(x) {
  .Call(`tibble_matrixToDataFrame`, x)
}

# Errors ------------------------------------------------------------------

abort_column_scalar_type <- function(
  names,
  positions,
  classes,
  call = caller_env()
) {
  tibble_abort(
    call = call,
    problems(
      "All columns in a tibble must be vectors:",
      x = paste0("Column ", name_or_pos(names, positions), " is ", classes)
    ),
    names = names
  )
}

abort_as_tibble_row_vector <- function(x, call = caller_env()) {
  tibble_abort(
    call = call,
    paste0(
      "`x` must be a vector in `as_tibble_row()`, not ",
      class(x)[[1]],
      "."
    )
  )
}

abort_as_tibble_row_size_one <- function(j, name, size, call = caller_env()) {
  desc <- tick(name)
  desc[name == ""] <- paste0("at position ", j[name == ""])

  tibble_abort(
    call = call,
    problems(
      "All elements must be size one, use `list()` to wrap.",
      paste0("Element ", desc, " is of size ", size, ".")
    )
  )
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/R/compat-friendly-type.R ---
# nocov start --- r-lib/rlang compat-friendly-type --- 2019-09-09 Mon 11:50

friendly_type_of <- function(x, length = FALSE) {
  if (is.object(x)) {
    return(sprintf("a `%s` object", paste(class(x), collapse = "/")))
  }

  friendly <- as_friendly_type(typeof(x))

  if (length && rlang::is_vector(x)) {
    friendly <- paste0(friendly, sprintf(" of length %s", length(x)))
  }

  friendly
}

as_friendly_type <- function(type) {
  switch(
    type,
    logical = "a logical vector",
    integer = "an integer vector",
    numeric = ,
    double = "a double vector",
    complex = "a complex vector",
    character = "a character vector",
    raw = "a raw vector",
    string = "a string",
    list = "a list",
    NULL = "NULL",
    environment = "an environment",
    externalptr = "a pointer",
    weakref = "a weak reference",
    S4 = "an S4 object",
    name = ,
    symbol = "a symbol",
    language = "a call",
    pairlist = "a pairlist node",
    expression = "an expression vector",
    quosure = "a quosure",
    formula = "a formula",
    char = "an internal string",
    promise = "an internal promise",
    ... = "an internal dots object",
    any = "an internal `any` object",
    bytecode = "an internal bytecode object",
    primitive = ,
    builtin = ,
    special = "a primitive function",
    closure = "a function",
    type
  )
}

# nocov end


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/R/compat-lazyeval.R ---
# nocov start - compat-lazyeval (last updated: rlang 0.3.0)

# This file serves as a reference for compatibility functions for lazyeval.
# Please find the most recent version in rlang's repository.

warn_underscored <- function() {
  return(NULL)
  warn(paste(
    "The underscored versions are deprecated in favour of",
    "tidy evaluation idioms. Please see the documentation",
    "for `quo()` in rlang"
  ))
}
warn_text_se <- function() {
  return(NULL)
  warn("Text parsing is deprecated, please supply an expression or formula")
}

compat_lazy <- function(lazy, env = caller_env(), warn = TRUE) {
  if (warn) {
    warn_underscored()
  }

  if (missing(lazy)) {
    return(quo())
  }
  if (is_quosure(lazy)) {
    return(lazy)
  }
  if (is_formula(lazy)) {
    return(as_quosure(lazy, env))
  }

  out <- switch(
    typeof(lazy),
    symbol = ,
    language = new_quosure(lazy, env),
    character = {
      if (warn) {
        warn_text_se()
      }
      parse_quo(lazy[[1]], env)
    },
    logical = ,
    integer = ,
    double = {
      if (length(lazy) > 1) {
        warn("Truncating vector to length 1")
        lazy <- lazy[[1]]
      }
      new_quosure(lazy, env)
    },
    list = if (inherits(lazy, "lazy")) {
      lazy = new_quosure(lazy$expr, lazy$env)
    }
  )

  if (is.null(out)) {
    abort_compat_lazy(lazy)
  } else {
    out
  }
}

abort_compat_lazy <- function(lazy, call = caller_env()) {
  tibble_abort(
    call = call,
    sprintf("Can't convert a %s to a quosure", typeof(lazy))
  )
}

compat_lazy_dots <- function(dots, env, ..., .named = FALSE) {
  if (missing(dots)) {
    dots <- list()
  }
  if (inherits(dots, c("lazy", "formula"))) {
    dots <- list(dots)
  } else {
    dots <- unclass(dots)
  }
  dots <- c(dots, list(...))

  warn <- TRUE
  for (i in seq_along(dots)) {
    dots[[i]] <- compat_lazy(dots[[i]], env, warn)
    warn <- FALSE
  }

  named <- have_name(dots)
  if (.named && any(!named)) {
    nms <- vapply(
      dots[!named],
      function(x) expr_text(get_expr(x)),
      character(1)
    )
    names(dots)[!named] <- nms
  }

  names(dots) <- names2(dots)
  dots
}

compat_as_lazy <- function(quo) {
  structure(
    class = "lazy",
    list(
      expr = get_expr(quo),
      env = get_env(quo)
    )
  )
}
compat_as_lazy_dots <- function(...) {
  structure(class = "lazy_dots", lapply(quos(...), compat_as_lazy))
}

# nocov end


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/R/compat-lifecycle.R ---
# nocov start - compat-lifecycle (last updated: rlang 0.3.0.9000)

scoped_lifecycle_silence <- function(frame = rlang::caller_env()) {
  rlang::local_options(
    .frame = frame,
    lifecycle_verbosity = "quiet"
  )
}
with_lifecycle_silence <- function(expr) {
  scoped_lifecycle_silence()
  expr
}

scoped_lifecycle_warnings <- function(frame = rlang::caller_env()) {
  rlang::local_options(
    .frame = frame,
    lifecycle_verbosity = "warning"
  )
}
with_lifecycle_warnings <- function(expr) {
  scoped_lifecycle_warnings()
  expr
}

scoped_lifecycle_errors <- function(frame = rlang::caller_env()) {
  rlang::local_options(
    .frame = frame,
    lifecycle_verbosity = "error"
  )
}
with_lifecycle_errors <- function(expr) {
  scoped_lifecycle_errors()
  expr
}

# Enable once signal_superseded() reaches stable state
signal_superseded <- function(...) {}

foreign_caller_env <- function(my_env = ns_env()) {
  for (n in 2:10) {
    caller <- caller_env(n)
    if (!is_reference(env_parent(caller), my_env)) {
      return(caller)
    }
  }

  # Safety net
  caller
}

# nocov end


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/R/compat-purrr.R ---
# nocov start - compat-purrr (last updated: rlang 0.3.2.9000)

# This file serves as a reference for compatibility functions for
# purrr. They are not drop-in replacements but allow a similar style
# of programming. This is useful in cases where purrr is too heavy a
# package to depend on. Please find the most recent version in rlang's
# repository.

map <- function(.x, .f, ...) {
  lapply(.x, .f, ...)
}
map_mold <- function(.x, .f, .mold, ...) {
  out <- vapply(.x, .f, .mold, ..., USE.NAMES = FALSE)
  names(out) <- names(.x)
  out
}
map_lgl <- function(.x, .f, ...) {
  map_mold(.x, .f, logical(1), ...)
}
map_int <- function(.x, .f, ...) {
  map_mold(.x, .f, integer(1), ...)
}
map_dbl <- function(.x, .f, ...) {
  map_mold(.x, .f, double(1), ...)
}
map_chr <- function(.x, .f, ...) {
  map_mold(.x, .f, character(1), ...)
}
map_cpl <- function(.x, .f, ...) {
  map_mold(.x, .f, complex(1), ...)
}

walk <- function(.x, .f, ...) {
  map(.x, .f, ...)
  invisible(.x)
}

pluck <- function(.x, .f) {
  map(.x, `[[`, .f)
}
pluck_lgl <- function(.x, .f) {
  map_lgl(.x, `[[`, .f)
}
pluck_int <- function(.x, .f) {
  map_int(.x, `[[`, .f)
}
pluck_dbl <- function(.x, .f) {
  map_dbl(.x, `[[`, .f)
}
pluck_chr <- function(.x, .f) {
  map_chr(.x, `[[`, .f)
}
pluck_cpl <- function(.x, .f) {
  map_cpl(.x, `[[`, .f)
}

map2 <- function(.x, .y, .f, ...) {
  out <- mapply(.f, .x, .y, MoreArgs = list(...), SIMPLIFY = FALSE)
  if (length(out) == length(.x)) {
    set_names(out, names(.x))
  } else {
    set_names(out, NULL)
  }
}
map2_lgl <- function(.x, .y, .f, ...) {
  as.vector(map2(.x, .y, .f, ...), "logical")
}
map2_int <- function(.x, .y, .f, ...) {
  as.vector(map2(.x, .y, .f, ...), "integer")
}
map2_dbl <- function(.x, .y, .f, ...) {
  as.vector(map2(.x, .y, .f, ...), "double")
}
map2_chr <- function(.x, .y, .f, ...) {
  as.vector(map2(.x, .y, .f, ...), "character")
}
map2_cpl <- function(.x, .y, .f, ...) {
  as.vector(map2(.x, .y, .f, ...), "complex")
}

args_recycle <- function(args) {
  lengths <- map_int(args, length)
  n <- max(lengths)

  stopifnot(all(lengths == 1L | lengths == n))
  to_recycle <- lengths == 1L
  args[to_recycle] <- map(args[to_recycle], function(x) rep.int(x, n))

  args
}
pmap <- function(.l, .f, ...) {
  args <- args_recycle(.l)
  do.call(
    "mapply",
    c(
      FUN = list(quote(.f)),
      args,
      MoreArgs = quote(list(...)),
      SIMPLIFY = FALSE,
      USE.NAMES = FALSE
    )
  )
}

probe <- function(.x, .p, ...) {
  if (is_logical(.p)) {
    stopifnot(length(.p) == length(.x))
    .p
  } else {
    map_lgl(.x, .p, ...)
  }
}

keep <- function(.x, .f, ...) {
  .x[probe(.x, .f, ...)]
}
discard <- function(.x, .p, ...) {
  sel <- probe(.x, .p, ...)
  .x[is.na(sel) | !sel]
}
map_if <- function(.x, .p, .f, ...) {
  matches <- probe(.x, .p)
  .x[matches] <- map(.x[matches], .f, ...)
  .x
}

compact <- function(.x) {
  Filter(length, .x)
}

transpose <- function(.l) {
  inner_names <- names(.l[[1]])
  if (is.null(inner_names)) {
    fields <- seq_along(.l[[1]])
  } else {
    fields <- set_names(inner_names)
  }

  map(fields, function(i) {
    map(.l, .subset2, i)
  })
}

every <- function(.x, .p, ...) {
  for (i in seq_along(.x)) {
    if (!rlang::is_true(.p(.x[[i]], ...))) return(FALSE)
  }
  TRUE
}
some <- function(.x, .p, ...) {
  for (i in seq_along(.x)) {
    if (rlang::is_true(.p(.x[[i]], ...))) return(TRUE)
  }
  FALSE
}
negate <- function(.p) {
  function(...) !.p(...)
}

reduce <- function(.x, .f, ..., .init) {
  f <- function(x, y) .f(x, y, ...)
  Reduce(f, .x, init = .init)
}
reduce_right <- function(.x, .f, ..., .init) {
  f <- function(x, y) .f(y, x, ...)
  Reduce(f, .x, init = .init, right = TRUE)
}
accumulate <- function(.x, .f, ..., .init) {
  f <- function(x, y) .f(x, y, ...)
  Reduce(f, .x, init = .init, accumulate = TRUE)
}
accumulate_right <- function(.x, .f, ..., .init) {
  f <- function(x, y) .f(y, x, ...)
  Reduce(f, .x, init = .init, right = TRUE, accumulate = TRUE)
}

detect <- function(.x, .f, ..., .right = FALSE, .p = is_true) {
  for (i in index(.x, .right)) {
    if (.p(.f(.x[[i]], ...))) {
      return(.x[[i]])
    }
  }
  NULL
}
detect_index <- function(.x, .f, ..., .right = FALSE, .p = is_true) {
  for (i in index(.x, .right)) {
    if (.p(.f(.x[[i]], ...))) {
      return(i)
    }
  }
  0L
}
index <- function(x, right = FALSE) {
  idx <- seq_along(x)
  if (right) {
    idx <- rev(idx)
  }
  idx
}

imap <- function(.x, .f, ...) {
  map2(.x, vecpurrr_index(.x), .f, ...)
}
vecpurrr_index <- function(x) {
  names(x) %||% seq_along(x)
}

# nocov end


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/R/compat-strrep.R ---
if (getRversion() < "3.3.0") {
  strrep <- function(x, times) {
    map_chr(
      times,
      function(n) paste(rep(x, n), collapse = "")
    )
  }
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/R/deprecated.R ---
#' Deprecated functions
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Use [tibble()] instead of `data_frame()`.
#'
#' @export
#' @keywords internal
#' @name deprecated
data_frame <- function(...) {
  deprecate_warn("1.1.0", "data_frame()", "tibble()")

  # Unquote-splice to avoid argument matching
  tibble(!!!quos(...))
}

#' @description
#' Use [quasiquotation] instead of `tibble_()`, `data_frame_()`, and `lst_()`.
#'
#' @export
#' @keywords internal
#' @rdname deprecated
tibble_ <- function(xs) {
  deprecate_soft(
    "2.0.0",
    "tibble_()",
    "tibble()",
    details = '`tibble()` supports dynamic dots, see `?"dyn-dots"`.'
  )

  xs <- compat_lazy_dots(xs, caller_env())
  tibble(!!!xs)
}

#' @export
#' @rdname deprecated
data_frame_ <- function(xs) {
  deprecate_stop(
    "2.0.0",
    "data_frame_()",
    "tibble()",
    details = '`tibble()` supports dynamic dots, see `?"dyn-dots"`.'
  )
}

#' @export
#' @rdname deprecated
lst_ <- function(xs) {
  deprecate_stop(
    "2.0.0",
    "lst_()",
    "lst()",
    details = '`lst()` supports dynamic dots, see `?"dyn-dots"`.'
  )
}

#' @description
#' Use [as_tibble()] instead of `as_data_frame()` or `as.tibble()`, but mind the
#' new signature and semantics.
#'
#' @export
#' @rdname deprecated
as_data_frame <- function(x, ...) {
  deprecate_warn(
    "2.0.0",
    "as_data_frame()",
    details = "Please use `as_tibble()` (with slightly different semantics) to convert to a tibble, or `as.data.frame()` to convert to a data frame."
  )

  as_tibble(x, ...)
}

#' @export
#' @rdname deprecated
as.tibble <- function(x, ...) {
  deprecate_warn(
    "2.0.0",
    "as.tibble()",
    "as_tibble()",
    details = "The signature and semantics have changed, see `?as_tibble`."
  )

  as_tibble(x, ...)
}

#' @description
#' Use [tribble()] instead of `frame_data()`.
#' @export
#' @rdname deprecated
frame_data <- function(...) {
  deprecate_stop("2.0.0", "frame_data()", "tribble()")
}

#' Name repair
#'
#' Please review [vctrs::vec_as_names()].
#'
#' @name name-repair
#' @keywords internal
NULL


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/R/dftbl.R ---
set_dftbl_hooks <- function() {
  width <- 36

  set_dftbl_opts_hook(width)
  set_dftbl_knit_hook(width)
  set_dftbl_source_hook()
  set_dftbl_chunk_hook()
}

# Defines a `dftbl` knitr option. If this chunk option is set, code is duplicated
# (running with tibbles or data frames, respectively): one line of df code
# followed by the same line of the corresponding tibble code.
# The code is also evaluated, if the results are identical (disregarding the
# class), only the tibble copy is retained.
#
set_dftbl_opts_hook <- function(width) {
  force(width)

  dftbl_opts_hook <- function(options) {
    df_code <- options$code
    tbl_code <- gsub("df", "tbl", df_code, fixed = TRUE)

    # FIXME: Evaluate, but surround in <details> element
    if (!isTRUE(options$dftbl_always) && isTRUE(options$eval)) {
      same <- map2_lgl(df_code, tbl_code, same_as_tbl_code)
      df_code[same] <- ""
    }

    new_code <- as.vector(t(matrix(c(df_code, tbl_code), ncol = 2)))
    options$code <- new_code
    options$width <- width - 4
    options
  }

  knitr::opts_hooks$set(dftbl = dftbl_opts_hook)
}

utils::globalVariables(c("new_df", "new_tbl"))

same_as_tbl_code <- function(df_code, tbl_code) {
  handler <- evaluate::new_output_handler(
    value = function(x, visible) {
      if (visible) x else NULL
    }
  )

  same_as_tbl(
    evaluate::evaluate(df_code, output_handler = handler),
    evaluate::evaluate(tbl_code, output_handler = handler)
  )
}

same_as_tbl <- function(df, tbl) {
  if (length(df) != length(tbl)) {
    return(FALSE)
  }
  if (length(df) < 2) {
    return(FALSE)
  }
  df <- df[-1]
  tbl <- tbl[-1]

  df_obj <- df[[length(df)]]
  tbl_obj <- tbl[[length(tbl)]]

  if (is.data.frame(df_obj) != is.data.frame(tbl_obj)) {
    return(FALSE)
  }

  if (is.data.frame(tbl_obj)) {
    df[[length(df)]] <- as_tibble_deep(df_obj)
  }

  identical(df, tbl)
}

as_tibble_deep <- function(x) {
  is_tibble <- which(map_lgl(x, is.data.frame))
  x[is_tibble] <- map(x[is_tibble], as_tibble)
  as_tibble(x)
}

# dftbl chunks have a reduced width
set_dftbl_knit_hook <- function(width) {
  force(width)

  # Need to use a closure here to keep state
  old_width <- NULL

  dftbl_knit_hook <- function(before, options, envir) {
    if (before) {
      old_width <<- options(width = width)
    } else {
      options(old_width)
      old_width <<- NULL
    }
  }

  knitr::knit_hooks$set(dftbl = dftbl_knit_hook)
}

# dftbl chunks are shown side by side, with the help of an HTML table.
# Each source chunk introduces a new table cell, even chunks also introduce
# a new table row.
# vertical-align: top keeps the table rows nicely aligned.
# This places some limitations on the chunk sources but works well so far.
set_dftbl_source_hook <- function() {
  # Need to use a closure here to daisy-chain hooks and to keep state

  old_source_hook <- knitr::knit_hooks$get("source")

  dftbl_source_even <- TRUE

  dftbl_source_hook_one <- function(x) {
    if (dftbl_source_even) {
      x <- paste0('</td></tr><tr style="vertical-align:top"><td>\n\n', x)
    } else {
      x <- paste0("</td><td>\n\n", x)
    }

    dftbl_source_even <<- !dftbl_source_even
    x
  }

  dftbl_source_hook <- function(x, options) {
    nonempty <- which(x != "")
    x[nonempty] <- vapply(
      x[nonempty],
      old_source_hook,
      options,
      FUN.VALUE = character(1)
    )
    if (isTRUE(options$dftbl)) {
      x <- vapply(x, dftbl_source_hook_one, FUN.VALUE = character(1))
    }
    paste(x, collapse = "\n")
  }

  knitr::knit_hooks$set(source = dftbl_source_hook)
}

# The entire chunk needs to be surrounded by <table><tbody><tr><td>...</...> .
# We use the dftbl CSS class for the HTML table.
set_dftbl_chunk_hook <- function() {
  # Need to use a closure here to daisy-chain hooks

  old_chunk_hook <- knitr::knit_hooks$get("chunk")

  dftbl_chunk_hook <- function(x, options) {
    x <- old_chunk_hook(x, options)
    if (isTRUE(options$dftbl)) {
      x <- paste0(
        '<table class="dftbl"><tbody><tr><td>\n\n',
        x,
        "\n\n</td></tr></tbody></table>"
      )
      x <- gsub("<tr><td>\n\n</td></tr>", "", x, fixed = TRUE)
    }
    x
  }

  knitr::knit_hooks$set(chunk = dftbl_chunk_hook)
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/R/enframe.R ---
#' Converting vectors to data frames, and vice versa
#'
#' @description
#' `enframe()` converts named atomic vectors or lists to one- or two-column
#' data frames.
#' For a list, the result will be a nested tibble with a column of type `list`.
#' For unnamed vectors, the natural sequence is used as name column.
#'
#' @param x A vector (for `enframe()`) or a data frame with one or two columns
#'   (for `deframe()`).
#' @param name,value Names of the columns that store the names and values.
#'   If `name` is `NULL`, a one-column tibble is returned; `value` cannot be `NULL`.
#'
#' @return For `enframe()`, a [tibble] with two columns (if `name` is not `NULL`, the default)
#'   or one column (otherwise).
#' @export
#'
#' @examples
#' enframe(1:3)
#' enframe(c(a = 5, b = 7))
#' enframe(list(one = 1, two = 2:3, three = 4:6))
enframe <- function(x, name = "name", value = "value") {
  if (is.null(value)) {
    abort_enframe_value_null()
  }

  if (is.null(x)) {
    x <- logical()
  }

  # FIXME: Enable again for data frames, add test
  if (!vec_is(x) || is.data.frame(x)) {
    abort_enframe_must_be_vector(x)
  }

  if (is.null(name)) {
    df <- list(vectbl_set_names(x))
  } else if (is.null(vec_names(x))) {
    df <- list(seq_len(vec_size(x)), x)
  } else {
    df <- list(vec_names2(x), vectbl_set_names(x))
  }

  names(df) <- c(name, value)
  new_tibble(df, nrow = vec_size(x))
}

#' @rdname enframe
#' @description
#' `deframe()` converts two-column data frames to a named vector or list,
#' using the first column as name and the second column as value.
#' If the input has only one column, an unnamed vector is returned.
#' @return For `deframe()`, a vector (named or unnamed).
#' @export
#' @examples
#' deframe(enframe(3:1))
#' deframe(tibble(a = 1:3))
#' deframe(tibble(a = as.list(1:3)))
deframe <- function(x) {
  if (length(x) == 1) {
    return(x[[1]])
  } else if (length(x) != 2) {
    warn("`x` must be a one- or two-column data frame in `deframe()`.")
  }

  value <- x[[2L]]
  name <- x[[1L]]
  vectbl_set_names(value, as.character(name))
}

abort_enframe_value_null <- function(call = caller_env()) {
  tibble_abort(call = call, "`value` can't be NULL.")
}

abort_enframe_must_be_vector <- function(x, call = caller_env()) {
  tibble_abort(
    call = call,
    paste0(
      "The `x` argument to `enframe()` must be a vector, not ",
      class(x)[[1]],
      "."
    )
  )
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/R/error.R ---
invalid_df <- function(problem, vars, extra = NULL, message = "Column(s)") {
  if (is.character(vars)) {
    vars <- tick(vars)
  }

  c(
    pluralise_commas(paste0(message, " "), vars, paste0(" ", problem, ".")),
    extra
  )
}

use_repair <- function(repair_hint) {
  if (repair_hint) "Use `.name_repair` to specify repair."
}

tibble_error_class <- function(class) {
  c(paste0("tibble_error_", class), "tibble_error")
}

# Errors get a class name derived from the name of the calling function
tibble_abort <- function(x, ..., call, parent = NULL) {
  abort_call <- sys.call(-1)
  fn_name <- as_name(abort_call[[1]])
  class <- tibble_error_class(gsub("^abort_", "", fn_name))

  abort(x, class, ..., call = call, parent = parent, use_cli_format = TRUE)
}

tibble_error <- function(x, ..., parent = NULL) {
  call <- sys.call(-1)
  fn_name <- as_name(call[[1]])
  class <- tibble_error_class(gsub("^error_", "", fn_name))
  error_cnd(class, ..., message = x, parent = parent, use_cli_format = TRUE)
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/R/galley.R ---
render_galley_ext <- function(
  input_path,
  pkg,
  installed,
  output_dir,
  output_file
) {
  # stopifnot(!installed)
  if (installed) {
    library(pkg, character.only = TRUE)
  } else {
    pkgload::load_all()
  }

  testthat::local_reproducible_output()

  Sys.time <- function() {
    structure(1627618285.45488, class = c("POSIXct", "POSIXt"), tzone = "UTC")
  }
  Sys.Date <- function() {
    structure(18838, class = "Date")
  }
  set.seed(20210730)
  Sys.setenv("IN_PKGDOWN" = "true", "IN_GALLEY" = "true")

  rmarkdown::render(
    input_path,
    output_dir = output_dir,
    output_file = output_file,
    run_pandoc = FALSE,
    output_format = rmarkdown::md_document(preserve_yaml = TRUE)
  )
}

galley_use_installed <- function() {
  grepl("[.]Rcheck$", basename(normalizePath("../..")))
}

render_galley <- function(name, md_name) {
  pkg <- utils::packageName()
  # FIXME: Hack!
  installed <- galley_use_installed()
  # stopifnot(!installed)

  input_path <- file.path("vignettes", name)

  # Need fixed file name for stability
  output_dir <- tempdir()
  output_file <- md_name

  out_text <- character()

  knit_path <- tryCatch(
    callr::r(
      render_galley_ext,
      args = list(
        input_path = input_path,
        pkg = pkg,
        installed = installed,
        output_dir = output_dir,
        output_file = output_file
      ),
      callback = function(x) {
        out_text <<- c(out_text, x)
      }
    ),
    error = function(e) {
      writeLines(c("", out_text, ""))
      stop(e)
      # rlang::abort(paste0("Error rendering ", name))
    }
  )

  path <- file.path(output_dir, output_file)
  full_knit_path <- file.path(dirname(input_path), knit_path)
  scrub_file(path, full_knit_path)
  unlink(full_knit_path)

  path
}

scrub_tempdir <- function(x) {
  stable_tmpdir <- "${TEMP}"

  tmpdir_rx <- utils::glob2rx(
    paste0("*", dirname(tempdir()), "*"),
    trim.head = TRUE
  )
  gsub(
    paste0("(/private)?", tmpdir_rx, "[/\\\\]+Rtmp[0-9a-zA-Z]+"),
    stable_tmpdir,
    x
  )
}

scrub <- function(x) {
  x <- gsub("[<]bytecode: 0x.*[>]", "<bytecode: 0x1ee4c0de>", x)
  x <- gsub("[<]environment: 0x.*[>]", "<environment: 0xdeadbeef>", x)

  x <- scrub_tempdir(x)

  paste0(x, "\n", collapse = "")
}

scrub_file <- function(path, in_path = path) {
  text <- brio::read_lines(in_path)
  brio::write_file(scrub(text), path)
}

test_galley <- function(name, variant = NULL) {
  testthat::skip_on_cran()
  testthat::skip_if("covr" %in% loadedNamespaces())

  rmd_name <- paste0(name, ".Rmd")
  md_name <- paste0(name, ".md")

  path <- render_galley(rmd_name, md_name)

  if (!is.null(variant)) {
    testthat::skip_if_not_installed("testthat", "3.1.1")
    testthat::expect_snapshot_file(
      path,
      name = md_name,
      compare = testthat::compare_file_text,
      variant = variant
    )
  } else {
    testthat::expect_snapshot_file(
      path,
      name = md_name,
      compare = testthat::compare_file_text
    )
  }

  # FIXME: Test generated files
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/R/glimpse.R ---
#' @export
pillar::glimpse


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/R/has-name.R ---
#' @export
rlang::has_name


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/R/lst.R ---
#' Build a list
#'
#' @description
#' `lst()` constructs a list, similar to [base::list()], but with some of the
#' same features as [tibble()]. `lst()` builds components sequentially. When
#' defining a component, you can refer to components created earlier in the
#' call. `lst()` also generates missing names automatically.
#'
#' See [rlang::list2()] for a simpler and faster alternative without tibble's
#' evaluation and auto-name semantics.
#'
#' @inheritParams tibble
#' @return A named list.
#' @export
#' @examples
#' # the value of n can be used immediately in the definition of x
#' lst(n = 5, x = runif(n))
#'
#' # missing names are constructed from user's input
#' lst(1:3, z = letters[4:6], runif(3))
#'
#' a <- 1:3
#' b <- letters[4:6]
#' lst(a, b)
#'
#' # pre-formed quoted expressions can be used with lst() and then
#' # unquoted (with !!) or unquoted and spliced (with !!!)
#' n1 <- 2
#' n2 <- 3
#' n_stuff <- quote(n1 + n2)
#' x_stuff <- quote(seq_len(n))
#' lst(!!!list(n = n_stuff, x = x_stuff))
#' lst(n = !!n_stuff, x = !!x_stuff)
#' lst(n = 4, x = !!x_stuff)
#' lst(!!!list(n = 2, x = x_stuff))
lst <- function(...) {
  xs <- quos(..., .named = TRUE)
  lst_quos(xs)
}

lst_quos <- function(xs) {
  # TODO:
  # - soft-deprecate lst()

  # Evaluate each column in turn
  col_names <- names2(xs)
  lengths <- rep_along(xs, 0L)

  output <- rep_named(rep_along(xs, ""), list(NULL))

  for (i in seq_along(xs)) {
    unique_output <- output[
      !duplicated(names(output)[seq_len(i)], fromLast = TRUE)
    ]
    res <- eval_tidy(xs[[i]], unique_output)
    if (!is.null(res)) {
      lengths[[i]] <- NROW(res)
      output[[i]] <- res
    }
    names(output)[[i]] <- col_names[[i]]
  }

  output
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/R/names.R ---
vectbl_names2 <- function(
  x,
  .name_repair = c(
    "check_unique",
    "unique",
    "universal",
    "minimal",
    "unique_quiet",
    "universal_quiet"
  ),
  quiet = FALSE,
  call = caller_env()
) {
  name <- vec_names2(x, repair = "minimal", quiet = quiet)
  repaired_names(
    name,
    repair_hint = TRUE,
    .name_repair = .name_repair,
    quiet = quiet,
    call = call
  )
}

set_repaired_names <- function(
  x,
  repair_hint,
  .name_repair = c(
    "check_unique",
    "unique",
    "universal",
    "minimal",
    "unique_quiet",
    "universal_quiet"
  ),
  quiet = FALSE,
  call = caller_env()
) {
  names <- repaired_names(
    names2(x),
    repair_hint,
    .name_repair = .name_repair,
    quiet = quiet,
    call = call
  )
  set_names(x, names)
}

repaired_names <- function(
  name,
  repair_hint,
  .name_repair = c(
    "check_unique",
    "unique",
    "universal",
    "minimal",
    "unique_quiet",
    "universal_quiet"
  ),
  quiet = FALSE,
  details = NULL,
  call = caller_env()
) {
  subclass_name_repair_errors(
    name = name,
    details = details,
    repair_hint = repair_hint,
    vec_as_names(
      name,
      repair = .name_repair,
      quiet = quiet || !is_character(.name_repair)
    ),
    call = call
  )
}

# Errors ------------------------------------------------------------------

abort_column_names_cannot_be_empty <- function(
  names,
  repair_hint,
  details = NULL,
  parent = NULL,
  call = caller_env()
) {
  tibble_abort(
    invalid_df("must be named", names, use_repair(repair_hint)),
    names = names,
    parent = parent,
    call = call
  )
}

abort_column_names_cannot_be_dot_dot <- function(
  names,
  repair_hint,
  parent = NULL,
  call = caller_env()
) {
  tibble_abort(
    invalid_df(
      "must not have names of the form ... or ..j",
      names,
      use_repair(repair_hint)
    ),
    names = names,
    parent = parent,
    call = call
  )
}

abort_column_names_must_be_unique <- function(
  names,
  repair_hint,
  parent = NULL,
  call = caller_env()
) {
  tibble_abort(
    invalid_df(
      "must not be duplicated",
      names,
      use_repair(repair_hint),
      message = "Column name(s)"
    ),
    names = names,
    parent = parent,
    call = call
  )
}

# Subclassing errors ------------------------------------------------------

subclass_name_repair_errors <- function(
  expr,
  name,
  details = NULL,
  repair_hint = FALSE,
  call
) {
  withCallingHandlers(
    expr,

    # FIXME: use cnd$names with vctrs >= 0.3.0
    vctrs_error_names_cannot_be_empty = function(cnd) {
      abort_column_names_cannot_be_empty(
        detect_empty_names(name),
        details = details,
        parent = cnd,
        repair_hint = repair_hint,
        call = call
      )
    },
    vctrs_error_names_cannot_be_dot_dot = function(cnd) {
      abort_column_names_cannot_be_dot_dot(
        detect_dot_dot(name),
        parent = cnd,
        repair_hint = repair_hint,
        call = call
      )
    },
    vctrs_error_names_must_be_unique = function(cnd) {
      abort_column_names_must_be_unique(
        detect_duplicates(name),
        parent = cnd,
        repair_hint = repair_hint,
        call = call
      )
    }
  )
}

# Anticipate vctrs 0.3.0 release: locations replaced by names
detect_empty_names <- function(names) {
  which(names == "")
}
detect_dot_dot <- function(names) {
  grep("^[.][.](?:[.]|[1-9][0-9]*)$", names)
}
detect_duplicates <- function(names) {
  names[which(duplicated(names))]
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/R/new.R ---
#' Tibble constructor and validator
#'
#' @description
#' Creates or validates a subclass of a tibble.
#' These function is mostly useful for package authors that implement subclasses
#' of a tibble, like \pkg{sf} or \pkg{tsibble}.
#'
#' `new_tibble()` creates a new object as a subclass of `tbl_df`, `tbl` and `data.frame`.
#' This function is optimized for performance, checks are reduced to a minimum.
#' See [vctrs::new_data_frame()] for details.
#'
#' @param x A tibble-like object.
#' @param ... Name-value pairs of additional attributes.
#' @param nrow The number of rows, inferred from `x` if omitted.
#' @param class Subclasses to assign to the new object, default: none.
#' @param subclass Deprecated, retained for compatibility. Please use the `class` argument.
#'
#' @seealso
#' [tibble()] and [as_tibble()] for ways to construct a tibble
#' with recycling of scalars and automatic name repair,
#' and [vctrs::df_list()] and [vctrs::new_data_frame()]
#' for lower-level implementations.
#'
#' @export
#' @examples
#' # The nrow argument can be omitted:
#' new_tibble(list(a = 1:3, b = 4:6))
#'
#' # Existing row.names attributes are ignored:
#' try(validate_tibble(new_tibble(trees, nrow = 3)))
#'
#' # The length of all columns must be compatible with the nrow argument:
#' try(validate_tibble(new_tibble(list(a = 1:3, b = 4:6), nrow = 2)))
new_tibble <- function(x, ..., nrow = NULL, class = NULL, subclass = NULL) {
  # For compatibility with tibble < 2.0.0
  if (is.null(class) && !is.null(subclass)) {
    deprecate_stop(
      "2.0.0",
      "tibble::new_tibble(subclass = )",
      "new_tibble(class = )"
    )
  }

  #' @section Construction:
  #'
  #' For `new_tibble()`, `x` must be a list.
  x <- unclass(x)

  if (!is.list(x)) {
    abort_new_tibble_must_be_list()
  }

  #' The `nrow` argument may be omitted as of tibble 3.1.4.
  #' If present, every element of the list `x` should have [vctrs::vec_size()]
  #' equal to this value.
  #' (But this is not checked by the constructor).
  #' This takes the place of the "row.names" attribute in a data frame.
  if (!is.null(nrow)) {
    if (
      !is.numeric(nrow) ||
        length(nrow) != 1 ||
        nrow < 0 ||
        !is_integerish(nrow, 1) ||
        nrow >= 2147483648
    ) {
      abort_new_tibble_nrow_must_be_nonnegative()
    }
    nrow <- as.integer(nrow)
  }

  args <- attributes(x)

  if (is.null(args)) {
    args <- list()
  }

  new_attrs <- pairlist2(...)
  nms <- names(new_attrs)

  for (i in seq_along(nms)) {
    nm <- nms[[i]]

    if (nm == "") {
      next
    }

    args[[nm]] <- new_attrs[[i]]
  }

  #' `x` must have names (or be empty),
  #' but the names are not checked for correctness.
  if (length(x) == 0) {
    # Leaving this because creating a named list of length zero seems difficult
    args[["names"]] <- character()
  } else if (is.null(args[["names"]])) {
    abort_names_must_be_non_null()
  }

  if (is.null(class)) {
    class <- tibble_class_no_data_frame
  } else {
    class <- c(class[!class %in% tibble_class], tibble_class_no_data_frame)
  }

  # `new_data_frame()` restores compact row names
  # Can't add to the assignment above, a literal NULL would be inserted otherwise
  args[["row.names"]] <- NULL

  # Attributes n and x are special and must be assigned after construction
  an <- args[["n"]]
  ax <- args[["x"]]
  args[["n"]] <- NULL
  args[["x"]] <- NULL

  # need exec() to avoid evaluating language attributes (e.g. rsample)
  out <- exec(new_data_frame, x = x, n = nrow, !!!args, class = class)

  if (!is.null(an)) {
    attr(out, "n") <- an
  }

  if (!is.null(ax)) {
    attr(out, "x") <- ax
  }

  out
}

#' @description
#' `validate_tibble()` checks a tibble for internal consistency.
#' Correct behavior can be guaranteed only if this function
#' runs without raising an error.
#'
#' @rdname new_tibble
#' @export
validate_tibble <- function(x) {
  #' @section Validation:
  #' `validate_tibble()` checks for "minimal" names
  check_minimal_names(x)

  #' and that all columns are vectors, data frames or matrices.
  check_valid_cols(unclass(x))

  #' It also makes sure that all columns have the same length,
  #' and that [vctrs::vec_size()] is consistent with the data.
  validate_nrow(names(x), col_lengths(x), vec_size(x))

  x
}

check_minimal_names <- function(x) {
  names <- names(x)

  if (is.null(names)) {
    abort_names_must_be_non_null()
  }

  if (anyNA(names)) {
    abort_column_names_cannot_be_empty(which(is.na(names)), repair_hint = FALSE)
  }

  invisible(x)
}

col_lengths <- function(x) {
  map_int(x, vec_size)
}

validate_nrow <- function(names, lengths, nrow) {
  # Validate column lengths, don't recycle
  bad_len <- which(lengths != nrow)
  if (has_length(bad_len)) {
    abort_incompatible_size(
      nrow,
      names,
      lengths,
      "Requested with `nrow` argument"
    )
  }
}

tibble_class <- c("tbl_df", "tbl", "data.frame")
tibble_class_no_data_frame <- c("tbl_df", "tbl")

# Errors ------------------------------------------------------------------

abort_new_tibble_must_be_list <- function(call = caller_env()) {
  tibble_abort(call = call, "`x` must be a list.")
}

abort_new_tibble_nrow_must_be_nonnegative <- function(call = caller_env()) {
  tibble_abort(
    call = call,
    "`nrow` must be a nonnegative whole number smaller than 2^31."
  )
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/R/options-tibble.R ---
## Legacy, for trunc_mat() and friends
op.tibble <- list(
  tibble.print_min = 10L,
  tibble.print_max = 20L,
  tibble.width = NULL,
  tibble.max_extra_cols = 100L
)

tibble_opt <- function(x, dplyr = TRUE) {
  x_tibble <- paste0("tibble.", x)
  res <- getOption(x_tibble)
  if (!is.null(res)) {
    return(res)
  }

  if (dplyr) {
    x_dplyr <- paste0("dplyr.", x)
    res <- getOption(x_dplyr)
    if (!is.null(res)) {
      return(res)
    }
  }

  op.tibble[[x_tibble]]
}

tibble_width <- function(width) {
  if (!is.null(width)) {
    return(width)
  }

  width <- tibble_opt("width")
  if (!is.null(width)) {
    return(width)
  }

  getOption("width")
}

tibble_glimpse_width <- function(width) {
  if (!is.null(width)) {
    return(width)
  }

  width <- tibble_opt("width")
  if (!is.null(width) && is.finite(width)) {
    return(width)
  }

  getOption("width")
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/R/options.R ---
#' Package options
#'
#' Options that affect interactive display.
#' See [pillar::pillar_options] for options that affect display on the console,
#' and [cli::num_ansi_colors()] for enabling and disabling colored output
#' via ANSI sequences like `[3m[38;5;246m[39m[23m`.
#'
#' These options can be set via [options()] and queried via [getOption()].
#' For this, add a `tibble.` prefix (the package name and a dot) to the option name.
#' Example: for an option `foo`, use `options(tibble.foo = value)` to set it
#' and `getOption("tibble.foo")` to retrieve the current value.
#' An option value of `NULL` means that the default is used.
#'
#' @format NULL
#'
#' @examples
#' # Default setting:
#' getOption("tibble.view_max")
#'
#' # Change for the duration of the session:
#' old <- options(tibble.view_max = 100)
#'
#' # view() would show only 100 rows e.g. for a lazy data frame
#'
#' # Change back to the original value:
#' options(old)
#'
#' # Local scope:
#' local({
#'   rlang::local_options(tibble.view_max = 100)
#'   # view() would show only 100 rows e.g. for a lazy data frame
#' })
#' # view() would show the default 1000 rows e.g. for a lazy data frame
#' @section Options for the tibble package:
tibble_options <- list2(
  #' - `view_max`: Maximum number of rows shown by [view()]
  #'   if the input is not a data frame, passed on to [head()]. Default: `1000`.
  view_max = make_option_impl(
    getOption("tibble.view_max", default = 1000L)
  ),
)


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/R/pillar.R ---
#' Format a numeric vector
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Constructs a numeric vector that can be formatted with predefined
#' significant digits, or with a maximum or fixed number of digits
#' after the decimal point.
#' Scaling is supported, as well as forcing a decimal, scientific
#' or engineering notation.
#' If a label is given, it is shown in the header of a column.
#'
#' The formatting is applied when the vector is printed or formatted,
#' and also in a tibble column.
#' The formatting annotation and the class survives most arithmetic transformations,
#' the most notable exceptions are [var()] and [sd()].
#'
#' @family vector classes
#' @inheritParams rlang::args_dots_empty
#' @param x A numeric vector.
#' @param sigfig Define the number of significant digits to show. Must be one or greater.
#'   The `"pillar.sigfig"` [option][pillar::pillar_options] is not consulted.
#'   Can't be combined with `digits`.
#' @param digits Number of digits after the decimal points to show.
#'   Positive numbers specify the exact number of digits to show.
#'   Negative numbers specify (after negation) the maximum number of digits to show.
#'   With `digits = 2`, the numbers 1.2 and 1.234 are printed as 1.20 and 1.23,
#'   with `digits = -2` as 1.2 and 1.23, respectively.
#'   Can't be combined with `sigfig`.
#' @param label A label to show instead of the type description.
#' @param scale Multiplier to apply to the data before showing.
#'   Useful for displaying e.g. percentages.
#'   Must be combined with `label`.
#' @param notation One of `"fit"`, `"dec"`, `"sci"`, `"eng"`, or `"si"`.
#'   - `"fit"`: Use decimal notation if it fits and if it consumes 13 digits or less,
#'     otherwise use scientific notation. (The default for numeric pillars.)
#'   - `"dec"`: Use decimal notation, regardless of width.
#'   - `"sci"`: Use scientific notation.
#'   - `"eng"`: Use engineering notation, i.e. scientific notation
#'       using exponents that are a multiple of three.
#'   - `"si"`: Use SI notation, prefixes between `1e-24` and `1e24` are supported.
#' @param fixed_exponent
#'   Use the same exponent for all numbers in scientific, engineering or SI notation.
#'   `-Inf` uses the smallest, `+Inf` the largest fixed_exponent present in the data.
#'   The default is to use varying exponents.
#' @param extra_sigfig
#'   If `TRUE`, increase the number of significant digits if the data consists of
#'   numbers of the same magnitude with subtle differences.
#' @export
#' @examples
#' # Display as a vector
#' num(9:11 * 100 + 0.5)
#' @examples
#'
#' # Significant figures
#' tibble(
#'   x3 = num(9:11 * 100 + 0.5, sigfig = 3),
#'   x4 = num(9:11 * 100 + 0.5, sigfig = 4),
#'   x5 = num(9:11 * 100 + 0.5, sigfig = 5),
#' )
#'
#' # Maximum digits after the decimal points
#' tibble(
#'   x0 = num(9:11 * 100 + 0.5, digits = 0),
#'   x1 = num(9:11 * 100 + 0.5, digits = -1),
#'   x2 = num(9:11 * 100 + 0.5, digits = -2),
#' )
#'
#' # Use fixed digits and a currency label
#' tibble(
#'   usd = num(9:11 * 100 + 0.5, digits = 2, label = "USD"),
#'   gbp = num(9:11 * 100 + 0.5, digits = 2, label = "£"),
#'   chf = num(9:11 * 100 + 0.5, digits = 2, label = "SFr")
#' )
#'
#' # Scale
#' tibble(
#'   small  = num(9:11 / 1000 + 0.00005, label = "%", scale = 100),
#'   medium = num(9:11 / 100 + 0.0005, label = "%", scale = 100),
#'   large  = num(9:11 / 10 + 0.005, label = "%", scale = 100)
#' )
#'
#' # Notation
#' tibble(
#'   sci = num(10^(-13:6), notation = "sci"),
#'   eng = num(10^(-13:6), notation = "eng"),
#'   si  = num(10^(-13:6), notation = "si"),
#'   dec = num(10^(-13:6), notation = "dec")
#' )
#'
#' # Fixed exponent
#' tibble(
#'   scimin = num(10^(-7:6) * 123, notation = "sci", fixed_exponent = -Inf),
#'   engmin = num(10^(-7:6) * 123, notation = "eng", fixed_exponent = -Inf),
#'   simin  = num(10^(-7:6) * 123, notation = "si", fixed_exponent = -Inf)
#' )
#'
#' tibble(
#'   scismall = num(10^(-7:6) * 123, notation = "sci", fixed_exponent = -3),
#'   scilarge = num(10^(-7:6) * 123, notation = "sci", fixed_exponent = 3),
#'   scimax   = num(10^(-7:6) * 123, notation = "sci", fixed_exponent = Inf)
#' )
#'
#' #' Extra significant digits
#' tibble(
#'   default = num(100 + 1:3 * 0.001),
#'   extra1 = num(100 + 1:3 * 0.001, extra_sigfig = TRUE),
#'   extra2 = num(100 + 1:3 * 0.0001, extra_sigfig = TRUE),
#'   extra3 = num(10000 + 1:3 * 0.00001, extra_sigfig = TRUE)
#' )
# Assigned in .onLoad()
num <- NULL

#' set_num_opts
#'
#' `set_num_opts()` adds formatting options to an arbitrary numeric vector,
#' useful for composing with other types.
#'
#' @export
#' @rdname num
# Assigned in .onLoad()
set_num_opts <- NULL

#' Format a character vector
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Constructs a character vector that can be formatted with predefined minimum width
#' or without width restrictions, and where the abbreviation style can be configured.
#'
#' The formatting is applied when the vector is printed or formatted,
#' and also in a tibble column.
#'
#' @family vector classes
#' @inheritParams rlang::args_dots_empty
#' @param x A character vector.
#' @param min_chars The minimum width to allocate to this column, defaults to 15.
#'   The `"pillar.min_chars"` [option][pillar::pillar_options] is not consulted.
#' @param shorten How to abbreviate the data if necessary:
#' - `"back"` (default): add an ellipsis at the end
#' - `"front"`: add an ellipsis at the front
#' - `"mid"`: add an ellipsis in the middle
#' - `"abbreviate"`: use [abbreviate()]
#' @export
#' @examples
#' # Display as a vector:
#' char(letters[1:3])
#' @examplesIf { set.seed(20210331); rlang::is_installed("stringi") }
#' # Space constraints:
#' rand_strings <- stringi::stri_rand_strings(10, seq(40, 22, by = -2))
#'
#' # Plain character vectors get truncated if space is limited:
#' data_with_id <- function(id) {
#'   tibble(
#'     id,
#'     some_number_1 = 1, some_number_2 = 2, some_number_3 = 3,
#'     some_number_4 = 4, some_number_5 = 5, some_number_6 = 6,
#'     some_number_7 = 7, some_number_8 = 8, some_number_9 = 9
#'   )
#' }
#' data_with_id(rand_strings)
#'
#' # Use char() to avoid or control truncation
#' data_with_id(char(rand_strings, min_chars = 24))
#' data_with_id(char(rand_strings, min_chars = Inf))
#' data_with_id(char(rand_strings, min_chars = 24, shorten = "mid"))
#'
#' # Lorem Ipsum, one sentence per row.
#' lipsum <- unlist(strsplit(stringi::stri_rand_lipsum(1), "(?<=[.]) +", perl = TRUE))
#' tibble(
#'   back = char(lipsum, shorten = "back"),
#'   front = char(lipsum, shorten = "front"),
#'   mid = char(lipsum, shorten = "mid")
#' )
#' tibble(abbr = char(lipsum, shorten = "abbreviate"))
# Assigned in .onLoad()
char <- NULL

#' set_char_opts
#'
#' `set_char_opts()` adds formatting options to an arbitrary character vector,
#' useful for composing with other types.
#'
#' @export
#' @rdname char
# Assigned in .onLoad()
set_char_opts <- NULL


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/R/pipe.R ---
#' @export
magrittr::`%>%`


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/R/print.R ---
#' Printing tibbles
#'
#' @description
#' One of the main features of the `tbl_df` class is the printing:
#'
#' * Tibbles only print as many rows and columns as fit on one screen,
#'   supplemented by a summary of the remaining rows and columns.
#' * Tibble reveals the type of each column, which keeps the user informed about
#'   whether a variable is, e.g., `<chr>` or `<fct>` (character versus factor).
#'   See `vignette("types")` for an overview of common
#'   type abbreviations.
#'
#' Printing can be tweaked for a one-off call by calling `print()` explicitly
#' and setting arguments like `n` and `width`. More persistent control is
#' available by setting the options described in [pillar::pillar_options].
#' See also `vignette("digits")` for a comparison to base options,
#' and `vignette("numbers")` that showcases [num()] and [char()]
#' for creating columns with custom formatting options.
#'
#' As of tibble 3.1.0, printing is handled entirely by the \pkg{pillar} package.
#' If you implement a package that extends tibble,
#' the printed output can be customized in various ways.
#' See `vignette("extending", package = "pillar")` for details,
#' and [pillar::pillar_options] for options that control the display in the console.
#'
# Copied from pillar::format.tbl() to avoid roxygen2 warning
#' @inheritParams rlang::args_dots_empty
#' @param x Object to format or print.
#' @param n Number of rows to show. If `NULL`, the default, will print all rows
#'   if less than the `print_max` [option][pillar::pillar_options].
#'   Otherwise, will print as many rows as specified by the
#'   `print_min` [option][pillar::pillar_options].
#' @param width Width of text output to generate. This defaults to `NULL`, which
#'   means use the `width` [option][pillar::pillar_options].
#' @param max_extra_cols Number of extra columns to print abbreviated information for,
#'   if the width is too small for the entire tibble. If `NULL`,
#'   the `max_extra_cols` [option][pillar::pillar_options] is used.
#'   The previously defined `n_extra` argument is soft-deprecated.
#' @param max_footer_lines Maximum number of footer lines. If `NULL`,
#'   the `max_footer_lines` [option][pillar::pillar_options] is used.
#'
#' @examples
#' print(as_tibble(mtcars))
#' print(as_tibble(mtcars), n = 1)
#' print(as_tibble(mtcars), n = 3)
#'
#' print(as_tibble(trees), n = 100)
#'
#' print(mtcars, width = 10)
#'
#' mtcars2 <- as_tibble(cbind(mtcars, mtcars), .name_repair = "unique")
#' print(mtcars2, n = 25, max_extra_cols = 3)
#'
#' @examplesIf requireNamespace("nycflights13", quietly = TRUE)
#' print(nycflights13::flights, max_footer_lines = 1)
#' print(nycflights13::flights, width = Inf)
#'
#' @name formatting
#' @aliases print.tbl format.tbl
NULL

# Only for documentation, doesn't do anything
#' @rdname formatting
print.tbl_df <- function(
  x,
  width = NULL,
  ...,
  n = NULL,
  max_extra_cols = NULL,
  max_footer_lines = NULL
) {
  NextMethod()
}

# Only for documentation, doesn't do anything
#' @rdname formatting
format.tbl_df <- function(
  x,
  width = NULL,
  ...,
  n = NULL,
  max_extra_cols = NULL,
  max_footer_lines = NULL
) {
  NextMethod()
}

#' Legacy printing
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' As of tibble 3.1.0, printing is handled entirely by the \pkg{pillar} package.
#' Do not use this function.
#' If you implement a package that extend tibble,
#' the printed output can be customized in various ways.
#' See `vignette("extending", package = "pillar")` for details.
#'
#' @param x Object to format or print.
#' @param n Number of rows to show. If `NULL`, the default, will print all rows
#'   if less than option `tibble.print_max`. Otherwise, will print
#'   `tibble.print_min` rows.
#' @param width Width of text output to generate. This defaults to `NULL`, which
#'   means use `getOption("tibble.width")` or (if also `NULL`)
#'   `getOption("width")`; the latter displays only the columns that fit on one
#'   screen. You can also set `options(tibble.width = Inf)` to override this
#'   default and always print all columns, this may be slow for very wide tibbles.
#' @param n_extra Number of extra columns to print abbreviated information for,
#'   if the width is too small for the entire tibble. If `NULL`, the default,
#'   will print information about at most `tibble.max_extra_cols` extra columns.
#'
#' @return An object with a `print()` method that will print the input
#'   similarly to a tibble.
#'   The internal data format is an implementation detail, do not rely on it.
#' @export
#' @keywords internal
trunc_mat <- function(x, n = NULL, width = NULL, n_extra = NULL) {
  deprecate_soft(
    "3.1.0",
    "tibble::trunc_mat()",
    details = "Printing has moved to the pillar package."
  )

  if (!inherits(x, "tbl")) {
    class(x) <- c("tbl", class(x))
  }

  setup <- pillar::tbl_format_setup(
    x,
    width = width,
    n = n,
    max_extra_cols = n_extra
  )

  header <- pillar::tbl_format_header(x, setup)
  body <- pillar::tbl_format_body(x, setup)
  footer <- pillar::tbl_format_footer(x, setup)

  text <- c(header, body, footer)
  structure(list(text = text, summary = list(NULL)), class = "trunc_mat")
}

#' @export
format.trunc_mat <- function(x, width = NULL, ...) {
  unclass(x)[[1]]
}

#' @export
print.trunc_mat <- function(x, ...) {
  writeLines(format(x, ...))
  invisible(x)
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/R/rownames.R ---
#' Tools for working with row names
#'
#' @description
#' While a tibble can have row names (e.g., when converting from a regular data
#' frame), they are removed when subsetting with the `[` operator.
#' A warning will be raised when attempting to assign non-`NULL` row names
#' to a tibble.
#' Generally, it is best to avoid row names, because they are basically a
#' character column with different semantics than every other column.
#'
#' These functions allow to you detect if a data frame has row names
#' (`has_rownames()`), remove them (`remove_rownames()`), or convert
#' them back-and-forth between an explicit column (`rownames_to_column()`
#' and `column_to_rownames()`).
#' Also included is `rowid_to_column()`, which adds a column at the start of the
#' dataframe of ascending sequential row ids starting at 1. Note that this will
#' remove any existing row names.
#'
#' @return `column_to_rownames()` always returns a data frame.
#'   `has_rownames()` returns a scalar logical.
#'   All other functions return an object of the same class as the input.
#'
#' @param .data A data frame.
#' @param var Name of column to use for rownames.
#' @examples
#' # Detect row names ----------------------------------------------------
#' has_rownames(mtcars)
#' has_rownames(trees)
#'
#' # Remove row names ----------------------------------------------------
#' remove_rownames(mtcars) %>% has_rownames()
#'
#' # Convert between row names and column --------------------------------
#' mtcars_tbl <- rownames_to_column(mtcars, var = "car") %>% as_tibble()
#' mtcars_tbl
#' column_to_rownames(mtcars_tbl, var = "car") %>% head()
#'
#' # Adding rowid as a column --------------------------------------------
#' rowid_to_column(trees) %>% head()
#'
#' @name rownames
NULL


#' @export
#' @rdname rownames
has_rownames <- function(.data) {
  .row_names_info(.data) > 0L && !is.na(.row_names_info(.data, 0L)[[1L]])
}

#' @export
#' @rdname rownames
remove_rownames <- function(.data) {
  stopifnot(is.data.frame(.data))
  rownames(.data) <- NULL
  .data
}

#' @export
#' @rdname rownames
rownames_to_column <- function(.data, var = "rowname") {
  # rename, because .data has special semantics in tidy evaluation
  df <- .data

  stopifnot(is.data.frame(df))

  # Side effect: check unique names
  repaired_names(c(unique(names2(df)), var), repair_hint = FALSE)

  new_df <- add_column(df, !!var := rownames(df), .before = 1)
  remove_rownames(new_df)
}

#' @export
#' @rdname rownames
rowid_to_column <- function(.data, var = "rowid") {
  # rename, because .data has special semantics in tidy evaluation
  df <- .data

  stopifnot(is.data.frame(df))

  # Side effect: check unique names
  repaired_names(c(unique(names2(df)), var), repair_hint = FALSE)

  new_df <- add_column(df, !!var := seq_len(nrow(df)), .before = 1)
  remove_rownames(new_df)
}

#' @rdname rownames
#' @export
column_to_rownames <- function(.data, var = "rowname") {
  stopifnot(is.data.frame(.data))

  if (has_rownames(.data)) {
    abort_already_has_rownames()
  }

  if (!has_name(.data, var)) {
    abort_unknown_column_names(var)
  }

  .data <- as.data.frame(.data)
  rownames(.data) <- .data[[var]]
  .data[[var]] <- NULL
  .data
}

#' @export
`row.names<-.tbl_df` <- function(x, value) {
  if (!is.null(value)) {
    warn("Setting row names on a tibble is deprecated.")
  }
  NextMethod()
}

raw_rownames <- function(x) {
  .row_names_info(x, 0L) %||% .set_row_names(.row_names_info(x, 2L))
}

# Errors ------------------------------------------------------------------

abort_already_has_rownames <- function(call = caller_env()) {
  tibble_abort(call = call, "`.data` must be a data frame without row names.")
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/R/str.R ---
#' @export
str.tbl_df <- function(object, ..., indent.str = " ", nest.lev = 0) {
  if (nest.lev != 0L) {
    cat(" ")
  }
  cat(
    tibble::obj_sum(object),
    " (S3: ",
    paste0(class(object), collapse = "/"),
    ")",
    "\n",
    sep = ""
  )

  utils::str(
    as.list(object),
    no.list = TRUE,
    ...,
    nest.lev = nest.lev + 1L,
    indent.str = indent.str
  )
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/R/string-to-indices.R ---
string_to_indices <- function(x) {
  .Call(`tibble_string_to_indices`, as.character(x))
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/R/sub.R ---
#' Subsetting tibbles
#'
#' @description
#' Accessing columns, rows, or cells via `$`, `[[`, or `[` is mostly similar to
#' [regular data frames][base::Extract]. However, the
#' behavior is different for tibbles and data frames in some cases:
#' * `[` always returns a tibble by default, even if
#'   only one column is accessed.
#' * Partial matching of column names with `$` and `[[` is not supported, and
#'   `NULL` is returned.
#'   For `$`, a warning is given.
#' * Only scalars (vectors of length one) or vectors with the
#'   same length as the number of rows can be used for assignment.
#' * Rows outside of the tibble's boundaries cannot be accessed.
#' * When updating with `[[<-` and `[<-`, type changes of entire columns are
#'   supported, but updating a part of a column requires that the new value is
#'   coercible to the existing type.
#'   See [vec_slice()] for the underlying implementation.
#'
#' Unstable return type and implicit partial matching can lead to surprises and
#' bugs that are hard to catch. If you rely on code that requires the original
#' data frame behavior, coerce to a data frame via [as.data.frame()].
#'
#' @details
#' For better compatibility with older code written for regular data frames,
#' `[` supports a `drop` argument which defaults to `FALSE`.
#' New code should use `[[` to turn a column into a vector.
#'
#' @param x A tibble.
#' @param value A value to store in a row, column, range or cell.
#'   Tibbles are stricter than data frames in what is accepted here.
#'
#' @name subsetting
#' @examples
#' df <- data.frame(a = 1:3, bc = 4:6)
#' tbl <- tibble(a = 1:3, bc = 4:6)
#'
#' # Subsetting single columns:
#' df[, "a"]
#' tbl[, "a"]
#' tbl[, "a", drop = TRUE]
#' as.data.frame(tbl)[, "a"]
#'
#' # Subsetting single rows with the drop argument:
#' df[1, , drop = TRUE]
#' tbl[1, , drop = TRUE]
#' as.list(tbl[1, ])
#'
#' @examplesIf (Sys.getenv("NOT_CRAN") != "true" || Sys.getenv("IN_PKGDOWN") == "true")
#' # Accessing non-existent columns:
#' df$b
#' tbl$b
#'
#' df[["b", exact = FALSE]]
#' tbl[["b", exact = FALSE]]
#'
#' df$bd <- c("n", "e", "w")
#' tbl$bd <- c("n", "e", "w")
#' df$b
#' tbl$b
#' @examples
#'
#' df$b <- 7:9
#' tbl$b <- 7:9
#' df$b
#' tbl$b
#'
#' # Identical behavior:
#' tbl[1, ]
#' tbl[1, c("bc", "a")]
#' tbl[, c("bc", "a")]
#' tbl[c("bc", "a")]
#' tbl["a"]
#' tbl$a
#' tbl[["a"]]
NULL

#' @rdname subsetting
#' @param name A [name] or a string.
#' @export
`$.tbl_df` <- function(x, name) {
  out <- .subset2(x, name)
  if (is.null(out)) {
    warn(paste0("Unknown or uninitialised column: ", tick(name), "."))
  }
  out
}

#' @rdname subsetting
#' @param i,j Row and column indices. If `j` is omitted, `i` is used as column index.
#' @param ... Ignored.
#' @param exact Ignored, with a warning.
#' @export
`[[.tbl_df` <- function(x, i, j, ..., exact = TRUE) {
  if (!exact) {
    warn("`exact` ignored.")
  }

  n_dots <- dots_n(...)
  if (n_dots > 0) {
    warn("Extra arguments ignored.")
  }

  # Ignore exact as an argument for counting
  n_real_args <- nargs() - !missing(exact) - n_dots

  # Column subsetting if nargs() == 2L
  if (n_real_args <= 2L) {
    if (missing(i)) {
      abort_subset_columns_non_missing_only()
    }
    tbl_subset2(x, j = i, j_arg = substitute(i))
  } else if (missing(j) || missing(i)) {
    abort_subset_columns_non_missing_only()
  } else {
    i_arg <- substitute(i)
    i <- vectbl_as_row_location2(i, fast_nrow(x), i_arg)
    x <- tbl_subset2(x, j = j, j_arg = substitute(j))

    if (is.null(x)) {
      x
    } else {
      # Drop inner names with double subscript
      vectbl_set_names(vec_slice(x, i), NULL)
    }
  }
}

#' @rdname subsetting
#' @param drop Coerce to a vector if fetching one column via `tbl[, j]` .
#'   Default `FALSE`, ignored when accessing a column via `tbl[j]` .
#' @export
`[.tbl_df` <- function(x, i, j, drop = FALSE, ...) {
  i_arg <- substitute(i)
  j_arg <- substitute(j)

  if (missing(i)) {
    i <- NULL
    i_arg <- NULL
  } else if (is.null(i)) {
    i <- integer()
  }

  if (missing(j)) {
    j <- NULL
    j_arg <- NULL
  } else if (is.null(j)) {
    j <- integer()
  }

  # Ignore drop as an argument for counting
  n_real_args <- nargs() - !missing(drop)

  # Column or matrix subsetting if nargs() == 2L
  if (n_real_args <= 2L) {
    if (!missing(drop)) {
      warn(
        "`drop` argument ignored for subsetting a tibble with `x[j]`, it has an effect only for `x[i, j]`."
      )
      drop <- FALSE
    }

    j <- i
    i <- NULL
    j_arg <- i_arg
    i_arg <- NULL

    # Special case, returns a vector:
    if (is.matrix(j)) {
      return(tbl_subset_matrix(x, j, j_arg))
    }
  }

  # From here on, i, j and drop contain correct values:
  if (is.null(j)) {
    xo <- x
  } else {
    j <- vectbl_as_col_location(
      j,
      length(x),
      names(x),
      j_arg = j_arg,
      assign = FALSE
    )

    xo <- .subset(x, j)

    if (anyDuplicated.default(j)) {
      xo <- set_repaired_names(
        xo,
        repair_hint = FALSE,
        .name_repair = "minimal"
      )
    }
  }

  if (is.null(i)) {
    nrow <- fast_nrow(x)
  } else {
    i <- vectbl_as_row_index(i, x, i_arg)
    xo <- lapply(xo, vec_slice, i = i)
    nrow <- length(i)
  }

  if (drop && length(xo) == 1L) {
    tbl_subset2(xo, 1L, j_arg)
  } else {
    attr(xo, "row.names") <- .set_row_names(nrow)
    vectbl_restore(xo, x)
  }
}

tbl_subset2 <- function(x, j, j_arg, call = caller_env()) {
  if (is.matrix(j)) {
    deprecate_stop(
      "3.0.0",
      "tibble::`[[.tbl_df`(j = 'can\\'t be a matrix')",
      details = "Recursive subsetting is deprecated for tibbles.",
      env = foreign_caller_env()
    )
  }

  if (is.object(j)) {
    j <- vectbl_as_col_subscript2(j, j_arg, call = call)
  }

  if (is.numeric(j)) {
    if (length(j) == 1L) {
      if (
        is.na(j) || j < 1 || j > length(x) || (is.double(j) && j != trunc(j))
      ) {
        # Side effect: throw error for invalid j
        vectbl_as_col_location2(j, length(x), j_arg = j_arg, call = call)
      }
    } else if (length(j) == 2L) {
      deprecate_stop(
        "3.0.0",
        "tibble::`[[.tbl_df`(j = 'can\\'t be a vector of length 2')",
        details = "Recursive subsetting is deprecated for tibbles.",
        env = foreign_caller_env()
      )
    } else {
      # Side effect: throw error for invalid j
      vectbl_as_col_location2(j, length(x), j_arg = j_arg, call = call)
    }
  } else if (is.symbol(j)) {
    # FIXME: Only relevant for R < 3.4
    j <- as.character(j)
  } else if (
    is.logical(j) || length(j) != 1L || !is_bare_atomic(j) || is.na(j)
  ) {
    # Side effect: throw error for invalid j
    vectbl_as_col_location2(j, length(x), names(x), j_arg = j_arg, call = call)
  }

  .subset2(x, j)
}

vectbl_as_col_subscript2 <- function(
  j,
  j_arg,
  assign = FALSE,
  call = caller_env()
) {
  subclass_col_index_errors(
    vec_as_subscript2(j, logical = "error", call = call),
    j_arg = j_arg,
    assign = assign
  )
}

vectbl_as_col_location2 <- function(
  j,
  n,
  names = NULL,
  j_arg,
  assign = FALSE,
  call = caller_env()
) {
  subclass_col_index_errors(
    vec_as_location2(j, n, names, call = call),
    j_arg = j_arg,
    assign = assign
  )
}

vectbl_as_row_location2 <- function(
  i,
  n,
  i_arg,
  assign = FALSE,
  call = caller_env()
) {
  subclass_row_index_errors(
    vec_as_location2(i, n, call = call),
    i_arg = i_arg,
    assign = assign
  )
}

vectbl_set_names <- function(x, names = NULL) {
  # Work around https://github.com/r-lib/vctrs/issues/1419
  if (inherits(x, "vctrs_rcrd")) {
    # A rcrd can't have names?
    return(x)
  }
  vec_set_names(x, names)
}

vectbl_as_col_location <- function(
  j,
  n,
  names = NULL,
  j_arg,
  assign = FALSE,
  call = caller_env()
) {
  subclass_col_index_errors(
    vec_as_location(j, n, names, missing = "error", call = call),
    j_arg = j_arg,
    assign = assign
  )
}

vectbl_as_row_index <- function(
  i,
  x,
  i_arg,
  assign = FALSE,
  call = caller_env()
) {
  stopifnot(!is.null(i))

  nr <- fast_nrow(x)

  if (is.character(i)) {
    is_na_orig <- is.na(i)

    if (has_rownames(x)) {
      i <- match(i, rownames(x))
    } else {
      i <- string_to_indices(i)
      i <- fix_oob(i, nr, warn = FALSE)
    }

    i <- fix_oob_invalid(i, is_na_orig)
    i
  } else if (is.numeric(i)) {
    i <- fix_oob(i, nr)
    vectbl_as_row_location(i, nr, i_arg, assign, call)
  } else {
    vectbl_as_row_location(i, nr, i_arg, assign, call)
  }
}

fix_oob <- function(i, n, warn = TRUE) {
  if (any(i > 0, na.rm = TRUE)) {
    fix_oob_positive(i, n, warn)
  } else if (any(i < 0, na.rm = TRUE)) {
    fix_oob_negative(i, n, warn)
  } else {
    # Will throw error in vec_as_location()
    i
  }
}

fix_oob_positive <- function(i, n, warn = TRUE) {
  oob <- which(i > n)
  if (warn && length(oob) > 0) {
    deprecate_soft(
      "3.0.0",
      "tibble::`[.tbl_df`(i = 'must lie in [0, rows] if positive,')",
      details = "Use `NA_integer_` as row index to obtain a row full of `NA` values.",
      env = foreign_caller_env()
    )
  }

  i[oob] <- NA_integer_
  i
}

fix_oob_negative <- function(i, n, warn = TRUE) {
  oob <- (i < -n)
  if (warn && any(oob, na.rm = TRUE)) {
    deprecate_soft(
      "3.0.0",
      "tibble::`[.tbl_df`(i = 'must lie in [-rows, 0] if negative,')",
      details = "Use `NA_integer_` as row index to obtain a row full of `NA` values.",
      env = foreign_caller_env()
    )
  }

  i <- i[!oob]
  if (is_empty(i)) {
    i <- seq_len(n)
  }
  i
}

fix_oob_invalid <- function(i, is_na_orig) {
  oob <- which(is.na(i) & !is_na_orig)

  if (length(oob) > 0) {
    deprecate_soft(
      "3.0.0",
      "tibble::`[.tbl_df`(i = 'must use valid row names')",
      details = "Use `NA_integer_` as row index to obtain a row full of `NA` values.",
      env = foreign_caller_env()
    )

    i[oob] <- NA_integer_
  }
  i
}

fast_nrow <- function(x) {
  .row_names_info(x, 2L)
}

# External ----------------------------------------------------------------

vectbl_restore <- function(xo, x) {
  .Call(`tibble_restore_impl`, xo, x)
}

# Errors ------------------------------------------------------------------

abort_subset_columns_non_missing_only <- function(call = caller_env()) {
  tibble_abort(call = call, "Subscript can't be missing for tibbles in `[[`.")
}

# Subclassing errors ------------------------------------------------------

subclass_col_index_errors <- function(expr, j_arg, assign) {
  withCallingHandlers(
    expr,
    vctrs_error_subscript = function(cnd) {
      cnd$subscript_arg <- j_arg
      cnd$subscript_elt <- "column"
      if (isTRUE(assign) && !isTRUE(cnd$subscript_action %in% c("negate"))) {
        cnd$subscript_action <- "assign"
      }
      cnd_signal(cnd)
    }
  )
}

subclass_row_index_errors <- function(expr, i_arg, assign) {
  withCallingHandlers(
    expr,
    vctrs_error_subscript = function(cnd) {
      cnd$subscript_arg <- i_arg
      cnd$subscript_elt <- "row"
      if (isTRUE(assign) && !isTRUE(cnd$subscript_action %in% c("negate"))) {
        cnd$subscript_action <- "assign"
      }
      cnd_signal(cnd)
    }
  )
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/R/subassign-backend.R ---
#' Main function for subset assignment
#'
#' Powers $<-, [[<- and [<- for tibbles.
#'
#' @param x A tibble.
#' @param i An integer vector, or `NULL` to replace all rows.
#' @param j A named integer vector, or `NULL` to replace all columns.
#'   The names are required to define the names for new columns.
#'   In addition, if new columns are created, their names must be stored
#'   in the `"new"` attribute, for performance.
#' @param value A list of length one or of the same length as `j`.
#'   `NULL` elements indicate column removal.
#' @param i_arg,value_arg Argument names.
#' @noRd
tbl_subassign <- function(
  x,
  i,
  j,
  value,
  i_arg,
  j_arg,
  value_arg,
  call = caller_env()
) {
  if (is.null(i)) {
    xo <- unclass(x)

    if (is.null(value)) {
      value <- list(value)
    } else {
      value <- result_vectbl_wrap_rhs(value)
      if (is.null(value)) {
        abort_need_rhs_vector_or_null(value_arg, call)
      }
    }

    if (is.null(j)) {
      j <- seq_along(xo)
      names(j) <- names2(j)
    } else if (!is.null(j_arg)) {
      j <- vectbl_as_new_col_index(
        j,
        xo,
        j_arg,
        names2(value),
        value_arg,
        call = call
      )
    }

    value <- vectbl_recycle_rhs_rows(
      value,
      fast_nrow(xo),
      i_arg = NULL,
      value_arg,
      call
    )
    value <- vectbl_recycle_rhs_cols(value, length(j), call)

    xo <- tbl_subassign_col(xo, j, value)
  } else if (is.null(i_arg)) {
    # x[NULL, ...] <- value
    return(x)
  } else {
    i <- vectbl_as_new_row_index(i, x, i_arg, call = call)

    # Fill up rows first if necessary
    x <- tbl_expand_to_nrow(x, i, call)
    value <- result_vectbl_wrap_rhs(value)
    if (is.null(value)) {
      abort_need_rhs_vector(value_arg, call)
    }

    # Only after tbl_expand_to_nrow() which needs data frame
    xo <- unclass(x)

    if (is.null(j)) {
      xo <- tbl_subassign_row(xo, i, value, i_arg, value_arg, call)
    } else {
      # Optimization: match only once
      # (Invariant: x[[j]] is equivalent to x[[vec_as_location(j)]],
      # allowed by corollary that only existing columns can be updated)
      if (!is.null(j_arg)) {
        j <- vectbl_as_new_col_index(
          j,
          xo,
          j_arg,
          names2(value),
          value_arg,
          call = call
        )
      }

      # Fill up columns if necessary
      new <- attr(j, "new")
      if (!is.null(new)) {
        init <- map(value[new], vec_slice, rep(NA_integer_, fast_nrow(xo)))
        xo <- tbl_subassign_col(xo, j[new], init)
      }

      xj <- .subset(xo, j)
      xj <- tbl_subassign_row(xj, i, value, i_arg, value_arg, call)
      xo <- tbl_subassign_col(xo, j, xj)
    }
  }

  vectbl_restore(xo, x)
}

vectbl_recycle_rhs_rows <- function(value, nrow, i_arg, value_arg, call) {
  if (length(value) > 0L) {
    withCallingHandlers(
      for (j in seq_along(value)) {
        if (!is.null(value[[j]])) {
          value[[j]] <- vec_recycle(value[[j]], nrow)
        }
      },
      vctrs_error_recycle_incompatible_size = function(cnd) {
        abort_assign_incompatible_size(
          nrow,
          value,
          j,
          i_arg,
          value_arg,
          cnd,
          call = call
        )
      },
      vctrs_error_scalar_type = function(cnd) {
        abort_assign_vector(value, j, value_arg, cnd, call = call)
      }
    )
  }

  value
}

vectbl_recycle_rhs_cols <- function(value, ncol, call) {
  if (length(value) != 1L || ncol != 1L) {
    # Errors have been caught beforehand in vectbl_as_new_col_index()
    value <- vec_recycle(value, ncol, call = call)
  }

  value
}

tbl_subassign_col <- function(x, j, value) {
  nrow <- fast_nrow(x)

  # Grow, assign new names
  new <- attr(j, "new")
  if (!is.null(new)) {
    length(x) <- max(j[new])
    names(x)[j[new]] <- names2(j)[new]
  }

  # Update
  to_remove <- integer()
  for (jj in seq_along(value)) {
    ji <- j[[jj]]
    value_jj <- value[[jj]]
    if (!is.null(value_jj)) {
      x[[ji]] <- value_jj
    } else {
      to_remove <- c(to_remove, ji)
    }
  }

  # Remove
  if (length(to_remove) > 0) {
    x <- x[-to_remove]
  }

  # Can be destroyed by setting length
  attr(x, "row.names") <- .set_row_names(nrow)
  x
}

tbl_expand_to_nrow <- function(x, i, call = caller_env()) {
  nrow <- fast_nrow(x)

  new_nrow <- max(i, nrow)

  if (is.na(new_nrow)) {
    abort_assign_rows_non_na_only(call)
  }

  if (new_nrow != nrow) {
    # FIXME: vec_expand()?
    i_expand <- c(seq_len(nrow), rep(NA_integer_, new_nrow - nrow))
    x <- vec_slice(x, i_expand)
  }

  x
}

tbl_subassign_row <- function(x, i, value, i_arg, value_arg, call) {
  recycled_value <- vectbl_recycle_rhs_cols(value, length(x), call)

  withCallingHandlers(
    for (j in seq_along(x)) {
      x[[j]] <- vectbl_assign(x[[j]], i, recycled_value[[j]])
    },
    vctrs_error = function(cnd) {
      # Side effect: check if `value` can be recycled
      vectbl_recycle_rhs_rows(value, length(i), i_arg, value_arg, call)

      abort_assign_incompatible_type(
        x,
        recycled_value,
        j,
        value_arg,
        cnd,
        call = call
      )
    }
  )

  x
}

vectbl_assign <- function(x, i, value) {
  if (is.logical(value)) {
    if (.Call(`tibble_need_coerce`, value)) {
      value <- vec_slice(x, NA_integer_)
    }
  } else {
    if (.Call(`tibble_need_coerce`, x)) {
      d <- dim(x)
      dn <- dimnames(x)
      x <- vec_slice(value, rep(NA_integer_, length(x)))
      dim(x) <- d
      dimnames(x) <- dn
    }
  }

  vec_assign(x, i, value)
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/R/subassign.R ---
#' @rdname subsetting
#' @export
`$<-.tbl_df` <- function(x, name, value) {
  tbl_subassign(
    x,
    i = NULL,
    as_string(name),
    list(value),
    i_arg = NULL,
    j_arg = name,
    value_arg = substitute(value)
  )
}

#' @rdname subsetting
#' @export
`[[<-.tbl_df` <- function(x, i, j, ..., value) {
  i_arg <- substitute(i)
  j_arg <- substitute(j)
  value_arg <- substitute(value)

  if (missing(i)) {
    abort_assign_columns_non_missing_only()
  }

  if (missing(j)) {
    if (nargs() < 4) {
      j <- i
      i <- NULL
      j_arg <- i_arg
      i_arg <- NULL
    } else {
      abort_assign_columns_non_missing_only()
    }
  }

  if (!is.null(i)) {
    i <- vectbl_as_row_location2(i, fast_nrow(x), i_arg, assign = TRUE)
  }

  if (is.object(j)) {
    j <- vectbl_as_col_subscript2(j, j_arg, assign = TRUE)
  }

  # Side effect: check scalar
  if (is.symbol(j)) {
    # FIXME: as_utf8_character() needs rlang > 0.4.11
    j <- chr_unserialise_unicode(as.character(j))
  } else {
    if (
      !is.vector(j) ||
        length(j) != 1L ||
        is.na(j) ||
        (is.numeric(j) && j < 0) ||
        is.logical(j)
    ) {
      vectbl_as_col_location2(j, length(x), j_arg = j_arg, assign = TRUE)
    }
  }

  j <- vectbl_as_new_col_index(j, x, j_arg)

  # New columns are added to the end, provide index to avoid matching column
  # names again
  value <- list(value)

  # j is already pretty
  tbl_subassign(
    x,
    i,
    j,
    value,
    i_arg = i_arg,
    j_arg = NULL,
    value_arg = value_arg
  )
}


#' @rdname subsetting
#' @export
`[<-.tbl_df` <- function(x, i, j, ..., value) {
  i_arg <- substitute(i)
  j_arg <- substitute(j)

  if (missing(i)) {
    i <- NULL
    i_arg <- NULL
  } else if (is.null(i)) {
    i <- integer()
  }

  if (missing(j)) {
    j <- NULL
    j_arg <- NULL
  } else if (is.null(j)) {
    j <- integer()
  }

  if (is.null(j) && nargs() < 4) {
    j <- i
    i <- NULL
    j_arg <- i_arg
    i_arg <- NULL

    # Special case:
    if (is.matrix(j)) {
      return(tbl_subassign_matrix(x, j, value, j_arg, substitute(value)))
    }
  }

  tbl_subassign(x, i, j, value, i_arg, j_arg, substitute(value))
}

vectbl_as_new_row_index <- function(i, x, i_arg, call) {
  if (is.null(i)) {
    i
  } else if (is_bare_numeric(i)) {
    if (anyDuplicated.default(i)) {
      abort_assign_duplicate_row_subscript(i, call)
    }

    nr <- fast_nrow(x)

    # Only update existing, caller knows how to deal with OOB
    numtbl_as_row_location_assign(i, nr, i_arg, call)
  } else if (is_logical(i)) {
    # Don't allow OOB logical
    vectbl_as_row_location(i, fast_nrow(x), i_arg, assign = TRUE, call = call)
  } else {
    i <- vectbl_as_row_index(i, x, i_arg, assign = TRUE, call = call)
    if (anyDuplicated.default(i, incomparables = NA)) {
      abort_assign_duplicate_row_subscript(i, call)
    }
    i
  }
}

vectbl_as_new_col_index <- function(
  j,
  x,
  j_arg,
  names = "",
  value_arg = NULL,
  call = caller_env()
) {
  # Creates a named index vector
  # Values: index
  # Name: column name (for all columns)

  if (is.object(j)) {
    j <- vectbl_as_col_subscript(j, j_arg = j_arg, assign = TRUE, call = call)
  }

  if (is.character(j)) {
    if (anyNA(j)) {
      abort_assign_columns_non_na_only(call)
    }

    names <- j

    j <- match(names, names(x))
    new <- which(is.na(j))
    if (length(new) > 0) {
      # FIXME: Check consistency with assigning to the same existing column twice
      j[new] <- seq.int(length(x) + 1L, length.out = length(new))
    } else {
      new <- NULL
    }
  } else if (is.numeric(j)) {
    if (anyNA(j)) {
      abort_assign_columns_non_na_only(call)
    }

    j <- numtbl_as_col_location_assign(j, length(x), j_arg, call)

    old <- (j <= length(x))
    new <- which(!old)
    j_new <- j[new]

    # FIXME: Recycled names are not repaired
    # FIXME: Hard-coded name repair
    if (length(names) != 1L) {
      # Side effect: check compatibility
      vec_recycle(names, length(j), x_arg = as_label(value_arg), call = call)
    } else if (length(j) != 1L) {
      # length(names) == 1
      names <- vec_recycle(names, length(j), x_arg = as_label(value_arg))
    }

    if (length(new) > 0) {
      j[new] <- j_new
      names[new][names[new] == ""] <- paste0("...", j_new)
    } else {
      new <- NULL
    }

    names[old] <- names(x)[j[old]]
  } else {
    j <- vectbl_as_col_location(
      j,
      length(x),
      names(x),
      j_arg = j_arg,
      assign = TRUE,
      call = call
    )

    if (length(names) != 1L) {
      # Side effect: check compatibility
      vec_recycle(names, length(j), x_arg = as_label(value_arg), call = call)
    } else if (length(j) != 1L) {
      # length(names) == 1
      names <- vec_recycle(names, length(j), x_arg = as_label(value_arg))
    }

    old <- (j <= length(x))
    names[old] <- names(x)[j[old]]

    new <- NULL
  }

  if (anyDuplicated.default(j)) {
    abort_assign_duplicate_column_subscript(j, call)
  }

  names(j) <- names
  attr(j, "new") <- new
  j
}

vectbl_as_col_subscript <- function(
  j,
  j_arg,
  assign = FALSE,
  call = caller_env()
) {
  subclass_col_index_errors(
    vec_as_subscript(j, call = call),
    j_arg = j_arg,
    assign = assign
  )
}

numtbl_as_row_location_assign <- function(i, n, i_arg, call) {
  subclass_row_index_errors(
    num_as_location(
      i,
      n,
      missing = "error",
      oob = "extend",
      zero = "error",
      call = call
    ),
    i_arg = i_arg,
    assign = TRUE
  )
}

vectbl_as_row_location <- function(i, n, i_arg, assign = FALSE, call) {
  if (is_bare_atomic(i) && is.matrix(i) && ncol(i) == 1) {
    what <- paste0(
      "tibble::",
      if (assign) "`[<-`" else "`[`",
      "(i = 'can\\'t be a matrix')"
    )

    lifecycle::deprecate_soft(
      "3.0.0",
      what,
      details = "Convert to a vector.",
      env = foreign_caller_env()
    )
    i <- i[, 1]
  }

  subclass_row_index_errors(
    vec_as_location(
      i,
      n,
      missing = if (assign) "error" else "propagate",
      call = call
    ),
    i_arg = i_arg,
    assign = assign
  )
}

numtbl_as_col_location_assign <- function(j, n, j_arg, call) {
  subclass_col_index_errors(
    num_as_location(
      j,
      n,
      missing = "error",
      oob = "extend",
      zero = "error",
      call = call
    ),
    j_arg = j_arg,
    assign = TRUE
  )
}

result_vectbl_wrap_rhs <- function(value) {
  if (!vec_is(value)) {
    NULL
  } else if (is.list(value)) {
    # Also covers the case of data frames
    unclass(value)
  } else if (is.array(value)) {
    if (any(dim(value)[-1:-2] != 1)) {
      return(NULL)
    }
    dim(value) <- head(dim(value), 2)
    as.list(as.data.frame(value, stringsAsFactors = FALSE))
  } else {
    list(value)
  }
}

# Errors ------------------------------------------------------------------

abort_need_rhs_vector <- function(value_arg, call = caller_env()) {
  tibble_abort(
    call = call,
    paste0(
      tick(as_label(value_arg)),
      " must be a vector, a bare list, a data frame or a matrix."
    )
  )
}

abort_need_rhs_vector_or_null <- function(value_arg, call = caller_env()) {
  tibble_abort(
    call = call,
    paste0(
      tick(as_label(value_arg)),
      " must be a vector, a bare list, a data frame, a matrix, or NULL."
    )
  )
}

abort_assign_columns_non_na_only <- function(call = caller_env()) {
  tibble_abort(
    call = call,
    "Can't use NA as column index in a tibble for assignment."
  )
}

abort_assign_columns_non_missing_only <- function(call = caller_env()) {
  tibble_abort(call = call, "Subscript can't be missing for tibbles in `[[<-`.")
}

abort_assign_duplicate_column_subscript <- function(j, call = caller_env()) {
  j <- unique(j[duplicated(j)])
  tibble_abort(
    call = call,
    pluralise_commas(
      "Column index(es) ",
      j,
      " [is](are) used more than once for assignment."
    ),
    j = j
  )
}

abort_assign_rows_non_na_only <- function(call = caller_env()) {
  tibble_abort(
    call = call,
    "Can't use NA as row index in a tibble for assignment."
  )
}

abort_assign_duplicate_row_subscript <- function(i, call = caller_env()) {
  i <- unique(i[duplicated(i)])
  tibble_abort(
    call = call,
    pluralise_commas(
      "Row index(es) ",
      i,
      " [is](are) used more than once for assignment."
    ),
    i = i
  )
}

abort_assign_incompatible_size <- function(
  nrow,
  value,
  j,
  i_arg,
  value_arg,
  parent = NULL,
  call = caller_env()
) {
  if (is.null(i_arg)) {
    target <- "existing data"
    existing <- pluralise_count("Existing data has ", nrow, " row(s)")
  } else {
    target <- paste0("row subscript ", tick(as_label(i_arg)))
    existing <- pluralise_count("", nrow, " row(s) must be assigned")
  }

  new <- paste0(pluralise_count("has ", vec_size(value[[j]]), " row(s)"))
  if (length(value) != 1) {
    new <- paste0("Element ", j, " of assigned data ", new)
  } else {
    new <- paste0("Assigned data ", new)
  }

  tibble_abort(
    bullets(
      paste0(
        "Assigned data ",
        tick(as_label(value_arg)),
        " must be compatible with ",
        target,
        ":"
      ),
      x = existing,
      x = new,
      i = if (nrow != 1) "Only vectors of size 1 are recycled",
      i = if (nrow == 1 && vec_size(value[[j]]) != 1) {
        "Row updates require a list value. Do you need `list()` or `as.list()`?"
      }
    ),
    expected = nrow,
    actual = vec_size(value[[j]]),
    j = j,
    parent = parent,
    call = call
  )
}

abort_assign_incompatible_type <- function(
  x,
  value,
  j,
  value_arg,
  parent = NULL,
  call = caller_env()
) {
  name <- names(x)[[j]]

  tibble_abort(
    bullets(
      paste0(
        "Assigned data ",
        tick(as_label(value_arg)),
        " must be compatible with existing data:"
      ),
      i = paste0("Error occurred for column ", tick(name))
    ),
    expected = x[[j]],
    actual = value[[j]],
    name = name,
    j = j,
    parent = parent,
    call = call
  )
}

abort_assign_vector <- function(
  value,
  j,
  value_arg,
  parent = NULL,
  call = caller_env()
) {
  tibble_abort(
    paste0("Assigned data ", tick(as_label(value_arg)), " must be a vector."),
    actual = value[[j]],
    j = j,
    parent = parent,
    call = call
  )
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/R/subsetting-matrix.R ---
tbl_subset_matrix <- function(x, j, j_arg, call = caller_env()) {
  cells <- matrix_to_cells(j, x, j_arg, call)
  col_idx <- cells_to_col_idx(cells)

  if (is_empty(col_idx)) {
    return(unspecified())
  }

  values <- map2(x[col_idx], cells[col_idx], vec_slice)

  # Also checks conformity of vectors:
  unname(vec_c(!!!values, .name_spec = ~.x))
}

tbl_subassign_matrix <- function(
  x,
  j,
  value,
  j_arg,
  value_arg,
  call = caller_env()
) {
  # FIXME: use size argument in vctrs >= 0.3.0

  if (!vec_is(value)) {
    abort_subset_matrix_scalar_type(j_arg, value_arg, call)
  }

  if (vec_size(value) != 1) {
    abort_subset_matrix_must_be_scalar(j_arg, value_arg, call)
  }

  cells <- matrix_to_cells(j, x, j_arg, call)
  col_idx <- cells_to_col_idx(cells)

  withCallingHandlers(
    for (j in col_idx) {
      x[[j]] <- vectbl_assign(x[[j]], cells[[j]], value)
    },
    vctrs_error_incompatible_type = function(cnd) {
      abort_assign_incompatible_type(
        x,
        rep(list(value), j),
        j,
        value_arg,
        cnd,
        call = call
      )
    }
  )

  x
}

matrix_to_cells <- function(j, x, j_arg, call = caller_env()) {
  if (!is_bare_logical(j)) {
    abort_subset_matrix_must_be_logical(j_arg, call)
  }
  if (!identical(dim(j), dim(x))) {
    abort_subset_matrix_must_have_same_dimensions(j_arg, call)
  }

  # Need unlist(list(...)) because apply() isn't type stable if the return
  # has the same length everywhere
  # FIXME: Faster with a C implementation?
  cells <- unlist(apply(j, 2, function(x) list(which(x))), recursive = FALSE)
  cells
}

cells_to_col_idx <- function(cells) {
  sizes <- map_int(cells, vec_size)
  col_idx <- which(sizes > 0)

  col_idx
}

# Errors ------------------------------------------------------------------

abort_subset_matrix_must_be_logical <- function(j_arg, call = caller_env()) {
  tibble_abort(
    call = call,
    paste0(
      "Subscript ",
      tick(as_label(j_arg)),
      " is a matrix, it must be of type logical."
    )
  )
}

abort_subset_matrix_must_have_same_dimensions <- function(
  j_arg,
  call = caller_env()
) {
  tibble_abort(
    call = call,
    paste0(
      "Subscript ",
      tick(as_label(j_arg)),
      " is a matrix, it must have the same dimensions as the input."
    )
  )
}

abort_subset_matrix_scalar_type <- function(
  j_arg,
  value_arg,
  call = caller_env()
) {
  tibble_abort(
    call = call,
    paste0(
      "Subscript ",
      tick(as_label(j_arg)),
      " is a matrix, the data ",
      tick(as_label(value_arg)),
      " must be a vector of size 1."
    )
  )
}

abort_subset_matrix_must_be_scalar <- function(
  j_arg,
  value_arg,
  call = caller_env()
) {
  tibble_abort(
    call = call,
    paste0(
      "Subscript ",
      tick(as_label(j_arg)),
      " is a matrix, the data ",
      tick(as_label(value_arg)),
      " must have size 1."
    )
  )
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/R/tbl_df.R ---
#' @exportClass tbl_df
setOldClass(c("tbl_df", "tbl", "data.frame"))

#' `tbl_df` class
#'
#' @description
#' The `tbl_df` class is a subclass of [`data.frame`][base::data.frame()],
#' created in order to have different default behaviour. The colloquial term
#' "tibble" refers to a data frame that has the `tbl_df` class. Tibble is the
#' central data structure for the set of packages known as the
#' [tidyverse](https://www.tidyverse.org/packages/), including
#' [dplyr](https://dplyr.tidyverse.org/),
#' [ggplot2](https://ggplot2.tidyverse.org/),
#' [tidyr](https://tidyr.tidyverse.org/), and
#' [readr](https://readr.tidyverse.org/).
#'
#' The general ethos is that tibbles are lazy and surly: they do less and
#' complain more than base [data.frame]s. This forces
#' problems to be tackled earlier and more explicitly, typically leading to code
#' that is more expressive and robust.
#'
#' @section Properties of `tbl_df`:
#'
#' Objects of class `tbl_df` have:
#' * A `class` attribute of `c("tbl_df", "tbl", "data.frame")`.
#' * A base type of `"list"`, where each element of the list has the same
#'   [vctrs::vec_size()].
#' * A `names` attribute that is a character vector the same length as the
#'   underlying list.
#' * A `row.names` attribute, included for compatibility with [data.frame].
#'   This attribute is only consulted to query the number of rows,
#'   any row names that might be stored there are ignored
#'   by most tibble methods.
#'
#' @section Behavior of `tbl_df`:
#'
#' How default behaviour of tibbles differs from that of
#' [data.frame]s, during creation and access:
#'
#' * Column data is not coerced. A character vector is not turned into a factor.
#'   List-columns are expressly anticipated and do not require special tricks.
#'   Internal names are never stripped from column data.
#'   Read more in [tibble()].
#' * Recycling only happens for a length 1 input.
#'   Read more in [vctrs::vec_recycle()].
#' * Column names are not munged, although missing names are auto-populated.
#'   Empty and duplicated column names are strongly discouraged, but the user
#'   must indicate how to resolve. Read more in [vctrs::vec_as_names()].
#' * Row names are not added and are strongly discouraged, in favor of storing
#'   that info as a column. Read about in [rownames].
#' * `df[, j]` returns a tibble; it does not automatically extract the column
#'   inside. `df[, j, drop = FALSE]` is the default. Read more in [subsetting].
#' * There is no partial matching when `$` is used to index by name. `df$name`
#'   for a nonexistent name generates a warning. Read more in [subsetting].
#'
#' See `vignette("invariants")` for a detailed description of the behavior.
#'
#' Furthermore, printing and inspection are a very high priority.
#' The goal is to convey as much information as possible, in a concise way,
#' even for large and complex tibbles. Read more in [formatting].
#'
#' @name tbl_df-class
#' @aliases tbl_df tbl_df-class
#' @seealso [tibble()], [as_tibble()], [tribble()], [print.tbl()],
#'   [glimpse()]
NULL

# Standard data frame methods --------------------------------------------------

#' @export
as.data.frame.tbl_df <- function(x, row.names = NULL, optional = FALSE, ...) {
  class(x) <- tibble_class
  unname <- which(!map_lgl(x, is_bare_list))
  x[unname] <- map(.subset(x, unname), vectbl_set_names, NULL)
  class(x) <- "data.frame"
  x
}

#' @export
`names<-.tbl_df` <- function(x, value) {
  # workaround for RStudio v1.1, which relies on the ability to set
  # data.frame names to NULL
  if (is.null(value) && is_rstudio()) {
    attr(x, "names") <- NULL
    return(x)
  }

  if (is.null(value)) {
    deprecate_soft("3.0.0", "tibble::`names<-`(value = 'can\\'t be NULL')")

    # FIXME: value <- rep("", length(x))
  }

  if (!has_length(value, length(x))) {
    deprecate_soft(
      "3.0.0",
      "tibble::`names<-`(value = 'must have the same length as `x`')"
    )

    # FIXME: Reset NA to "" in names

    if (length(value) < length(x)) {
      value <- c(value, rep(NA_character_, length(x) - length(value)))
    } else {
      length(value) <- length(x)
    }
  }

  if (anyNA(value)) {
    deprecate_soft("3.0.0", "tibble::`names<-`(value = 'can\\'t be empty')")

    # FIXME: Reset NA to "" in names
  }

  if (!is_character(value)) {
    deprecate_soft(
      "3.0.0",
      "tibble::`names<-`(value = 'must be a character vector')"
    )
    value <- as.character(value)
  }

  attr(x, "names") <- as.character(value)
  x
}

# Errors ------------------------------------------------------------------

msg_names_must_be_non_null <- function() {
  "`names` must not be `NULL`."
}

msg_names_must_have_length <- function(length, n) {
  paste0("`names` must have length ", n, ", not ", length, ".")
}

abort_names_must_be_non_null <- function(call = caller_env()) {
  tibble_abort(call = call, msg_names_must_be_non_null())
}

abort_names_must_have_length <- function(length, n, call = caller_env()) {
  tibble_abort(
    call = call,
    msg_names_must_have_length(length, n),
    expected = n,
    actual = length
  )
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/R/tbl_sum.R ---
#' @export
pillar::tbl_sum

#' @export
tbl_sum.tbl_df <- function(x, ...) {
  c("A tibble" = dim_desc(x))
}

#' @export
pillar::obj_sum

#' @export
pillar::type_sum

#' @export
pillar::size_sum

#' @export
vec_ptype_abbr.tbl_df <- function(x, ...) {
  abbr <- class(x)[[1]]
  if (abbr == "tbl_df") {
    abbr <- "tibble"
  }
  abbr
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/R/tibble-package.R ---
## usethis namespace: start
#' @import rlang
#' @importFrom lifecycle deprecate_soft
#' @importFrom lifecycle deprecate_stop
#' @importFrom lifecycle deprecate_warn
#' @importFrom lifecycle expect_deprecated
#' @importFrom magrittr %>%
#' @importFrom methods setOldClass
#' @importFrom pillar dim_desc
#' @importFrom pillar glimpse
#' @importFrom pillar obj_sum
#' @importFrom pillar size_sum
#' @importFrom pillar style_subtle
#' @importFrom pillar tbl_sum
#' @importFrom pillar type_sum
#' @importFrom pkgconfig set_config
#' @importFrom utils head tail
#' @importFrom vctrs new_data_frame
#' @importFrom vctrs new_rcrd
#' @importFrom vctrs num_as_location
#' @importFrom vctrs unspecified
#' @importFrom vctrs vec_as_location
#' @importFrom vctrs vec_as_location2
#' @importFrom vctrs vec_as_names
#' @importFrom vctrs vec_as_names_legacy
#' @importFrom vctrs vec_as_subscript
#' @importFrom vctrs vec_as_subscript2
#' @importFrom vctrs vec_assign
#' @importFrom vctrs vec_c
#' @importFrom vctrs vec_is
#' @importFrom vctrs vec_names
#' @importFrom vctrs vec_names2
#' @importFrom vctrs vec_ptype_abbr
#' @importFrom vctrs vec_rbind
#' @importFrom vctrs vec_recycle
#' @importFrom vctrs vec_set_names
#' @importFrom vctrs vec_size
#' @importFrom vctrs vec_slice
## usethis namespace: end
NULL

#' @useDynLib tibble, .registration = TRUE
#' @details
#' `r lifecycle::badge("stable")`
#'
#' The tibble package provides utilities for handling __tibbles__, where
#' "tibble" is a colloquial term for the S3 [`tbl_df`] class. The [`tbl_df`]
#' class is a special case of the base [`data.frame`][base::data.frame()]
#' class, developed in response to lessons learned over many years of data
#' analysis with data frames.
#'
#' Tibble is the central data structure for the set of packages known as the
#' [tidyverse](https://www.tidyverse.org/packages/), including
#' [dplyr](https://dplyr.tidyverse.org/),
#' [ggplot2](https://ggplot2.tidyverse.org/),
#' [tidyr](https://tidyr.tidyverse.org/), and
#' [readr](https://readr.tidyverse.org/).
#'
#' General resources:
#'   * Website for the tibble package: <https://tibble.tidyverse.org>
#'   * [Vectors chapter](https://adv-r.hadley.nz/vectors-chap.html) in *Advanced R*
#'     (2nd edition), specifically the
#'     [Data frames and tibbles section](https://adv-r.hadley.nz/vectors-chap.html#tibble)
#'
#' Resources on specific topics:
#'   * Create a tibble: [tibble()], [as_tibble()], [tribble()], [enframe()]
#'   * Inspect a tibble: [print.tbl()], [glimpse()]
#'   * Details on the S3 `tbl_df` class: [`tbl_df-class`]
#'   * Package options: [tibble_options]
"_PACKAGE"


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/R/tibble.R ---
#' Build a data frame
#'
#' @description
#'
#' `tibble()` constructs a data frame. It is used like [base::data.frame()], but
#' with a couple notable differences:
#'
#'   * The returned data frame has the class [`tbl_df`][tbl_df-class], in
#'     addition to `data.frame`. This allows so-called "tibbles" to exhibit some
#'     special behaviour, such as [enhanced printing][formatting]. Tibbles are
#'     fully described in [`tbl_df`][tbl_df-class].
#'   * `tibble()` is much lazier than [base::data.frame()] in terms of
#'     transforming the user's input.
#'
#'       - List-columns are expressly anticipated and do not require special tricks.
#'       - Column names are not modified.
#'       - Inner names in columns are left unchanged.
#'       - For R < 4.0, [character vectors were not coerced to factor](https://blog.r-project.org/2020/02/16/stringsasfactors/).
#'
#'   * `tibble()` builds columns sequentially. When defining a column, you can
#'     refer to columns created earlier in the call. Only columns of length one
#'     are recycled.
#'   * If a column evaluates to a data frame or tibble, it is nested or spliced.
#'     If it evaluates to a matrix or a array, it remains a matrix or array,
#'     respectively.
#'     See examples.
#'
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]>
#'   A set of name-value pairs. These arguments are
#'   processed with [rlang::quos()] and support unquote via [`!!`] and
#'   unquote-splice via [`!!!`]. Use `:=` to create columns that start with a dot.
#'
#'   Arguments are evaluated sequentially.
#'   You can refer to previously created elements directly or using the [.data]
#'   pronoun.
#'   To refer explicitly to objects in the calling environment, use [`!!`] or
#'   [.env], e.g. `!!.data` or `.env$.data` for the special case of an object
#'   named `.data`.
#' @param .rows The number of rows, useful to create a 0-column tibble or
#'   just as an additional check.
#' @param .name_repair Treatment of problematic column names:
#'   * `"minimal"`: No name repair or checks, beyond basic existence,
#'   * `"unique"`: Make sure names are unique and not empty,
#'   * `"check_unique"`: (default value), no name repair, but check they are
#'     `unique`,
#'   * `"universal"`: Make the names `unique` and syntactic
#'   * `"unique_quiet"`: Same as `"unique"`, but "quiet"
#'   * `"universal_quiet"`: Same as `"universal"`, but "quiet"
#'   * a function: apply custom name repair (e.g., `.name_repair = make.names`
#'     for names in the style of base R).
#'   * A purrr-style anonymous function, see [rlang::as_function()]
#'
#'   This argument is passed on as `repair` to [vctrs::vec_as_names()].
#'   See there for more details on these terms and the strategies used
#'   to enforce them.
#'
#' @return A tibble, which is a colloquial term for an object of class
#'   [`tbl_df`][tbl_df-class]. A [`tbl_df`][tbl_df-class] object is also a data
#'   frame, i.e. it has class `data.frame`.
#' @seealso Use [as_tibble()] to turn an existing object into a tibble. Use
#'   `enframe()` to convert a named vector into a tibble. Name repair is
#'   detailed in [vctrs::vec_as_names()].
#'   See [quasiquotation] for more details on tidy dots semantics,
#'   i.e. exactly how  the `...` argument is processed.
#' @export
#' @examples
#' # Unnamed arguments are named with their expression:
#' a <- 1:5
#' tibble(a, a * 2)
#'
#' # Scalars (vectors of length one) are recycled:
#' tibble(a, b = a * 2, c = 1)
#'
#' # Columns are available in subsequent expressions:
#' tibble(x = runif(10), y = x * 2)
#'
#' # tibble() never coerces its inputs,
#' str(tibble(letters))
#' str(tibble(x = list(diag(1), diag(2))))
#'
#' # or munges column names (unless requested),
#' tibble(`a + b` = 1:5)
#'
#' # but it forces you to take charge of names, if they need repair:
#' try(tibble(x = 1, x = 2))
#' tibble(x = 1, x = 2, .name_repair = "unique")
#' tibble(x = 1, x = 2, .name_repair = "minimal")
#'
#' ## By default, non-syntactic names are allowed,
#' df <- tibble(`a 1` = 1, `a 2` = 2)
#' ## because you can still index by name:
#' df[["a 1"]]
#' df$`a 1`
#' with(df, `a 1`)
#'
#' ## Syntactic names are easier to work with, though, and you can request them:
#' df <- tibble(`a 1` = 1, `a 2` = 2, .name_repair = "universal")
#' df$a.1
#'
#' ## You can specify your own name repair function:
#' tibble(x = 1, x = 2, .name_repair = make.unique)
#'
#' fix_names <- function(x) gsub("\\s+", "_", x)
#' tibble(`year 1` = 1, `year 2` = 2, .name_repair = fix_names)
#'
#' ## purrr-style anonymous functions and constants
#' ## are also supported
#' tibble(x = 1, x = 2, .name_repair = ~ make.names(., unique = TRUE))
#'
#' tibble(x = 1, x = 2, .name_repair = ~ c("a", "b"))
#'
#' # Tibbles can contain columns that are tibbles or matrices
#' # if the number of rows is compatible. Unnamed tibbled are
#' # spliced, i.e. the inner columns are inserted into the
#' # tibble under construction.
#' tibble(
#'   a = 1:3,
#'   tibble(
#'     b = 4:6,
#'     c = 7:9
#'   ),
#'   d = tibble(
#'     e = tibble(
#'       f = b
#'     )
#'   )
#' )
#' tibble(
#'   a = 1:3,
#'   b = diag(3),
#'   c = cor(trees),
#'   d = Titanic[1:3, , , ]
#' )
#'
#' # Data can not contain tibbles or matrices with incompatible number of rows:
#' try(tibble(a = 1:3, b = tibble(c = 4:7)))
#'
#' # Use := to create columns with names that start with a dot:
#' tibble(.dotted := 3)
#'
#' # This also works, but might break in the future:
#' tibble(.dotted = 3)
#'
#' # You can unquote an expression:
#' x <- 3
#' tibble(x = 1, y = x)
#' tibble(x = 1, y = !!x)
#'
#' # You can splice-unquote a list of quosures and expressions:
#' tibble(!!!list(x = rlang::quo(1:10), y = quote(x * 2)))
#'
#' # Use .data, .env and !! to refer explicitly to columns or outside objects
#' a <- 1
#' tibble(a = 2, b = a)
#' tibble(a = 2, b = .data$a)
#' tibble(a = 2, b = .env$a)
#' tibble(a = 2, b = !!a)
#' try(tibble(a = 2, b = .env$bogus))
#' try(tibble(a = 2, b = !!bogus))
tibble <- function(
  ...,
  .rows = NULL,
  .name_repair = c(
    "check_unique",
    "unique",
    "universal",
    "minimal",
    "unique_quiet",
    "universal_quiet"
  )
) {
  xs <- quos(...)

  tibble_quos(xs, .rows, .name_repair)
}

#' tibble_row()
#'
#' @description
#' `tibble_row()` constructs a data frame that is guaranteed to occupy one row.
#' Vector columns are required to have size one, non-vector columns are wrapped
#' in a list.
#'
#' @rdname tibble
#' @export
#' @examples
#'
#' # Use tibble_row() to construct a one-row tibble:
#' tibble_row(a = 1, lm = lm(Height ~ Girth + Volume, data = trees))
tibble_row <- function(
  ...,
  .name_repair = c(
    "check_unique",
    "unique",
    "universal",
    "minimal",
    "unique_quiet",
    "universal_quiet"
  )
) {
  xs <- enquos(...)

  tibble_quos(xs, .rows = 1, .name_repair = .name_repair, single_row = TRUE)
}

#' Test if the object is a tibble
#'
#' This function returns `TRUE` for tibbles or subclasses thereof,
#' and `FALSE` for all other objects, including regular data frames.
#'
#' @param x An object
#' @return `TRUE` if the object inherits from the `tbl_df` class.
#' @export
is_tibble <- function(x) {
  inherits(x, "tbl_df")
}

#' Deprecated test for tibble-ness
#'
#' @description
#' `r lifecycle::badge("soft-deprecated")`
#'
#' Please use [is_tibble()] instead.
#'
#' @inheritParams is_tibble
#' @export
#' @keywords internal
is.tibble <- function(x) {
  deprecate_warn("2.0.0", "is.tibble()", "is_tibble()")

  is_tibble(x)
}

tibble_quos <- function(
  xs,
  .rows,
  .name_repair,
  single_row = FALSE,
  call = caller_env()
) {
  # Evaluate each column in turn
  col_names <- given_col_names <- names2(xs)
  empty_col_names <- which(col_names == "")
  col_names[empty_col_names] <- names(quos_auto_name(xs[empty_col_names]))
  lengths <- rep_along(xs, 0L)

  output <- rep_along(xs, list(NULL))

  env <- new_environment()
  mask <- new_data_mask_with_data(env)

  first_size <- .rows

  for (j in seq_along(xs)) {
    res <- eval_tidy(xs[[j]], mask)

    if (!is.null(res)) {
      # Single-row mode: Vectors must be length one, non-vectors are wrapped
      # in a list (which is length one by definition)
      if (single_row) {
        if (vec_is(res)) {
          if (vec_size(res) != 1) {
            abort_tibble_row_size_one(j, given_col_names[[j]], vec_size(res))
          }
        } else {
          res <- list(res)
        }
      } else {
        # 657
        res <- check_valid_col(res, col_names[[j]], j, call)

        lengths[[j]] <- current_size <- vec_size(res)
        if (is.null(first_size)) {
          first_size <- current_size
        } else if (first_size == 1L && current_size != 1L) {
          idx_to_fix <- seq2(1L, j - 1L)
          output[idx_to_fix] <- fixed_output <-
            map(output[idx_to_fix], vec_recycle, current_size)

          # Refill entire data mask
          map2(
            output[idx_to_fix],
            col_names[idx_to_fix],
            add_to_env2,
            env = env
          )

          first_size <- current_size
        } else {
          res <- vectbl_recycle_rows(
            res,
            first_size,
            j,
            given_col_names[[j]],
            call
          )
        }
      }

      output[[j]] <- res
      col_names[[j]] <- add_to_env2(
        res,
        given_col_names[[j]],
        col_names[[j]],
        env
      )
    }
  }

  names(output) <- col_names

  is_null <- map_lgl(output, is.null)
  output <- output[!is_null]

  output <- splice_dfs(output)
  output <- set_repaired_names(
    output,
    repair_hint = TRUE,
    .name_repair = .name_repair,
    call = call
  )

  new_tibble(output, nrow = first_size %||% 0L)
}

check_valid_col <- function(x, name, pos, call) {
  if (name == "") {
    ret <- check_valid_cols(list(x), pos = pos, call = call)
  } else {
    ret <- check_valid_cols(set_names(list(x), name), call = call)
  }
  invisible(ret[[1]])
}

new_data_mask_with_data <- function(env) {
  mask <- new_data_mask(env)
  mask$.data <- as_data_pronoun(env)
  mask
}

add_to_env2 <- function(x, given_name, name = given_name, env) {
  if (is.data.frame(x) && given_name == "") {
    imap(x, add_to_env, env)
    ""
  } else {
    add_to_env(x, name, env)
    name
  }
}

add_to_env <- function(x, name, env) {
  env[[name]] <- x
  invisible()
}

splice_dfs <- function(x) {
  # Avoiding .ptype argument to vec_c()
  if (is_empty(x)) {
    return(list())
  }

  x <- imap(x, function(.x, .y) {
    if (.y == "") unclass(.x) else list2(!!.y := .x)
  })
  vec_c(!!!x, .name_spec = "{inner}")
}

vectbl_recycle_rows <- function(x, n, j, name, call = caller_env()) {
  size <- vec_size(x)
  if (size == n) {
    return(x)
  }
  if (size == 1) {
    return(vec_recycle(x, n))
  }

  if (name == "") {
    name <- j
  }

  abort_incompatible_size(n, name, size, "Existing data", call)
}

# Errors ------------------------------------------------------------------

abort_tibble_row_size_one <- function(j, name, size, call = caller_env()) {
  if (name != "") {
    desc <- tick(name)
  } else {
    desc <- paste0("at position ", j)
  }

  tibble_abort(
    call = call,
    problems(
      "All vectors must be size one, use `list()` to wrap.",
      paste0("Column ", desc, " is of size ", size, ".")
    )
  )
}

abort_incompatible_size <- function(
  .rows,
  vars,
  vars_len,
  rows_source,
  call = caller_env()
) {
  vars_split <- split(vars, vars_len)

  vars_split[["1"]] <- NULL
  if (!is.null(.rows)) {
    vars_split[[as.character(.rows)]] <- NULL
  }

  problems <- map2_chr(names(vars_split), vars_split, function(x, y) {
    if (is.numeric(y)) {
      text <- "Column(s) at position(s) "
    } else {
      text <- "Column(s) "
      y <- tick(y)
    }

    paste0("Size ", x, ": ", pluralise_commas(text, y))
  })

  tibble_abort(
    call = call,
    bullets(
      "Tibble columns must have compatible sizes:",
      if (!is.null(.rows)) paste0("Size ", .rows, ": ", rows_source),
      problems,
      info = "Only values of size one are recycled."
    )
  )
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/R/tidy_names.R ---
#' Superseded functions for name repair
#'
#' @description
#' `r lifecycle::badge("superseded")`
#'
#' @description
#' `tidy_names()`, `set_tidy_names()`, and `repair_names()` were early efforts
#' to facilitate *post hoc* name repair in tibble, given that [tibble()] and
#' [as_tibble()] did not do this.
#'
#' From tibble v2.0.0, the `.name_repair` argument gives direct access to three
#' specific levels of name repair: `minimal`, `unique`, and `universal`.
#' See [vctrs::vec_as_names()] for the implementation of the underlying logic.
#'
#' @section Life cycle:
#'
#' These functions are superseded. The `repair_names()` logic
#' will also remain available in [vctrs::vec_as_names_legacy()].
#'
#' ```
#' tibble(..., `.name_repair = "unique"`)
#' ## is preferred to
#' df <- tibble(...)
#' set_tidy_names(df, syntactic = FALSE)
#'
#' tibble(..., `.name_repair = "universal"`)
#' ## is preferred to
#' df <- tibble(...)
#' set_tidy_names(df, syntactic = TRUE)
#' ```
#'
#' @param x A vector.
#' @param name A `names` attribute, usually a character vector.
#' @param syntactic Should names be made syntactically valid? If `FALSE`, uses
#'   same logic as `.name_repair = "unique"`. If `TRUE`, uses same logic as
#'   `.name_repair = "universal"`.
#' @param quiet Whether to suppress messages about name repair.
#'
#' @return `x` with repaired names or a repaired version of `name`.
#'
#' @export
#' @name name-repair-superseded
#' @aliases name-repair-retired
#' @keywords internal
tidy_names <- function(name, syntactic = FALSE, quiet = FALSE) {
  # Local functions to preserve behavior in v1.4.2
  is_syntactic <- function(x) {
    ret <- make.names(x) == x
    ret[is.na(x)] <- FALSE
    ret
  }

  make_syntactic <- function(name, syntactic) {
    if (!syntactic) {
      return(name)
    }

    blank <- name == ""
    fix_syntactic <- (name != "") & !is_syntactic(name)
    name[fix_syntactic] <- make.names(name[fix_syntactic])
    name
  }

  append_pos <- function(name) {
    need_append_pos <- duplicated(name) |
      duplicated(name, fromLast = TRUE) |
      name == ""
    if (any(need_append_pos)) {
      rx <- "[.][.][1-9][0-9]*$"
      has_suffix <- grepl(rx, name)
      name[has_suffix] <- gsub(rx, "", name[has_suffix])
      need_append_pos <- need_append_pos | has_suffix
    }

    need_append_pos <- which(need_append_pos)
    name[need_append_pos] <- paste0(
      name[need_append_pos],
      "..",
      need_append_pos
    )
    name
  }

  describe_tidying <- function(orig_name, name, quiet) {
    stopifnot(length(orig_name) == length(name))
    if (quiet) {
      return()
    }
    new_names <- name != orig_name
    if (any(new_names)) {
      message(
        "New names:\n",
        paste0(orig_name[new_names], " -> ", name[new_names], collapse = "\n")
      )
    }
  }

  name[is.na(name)] <- ""
  orig_name <- name

  name <- make_syntactic(name, syntactic)
  name <- append_pos(name)

  describe_tidying(orig_name, name, quiet)
  name
}

#' @export
#' @rdname name-repair-superseded
set_tidy_names <- function(x, syntactic = FALSE, quiet = FALSE) {
  x <- set_repaired_names(x, repair_hint = FALSE, "minimal", quiet = TRUE)
  new_names <- tidy_names(names(x), syntactic = syntactic, quiet = quiet)
  set_names(x, new_names)
}

#' @param prefix A string, the prefix to use for new column names.
#' @param sep A string inserted between the column name and de-duplicating
#'   number.
#' @export
#' @rdname name-repair-superseded
repair_names <- function(x, prefix = "V", sep = "") {
  if (length(x) == 0) {
    names(x) <- character()
    return(x)
  }

  new_names <- vec_as_names_legacy(names2(x), prefix = prefix, sep = sep)
  set_names(x, new_names)
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/R/tribble.R ---
#' Row-wise tibble creation
#'
#' @description
#' Create [tibble]s using an easier to read row-by-row layout.
#' This is useful for small tables of data where readability is
#' important.  Please see \link{tibble-package} for a general introduction.
#'
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]>
#'   Arguments specifying the structure of a `tibble`.
#'   Variable names should be formulas, and may only appear before the data.
#'   These arguments are processed with [rlang::list2()]
#'   and support unquote via [`!!`] and unquote-splice via [`!!!`].
#' @return A [tibble].
#' @seealso
#'   See [quasiquotation] for more details on tidy dots semantics,
#'   i.e. exactly how  the `...` argument is processed.
#' @export
#' @examples
#' tribble(
#'   ~colA, ~colB,
#'   "a",   1,
#'   "b",   2,
#'   "c",   3
#' )
#'
#' # tribble will create a list column if the value in any cell is
#' # not a scalar
#' tribble(
#'   ~x,  ~y,
#'   "a", 1:3,
#'   "b", 4:6
#' )
#' @examplesIf rlang::is_installed("dplyr") && packageVersion("dplyr") >= "1.0.5"
#'
#' # Use dplyr::mutate(dplyr::across(...)) to assign an explicit type
#' tribble(
#'   ~a, ~b, ~c,
#'   1, "2000-01-01", "1.5"
#' ) %>%
#'   dplyr::mutate(
#'     dplyr::across(a, as.integer),
#'     dplyr::across(b, as.Date)
#'   )
tribble <- function(...) {
  data <- extract_frame_data_from_dots(...)
  turn_frame_data_into_tibble(data$frame_names, data$frame_rest)
}

#' Row-wise matrix creation
#'
#' @description
#' Create matrices laying out the data in rows, similar to
#' `matrix(..., byrow = TRUE)`, with a nicer-to-read syntax.
#' This is useful for small matrices, e.g. covariance matrices, where readability
#' is important. The syntax is inspired by [tribble()].
#'
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]>
#'   Arguments specifying the structure of a `frame_matrix`.
#'   Column names should be formulas, and may only appear before the data.
#'   These arguments are processed with [rlang::list2()]
#'   and support unquote via [`!!`] and unquote-splice via [`!!!`].
#' @return A [matrix].
#' @seealso
#'   See [quasiquotation] for more details on tidy dots semantics,
#'   i.e. exactly how  the `...` argument is processed.
#' @export
#' @examples
#' frame_matrix(
#'   ~col1, ~col2,
#'   1,     3,
#'   5,     2
#' )
frame_matrix <- function(...) {
  data <- extract_frame_data_from_dots(...)
  turn_frame_data_into_frame_matrix(data$frame_names, data$frame_rest)
}

extract_frame_data_from_dots <- function(..., .call = caller_env()) {
  dots <- list2(...)

  # Extract the names.
  frame_names <- extract_frame_names_from_dots(dots, .call)

  # Extract the data
  if (length(frame_names) == 0 && length(dots) != 0) {
    abort_tribble_needs_columns(.call)
  }
  frame_rest <- dots[-seq_along(frame_names)]
  if (!is.null(names(frame_rest))) {
    abort_tribble_named_after_tilde(.call)
  }
  if (length(frame_rest) == 0L) {
    # Can't decide on type in absence of data -- use logical which is
    # coercible to all types
    frame_rest <- unspecified()
  }

  validate_rectangular_shape(frame_names, frame_rest, .call)

  list(frame_names = frame_names, frame_rest = frame_rest)
}

extract_frame_names_from_dots <- function(dots, call = caller_env()) {
  frame_names <- character()

  for (i in seq_along(dots)) {
    el <- dots[[i]]
    if (!is.call(el)) {
      break
    }

    if (!identical(el[[1]], as.name("~"))) {
      break
    }

    if (length(el) != 2) {
      abort_tribble_lhs_column_syntax(el[[2]], call)
    }

    candidate <- el[[2]]
    if (!(is.symbol(candidate) || is.character(candidate))) {
      abort_tribble_rhs_column_syntax(candidate)
    }

    frame_names <- c(frame_names, as.character(candidate))
  }

  frame_names
}

validate_rectangular_shape <- function(
  frame_names,
  frame_rest,
  call = caller_env()
) {
  if (length(frame_names) == 0 && length(frame_rest) == 0) {
    return()
  }

  # Figure out the associated number of rows and number of columns,
  # and validate that the supplied formula produces a rectangular
  # structure.
  if (length(frame_rest) %% length(frame_names) != 0) {
    abort_tribble_non_rectangular(length(frame_names), length(frame_rest), call)
  }
}

turn_frame_data_into_tibble <- function(names, rest, call = caller_env()) {
  if (is_empty(names)) {
    return(new_tibble(list(), nrow = 0))
  }

  nrow <- length(rest) / length(names)
  dim(rest) <- c(length(names), nrow)
  dimnames(rest) <- list(names, NULL)

  frame_mat <- t(rest)
  frame_col <- turn_matrix_into_column_list(frame_mat, call)

  new_tibble(frame_col, nrow = nrow)
}

turn_matrix_into_column_list <- function(frame_mat, call) {
  frame_col <- vector("list", length = ncol(frame_mat))
  names(frame_col) <- colnames(frame_mat)

  # if a frame_mat's col is a list column, keep it unchanged (does not unlist)
  for (i in seq_len(ncol(frame_mat))) {
    col <- frame_mat[, i]

    if (inherits(col, "list") && !some(col, needs_list_col)) {
      subclass_tribble_c_errors(
        names(frame_col)[[i]],
        col <- vec_c(!!!unname(col)),
        call
      )
    }

    frame_col[[i]] <- unname(col)
  }
  return(frame_col)
}

turn_frame_data_into_frame_matrix <- function(
  names,
  rest,
  call = caller_env()
) {
  list_cols <- which(map_lgl(rest, needs_list_col))
  if (has_length(list_cols)) {
    abort_frame_matrix_list(list_cols, call)
  }

  frame_ncol <- length(names)
  frame_mat <- matrix(unlist(rest), ncol = frame_ncol, byrow = TRUE)

  colnames(frame_mat) <- names
  frame_mat
}

subclass_tribble_c_errors <- function(name, code, call) {
  withCallingHandlers(
    code,
    vctrs_error = function(cnd) {
      abort_tribble_c(name, cnd, call)
    }
  )
}

# Errors ------------------------------------------------------------------

abort_tribble_needs_columns <- function(call = caller_env()) {
  tibble_abort(
    call = call,
    "Must specify at least one column using the `~name` syntax."
  )
}

abort_tribble_named_after_tilde <- function(call = caller_env()) {
  tibble_abort(
    call = call,
    "When using the `~name` syntax, subsequent values must not have names."
  )
}

abort_tribble_lhs_column_syntax <- function(lhs, call = caller_env()) {
  tibble_abort(
    call = call,
    problems(
      "All column specifications must use the `~name` syntax.",
      paste0("Found ", expr_label(lhs), " on the left-hand side of `~`.")
    )
  )
}

abort_tribble_rhs_column_syntax <- function(rhs, call = caller_env()) {
  tibble_abort(
    call = call,
    problems(
      'All column specifications must use the `~name` or `~"name"` syntax.',
      paste0("Found ", expr_label(rhs), " on the right-hand side of `~`.")
    )
  )
}

abort_tribble_non_rectangular <- function(cols, cells, call = caller_env()) {
  tibble_abort(
    call = call,
    bullets(
      "Data must be rectangular:",
      paste0("Found ", cols, " columns."),
      paste0("Found ", cells, " cells."),
      info = paste0(cells, " is not an integer multiple of ", cols, ".")
    )
  )
}

abort_frame_matrix_list <- function(pos, call = caller_env()) {
  tibble_abort(
    call = call,
    problems(
      "All values must be atomic:",
      pluralise_commas("Found list-valued element(s) at position(s) ", pos, ".")
    )
  )
}

abort_tribble_c <- function(name, cnd, call) {
  tibble_abort(
    paste0("Can't create column ", tick(name)),
    parent = cnd,
    call = call
  )
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/R/utils-msg-format.R ---
pluralise_msg <- function(message, objects) {
  paste0(
    pluralise(message, objects),
    format_n(objects)
  )
}

format_n <- function(x) collapse(quote_n(x))

quote_n <- function(x) UseMethod("quote_n")
#' @export
quote_n.default <- function(x) as.character(x)
#' @export
quote_n.character <- function(x) tick(x)

collapse <- function(x) paste(x, collapse = ", ")

pluralise_commas <- function(message, objects, ...) {
  paste0(
    pluralise_n(message, length(objects)),
    commas(objects),
    pluralise_n(paste0(...), length(objects))
  )
}

pluralise_count <- function(message, count, ...) {
  paste0(
    pluralise_n(message, count),
    count,
    pluralise_n(paste0(...), count)
  )
}

pluralise <- function(message, objects) {
  pluralise_n(message, length(objects))
}

pluralise_n <- function(message, n) {
  stopifnot(n >= 0)

  # Don't strip parens if they have a space in between
  # (but not if the space comes before the closing paren)

  if (n == 1) {
    # strip [
    message <- gsub("\\[([^\\] ]* *)\\]", "\\1", message, perl = TRUE)
    # remove ( and its content
    message <- gsub("\\([^\\) ]* *\\)", "", message, perl = TRUE)
  } else {
    # strip (
    message <- gsub("\\(([^\\) ]* *)\\)", "\\1", message, perl = TRUE)
    # remove [ and its content
    message <- gsub("\\[[^\\] ]* *\\]", "", message, perl = TRUE)
  }

  message
}

bullets <- function(header, ..., info = NULL) {
  # FIXME: Avoid ensure_full_stop()
  bullets <- vec_c(..., .name_spec = "{outer}")
  bullets <- set_default_name(bullets, "*")

  vec_c(
    ensure_full_stop(vec_c(header, bullets, .name_spec = "{outer}")),
    i = info,
    .name_spec = "{outer}"
  )
}

problems <- function(header, ..., .problem = " problem(s)") {
  problems <- vec_c(..., .name_spec = "{outer}")
  MAX_BULLETS <- 6L
  if (length(problems) >= MAX_BULLETS) {
    n_more <- length(problems) - MAX_BULLETS + 1L
    problems[[MAX_BULLETS]] <-
      pluralise_n(paste0(pre_dots("and "), n_more, " more", .problem), n_more)
    length(problems) <- MAX_BULLETS
  }

  problems <- set_default_name(problems, "x")
  bullets(header, problems)
}

pre_dots <- function(x) {
  if (length(x) > 0) {
    paste0(cli::symbol$ellipsis, " ", x)
  } else {
    character()
  }
}

commas <- function(problems) {
  MAX_BULLETS <- 6L

  n <- length(problems)
  if (n <= 1) {
    return(problems)
  } else if (n == 2) {
    return(paste(problems, collapse = " and "))
  }

  if (n >= MAX_BULLETS) {
    n_more <- length(problems) - MAX_BULLETS + 1L
    problems[[MAX_BULLETS]] <- paste0(n_more, " more")
    length(problems) <- MAX_BULLETS
    n <- MAX_BULLETS
  }

  problems[[n]] <- paste0("and ", problems[[n]])

  paste(problems, collapse = ", ")
}

ensure_full_stop <- function(x) {
  set_names(gsub("(?::|([^.?]))$", "\\1.", x), names(x))
}

set_default_name <- function(x, name) {
  if (is.null(names(x))) {
    names(x) <- rep_along(x, name)
  } else {
    names(x)[names(x) == ""] <- name
  }

  x
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/R/utils-tick.R ---
name_or_pos <- function(names, positions) {
  empty <- (names == "")
  names[!empty] <- tick(names[!empty])
  names[empty] <- positions[empty]
  names
}

# FIXME: Also exists in pillar, do we need to export?
tick <- function(x) {
  ifelse(is.na(x), "NA", encodeString(x, quote = "`"))
}

tick_if_needed <- function(x) {
  needs_ticks <- !is_syntactic(x)
  x[needs_ticks] <- tick(x[needs_ticks])
  x
}

is_syntactic <- function(x) {
  ret <- rep_along(x, FALSE)
  valid <- which(!is.na(x))
  ret[valid] <- is_syntactic_impl(x[valid])
  ret
}

is_syntactic_impl <- function(x) {
  unchanged_after_repair <- (x == make.names(x))
  # https://r.789695.n4.nabble.com/Dots-are-not-fixed-by-make-names-td4752920.html
  dot_dot_dot_or_numbers <- grepl("^(?:(?:[.][.][.])|(?:[.][.][0-9]+))$", x)

  unchanged_after_repair & !dot_dot_dot_or_numbers
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/R/utils.R ---
needs_dim <- function(x) {
  length(dim(x)) > 1L
}

has_null_names <- function(x) {
  is.null(names(x))
}

needs_list_col <- function(x) {
  is_list(x) || !vec_is(x) || vec_size(x) != 1L
}

# Work around bug in R 3.3.0
# Can be ressigned during loading (#544)
safe_match <- match


nchar_width <- function(x) {
  nchar(x, type = "width")
}

is_rstudio <- function() {
  !is.na(Sys.getenv("RSTUDIO", unset = NA))
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/R/view.R ---
#' View an object
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Calls [utils::View()] on the input and returns it, invisibly.
#' If the input is not a data frame, it is processed using a variant of
#' `as.data.frame(head(x, n))`.
#' A message is printed if the number of rows exceeds `n`.
#' This function has no effect in non[interactive] sessions.
#'
#' @details
#' The RStudio IDE overrides `utils::View()`, this is picked up
#' correctly.
#'
#' @param x The object to display.
#' @param title The title to use for the display, by default
#'   the deparsed expression is used.
#' @param ... Unused, must be empty.
#' @param n Maximum number of rows to display. Only used if `x` is not a
#'   data frame. Uses the `view_max` [option][tibble_options] by default.
#'
#' @export
view <- function(x, title = NULL, ..., n = NULL) {
  check_dots_empty()

  if (!is_interactive()) {
    return(invisible(x))
  }

  # The user's expression and the environment to re-evaluate it in
  quo <- enquo0(x)
  expr <- quo_get_expr(quo)
  env <- quo_get_env(quo)

  if (is.null(title)) {
    title <- as_label(expr)
  }

  # Retrieve the `View()` function, which includes the special
  # hooks created by RStudio or Positron
  fn <- get("View", envir = as.environment("package:utils"))

  if (!is.data.frame(x)) {
    return(view_with_coercion(x, n, title, fn))
  }

  # Make a `View()` call that we evaluate in the parent frame,
  # as if the user called `View()` directly rather than `view()`.
  # If `expr` directly references a data frame in the parent frame, this
  # allows RStudio and Positron to "track" that original object
  # for live updates in the data viewer.
  inject((!!fn)(!!expr, !!title), env = env)

  invisible(x)
}

view_with_coercion <- function(x, n, title, fn) {
  if (is.null(n)) {
    n <- get_tibble_option_view_max()
  }

  x <- head(x, n + 1)
  x <- as.data.frame(x)

  if (nrow(x) > n) {
    message("Showing the first ", n, " rows.")
    x <- vec_slice(x, seq_len(n))
  }

  # Since we just created `x`, there won't be anything for
  # RStudio or Positron to "track", so don't even make an effort
  # to try and evaluate in the parent frame with the original
  # expression
  fn(x, title)

  invisible(x)
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/R/zzz.R ---
# nocov start
.onLoad <- function(libname, pkgname) {
  if (getRversion() == "3.3.0") {
    safe_match <<- safe_match_3_0
  } else {
    safe_match <<- safe_match_default
  }

  num <<- pillar::num
  set_num_opts <<- pillar::set_num_opts
  char <<- pillar::char
  set_char_opts <<- pillar::set_char_opts
}

safe_match_3_0 <- function(x, table) {
  match(x, table, incomparables = character())
}

safe_match_default <- function(x, table) {
  match(x, table)
}
# nocov end


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/bench/bench-subsetting.R ---
library(tibble)
library(bench)
library(here)

source(here("bench/fun/subsetting.R"))

df <- tibble(a = 1:10, aa = 1:10, aaa = 1:10)
bm(df)


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/bench/code/compare-df.R ---
library(tibble)
library(dplyr)
library(bench)

source("bench/fun/subsetting.R")

df <- tibble(a = 1:10, aa = 1:10, aaa = 1:10)
b_tibble <- bm(df)

df <- data.frame(a = 1:10, aa = 1:10, aaa = 1:10)
b_df <- bm(df)

b_df %>%
  select(expression, median) %>%
  left_join(b_tibble %>% select(expression, median), by = "expression") %>%
  mutate(ratio = as.numeric(median.y / median.x)) %>%
  arrange(-ratio) %>%
  view()

b_tibble %>% arrange(desc(median))


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/bench/fun/subsetting.R ---
bm <- function(df) {
  idx <- c(rep(TRUE, 5), rep(FALSE, 5))
  bench::mark(
    df[[1]],
    df[[1]] <- 1,
    df[["a"]],
    df[["a"]] <- 1,
    df[["aa"]],
    df[["aa"]] <- 1,
    df[["aaa"]],
    df[["aaa"]] <- 1,
    df[["b"]],

    df$a,
    df$a <- 1,
    df$aa,
    df$aa <- 1,
    df$aaa,
    df$aaa <- 1,

    df["a"],
    df["a"] <- 1,
    df["aa"],
    df["aa"] <- 1,
    df[c("aa", "aaa")],
    df[c("aa", "aaa")] <- 1,
    df[1],
    df[1] <- 1,
    df[2:3],
    df[2:3] <- 1,
    df[TRUE],
    df[TRUE] <- 1,
    df[c(TRUE, FALSE, TRUE)],
    df[c(TRUE, FALSE, TRUE)] <- 1,

    df[, "a"],
    df[, "a"] <- 1,
    df[, "aa"],
    df[, "aa"] <- 1,
    df[, c("aa", "aaa")],
    df[, c("aa", "aaa")] <- 1,
    df[, 1],
    df[, 1] <- 1,
    df[, 2:3],
    df[, 2:3] <- 1,
    df[, TRUE],
    df[, TRUE] <- 1,
    df[, c(TRUE, FALSE, TRUE)],
    df[, c(TRUE, FALSE, TRUE)] <- 1,

    df[1, ],
    df[1, ] <- 1,
    df[3:7, ],
    df[3:7, ] <- 1,
    df[TRUE, ],
    df[TRUE, ] <- 1,
    df[idx, ],
    df[idx, ] <- 1,

    df[1, "a"],
    df[1, "a"] <- 1,
    df[1, "aa"],
    df[1, "aa"] <- 1,
    df[1, c("aa", "aaa")],
    df[1, c("aa", "aaa")] <- 1,
    df[1, 1],
    df[1, 1] <- 1,
    df[1, 2:3],
    df[1, 2:3] <- 1,
    df[1, TRUE],
    df[1, TRUE] <- 1,
    df[1, c(TRUE, FALSE, TRUE)],
    df[1, c(TRUE, FALSE, TRUE)] <- 1,

    df[3:7, "a"],
    df[3:7, "a"] <- 1,
    df[3:7, "aa"],
    df[3:7, "aa"] <- 1,
    df[3:7, c("aa", "aaa")],
    df[3:7, c("aa", "aaa")] <- 1,
    df[3:7, 1],
    df[3:7, 1] <- 1,
    df[3:7, 2:3],
    df[3:7, 2:3] <- 1,
    df[3:7, TRUE],
    df[3:7, TRUE] <- 1,
    df[3:7, c(TRUE, FALSE, TRUE)],
    df[3:7, c(TRUE, FALSE, TRUE)] <- 1,

    df[TRUE, "a"],
    df[TRUE, "a"] <- 1,
    df[TRUE, "aa"],
    df[TRUE, "aa"] <- 1,
    df[TRUE, c("aa", "aaa")],
    df[TRUE, c("aa", "aaa")] <- 1,
    df[TRUE, 1],
    df[TRUE, 1] <- 1,
    df[TRUE, 2:3],
    df[TRUE, 2:3] <- 1,
    df[TRUE, TRUE],
    df[TRUE, TRUE] <- 1,
    df[TRUE, c(TRUE, FALSE, TRUE)],
    df[TRUE, c(TRUE, FALSE, TRUE)] <- 1,

    df[idx, "a"],
    df[idx, "a"] <- 1,
    df[idx, "aa"],
    df[idx, "aa"] <- 1,
    df[idx, c("aa", "aaa")],
    df[idx, c("aa", "aaa")] <- 1,
    df[idx, 1],
    df[idx, 1] <- 1,
    df[idx, 2:3],
    df[idx, 2:3] <- 1,
    df[idx, TRUE],
    df[idx, TRUE] <- 1,
    df[idx, c(TRUE, FALSE, TRUE)],
    df[idx, c(TRUE, FALSE, TRUE)] <- 1,

    check = FALSE,
    iterations = 2000
  )
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/revdep/check.R ---
if (Sys.getenv("DISPLAY") == "") stop("Run with xvfb-run")

free <- system(paste0("df --output=avail ", tempdir(), " | tail -n 1"), intern = TRUE)
if (as.numeric(free) < 1e8) stop("Set TMPDIR to a location with at least 100 GB free space")

package <- basename(getwd())

library(revdepcheck)

dir_setup(getwd())
if (!revdepcheck:::db_exists(getwd())) {
  revdepcheck:::db_setup(getwd())
}


if (length(revdep_todo()) == 0) {
  import_revdeps <- revdepcheck:::cran_revdeps(package = package, dependencies = c("Depends", "Imports"), bioc = TRUE)
  import_revdeps <- setdiff(import_revdeps, package)
  todo_import_revdeps <- import_revdeps

  while (FALSE && length(todo_import_revdeps) > 0) {
    print(length(todo_import_revdeps))
    print(todo_import_revdeps)
    print(Sys.time())
    new_import_revdeps <- unlist(purrr::map(todo_import_revdeps, revdepcheck:::cran_revdeps, dependencies = c("Depends", "Imports"), bioc = TRUE))
    todo_import_revdeps <- setdiff(new_import_revdeps, import_revdeps)
    import_revdeps <- union(import_revdeps, new_import_revdeps)
    print(new_import_revdeps)

    break # only one level for now
  }

  weak_revdeps <- revdepcheck:::cran_revdeps(package = package, dependencies = c("Suggests", "Enhances", "LinkingTo"), bioc = TRUE)
  print(weak_revdeps)

  revdep_add(".", c(import_revdeps, weak_revdeps))
}

N <- 100
for (i in seq_len(N)) {
  try(
    revdepcheck::revdep_check(
      bioc = TRUE,
      dependencies = character(),
      quiet = FALSE,
      num_workers = 24,
      timeout = as.difftime(60, units = "mins")
    )
  )

  if (length(revdep_todo()) == 0) break
}

withr::with_output_sink(
  "revdep/cran.md",
  revdep_report_cran()
)

system("git add revdep/*.md")
system("git commit -m 'update revdep results'")
system("git push -u origin HEAD")


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/revdep/post-mortem/01-load.R ---
library(tidyverse)

new_error <- "BAwiR CGPfunctions DMwR2 INDperform MIMSunit OpenLand REDCapR RSDA RmarineHeatWaves SWMPrExtension SanzCircos SortedEffects anchoredDistr apa basket beadplexr biscale brazilmaps casen cdcfluview collateral concaveman concurve convergEU cutpointr cvms dscore epikit estatapi evaluator fable forestmangr germanpolls graphicalVAR heemod highlightHTML ijtiff ipfr jstor mcp metacoder micropan modeltests nationwider nhdplusTools nosoi openair padr pointblank portalr psychonetrics readroper rematch2 riskclustr rsample rubias simrel sjmisc ssdtools statsr taxa tidybayes tidytext tidytransit tradestatistics trialr ushr viafr vip vpc weathercan"
new_warning <- "RNeXML Rdrools analysisPipelines broom.mixed cdcfluview coveffectsplot dexter dialr dodgr highlightHTML metan naniar ozmaps photobiologyInOut poio raceland sigmajs tbrf tidydice tidytree"

input <- tribble(
  ~type,     ~pkg,
  "error", new_error,
  "warning", new_warning
)

long_pkg <-
  input %>%
  mutate(pkg = strsplit(pkg, " ")) %>%
  unnest(pkg) %>%
  distinct(pkg, .keep_all = TRUE) %>%
  filter(!(pkg %in% c("cutpointr", "tidytransit")))

long_pkg %>%
  count(pkg) %>%
  count(n)

fog <- foghorn::cran_details(long_pkg$pkg, src = "crandb")

saveRDS(fog, "fog.rds")


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/revdep/post-mortem/02-clean.R ---
library(tidyverse)

fog_res <- readRDS("fog_res.rds")

fog_res %>%
  arrange(-error, -fail, -warn, -note, !has_other_issues) %>%
  view()

fog <- readRDS("fog.rds")

checked_manually <- c("cdcfluview", "metan", "openair", "vip")

fog_recent <-
  fog %>%
  filter(!(package %in% checked_manually)) %>%
  mutate(version = package_version(version)) %>%
  group_by(package) %>%
  filter(version == max(version)) %>%
  ungroup() %>%
  mutate(result = ordered(result, levels = c("ERROR", "WARN", "NOTE"))) %>%
  group_by(package) %>%
  filter(result == min(result)) %>%
  ungroup() %>%
  mutate(
    check = case_when(
      str_detect(check, "^running tests for arch") ~ "tests",
      check == "re-building of vignette outputs" ~ "vignettes",
      TRUE ~ check
    )
  )

fog_recent %>%
  count(check, wt = n_flavors) %>%
  arrange(-n)

# RNeXML, fixed by tibble 3.0.1
fog_recent %>%
  filter(check == "whether the namespace can be unloaded cleanly")

# No failures
fog_recent %>%
  filter(check == "whether package can be installed") %>%
  filter(flavors != "r-release-windows-ix86+x86_64") %>%
  pull(package)

# Not us
fog_recent %>%
  filter(check == "package dependencies") %>%
  pull()

# Not us
fog_recent %>%
  filter(check == "dependencies in R code") %>%
  pull()

fog_recent %>%
  filter(check %in% c("tests", "examples", "vignettes")) %>%
  saveRDS("fog_recent.rds")


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/revdep/post-mortem/03-analyze.R ---
library(tidyverse)
library(tidytext)


fog_recent <- readRDS("fog_recent.rds")

fog_words <-
  fog_recent %>%
  select(package, flavors, n = n_flavors, message) %>%
  rowid_to_column() %>%
  mutate(message = str_replace_all(message, "[0-9]", "")) %>%
  unnest_tokens(token, message, "words", stopwords = c("s", "in", "re", "x", "i", "tibble", "be"))

fog_words %>%
  count(token, rowid, wt = n) %>%
  bind_tf_idf(token, rowid, n) %>%
  group_by(rowid) %>%
  filter(row_number(-tf_idf) %in% 1:3) %>%
  ungroup() %>%
  count(token, sort = TRUE)

fog_ngrams <-
  fog_recent %>%
  select(package, flavors, n = n_flavors, message) %>%
  rowid_to_column() %>%
  mutate(message = str_replace_all(message, "[0-9]", "")) %>%
  unnest_tokens(token, message, "ngrams", n = 2, stopwords = c("s", "in", "re", "x", "i", "tibble", "be"))

fog_ngrams_tf_idf <-
  fog_ngrams %>%
  count(token, rowid, wt = n) %>%
  bind_tf_idf(token, rowid, n)

fog_ngrams_tf_idf %>%
  filter(between(percent_rank(tf_idf), 0.25, 0.75))



# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/revdep/post-mortem/04-review.R ---
library(tidyverse)
library(tidytext)


fog_recent <- readRDS("fog_recent.rds")

fog_recent %>%
  count(message) %>%
  count(n)

tr_normal <- function(x) {
  unicode_symbols <- map(cli:::symbol_utf8, rex::escape)
  normal_symbols <- rlang::with_options(cli.unicode = FALSE, cli::symbol)

  reduce2(unicode_symbols, normal_symbols, str_replace_all, .init = x)
}

clean_timings <- function(x) {
  str_replace_all(x, "\\[[0-9]+s(?:/[0-9]+s)?\\]", "[timing]")
}

clean_testthat_summary <- function(x) {
  str_replace_all(x, "\\[ OK:[^\n]+ \\]", "[testthat summary]")
}

clean_backtrace <- function(x) {
  str_replace_all(x, "( +[1-9][0-9]*[.] +(?:[\\\\|+└│├].*| *)\n)+", "[backtrace]\n")
}

clean_pre_backtrace <- function(x) {
  str_replace_all(x, "[x#]\n", "[pre-backtrace]\n")
}

clean_ptime <- function(x) {
  str_replace_all(x, "     [>] base::assign.*\n", "")
}

clean_paths <- function(x) {
  str_replace_all(x, "/home/hornik/tmp/[^\n]+/Work/build/Packages/|/data/gannet/ripley/R/packages/[^\n]+[.]Rcheck/|/home/ripley/R/Lib32-dev/|D:/temp/[^\n]+/RLIBS_[^\n/]+/", "[path]/")
}

fog_clean <-
  fog_recent %>%
  mutate(message = tr_normal(message)) %>%
  mutate(message = clean_timings(message)) %>%
  mutate(message = clean_testthat_summary(message)) %>%
  mutate(message = clean_backtrace(message)) %>%
  mutate(message = clean_pre_backtrace(message)) %>%
  mutate(message = clean_ptime(message)) %>%
  mutate(message = clean_paths(message)) %>%
  group_by(package, result, check, message) %>%
  summarize(
    flavors = paste0(flavors, collapse = ", "),
    n_flavors = sum(n_flavors),
    version = version[[1]]
  ) %>%
  ungroup()

unlink("msg", recursive = TRUE)
dir.create("msg", showWarnings = FALSE)

fog_clean %>%
  filter(check != "vignettes") %>%
  rowid_to_column() %>%
  transmute(text = message, path = sprintf("msg/%04d-%s.txt", rowid, package)) %>%
  pwalk(brio::write_lines)


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/revdep/post-mortem/05-count.R ---
library(tidyverse)
library(tidytext)


fog_recent <- readRDS("fog_recent.rds")


fog_dump <-
  fog_recent %>%
  group_by(package) %>%
  summarize(message = glue::glue_collapse(message, sep = "\n")) %>%
  ungroup()

# wide vector
fog_dump %>%
  filter(str_detect(message, fixed("1 row must be assigned")))

# array indexing
fog_dump %>%
  filter(str_detect(message, fixed("must have one dimension, not 2")))

fog_dump %>%
  filter(package == "naniar") %>%
  pull(message) %>%
  cli::cat_line()

# compare unnamed, perhaps ensure that names are not added to tibble
# if not intended?
fog_dump %>%
  count(str_detect(message, fixed("names for")))

# load sf prior to adding to tibble
fog_dump %>%
  filter(str_detect(message, fixed("sfc"))) %>%
  pull(message) %>%
  cli::cat_line()

fog_dump %>%
  filter(str_detect(message, fixed("Input must be a vector")))

fog_dump %>%
  count(str_detect(message, "Lossy cast from .* to .* <logical>"))


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/revdep/post-mortem/06-mail.R ---
library(tidyverse)

fog <- readRDS("fog.rds")

fog_pkg <-
  fog %>%
  count(package) %>%
  pull(package)

maint <- tools:::CRAN_package_maintainers_db()

maint %>%
  filter(Package %in% !!fog_pkg) %>%
  count(Maintainer) %>%
  pull(Maintainer) %>%
  clipr::write_clip()


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/revdep/problem-analyze.R ---
library(tidyverse)

problems <- readLines("revdep/new-problems.md")

doc_frame <-
  problems %>%
  enframe() %>%
  mutate(is_package = grepl("^# [a-zA-Z][a-zA-Z0-9.]*[a-zA-Z0-9]$", value)) %>%
  mutate(package_id = cumsum(is_package)) %>%
  group_by(package_id) %>%
  mutate(package_name = gsub("^# ", "", value[[1]])) %>%
  ungroup() %>%
  select(package_name, text = value)

doc_frame %>%
  filter(grepl("must be named|error_column_must_be_named", text)) %>%
  count(package_name)

doc_frame_2 <-
  doc_frame %>%
  group_by(package_name) %>%
  mutate(flag = any(grepl("must be named|error_column_must_be_named", text))) %>%
  ungroup() %>%
  filter(!flag) %>%
  select(-flag)

doc_frame_2 %>%
  filter(grepl("scalar integer", text)) %>%
  count(package_name)

doc_frame_3 <-
  doc_frame_2 %>%
  group_by(package_name) %>%
  mutate(flag = any(grepl("scalar integer", text))) %>%
  ungroup() %>%
  filter(!flag) %>%
  select(-flag)

doc_frame_3 %>%
  filter(grepl("`names` must have length", text)) %>%
  count(package_name)


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/tests/testthat.R ---
library("testthat")

test_check("tibble")


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/tests/testthat/helper-data.R ---
# A data frame with all major types
df_all <- tibble(
  a = c(1, 2.5, NA),
  b = c(1:2, NA),
  c = c(T, F, NA),
  d = c("a", "b", NA),
  e = factor(c("a", "b", NA)),
  f = as.Date("2015-12-09") + c(1:2, NA),
  g = as.POSIXct("2015-12-09 10:51:34 UTC") + c(1:2, NA),
  h = as.list(c(1:2, NA)),
  i = list(list(1, 2:3), list(4:6), list(NA))
)

# An empty data frame with all major types
df_empty <- tibble(
  a = integer(0),
  b = double(0),
  c = logical(0),
  d = character(0),
  e = factor(integer(0)),
  f = as.Date(character(0)),
  g = as.POSIXct(character(0)),
  h = as.list(double(0)),
  # i = list(list(integer(0)), list(character(0))),
  to_be_added = double(0)
)


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/tests/testthat/helper-encoding.R ---
get_lang_strings <- function() {
  lang_strings <- c(
    de = "Gl\u00fcck",
    cn = "\u5e78\u798f",
    ru = "\u0441\u0447\u0430\u0441\u0442\u044c\u0435",
    ko = "\ud589\ubcf5"
  )

  native_lang_strings <- enc2native(lang_strings)

  same <- (lang_strings == native_lang_strings)

  list(
    same = lang_strings[same],
    different = lang_strings[!same]
  )
}

get_native_lang_string <- function() {
  lang_strings <- get_lang_strings()
  if (length(lang_strings$same) == 0) {
    testthat::skip("No native language string available")
  }
  lang_strings$same[[1L]]
}

get_alien_lang_string <- function() {
  lang_strings <- get_lang_strings()
  if (length(lang_strings$different) == 0) {
    testthat::skip("No alien language string available")
  }
  lang_strings$different[[1L]]
}

with_non_utf8_locale <- function(code) {
  old_locale <- set_non_utf8_locale()
  on.exit(set_locale(old_locale), add = TRUE)
  code
}

set_non_utf8_locale <- function() {
  if (.Platform$OS.type == "windows") {
    return(NULL)
  }
  tryCatch(
    locale <- set_locale("en_US.ISO88591"),
    warning = function(e) {
      testthat::skip("Cannot set latin-1 locale")
    }
  )
  locale
}

set_locale <- function(locale) {
  if (is.null(locale)) {
    return(NULL)
  }
  locale <- Sys.getlocale("LC_CTYPE")
  Sys.setlocale("LC_CTYPE", locale)
  locale
}

skip_on_non_utf8_locale <- function() {
  if (!l10n_info()$"UTF-8") {
    skip("Non-UTF-8 locale")
  }
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/tests/testthat/helper-error.R ---
get_defunct_error_class <- function() {
  "lifecycle_error_deprecated"
}

# Dummy to remind us to keep tests and verifications in sync
verify_errors <- identity

print_error <- function(expr) {
  print(expect_error(expr), backtrace = FALSE)
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/tests/testthat/helper-expectations.R ---
expect_tibble_abort <- function(object, error, fixed = NULL) {
  cnd <- tryCatch(error, error = identity)
  expect_tibble_error(object, cnd, fixed = fixed)
}

expect_tibble_error <- function(object, cnd, fixed = NULL) {
  cnd_actual <- expect_error(object, class = class(cnd)[[1]])
  expect_cnd_equivalent(cnd_actual, cnd)
  expect_s3_class(cnd_actual, class(cnd), exact = TRUE)
}

expect_cnd_equivalent <- function(actual, expected) {
  actual$trace <- NULL
  actual$parent <- NULL
  actual$body <- NULL
  actual$call <- NULL
  expected$trace <- NULL
  expected$parent <- NULL
  expected$body <- NULL
  expected$call <- NULL
  expect_equal(actual, expected)
}

rlang_variant <- function() {
  NULL
}

rlang_pillar_variant <- function() {
  NULL
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/tests/testthat/helper-my.R ---
mychr <- function(x) {
  structure(x, class = c("mychr", "character"))
}
vec_ptype2.mychr.character <- function(x, y, ...) {
  x
}
vec_cast.character.mychr <- function(x, to, ...) {
  unclass(x)
}

# Explicit method required for R CMD check:
vctrs::s3_register(
  "vctrs::vec_ptype2",
  "mychr.character",
  vec_ptype2.mychr.character
)
vctrs::s3_register(
  "vctrs::vec_cast",
  "character.mychr",
  vec_cast.character.mychr
)

myint <- function(x) {
  structure(x, class = c("myint", "integer"))
}
vec_ptype2.myint.integer <- function(x, y, ...) {
  x
}
vec_cast.integer.myint <- function(x, to, ...) {
  as.integer(unclass(x))
}

# Explicit method required for R CMD check:
vctrs::s3_register(
  "vctrs::vec_ptype2",
  "myint.integer",
  vec_ptype2.myint.integer
)
vctrs::s3_register("vctrs::vec_cast", "integer.myint", vec_cast.integer.myint)

mylgl <- function(x) {
  structure(x, class = c("mylgl", "logical"))
}
vec_ptype2.mylgl.logical <- function(x, y, ...) {
  x
}
vec_cast.logical.mylgl <- function(x, to, ...) {
  unclass(x)
}

# Explicit method required for R CMD check:
vctrs::s3_register(
  "vctrs::vec_ptype2",
  "mylgl.logical",
  vec_ptype2.mylgl.logical
)
vctrs::s3_register("vctrs::vec_cast", "logical.mylgl", vec_cast.logical.mylgl)


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/tests/testthat/helper-output.R ---
unell <- function(x) {
  gsub(cli::symbol$ellipsis, "...", x, fixed = TRUE)
}

unell_bullets <- function(...) {
  unell(bullets(...))
}

unell_commas <- function(...) {
  unell(commas(...))
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/tests/testthat/helper-pillar.R ---
print_without_body <- function(x, ...) {
  class(x) <- c("tbl_df_without_body", class(x))
  print(x, ...)
}

tbl_format_body.tbl_df_without_body <- function(x, ...) {
  "<body created by pillar>"
}

# Need explicit method because can't be found in method env
vctrs::s3_register(
  "pillar::tbl_format_body",
  "tbl_df_without_body",
  tbl_format_body.tbl_df_without_body
)


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/tests/testthat/helper-sync.R ---
dir.create("vignettes", showWarnings = FALSE)
for (file in list.files(
  "../../vignettes",
  pattern = "[.]Rmd$",
  full.names = TRUE
)) {
  text <- readLines(file)
  text <- c(
    text[1],
    "# Generated by helper-sync.R, do not edit by hand",
    text[-1]
  )
  writeLines(text, file.path("vignettes", basename(file)))
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/tests/testthat/helper-testthat.R ---
options(testthat.progress.verbose_skips = FALSE)
# options(Ncpus = parallel::detectCores())


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/tests/testthat/helper-type-sum.R ---
as_override_type_sum <- function(x) {
  structure(x, class = "override_type_sum")
}

type_sum.override_type_sum <- function(x, ...) {
  "SC"
}

registerS3method(
  "type_sum",
  "override_type_sum",
  type_sum.override_type_sum,
  envir = asNamespace("pillar")
)

`[.override_type_sum` <- function(x, ...) {
  as_override_type_sum(NextMethod())
}

registerS3method(
  "[",
  "override_type_sum",
  `[.override_type_sum`,
  envir = asNamespace("tibble")
)

as_override_tbl_sum <- function(x) {
  structure(x, class = c("override_tbl_sum", class(x)))
}

tbl_sum.override_tbl_sum <- function(x) {
  c(NextMethod(), "Overridden" = "tbl_sum")
}

registerS3method(
  "tbl_sum",
  "override_tbl_sum",
  tbl_sum.override_tbl_sum,
  envir = asNamespace("tibble")
)


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/tests/testthat/helper-unknown-rows.R ---
as_unknown_rows <- function(x) {
  x <- as_tibble(x)
  class(x) <- c("unknown_rows", class(x))
  x
}

dim.unknown_rows <- function(x) {
  c(NA_integer_, length(x))
}

registerS3method("dim", "unknown_rows", dim.unknown_rows)

head.unknown_rows <- function(x, n) {
  head(as.data.frame(x), n)
}

registerS3method("head", "unknown_rows", head.unknown_rows)


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/tests/testthat/helper-zzz.R ---
expect_legacy_error <- function(code, ...) {
  expect_error(code)
}

expect_legacy_warning <- function(code, ...) {
  suppressWarnings(expect_warning(code))
}

skip_legacy <- function() {
  skip_if(packageVersion("tibble") >= "2.99.99")
}

skip_brk_add_row_vctrs <- function() {
  # BRK: add_row() uses vctrs coercion rules
  skip_legacy()
}

skip_brk_auto_splice_anonymous <- function() {
  # ENH: tibble() auto-splices anonymous tibble arguments
  skip_legacy()
}

skip_brk_inner_data_frames <- function() {
  # BRK: data frame columns are no longer coerced to tibbles
  skip_legacy()
}

skip_brk_inner_names_not_stripped <- function() {
  # BRK: inner names not stripped from coluns
  skip_legacy()
}

skip_brk_inner_dim_not_stripped <- function() {
  # BRK: inner dim not stripped from coluns
  skip_legacy()
}

skip_brk_no_recursive_indexing <- function() {
  # BRK: no recursive indexing in [[
  skip_legacy()
}

skip_brk_logical_subsetting_no_base_recycling <- function() {
  # BRK: logical indexes must be length one or match the length
  skip_legacy()
}

skip_brk_character_subsetting_no_negative <- function() {
  # BRK: character subsetting for rows no longer supports negative numbers
  skip_legacy()
}

skip_dep_oob_subsetting_warning <- function() {
  # DEP: numeric indexes give warning when indexing OOB
  skip_legacy()
}

skip_dep_rowname_subsetting_warning <- function() {
  # DEP: character indexes give warning with OOB matching
  skip_legacy()
}

skip_dep_new_tibble_subclass <- function() {
  # DEP: new_tibble() warns with subclass argument
  skip_legacy()
}

skip_dep_glimpse <- function() {
  # DEP: glimpse() and format_v() now in pillar
  skip_legacy()
}

skip_enh_posixlt_supported <- function() {
  # ENH: POSIXlt supported
  skip_legacy()
}

skip_enh_as_tibble_rownames <- function() {
  # ENH: rownames argument to as_tibble() works if data frame or matrix doesn't have row names
  skip_legacy()
}

skip_enh_tibble_null <- function() {
  # ENH: NULL arguments to tibble() are silently removed
  skip_legacy()
}

skip_enh_new_tibble_nrow_null <- function() {
  # ENH: new_tibble(nrow = NULL), #781
  skip_legacy()
}

skip_enh_empty_tribble_unspecified <- function() {
  # ENH: zero-row tribbles create unspecified columns
  skip_legacy()
}

skip_enh_as_tibble_retired <- function() {
  # ENH: retiring as_tibble() for vectors and lists, #447
  skip_legacy()
}

skip_enh_bullets_format <- function() {
  # ENH: new bullets format
  skip_legacy()
}

skip_enh_enframe_vector <- function() {
  # ENH: enframe() supports all vectors (#730)
  skip_legacy()
}

skip_enh_print_tbl_args <- function() {
  # ENH: print() and format() support more arguments
  skip_legacy()
}

skip_int_error_unknown_names <- function() {
  # INT: error_unknown_names() no longer implemented
  skip_legacy()
}

skip_int_error_names_must_be_null <- function() {
  # INT: error_names_must_be_null() no longer implemented
  skip_legacy()
}

skip_int_data_frame_tibble_diff <- function() {
  # INT: changed data_frame_() implementation to support custom deprecation warning
  skip_legacy()
}

skip_int_lifecycle <- function() {
  # INT: lifecycle changes classes for deprecation messages in R < 3.6
  skip_legacy()
}

universal_names <- function(...) {
  # INT: universal_names() no longer implemented
  skip_legacy()
}

set_universal_names <- function(...) {
  # INT: set_universal_names() no longer implemented
  skip_legacy()
}

unique_names <- function(...) {
  # INT: unique_names() no longer implemented
  skip_legacy()
}

minimal_names <- function(...) {
  # INT: minimal_names() no longer implemented
  skip_legacy()
}

set_minimal_names <- function(...) {
  # INT: set_minimal_names() no longer implemented
  skip_legacy()
}

make_syntactic <- function(...) {
  # INT: make_syntactic() no longer implemented
  skip_legacy()
}

make_unique <- function(...) {
  # INT: make_unique() no longer implemented
  skip_legacy()
}

two_to_three_dots <- function(...) {
  # INT: two_to_three_dots() no longer implemented
  skip_legacy()
}

expect_output <- function(...) {
  suppressWarnings(testthat::expect_output(...))
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/tests/testthat/setup.R ---
colon_colon <- `::`

`::` <- function(x, y) {
  x_sym <- ensym(x)
  y_sym <- ensym(y)
  tryCatch(
    inject(colon_colon(!!x_sym, !!y_sym)),
    packageNotFoundError = function(e) {
      skip_if_not_installed(as_string(x_sym))
    }
  )
}

if (getRversion() >= "3.6") {
  local_options(
    warnPartialMatchArgs = TRUE,
    warnPartialMatchAttr = TRUE,
    warnPartialMatchDollar = TRUE,
    .frame = testthat::teardown_env()
  )
} else {
  local_options(
    warnPartialMatchAttr = TRUE,
    warnPartialMatchDollar = TRUE,
    .frame = testthat::teardown_env()
  )
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/tests/testthat/test-add.R ---
# add_row ---------------------------------------------------------------

test_that("can add new row", {
  df_all_new <- add_row(df_all, a = 4, b = 3L)
  expect_identical(colnames(df_all_new), colnames(df_all))
  expect_identical(nrow(df_all_new), nrow(df_all) + 1L)
  expect_identical(df_all_new$a, c(df_all$a, 4))
  expect_identical(df_all_new$b, c(df_all$b, 3L))
  expect_identical(df_all_new$c, c(df_all$c, NA))
})

test_that("add_row() keeps class of object", {
  trees_new <- add_row(trees, Volume = NA)
  expect_equal(class(trees), class(trees_new))

  trees_new <- add_row(as_tibble(trees), Volume = NA)
  expect_equal(class(as_tibble(trees)), class(trees_new))
})

test_that("add_row() keeps class of object when adding in the middle", {
  trees_new <- add_row(trees, Volume = NA, .after = 10)
  expect_equal(class(trees), class(trees_new))

  trees_new <- add_row(as_tibble(trees), Volume = NA)
  expect_equal(class(as_tibble(trees)), class(trees_new))
})

test_that("add_row() keeps class of object when adding in the beginning", {
  trees_new <- add_row(trees, Volume = NA, .after = 0)
  expect_equal(class(trees), class(trees_new))

  trees_new <- add_row(as_tibble(trees), Volume = NA)
  expect_equal(class(as_tibble(trees)), class(trees_new))
})

test_that("adds empty row if no arguments", {
  trees1 <- add_row(trees)
  expect_equal(nrow(trees1), nrow(trees) + 1)
  new_trees_row <- trees1[nrow(trees1), , drop = TRUE]
  expect_true(all(is.na(new_trees_row)))
})

test_that("error if adding row with unknown variables", {
  expect_tibble_abort(
    add_row(tibble(a = 3), xxyzy = "err"),
    abort_incompatible_new_rows("xxyzy")
  )

  expect_tibble_abort(
    add_row(tibble(a = 3), b = "err", c = "oops"),
    abort_incompatible_new_rows(c("b", "c"))
  )
})

test_that("deprecated adding rows to non-data-frames", {
  expect_error(
    expect_warning(add_row(as.matrix(mtcars), mpg = 4))
  )
})

test_that("can add multiple rows", {
  df <- tibble(a = 3L)
  df_new <- add_row(df, a = 4:5)
  expect_identical(df_new, tibble(a = 3:5))
})

test_that("can recycle when adding rows", {
  trees_new <- add_row(trees, Height = -1:-2, Volume = 2:3)
  expect_identical(nrow(trees_new), nrow(trees) + 2L)
  expect_identical(trees_new$Height, c(trees$Height, -1:-2))
  expect_identical(
    trees_new$Volume,
    c(trees$Volume, 2:3)
  )
})

test_that("can add as first row via .before = 1", {
  df <- tibble(a = 3L)
  df_new <- add_row(df, a = 2L, .before = 1)
  expect_identical(df_new, tibble(a = 2:3))
})

test_that("can add as first row via .after = 0", {
  df <- tibble(a = 3L)
  df_new <- add_row(df, a = 2L, .after = 0)
  expect_identical(df_new, tibble(a = 2:3))
})

test_that("can add row inbetween", {
  df <- tibble(a = 1:3)
  df_new <- add_row(df, a = 4:5, .after = 2)
  expect_identical(df_new, tibble(a = c(1:2, 4:5, 3L)))
})

test_that("can safely add to factor columns everywhere (#296)", {
  df <- tibble(a = factor(letters[1:3]))
  expect_identical(add_row(df), tibble(a = factor(c(letters[1:3], NA))))
  expect_identical(
    add_row(df, .before = 1),
    tibble(a = factor(c(NA, letters[1:3])))
  )
  expect_identical(
    add_row(df, .before = 2),
    tibble(a = factor(c("a", NA, letters[2:3])))
  )
  expect_identical(add_row(df, a = "d"), tibble(a = c(letters[1:4])))
  expect_identical(
    add_row(df, a = "d", .before = 1),
    tibble(a = c("d", letters[1:3]))
  )
  expect_identical(
    add_row(df, a = "d", .before = 2),
    tibble(a = c("a", "d", letters[2:3]))
  )
})

test_that("error if both .before and .after are given", {
  df <- tibble(a = 1:3)
  expect_tibble_abort(
    add_row(df, a = 4:5, .after = 2, .before = 3),
    abort_both_before_after()
  )
})

test_that("missing row names stay missing when adding row", {
  expect_false(has_rownames(trees))
  expect_false(has_rownames(add_row(trees, Volume = NA, .after = 0)))
  expect_false(has_rownames(add_row(trees, Volume = NA, .after = nrow(trees))))
  expect_false(has_rownames(add_row(trees, Volume = NA, .before = 10)))
})

test_that("adding to a list column adds a NULL value (#148)", {
  expect_null(add_row(tibble(a = as.list(1:3)))$a[[4]])
  expect_null(add_row(tibble(a = as.list(1:3)), .before = 1)$a[[1]])
  expect_null(add_row(tibble(a = as.list(1:3)), .after = 1)$a[[2]])
  expect_null(add_row(tibble(a = as.list(1:3), b = 1:3), b = 4:6)$a[[5]])
})

test_that("add_row() keeps the class of empty columns", {
  new_tibble <- add_row(df_empty, to_be_added = 5)
  expect_equal(sapply(df_empty, class), sapply(new_tibble, class))
})

test_that("add_row() fails nicely for grouped data frames (#179)", {
  skip_if_not_installed("dplyr")
  expect_tibble_abort(
    add_row(dplyr::group_by(trees, Volume), Height = 3),
    abort_add_rows_to_grouped_df()
  )
})

test_that("add_row() works when adding zero row input (#809)", {
  x <- tibble(x = 1, y = 2)
  y <- tibble(y = double())

  expect_identical(add_row(x, x = double()), x)
  expect_identical(add_row(x, y), x)
  expect_identical(add_row(x, NULL), x)
  expect_identical(add_row(x, ), x)
})

# add_column ------------------------------------------------------------

test_that("can add new column", {
  df_all_new <- add_column(df_all, j = 1:3, k = 3:1)
  expect_identical(nrow(df_all_new), nrow(df_all))

  # Since the switch to vctrs the `tzone` attribute is set in
  # `df_all_new`. Test with `equal` instead of `identical`. Also
  # `dplyr:::all.equal.tbl_df()` somehow fails with a type error
  # (dplyr 0.8.3), so we bypass the method.
  expect_true(all.equal.default(df_all_new[seq_along(df_all)], df_all))

  expect_identical(df_all_new$j, 1:3)
  expect_identical(df_all_new$k, 3:1)
})

test_that("add_column() works with 0-col tibbles (#786)", {
  local_options(lifecycle_verbosity = "error")

  expect_identical(
    add_column(new_tibble(list(), nrow = 1), a = 1),
    tibble(a = 1)
  )
})

test_that("add_column() keeps class of object", {
  trees_new <- add_column(trees, x = 1:31)
  expect_equal(class(trees), class(trees_new))

  trees_new <- add_column(as_tibble(trees), x = 1:31)
  expect_equal(class(as_tibble(trees)), class(trees_new))
})

test_that("add_column() keeps class of object when adding in the middle", {
  trees_new <- add_column(trees, x = 1:31, .after = 3)
  expect_equal(class(trees), class(trees_new))

  trees_new <- add_column(as_tibble(trees), x = 1:31)
  expect_equal(class(as_tibble(trees)), class(trees_new))
})

test_that("add_column() keeps class of object when adding in the beginning", {
  trees_new <- add_column(trees, x = 1:31, .after = 0)
  expect_equal(class(trees), class(trees_new))

  trees_new <- add_column(as_tibble(trees), x = 1:31)
  expect_equal(class(as_tibble(trees)), class(trees_new))
})

test_that("add_column() keeps unchanged if no arguments", {
  expect_identical(trees, add_column(trees))
})

test_that("add_column() can add to empty tibble or data frame", {
  expect_identical(add_column(tibble(.rows = 3), a = 1:3), tibble(a = 1:3))
  expect_identical(
    add_column(as.data.frame(tibble(.rows = 3)), a = 1:3),
    data.frame(a = 1:3)
  )
})

test_that("error if adding existing columns", {
  expect_tibble_abort(
    add_column(tibble(a = 3), a = 5),
    abort_column_names_must_be_unique("a", repair_hint = TRUE)
  )
})

test_that("error if adding wrong number of rows with add_column()", {
  expect_tibble_abort(
    add_column(tibble(a = 3), b = 4:5),
    abort_incompatible_new_cols(1, data.frame(b = 4:5))
  )
})

test_that("can add multiple columns", {
  df <- tibble(a = 1:3)
  df_new <- add_column(df, b = 4:6, c = 3:1)
  expect_identical(df_new, tibble(a = 1:3, b = 4:6, c = 3:1))
})

test_that("can recycle when adding columns", {
  df <- tibble(a = 1:3)
  df_new <- add_column(df, b = 4, c = 3:1)
  expect_identical(df_new, tibble(a = 1:3, b = rep(4, 3), c = 3:1))
})

test_that("can recycle when adding a column of length 1", {
  df <- tibble(a = 1:3)
  df_new <- add_column(df, b = 4)
  expect_identical(df_new, tibble(a = 1:3, b = rep(4, 3)))
})

test_that("can recyle when adding multiple columns of length 1", {
  df <- tibble(a = 1:3)
  df_new <- add_column(df, b = 4, c = 5)
  expect_identical(df_new, tibble(a = 1:3, b = rep(4, 3), c = rep(5, 3)))
})

test_that("can recyle for zero-row data frame (#167)", {
  df <- tibble(a = 1:3)[0, ]
  df_new <- add_column(df, b = 4, c = character())
  expect_identical(
    df_new,
    tibble(a = integer(), b = numeric(), c = character())
  )
})

test_that("can add as first column via .before = 1", {
  df <- tibble(a = 3L)
  df_new <- add_column(df, b = 2L, .before = 1)
  expect_identical(df_new, tibble(b = 2L, a = 3L))
})

test_that("can add as first column via .after = 0", {
  df <- tibble(a = 3L)
  df_new <- add_column(df, b = 2L, .after = 0)
  expect_identical(df_new, tibble(b = 2L, a = 3L))
})

test_that("can add column inbetween", {
  df <- tibble(a = 1:3, c = 4:6)
  df_new <- add_column(df, b = -1:1, .after = 1)
  expect_identical(df_new, tibble(a = 1:3, b = -1:1, c = 4:6))
})

test_that("can add column relative to named column", {
  df <- tibble(a = 1:3, c = 4:6)
  df_new <- add_column(df, b = -1:1, .before = "c")
  expect_identical(df_new, tibble(a = 1:3, b = -1:1, c = 4:6))
})

test_that("error if both .before and .after are given", {
  df <- tibble(a = 1:3)
  expect_tibble_abort(
    add_column(df, b = 4:6, .after = 2, .before = 3),
    abort_both_before_after()
  )
})

test_that("error if column named by .before or .after not found", {
  df <- tibble(a = 1:3)
  expect_tibble_abort(
    add_column(df, b = 4:6, .after = "x"),
    abort_unknown_column_names("x")
  )
  expect_tibble_abort(
    add_column(df, b = 4:6, .before = "x"),
    abort_unknown_column_names("x")
  )
})

test_that("deprecated adding columns to non-data-frames", {
  expect_error(
    # Two lifecycle warnings, requires testthat > 2.3.2:
    suppressWarnings(
      expect_warning(add_column(as.matrix(mtcars), x = 1))
    )
  )
})

test_that("missing row names stay missing when adding column", {
  expect_false(has_rownames(trees))
  expect_false(has_rownames(add_column(trees, x = 1:31, .after = 0)))
  expect_false(has_rownames(add_column(trees, x = 1:31, .after = ncol(trees))))
  expect_false(has_rownames(add_column(trees, x = 1:31, .before = 2)))
})

test_that("output test", {
  expect_snapshot(error = TRUE, {
    add_row(tibble(), a = 1)
    add_row(tibble(), a = 1, b = 2)
    add_row(tibble(), !!!set_names(letters))
    add_row(dplyr::group_by(tibble(a = 1), a))
    add_row(tibble(a = 1), a = 2, .before = 1, .after = 1)

    add_column(tibble(a = 1), a = 1)
    add_column(tibble(a = 1, b = 2), a = 1, b = 2)
    add_column(tibble(!!!set_names(letters)), !!!set_names(letters))
    add_column(tibble(a = 2:3), b = 4:6)
    add_column(tibble(a = 1), b = 1, .before = 1, .after = 1)
  })
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/tests/testthat/test-as_tibble.R ---
# as_tibble -----------------------------------------------------------

test_that("columns are recycled to common length", {
  expect_identical(
    as_tibble(list(x = 1, y = 1:3)),
    tibble(x = rep(1, 3), y = 1:3)
  )
  expect_identical(
    as_tibble(list(x = 1:3, y = 1)),
    tibble(x = 1:3, y = rep(1, 3))
  )
  expect_identical(
    as_tibble(list(x = character(), y = 1)),
    tibble(x = character(), y = numeric())
  )
})

test_that("columns must be same length", {
  expect_tibble_abort(
    as_tibble(list(x = 1:2, y = 1:3)),
    abort_incompatible_size(NULL, c("x", "y"), 2:3, NA)
  )
  expect_tibble_abort(
    as_tibble(list(x = 1:2, y = 1:3, z = 1:4)),
    abort_incompatible_size(
      NULL,
      c("x", "y", "z"),
      2:4,
      NA
    )
  )
  expect_tibble_abort(
    as_tibble(list(x = 1:4, y = 1:2, z = 1:2)),
    abort_incompatible_size(
      NULL,
      c("x", "y", "z"),
      c(4, 2, 2),
      NA
    )
  )
  expect_tibble_abort(
    as_tibble(list(x = 1, y = 1:4, z = 1:2)),
    abort_incompatible_size(
      NULL,
      c("y", "z"),
      c(4, 2),
      NA
    )
  )
  expect_tibble_abort(
    as_tibble(list(x = 1:2, y = 1:4, z = 1)),
    abort_incompatible_size(
      NULL,
      c("x", "y"),
      c(2, 4),
      NA
    )
  )
})

test_that("empty list() makes 0 x 0 tbl_df", {
  zero <- as_tibble(list())
  expect_s3_class(zero, "tbl_df")
  expect_equal(dim(zero), c(0L, 0L))
})


test_that("NULL makes 0 x 0 tbl_df", {
  nnnull <- as_tibble(NULL)
  expect_s3_class(nnnull, "tbl_df")
  expect_equal(dim(nnnull), c(0L, 0L))
})


test_that("as_tibble() without arguments raises a lifecycle warning", {
  scoped_lifecycle_errors()

  expect_error(as_tibble())
})


test_that("as_tibble.tbl_df() leaves classes unchanged (#60)", {
  df <- tibble()
  expect_equal(
    class(df),
    c("tbl_df", "tbl", "data.frame")
  )
  expect_equal(
    class(structure(df, class = c("my_df", class(df)))),
    c("my_df", "tbl_df", "tbl", "data.frame")
  )
})


test_that("Can convert tables to data frame", {
  mtcars_table <- xtabs(mtcars, formula = ~ vs + am + cyl)

  mtcars2 <- as_tibble(mtcars_table)
  expect_equal(names(mtcars2), c(names(dimnames(mtcars_table)), "n"))

  expect_warning(
    mtcars2 <- as_tibble(mtcars_table, "Freq"),
    "named argument",
    fixed = TRUE
  )
  expect_equal(names(mtcars2), c(names(dimnames(mtcars_table)), "Freq"))

  mtcars2 <- as_tibble(mtcars_table, n = "Freq")
  expect_equal(names(mtcars2), c(names(dimnames(mtcars_table)), "Freq"))
})


test_that("Superseded: Can convert unnamed atomic vectors to tibble by default", {
  expect_equal(as_tibble(1:3), tibble(value = 1:3))
  expect_equal(
    as_tibble(c(TRUE, FALSE, NA)),
    tibble(value = c(TRUE, FALSE, NA))
  )
  expect_equal(as_tibble(1.5:3.5), tibble(value = 1.5:3.5))
  expect_equal(as_tibble(letters), tibble(value = letters))
})


test_that("as_tibble() checks for `unique` names by default (#278)", {
  l1 <- list(1:10)
  expect_tibble_abort(
    as_tibble(l1),
    abort_column_names_cannot_be_empty(1, repair_hint = TRUE)
  )

  l2 <- list(x = 1, 2)
  expect_tibble_abort(
    as_tibble(l2),
    abort_column_names_cannot_be_empty(2, repair_hint = TRUE)
  )

  l3 <- list(x = 1, ... = 2)
  expect_tibble_abort(
    as_tibble(l3),
    abort_column_names_cannot_be_dot_dot(2, repair_hint = TRUE)
  )

  l4 <- list(x = 1, ..1 = 2)
  expect_tibble_abort(
    as_tibble(l4),
    abort_column_names_cannot_be_dot_dot(2, repair_hint = TRUE)
  )

  df <- list(a = 1, b = 2)
  names(df) <- c("", NA)
  df <- new_tibble(df, nrow = 1)
  expect_tibble_abort(
    as_tibble(df),
    abort_column_names_cannot_be_empty(1:2, repair_hint = TRUE)
  )
})


test_that("as_tibble() makes names `minimal`, even if not fixing names", {
  invalid_df <- as_tibble(list(3, 4, 5), .name_repair = "minimal")
  expect_equal(length(invalid_df), 3)
  expect_equal(nrow(invalid_df), 1)
  expect_equal(names(invalid_df), rep("", 3))
})

test_that("as_tibble() implements unique names", {
  skip_if_not_installed("vctrs", "0.3.8.9001")

  expect_snapshot({
    invalid_df <- as_tibble(list(3, 4, 5), .name_repair = "unique")
  })
  expect_equal(length(invalid_df), 3)
  expect_equal(nrow(invalid_df), 1)
  expect_equal(
    names(invalid_df),
    vec_as_names(rep("", 3), repair = "unique", quiet = TRUE)
  )
})

test_that("as_tibble() implements universal names", {
  skip_if_not_installed("vctrs", "0.3.8.9001")

  expect_snapshot({
    invalid_df <- as_tibble(list(3, 4, 5), .name_repair = "universal")
  })
  expect_equal(length(invalid_df), 3)
  expect_equal(nrow(invalid_df), 1)
  expect_equal(
    names(invalid_df),
    vec_as_names(rep("", 3), repair = "universal", quiet = TRUE)
  )
})

test_that("as_tibble() implements unique_quiet", {
  skip_if_not_installed("vctrs", "0.5.0")

  expect_no_message({
    invalid_df <- as_tibble(list(3, 4, 5), .name_repair = "unique_quiet")
  })
  expect_equal(length(invalid_df), 3)
  expect_equal(nrow(invalid_df), 1)
  # it is "quiet" despite `quiet` being FALSE
  expect_equal(
    names(invalid_df),
    vec_as_names(rep("", 3), repair = "unique_quiet", quiet = FALSE)
  )
})

test_that("as_tibble() implements universal_quiet", {
  skip_if_not_installed("vctrs", "0.5.0")

  expect_no_message({
    invalid_df <- as_tibble(list(3, 4, 5), .name_repair = "universal_quiet")
  })
  expect_equal(length(invalid_df), 3)
  expect_equal(nrow(invalid_df), 1)
  # it is "quiet" despite `quiet` being FALSE
  expect_equal(
    names(invalid_df),
    vec_as_names(rep("", 3), repair = "universal_quiet", quiet = FALSE)
  )
})


test_that("as_tibble() implements custom name repair", {
  expect_silent(
    invalid_df <- as_tibble(
      list(3, 4, 5),
      .name_repair = function(x) make.names(x, unique = TRUE)
    )
  )
  expect_equal(length(invalid_df), 3)
  expect_equal(nrow(invalid_df), 1)
  expect_equal(names(invalid_df), make.names(rep("", 3), unique = TRUE))

  invalid_df_purrr <- as_tibble(
    list(3, 4, 5),
    .name_repair = ~ make.names(., unique = TRUE)
  )
  expect_identical(invalid_df_purrr, invalid_df)
})

test_that("as_tibble.matrix() supports validate (with warning) (#558)", {
  expect_warning(
    expect_identical(
      as_tibble(diag(3), validate = TRUE),
      tibble(
        V1 = c(1, 0, 0),
        V2 = c(0, 1, 0),
        V3 = c(0, 0, 1)
      )
    )
  )
})

test_that("as_tibble.matrix() supports .name_repair", {
  skip_if_not_installed("vctrs", "0.3.8.9001")

  scoped_lifecycle_warnings()

  x <- matrix(1:6, nrow = 3)

  expect_warning(as_tibble(x))

  minimal <- as_tibble(x, .name_repair = "minimal")
  expect_identical(names(minimal), rep("", 2))

  expect_snapshot(
    universal <- as_tibble(x, .name_repair = "universal")
  )
  expect_identical(names(universal), paste0("...", 1:2))

  x <- matrix(
    1:6,
    nrow = 3,
    dimnames = list(x = LETTERS[1:3], y = c("if", "when"))
  )

  expect_identical(
    names(as_tibble(x)),
    c("if", "when")
  )
  expect_identical(
    names(as_tibble(x, .name_repair = "minimal")),
    c("if", "when")
  )
  expect_snapshot(
    universal <- as_tibble(x, .name_repair = "universal")
  )
  expect_identical(names(universal), c(".if", "when"))
})

test_that("as_tibble.poly() supports .name_repair", {
  skip_if_not_installed("vctrs", "0.3.8.9001")

  x <- poly(1:6, 3)

  expect_identical(
    names(as_tibble(x)),
    as.character(1:3)
  )
  expect_identical(
    names(as_tibble(x, .name_repair = "minimal")),
    as.character(1:3)
  )
  expect_snapshot(
    universal <- as_tibble(x, .name_repair = "universal")
  )
  expect_identical(names(universal), paste0("...", 1:3))
})

test_that("as_tibble.table() supports .name_repair", {
  skip_if_not_installed("vctrs", "0.3.8.9001")

  expect_snapshot(error = TRUE, {
    as_tibble(table(a = c(1, 1, 1, 2, 2, 2), a = c(3, 4, 5, 3, 4, 5)))
    as_tibble(table(c(1, 1, 1, 2, 2, 2), c(3, 4, 5, 3, 4, 5)))
  })

  x <- table(a = c(1, 1, 1, 2, 2, 2), a = c(3, 4, 5, 3, 4, 5))
  expect_identical(
    names(as_tibble(x, .name_repair = "minimal")),
    c("a", "a", "n")
  )
  expect_snapshot(
    universal <- as_tibble(x, .name_repair = "universal")
  )
  expect_identical(names(universal), c("a...1", "a...2", "n"))

  x <- table("if" = c(1, 1, 1, 2, 2, 2), "when" = c(3, 4, 5, 3, 4, 5))

  expect_identical(
    names(as_tibble(x)),
    c("if", "when", "n")
  )
  expect_identical(
    names(as_tibble(x, .name_repair = "minimal")),
    c("if", "when", "n")
  )
  expect_snapshot(
    universal <- as_tibble(x, .name_repair = "universal")
  )
  expect_identical(names(universal), c(".if", "when", "n"))

  x <- table("m" = c(1, 1, 1, 2, 2, 2), "n" = c(3, 4, 5, 3, 4, 5))

  expect_identical(
    names(as_tibble(x, .name_repair = "minimal")),
    c("m", "n", "n")
  )
  expect_snapshot(
    universal <- as_tibble(x, .name_repair = "universal")
  )
  expect_identical(names(universal), c("m", "n...2", "n...3"))
})

test_that("as_tibble.ts() supports .name_repair, minimal by default (#537)", {
  skip_if_not_installed("vctrs", "0.3.8.9001")

  x <- ts(
    matrix(rnorm(6), nrow = 3),
    start = c(1961, 1),
    frequency = 12,
    names = NULL
  )

  expect_identical(
    names(as_tibble(x)),
    rep("", 2)
  )
  expect_identical(
    names(as_tibble(x, .name_repair = "minimal")),
    rep("", 2)
  )
  expect_snapshot(
    universal <- as_tibble(x, .name_repair = "universal")
  )
  expect_identical(names(universal), paste0("...", 1:2))

  x <- ts(
    matrix(rnorm(6), nrow = 3),
    start = c(1961, 1),
    frequency = 12,
    names = c("if", "when")
  )

  expect_identical(
    names(as_tibble(x)),
    c("if", "when")
  )
  expect_identical(
    names(as_tibble(x, .name_repair = "minimal")),
    c("if", "when")
  )
  expect_snapshot(
    universal <- as_tibble(x, .name_repair = "universal")
  )
  expect_identical(names(universal), c(".if", "when"))
})

test_that("as_tibble() can convert row names", {
  df <- data.frame(a = 1:3, b = 2:4, row.names = letters[5:7])

  expect_identical(
    as_tibble(df, rownames = NULL),
    tibble(a = 1:3, b = 2:4)
  )
  expect_identical(
    as_tibble(df, rownames = "id"),
    tibble(id = letters[5:7], a = 1:3, b = 2:4)
  )
  tbl_df <- as_tibble(df, rownames = NA)
  expect_identical(rownames(tbl_df), rownames(df))
  expect_identical(unclass(tbl_df), unclass(df))
})

test_that("as_tibble() can convert row names for zero-row tibbles", {
  df <- data.frame(a = 1:3, b = 2:4, row.names = letters[5:7])[0, ]

  expect_identical(
    as_tibble(df, rownames = NULL),
    tibble(a = integer(), b = integer())
  )
  expect_identical(
    as_tibble(df, rownames = "id"),
    tibble(id = character(), a = integer(), b = integer())
  )
  tbl_df <- as_tibble(df, rownames = NA)
  expect_identical(rownames(tbl_df), rownames(df))
  expect_identical(unclass(tbl_df), unclass(df))
})

test_that("as_tibble() converts implicit row names when `rownames =` is passed", {
  df <- data.frame(a = 1:3, b = 2:4)
  expect_equal(
    as_tibble(df, rownames = "id"),
    tibble(id = as.character(1:3), a = 1:3, b = 2:4)
  )
  expect_equal(
    as_tibble(df[0, ], rownames = "id"),
    tibble(id = character(0), a = integer(0), b = integer(0))
  )
})

test_that("as_data_frame() is an alias of as_tibble()", {
  scoped_lifecycle_silence()
  expect_identical(as_data_frame(NULL), as_tibble(NULL))
})

test_that("as.tibble() is an alias of as_tibble()", {
  scoped_lifecycle_silence()
  expect_identical(as.tibble(NULL), as_tibble(NULL))
})


# as_tibble_row -----------------------------------------------------------

test_that("as_tibble_row() can convert named bare vectors to data frame", {
  expect_identical(
    as_tibble_row(setNames(nm = 1:3)),
    tibble(`1` = 1L, `2` = 2L, `3` = 3L)
  )
  expect_identical(
    as_tibble_row(setNames(nm = c(TRUE, FALSE))),
    tibble(`TRUE` = TRUE, `FALSE` = FALSE)
  )
  expect_identical(
    as_tibble_row(setNames(nm = 1.5:3.5)),
    tibble(`1.5` = 1.5, `2.5` = 2.5, `3.5` = 3.5)
  )
  expect_identical(
    as_tibble_row(setNames(nm = letters)),
    tibble(!!!setNames(nm = letters))
  )
  expect_identical(
    as_tibble_row(list(a = 1, b = list(2:3))),
    tibble(a = 1, b = list(2:3))
  )

  expect_tibble_abort(
    as_tibble_row(list(a = 1, b = 2:3)),
    abort_as_tibble_row_size_one(2, "b", 2)
  )
  expect_tibble_abort(
    as_tibble_row(setNames(nm = c(TRUE, FALSE, NA))),
    abort_column_names_cannot_be_empty(3, repair_hint = TRUE)
  )
})

test_that("as_tibble_row() works with non-bare vectors (#797)", {
  expect_tibble_abort(
    as_tibble_row(new_environment()),
    abort_as_tibble_row_vector(new_environment())
  )

  time <- vec_slice(Sys.time(), 1)
  withr::local_options(list(rlib_name_repair_verbosity = "quiet"))
  expect_identical(
    as_tibble_row(time, .name_repair = "unique"),
    tibble(...1 = time)
  )
  expect_identical(
    as_tibble_row(trees[1:3, ], .name_repair = "unique"),
    tibble(
      ...1 = remove_rownames(trees[1, ]),
      ...2 = remove_rownames(trees[2, ]),
      ...3 = remove_rownames(trees[3, ])
    )
  )

  remove_first_dimname <- function(x) {
    dn <- dimnames(x)
    dn[1] <- list(NULL)
    dimnames(x) <- dn
    x
  }

  expect_identical(
    as_tibble_row(Titanic),
    tibble(
      "1st" = remove_first_dimname(Titanic[1, , , , drop = FALSE]),
      "2nd" = remove_first_dimname(Titanic[2, , , , drop = FALSE]),
      "3rd" = remove_first_dimname(Titanic[3, , , , drop = FALSE]),
      Crew = remove_first_dimname(Titanic[4, , , , drop = FALSE])
    )
  )
})


# as_tibble_col -----------------------------------------------------------

test_that("as_tibble_col() can convert atomic vectors to data frame", {
  expect_identical(as_tibble_col(1:3), tibble(value = 1:3))
  expect_identical(
    as_tibble_col(list(4, 5:6), column_name = "data"),
    tibble(data = list(4, 5:6))
  )

  expect_tibble_abort(
    as_tibble_col(lm(y ~ x, data.frame(x = 1:3, y = 2:4))),
    abort_column_scalar_type("value", 1, "a `lm` object")
  )
})

# Validation --------------------------------------------------------------

test_that("`validate` triggers deprecation message, but then works", {
  scoped_lifecycle_warnings()

  expect_error(
    as_tibble(list(a = 1, "hi"), validate = TRUE)
  )

  expect_error(
    as_tibble(list(a = 1, "hi", a = 2), validate = FALSE),
    "deprecated",
    fixed = TRUE
  )

  df <- data.frame(a = 1, "hi", a = 2)
  names(df) <- c("a", "", "a")
  expect_error(
    as_tibble(df, validate = FALSE)
  )

  df <- data.frame(a = 1, "hi")
  names(df) <- c("a", "")
  expect_error(
    as_tibble(df, validate = TRUE)
  )
})

test_that("`validate` always raises lifecycle warning.", {
  expect_error(
    as_tibble(list(a = 1, "hi"), validate = TRUE, .name_repair = "check_unique")
  )

  expect_error(
    as_tibble(
      list(a = 1, "hi", a = 2),
      validate = FALSE,
      .name_repair = "minimal"
    )
  )

  df <- data.frame(a = 1, "hi", a = 2)
  names(df) <- c("a", "", "a")
  expect_error(
    as_tibble(df, validate = FALSE, .name_repair = "minimal")
  )

  df <- data.frame(a = 1, "hi")
  names(df) <- c("a", "")
  expect_error(
    as_tibble(df, validate = TRUE, .name_repair = "check_unique")
  )
})

test_that("Inconsistent `validate` and `.name_repair` used together raise a warning.", {
  expect_error(
    as_tibble(
      list(a = 1, "hi"),
      validate = FALSE,
      .name_repair = "check_unique"
    )
  )

  expect_error(
    as_tibble(
      list(a = 1, "hi", a = 2),
      validate = TRUE,
      .name_repair = "minimal"
    )
  )

  df <- data.frame(a = 1, "hi", a = 2)
  names(df) <- c("a", "", "a")
  expect_error(
    as_tibble(df, validate = TRUE, .name_repair = "minimal")
  )

  df <- data.frame(a = 1, "hi")
  names(df) <- c("a", "")
  expect_error(
    as_tibble(df, validate = FALSE, .name_repair = "check_unique")
  )
})

test_that("correct rows and cols", {
  x <- matrix(1:6, nrow = 2)
  out <- as_tibble(x, .name_repair = "minimal")

  expect_equal(dim(out), c(2, 3))
})

test_that("correct rows and cols for 0 cols", {
  x <- matrix(integer(), nrow = 2)
  out <- as_tibble(x, .name_repair = "minimal")

  expect_equal(dim(out), c(2, 0))
})

test_that("correct rows and cols for 0 cols and legacy naming", {
  scoped_lifecycle_silence()

  x <- matrix(integer(), nrow = 2)
  out <- as_tibble(x)

  expect_equal(dim(out), c(2, 0))
})

test_that("correct rows and cols for 0 rows", {
  x <- matrix(integer(), ncol = 3)
  out <- as_tibble(x, .name_repair = "minimal")

  expect_equal(dim(out), c(0, 3))
})

test_that("preserves col names", {
  x <- matrix(1:4, nrow = 2)
  colnames(x) <- c("a", "b")

  out <- as_tibble(x)
  expect_equal(names(out), c("a", "b"))
})

test_that("supports compat col names", {
  scoped_lifecycle_warnings()

  x <- matrix(1:4, nrow = 2)

  expect_warning(out <- as_tibble(x))
  expect_equal(names(out), c("V1", "V2"))
})

test_that("creates col names with name repair", {
  skip_if_not_installed("vctrs", "0.3.8.9001")

  x <- matrix(1:4, nrow = 2)

  expect_snapshot(
    out <- as_tibble(x, .name_repair = "unique")
  )
  expect_equal(names(out), c("...1", "...2"))

  expect_snapshot(
    out <- as_tibble(x, .name_repair = "universal")
  )
  expect_equal(names(out), c("...1", "...2"))
})

test_that("preserves attributes except dim and names", {
  date <- Sys.Date() + 0:3
  dim(date) <- c(2, 2)
  colnames(date) <- c("a", "b")
  attr(date, "special") <- 42

  out <- as_tibble.matrix(date)
  expect_null(attributes(out[[1]])$names)
  expect_equal(attributes(out[[1]])$class, "Date")
  expect_equal(attributes(out[[2]])$special, 42)
})

test_that("properly handles poly class (#110)", {
  p <- poly(1:6, 3)
  p_df <- as_tibble(p)

  expect_equal(names(p_df), colnames(p))
  expect_equal(class(p_df[[1L]]), class(p[, 1]))
})

test_that("handles atomic vectors", {
  x <- matrix(TRUE, nrow = 2)
  out <- as_tibble(x, .name_repair = "minimal")
  expect_equal(out[[1]], c(TRUE, TRUE))

  x <- matrix(1L, nrow = 2)
  out <- as_tibble(x, .name_repair = "minimal")
  expect_equal(out[[1]], c(1L, 1L))

  x <- matrix(1.5, nrow = 2)
  out <- as_tibble(x, .name_repair = "minimal")
  expect_equal(out[[1]], c(1.5, 1.5))

  x <- matrix("a", nrow = 2)
  out <- as_tibble(x, .name_repair = "minimal")
  expect_equal(out[[1]], c("a", "a"))

  x <- matrix(complex(real = 1, imaginary = 2), nrow = 2)
  out <- as_tibble(x, .name_repair = "minimal")
  expect_equal(out[[1]], as.vector(x))
})

test_that("forwarding to as.data.frame() for ts objects (#184)", {
  mts <- cbind(
    A = ts(c(1, 1, 2, 2), start = 2016, frequency = 4),
    B = ts(c(11, 11, 12, 13), start = 2016, frequency = 4)
  )
  expect_identical(as_tibble(mts), as_tibble(as.data.frame(mts)))
})


test_that("converting from matrix removes row names by default", {
  x <- matrix(1:30, 6, 5, dimnames = list(letters[1:6], LETTERS[1:5]))
  df <- data.frame(A = 1:6, B = 7:12, C = 13:18, D = 19:24, E = 25:30)
  out <- as_tibble(x)
  expect_false(has_rownames(out))
  expect_identical(out, as_tibble(df))
})

test_that("converting from matrix keeps row names if argument has them, with rownames = NA", {
  x <- matrix(1:30, 6, 5, dimnames = list(letters[1:6], LETTERS[1:5]))
  df <- data.frame(
    A = 1:6,
    B = 7:12,
    C = 13:18,
    D = 19:24,
    E = 25:30,
    row.names = letters[1:6]
  )

  out <- as_tibble(x, rownames = NA)
  expect_identical(rownames(out), rownames(x))
  expect_identical(remove_rownames(out), as_tibble(df))
})

test_that("converting from matrix supports storing row names in a column", {
  x <- matrix(1:30, 6, 5, dimnames = list(letters[1:6], LETTERS[1:5]))
  df <- tibble(
    id = letters[1:6],
    A = 1:6,
    B = 7:12,
    C = 13:18,
    D = 19:24,
    E = 25:30
  )
  out <- as_tibble(x, rownames = "id")
  expect_identical(out, df)
})

test_that("converting from matrix uses implicit row names when `rownames =` is passed", {
  x <- matrix(1:30, 6, 5)
  y <- as_tibble(x, rownames = "id", .name_repair = "minimal")
  z <- new_tibble(
    list(
      id = c("1", "2", "3", "4", "5", "6"),
      c(1L, 2L, 3L, 4L, 5L, 6L),
      c(7L, 8L, 9L, 10L, 11L, 12L),
      c(13L, 14L, 15L, 16L, 17L, 18L),
      c(19L, 20L, 21L, 22L, 23L, 24L),
      c(25L, 26L, 27L, 28L, 29L, 30L)
    ),
    nrow = 6
  )
  expect_equal(y, z)
})

test_that("output test", {
  expect_snapshot(error = TRUE, {
    as_tibble(list(1))
    as_tibble(list(1, 2))
    as_tibble(list(a = 1, 2))
    as_tibble(as.list(1:26))
    as_tibble(set_names(list(1), "..1"))
    as_tibble(set_names(as.list(1:26), paste0("..", 1:26)))
    as_tibble(list(a = 1, a = 1))
    as_tibble(list(a = 1, a = 1, b = 1, b = 1))
    as_tibble(list(a = new_environment()))

    as_tibble_row(list(1))
    as_tibble_row(list(1, 2))
    as_tibble_row(list(a = 1, 2))
    as_tibble_row(as.list(1:26))
    as_tibble_row(set_names(list(1), "..1"))
    as_tibble_row(set_names(as.list(1:26), paste0("..", 1:26)))
    as_tibble_row(list(a = 1, a = 1))
    as_tibble_row(list(a = 1, a = 1, b = 1, b = 1))
    as_tibble_row(list(a = 1:3))
    as_tibble_row(list(a = 1:3, b = 1:3))
  })

  skip_if_not_installed("vctrs", "0.6.5.9000")
  expect_snapshot(error = TRUE, {
    as_tibble_row(list(a = new_environment()))
  })
})

# utilise as.data.frame for extended data.frames
test_that("as_tibble.data.frame coerces extended data.frames first", {
  x <- structure(mtcars, extra = "extra", class = c("ext_df_", "data.frame"))
  y <- as_tibble(head(mtcars))
  with_mocked_bindings(
    as.data.frame = function(x, row.names = NULL, optional = FALSE, ...) y,
    code = expect_identical(as_tibble(x), y),
    .package = "base"
  )
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/tests/testthat/test-enframe.R ---
# enframe -----------------------------------------------------------------

test_that("can convert unnamed vector", {
  expect_identical(
    enframe(3:1),
    tibble(name = 1:3, value = 3:1)
  )
})

test_that("can convert unnamed list", {
  expect_identical(
    enframe(as.list(3:1)),
    tibble(name = 1:3, value = as.list(3:1))
  )
})

test_that("can convert named vector", {
  expect_identical(
    enframe(c(a = 2, b = 1)),
    tibble(name = letters[1:2], value = as.numeric(2:1))
  )
})

test_that("can convert zero-length vector", {
  expect_identical(
    enframe(logical()),
    tibble(name = integer(), value = logical())
  )
})

test_that("can convert NULL (#352)", {
  expect_identical(
    enframe(NULL),
    tibble(name = integer(), value = logical())
  )
})

test_that("can use custom names", {
  expect_identical(
    enframe(letters, name = "index", value = "letter"),
    tibble(
      index = seq_along(letters),
      letter = letters
    )
  )
})

test_that("can enframe without names", {
  expect_identical(
    enframe(letters, name = NULL, value = "letter"),
    tibble(letter = letters)
  )
})

test_that("can't use value = NULL", {
  expect_tibble_abort(
    enframe(letters, value = NULL),
    abort_enframe_value_null()
  )
})

test_that("can't pass non-vector", {
  expect_tibble_abort(
    enframe(lm(speed ~ ., cars)),
    abort_enframe_must_be_vector(lm(speed ~ ., cars))
  )
})


# deframe -----------------------------------------------------------------

test_that("can deframe two-column data frame", {
  expect_identical(
    deframe(tibble(name = letters[1:3], value = 3:1)),
    c(a = 3L, b = 2L, c = 1L)
  )
})

test_that("can deframe one-column data frame", {
  expect_identical(
    deframe(tibble(value = 3:1)),
    3:1
  )
})

test_that("can deframe tibble with list column", {
  expect_identical(
    deframe(tibble(name = letters[1:3], value = as.list(3:1))),
    setNames(as.list(3:1), nm = letters[1:3])
  )
})

test_that("can deframe three-column data frame with warning", {
  expect_warning(
    expect_identical(
      deframe(tibble(name = letters[1:3], value = 3:1, oops = 1:3)),
      c(a = 3L, b = 2L, c = 1L)
    ),
    "one- or two-column",
    fixed = TRUE
  )
})


# roundtrip----------------------------------------------------------------

test_that("can roundtrip record", {
  rcrd <- new_rcrd(data.frame(a = 1:3))
  expect_identical(deframe(enframe(rcrd)), rcrd)
  expect_identical(deframe(enframe(rcrd, name = NULL)), rcrd)
  rcrd_named <- new_rcrd(data.frame(a = 1:3, row.names = letters[1:3]))
  expect_identical(deframe(enframe(rcrd_named)), rcrd_named)
})


# output ------------------------------------------------------------------

test_that("output test", {
  expect_snapshot(error = TRUE, {
    enframe(1:3, value = NULL)

    nrow(enframe(Titanic))
    vec_names(enframe(Titanic)$value)
    enframe(Titanic)$value
  })
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/tests/testthat/test-error.R ---
test_that("tibble_abort()", {
  # Must be called from a function whose name starts with `abort_`
  abort_foo <- function(call = caller_env()) {
    tibble_abort("message", call = call, foo = 42, bar = 7)
  }
  cnd <- tryCatch(abort_foo(), error = identity)

  expect_s3_class(
    cnd,
    c("tibble_error_foo", "tibble_error", "rlang_error", "error", "condition"),
    exact = TRUE
  )
  expect_equal(cnd$message, "message")
  expect_equal(cnd$foo, 42)
  expect_equal(cnd$bar, 7)
  expect_true(cnd$use_cli_format)
})

test_that("output test", {
  expect_snapshot({
    invalid_df("must be integer", "col", "Fix this.")
    invalid_df("must be numeric", c("col1", "col2"))

    use_repair(TRUE)
    use_repair(FALSE)
  })
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/tests/testthat/test-has-name.R ---
test_that("basic", {
  expect_true(has_name(trees, "Volume"))
  expect_false(has_name(mtcars, "gears"))
})

test_that("other types", {
  expect_true(has_name(list(a = 1), "a"))
  expect_true(has_name(c(a = 1), "a"))
})

test_that("vectorized", {
  expect_equal(has_name(list(a = 1), letters), c(TRUE, rep(FALSE, 25)))
})

test_that("NA", {
  expect_false(has_name(list(a = 1), NA))
})

test_that("unnamed", {
  expect_false(has_name(1, "a"))
  expect_true(has_name(1, ""))
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/tests/testthat/test-lst.R ---
test_that("lst handles named and unnamed NULL arguments", {
  expect_equal(lst(NULL), list("NULL" = NULL))
  expect_identical(lst(a = NULL), list(a = NULL))
  expect_identical(
    lst(NULL, b = NULL, 1:3),
    list("NULL" = NULL, b = NULL, "1:3" = 1:3)
  )
})

test_that("lst handles internal references", {
  expect_identical(lst(a = 1, b = a), list(a = 1, b = 1))
  expect_identical(lst(a = NULL, b = a), list(a = NULL, b = NULL))
})

test_that("lst supports duplicate names (#291)", {
  expect_identical(lst(a = 1, a = a + 1, b = a), list(a = 1, a = 2, b = 2))
  expect_identical(
    lst(b = 1, a = b, a = b + 1, b = a),
    list(b = 1, a = 1, a = 2, b = 2)
  )
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/tests/testthat/test-msg.R ---
test_that("error class", {
  expect_equal(tibble_error_class("boo"), c("tibble_error_boo", "tibble_error"))
})

test_that("aborting with class", {
  expect_error(
    abort_enframe_value_null(),
    class = tibble_error_class("enframe_value_null")[[1]]
  )
})

test_that("output test", {
  skip_if_not_installed("testthat", "3.1.1")

  expect_snapshot(variant = rlang_variant(), {
    "# add"
    print_error(abort_add_rows_to_grouped_df())

    print_error(abort_incompatible_new_rows("a"))
    print_error(abort_incompatible_new_rows(letters[2:3]))
    print_error(abort_incompatible_new_rows(LETTERS))

    print_error(abort_both_before_after())

    print_error(abort_unknown_column_names("a"))
    print_error(abort_unknown_column_names(c("b", "c")))
    print_error(abort_unknown_column_names(LETTERS))

    print_error(abort_incompatible_new_cols(10, data.frame(a = 1:2)))
    print_error(abort_incompatible_new_cols(1, data.frame(a = 1:3, b = 2:4)))

    "# as_tibble"
    print_error(abort_column_scalar_type("a", 3, "environment"))
    print_error(abort_column_scalar_type("", 3, "environment"))
    print_error(abort_column_scalar_type(letters[2:3], 3:4, c("name", "NULL")))
    print_error(abort_column_scalar_type(
      c("", "", LETTERS),
      1:28,
      c("QQ", "VV", letters)
    ))

    print_error(abort_as_tibble_row_vector(new_environment()))
    print_error(abort_as_tibble_row_size_one(3, "foo", 7))

    "# class-tbl_df"
    print_error(abort_names_must_be_non_null())

    print_error(abort_names_must_have_length(length = 5, n = 3))

    "#enframe"
    print_error(abort_enframe_value_null())
    print_error(abort_enframe_must_be_vector(lm(speed ~ ., cars)))

    "# names"
    print_error(abort_column_names_cannot_be_empty(1, repair_hint = TRUE))
    print_error(abort_column_names_cannot_be_empty(2:3, repair_hint = TRUE))
    print_error(abort_column_names_cannot_be_empty(
      seq_along(letters),
      repair_hint = TRUE
    ))
    print_error(abort_column_names_cannot_be_empty(4:6, repair_hint = FALSE))

    print_error(abort_column_names_cannot_be_dot_dot(1, repair_hint = FALSE))
    print_error(abort_column_names_cannot_be_dot_dot(2:3, repair_hint = TRUE))
    print_error(abort_column_names_cannot_be_dot_dot(1:26, repair_hint = TRUE))

    print_error(abort_column_names_must_be_unique("a", repair_hint = FALSE))
    print_error(abort_column_names_must_be_unique(
      letters[2:3],
      repair_hint = TRUE
    ))
    print_error(abort_column_names_must_be_unique(LETTERS, repair_hint = TRUE))

    "# new"
    print_error(abort_new_tibble_must_be_list())
    print_error(abort_new_tibble_nrow_must_be_nonnegative())

    "# rownames"
    print_error(abort_already_has_rownames())

    "# subsetting"
    print_error(abort_need_rhs_vector(quote(RHS)))
    print_error(abort_need_rhs_vector_or_null(quote(RHS)))

    print_error(abort_dim_column_index(as.matrix("x")))

    print_error(abort_assign_columns_non_na_only())
    print_error(abort_subset_columns_non_missing_only())
    print_error(abort_assign_columns_non_missing_only())

    print_error(abort_assign_duplicate_column_subscript(c(1, 1)))
    print_error(abort_assign_duplicate_column_subscript(c(1, 1, 2, 2)))

    print_error(abort_assign_rows_non_na_only())

    print_error(abort_assign_duplicate_row_subscript(c(1, 1)))
    print_error(abort_assign_duplicate_row_subscript(c(1, 1, 2, 2)))

    print_error(abort_assign_incompatible_size(
      3,
      list(1:2),
      1,
      NULL,
      quote(rhs)
    ))
    print_error(abort_assign_incompatible_size(
      4,
      list(1:4, 3:4),
      2,
      quote(4:1),
      quote(rhs)
    ))

    print_error(abort_assign_incompatible_type(
      tibble(a = 1),
      list("c"),
      1,
      quote(rhs)
    ))
    print_error(abort_assign_vector(list("c"), 1, quote(rhs)))

    "# subsetting-matrix"
    print_error(abort_subset_matrix_must_be_logical(quote(is.na(x) + 1)))
    print_error(abort_subset_matrix_must_have_same_dimensions(quote(t(is.na(
      x
    )))))
    print_error(abort_subset_matrix_scalar_type(
      quote(is.na(x)),
      quote(new_environment())
    ))
    print_error(abort_subset_matrix_must_be_scalar(quote(is.na(x)), quote(1:3)))

    "# tibble"
    print_error(abort_tibble_row_size_one(3, "foo", 7))

    print_error(abort_incompatible_size(
      10,
      letters[1:3],
      c(4, 4, 3),
      "Requested with `uvw` argument"
    ))
    print_error(abort_incompatible_size(
      10,
      letters[1:3],
      c(2, 2, 3),
      "Requested with `xyz` argument"
    ))
    print_error(abort_incompatible_size(
      NULL,
      letters[1:3],
      c(2, 2, 3),
      "Requested with `xyz` argument"
    ))
    print_error(abort_incompatible_size(
      10,
      1:3,
      c(4, 4, 3),
      "Requested with `uvw` argument"
    ))
    print_error(abort_incompatible_size(
      10,
      1:3,
      c(2, 2, 3),
      "Requested with `xyz` argument"
    ))
    print_error(abort_incompatible_size(
      NULL,
      1:3,
      c(2, 2, 3),
      "Requested with `xyz` argument"
    ))

    "# tribble"
    print_error(abort_tribble_needs_columns())

    print_error(abort_tribble_lhs_column_syntax(quote(lhs)))

    print_error(abort_tribble_rhs_column_syntax(quote(a + b)))

    print_error(abort_tribble_non_rectangular(5, 17))

    print_error(abort_frame_matrix_list(2:4))
  })
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/tests/testthat/test-names.R ---
test_that("set_repaired_names()", {
  x <- set_names(1:3, letters[1:3])
  expect_equal(set_repaired_names(x), x)
  expect_tibble_abort(
    set_repaired_names(1, repair_hint = FALSE),
    abort_column_names_cannot_be_empty(1, repair_hint = FALSE)
  )
})

test_that("repaired_names()", {
  expect_equal(repaired_names(letters[1:3], repair_hint = FALSE), letters[1:3])
  expect_tibble_abort(
    repaired_names(c(""), repair_hint = FALSE),
    abort_column_names_cannot_be_empty(1, repair_hint = FALSE)
  )
  expect_tibble_abort(
    repaired_names(c("..1"), repair_hint = FALSE),
    abort_column_names_cannot_be_dot_dot(1, repair_hint = FALSE)
  )
  expect_tibble_abort(
    repaired_names(c("a", "a"), repair_hint = FALSE),
    abort_column_names_must_be_unique("a", repair_hint = FALSE)
  )
  expect_equal(
    repaired_names(c("a", "a"), .name_repair = "minimal"),
    c("a", "a")
  )
})

test_that("output test", {
  skip_if_not_installed("vctrs", "0.3.8.9001")

  expect_snapshot(error = TRUE, {
    repaired_names(letters[1:3], repair_hint = FALSE)
    repaired_names("", repair_hint = FALSE)
    repaired_names("", repair_hint = TRUE)
    repaired_names(c("a", "a"), repair_hint = FALSE)
    repaired_names("..1", repair_hint = FALSE)
    repaired_names(c("a", "a"), repair_hint = FALSE, .name_repair = "universal")
    repaired_names(
      c("a", "a"),
      repair_hint = FALSE,
      .name_repair = "universal",
      quiet = TRUE
    )
    repaired_names(c("if"), repair_hint = FALSE, .name_repair = "universal")
  })
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/tests/testthat/test-new.R ---
test_that("new_tibble() with new class argument", {
  tbl <- new_tibble(
    data.frame(a = 1:3),
    names = "b",
    attr1 = "value1",
    attr2 = 2,
    nrow = 3,
    class = c("nt", "data.frame")
  )

  # Can't compare directly due to dplyr:::all.equal.tbl_df()
  expect_identical(class(tbl), c("nt", "tbl_df", "tbl", "data.frame"))
  expect_equal(
    unclass(tbl),
    structure(
      list(b = 1:3),
      attr1 = "value1",
      attr2 = 2,
      .Names = "b",
      row.names = .set_row_names(3L)
    )
  )
})

test_that("new_tibble() with additional attributes", {
  df <- data.frame(a = 1:3)
  foo <- df
  attr(foo, "foo") <- "bar"

  tbl_df <- new_tibble(df, nrow = 3, foo = "baz")
  tbl_foo <- new_tibble(foo, nrow = 3, foo = "baz")
  expect_identical(tbl_df, tbl_foo)
})

test_that("new_tibble() can add attributes on zero column tibbles with no attributes", {
  expect_identical(
    attr(new_tibble(list(), nrow = 0L, foo = 10), "foo"),
    10
  )
})

test_that("new_tibble() ignores unnamed additional attributes", {
  expect_identical(
    new_tibble(list(x = 1), "foo", nrow = 1),
    new_tibble(list(x = 1), nrow = 1)
  )

  expect_identical(
    new_tibble(list(x = 1), "foo", bar = "bar", nrow = 1),
    new_tibble(list(x = 1), bar = "bar", nrow = 1)
  )
})

test_that("new_tibble() allows setting names through `...`", {
  expect_identical(
    new_tibble(list(1), names = "x", nrow = 1),
    new_tibble(list(x = 1), nrow = 1)
  )
})

test_that("new_tibble() supports missing `nrow` (#781)", {
  expect_identical(new_tibble(list()), tibble())
  expect_identical(new_tibble(list(a = 1:3)), tibble(a = 1:3))
})

test_that("new_tibble() keeps x and n attributes", {
  expect_identical(
    attr(new_tibble(list(x = 1), n = 2, nrow = 1), "n"),
    2
  )

  expect_identical(
    attr(new_tibble(structure(list(x = 1), n = 2), nrow = 1), "n"),
    2
  )

  expect_identical(
    attr(new_tibble(structure(list(x = 1), x = "value"), nrow = 1), "x"),
    "value"
  )
})

test_that("new_tibble() supports language objects", {
  expect_identical(
    new_tibble(list(), foo = quote(bar())),
    structure(new_tibble(list()), foo = quote(bar()))
  )
})

test_that("new_tibble checks", {
  scoped_lifecycle_errors()

  expect_identical(new_tibble(list(), nrow = 0), tibble())
  expect_identical(new_tibble(list(), nrow = 5), tibble(.rows = 5))
  expect_identical(
    new_tibble(list(a = 1:3, b = 4:6), nrow = 3),
    tibble(a = 1:3, b = 4:6)
  )
  expect_tibble_abort(
    new_tibble(1:3, nrow = 1),
    abort_new_tibble_must_be_list()
  )
  expect_tibble_abort(
    new_tibble(list(a = 1), nrow = -1),
    abort_new_tibble_nrow_must_be_nonnegative()
  )
  expect_tibble_abort(
    new_tibble(list(a = 1), nrow = "a"),
    abort_new_tibble_nrow_must_be_nonnegative()
  )
  expect_tibble_abort(
    new_tibble(list(a = 1), nrow = 1:2),
    abort_new_tibble_nrow_must_be_nonnegative()
  )
  expect_tibble_abort(
    new_tibble(list(a = 1), nrow = 2147483648),
    abort_new_tibble_nrow_must_be_nonnegative()
  )
  expect_tibble_abort(
    new_tibble(list(1), nrow = 1),
    abort_names_must_be_non_null()
  )
  expect_error(
    new_tibble(set_names(list(1), NA_character_), nrow = 1),
    NA
  )
  expect_error(
    new_tibble(set_names(list(1), ""), nrow = 1),
    NA
  )
  expect_error(
    new_tibble(list(a = 1, b = 2:3), nrow = 1),
    NA
  )
  expect_error(
    new_tibble(
      structure(list(a = 1, b = 2), row.names = .set_row_names(2)),
      nrow = 1
    ),
    NA
  )
})

test_that("validate_tibble() checks", {
  expect_tibble_abort(
    validate_tibble(new_tibble(list(a = 1, b = 2:3), nrow = 1)),
    abort_incompatible_size(
      1,
      c("a", "b"),
      1:2,
      "Requested with `nrow` argument"
    )
  )
})

test_that("output test", {
  expect_snapshot(error = TRUE, {
    new_tibble(1:3, nrow = 1)
    new_tibble(as.list(1:3))
  })
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/tests/testthat/test-options.R ---
test_that("tibble option takes preference", {
  withr::with_options(
    list(
      tibble.width = 10,
      dplyr.width = 20
    ),
    expect_equal(tibble_opt("width"), 10)
  )
})

test_that("dplyr option is used for compatibility", {
  withr::with_options(
    list(
      tibble.width = NULL,
      dplyr.width = 20
    ),
    expect_equal(tibble_opt("width"), 20)
  )
})

test_that("fallback to default option", {
  withr::with_options(
    list(
      tibble.width = NULL,
      dplyr.width = NULL
    ),
    expect_equal(tibble_opt("width"), op.tibble[["tibble.width"]])
  )
})

test_that("tibble_width returns user-input width,
          then tibble.width option, then width option", {
  test_width <- 42

  expect_equal(tibble_width(test_width), test_width)
  withr::with_options(
    list(tibble.width = test_width),
    expect_equal(tibble_width(NULL), test_width)
  )
  withr::with_options(
    list(width = test_width),
    expect_equal(tibble_width(NULL), test_width)
  )
})

test_that("tibble_width prefers tibble.width / dplyr.width over width", {
  withr::with_options(
    list(tibble.width = 10, width = 20),
    expect_equal(tibble_width(NULL), 10)
  )

  withr::with_options(
    list(dplyr.width = 10, width = 20),
    expect_equal(tibble_width(NULL), 10)
  )
})

test_that("tibble_glimpse_width returns user-input width,
          then tibble.width option, then width option", {
  test_width <- 42

  expect_equal(tibble_glimpse_width(test_width), test_width)
  withr::with_options(
    list(tibble.width = test_width),
    expect_equal(tibble_glimpse_width(NULL), test_width)
  )
  withr::with_options(
    list(width = test_width),
    expect_equal(tibble_glimpse_width(NULL), test_width)
  )
})

test_that("tibble_glimpse_width prefers tibble.width / dplyr.width over width", {
  withr::with_options(
    list(tibble.width = 10, width = 20),
    expect_equal(tibble_glimpse_width(NULL), 10)
  )

  withr::with_options(
    list(dplyr.width = 10, width = 20),
    expect_equal(tibble_glimpse_width(NULL), 10)
  )
})

test_that("tibble_glimpse_width ignores Inf tibble.width", {
  withr::with_options(
    list(tibble.width = Inf, width = 20),
    expect_equal(tibble_glimpse_width(NULL), 20)
  )
})

test_that("print.tbl ignores max.print option", {
  trees2 <- as_tibble(trees)
  expect_output(
    withr::with_options(list(max.print = 3), print(trees2)),
    capture_output(print(trees2)),
    fixed = TRUE
  )
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/tests/testthat/test-print.R ---
test_that("print() returns output invisibly", {
  expect_output(ret <- withVisible(print(as_tibble(trees))))
  expect_false(ret$visible)
  expect_identical(ret$value, as_tibble(trees))
})

test_that("output test", {
  skip_if(getRversion() < "3.2")

  expect_snapshot({
    mtcars2 <- as_tibble(mtcars, rownames = NA)

    print_without_body(mtcars2, n = 8L, width = 30L)

    print_without_body(as_tibble(trees), n = 5L, width = 30L)

    print_without_body(as_tibble(trees), n = -1L, width = 30L)

    print_without_body(as_tibble(trees), n = Inf, width = 15L)

    print_without_body(as_tibble(trees), n = NULL, width = 70L)

    print_without_body(as_unknown_rows(trees), n = 10, width = 70L)

    print_without_body(as_unknown_rows(trees[1:9, ]), n = 10, width = 70L)

    print_without_body(as_unknown_rows(trees[1:10, ]), n = 10, width = 70L)

    print_without_body(as_unknown_rows(trees[1:11, ]), n = 10, width = 70L)

    print_without_body(df_all, n = NULL, width = 30L)

    print_without_body(df_all, n = NULL, width = 300L)

    print_without_body(tibble(a = seq.int(10000)), n = 5L, width = 30L)

    print_without_body(tibble(a = character(), b = logical()), width = 30L)

    print_without_body(as_tibble(trees)[character()], n = 5L, width = 30L)

    print_without_body(as_unknown_rows(trees[integer(), ]), n = 5L, width = 30L)

    print_without_body(
      as_unknown_rows(trees[, character()]),
      n = 5L,
      width = 30L
    )

    print_without_body(
      as_unknown_rows(tibble(a = seq.int(10000))),
      n = 5L,
      width = 30L
    )
  })
})

test_that("full output test", {
  skip_if(getRversion() < "3.2")

  expect_snapshot({
    df <- tibble(x = as.POSIXct("2016-01-01 12:34:56 GMT") + 1:12)
    df$y <- as.POSIXlt(df$x)

    print(df, n = 8L, width = 60L)

    x <- c("\u6210\u4ea4\u65e5\u671f", "\u5408\u540c\u5f55\u5165\u65e5\u671f")
    df <- setNames(tibble(1:3, 4:6), x)
    print(df, n = 8L, width = 60L)
  })
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/tests/testthat/test-rownames.R ---
test_that("has_rownames and remove_rownames", {
  expect_false(has_rownames(trees))
  expect_true(has_rownames(mtcars))
  expect_false(has_rownames(remove_rownames(mtcars)))
  expect_false(has_rownames(remove_rownames(trees)))
  expect_false(has_rownames(1:10))
})

test_that("setting row names on a tibble raises a warning", {
  mtcars2 <- as_tibble(mtcars)
  expect_false(has_rownames(mtcars2))

  expect_warning(
    rownames(mtcars2) <- rownames(mtcars),
    "deprecated",
    fixed = TRUE
  )
})

test_that("rownames_to_column keeps the tbl classes (#882)", {
  res <- rownames_to_column(mtcars)
  expect_false(has_rownames(res))
  expect_equal(class(res), class(mtcars))
  expect_equal(res$rowname, rownames(mtcars))
  expect_tibble_abort(
    rownames_to_column(mtcars, "wt"),
    abort_column_names_must_be_unique("wt", repair_hint = FALSE)
  )

  mtcars2 <- as_tibble(mtcars, rownames = NA)

  res1 <- rownames_to_column(mtcars2, "Make&Model")
  expect_false(has_rownames(res1))
  expect_equal(class(res1), class(mtcars2))
  expect_equal(res1$`Make&Model`, rownames(mtcars))
  expect_tibble_abort(
    rownames_to_column(mtcars2, "wt"),
    abort_column_names_must_be_unique("wt", repair_hint = FALSE)
  )
})

test_that("rowid_to_column keeps the tbl classes", {
  res <- rowid_to_column(mtcars)
  expect_false(has_rownames(res))
  expect_equal(class(res), class(mtcars))
  expect_equal(res$rowid, seq_len(nrow(mtcars)))
  expect_tibble_abort(
    rowid_to_column(mtcars, "wt"),
    abort_column_names_must_be_unique("wt", repair_hint = FALSE)
  )

  mtcars2 <- as_tibble(mtcars, rownames = NA)

  res1 <- rowid_to_column(mtcars2, "row_id")
  expect_false(has_rownames(res1))
  expect_equal(class(res1), class(mtcars2))
  expect_equal(res1$row_id, seq_len(nrow(mtcars)))
  expect_tibble_abort(
    rowid_to_column(mtcars2, "wt"),
    abort_column_names_must_be_unique("wt", repair_hint = FALSE)
  )
})

test_that("column_to_rownames returns tbl", {
  var <- "car"
  mtcars1 <- as_tibble(mtcars, rownames = NA)

  expect_true(has_rownames(mtcars1))
  res0 <- rownames_to_column(mtcars1, var)
  expect_warning(res <- column_to_rownames(res0, var), NA)
  expect_true(has_rownames(res))
  expect_equal(class(res), class(mtcars))
  expect_equal(rownames(res), rownames(mtcars1))
  expect_equal(res, mtcars)
  expect_false(has_name(res, var))

  mtcars1$num <- rev(seq_len(nrow(mtcars)))
  res0 <- rownames_to_column(mtcars1)
  expect_warning(res <- column_to_rownames(res0, var = "num"), NA)
  expect_true(has_rownames(res))
  expect_equal(rownames(res), as.character(mtcars1$num))
  expect_tibble_abort(
    column_to_rownames(res),
    abort_already_has_rownames()
  )
  expect_tibble_abort(
    column_to_rownames(rownames_to_column(mtcars1, var), "num2"),
    abort_unknown_column_names("num2")
  )
})

test_that("converting to data frame does not add row names", {
  expect_false(has_rownames(as.data.frame(as_tibble(trees))))
})

test_that("work around structure() bug (#852)", {
  expect_false(has_rownames(structure(trees, .drop = FALSE)))
})

test_that("output test", {
  expect_snapshot(error = TRUE, {
    rownames_to_column(mtcars, "cyl")
    rowid_to_column(trees, "Volume")

    column_to_rownames(mtcars, "cyl")
    column_to_rownames(trees, "foo")
  })
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/tests/testthat/test-str.R ---
test_that("output test", {
  expect_snapshot({
    str(as_tibble(mtcars), width = 70L)

    str(as_tibble(trees), width = 70L)

    "No columns"
    str(as_tibble(trees[integer()]), width = 70L)

    "Non-syntactic names"
    df <- tibble(!!!set_names(c(5, 3), c("mean(x)", "var(x)")))
    str(df, width = 28)

    str(as_tibble(df_all), width = 70L)

    "options(tibble.width = 50)"
    withr::with_options(
      list(tibble.width = 50),
      str(as_tibble(df_all))
    )

    "options(tibble.width = 35)"
    withr::with_options(
      list(tibble.width = 35),
      str(as_tibble(df_all))
    )

    "non-tibble"
    str(5)

    Volume <- unique(trees$Volume)
    data <- unname(split(trees, trees$Volume))
    nested_trees_df <- tibble(Volume, data)
    str(nested_trees_df, width = 70L)

    data <- map(data, as_tibble)
    nested_trees_tbl <- tibble(Volume, data)
    str(nested_trees_tbl, width = 70L)
  })

  skip_if_not_installed("vctrs", "0.4.1.9000")

  expect_snapshot({
    trees2 <- as_unknown_rows(trees)
    str(trees2, width = 70L)
  })
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/tests/testthat/test-string-to-indices.R ---
test_that("works as expected", {
  expect_identical(string_to_indices(as.character(1:3)), 1:3)
  expect_identical(string_to_indices(as.character(0:2)), c(NA_integer_, 1L, 2L))
  expect_identical(string_to_indices(letters[1:3]), rep(NA_integer_, 3))
  expect_identical(string_to_indices(as.character(1:3 + 1e10)), 1:3 + 1e10)
  expect_identical(
    string_to_indices(c(as.character(1:3 + 1e10), "x")),
    c(1:3 + 1e10, NA)
  )
  expect_identical(
    string_to_indices(as.character(c(1:3, 1:3 + 1e10))),
    c(1:3, 1:3 + 1e10)
  )
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/tests/testthat/test-subsetting-matrix.R ---
test_that("[.tbl_df supports subsetting with a logical matrix (#649)", {
  foo <- tibble(x = 1:10, y = 1:10, z = letters[1:10])

  m <- matrix(c(rep(FALSE, 8), rep(TRUE, 3), rep(FALSE, 19)), ncol = 3)
  expect_equal(foo[m], c(9, 10, 1))

  m <- matrix(c(rep(FALSE, 18), rep(TRUE, 3), rep(FALSE, 9)), ncol = 3)
  expect_error(foo[m], class = "vctrs_error_incompatible_type")
})

test_that("[<-.tbl_df supports subsetting with a logical matrix (#649)", {
  foo <- tibble(x = 1:10, y = 1:10, z = letters[1:10])

  m <- matrix(c(rep(FALSE, 8), rep(TRUE, 3), rep(FALSE, 19)), ncol = 3)
  foo[m] <- 1
  expect_equal(foo[m], c(1, 1, 1))

  expect_error(foo[m] <- 1:3)

  m <- matrix(c(rep(FALSE, 18), rep(TRUE, 3), rep(FALSE, 9)), ncol = 3)
  expect_snapshot(error = TRUE, {
    foo[m] <- 1
  })
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/tests/testthat/test-subsetting.R ---
# [ -----------------------------------------------------------------------

test_that("[ never drops", {
  mtcars2 <- as_tibble(mtcars)

  expect_s3_class(mtcars2[, 1], "data.frame")
  expect_s3_class(mtcars2[, 1], "tbl_df")
  expect_equal(mtcars2[, 1], mtcars2[1])
})

test_that("[ retains class", {
  mtcars2 <- as_tibble(mtcars)

  expect_identical(class(mtcars2), class(mtcars2[1:5, ]))
  expect_identical(class(mtcars2), class(mtcars2[, 1:5]))
  expect_identical(class(mtcars2), class(mtcars2[1:5, 1:5]))
})

test_that("[ and as_tibble commute", {
  mtcars2 <- as_tibble(mtcars)
  expect_identical(mtcars2, as_tibble(mtcars))
  expect_identical(mtcars2[], remove_rownames(as_tibble(mtcars[])))
  expect_identical(mtcars2[1:5, ], remove_rownames(as_tibble(mtcars[1:5, ])))
  expect_identical(mtcars2[, 1:5], remove_rownames(as_tibble(mtcars[, 1:5])))
  expect_equal(mtcars2[1:5, 1:5], remove_rownames(as_tibble(mtcars[1:5, 1:5])))
  expect_identical(mtcars2[1:5], remove_rownames(as_tibble(mtcars[1:5])))
})

test_that("[ with 0 cols creates correct row names (#656)", {
  zero_row <- as_tibble(trees)[, 0]
  expect_s3_class(zero_row, "tbl_df")
  expect_equal(nrow(zero_row), 31)
  expect_equal(ncol(zero_row), 0)

  expect_identical(zero_row, as_tibble(trees)[0])
})

test_that("[ with 0 cols returns correct number of rows", {
  trees_tbl <- as_tibble(trees)
  nrow_trees <- nrow(trees_tbl)

  expect_equal(nrow(trees_tbl[0]), nrow_trees)
  expect_equal(nrow(trees_tbl[, 0]), nrow_trees)

  expect_equal(nrow(trees_tbl[, 0][1:10, ]), 10)
  expect_equal(nrow(trees_tbl[0][1:10, ]), 10)
  expect_equal(nrow(trees_tbl[1:10, ][, 0]), 10)
  expect_equal(nrow(trees_tbl[1:10, ][0]), 10)
  expect_equal(nrow(trees_tbl[1:10, 0]), 10)

  expect_equal(nrow(trees_tbl[, 0][-(1:10), ]), nrow_trees - 10)
  expect_equal(nrow(trees_tbl[0][-(1:10), ]), nrow_trees - 10)
  expect_equal(nrow(trees_tbl[-(1:10), ][, 0]), nrow_trees - 10)
  expect_equal(nrow(trees_tbl[-(1:10), ][0]), nrow_trees - 10)
  expect_equal(nrow(trees_tbl[-(1:10), 0]), nrow_trees - 10)
})

test_that("[ with '0' for rows works correctly (#1636)", {
  simple_tbl <- tibble(a = 1:3)
  simple_df <- data.frame(a = 1:3)

  expect_identical(
    suppressWarnings(simple_tbl["0", ]),
    as_tibble(simple_df["0", , drop = FALSE])
  )
  expect_identical(
    suppressWarnings(simple_tbl[as.character(0:1), ]),
    as_tibble(simple_df[as.character(0:1), , drop = FALSE])
  )
  expect_identical(
    suppressWarnings(simple_tbl[as.character(-1:0), ]),
    as_tibble(simple_df[as.character(-1:0), , drop = FALSE])
  )
})

test_that("[ with explicit NULL works as expected (#696)", {
  trees_tbl <- as_tibble(trees)

  expect_identical(trees_tbl[NULL], trees_tbl[0])
  expect_identical(trees_tbl[, NULL], trees_tbl[, 0])
  expect_identical(trees_tbl[NULL, ], trees_tbl[0, ])
  expect_identical(trees_tbl[NULL, NULL], tibble())
})

test_that("[.tbl_df is careful about names (#1245)", {
  foo <- tibble(x = 1:10, y = 1:10)

  expect_error(
    foo["z"],
    class = "vctrs_error_subscript_oob"
  )
  expect_error(
    foo[c("x", "y", "z")],
    class = "vctrs_error_subscript_oob"
  )

  expect_error(
    foo[, "z"],
    class = "vctrs_error_subscript_oob"
  )
  expect_error(
    foo[, c("x", "y", "z")],
    class = "vctrs_error_subscript_oob"
  )

  verify_errors({
    foo <- tibble(x = 1:10, y = 1:10)
    expect_error(
      foo[c("x", "y", "z")],
      class = "vctrs_error_subscript_oob"
    )
    expect_error(
      foo[c("w", "x", "y", "z")],
      class = "vctrs_error_subscript_oob"
    )
  })
})

test_that("[.tbl_df is careful about column indexes (#83)", {
  verify_errors({
    foo <- tibble(x = 1:10, y = 1:10, z = 1:10)
    expect_identical(foo[1:3], foo)

    expect_error(
      foo[0.5],
      class = "vctrs_error_subscript_type"
    )
    expect_error(
      foo[1:5],
      class = "vctrs_error_subscript_oob"
    )

    expect_error(
      foo[-1:1],
      class = "vctrs_error_subscript_type"
    )
    expect_error(
      foo[c(-1, 1)],
      class = "vctrs_error_subscript_type"
    )
    expect_error(
      foo[c(-1, NA)],
      class = "vctrs_error_subscript_type"
    )

    expect_error(
      foo[-4],
      class = "vctrs_error_subscript_oob"
    )
    expect_error(
      foo[c(1:3, NA)],
      class = "vctrs_error_subscript_type"
    )

    expect_error(foo[as.matrix(1)])

    expect_error(foo[array(1, dim = c(1, 1, 1))])
  })
})

test_that("[.tbl_df is careful about column flags (#83)", {
  verify_errors({
    foo <- tibble(x = 1:10, y = 1:10, z = 1:10)
    expect_identical(foo[TRUE], foo)
    expect_identical(foo[c(TRUE, TRUE, TRUE)], foo)
    expect_identical(foo[FALSE], foo[integer()])
    expect_identical(foo[c(FALSE, TRUE, FALSE)], foo[2])

    expect_error(
      foo[c(TRUE, TRUE)],
      class = "vctrs_error_subscript_size"
    )
    expect_error(
      foo[c(TRUE, TRUE, FALSE, FALSE)],
      class = "vctrs_error_subscript_size"
    )
    expect_error(
      foo[c(TRUE, TRUE, NA)],
      class = "vctrs_error_subscript_type"
    )

    expect_tibble_abort(
      foo[as.matrix(TRUE)],
      abort_subset_matrix_must_have_same_dimensions(quote(as.matrix(TRUE)))
    )
    expect_error(
      foo[array(TRUE, dim = c(1, 1, 1))],
      class = "vctrs_error_subscript_type"
    )
  })
})

test_that("[.tbl_df rejects unknown column indexes (#83)", {
  verify_errors({
    foo <- tibble(x = 1:10, y = 1:10, z = 1:10)
    expect_error(
      foo[list(1:3)],
      class = "vctrs_error_subscript_type"
    )
    expect_error(
      foo[as.list(1:3)],
      class = "vctrs_error_subscript_type"
    )
    expect_error(
      foo[factor(1:3)],
      class = "vctrs_error_subscript_oob"
    )
    expect_error(
      foo[Sys.Date()],
      class = "vctrs_error_subscript_type"
    )
    expect_error(
      foo[Sys.time()],
      class = "vctrs_error_subscript_type"
    )
  })
})

test_that("[.tbl_df supports character subsetting (#312)", {
  foo <- tibble(x = 1:10, y = 1:10, z = 1:10)
  expect_identical(foo[as.character(2:4), ], foo[2:4, ])

  scoped_lifecycle_silence()

  expect_identical(foo[as.character(9:12), ], foo[c(9:10, NA, NA), ])
  expect_identical(
    foo[letters, ],
    foo[rlang::rep_along(letters, NA_integer_), ]
  )
  expect_identical(foo["9a", ], foo[NA_integer_, ])
})

test_that("[.tbl_df emits lifecycle warnings with invalid character subsetting", {
  scoped_lifecycle_errors()

  foo <- tibble(x = 1:10, y = 1:10, z = 1:10)
  expect_error(foo[as.character(9:12), ])
  expect_error(foo[letters, ])
  expect_error(foo["9a", ])
})

test_that("[.tbl_df supports integer subsetting (#312)", {
  foo <- tibble(x = 1:10, y = 1:10, z = 1:10)
  expect_identical(foo[2:4, ], as_tibble(as.data.frame(foo)[2:4, ]))
  expect_identical(foo[-3:-5, ], foo[c(1:2, 6:10), ])

  scoped_lifecycle_silence()

  expect_identical(foo[9:12, ], foo[c(9:10, NA, NA), ])
  expect_identical(foo[-(9:12), ], foo[1:8, ])
})

test_that("[.tbl_df emits lifecycle warnings with invalid integer subsetting", {
  scoped_lifecycle_errors()

  foo <- tibble(x = 1:10, y = 1:10, z = 1:10)
  expect_error(foo[9:12, ])
  expect_error(foo[-(9:12), ])
})

test_that("[.tbl_df supports character subsetting if row names are present (#312)", {
  foo <- as_tibble(mtcars, rownames = NA)
  idx <- function(x) rownames(mtcars)[x]

  expect_identical(foo[idx(2:4), ], foo[2:4, ])
  expect_identical(foo[idx(-3:-5), ], foo[-3:-5, ])
  expect_identical(foo[idx(29:34), ], foo[c(29:32, NA, NA), ])

  scoped_lifecycle_silence()

  expect_identical(
    foo[letters, ],
    foo[rlang::rep_along(letters, NA_integer_), ]
  )
  expect_identical(foo["9a", ], foo[NA_integer_, ])
})

test_that("[.tbl_df emits lifecycle warnings with invalid character subsetting", {
  scoped_lifecycle_errors()

  foo <- as_tibble(mtcars, rownames = NA)
  idx <- function(x) rownames(mtcars)[x]

  expect_error(foo[letters, ])
  expect_error(foo["9a", ])
})

test_that("[.tbl_df supports logical subsetting (#318)", {
  foo <- tibble(x = 1:10, y = 1:10, z = 1:10)
  expect_identical(foo[c(FALSE, rep(TRUE, 3), rep(F, 6)), ], foo[2:4, ])
  expect_identical(foo[TRUE, ], foo)
  expect_identical(foo[FALSE, ], foo[0L, ])

  expect_error(foo[c(TRUE, FALSE), ], class = "vctrs_error_subscript_size")
})

test_that("[.tbl_df is no-op if args missing", {
  expect_identical(df_all[], df_all)
})

test_that("[.tbl_df supports drop argument (#311)", {
  expect_identical(df_all[, 2, drop = TRUE], df_all[[2]])
  expect_identical(df_all[1, 2, drop = TRUE], df_all[[2]][[1]])
  expect_identical(df_all[1, , drop = TRUE], df_all[1, , ])
})

test_that("[.tbl_df ignores drop argument (with warning) without j argument (#307)", {
  expect_warning(expect_identical(df_all[1, drop = TRUE], df_all[1]))
})

test_that("[.tbl_df emits errors with matrix row subsetting (#760)", {
  scoped_lifecycle_errors()

  foo <- tibble(x = 1:10, y = 1:10, z = 1:10)
  expect_error(foo[matrix(1:2, ncol = 2), ])
  expect_error(foo[matrix(rep(TRUE, 10), ncol = 2), ])
})


test_that("[.tbl_df is careful about attributes (#155)", {
  df <- tibble(x = 1:2, y = x)
  attr(df, "along for the ride") <- "still here"

  expect_identical(attr(df[names(df)], "along for the ride"), "still here")
  expect_identical(attr(df["x"], "along for the ride"), "still here")
  expect_identical(attr(df[1:2], "along for the ride"), "still here")
  expect_identical(attr(df[2], "along for the ride"), "still here")
  expect_identical(attr(df[c(TRUE, FALSE)], "along for the ride"), "still here")
  expect_identical(attr(df[, names(df)], "along for the ride"), "still here")
  expect_identical(attr(df[, "x"], "along for the ride"), "still here")
  expect_identical(attr(df[, 1:2], "along for the ride"), "still here")
  expect_identical(attr(df[, 2], "along for the ride"), "still here")
  expect_identical(
    attr(df[, c(TRUE, FALSE)], "along for the ride"),
    "still here"
  )
  expect_identical(attr(df[1, names(df)], "along for the ride"), "still here")
  expect_identical(attr(df[1, "x"], "along for the ride"), "still here")
  expect_identical(attr(df[1, 1:2], "along for the ride"), "still here")
  expect_identical(attr(df[1, 2], "along for the ride"), "still here")
  expect_identical(
    attr(df[1, c(TRUE, FALSE)], "along for the ride"),
    "still here"
  )

  expect_identical(attr(df[1:2, ], "along for the ride"), "still here")
  expect_identical(attr(df[-1, ], "along for the ride"), "still here")

  expect_identical(attr(df[,], "along for the ride"), "still here")
  expect_identical(attr(df[], "along for the ride"), "still here")
})

# [[ ----------------------------------------------------------------------

test_that("[[.tbl_df ignores exact argument", {
  foo <- tibble(x = 1:10, y = 1:10)
  expect_warning(foo[["x"]], NA)
  expect_warning(foo[["x", exact = FALSE]], "ignored")
  expect_identical(getElement(foo, "y"), 1:10)
})

test_that("[[.tbl_df supports symbols (#691)", {
  foo <- tibble(x = 1:10, y = 1:10)
  expect_identical(foo[[quote(x)]], 1:10)
})

test_that("[[.tbl_df throws error with NA index", {
  verify_errors({
    foo <- tibble(x = 1:10, y = 1:10)
    expect_error(foo[[NA]])
    expect_error(foo[[NA_integer_]])
    expect_error(foo[[NA_real_]])
    expect_error(foo[[NA_character_]])
  })
})

test_that("[[ returns NULL if name doesn't exist", {
  scoped_lifecycle_silence()

  df <- tibble(x = 1)
  expect_null(df[["y"]])
  expect_null(df[[1, "y"]])
})

test_that("[[ drops inner names only with double subscript (#681)", {
  a <- c(x = 1)
  b <- data.frame(bb = 1, row.names = "y")
  c <- matrix(1, dimnames = list(rows = "z", cols = "cc"))

  df <- tibble(a, b = b, c)
  expect_identical(df[["a"]], a)
  expect_identical(df[[1, "a"]], 1)
  expect_identical(df[["b"]], b)
  expect_identical(df[[1, "b"]], data.frame(bb = 1))
  expect_identical(df[["c"]], c)
  expect_null(rownames(df[[1, "c"]]))

  df <- tibble(x = new_rcrd(list(a = 1:3)))
  expect_identical(df[[1, "x"]], new_rcrd(list(a = 1L)))
})

test_that("can use two-dimensional indexing with [[", {
  trees2 <- as_tibble(trees)
  expect_equal(trees2[[1, 2]], trees[[1, 2]])
  expect_equal(trees2[[2, 3]], trees[[2, 3]])
})

test_that("can use two-dimensional indexing with matrix and data frame columns (#440)", {
  df <- tibble::tibble(
    x = 1:3,
    y = matrix(9:1, ncol = 3),
    z = tibble::tibble(a = 1:3, b = 3:1)
  )

  expect_identical(df[[1, "y"]], df[1, ]$y)
  expect_identical(df[[1, "z"]], df[1, ]$z)
})

test_that("can use classed character indexes (#778)", {
  df <- tibble::tibble(a = 1:3, b = LETTERS[1:3])

  expect_identical(df[mychr(letters[1:2])], df)
  expect_identical(df[[mychr("a")]], df[["a"]])
  expect_null(df[[mychr("c")]])

  expect_silent(df[mychr(letters[1:2])] <- df)
  expect_silent(df[mychr(letters[3:4])] <- df)
  expect_silent(df[[mychr("c")]] <- 1)
  expect_silent(df[[mychr("a")]] <- df[["a"]])
})

test_that("can use classed integer indexes (#778)", {
  df <- tibble::tibble(a = 1:3, b = LETTERS[1:3])

  expect_identical(df[myint(1:3), myint(1:2)], df)
  expect_identical(df[[myint(2)]], df[[2]])

  expect_silent(df[myint(1:2)] <- df)
  expect_silent(df[myint(3:4)] <- list(c = 4, d = 5))
  expect_silent(df[[myint(2)]] <- df[[2]])
  expect_silent(df[[myint(3)]] <- 1)
})

test_that("can use classed logical indexes (#778)", {
  df <- tibble::tibble(a = 1:3, b = LETTERS[1:3])

  expect_identical(df[mylgl(TRUE), mylgl(TRUE)], df)

  expect_silent(df[mylgl(TRUE), ] <- df)
  expect_silent(df[mylgl(TRUE), mylgl(TRUE)] <- df)
})

# $ -----------------------------------------------------------------------

test_that("$ throws warning if name doesn't exist", {
  df <- tibble(x = 1)
  expect_warning(
    expect_null(df$y),
    "Unknown or uninitialised column: `y`",
    fixed = TRUE
  )
})

test_that("$ doesn't do partial matching", {
  df <- tibble(partial = 1)
  expect_warning(
    expect_null(df$p),
    "Unknown or uninitialised column: `p`",
    fixed = TRUE
  )
  expect_warning(
    expect_null(df$part),
    "Unknown or uninitialised column: `part`",
    fixed = TRUE
  )
  expect_error(df$partial, NA)
})

# [[<- --------------------------------------------------------------------

test_that("[[<-.tbl_df with two indexes assigns", {
  df <- tibble(x = 1:2, y = x)
  df[[1, "x"]] <- 3
  expect_identical(df, tibble(x = 3:2, y = 1:2))
  df[[2, 2]] <- 0
  expect_identical(df, tibble(x = 3:2, y = 1:0))
})

test_that("[[<-.tbl_df can update and add columns (#748)", {
  df <- tibble(x = 1:2, y = x)
  df[["x"]] <- 3:4
  expect_identical(df, tibble(x = 3:4, y = 1:2))
  df[["w"]] <- 5:6
  expect_identical(df, tibble(x = 3:4, y = 1:2, w = 5:6))
})

test_that("[[<-.tbl_df can remove columns (#666)", {
  df <- tibble(x = 1:2, y = x)
  df[["x"]] <- NULL
  expect_identical(df, tibble(y = 1:2))
  df[["z"]] <- NULL
  expect_identical(df, tibble(y = 1:2))
})

test_that("[[<-.tbl_df requires scalar, positive if numeric", {
  df <- tibble(x = 1:2, y = x)
  expect_error(df[[c("x", "y")]] <- 1, class = "vctrs_error_subscript_type")
  expect_error(df[[1:2]] <- 1, class = "vctrs_error_subscript_type")
  expect_error(df[[-1]] <- 1, class = "vctrs_error_subscript_type")
})

test_that("[[<-.tbl_df supports symbols (#691)", {
  foo <- tibble(x = 1:10, y = 1:10)
  foo[[quote(x)]] <- 10:1
  expect_identical(foo$x, 10:1)
})

# [<- ---------------------------------------------------------------------

test_that("[<-.tbl_df can remove columns", {
  df <- tibble(x = 1:2, y = x)
  df["x"] <- NULL
  expect_identical(df, tibble(y = 1:2))

  df <- tibble(x = 1:2, y = x)
  df[, "x"] <- NULL
  expect_identical(df, tibble(y = 1:2))

  df <- tibble(x = 1:2, y = x, z = y)
  df[, c("x", "z")] <- NULL
  expect_identical(df, tibble(y = 1:2))

  df["z"] <- NULL
  expect_identical(df, tibble(y = 1:2))
})

test_that("[<-.tbl_df throws an error with duplicate indexes (#658)", {
  verify_errors({
    df <- tibble(x = 1:2, y = x)
    expect_tibble_abort(
      df[c(1, 1)] <- 3,
      abort_assign_duplicate_column_subscript(c(1, 1))
    )
    expect_tibble_abort(
      df[, c(1, 1)] <- 3,
      abort_assign_duplicate_column_subscript(c(1, 1))
    )
    expect_tibble_abort(
      df[c(1, 1), ] <- 3,
      abort_assign_duplicate_row_subscript(c(1, 1))
    )
  })
})

test_that("[<-.tbl_df supports adding new rows with [i, j] (#651)", {
  df <- tibble(x = 1:2, y = x)
  df[3, "x"] <- 3
  expect_identical(df, tibble(x = 1:3, y = c(1:2, NA)))
  expect_false(has_rownames(df))
})

test_that("[<-.tbl_df supports adding new columns with [i, j] (#651)", {
  df <- tibble(x = 1:2, y = x)
  df[2, "z"] <- 3
  expect_identical(df, tibble(x = 1:2, y = x, z = c(NA, 3)))
  expect_false(has_rownames(df))
})

test_that("[<-.tbl_df supports adding new rows and columns with [i, j] (#651)", {
  df <- tibble(x = 1:2, y = x)
  df[3, "z"] <- 3
  expect_identical(df, tibble(x = c(1:2, NA), y = x, z = c(NA, NA, 3)))
  expect_false(has_rownames(df))
})

test_that("[<-.tbl_df supports negative subsetting", {
  df <- tibble(x = 1:3, y = x, z = y)
  df[2:3, 2:3] <- 0:-1
  expect_equal(df, tibble(x = 1:3, y = 1:-1, z = 1:-1))

  df <- tibble(x = 1:3, y = x, z = y)
  df[-1, 2:3] <- 0:-1
  expect_equal(df, tibble(x = 1:3, y = 1:-1, z = 1:-1))

  df <- tibble(x = 1:3, y = x, z = y)
  df[2:3, -1] <- 0:-1
  expect_equal(df, tibble(x = 1:3, y = 1:-1, z = 1:-1))

  df <- tibble(x = 1:3, y = x, z = y)
  df[2:3, -1] <- list(0:-1, 0:-1)
  expect_equal(df, tibble(x = 1:3, y = 1:-1, z = 1:-1))

  df <- tibble(x = 1:3, y = x, z = y)
  df[-1, -1] <- 0:-1
  expect_equal(df, tibble(x = 1:3, y = 1:-1, z = 1:-1))

  df <- tibble(x = 1:3, y = x, z = y)
  df[-1, -1] <- list(0:-1, 0:-1)
  expect_equal(df, tibble(x = 1:3, y = 1:-1, z = 1:-1))
})

test_that("[<-.tbl_df supports adding duplicate columns", {
  df <- tibble(x = 1:2)
  df[2] <- tibble(x = 3:4)
  expect_identical(df, tibble(x = 1:2, x = 3:4, .name_repair = "minimal"))
})


test_that("[<-.tbl_df supports matrix on the RHS (#762)", {
  df <- tibble(x = 1:4, y = letters[1:4])
  df[1:2] <- matrix(8:1, ncol = 2)
  expect_identical(df, tibble(x = 8:5, y = 4:1))

  df <- tibble(x = 1:4, y = letters[1:4])
  df[1:2] <- array(4:1, dim = c(4, 1, 1))
  expect_identical(df, tibble(x = 4:1, y = 4:1))

  df <- tibble(x = 1:4, y = letters[1:4])
  df[1:2] <- array(8:1, dim = c(4, 2, 1))
  expect_identical(df, tibble(x = 8:5, y = 4:1))

  df <- tibble(x = 1:4, y = letters[1:4])
  expect_tibble_abort(
    df[1:3, 1:2] <- matrix(6:1, ncol = 2),
    abort_assign_incompatible_type(
      df,
      as.data.frame(matrix(6:1, ncol = 2)),
      2,
      quote(matrix(6:1, ncol = 2)),
      tryCatch(vctrs::vec_assign(letters, 1:3, 3:1), error = identity)
    )
  )
  expect_tibble_abort(
    df[1:2] <- array(8:1, dim = c(2, 1, 4)),
    abort_need_rhs_vector_or_null(quote(array(8:1, dim = c(2, 1, 4))))
  )
  expect_tibble_abort(
    df[1:2] <- array(8:1, dim = c(4, 1, 2)),
    abort_need_rhs_vector_or_null(quote(array(8:1, dim = c(4, 1, 2))))
  )
})

test_that("[<- with explicit NULL doesn't change anything (#696)", {
  trees_tbl_orig <- as_tibble(trees)

  trees_tbl <- trees_tbl_orig
  trees_tbl[NULL] <- NA
  expect_identical(trees_tbl, trees_tbl_orig)

  trees_tbl <- trees_tbl_orig
  trees_tbl[, NULL] <- NA
  expect_identical(trees_tbl, trees_tbl_orig)

  trees_tbl <- trees_tbl_orig
  trees_tbl[NULL, ] <- NA
  expect_identical(trees_tbl, trees_tbl_orig)

  trees_tbl <- trees_tbl_orig
  trees_tbl[NULL, NULL] <- NA
  expect_identical(trees_tbl, trees_tbl_orig)
})

test_that("[<- with FALSE still adds column (#846)", {
  tbl <- tibble(a = 1:3)
  tbl[FALSE, "b"] <- 2
  expect_identical(tbl, tibble(a = 1:3, b = NA_real_))
})

test_that("[<-.tbl_df is careful about attributes (#155)", {
  df <- tibble(x = 1:2, y = x)
  attr(df, "along for the ride") <- "still here"

  df[names(df)] <- df
  expect_identical(attr(df, "along for the ride"), "still here")
  expect_false(has_rownames(df))
  df["x"] <- 3:4
  expect_identical(attr(df, "along for the ride"), "still here")
  expect_false(has_rownames(df))
  df[1:2] <- 5:6
  expect_identical(attr(df, "along for the ride"), "still here")
  expect_false(has_rownames(df))
  df[2] <- 7:8
  expect_identical(attr(df, "along for the ride"), "still here")
  expect_false(has_rownames(df))
  df[c(TRUE, FALSE)] <- 9:10
  expect_identical(attr(df, "along for the ride"), "still here")
  expect_false(has_rownames(df))

  df[, names(df)] <- df
  expect_identical(attr(df, "along for the ride"), "still here")
  expect_false(has_rownames(df))
  df[, "x"] <- 3:4
  expect_identical(attr(df, "along for the ride"), "still here")
  expect_false(has_rownames(df))
  df[, 1:2] <- 5:6
  expect_identical(attr(df, "along for the ride"), "still here")
  expect_false(has_rownames(df))
  df[, 2] <- 7:8
  expect_identical(attr(df, "along for the ride"), "still here")
  expect_false(has_rownames(df))
  df[, c(TRUE, FALSE)] <- 9:10
  expect_identical(attr(df, "along for the ride"), "still here")
  expect_false(has_rownames(df))

  df[1, names(df)] <- df[1, ]
  expect_identical(attr(df, "along for the ride"), "still here")
  expect_false(has_rownames(df))
  df[1, "x"] <- 3
  expect_identical(attr(df, "along for the ride"), "still here")
  expect_false(has_rownames(df))
  df[1, 1:2] <- 5
  expect_identical(attr(df, "along for the ride"), "still here")
  expect_false(has_rownames(df))
  df[1, 2] <- 7
  expect_identical(attr(df, "along for the ride"), "still here")
  expect_false(has_rownames(df))
  df[1, c(TRUE, FALSE)] <- 9
  expect_identical(attr(df, "along for the ride"), "still here")
  expect_false(has_rownames(df))

  df[1:2, ] <- df
  expect_identical(attr(df, "along for the ride"), "still here")
  expect_false(has_rownames(df))
  df[1:2, ] <- df[1, ]
  expect_identical(attr(df, "along for the ride"), "still here")
  expect_false(has_rownames(df))

  df[,] <- df
  expect_identical(attr(df, "along for the ride"), "still here")
  expect_false(has_rownames(df))
  df[] <- df
  expect_identical(attr(df, "along for the ride"), "still here")
  expect_false(has_rownames(df))
})

# $<- ---------------------------------------------------------------------

test_that("$<- doesn't throw warning if name doesn't exist", {
  df <- tibble(x = 1)
  expect_warning(
    df$y <- 2,
    NA
  )
  expect_identical(df, tibble(x = 1, y = 2))
  expect_false(has_rownames(df))
})

test_that("$<- throws different warning if attempting a partial initialization (#199)", {
  df <- tibble(x = 1:3)
  expect_warning(
    df$y[1] <- 2,
    "Unknown or uninitialised column: `y`",
    fixed = TRUE
  )

  expect_tibble_abort(
    expect_warning(
      df$z[1:2] <- 2,
      "Unknown or uninitialised column: `z`",
      fixed = TRUE
    ),
    abort_assign_incompatible_size(3, list(1:2), 1, NULL, quote(`<dbl>`))
  )
})

test_that("$<- recycles only values of length one", {
  df <- tibble(x = 1:3)

  df$y <- 4
  expect_identical(df, tibble(x = 1:3, y = 4))
  expect_false(has_rownames(df))

  df$z <- 5:7
  expect_identical(df, tibble(x = 1:3, y = 4, z = 5:7))
  expect_false(has_rownames(df))

  verify_errors({
    df <- tibble(x = 1:3)

    expect_tibble_abort(
      df$w <- 8:9,
      abort_assign_incompatible_size(3, list(8:9), 1, NULL, quote(8:9))
    )

    expect_tibble_abort(
      df$a <- character(),
      abort_assign_incompatible_size(
        3,
        list(character()),
        1,
        NULL,
        quote(character())
      )
    )
  })
})

test_that("output test", {
  skip_if_not_installed("vctrs", "0.6.5.9000")

  expect_snapshot(error = TRUE, {
    "# [.tbl_df is careful about names (#1245)"
    foo <- tibble(x = 1:10, y = 1:10)
    foo[c("x", "y", "z")]
    foo[c("w", "x", "y", "z")]
    foo[as.matrix("x")]
    foo[array("x", dim = c(1, 1, 1))]

    "# [.tbl_df is careful about column indexes (#83)"
    foo <- tibble(x = 1:10, y = 1:10, z = 1:10)
    foo[0.5]
    foo[1:5]
    foo[-1:1]
    foo[c(-1, 1)]
    foo[c(-1, NA)]
    foo[-4]
    foo[c(1:3, NA)]
    foo[as.matrix(1)]
    foo[array(1, dim = c(1, 1, 1))]
    foo[mean]
    foo[foo]

    "# [.tbl_df is careful about row indexes"
    foo <- tibble(x = 1:3, y = 1:3, z = 1:3)
    foo[0.5, ]
    invisible(foo[1:5, ])
    foo[-1:1, ]
    foo[c(-1, 1), ]
    foo[c(-1, NA), ]
    invisible(foo[-4, ])
    foo[array(1, dim = c(1, 1, 1)), ]
    foo[mean, ]
    foo[foo, ]

    "# [.tbl_df is careful about column flags (#83)"
    foo <- tibble(x = 1:10, y = 1:10, z = 1:10)
    foo[c(TRUE, TRUE)]
    foo[c(TRUE, TRUE, FALSE, FALSE)]
    foo[c(TRUE, TRUE, NA)]
    foo[as.matrix(TRUE)]
    foo[array(TRUE, dim = c(1, 1, 1))]

    "# [.tbl_df is careful about row flags"
    foo <- tibble(x = 1:3, y = 1:3, z = 1:3)
    foo[c(TRUE, TRUE), ]
    foo[c(TRUE, TRUE, FALSE, FALSE), ]
    foo[array(TRUE, dim = c(1, 1, 1)), ]

    "# [.tbl_df rejects unknown column indexes (#83)"
    foo <- tibble(x = 1:10, y = 1:10, z = 1:10)
    foo[list(1:3)]
    foo[as.list(1:3)]
    foo[factor(1:3)]
    foo[Sys.Date()]

    "# [.tbl_df rejects unknown row indexes"
    foo <- tibble(x = 1:10, y = 1:10, z = 1:10)
    foo[list(1:3), ]
    foo[as.list(1:3), ]
    foo[factor(1:3), ]
    foo[Sys.Date(), ]

    "# [.tbl_df and matrix subsetting"
    foo <- tibble(a = 1:3, b = letters[1:3])
    foo[is.na(foo)]
    foo[!is.na(foo)]
    foo[as.matrix("x")]
    foo[array("x", dim = c(1, 1, 1))]

    "# [.tbl_df and OOB indexing"
    foo <- tibble(a = 1:3, b = letters[1:3])
    invisible(foo[3:5, ])
    invisible(foo[-(3:5), ])
    invisible(foo["x", ])

    "# [.tbl_df and logical recycling"
    foo <- tibble(a = 1:4, b = a)
    foo[c(TRUE, FALSE), ]

    "# [[.tbl_df rejects invalid column indexes"
    foo <- tibble(x = 1:10, y = 1:10)
    foo[[]]
    foo[[, 1]]
    foo[[1, ]]
    foo[[,]]
    foo[[1:3]]
    foo[[letters[1:3]]]
    foo[[TRUE]]
    foo[[-1]]
    foo[[1.5]]
    foo[[3]]
    foo[[Inf]]
    foo[[mean]]
    foo[[foo]]

    "# [[.tbl_df throws error with NA index"
    foo <- tibble(x = 1:10, y = 1:10)
    foo[[NA]]

    "# $.tbl_df and partial matching/invalid columns"
    foo <- tibble(data = 1:10)
    foo$d
    foo$e

    "# [<-.tbl_df rejects unknown column indexes (#83)"
    foo <- tibble(x = 1:10, y = 1:10, z = 1:10)
    foo[list(1:3)] <- 1
    foo[as.list(1:3)] <- 1
    foo[factor(1:3)] <- 1
    foo[Sys.Date()] <- 1

    "# [.tbl_df emits lifecycle warnings with one-column matrix indexes (#760)"
    foo <- tibble(x = 1:10, y = 1:10, z = 1:10)
    invisible(foo[matrix(1:2, ncol = 1), ])
    invisible(foo[matrix(rep(TRUE, 10), ncol = 1), ])

    "# [<-.tbl_df rejects unknown row indexes"
    foo <- tibble(x = 1:10, y = 1:10, z = 1:10)
    foo[list(1:3), ] <- 1
    foo[as.list(1:3), ] <- 1
    foo[factor(1:3), ] <- 1
    foo[Sys.Date(), ] <- 1

    "# [<-.tbl_df throws an error with duplicate indexes (#658)"
    df <- tibble(x = 1:2, y = x)
    df[c(1, 1)] <- 3
    df[, c(1, 1)] <- 3
    df[c(1, 1), ] <- 3

    "# [<-.tbl_df throws an error with NA indexes"
    df <- tibble(x = 1:2, y = x)
    df[NA] <- 3
    df[NA, ] <- 3

    "# [<-.tbl_df and logical indexes"
    df <- tibble(x = 1:2, y = x)
    df[FALSE] <- 1
    df
    df[, TRUE] <- 2
    df

    "# [<-.tbl_df throws an error with bad RHS"
    df <- tibble(x = 1:2, y = x)
    df[] <- mean
    df[] <- lm(y ~ x, df)

    "# [<-.tbl_df throws an error with OOB assignment"
    df <- tibble(x = 1:2, y = x)
    df[4:5] <- 3
    df[4:5, ] <- 3
    df[-4, ] <- 3
    df[-(4:5), ] <- 3

    "# [<-.tbl_df and recycling"
    df <- tibble(x = 1:3, y = x, z = y)
    df[1:2] <- list(0, 0, 0)
    df[] <- list(0, 0)
    df[1, ] <- 1:3
    df[1:2, ] <- 1:3
    df[,] <- 1:2
    df[1, ] <- list(a = 1:3, b = 1)
    df[1, ] <- list(a = 1, b = 1:3)
    df[1:2, ] <- list(a = 1:3, b = 1)
    df[1:2, ] <- list(a = 1, b = 1:3)
    df[1, 1:2] <- list(a = 1:3, b = 1)
    df[1, 1:2] <- list(a = 1, b = 1:3)
    df[1:2, 1:2] <- list(a = 1:3, b = 1)
    df[1:2, 1:2] <- list(a = 1, b = 1:3)
    df[1, ] <- list(a = 1:3, b = 1, c = 1:3)
    df[1, ] <- list(a = 1, b = 1:3, c = 1:3)
    df[1:2, ] <- list(a = 1:3, b = 1, c = 1:3)
    df[1:2, ] <- list(a = 1, b = 1:3, c = 1:3)

    "# [<-.tbl_df and coercion"
    df <- tibble(x = 1:3, y = letters[1:3], z = as.list(1:3))
    df[1:3, 1:2] <- df[2:3]
    df[1:3, 1:2] <- df[1]
    df[1:3, 1:2] <- df[[1]]
    df[1:3, 1:3] <- df[3:1]
    df[1:3, 1:3] <- NULL

    "# [<-.tbl_df and overwriting NA"
    df <- tibble(
      x = rep(NA, 3),
      z = matrix(NA, ncol = 2, dimnames = list(NULL, c("a", "b")))
    )
    df[1, "x"] <- 5
    df[1, "z"] <- 5
    df

    "# [<-.tbl_df and overwriting with NA"
    df <- tibble(
      a = TRUE,
      b = 1L,
      c = sqrt(2),
      d = 3i + 1,
      e = "e",
      f = raw(1),
      g = tibble(x = 1, y = 1),
      h = matrix(1:3, nrow = 1)
    )
    df[FALSE, "a"] <- NA
    df[FALSE, "b"] <- NA
    df[FALSE, "c"] <- NA
    df[FALSE, "d"] <- NA
    df[FALSE, "e"] <- NA
    df[FALSE, "f"] <- NA
    df[FALSE, "g"] <- NA
    df[FALSE, "h"] <- NA
    df
    df[integer(), "a"] <- NA
    df[integer(), "b"] <- NA
    df[integer(), "c"] <- NA
    df[integer(), "d"] <- NA
    df[integer(), "e"] <- NA
    df[integer(), "f"] <- NA
    df[integer(), "g"] <- NA
    df[integer(), "h"] <- NA
    df
    df[1, "a"] <- NA
    df[1, "b"] <- NA
    df[1, "c"] <- NA
    df[1, "d"] <- NA
    df[1, "e"] <- NA
    df[1, "f"] <- NA
    df[1, "g"] <- NA
    df[1, "h"] <- NA
    df

    "# [<-.tbl_df and matrix subsetting"
    foo <- tibble(a = 1:3, b = letters[1:3])
    foo[!is.na(foo)] <- "bogus"
    foo[as.matrix("x")] <- NA
    foo[array("x", dim = c(1, 1, 1))] <- NA
    foo[is.na(foo)] <- 1:3
    foo[is.na(foo)] <- lm(a ~ b, foo)

    "# [[<-.tbl_df rejects invalid column indexes"
    foo <- tibble(x = 1:10, y = 1:10)
    foo[[]] <- 1
    foo[[, 1]] <- 1
    foo[[1, ]] <- 1
    foo[[,]] <- 1
    foo[[1:3]] <- 1
    foo[[letters[1:3]]] <- 1
    foo[[TRUE]] <- 1
    foo[[NA_integer_]] <- 1
    foo[[mean]] <- 1
    foo[[foo]] <- 1
    foo[[1:3, 1]] <- 1
    foo[[TRUE, 1]] <- 1
    foo[[mean, 1]] <- 1
    foo[[foo, 1]] <- 1

    "# [[<-.tbl_df throws an error with OOB assignment"
    df <- tibble(x = 1:2, y = x)
    df[[4]] <- 3

    "# [[<-.tbl_df throws an error with bad RHS"
    df <- tibble(x = 1:2, y = x)
    df[[1]] <- mean
    df[[1]] <- lm(y ~ x, df)

    "# [[<-.tbl_df recycles only values of length one"
    df <- tibble(x = 1:3)
    df[["x"]] <- 8:9
    df[["w"]] <- 8:9
    df[["a"]] <- character()

    "# [<-.tbl_df throws an error with invalid values"
    df <- tibble(x = 1:2, y = x)
    df[1] <- lm(y ~ x, df)
    df[1:2, 1] <- NULL

    "# $<- recycles only values of length one"
    df <- tibble(x = 1:3)
    df$x <- 8:9
    df$w <- 8:9
    df$a <- character()
  })
})

test_that("[[<- restores class", {
  skip_if_not_installed("dplyr")

  df <- dplyr::group_by(mtcars, cyl)
  df[[1]] <- mtcars$cyl
  expect_s3_class(df, "grouped_df")

  df <- dplyr::group_by(mtcars, cyl)
  df[[2]] <- mtcars$vs
  expect_s3_class(df, "grouped_df")
  expect_identical(dplyr::group_data(df)$cyl, c(0, 1))
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/tests/testthat/test-tbl_df.R ---
test_that("as.data.frame()", {
  df <- data.frame(a = 1:3)
  expect_identical(as.data.frame(as_tibble(df)), df)
})

test_that("as.data.frame() strips inner names (#837)", {
  tbl <- tibble(a = c(b = 1:3))
  expect_identical(as.data.frame(tbl)$a, 1:3)
})

test_that("as.data.frame() keeps inner names for lists (#837)", {
  tbl <- tibble(a = list(b = 1:3))
  expect_identical(as.data.frame(tbl)$a, list(b = 1:3))
})

test_that("as.data.frame() keeps inner names for records", {
  tbl <- tibble(x = new_rcrd(list(a = 1:3)))
  expect_identical(as.data.frame(tbl)$x, new_rcrd(list(a = 1:3)))
})

test_that("as.data.frame() keeps zero-column data frames and matrices (#970)", {
  tbl <- tibble(x = 1:2, y = new_tibble(list(), nrow = 2))
  expect_identical(as.data.frame(tbl)$y, tbl$y)

  mat <- tibble(x = 1:2, y = matrix(integer(), nrow = 2))
  expect_identical(as.data.frame(mat)$y, mat$y)
})

test_that("names<-()", {
  new_tbl <- function(...) {
    data <- list(1, "b")
    names(data) <- c(...)
    new_tibble(data, nrow = 1)
  }

  set_tbl_names <- function(names) {
    tbl_copy <- new_tbl("a", "b")
    names(tbl_copy) <- names
    tbl_copy
  }

  expect_equal(set_tbl_names(c("c", "d")), new_tbl("c", "d"))

  scoped_lifecycle_warnings()

  if (!is_rstudio()) {
    suppressWarnings(
      expect_warning(
        set_tbl_names(NULL),
        class = "lifecycle_warning_deprecated"
      )
    )
  }

  # Two warnings, require testthat > 2.3.2:
  suppressWarnings(
    expect_warning(
      expect_identical(set_tbl_names("c"), new_tbl("c", NA_character_)),
      class = "lifecycle_warning_deprecated"
    )
  )

  expect_warning(
    expect_identical(set_tbl_names(letters[3:5]), new_tbl("c", "d")),
    class = "lifecycle_warning_deprecated"
  )

  expect_warning(
    expect_identical(set_tbl_names(3:4), new_tbl(3:4)),
    class = "lifecycle_warning_deprecated"
  )
})

test_that("output test", {
  skip_if_not_installed("testthat", "3.0.0.9000")

  expect_snapshot({
    df <- tibble(a = 1, b = 2)

    names(df) <- NULL
    names(df) <- "c"
    names(df) <- c("..1", "..2")
  })
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/tests/testthat/test-tbl_sum.R ---
test_that("output test", {
  expect_snapshot({
    str(tbl_sum(1:3))
    str(tbl_sum(vctrs::new_data_frame(a = 1:3, class = "tbl")))
    str(tbl_sum(tibble(a = 1:3, b = letters[2:4])))

    dim_desc(trees)
    dim_desc(Titanic)
    dim_desc(1:3)
  })
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/tests/testthat/test-tibble.R ---
test_that("tibble returns correct number of rows with all combinatinos", {
  expect_equal(nrow(tibble(value = 1:10)), 10L)
  expect_equal(nrow(tibble(value = 1:10, name = "recycle_me")), 10L)
  expect_equal(nrow(tibble(name = "recycle_me", value = 1:10)), 10L)
  expect_equal(
    nrow(tibble(name = "recycle_me", value = 1:10, value2 = 11:20)),
    10L
  )
  expect_equal(
    nrow(tibble(value = 1:10, name = "recycle_me", value2 = 11:20)),
    10L
  )
})

test_that("NULL is ignored (#580)", {
  expect_identical(tibble(a = NULL), tibble())
  expect_identical(tibble(a = NULL, a = 1), tibble(a = 1))
  expect_identical(tibble(a = NULL, b = 1, c = 2:3), tibble(b = 1, c = 2:3))
  expect_identical(tibble(b = 1, NULL, c = 2:3), tibble(b = 1, c = 2:3))
})

test_that("NULL is ignored when passed by value (#895, #900)", {
  expect_identical(tibble(a = c()), tibble(a = NULL))
  expect_identical(tibble(a = c(), a = 1), tibble(a = 1))
})

test_that("bogus columns raise an error", {
  expect_tibble_abort(
    tibble(a = new.env()),
    abort_column_scalar_type("a", 1, "an environment")
  )
  expect_tibble_abort(
    tibble(a = quote(a)),
    abort_column_scalar_type("a", 1, "a symbol")
  )
})

test_that("length 1 vectors are recycled", {
  expect_equal(nrow(tibble(x = 1:10)), 10)
  expect_equal(nrow(tibble(x = 1:10, y = 1)), 10)
  expect_tibble_abort(
    tibble(x = 1:10, y = 1:2),
    abort_incompatible_size(10, "y", 2, "Existing data")
  )
})

test_that("length 1 vectors in hierarchical data frames are recycled (#502)", {
  expect_identical(
    tibble(x = 1:10, y = tibble(z = 1)),
    tibble(x = 1:10, y = tibble(z = rep(1, 10)))
  )
  expect_identical(
    tibble(y = tibble(z = 1), x = 1:10),
    tibble(y = tibble(z = rep(1, 10)), x = 1:10)
  )
  expect_identical(
    tibble(x = 1, y = tibble(z = 1:10)),
    tibble(x = rep(1, 10), y = tibble(z = 1:10))
  )
  expect_identical(
    tibble(y = tibble(z = 1:10), x = 1),
    tibble(y = tibble(z = 1:10), x = rep(1, 10))
  )
})

test_that("missing names are imputed from call", {
  x <- 1:10
  df <- tibble(x, y = x)
  expect_equal(names(df), c("x", "y"))
})

test_that("empty input makes 0 x 0 tbl_df", {
  zero <- tibble()
  expect_s3_class(zero, "tbl_df")
  expect_equal(dim(zero), c(0L, 0L))
  expect_identical(attr(zero, "names"), character(0L))
})

test_that("SE version", {
  scoped_lifecycle_silence()
  expect_identical(tibble_(list(a = ~ 1:10)), tibble(a = 1:10))
})

test_that("names are maintained vectors (#630)", {
  foo <- tibble(x = c(y = 1, z = 2))
  expect_equal(names(foo), "x")
  expect_equal(names(foo$x), c("y", "z"))
})

test_that("names in list columns are maintained (#630)", {
  foo <- tibble(x = list(y = 1:3, z = 4:5))
  expect_equal(names(foo), "x")
  expect_equal(names(foo$x), c("y", "z"))
})

test_that("can create a tibble with an expression column (#657)", {
  foo <- tibble(x = expression(1 + 2))
  expect_equal(as.list(foo$x), as.list(expression(1 + 2)))
})

test_that("attributes are preserved", {
  df <- structure(
    data.frame(x = 1:10, g1 = rep(1:2, each = 5), g2 = rep(1:5, 2)),
    meta = "this is important"
  )
  res <- as_tibble(df)

  expect_identical(attr(res, "meta"), attr(df, "meta"))
})

test_that(".data pronoun", {
  expect_identical(tibble(a = 1, b = .data$a), tibble(a = 1, b = 1))
})

# Validation --------------------------------------------------------------

test_that("NULL isn't a valid column", {
  expect_tibble_abort(
    check_valid_cols(list(a = NULL)),
    abort_column_scalar_type("a", 1, "NULL")
  )
})

test_that("mutate() semantics for tibble() (#213)", {
  expect_equal(
    tibble(a = 1:2, b = 1, c = b / sum(b)),
    tibble(a = 1:2, b = c(1, 1), c = c(0.5, 0.5))
  )

  expect_equal(
    tibble(b = 1, a = 1:2, c = b / sum(b)),
    tibble(b = c(1, 1), a = 1:2, c = c(0.5, 0.5))
  )

  expect_equal(
    tibble(b = 1, c = b / sum(b), a = 1:2),
    tibble(b = c(1, 1), c = c(1, 1), a = 1:2)
  )
})

test_that("types preserved when recycling in tibble() (#284)", {
  expect_equal(
    tibble(a = 1:2, b = as.difftime(1, units = "hours")),
    tibble(a = 1:2, b = as.difftime(c(1, 1), units = "hours"))
  )

  expect_equal(
    tibble(b = as.difftime(1, units = "hours"), a = 1:2),
    tibble(b = as.difftime(c(1, 1), units = "hours"), a = 1:2)
  )
})

# Data frame and matrix columns -------------------------------------------

test_that("can make tibble containing data.frame or array (#416)", {
  expect_identical(
    tibble(mtcars = remove_rownames(mtcars)),
    new_tibble(list(mtcars = remove_rownames(mtcars)), nrow = nrow(mtcars))
  )
  expect_identical(
    tibble(diag(5)),
    new_tibble(list(`diag(5)` = diag(5)), nrow = 5)
  )
})

test_that("auto-splicing anonymous tibbles (#581)", {
  df <- tibble(a = 1, b = 2)
  expect_identical(
    tibble(df),
    df
  )
  expect_identical(
    tibble(df, c = b),
    add_column(df, c = 2)
  )
})

test_that("can coerce list data.frame or array (#416)", {
  expect_identical(
    as_tibble(list(x = trees)),
    new_tibble(list(x = trees), nrow = nrow(trees))
  )
  expect_identical(
    as_tibble(list(x = diag(5))),
    new_tibble(list(x = diag(5)), nrow = 5)
  )
})

test_that("susbsetting returns the correct number of rows", {
  expect_identical(
    tibble(x = mtcars)[1:3, ],
    tibble(x = mtcars[1:3, ])
  )
  expect_identical(
    tibble(y = diag(5))[1:3, ],
    tibble(y = diag(5)[1:3, ])
  )
})

test_that("subsetting one row retains columns", {
  expect_identical(
    tibble(y = diag(5))[1, ],
    tibble(y = diag(5)[1, , drop = FALSE])
  )
})

test_that("package_version is a vector (#690)", {
  ver <- utils::packageVersion("tibble")

  expect_identical(tibble(x = ver)$x, ver)
})


# tibble_row() ------------------------------------------------------------

test_that("returns a single row (#416)", {
  model <- lm(Height ~ Girth + Volume, data = trees)
  expect_identical(
    tibble_row(a = 1, b = vctrs::list_of(2:3), lm = model),
    tibble(a = 1, b = vctrs::list_of(2:3), lm = list(model))
  )
  expect_equal(
    tibble_row(trees[1, ]),
    tibble(trees[1, ])
  )
  expect_tibble_abort(
    tibble_row(a = 1, b = 2:3),
    abort_tibble_row_size_one(2, "b", 2)
  )
  expect_tibble_abort(
    tibble_row(trees[2:3, ]),
    abort_tibble_row_size_one(1, "", 2)
  )
})

# is_tibble ---------------------------------------------------------------

test_that("is_tibble", {
  expect_false(is_tibble(trees))
  expect_true(is_tibble(as_tibble(trees)))
  expect_false(is_tibble(NULL))
  expect_false(is_tibble(0))
})

test_that("is_tibble", {
  scoped_lifecycle_silence()
  expect_identical(is.tibble(trees), is_tibble(trees))
})

test_that("output test", {
  expect_snapshot(error = TRUE, {
    tibble(a = 1, a = 1)
    tibble(a = new_environment())
    tibble(a = 1, b = 2:3, c = 4:6, d = 7:10)
  })
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/tests/testthat/test-tidy_names.R ---
test_that("zero-length inputs given character names", {
  out <- set_tidy_names(character())
  expect_equal(names(out), character())
})

test_that("unnamed input gives uniquely named output", {
  expect_snapshot(
    out <- set_tidy_names(1:3)
  )
  expect_equal(names(out), c("..1", "..2", "..3"))
})

test_that("messages by default", {
  expect_snapshot(
    set_tidy_names(set_names(1, ""))
  )
})

test_that("quiet = TRUE", {
  expect_message(set_tidy_names(set_names(1, ""), quiet = TRUE), NA)
})

test_that("syntactic = TRUE", {
  out <- set_tidy_names(set_names(1, "a b"))
  expect_equal(names(out), tidy_names("a b"))
})

# tidy_names ---------------------------------------------------------------

test_that("zero-length input", {
  expect_equal(tidy_names(character()), character())
})

test_that("proper names", {
  expect_equal(tidy_names(letters), letters)
})

test_that("dupes", {
  expect_snapshot(
    names <- tidy_names(c("a", "b", "a", "c", "b"))
  )
  expect_equal(names, c("a..1", "b..2", "a..3", "c", "b..5"))
})

test_that("empty", {
  expect_snapshot(
    names <- tidy_names("")
  )
  expect_equal(names, "..1")
})

test_that("NA", {
  expect_snapshot(
    names <- tidy_names(NA_character_)
  )
  expect_equal(names, "..1")
})

test_that("corner case", {
  expect_snapshot({
    expect_equal(tidy_names(c("a..2", "a")), c("a..2", "a"))
    expect_equal(tidy_names(c("a..3", "a", "a")), c("a..1", "a..2", "a..3"))
    expect_equal(tidy_names(c("a..2", "a", "a")), c("a..1", "a..2", "a..3"))
    expect_equal(
      tidy_names(c("a..2", "a..2", "a..2")),
      c("a..1", "a..2", "a..3")
    )
  })
})

test_that("syntactic", {
  expect_snapshot(
    names <- tidy_names(c("a b"), syntactic = TRUE)
  )
  expect_equal(names, make.names("a b"))
})

test_that("some syntactic + message (#260)", {
  expect_snapshot(
    names <- tidy_names(c("a b", "c"), syntactic = TRUE)
  )
  expect_equal(names, c(make.names("a b"), "c"))
})

test_that("message", {
  expect_message(
    tidy_names(""),
    "New names:\n -> ..1\n",
    fixed = TRUE
  )
})

test_that("quiet", {
  expect_message(
    tidy_names("", quiet = TRUE),
    NA
  )
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/tests/testthat/test-tribble.R ---
test_that("tribble() constructs 'tibble' as expected", {
  result <- tribble(
    ~colA, ~colB,
    "a", 1,
    "b", 2
  )

  compared <- tibble(colA = c("a", "b"), colB = c(1, 2))
  expect_equal(result, compared)

  ## wide
  wide <- tribble(
    ~colA, ~colB, ~colC, ~colD,
    1, 2, 3, 4,
    5, 6, 7, 8
  )

  wide_expectation <- tibble(
    colA = c(1, 5),
    colB = c(2, 6),
    colC = c(3, 7),
    colD = c(4, 8)
  )

  expect_equal(wide, wide_expectation)

  ## long
  long <- tribble(
    ~colA, ~colB,
    1, 6,
    2, 7,
    3, 8,
    4, 9,
    5, 10
  )

  long_expectation <- tibble(
    colA = as.numeric(1:5),
    colB = as.numeric(6:10)
  )

  expect_equal(long, long_expectation)
})

test_that("tribble() tolerates a trailing comma", {
  result <- tribble(
    ~colA, ~colB,
    "a", 1,
    "b", 2,
  )

  compared <- tibble(colA = c("a", "b"), colB = c(1, 2))
  expect_equal(result, compared)
})

test_that("tribble() handles columns with a class (#161)", {
  sys_date <- Sys.Date()
  sys_time <- Sys.time()
  date_time_col <- tribble(
    ~dt, ~dttm,
    sys_date, sys_time,
    as.Date("2003-01-02"), as.POSIXct("2004-04-05 13:45:17", tz = "UTC")
  )

  date_time_col_expectation <- tibble(
    dt = vec_c(sys_date, as.Date("2003-01-02")),
    dttm = vec_c(sys_time, as.POSIXct("2004-04-05 13:45:17", tz = "UTC"))
  )

  expect_equal(date_time_col, date_time_col_expectation)
})

test_that("tribble() creates lists for non-atomic inputs (#7)", {
  expect_identical(
    tribble(~a, ~b, NA, "A", letters, LETTERS[-1L]),
    tibble(a = list(NA, letters), b = list("A", LETTERS[-1L]))
  )

  expect_identical(
    tribble(~a, ~b, NA, NULL, 1, 2),
    tibble(a = c(NA, 1), b = list(NULL, 2))
  )
})

test_that("tribble() errs appropriately on bad calls", {
  # no colname
  expect_tibble_abort(
    tribble(1, 2, 3),
    abort_tribble_needs_columns()
  )

  # invalid colname syntax
  expect_tibble_abort(
    tribble(a ~ b),
    abort_tribble_lhs_column_syntax(quote(a))
  )

  # invalid colname syntax
  expect_tibble_abort(
    tribble(~ a + b),
    abort_tribble_rhs_column_syntax(quote(a + b))
  )

  # tribble() must be passed colnames
  expect_tibble_abort(
    tribble(
      "a", "b",
      1, 2
    ),
    abort_tribble_needs_columns()
  )

  # tribble() must produce rectangular structure (no filling)
  expect_tibble_abort(
    tribble(
      ~a, ~b, ~c,
      1, 2,
      3, 4, 5
    ),
    abort_tribble_non_rectangular(3, 5)
  )

  expect_tibble_abort(
    tribble(
      ~a, ~b, ~c, ~d,
      1, 2, 3, 4, 5,
      6, 7, 8, 9,
    ),
    abort_tribble_non_rectangular(4, 9)
  )
})

test_that("tribble can have list columns", {
  df <- tribble(
    ~x, ~y,
    1, list(a = 1),
    2, list(b = 2)
  )
  expect_equal(df$x, c(1, 2))
  expect_equal(df$y, list(list(a = 1), list(b = 2)))
})

test_that("tribble creates n-col empty data frame", {
  df <- tribble(~x, ~y)
  expect_equal(df, tibble(x = unspecified(), y = unspecified()))
})

test_that("tribble recognizes quoted non-formula call", {
  df <- tribble(
    ~x, ~y,
    quote(mean(1)), 1
  )
  expect_equal(df$x, list(quote(mean(1))))
  expect_equal(df$y, 1)
})

test_that("tribble returns 0x0 tibble when there's no argument", {
  df <- tribble()
  expect_equal(df, tibble())
})

test_that("names stripped at appropriate time (#775)", {
  expect_equal(
    tribble(~x, c(a = 1)),
    tibble(x = 1)
  )
})

test_that("lubridate::Period (#784)", {
  skip_if_not_installed("lubridate")
  expect_equal(
    tribble(~x, lubridate::days(1), lubridate::days(2)),
    tibble(x = lubridate::days(1:2))
  )
})

test_that("formattable (#785)", {
  skip_if_not_installed("formattable")
  expect_equal(
    tribble(~x, formattable::formattable(1.0, 1), formattable::formattable(2.0, 1)),
    tibble(x = formattable::formattable(1:2 + 0, 1))
  )
})

# ---- frame_matrix() ----

test_that("frame_matrix constructs a matrix as expected", {
  result <- frame_matrix(
    ~col1,
    ~col2,
    10,
    3,
    5,
    2
  )
  expected <- matrix(c(10, 5, 3, 2), ncol = 2)
  colnames(expected) <- c("col1", "col2")
  expect_equal(result, expected)
})

test_that("frame_matrix constructs empty matrix as expected", {
  result <- frame_matrix(
    ~col1,
    ~col2
  )
  expected <- matrix(logical(), ncol = 2)
  colnames(expected) <- c("col1", "col2")
  expect_equal(result, expected)
})

test_that("frame_matrix cannot have list columns", {
  expect_tibble_abort(
    frame_matrix(
      ~x,
      ~y,
      "a",
      1:3,
      "b",
      4:6
    ),
    abort_frame_matrix_list(c(2, 4))
  )
})

test_that("tribble and frame_matrix cannot have named arguments", {
  expect_tibble_abort(
    extract_frame_data_from_dots(
      ~x,
      ~y,
      "a" = 1:3,
      "b" = 4:6
    ),
    abort_tribble_named_after_tilde()
  )
})

test_that("output test", {
  expect_snapshot(error = TRUE, {
    tribble(1)
    tribble(~a, ~b, 1)
    tribble(a ~ b, 1)
    tribble(a ~ b + c, 1)
    tribble(~b, 1, "a")

    frame_matrix(1)
    frame_matrix(~a, list(1))
  })
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/tests/testthat/test-type_sum.R ---
test_that("works with glimpse", {
  foo <- as_override_type_sum(2011:2013)
  expect_equal(type_sum(foo), "SC")
  expect_output(glimpse(tibble(foo)), "foo <SC>")
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/tests/testthat/test-utils-msg-format.R ---
test_that("pluralise works correctly", {
  expect_identical(pluralise("[an ]index(es)", c("x")), "an index")
  expect_identical(pluralise("[an ]index(es)", c("x", "y")), "indexes")
})

test_that("pluralise leaves alone parentheses / square brackets that have spaces inside", {
  expect_identical(
    pluralise("[an ]invalid index(es) (be careful) [for real]", c("x")),
    "an invalid index (be careful) [for real]"
  )
  expect_identical(
    pluralise("[an ]invalid index(es) (be careful) [for real]", c("x", "y")),
    "invalid indexes (be careful) [for real]"
  )
})

test_that("pluralise_msg works correctly", {
  expect_identical(pluralise_msg("[an ]index(es) ", c("x")), "an index `x`")
  expect_identical(
    pluralise_msg("[an ]index(es) ", c("x", "y")),
    "indexes `x`, `y`"
  )
  expect_identical(
    pluralise_msg("[an ]index(es) ", c(-4, -5)),
    "indexes -4, -5"
  )
})

test_that("output test", {
  expect_snapshot({
    "# Problems"
    writeLines(format_error_bullets(problems("header", c("item 1", "item 2"))))
    writeLines(format_error_bullets(problems("header", LETTERS)))
    writeLines(format_error_bullets(problems("header", as.character(1:6))))

    "# Bullets"
    writeLines(format_error_bullets(bullets("header", c("item 1", "item 2"))))
    writeLines(format_error_bullets(bullets("header", LETTERS)))
    writeLines(format_error_bullets(bullets("header", as.character(1:6))))

    "# Commas"
    commas("1")
    commas(letters[2:4])
    commas(LETTERS)
  })
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/tests/testthat/test-utils-tick.R ---
test_that("output test", {
  expect_snapshot({
    name_or_pos(c("a", "", "c"), 1:3)

    cat(tick(c("a", "b c", "if", "`")))
    cat(tick_if_needed(c("a", "b c", "if", "`")))
  })
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/tests/testthat/test-utils.R ---
test_that("needs_dim()", {
  expect_false(needs_dim(NULL))
  expect_false(needs_dim(1))
  expect_false(needs_dim(1:3))
  expect_true(needs_dim(trees))
  expect_true(needs_dim(Titanic))
})

test_that("has_nonnull_names()", {
  expect_false(has_null_names(c(a = 1)))
  expect_true(has_null_names(NULL))
  expect_true(has_null_names(1))
})

test_that("needs_list_col()", {
  expect_false(needs_list_col(1))
  expect_false(needs_list_col(matrix(1:3, nrow = 1, ncol = 3)))
  expect_true(needs_list_col(matrix(1:3, nrow = 3, ncol = 1)))
  expect_true(needs_list_col(list(1:3)))
  expect_true(needs_list_col(1:3))
  expect_true(needs_list_col(integer()))
  expect_true(needs_list_col(NULL))
  expect_true(needs_list_col(trees))
  expect_true(needs_list_col(Titanic))
})

test_that("nchar_width()", {
  expect_equal(nchar_width(""), 0)
  expect_equal(nchar_width("abc"), 3)
  expect_equal(nchar_width("\u6210"), 2)
})

test_that("is_rstudio()", {
  expect_false(withr::with_envvar(c(RSTUDIO = NA), is_rstudio()))
  expect_true(withr::with_envvar(c(RSTUDIO = 1), is_rstudio()))
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/tests/testthat/test-vignette-digits.R ---
test_that("digits vignette", {
  test_galley("digits")
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/tests/testthat/test-vignette-extending.R ---
test_that("extending vignette", {
  test_galley("extending")
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/tests/testthat/test-vignette-formats.R ---
test_that("formats vignette", {
  skip_if_not_installed("knitr", "1.34.2")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("formattable")
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("tidyr")

  # Fails on Linux
  test_galley("formats")
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/tests/testthat/test-vignette-invariants.R ---
test_that("invariants vignette", {
  skip_if_not_installed("vctrs", "0.4.1.9000")
  skip_if_not_installed("knitr", "1.50.4")
  skip_if(getRversion() < "4.0")
  test_galley("invariants", variant = rlang_pillar_variant())
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/tests/testthat/test-vignette-numbers.R ---
test_that("numbers vignette", {
  skip_if_not_installed("dplyr")

  test_galley("numbers")
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/tests/testthat/test-vignette-tibble.R ---
test_that("tibble vignette", {
  # Unclear
  skip_if_not_installed("knitr", "1.34.2")

  test_galley("tibble", variant = rlang_variant())
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/tibble/main/tests/testthat/test-vignette-types.R ---
test_that("types vignette", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")
  skip_if_not_installed("purrr")
  skip_if_not_installed("bit64")
  skip_if_not_installed("blob")
  skip_if_not_installed("hms")

  test_galley("types", variant = rlang_variant())
})
