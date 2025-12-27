

# --- FILE: https://raw.githubusercontent.com/tidyverse/stringr/main/R/c.R ---
#' Join multiple strings into one string
#'
#' @description
#' `str_c()` combines multiple character vectors into a single character
#' vector. It's very similar to [paste0()] but uses tidyverse recycling and
#' `NA` rules.
#'
#' One way to understand how `str_c()` works is picture a 2d matrix of strings,
#' where each argument forms a column. `sep` is inserted between each column,
#' and then each row is combined together into a single string. If `collapse`
#' is set, it's inserted between each row, and then the result is again
#' combined, this time into a single string.
#'
#' @param ... One or more character vectors.
#'
#'   `NULL`s are removed; scalar inputs (vectors of length 1) are recycled to
#'   the common length of vector inputs.
#'
#'   Like most other R functions, missing values are "infectious": whenever
#'   a missing value is combined with another string the result will always
#'   be missing. Use [dplyr::coalesce()] or [str_replace_na()] to convert to
#'   the desired value.
#' @param sep String to insert between input vectors.
#' @param collapse Optional string used to combine output into single
#'   string. Generally better to use [str_flatten()] if you needed this
#'   behaviour.
#' @return If `collapse = NULL` (the default) a character vector with
#'   length equal to the longest input. If `collapse` is a string, a character
#'   vector of length 1.
#' @export
#' @examples
#' str_c("Letter: ", letters)
#' str_c("Letter", letters, sep = ": ")
#' str_c(letters, " is for", "...")
#' str_c(letters[-26], " comes before ", letters[-1])
#'
#' str_c(letters, collapse = "")
#' str_c(letters, collapse = ", ")
#'
#' # Differences from paste() ----------------------
#' # Missing inputs give missing outputs
#' str_c(c("a", NA, "b"), "-d")
#' paste0(c("a", NA, "b"), "-d")
#' # Use str_replace_NA to display literal NAs:
#' str_c(str_replace_na(c("a", NA, "b")), "-d")
#'
#' # Uses tidyverse recycling rules
#' \dontrun{str_c(1:2, 1:3)} # errors
#' paste0(1:2, 1:3)
#'
#' str_c("x", character())
#' paste0("x", character())
str_c <- function(..., sep = "", collapse = NULL) {
  check_string(sep)
  check_string(collapse, allow_null = TRUE)

  dots <- list(...)
  dots <- dots[!map_lgl(dots, is.null)]
  vctrs::vec_size_common(!!!dots)

  inject(stri_c(!!!dots, sep = sep, collapse = collapse))
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/stringr/main/R/case.R ---
#' Convert string to upper case, lower case, title case, or sentence case
#'
#' * `str_to_upper()` converts to upper case.
#' * `str_to_lower()` converts to lower case.
#' * `str_to_title()` converts to title case, where only the first letter of
#'   each word is capitalized.
#' * `str_to_sentence()` convert to sentence case, where only the first letter
#'   of sentence is capitalized.
#'
#' @inheritParams str_detect
#' @inheritParams coll
#' @return A character vector the same length as `string`.
#' @examples
#' dog <- "The quick brown dog"
#' str_to_upper(dog)
#' str_to_lower(dog)
#' str_to_title(dog)
#' str_to_sentence("the quick brown dog")
#'
#' # Locale matters!
#' str_to_upper("i") # English
#' str_to_upper("i", "tr") # Turkish
#' @name case
NULL

#' @export
#' @rdname case
str_to_upper <- function(string, locale = "en") {
  check_string(locale)
  copy_names(string, stri_trans_toupper(string, locale = locale))
}
#' @export
#' @rdname case
str_to_lower <- function(string, locale = "en") {
  check_string(locale)
  copy_names(string, stri_trans_tolower(string, locale = locale))
}
#' @export
#' @rdname case
str_to_title <- function(string, locale = "en") {
  check_string(locale)
  out <- stri_trans_totitle(
    string,
    opts_brkiter = stri_opts_brkiter(locale = locale)
  )
  copy_names(string, out)
}
#' @export
#' @rdname case
str_to_sentence <- function(string, locale = "en") {
  check_string(locale)
  out <- stri_trans_totitle(
    string,
    opts_brkiter = stri_opts_brkiter(type = "sentence", locale = locale)
  )
  copy_names(string, out)
}


#' Convert between different types of programming case
#'
#' @description
#' * `str_to_camel()` converts to camel case, where the first letter of
#'   each word is capitalized, with no separation between words. By default
#'   the first letter of the first word is not capitalized.
#'
#' * `str_to_kebab()` converts to kebab case, where words are converted to
#'   lower case and separated by dashes (`-`).
#'
#' * `str_to_snake()` converts to snake case, where words are converted to
#'   lower case and separated by underscores (`_`).
#' @inheritParams str_to_lower
#' @export
#' @param first_upper Logical. Should the first letter be capitalized?
#' @examples
#' str_to_camel("my-variable")
#' str_to_camel("my-variable", first_upper = TRUE)
#'
#' str_to_snake("MyVariable")
#' str_to_kebab("MyVariable")
str_to_camel <- function(string, first_upper = FALSE) {
  check_character(string)
  check_bool(first_upper)

  string <- string |>
    to_words() |>
    str_to_title() |>
    str_remove_all(pattern = fixed(" "))

  if (!first_upper) {
    str_sub(string, 1, 1) <- str_to_lower(str_sub(string, 1, 1))
  }

  string
}
#' @export
#' @rdname str_to_camel
str_to_snake <- function(string) {
  check_character(string)
  to_separated_case(string, sep = "_")
}
#' @export
#' @rdname str_to_camel
str_to_kebab <- function(string) {
  check_character(string)
  to_separated_case(string, sep = "-")
}

to_separated_case <- function(string, sep) {
  out <- to_words(string)
  str_replace_all(out, fixed(" "), sep)
}

to_words <- function(string) {
  breakpoints <- paste(
    # non-word characters
    "[^\\p{L}\\p{N}]+",
    # lowercase followed by uppercase
    "(?<=\\p{Ll})(?=\\p{Lu})",
    # letter followed by number
    "(?<=\\p{L})(?=\\p{N})",
    # number followed by letter
    "(?<=\\p{N})(?=\\p{L})",
    # uppercase followed uppercase then lowercase (i.e. end of acronym)
    "(?<=\\p{Lu})(?=\\p{Lu}\\p{Ll})",
    sep = "|"
  )
  out <- str_replace_all(string, breakpoints, " ")
  out <- str_to_lower(out)
  str_trim(out)
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/stringr/main/R/compat-obj-type.R ---
# nocov start --- r-lib/rlang compat-obj-type
#
# Changelog
# =========
#
# 2022-10-04:
# - `obj_type_friendly(value = TRUE)` now shows numeric scalars
#   literally.
# - `stop_friendly_type()` now takes `show_value`, passed to
#   `obj_type_friendly()` as the `value` argument.
#
# 2022-10-03:
# - Added `allow_na` and `allow_null` arguments.
# - `NULL` is now backticked.
# - Better friendly type for infinities and `NaN`.
#
# 2022-09-16:
# - Unprefixed usage of rlang functions with `rlang::` to
#   avoid onLoad issues when called from rlang (#1482).
#
# 2022-08-11:
# - Prefixed usage of rlang functions with `rlang::`.
#
# 2022-06-22:
# - `friendly_type_of()` is now `obj_type_friendly()`.
# - Added `obj_type_oo()`.
#
# 2021-12-20:
# - Added support for scalar values and empty vectors.
# - Added `stop_input_type()`
#
# 2021-06-30:
# - Added support for missing arguments.
#
# 2021-04-19:
# - Added support for matrices and arrays (#141).
# - Added documentation.
# - Added changelog.

#' Return English-friendly type
#' @param x Any R object.
#' @param value Whether to describe the value of `x`. Special values
#'   like `NA` or `""` are always described.
#' @param length Whether to mention the length of vectors and lists.
#' @return A string describing the type. Starts with an indefinite
#'   article, e.g. "an integer vector".
#' @noRd
obj_type_friendly <- function(x, value = TRUE) {
  if (is_missing(x)) {
    return("absent")
  }

  if (is.object(x)) {
    if (inherits(x, "quosure")) {
      type <- "quosure"
    } else {
      type <- paste(class(x), collapse = "/")
    }
    return(sprintf("a <%s> object", type))
  }

  if (!is_vector(x)) {
    return(.rlang_as_friendly_type(typeof(x)))
  }

  n_dim <- length(dim(x))

  if (!n_dim) {
    if (!is_list(x) && length(x) == 1) {
      if (is_na(x)) {
        return(switch(
          typeof(x),
          logical = "`NA`",
          integer = "an integer `NA`",
          double = if (is.nan(x)) {
            "`NaN`"
          } else {
            "a numeric `NA`"
          },
          complex = "a complex `NA`",
          character = "a character `NA`",
          .rlang_stop_unexpected_typeof(x)
        ))
      }

      show_infinites <- function(x) {
        if (x > 0) {
          "`Inf`"
        } else {
          "`-Inf`"
        }
      }
      str_encode <- function(x, width = 30, ...) {
        if (nchar(x) > width) {
          x <- substr(x, 1, width - 3)
          x <- paste0(x, "...")
        }
        encodeString(x, ...)
      }

      if (value) {
        if (is.numeric(x) && is.infinite(x)) {
          return(show_infinites(x))
        }

        if (is.numeric(x) || is.complex(x)) {
          number <- as.character(round(x, 2))
          what <- if (is.complex(x)) "the complex number" else "the number"
          return(paste(what, number))
        }

        return(switch(
          typeof(x),
          logical = if (x) "`TRUE`" else "`FALSE`",
          character = {
            what <- if (nzchar(x)) "the string" else "the empty string"
            paste(what, str_encode(x, quote = "\""))
          },
          raw = paste("the raw value", as.character(x)),
          .rlang_stop_unexpected_typeof(x)
        ))
      }

      return(switch(
        typeof(x),
        logical = "a logical value",
        integer = "an integer",
        double = if (is.infinite(x)) show_infinites(x) else "a number",
        complex = "a complex number",
        character = if (nzchar(x)) "a string" else "\"\"",
        raw = "a raw value",
        .rlang_stop_unexpected_typeof(x)
      ))
    }

    if (length(x) == 0) {
      return(switch(
        typeof(x),
        logical = "an empty logical vector",
        integer = "an empty integer vector",
        double = "an empty numeric vector",
        complex = "an empty complex vector",
        character = "an empty character vector",
        raw = "an empty raw vector",
        list = "an empty list",
        .rlang_stop_unexpected_typeof(x)
      ))
    }
  }

  vec_type_friendly(x)
}

vec_type_friendly <- function(x, length = FALSE) {
  if (!is_vector(x)) {
    abort("`x` must be a vector.")
  }
  type <- typeof(x)
  n_dim <- length(dim(x))

  add_length <- function(type) {
    if (length && !n_dim) {
      paste0(type, sprintf(" of length %s", length(x)))
    } else {
      type
    }
  }

  if (type == "list") {
    if (n_dim < 2) {
      return(add_length("a list"))
    } else if (is.data.frame(x)) {
      return("a data frame")
    } else if (n_dim == 2) {
      return("a list matrix")
    } else {
      return("a list array")
    }
  }

  type <- switch(
    type,
    logical = "a logical %s",
    integer = "an integer %s",
    numeric = ,
    double = "a double %s",
    complex = "a complex %s",
    character = "a character %s",
    raw = "a raw %s",
    type = paste0("a ", type, " %s")
  )

  if (n_dim < 2) {
    kind <- "vector"
  } else if (n_dim == 2) {
    kind <- "matrix"
  } else {
    kind <- "array"
  }
  out <- sprintf(type, kind)

  if (n_dim >= 2) {
    out
  } else {
    add_length(out)
  }
}

.rlang_as_friendly_type <- function(type) {
  switch(
    type,

    list = "a list",

    NULL = "`NULL`",
    environment = "an environment",
    externalptr = "a pointer",
    weakref = "a weak reference",
    S4 = "an S4 object",

    name = ,
    symbol = "a symbol",
    language = "a call",
    pairlist = "a pairlist node",
    expression = "an expression vector",

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

.rlang_stop_unexpected_typeof <- function(x, call = caller_env()) {
  abort(
    sprintf("Unexpected type <%s>.", typeof(x)),
    call = call
  )
}

#' Return OO type
#' @param x Any R object.
#' @return One of `"bare"` (for non-OO objects), `"S3"`, `"S4"`,
#'   `"R6"`, or `"R7"`.
#' @noRd
obj_type_oo <- function(x) {
  if (!is.object(x)) {
    return("bare")
  }

  class <- inherits(x, c("R6", "R7_object"), which = TRUE)

  if (class[[1]]) {
    "R6"
  } else if (class[[2]]) {
    "R7"
  } else if (isS4(x)) {
    "S4"
  } else {
    "S3"
  }
}

#' @param x The object type which does not conform to `what`. Its
#'   `obj_type_friendly()` is taken and mentioned in the error message.
#' @param what The friendly expected type as a string. Can be a
#'   character vector of expected types, in which case the error
#'   message mentions all of them in an "or" enumeration.
#' @param show_value Passed to `value` argument of `obj_type_friendly()`.
#' @param ... Arguments passed to [abort()].
#' @inheritParams args_error_context
#' @noRd
stop_input_type <- function(
  x,
  what,
  ...,
  allow_na = FALSE,
  allow_null = FALSE,
  show_value = TRUE,
  arg = caller_arg(x),
  call = caller_env()
) {
  # From compat-cli.R
  cli <- env_get_list(
    nms = c("format_arg", "format_code"),
    last = topenv(),
    default = function(x) sprintf("`%s`", x),
    inherit = TRUE
  )

  if (allow_na) {
    what <- c(what, cli$format_code("NA"))
  }
  if (allow_null) {
    what <- c(what, cli$format_code("NULL"))
  }
  if (length(what)) {
    what <- oxford_comma(what)
  }

  message <- sprintf(
    "%s must be %s, not %s.",
    cli$format_arg(arg),
    what,
    obj_type_friendly(x, value = show_value)
  )

  abort(message, ..., call = call, arg = arg)
}

oxford_comma <- function(chr, sep = ", ", final = "or") {
  n <- length(chr)

  if (n < 2) {
    return(chr)
  }

  head <- chr[seq_len(n - 1)]
  last <- chr[n]

  head <- paste(head, collapse = sep)

  # Write a or b. But a, b, or c.
  if (n > 2) {
    paste0(head, sep, final, " ", last)
  } else {
    paste0(head, " ", final, " ", last)
  }
}

# nocov end


# --- FILE: https://raw.githubusercontent.com/tidyverse/stringr/main/R/compat-purrr.R ---
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
  map2(.x, vec_index(.x), .f, ...)
}
vec_index <- function(x) {
  names(x) %||% seq_along(x)
}

# nocov end


# --- FILE: https://raw.githubusercontent.com/tidyverse/stringr/main/R/compat-types-check.R ---
# nocov start --- r-lib/rlang compat-types-check
#
# Dependencies
# ============
#
# - compat-obj-type.R
#
# Changelog
# =========
#
# 2022-10-04:
# - Added `check_name()` that forbids the empty string.
#   `check_string()` allows the empty string by default.
#
# 2022-09-28:
# - Removed `what` arguments.
# - Added `allow_na` and `allow_null` arguments.
# - Added `allow_decimal` and `allow_infinite` arguments.
# - Improved errors with absent arguments.
#
#
# 2022-09-16:
# - Unprefixed usage of rlang functions with `rlang::` to
#   avoid onLoad issues when called from rlang (#1482).
#
# 2022-08-11:
# - Added changelog.

# Scalars -----------------------------------------------------------------

check_bool <- function(
  x,
  ...,
  allow_na = FALSE,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  if (!missing(x)) {
    if (is_bool(x)) {
      return(invisible(NULL))
    }
    if (allow_null && is_null(x)) {
      return(invisible(NULL))
    }
    if (allow_na && identical(x, NA)) {
      return(invisible(NULL))
    }
  }

  stop_input_type(
    x,
    c("`TRUE`", "`FALSE`"),
    ...,
    allow_na = allow_na,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

check_string <- function(
  x,
  ...,
  allow_empty = TRUE,
  allow_na = FALSE,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  if (!missing(x)) {
    is_string <- .rlang_check_is_string(
      x,
      allow_empty = allow_empty,
      allow_na = allow_na,
      allow_null = allow_null
    )
    if (is_string) {
      return(invisible(NULL))
    }
  }

  stop_input_type(
    x,
    "a single string",
    ...,
    allow_na = allow_na,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

.rlang_check_is_string <- function(x, allow_empty, allow_na, allow_null) {
  if (is_string(x)) {
    if (allow_empty || !is_string(x, "")) {
      return(TRUE)
    }
  }

  if (allow_null && is_null(x)) {
    return(TRUE)
  }

  if (allow_na && (identical(x, NA) || identical(x, na_chr))) {
    return(TRUE)
  }

  FALSE
}

check_name <- function(
  x,
  ...,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  if (!missing(x)) {
    is_string <- .rlang_check_is_string(
      x,
      allow_empty = FALSE,
      allow_na = FALSE,
      allow_null = allow_null
    )
    if (is_string) {
      return(invisible(NULL))
    }
  }

  stop_input_type(
    x,
    "a valid name",
    ...,
    allow_na = FALSE,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

check_number_decimal <- function(
  x,
  ...,
  min = -Inf,
  max = Inf,
  allow_infinite = TRUE,
  allow_na = FALSE,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  .rlang_types_check_number(
    x,
    ...,
    min = min,
    max = max,
    allow_decimal = TRUE,
    allow_infinite = allow_infinite,
    allow_na = allow_na,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

check_number_whole <- function(
  x,
  ...,
  min = -Inf,
  max = Inf,
  allow_na = FALSE,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  .rlang_types_check_number(
    x,
    ...,
    min = min,
    max = max,
    allow_decimal = FALSE,
    allow_infinite = FALSE,
    allow_na = allow_na,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

.rlang_types_check_number <- function(
  x,
  ...,
  min = -Inf,
  max = Inf,
  allow_decimal = FALSE,
  allow_infinite = FALSE,
  allow_na = FALSE,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  if (allow_decimal) {
    what <- "a number"
  } else {
    what <- "a whole number"
  }

  .stop <- function(x, what, ...) {
    stop_input_type(
      x,
      what,
      ...,
      allow_na = allow_na,
      allow_null = allow_null,
      arg = arg,
      call = call
    )
  }

  if (!missing(x)) {
    is_number <- is_number(
      x,
      allow_decimal = allow_decimal,
      allow_infinite = allow_infinite
    )

    if (is_number) {
      if (min > -Inf && max < Inf) {
        what <- sprintf("a number between %s and %s", min, max)
      } else {
        what <- NULL
      }
      if (x < min) {
        what <- what %||% sprintf("a number larger than %s", min)
        .stop(x, what, ...)
      }
      if (x > max) {
        what <- what %||% sprintf("a number smaller than %s", max)
        .stop(x, what, ...)
      }
      return(invisible(NULL))
    }

    if (allow_null && is_null(x)) {
      return(invisible(NULL))
    }
    if (
      allow_na &&
        (identical(x, NA) ||
          identical(x, na_dbl) ||
          identical(x, na_int))
    ) {
      return(invisible(NULL))
    }
  }

  .stop(x, what, ...)
}

is_number <- function(x, allow_decimal = FALSE, allow_infinite = FALSE) {
  if (!typeof(x) %in% c("integer", "double")) {
    return(FALSE)
  }
  if (length(x) != 1) {
    return(FALSE)
  }
  if (is.na(x)) {
    return(FALSE)
  }
  if (!allow_decimal && !is_integerish(x)) {
    return(FALSE)
  }
  if (!allow_infinite && is.infinite(x)) {
    return(FALSE)
  }
  TRUE
}

check_symbol <- function(
  x,
  ...,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  if (!missing(x)) {
    if (is_symbol(x)) {
      return(invisible(NULL))
    }
    if (allow_null && is_null(x)) {
      return(invisible(NULL))
    }
  }

  stop_input_type(
    x,
    "a symbol",
    ...,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

check_arg <- function(
  x,
  ...,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  if (!missing(x)) {
    if (is_symbol(x)) {
      return(invisible(NULL))
    }
    if (allow_null && is_null(x)) {
      return(invisible(NULL))
    }
  }

  stop_input_type(
    x,
    "an argument name",
    ...,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

check_call <- function(
  x,
  ...,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  if (!missing(x)) {
    if (is_call(x)) {
      return(invisible(NULL))
    }
    if (allow_null && is_null(x)) {
      return(invisible(NULL))
    }
  }

  stop_input_type(
    x,
    "a defused call",
    ...,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

check_environment <- function(
  x,
  ...,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  if (!missing(x)) {
    if (is_environment(x)) {
      return(invisible(NULL))
    }
    if (allow_null && is_null(x)) {
      return(invisible(NULL))
    }
  }

  stop_input_type(
    x,
    "an environment",
    ...,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

check_function <- function(
  x,
  ...,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  if (!missing(x)) {
    if (is_function(x)) {
      return(invisible(NULL))
    }
    if (allow_null && is_null(x)) {
      return(invisible(NULL))
    }
  }

  stop_input_type(
    x,
    "a function",
    ...,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

check_closure <- function(
  x,
  ...,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  if (!missing(x)) {
    if (is_closure(x)) {
      return(invisible(NULL))
    }
    if (allow_null && is_null(x)) {
      return(invisible(NULL))
    }
  }

  stop_input_type(
    x,
    "an R function",
    ...,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

check_formula <- function(
  x,
  ...,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  if (!missing(x)) {
    if (is_formula(x)) {
      return(invisible(NULL))
    }
    if (allow_null && is_null(x)) {
      return(invisible(NULL))
    }
  }

  stop_input_type(
    x,
    "a formula",
    ...,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}


# Vectors -----------------------------------------------------------------

check_character <- function(
  x,
  ...,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  if (!missing(x)) {
    if (is_character(x)) {
      return(invisible(NULL))
    }
    if (allow_null && is_null(x)) {
      return(invisible(NULL))
    }
  }

  stop_input_type(
    x,
    "a character vector",
    ...,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

# nocov end


# --- FILE: https://raw.githubusercontent.com/tidyverse/stringr/main/R/conv.R ---
#' Specify the encoding of a string
#'
#' This is a convenient way to override the current encoding of a string.
#'
#' @inheritParams str_detect
#' @param encoding Name of encoding. See [stringi::stri_enc_list()]
#'   for a complete list.
#' @export
#' @examples
#' # Example from encoding?stringi::stringi
#' x <- rawToChar(as.raw(177))
#' x
#' str_conv(x, "ISO-8859-2") # Polish "a with ogonek"
#' str_conv(x, "ISO-8859-1") # Plus-minus
str_conv <- function(string, encoding) {
  check_string(encoding)

  copy_names(string, stri_conv(string, encoding, "UTF-8"))
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/stringr/main/R/count.R ---
#' Count number of matches
#'
#' Counts the number of times `pattern` is found within each element
#' of `string.`
#'
#' @inheritParams str_detect
#' @param pattern Pattern to look for.
#'
#'   The default interpretation is a regular expression, as described in
#'   `vignette("regular-expressions")`. Use [regex()] for finer control of the
#'   matching behaviour.
#'
#'   Match a fixed string (i.e. by comparing only bytes), using
#'   [fixed()]. This is fast, but approximate. Generally,
#'   for matching human text, you'll want [coll()] which
#'   respects character matching rules for the specified locale.
#'
#'   Match character, word, line and sentence boundaries with
#'   [boundary()]. The empty string, `""``, is equivalent to
#'   `boundary("character")`.
#' @return An integer vector the same length as `string`/`pattern`.
#' @seealso [stringi::stri_count()] which this function wraps.
#'
#'  [str_locate()]/[str_locate_all()] to locate position
#'  of matches
#'
#' @export
#' @examples
#' fruit <- c("apple", "banana", "pear", "pineapple")
#' str_count(fruit, "a")
#' str_count(fruit, "p")
#' str_count(fruit, "e")
#' str_count(fruit, c("a", "b", "p", "p"))
#'
#' str_count(c("a.", "...", ".a.a"), ".")
#' str_count(c("a.", "...", ".a.a"), fixed("."))
str_count <- function(string, pattern = "") {
  check_lengths(string, pattern)

  out <- switch(
    type(pattern),
    empty = ,
    bound = stri_count_boundaries(string, opts_brkiter = opts(pattern)),
    fixed = stri_count_fixed(string, pattern, opts_fixed = opts(pattern)),
    coll = stri_count_coll(string, pattern, opts_collator = opts(pattern)),
    regex = stri_count_regex(string, pattern, opts_regex = opts(pattern))
  )
  preserve_names_if_possible(string, pattern, out)
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/stringr/main/R/data.R ---
#' Sample character vectors for practicing string manipulations
#'
#' `fruit` and `words` come from the `rcorpora` package
#' written by Gabor Csardi; the data was collected by Darius Kazemi
#' and made available at \url{https://github.com/dariusk/corpora}.
#' `sentences` is a collection of "Harvard sentences" used for
#' standardised testing of voice.
#'
#' @format Character vectors.
#' @name stringr-data
#' @examples
#' length(sentences)
#' sentences[1:5]
#'
#' length(fruit)
#' fruit[1:5]
#'
#' length(words)
#' words[1:5]
NULL

#' @rdname stringr-data
#' @format NULL
"sentences"

#' @rdname stringr-data
#' @format NULL
"fruit"

#' @rdname stringr-data
#' @format NULL
"words"


# --- FILE: https://raw.githubusercontent.com/tidyverse/stringr/main/R/detect.R ---
#' Detect the presence/absence of a match
#'
#' `str_detect()` returns a logical vector with `TRUE` for each element of
#' `string` that matches `pattern` and `FALSE` otherwise. It's equivalent to
#' `grepl(pattern, string)`.
#'
#' @param string Input vector. Either a character vector, or something
#'  coercible to one.
#' @param pattern Pattern to look for.
#'
#'   The default interpretation is a regular expression, as described in
#'   `vignette("regular-expressions")`. Use [regex()] for finer control of the
#'   matching behaviour.
#'
#'   Match a fixed string (i.e. by comparing only bytes), using
#'   [fixed()]. This is fast, but approximate. Generally,
#'   for matching human text, you'll want [coll()] which
#'   respects character matching rules for the specified locale.
#'
#'   You can not match boundaries, including `""`, with this function.
#'
#' @param negate If `TRUE`, inverts the resulting boolean vector.
#' @return A logical vector the same length as `string`/`pattern`.
#' @seealso [stringi::stri_detect()] which this function wraps,
#'   [str_subset()] for a convenient wrapper around
#'   `x[str_detect(x, pattern)]`
#' @export
#' @examples
#' fruit <- c("apple", "banana", "pear", "pineapple")
#' str_detect(fruit, "a")
#' str_detect(fruit, "^a")
#' str_detect(fruit, "a$")
#' str_detect(fruit, "b")
#' str_detect(fruit, "[aeiou]")
#'
#' # Also vectorised over pattern
#' str_detect("aecfg", letters)
#'
#' # Returns TRUE if the pattern do NOT match
#' str_detect(fruit, "^p", negate = TRUE)
str_detect <- function(string, pattern, negate = FALSE) {
  check_lengths(string, pattern)
  check_bool(negate)

  out <- switch(
    type(pattern),
    empty = no_empty(),
    bound = no_boundary(),
    fixed = stri_detect_fixed(
      string,
      pattern,
      negate = negate,
      opts_fixed = opts(pattern)
    ),
    coll = stri_detect_coll(
      string,
      pattern,
      negate = negate,
      opts_collator = opts(pattern)
    ),
    regex = stri_detect_regex(
      string,
      pattern,
      negate = negate,
      opts_regex = opts(pattern)
    )
  )

  preserve_names_if_possible(string, pattern, out)
}

#' Detect the presence/absence of a match at the start/end
#'
#' `str_starts()` and `str_ends()` are special cases of [str_detect()] that
#' only match at the beginning or end of a string, respectively.
#'
#' @inheritParams str_detect
#' @param pattern Pattern with which the string starts or ends.
#'
#'   The default interpretation is a regular expression, as described in
#'   [stringi::about_search_regex]. Control options with [regex()].
#'
#'   Match a fixed string (i.e. by comparing only bytes), using [fixed()]. This
#'   is fast, but approximate. Generally, for matching human text, you'll want
#'   [coll()] which respects character matching rules for the specified locale.
#'
#' @return A logical vector.
#' @export
#' @examples
#' fruit <- c("apple", "banana", "pear", "pineapple")
#' str_starts(fruit, "p")
#' str_starts(fruit, "p", negate = TRUE)
#' str_ends(fruit, "e")
#' str_ends(fruit, "e", negate = TRUE)
str_starts <- function(string, pattern, negate = FALSE) {
  check_lengths(string, pattern)
  check_bool(negate)

  out <- switch(
    type(pattern),
    empty = no_empty(),
    bound = no_boundary(),
    fixed = stri_startswith_fixed(
      string,
      pattern,
      negate = negate,
      opts_fixed = opts(pattern)
    ),
    coll = stri_startswith_coll(
      string,
      pattern,
      negate = negate,
      opts_collator = opts(pattern)
    ),
    regex = {
      pattern2 <- paste0("^(", pattern, ")")
      stri_detect_regex(
        string,
        pattern2,
        negate = negate,
        opts_regex = opts(pattern)
      )
    }
  )
  preserve_names_if_possible(string, pattern, out)
}

#' @rdname str_starts
#' @export
str_ends <- function(string, pattern, negate = FALSE) {
  check_lengths(string, pattern)
  check_bool(negate)

  out <- switch(
    type(pattern),
    empty = no_empty(),
    bound = no_boundary(),
    fixed = stri_endswith_fixed(
      string,
      pattern,
      negate = negate,
      opts_fixed = opts(pattern)
    ),
    coll = stri_endswith_coll(
      string,
      pattern,
      negate = negate,
      opts_collator = opts(pattern)
    ),
    regex = {
      pattern2 <- paste0("(", pattern, ")$")
      stri_detect_regex(
        string,
        pattern2,
        negate = negate,
        opts_regex = opts(pattern)
      )
    }
  )
  preserve_names_if_possible(string, pattern, out)
}

#' Detect a pattern in the same way as `SQL`'s `LIKE` and `ILIKE` operators
#'
#' @description
#' `str_like()` and `str_like()` follow the conventions of the SQL `LIKE`
#' and `ILIKE` operators, namely:
#'
#' * Must match the entire string.
#' * `_` matches a single character (like `.`).
#' * `%` matches any number of characters (like `.*`).
#' * `\%` and `\_` match literal `%` and `_`.
#'
#' The difference between the two functions is their case-sensitivity:
#' `str_like()` is case sensitive and `str_ilike()` is not.
#'
#' @note
#' Prior to stringr 1.6.0, `str_like()` was incorrectly case-insensitive.
#'
#' @inheritParams str_detect
#' @param pattern A character vector containing a SQL "like" pattern.
#'   See above for details.
#' @param ignore_case `r lifecycle::badge("deprecated")`
#' @return A logical vector the same length as `string`.
#' @export
#' @examples
#' fruit <- c("apple", "banana", "pear", "pineapple")
#' str_like(fruit, "app")
#' str_like(fruit, "app%")
#' str_like(fruit, "APP%")
#' str_like(fruit, "ba_ana")
#' str_like(fruit, "%apple")
#'
#' str_ilike(fruit, "app")
#' str_ilike(fruit, "app%")
#' str_ilike(fruit, "APP%")
#' str_ilike(fruit, "ba_ana")
#' str_ilike(fruit, "%apple")
str_like <- function(string, pattern, ignore_case = deprecated()) {
  check_lengths(string, pattern)
  check_character(pattern)
  if (inherits(pattern, "stringr_pattern")) {
    cli::cli_abort(
      "{.arg pattern} must be a plain string, not a stringr modifier."
    )
  }
  if (lifecycle::is_present(ignore_case)) {
    lifecycle::deprecate_warn(
      when = "1.6.0",
      what = "str_like(ignore_case)",
      details = c(
        "`str_like()` is always case sensitive.",
        "Use `str_ilike()` for case insensitive string matching."
      )
    )
    check_bool(ignore_case)
    if (ignore_case) {
      return(str_ilike(string, pattern))
    }
  }

  pattern <- regex(like_to_regex(pattern), ignore_case = FALSE)
  out <- stri_detect_regex(string, pattern, opts_regex = opts(pattern))
  preserve_names_if_possible(string, pattern, out)
}

#' @export
#' @rdname str_like
str_ilike <- function(string, pattern) {
  check_lengths(string, pattern)
  check_character(pattern)
  if (inherits(pattern, "stringr_pattern")) {
    cli::cli_abort(tr_(
      "{.arg pattern} must be a plain string, not a stringr modifier."
    ))
  }

  pattern <- regex(like_to_regex(pattern), ignore_case = TRUE)
  out <- stri_detect_regex(string, pattern, opts_regex = opts(pattern))
  preserve_names_if_possible(string, pattern, out)
}

like_to_regex <- function(pattern) {
  converted <- stri_replace_all_regex(
    pattern,
    "(?<!\\\\|\\[)%(?!\\])",
    "\\.\\*"
  )
  converted <- stri_replace_all_regex(converted, "(?<!\\\\|\\[)_(?!\\])", "\\.")
  paste0("^", converted, "$")
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/stringr/main/R/dup.R ---
#' Duplicate a string
#'
#' `str_dup()` duplicates the characters within a string, e.g.
#' `str_dup("xy", 3)` returns `"xyxyxy"`.
#'
#' @inheritParams str_detect
#' @param times Number of times to duplicate each string.
#' @param sep String to insert between each duplicate.
#' @return A character vector the same length as `string`/`times`.
#' @export
#' @examples
#' fruit <- c("apple", "pear", "banana")
#' str_dup(fruit, 2)
#' str_dup(fruit, 2, sep = " ")
#' str_dup(fruit, 1:3)
#' str_c("ba", str_dup("na", 0:5))
str_dup <- function(string, times, sep = NULL) {
  input <- vctrs::vec_recycle_common(string = string, times = times)
  check_string(sep, allow_null = TRUE)

  if (is.null(sep)) {
    out <- stri_dup(input$string, input$times)
  } else {
    out <- map_chr(seq_along(input$string), function(i) {
      paste(rep(string[[i]], input$times[[i]]), collapse = sep)
    })
  }
  names(out) <- names(input$string)
  out
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/stringr/main/R/equal.R ---
#' Determine if two strings are equivalent
#'
#' This uses Unicode canonicalisation rules, and optionally ignores case.
#'
#' @param x,y A pair of character vectors.
#' @inheritParams str_order
#' @param ignore_case Ignore case when comparing strings?
#' @return An logical vector the same length as `x`/`y`.
#' @seealso [stringi::stri_cmp_equiv()] for the underlying implementation.
#' @export
#' @examples
#' # These two strings encode "a" with an accent in two different ways
#' a1 <- "\u00e1"
#' a2 <- "a\u0301"
#' c(a1, a2)
#'
#' a1 == a2
#' str_equal(a1, a2)
#'
#' # ohm and omega use different code points but should always be treated
#' # as equal
#' ohm <- "\u2126"
#' omega <- "\u03A9"
#' c(ohm, omega)
#'
#' ohm == omega
#' str_equal(ohm, omega)
str_equal <- function(x, y, locale = "en", ignore_case = FALSE, ...) {
  vctrs::vec_size_common(x = x, y = y)
  check_string(locale)
  check_bool(ignore_case)

  opts <- str_opts_collator(
    locale = locale,
    ignore_case = ignore_case,
    ...
  )
  stri_cmp_equiv(x, y, opts_collator = opts)
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/stringr/main/R/escape.R ---
#' Escape regular expression metacharacters
#'
#' This function escapes metacharacter, the characters that have special
#' meaning to the regular expression engine. In most cases you are better
#' off using [fixed()] since it is faster, but `str_escape()` is useful
#' if you are composing user provided strings into a pattern.
#'
#' @inheritParams str_detect
#' @return A character vector the same length as `string`.
#' @export
#' @examples
#' str_detect(c("a", "."), ".")
#' str_detect(c("a", "."), str_escape("."))
str_escape <- function(string) {
  out <- str_replace_all(string, "([.^$\\\\|*+?{}\\[\\]()])", "\\\\\\1")
  copy_names(string, out)
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/stringr/main/R/extract.R ---
#' Extract the complete match
#'
#' `str_extract()` extracts the first complete match from each string,
#' `str_extract_all()`extracts all matches from each string.
#'
#' @inheritParams str_count
#' @param group If supplied, instead of returning the complete match, will
#'   return the matched text from the specified capturing group.
#' @seealso [str_match()] to extract matched groups;
#'   [stringi::stri_extract()] for the underlying implementation.
#' @param simplify A boolean.
#'   * `FALSE` (the default): returns a list of character vectors.
#'   * `TRUE`: returns a character matrix.
#' @return
#' * `str_extract()`: an character vector the same length as `string`/`pattern`.
#' * `str_extract_all()`: a list of character vectors the same length as
#'   `string`/`pattern`.
#' @export
#' @examples
#' shopping_list <- c("apples x4", "bag of flour", "bag of sugar", "milk x2")
#' str_extract(shopping_list, "\\d")
#' str_extract(shopping_list, "[a-z]+")
#' str_extract(shopping_list, "[a-z]{1,4}")
#' str_extract(shopping_list, "\\b[a-z]{1,4}\\b")
#'
#' str_extract(shopping_list, "([a-z]+) of ([a-z]+)")
#' str_extract(shopping_list, "([a-z]+) of ([a-z]+)", group = 1)
#' str_extract(shopping_list, "([a-z]+) of ([a-z]+)", group = 2)
#'
#' # Extract all matches
#' str_extract_all(shopping_list, "[a-z]+")
#' str_extract_all(shopping_list, "\\b[a-z]+\\b")
#' str_extract_all(shopping_list, "\\d")
#'
#' # Simplify results into character matrix
#' str_extract_all(shopping_list, "\\b[a-z]+\\b", simplify = TRUE)
#' str_extract_all(shopping_list, "\\d", simplify = TRUE)
#'
#' # Extract all words
#' str_extract_all("This is, suprisingly, a sentence.", boundary("word"))
str_extract <- function(string, pattern, group = NULL) {
  if (!is.null(group)) {
    out <- str_match(string, pattern)[, group + 1]
    return(preserve_names_if_possible(string, pattern, out))
  }

  check_lengths(string, pattern)
  opt <- opts(pattern)
  out <- switch(
    type(pattern),
    empty = stri_extract_first_boundaries(string, opts_brkiter = opt),
    bound = stri_extract_first_boundaries(string, opts_brkiter = opt),
    fixed = stri_extract_first_fixed(string, pattern, opts_fixed = opt),
    coll = stri_extract_first_coll(string, pattern, opts_collator = opt),
    regex = stri_extract_first_regex(string, pattern, opts_regex = opt)
  )
  preserve_names_if_possible(string, pattern, out)
}

#' @rdname str_extract
#' @export
str_extract_all <- function(string, pattern, simplify = FALSE) {
  check_lengths(string, pattern)
  check_bool(simplify)

  opt <- opts(pattern)
  out <- switch(
    type(pattern),
    empty = stri_extract_all_boundaries(
      string,
      simplify = simplify,
      omit_no_match = TRUE,
      opts_brkiter = opt
    ),
    bound = stri_extract_all_boundaries(
      string,
      simplify = simplify,
      omit_no_match = TRUE,
      opts_brkiter = opt
    ),
    fixed = stri_extract_all_fixed(
      string,
      pattern,
      simplify = simplify,
      omit_no_match = TRUE,
      opts_fixed = opt
    ),
    coll = stri_extract_all_coll(
      string,
      pattern,
      simplify = simplify,
      omit_no_match = TRUE,
      opts_collator = opt
    ),
    regex = stri_extract_all_regex(
      string,
      pattern,
      simplify = simplify,
      omit_no_match = TRUE,
      opts_regex = opt
    )
  )
  preserve_names_if_possible(string, pattern, out)
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/stringr/main/R/flatten.R ---
#' Flatten a string
#
#' @description
#' `str_flatten()` reduces a character vector to a single string. This is a
#' summary function because regardless of the length of the input `x`, it
#' always returns a single string.
#'
#' `str_flatten_comma()` is a variation designed specifically for flattening
#' with commas. It automatically recognises if `last` uses the Oxford comma
#' and handles the special case of 2 elements.
#'
#' @inheritParams str_detect
#' @param collapse String to insert between each piece. Defaults to `""`.
#' @param last Optional string to use in place of the final separator.
#' @param na.rm Remove missing values? If `FALSE` (the default), the result
#'   will be `NA` if any element of `string` is `NA`.
#' @return A string, i.e. a character vector of length 1.
#' @export
#' @examples
#' str_flatten(letters)
#' str_flatten(letters, "-")
#'
#' str_flatten(letters[1:3], ", ")
#'
#' # Use last to customise the last component
#' str_flatten(letters[1:3], ", ", " and ")
#'
#' # this almost works if you want an Oxford (aka serial) comma
#' str_flatten(letters[1:3], ", ", ", and ")
#'
#' # but it will always add a comma, even when not necessary
#' str_flatten(letters[1:2], ", ", ", and ")
#'
#' # str_flatten_comma knows how to handle the Oxford comma
#' str_flatten_comma(letters[1:3], ", and ")
#' str_flatten_comma(letters[1:2], ", and ")
str_flatten <- function(string, collapse = "", last = NULL, na.rm = FALSE) {
  check_string(collapse)
  check_string(last, allow_null = TRUE)
  check_bool(na.rm)

  if (na.rm) {
    string <- string[!is.na(string)]
  }

  n <- length(string)
  if (!is.null(last) && n >= 2) {
    string <- c(
      string[seq2(1, n - 2)],
      stringi::stri_c(string[[n - 1]], last, string[[n]])
    )
  }

  stri_flatten(string, collapse = collapse)
}

#' @export
#' @rdname str_flatten
str_flatten_comma <- function(string, last = NULL, na.rm = FALSE) {
  check_string(last, allow_null = TRUE)
  check_bool(na.rm)

  # Remove comma if exactly two elements, and last uses Oxford comma
  if (length(string) == 2 && !is.null(last) && str_detect(last, "^,")) {
    last <- str_replace(last, "^,", "")
  }
  str_flatten(string, ", ", last = last, na.rm = na.rm)
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/stringr/main/R/glue.R ---
#' Interpolation with glue
#'
#' @description
#' These functions are wrappers around [glue::glue()] and [glue::glue_data()],
#' which provide a powerful and elegant syntax for interpolating strings
#' with `{}`.
#'
#' These wrappers provide a small set of the full options. Use `glue()` and
#' `glue_data()` directly from glue for more control.
#'
#' @inheritParams glue::glue
#' @return A character vector with same length as the longest input.
#' @export
#' @examples
#' name <- "Fred"
#' age <- 50
#' anniversary <- as.Date("1991-10-12")
#' str_glue(
#'   "My name is {name}, ",
#'   "my age next year is {age + 1}, ",
#'   "and my anniversary is {format(anniversary, '%A, %B %d, %Y')}."
#' )
#'
#' # single braces can be inserted by doubling them
#' str_glue("My name is {name}, not {{name}}.")
#'
#' # You can also used named arguments
#' str_glue(
#'   "My name is {name}, ",
#'   "and my age next year is {age + 1}.",
#'   name = "Joe",
#'   age = 40
#' )
#'
#' # `str_glue_data()` is useful in data pipelines
#' mtcars %>% str_glue_data("{rownames(.)} has {hp} hp")
str_glue <- function(..., .sep = "", .envir = parent.frame(), .trim = TRUE) {
  glue::glue(..., .sep = .sep, .envir = .envir, .trim = .trim)
}

#' @export
#' @rdname str_glue
str_glue_data <- function(
  .x,
  ...,
  .sep = "",
  .envir = parent.frame(),
  .na = "NA"
) {
  glue::glue_data(
    .x,
    ...,
    .sep = .sep,
    .envir = .envir,
    .na = .na
  )
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/stringr/main/R/interp.R ---
#' String interpolation
#'
#' @description
#' `r lifecycle::badge("superseded")`
#'
#' `str_interp()` is superseded in favour of [str_glue()].
#'
#' String interpolation is a useful way of specifying a character string which
#' depends on values in a certain environment. It allows for string creation
#' which is easier to read and write when compared to using e.g.
#' [paste()] or [sprintf()]. The (template) string can
#' include expression placeholders of the form `${expression}` or
#' `$[format]{expression}`, where expressions are valid R expressions that
#' can be evaluated in the given environment, and `format` is a format
#' specification valid for use with [sprintf()].
#'
#' @param string A template character string. This function is not vectorised:
#'   a character vector will be collapsed into a single string.
#' @param env The environment in which to evaluate the expressions.
#' @seealso [str_glue()] and [str_glue_data()] for alternative approaches to
#'   the same problem.
#' @keywords internal
#' @return An interpolated character string.
#' @author Stefan Milton Bache
#' @export
#' @examples
#'
#' # Using values from the environment, and some formats
#' user_name <- "smbache"
#' amount <- 6.656
#' account <- 1337
#' str_interp("User ${user_name} (account $[08d]{account}) has $$[.2f]{amount}.")
#'
#' # Nested brace pairs work inside expressions too, and any braces can be
#' # placed outside the expressions.
#' str_interp("Works with } nested { braces too: $[.2f]{{{2 + 2}*{amount}}}")
#'
#' # Values can also come from a list
#' str_interp(
#'   "One value, ${value1}, and then another, ${value2*2}.",
#'   list(value1 = 10, value2 = 20)
#' )
#'
#' # Or a data frame
#' str_interp(
#'   "Values are $[.2f]{max(Sepal.Width)} and $[.2f]{min(Sepal.Width)}.",
#'   iris
#' )
#'
#' # Use a vector when the string is long:
#' max_char <- 80
#' str_interp(c(
#'   "This particular line is so long that it is hard to write ",
#'   "without breaking the ${max_char}-char barrier!"
#' ))
str_interp <- function(string, env = parent.frame()) {
  check_character(string)
  string <- str_c(string, collapse = "")

  # Find expression placeholders
  matches <- interp_placeholders(string)

  # Determine if any placeholders were found.
  if (matches$indices[1] <= 0) {
    string
  } else {
    # Evaluate them to get the replacement strings.
    replacements <- eval_interp_matches(matches$matches, env)

    # Replace the expressions by their values and return.
    `regmatches<-`(string, list(matches$indices), FALSE, list(replacements))
  }
}

#' Match String Interpolation Placeholders
#'
#' Given a character string a set of expression placeholders are matched. They
#' are of the form \code{${...}} or optionally \code{$[f]{...}} where `f`
#' is a valid format for [sprintf()].
#'
#' @param string character: The string to be interpolated.
#'
#' @return list containing `indices` (regex match data) and `matches`,
#'   the string representations of matched expressions.
#'
#' @noRd
#' @author Stefan Milton Bache
interp_placeholders <- function(string, error_call = caller_env()) {
  # Find starting position of ${} or $[]{} placeholders.
  starts <- gregexpr("\\$(\\[.*?\\])?\\{", string)[[1]]

  # Return immediately if no matches are found.
  if (starts[1] <= 0) {
    return(list(indices = starts))
  }

  # Break up the string in parts
  parts <- substr(
    rep(string, length(starts)),
    start = starts,
    stop = c(starts[-1L] - 1L, nchar(string))
  )

  # If there are nested placeholders, each part will not contain a full
  # placeholder in which case we report invalid string interpolation template.
  if (any(!grepl("\\$(\\[.*?\\])?\\{.+\\}", parts))) {
    cli::cli_abort(
      tr_("Invalid template string for interpolation."),
      call = error_call
    )
  }

  # For each part, find the opening and closing braces.
  opens <- lapply(strsplit(parts, ""), function(v) which(v == "{"))
  closes <- lapply(strsplit(parts, ""), function(v) which(v == "}"))

  # Identify the positions within the parts of the matching closing braces.
  # These are the lengths of the placeholder matches.
  lengths <- mapply(match_brace, opens, closes)

  # Update the `starts` match data with the
  attr(starts, "match.length") <- lengths

  # Return both the indices (regex match data) and the actual placeholder
  # matches (as strings.)
  list(
    indices = starts,
    matches = mapply(substr, starts, starts + lengths - 1, x = string)
  )
}

#' Evaluate String Interpolation Matches
#'
#' The expression part of string interpolation matches are evaluated in a
#' specified environment and formatted for replacement in the original string.
#' Used internally by [str_interp()].
#'
#' @param matches Match data
#'
#' @param env The environment in which to evaluate the expressions.
#'
#' @return A character vector of replacement strings.
#'
#' @noRd
#' @author Stefan Milton Bache
eval_interp_matches <- function(matches, env, error_call = caller_env()) {
  # Extract expressions from the matches
  expressions <- extract_expressions(matches, error_call = error_call)

  # Evaluate them in the given environment
  values <- lapply(
    expressions,
    eval,
    envir = env,
    enclos = if (is.environment(env)) env else environment(env)
  )

  # Find the formats to be used
  formats <- extract_formats(matches)

  # Format the values and return.
  mapply(sprintf, formats, values, SIMPLIFY = FALSE)
}

#' Extract Expression Objects from String Interpolation Matches
#'
#' An interpolation match object will contain both its wrapping \code{${ }} part
#' and possibly a format. This extracts the expression parts and parses them to
#' prepare them for evaluation.
#'
#' @param matches Match data
#'
#' @return list of R expressions
#'
#' @noRd
#' @author Stefan Milton Bache
extract_expressions <- function(matches, error_call = caller_env()) {
  # Parse function for text argument as first argument.

  parse_text <- function(text) {
    withCallingHandlers(
      parse(text = text),
      error = function(e) {
        cli::cli_abort(
          tr_("Failed to parse input {.str {text}}"),
          parent = e,
          call = error_call
        )
      }
    )
  }

  # string representation of the expressions (without the possible formats).
  strings <- gsub("\\$(\\[.+?\\])?\\{", "", matches)

  # Remove the trailing closing brace and parse.
  lapply(substr(strings, 1L, nchar(strings) - 1), parse_text)
}


#' Extract String Interpolation Formats from Matched Placeholders
#'
#' An expression placeholder for string interpolation may optionally contain a
#' format valid for [sprintf()]. This function will extract such or
#' default to "s" the format for strings.
#'
#' @param matches Match data
#'
#' @return A character vector of format specifiers.
#'
#' @noRd
#' @author Stefan Milton Bache
extract_formats <- function(matches) {
  # Extract the optional format parts.
  formats <- gsub("\\$(\\[(.+?)\\])?.*", "\\2", matches)

  # Use string options "s" as default when not specified.
  paste0("%", ifelse(formats == "", "s", formats))
}

#' Utility Function for Matching a Closing Brace
#'
#' Given positions of opening and closing braces `match_brace` identifies
#' the closing brace matching the first opening brace.
#'
#' @param opening integer: Vector with positions of opening braces.
#'
#' @param closing integer: Vector with positions of closing braces.
#'
#' @return Integer with the posision of the matching brace.
#'
#' @noRd
#' @author Stefan Milton Bache
match_brace <- function(opening, closing) {
  # maximum index for the matching closing brace
  max_close <- max(closing)

  # "path" for mapping opening and closing breaces
  path <- numeric(max_close)

  # Set openings to 1, and closings to -1
  path[opening[opening < max_close]] <- 1
  path[closing] <- -1

  # Cumulate the path ...
  cumpath <- cumsum(path)

  # ... and the first 0 after the first opening identifies the match.
  min(which(1:max_close > min(which(cumpath == 1)) & cumpath == 0))
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/stringr/main/R/length.R ---
#' Compute the length/width
#'
#' @description
#' `str_length()` returns the number of codepoints in a string. These are
#' the individual elements (which are often, but not always letters) that
#' can be extracted with [str_sub()].
#'
#' `str_width()` returns how much space the string will occupy when printed
#' in a fixed width font (i.e. when printed in the console).
#'
#' @inheritParams str_detect
#' @return A numeric vector the same length as `string`.
#' @seealso [stringi::stri_length()] which this function wraps.
#' @export
#' @examples
#' str_length(letters)
#' str_length(NA)
#' str_length(factor("abc"))
#' str_length(c("i", "like", "programming", NA))
#'
#' # Some characters, like emoji and Chinese characters (hanzi), are square
#' # which means they take up the width of two Latin characters
#' x <- c("\u6c49\u5b57", "\U0001f60a")
#' str_view(x)
#' str_width(x)
#' str_length(x)
#'
#' # There are two ways of representing a u with an umlaut
#' u <- c("\u00fc", "u\u0308")
#' # They have the same width
#' str_width(u)
#' # But a different length
#' str_length(u)
#' # Because the second element is made up of a u + an accent
#' str_sub(u, 1, 1)
str_length <- function(string) {
  copy_names(string, stri_length(string))
}

#' @export
#' @rdname str_length
str_width <- function(string) {
  copy_names(string, stri_width(string))
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/stringr/main/R/locate.R ---
#' Find location of match
#'
#' @description
#' `str_locate()` returns the `start` and `end` position of the first match;
#' `str_locate_all()` returns the `start` and `end` position of each match.
#'
#' Because the `start` and `end` values are inclusive, zero-length matches
#' (e.g. `$`, `^`, `\\b`) will have an `end` that is smaller than `start`.
#'
#' @inheritParams str_count
#' @returns
#' * `str_locate()` returns an integer matrix with two columns and
#'   one row for each element of `string`. The first column, `start`,
#'   gives the position at the start of the match, and the second column, `end`,
#'   gives the position of the end.
#'
#'* `str_locate_all()` returns a list of integer matrices with the same
#'   length as `string`/`pattern`. The matrices have columns `start` and `end`
#'   as above, and one row for each match.
#' @seealso
#'   [str_extract()] for a convenient way of extracting matches,
#'   [stringi::stri_locate()] for the underlying implementation.
#' @export
#' @examples
#' fruit <- c("apple", "banana", "pear", "pineapple")
#' str_locate(fruit, "$")
#' str_locate(fruit, "a")
#' str_locate(fruit, "e")
#' str_locate(fruit, c("a", "b", "p", "p"))
#'
#' str_locate_all(fruit, "a")
#' str_locate_all(fruit, "e")
#' str_locate_all(fruit, c("a", "b", "p", "p"))
#'
#' # Find location of every character
#' str_locate_all(fruit, "")
str_locate <- function(string, pattern) {
  check_lengths(string, pattern)

  out <- switch(
    type(pattern),
    empty = ,
    bound = stri_locate_first_boundaries(string, opts_brkiter = opts(pattern)),
    fixed = stri_locate_first_fixed(
      string,
      pattern,
      opts_fixed = opts(pattern)
    ),
    coll = stri_locate_first_coll(
      string,
      pattern,
      opts_collator = opts(pattern)
    ),
    regex = stri_locate_first_regex(string, pattern, opts_regex = opts(pattern))
  )
  preserve_names_if_possible(string, pattern, out)
}

#' @rdname str_locate
#' @export
str_locate_all <- function(string, pattern) {
  check_lengths(string, pattern)
  opts <- opts(pattern)

  out <- switch(
    type(pattern),
    empty = ,
    bound = stri_locate_all_boundaries(
      string,
      omit_no_match = TRUE,
      opts_brkiter = opts
    ),
    fixed = stri_locate_all_fixed(
      string,
      pattern,
      omit_no_match = TRUE,
      opts_fixed = opts
    ),
    regex = stri_locate_all_regex(
      string,
      pattern,
      omit_no_match = TRUE,
      opts_regex = opts
    ),
    coll = stri_locate_all_coll(
      string,
      pattern,
      omit_no_match = TRUE,
      opts_collator = opts
    )
  )
  preserve_names_if_possible(string, pattern, out)
}


#' Switch location of matches to location of non-matches
#'
#' Invert a matrix of match locations to match the opposite of what was
#' previously matched.
#'
#' @param loc matrix of match locations, as from [str_locate_all()]
#' @return numeric match giving locations of non-matches
#' @export
#' @examples
#' numbers <- "1 and 2 and 4 and 456"
#' num_loc <- str_locate_all(numbers, "[0-9]+")[[1]]
#' str_sub(numbers, num_loc[, "start"], num_loc[, "end"])
#'
#' text_loc <- invert_match(num_loc)
#' str_sub(numbers, text_loc[, "start"], text_loc[, "end"])
invert_match <- function(loc) {
  cbind(
    start = c(0L, loc[, "end"] + 1L),
    end = c(loc[, "start"] - 1L, -1L)
  )
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/stringr/main/R/match.R ---
#' Extract components (capturing groups) from a match
#'
#' @description
#' Extract any number of matches defined by unnamed, `(pattern)`, and
#' named, `(?<name>pattern)` capture groups.
#'
#' Use a non-capturing group, `(?:pattern)`, if you need to override default
#' operate precedence but don't want to capture the result.
#'
#' @inheritParams str_detect
#' @param pattern Unlike other stringr functions, `str_match()` only supports
#'   regular expressions, as described `vignette("regular-expressions")`.
#'   The pattern should contain at least one capturing group.
#' @return
#' * `str_match()`: a character matrix with the same number of rows as the
#'   length of `string`/`pattern`. The first column is the complete match,
#'   followed by one column for each capture group. The columns will be named
#'   if you used "named captured groups", i.e. `(?<name>pattern')`.
#'
#' * `str_match_all()`: a list of the same length as `string`/`pattern`
#'   containing character matrices. Each matrix has columns as described above
#'   and one row for each match.
#'
#' @seealso [str_extract()] to extract the complete match,
#'   [stringi::stri_match()] for the underlying implementation.
#' @export
#' @examples
#' strings <- c(" 219 733 8965", "329-293-8753 ", "banana", "595 794 7569",
#'   "387 287 6718", "apple", "233.398.9187  ", "482 952 3315",
#'   "239 923 8115 and 842 566 4692", "Work: 579-499-7527", "$1000",
#'   "Home: 543.355.3679")
#' phone <- "([2-9][0-9]{2})[- .]([0-9]{3})[- .]([0-9]{4})"
#'
#' str_extract(strings, phone)
#' str_match(strings, phone)
#'
#' # Extract/match all
#' str_extract_all(strings, phone)
#' str_match_all(strings, phone)
#'
#' # You can also name the groups to make further manipulation easier
#' phone <- "(?<area>[2-9][0-9]{2})[- .](?<phone>[0-9]{3}[- .][0-9]{4})"
#' str_match(strings, phone)
#'
#' x <- c("<a> <b>", "<a> <>", "<a>", "", NA)
#' str_match(x, "<(.*?)> <(.*?)>")
#' str_match_all(x, "<(.*?)>")
#'
#' str_extract(x, "<.*?>")
#' str_extract_all(x, "<.*?>")
str_match <- function(string, pattern) {
  check_lengths(string, pattern)
  if (type(pattern) != "regex") {
    cli::cli_abort(tr_("{.arg pattern} must be a regular expression."))
  }

  out <- stri_match_first_regex(string, pattern, opts_regex = opts(pattern))
  preserve_names_if_possible(string, pattern, out)
}

#' @rdname str_match
#' @export
str_match_all <- function(string, pattern) {
  check_lengths(string, pattern)
  if (type(pattern) != "regex") {
    cli::cli_abort(tr_("{.arg pattern} must be a regular expression."))
  }

  out <- stri_match_all_regex(
    string,
    pattern,
    omit_no_match = TRUE,
    opts_regex = opts(pattern)
  )
  preserve_names_if_possible(string, pattern, out)
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/stringr/main/R/modifiers.R ---
#' Control matching behaviour with modifier functions
#'
#' @description
#' Modifier functions control the meaning of the `pattern` argument to
#' stringr functions:
#'
#' * `boundary()`: Match boundaries between things.
#' * `coll()`: Compare strings using standard Unicode collation rules.
#' * `fixed()`: Compare literal bytes.
#' * `regex()` (the default): Uses ICU regular expressions.
#'
#' @param pattern Pattern to modify behaviour.
#' @param ignore_case Should case differences be ignored in the match?
#'   For `fixed()`, this uses a simple algorithm which assumes a
#'   one-to-one mapping between upper and lower case letters.
#' @return A stringr modifier object, i.e. a character vector with
#'   parent S3 class `stringr_pattern`.
#' @name modifiers
#' @examples
#' pattern <- "a.b"
#' strings <- c("abb", "a.b")
#' str_detect(strings, pattern)
#' str_detect(strings, fixed(pattern))
#' str_detect(strings, coll(pattern))
#'
#' # coll() is useful for locale-aware case-insensitive matching
#' i <- c("I", "\u0130", "i")
#' i
#' str_detect(i, fixed("i", TRUE))
#' str_detect(i, coll("i", TRUE))
#' str_detect(i, coll("i", TRUE, locale = "tr"))
#'
#' # Word boundaries
#' words <- c("These are   some words.")
#' str_count(words, boundary("word"))
#' str_split(words, " ")[[1]]
#' str_split(words, boundary("word"))[[1]]
#'
#' # Regular expression variations
#' str_extract_all("The Cat in the Hat", "[a-z]+")
#' str_extract_all("The Cat in the Hat", regex("[a-z]+", TRUE))
#'
#' str_extract_all("a\nb\nc", "^.")
#' str_extract_all("a\nb\nc", regex("^.", multiline = TRUE))
#'
#' str_extract_all("a\nb\nc", "a.")
#' str_extract_all("a\nb\nc", regex("a.", dotall = TRUE))
NULL

#' @export
#' @rdname modifiers
fixed <- function(pattern, ignore_case = FALSE) {
  pattern <- as_bare_character(pattern)
  check_bool(ignore_case)

  options <- stri_opts_fixed(case_insensitive = ignore_case)

  structure(
    pattern,
    options = options,
    class = c("stringr_fixed", "stringr_pattern", "character")
  )
}

#' @export
#' @rdname modifiers
#' @param locale Locale to use for comparisons. See
#'   [stringi::stri_locale_list()] for all possible options.
#'   Defaults to "en" (English) to ensure that default behaviour is
#'   consistent across platforms.
#' @param ... Other less frequently used arguments passed on to
#'   [stringi::stri_opts_collator()],
#'   [stringi::stri_opts_regex()], or
#'   [stringi::stri_opts_brkiter()]
coll <- function(pattern, ignore_case = FALSE, locale = "en", ...) {
  pattern <- as_bare_character(pattern)
  check_bool(ignore_case)
  check_string(locale)

  options <- str_opts_collator(
    ignore_case = ignore_case,
    locale = locale,
    ...
  )

  structure(
    pattern,
    options = options,
    class = c("stringr_coll", "stringr_pattern", "character")
  )
}


str_opts_collator <- function(
  locale = "en",
  ignore_case = FALSE,
  strength = NULL,
  ...
) {
  strength <- strength %||% if (ignore_case) 2L else 3L
  stri_opts_collator(
    strength = strength,
    locale = locale,
    ...
  )
}

# used for testing
turkish_I <- function() {
  coll("I", ignore_case = TRUE, locale = "tr")
}

#' @export
#' @rdname modifiers
#' @param multiline If `TRUE`, `$` and `^` match
#'   the beginning and end of each line. If `FALSE`, the
#'   default, only match the start and end of the input.
#' @param comments If `TRUE`, white space and comments beginning with
#'   `#` are ignored. Escape literal spaces with `\\ `.
#' @param dotall If `TRUE`, `.` will also match line terminators.
regex <- function(
  pattern,
  ignore_case = FALSE,
  multiline = FALSE,
  comments = FALSE,
  dotall = FALSE,
  ...
) {
  pattern <- as_bare_character(pattern)
  check_bool(ignore_case)
  check_bool(multiline)
  check_bool(comments)
  check_bool(dotall)

  options <- stri_opts_regex(
    case_insensitive = ignore_case,
    multiline = multiline,
    comments = comments,
    dotall = dotall,
    ...
  )

  structure(
    pattern,
    options = options,
    class = c("stringr_regex", "stringr_pattern", "character")
  )
}

#' @param type Boundary type to detect.
#' \describe{
#'  \item{`character`}{Every character is a boundary.}
#'  \item{`line_break`}{Boundaries are places where it is acceptable to have
#'    a line break in the current locale.}
#'  \item{`sentence`}{The beginnings and ends of sentences are boundaries,
#'    using intelligent rules to avoid counting abbreviations
#'    ([details](https://www.unicode.org/reports/tr29/#Sentence_Boundaries)).}
#'  \item{`word`}{The beginnings and ends of words are boundaries.}
#' }
#' @param skip_word_none Ignore "words" that don't contain any characters
#'   or numbers - i.e. punctuation. Default `NA` will skip such "words"
#'   only when splitting on `word` boundaries.
#' @export
#' @rdname modifiers
boundary <- function(
  type = c("character", "line_break", "sentence", "word"),
  skip_word_none = NA,
  ...
) {
  type <- arg_match(type)
  check_bool(skip_word_none, allow_na = TRUE)

  if (identical(skip_word_none, NA)) {
    skip_word_none <- type == "word"
  }

  options <- stri_opts_brkiter(
    type = type,
    skip_word_none = skip_word_none,
    ...
  )

  structure(
    NA_character_,
    options = options,
    class = c("stringr_boundary", "stringr_pattern", "character")
  )
}

opts <- function(x) {
  if (identical(x, "")) {
    stri_opts_brkiter(type = "character")
  } else {
    attr(x, "options")
  }
}

type <- function(x, error_call = caller_env()) {
  UseMethod("type")
}
#' @export
type.stringr_boundary <- function(x, error_call = caller_env()) {
  "bound"
}
#' @export
type.stringr_regex <- function(x, error_call = caller_env()) {
  "regex"
}
#' @export
type.stringr_coll <- function(x, error_call = caller_env()) {
  "coll"
}
#' @export
type.stringr_fixed <- function(x, error_call = caller_env()) {
  "fixed"
}
#' @export
type.character <- function(x, error_call = caller_env()) {
  if (any(is.na(x))) {
    cli::cli_abort(
      tr_("{.arg pattern} can not contain NAs."),
      call = error_call
    )
  }

  if (identical(x, "")) "empty" else "regex"
}

#' @export
type.default <- function(x, error_call = caller_env()) {
  if (inherits(x, "regex")) {
    # Fallback for rex
    return("regex")
  }

  cli::cli_abort(
    tr_(
      "{.arg pattern} must be a character vector, not {.obj_type_friendly {x}}."
    ),
    call = error_call
  )
}

#' @export
`[.stringr_pattern` <- function(x, i) {
  structure(
    NextMethod(),
    options = attr(x, "options"),
    class = class(x)
  )
}

#' @export
`[[.stringr_pattern` <- function(x, i) {
  structure(
    NextMethod(),
    options = attr(x, "options"),
    class = class(x)
  )
}

as_bare_character <- function(x, call = caller_env()) {
  if (is.character(x) && !is.object(x)) {
    # All OK!
    return(x)
  }

  warn("Coercing `pattern` to a plain character vector.", call = call)
  as.character(x)
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/stringr/main/R/pad.R ---
#' Pad a string to minimum width
#'
#' Pad a string to a fixed width, so that
#' `str_length(str_pad(x, n))` is always greater than or equal to `n`.
#'
#' @inheritParams str_detect
#' @param width Minimum width of padded strings.
#' @param side Side on which padding character is added (left, right or both).
#' @param pad Single padding character (default is a space).
#' @param use_width If `FALSE`, use the length of the string instead of the
#'   width; see [str_width()]/[str_length()] for the difference.
#' @return A character vector the same length as `stringr`/`width`/`pad`.
#' @seealso [str_trim()] to remove whitespace;
#'   [str_trunc()] to decrease the maximum width of a string.
#' @export
#' @examples
#' rbind(
#'   str_pad("hadley", 30, "left"),
#'   str_pad("hadley", 30, "right"),
#'   str_pad("hadley", 30, "both")
#' )
#'
#' # All arguments are vectorised except side
#' str_pad(c("a", "abc", "abcdef"), 10)
#' str_pad("a", c(5, 10, 20))
#' str_pad("a", 10, pad = c("-", "_", " "))
#'
#' # Longer strings are returned unchanged
#' str_pad("hadley", 3)
str_pad <- function(
  string,
  width,
  side = c("left", "right", "both"),
  pad = " ",
  use_width = TRUE
) {
  vctrs::vec_size_common(string = string, width = width, pad = pad)
  side <- arg_match(side)
  check_bool(use_width)

  out <- switch(
    side,
    left = stri_pad_left(string, width, pad = pad, use_length = !use_width),
    right = stri_pad_right(string, width, pad = pad, use_length = !use_width),
    both = stri_pad_both(string, width, pad = pad, use_length = !use_width)
  )
  # Preserve names unless `string` is recycled
  if (length(out) == length(string)) copy_names(string, out) else out
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/stringr/main/R/remove.R ---
#' Remove matched patterns
#'
#' Remove matches, i.e. replace them with `""`.
#'
#' @inheritParams str_detect
#' @return A character vector the same length as `string`/`pattern`.
#' @seealso [str_replace()] for the underlying implementation.
#' @export
#' @examples
#' fruits <- c("one apple", "two pears", "three bananas")
#' str_remove(fruits, "[aeiou]")
#' str_remove_all(fruits, "[aeiou]")
str_remove <- function(string, pattern) {
  str_replace(string, pattern, "")
}

#' @export
#' @rdname str_remove
str_remove_all <- function(string, pattern) {
  str_replace_all(string, pattern, "")
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/stringr/main/R/replace.R ---
#' Replace matches with new text
#'
#' `str_replace()` replaces the first match; `str_replace_all()` replaces
#' all matches.
#'
#' @inheritParams str_detect
#' @param pattern Pattern to look for.
#'
#'   The default interpretation is a regular expression, as described
#'   in [stringi::about_search_regex]. Control options with
#'   [regex()].
#'
#'   For `str_replace_all()` this can also be a named vector
#'   (`c(pattern1 = replacement1)`), in order to perform multiple replacements
#'   in each element of `string`.
#'
#'   Match a fixed string (i.e. by comparing only bytes), using
#'   [fixed()]. This is fast, but approximate. Generally,
#'   for matching human text, you'll want [coll()] which
#'   respects character matching rules for the specified locale.
#'
#'   You can not match boundaries, including `""`, with this function.
#' @param replacement The replacement value, usually a single string,
#'   but it can be the a vector the same length as `string` or `pattern`.
#'   References of the form `\1`, `\2`, etc will be replaced with
#'   the contents of the respective matched group (created by `()`).
#'
#'   Alternatively, supply a function (or formula): it will be passed a single
#'   character vector and should return a character vector of the same length.
#'
#'   To replace the complete string with `NA`, use
#'   `replacement = NA_character_`.
#' @return A character vector the same length as
#'   `string`/`pattern`/`replacement`.
#' @seealso [str_replace_na()] to turn missing values into "NA";
#'   [stringi::stri_replace()] for the underlying implementation.
#' @export
#' @examples
#' fruits <- c("one apple", "two pears", "three bananas")
#' str_replace(fruits, "[aeiou]", "-")
#' str_replace_all(fruits, "[aeiou]", "-")
#' str_replace_all(fruits, "[aeiou]", toupper)
#' str_replace_all(fruits, "b", NA_character_)
#'
#' str_replace(fruits, "([aeiou])", "")
#' str_replace(fruits, "([aeiou])", "\\1\\1")
#'
#' # Note that str_replace() is vectorised along text, pattern, and replacement
#' str_replace(fruits, "[aeiou]", c("1", "2", "3"))
#' str_replace(fruits, c("a", "e", "i"), "-")
#'
#' # If you want to apply multiple patterns and replacements to the same
#' # string, pass a named vector to pattern.
#' fruits %>%
#'   str_c(collapse = "---") %>%
#'   str_replace_all(c("one" = "1", "two" = "2", "three" = "3"))
#'
#' # Use a function for more sophisticated replacement. This example
#' # replaces colour names with their hex values.
#' colours <- str_c("\\b", colors(), "\\b", collapse="|")
#' col2hex <- function(col) {
#'   rgb <- col2rgb(col)
#'   rgb(rgb["red", ], rgb["green", ], rgb["blue", ], maxColorValue = 255)
#' }
#'
#' x <- c(
#'   "Roses are red, violets are blue",
#'   "My favourite colour is green"
#' )
#' str_replace_all(x, colours, col2hex)
str_replace <- function(string, pattern, replacement) {
  if (!missing(replacement) && is_replacement_fun(replacement)) {
    replacement <- as_function(replacement)
    return(str_transform(string, pattern, replacement))
  }

  check_lengths(string, pattern, replacement)

  out <- switch(
    type(pattern),
    empty = no_empty(),
    bound = no_boundary(),
    fixed = stri_replace_first_fixed(
      string,
      pattern,
      replacement,
      opts_fixed = opts(pattern)
    ),
    coll = stri_replace_first_coll(
      string,
      pattern,
      replacement,
      opts_collator = opts(pattern)
    ),
    regex = stri_replace_first_regex(
      string,
      pattern,
      fix_replacement(replacement),
      opts_regex = opts(pattern)
    )
  )
  preserve_names_if_possible(string, pattern, out)
}

#' @export
#' @rdname str_replace
str_replace_all <- function(string, pattern, replacement) {
  if (!missing(replacement) && is_replacement_fun(replacement)) {
    replacement <- as_function(replacement)
    return(str_transform_all(string, pattern, replacement))
  }

  if (!is.null(names(pattern))) {
    vec <- FALSE
    replacement <- unname(pattern)
    pattern[] <- names(pattern)
  } else {
    check_lengths(string, pattern, replacement)
    vec <- TRUE
  }

  out <- switch(
    type(pattern),
    empty = no_empty(),
    bound = no_boundary(),
    fixed = stri_replace_all_fixed(
      string,
      pattern,
      replacement,
      vectorize_all = vec,
      opts_fixed = opts(pattern)
    ),
    coll = stri_replace_all_coll(
      string,
      pattern,
      replacement,
      vectorize_all = vec,
      opts_collator = opts(pattern)
    ),
    regex = stri_replace_all_regex(
      string,
      pattern,
      fix_replacement(replacement),
      vectorize_all = vec,
      opts_regex = opts(pattern)
    )
  )
  preserve_names_if_possible(string, pattern, out)
}

is_replacement_fun <- function(x) {
  is.function(x) || is_formula(x)
}

fix_replacement <- function(x, error_call = caller_env()) {
  check_character(x, arg = "replacement", call = error_call)
  vapply(x, fix_replacement_one, character(1), USE.NAMES = FALSE)
}

fix_replacement_one <- function(x) {
  if (is.na(x)) {
    return(x)
  }

  chars <- str_split(x, "")[[1]]
  out <- character(length(chars))
  escaped <- logical(length(chars))

  in_escape <- FALSE
  for (i in seq_along(chars)) {
    escaped[[i]] <- in_escape
    char <- chars[[i]]

    if (in_escape) {
      # Escape character not printed previously so must include here
      if (char == "$") {
        out[[i]] <- "\\\\$"
      } else if (char >= "0" && char <= "9") {
        out[[i]] <- paste0("$", char)
      } else {
        out[[i]] <- paste0("\\", char)
      }

      in_escape <- FALSE
    } else {
      if (char == "$") {
        out[[i]] <- "\\$"
      } else if (char == "\\") {
        in_escape <- TRUE
      } else {
        out[[i]] <- char
      }
    }
  }

  # tibble::tibble(chars, out, escaped)
  paste0(out, collapse = "")
}


#' Turn NA into "NA"
#'
#' @inheritParams str_replace
#' @param replacement A single string.
#' @export
#' @examples
#' str_replace_na(c(NA, "abc", "def"))
str_replace_na <- function(string, replacement = "NA") {
  check_string(replacement)
  copy_names(string, stri_replace_na(string, replacement))
}

str_transform <- function(string, pattern, replacement) {
  loc <- str_locate(string, pattern)
  new <- replacement(str_sub(string, loc))
  str_sub(string, loc, omit_na = TRUE) <- new
  string
}

str_transform_all <- function(
  string,
  pattern,
  replacement,
  error_call = caller_env()
) {
  locs <- str_locate_all(string, pattern)

  old <- str_sub_all(string, locs)

  # unchop list into a vector, apply replacement(), and then rechop back into
  # a list
  old_flat <- vctrs::list_unchop(old)
  if (length(old_flat) == 0) {
    # minor optimisation to avoid problems with the many replacement
    # functions that use paste
    new_flat <- character()
  } else {
    withCallingHandlers(
      new_flat <- replacement(old_flat),
      error = function(cnd) {
        cli::cli_abort(
          c(
            tr_("Failed to apply {.arg replacement} function."),
            i = tr_("It must accept a character vector of any length.")
          ),
          parent = cnd,
          call = error_call
        )
      }
    )
  }

  if (!is.character(new_flat)) {
    cli::cli_abort(
      tr_(
        "{.arg replacement} function must return a character vector, not {.obj_type_friendly {new_flat}}."
      ),
      call = error_call
    )
  }
  if (length(new_flat) != length(old_flat)) {
    cli::cli_abort(
      tr_(
        "{.arg replacement} function must return a vector the same length as the input ({length(old_flat)}), not length {length(new_flat)}."
      ),
      call = error_call
    )
  }

  idx <- chop_index(old)
  new <- vctrs::vec_chop(new_flat, idx)

  stringi::stri_sub_all(string, locs) <- new
  string
}

chop_index <- function(x) {
  ls <- lengths(x)
  start <- cumsum(c(1L, ls[-length(ls)]))
  end <- start + ls - 1L
  lapply(seq_along(ls), function(i) seq2(start[[i]], end[[i]]))
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/stringr/main/R/sort.R ---
#' Order, rank, or sort a character vector
#'
#' * `str_sort()` returns the sorted vector.
#' * `str_order()` returns an integer vector that returns the desired
#'   order when used for subsetting, i.e. `x[str_order(x)]` is the same
#'   as `str_sort()`
#' * `str_rank()` returns the ranks of the values, i.e.
#'   `arrange(df, str_rank(x))` is the same as `str_sort(df$x)`.
#'
#' @param x A character vector to sort.
#' @param decreasing A boolean. If `FALSE`, the default, sorts from
#'   lowest to highest; if `TRUE` sorts from highest to lowest.
#' @param na_last Where should `NA` go? `TRUE` at the end,
#'   `FALSE` at the beginning, `NA` dropped.
#' @param numeric If `TRUE`, will sort digits numerically, instead
#'    of as strings.
#' @param ... Other options used to control collation. Passed on to
#'   [stringi::stri_opts_collator()].
#' @inheritParams coll
#' @return A character vector the same length as `string`.
#' @seealso [stringi::stri_order()] for the underlying implementation.
#' @export
#' @examples
#' x <- c("apple", "car", "happy", "char")
#' str_sort(x)
#'
#' str_order(x)
#' x[str_order(x)]
#'
#' str_rank(x)
#'
#' # In Czech, ch is a digraph that sorts after h
#' str_sort(x, locale = "cs")
#'
#' # Use numeric = TRUE to sort numbers in strings
#' x <- c("100a10", "100a5", "2b", "2a")
#' str_sort(x)
#' str_sort(x, numeric = TRUE)
str_order <- function(
  x,
  decreasing = FALSE,
  na_last = TRUE,
  locale = "en",
  numeric = FALSE,
  ...
) {
  check_bool(decreasing)
  check_bool(na_last, allow_na = TRUE)
  check_string(locale)
  check_bool(numeric)

  opts <- stri_opts_collator(locale, numeric = numeric, ...)
  stri_order(
    x,
    decreasing = decreasing,
    na_last = na_last,
    opts_collator = opts
  )
}

#' @export
#' @rdname str_order
str_rank <- function(x, locale = "en", numeric = FALSE, ...) {
  check_string(locale)
  check_bool(numeric)

  opts <- stri_opts_collator(locale, numeric = numeric, ...)
  stri_rank(x, opts_collator = opts)
}

#' @export
#' @rdname str_order
str_sort <- function(
  x,
  decreasing = FALSE,
  na_last = TRUE,
  locale = "en",
  numeric = FALSE,
  ...
) {
  check_bool(decreasing)
  check_bool(na_last, allow_na = TRUE)
  check_string(locale)
  check_bool(numeric)

  opts <- stri_opts_collator(locale, numeric = numeric, ...)
  idx <- stri_order(
    x,
    decreasing = decreasing,
    na_last = na_last,
    opts_collator = opts
  )
  x[idx]
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/stringr/main/R/split.R ---
#' Split up a string into pieces
#'
#' @description
#' This family of functions provides various ways of splitting a string up
#' into pieces. These two functions return a character vector:
#'
#' * `str_split_1()` takes a single string and splits it into pieces,
#'    returning a single character vector.
#' * `str_split_i()` splits each string in a character vector into pieces and
#'    extracts the `i`th value, returning a character vector.
#'
#' These two functions return a more complex object:
#'
#' * `str_split()` splits each string in a character vector into a varying
#'    number of pieces, returning a list of character vectors.
#' * `str_split_fixed()` splits each string in a character vector into a
#'    fixed number of pieces, returning a character matrix.
#'
#' @inheritParams str_extract
#' @param n Maximum number of pieces to return. Default (Inf) uses all
#'   possible split positions.
#'
#'   For `str_split()`, this determines the maximum length of each element
#'   of the output. For `str_split_fixed()`, this determines the number of
#'   columns in the output; if an input is too short, the result will be padded
#'   with `""`.
#' @return
#' * `str_split_1()`: a character vector.
#' * `str_split()`: a list the same length as `string`/`pattern` containing
#'   character vectors.
#' * `str_split_fixed()`: a character matrix with `n` columns and the same
#'   number of rows as the length of `string`/`pattern`.
#' * `str_split_i()`: a character vector the same length as `string`/`pattern`.
#' @seealso [stringi::stri_split()] for the underlying implementation.
#' @export
#' @examples
#' fruits <- c(
#'   "apples and oranges and pears and bananas",
#'   "pineapples and mangos and guavas"
#' )
#'
#' str_split(fruits, " and ")
#' str_split(fruits, " and ", simplify = TRUE)
#'
#' # If you want to split a single string, use `str_split_1`
#' str_split_1(fruits[[1]], " and ")
#'
#' # Specify n to restrict the number of possible matches
#' str_split(fruits, " and ", n = 3)
#' str_split(fruits, " and ", n = 2)
#' # If n greater than number of pieces, no padding occurs
#' str_split(fruits, " and ", n = 5)
#'
#' # Use fixed to return a character matrix
#' str_split_fixed(fruits, " and ", 3)
#' str_split_fixed(fruits, " and ", 4)
#'
#' # str_split_i extracts only a single piece from a string
#' str_split_i(fruits, " and ", 1)
#' str_split_i(fruits, " and ", 4)
#' # use a negative number to select from the end
#' str_split_i(fruits, " and ", -1)
str_split <- function(string, pattern, n = Inf, simplify = FALSE) {
  check_lengths(string, pattern)
  check_positive_integer(n)
  check_bool(simplify, allow_na = TRUE)

  if (identical(n, Inf)) {
    n <- -1L
  }

  out <- switch(
    type(pattern),
    empty = stri_split_boundaries(
      string,
      n = n,
      simplify = simplify,
      opts_brkiter = opts(pattern)
    ),
    bound = stri_split_boundaries(
      string,
      n = n,
      simplify = simplify,
      opts_brkiter = opts(pattern)
    ),
    fixed = stri_split_fixed(
      string,
      pattern,
      n = n,
      simplify = simplify,
      opts_fixed = opts(pattern)
    ),
    regex = stri_split_regex(
      string,
      pattern,
      n = n,
      simplify = simplify,
      opts_regex = opts(pattern)
    ),
    coll = stri_split_coll(
      string,
      pattern,
      n = n,
      simplify = simplify,
      opts_collator = opts(pattern)
    )
  )

  preserve_names_if_possible(string, pattern, out)
}

#' @export
#' @rdname str_split
str_split_1 <- function(string, pattern) {
  check_string(string)

  str_split(string, pattern)[[1]]
}

#' @export
#' @rdname str_split
str_split_fixed <- function(string, pattern, n) {
  check_lengths(string, pattern)
  check_positive_integer(n)

  str_split(string, pattern, n = n, simplify = TRUE)
}

#' @export
#' @rdname str_split
#' @param i Element to return. Use a negative value to count from the
#'   right hand side.
str_split_i <- function(string, pattern, i) {
  check_number_whole(i)

  if (i > 0) {
    out <- str_split(string, pattern, simplify = NA, n = i + 1)
    col <- out[, i]
    if (keep_names(string, pattern)) copy_names(string, col) else col
  } else if (i < 0) {
    i <- abs(i)
    pieces <- str_split(string, pattern)
    last <- function(x) {
      n <- length(x)
      if (i > n) {
        NA_character_
      } else {
        x[[n + 1 - i]]
      }
    }
    out <- map_chr(pieces, last)
    preserve_names_if_possible(string, pattern, out)
  } else {
    cli::cli_abort(tr_("{.arg i} must not be 0."))
  }
}

check_positive_integer <- function(
  x,
  arg = caller_arg(x),
  call = caller_env()
) {
  if (!identical(x, Inf)) {
    check_number_whole(x, min = 1, arg = arg, call = call)
  }
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/stringr/main/R/stringr-package.R ---
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import stringi
#' @import rlang
#' @importFrom glue glue
#' @importFrom lifecycle deprecated
## usethis namespace: end
NULL


# --- FILE: https://raw.githubusercontent.com/tidyverse/stringr/main/R/sub.R ---
#' Get and set substrings using their positions
#'
#' `str_sub()` extracts or replaces the elements at a single position in each
#' string. `str_sub_all()` allows you to extract strings at multiple elements
#' in every string.
#'
#' @inheritParams str_detect
#' @param start,end A pair of integer vectors defining the range of characters
#'   to extract (inclusive). Positive values count from the left of the string,
#'   and negative values count from the right. In other words, if `string` is
#'   `"abcdef"` then 1 refers to `"a"` and -1 refers to `"f"`.
#'
#'   Alternatively, instead of a pair of vectors, you can pass a matrix to
#'   `start`. The matrix should have two columns, either labelled `start`
#'   and `end`, or `start` and `length`. This makes `str_sub()` work directly
#'   with the output from [str_locate()] and friends.
#'
#' @param omit_na Single logical value. If `TRUE`, missing values in any of the
#'   arguments provided will result in an unchanged input.
#' @param value Replacement string.
#' @return
#' * `str_sub()`: A character vector the same length as `string`/`start`/`end`.
#' * `str_sub_all()`: A list the same length as `string`. Each element is
#'    a character vector the same length as `start`/`end`.
#'
#' If `end` comes before `start` or `start` is outside the range of `string`
#' then the corresponding output will be the empty string.
#' @seealso The underlying implementation in [stringi::stri_sub()]
#' @export
#' @examples
#' hw <- "Hadley Wickham"
#'
#' str_sub(hw, 1, 6)
#' str_sub(hw, end = 6)
#' str_sub(hw, 8, 14)
#' str_sub(hw, 8)
#'
#' # Negative values index from end of string
#' str_sub(hw, -1)
#' str_sub(hw, -7)
#' str_sub(hw, end = -7)
#'
#' # str_sub() is vectorised by both string and position
#' str_sub(hw, c(1, 8), c(6, 14))
#'
#' # if you want to extract multiple positions from multiple strings,
#' # use str_sub_all()
#' x <- c("abcde", "ghifgh")
#' str_sub(x, c(1, 2), c(2, 4))
#' str_sub_all(x, start = c(1, 2), end = c(2, 4))
#'
#' # Alternatively, you can pass in a two column matrix, as in the
#' # output from str_locate_all
#' pos <- str_locate_all(hw, "[aeio]")[[1]]
#' pos
#' str_sub(hw, pos)
#'
#' # You can also use `str_sub()` to modify strings:
#' x <- "BBCDEF"
#' str_sub(x, 1, 1) <- "A"; x
#' str_sub(x, -1, -1) <- "K"; x
#' str_sub(x, -2, -2) <- "GHIJ"; x
#' str_sub(x, 2, -2) <- ""; x
str_sub <- function(string, start = 1L, end = -1L) {
  vctrs::vec_size_common(string = string, start = start, end = end)

  out <- if (is.matrix(start)) {
    stri_sub(string, from = start)
  } else {
    stri_sub(string, from = start, to = end)
  }
  # Preserve names unless `string` is recycled
  if (length(out) == length(string)) copy_names(string, out) else out
}


#' @export
#' @rdname str_sub
"str_sub<-" <- function(string, start = 1L, end = -1L, omit_na = FALSE, value) {
  vctrs::vec_size_common(
    string = string,
    start = start,
    end = end,
    value = value
  )

  if (is.matrix(start)) {
    stri_sub(string, from = start, omit_na = omit_na) <- value
  } else {
    stri_sub(string, from = start, to = end, omit_na = omit_na) <- value
  }
  string
}

#' @export
#' @rdname str_sub
str_sub_all <- function(string, start = 1L, end = -1L) {
  out <- if (is.matrix(start)) {
    stri_sub_all(string, from = start)
  } else {
    stri_sub_all(string, from = start, to = end)
  }
  copy_names(string, out)
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/stringr/main/R/subset.R ---
#' Find matching elements
#'
#' @description
#' `str_subset()` returns all elements of `string` where there's at least
#' one match to `pattern`. It's a wrapper around `x[str_detect(x, pattern)]`,
#' and is equivalent to `grep(pattern, x, value = TRUE)`.
#'
#' Use [str_extract()] to find the location of the match _within_ each string.
#'
#' @inheritParams str_detect
#' @return A character vector, usually smaller than `string`.
#' @seealso [grep()] with argument `value = TRUE`,
#'    [stringi::stri_subset()] for the underlying implementation.
#' @export
#' @examples
#' fruit <- c("apple", "banana", "pear", "pineapple")
#' str_subset(fruit, "a")
#'
#' str_subset(fruit, "^a")
#' str_subset(fruit, "a$")
#' str_subset(fruit, "b")
#' str_subset(fruit, "[aeiou]")
#'
#' # Elements that don't match
#' str_subset(fruit, "^p", negate = TRUE)
#'
#' # Missings never match
#' str_subset(c("a", NA, "b"), ".")
str_subset <- function(string, pattern, negate = FALSE) {
  check_lengths(string, pattern)
  check_bool(negate)

  idx <- switch(
    type(pattern),
    empty = no_empty(),
    bound = no_boundary(),
    fixed = str_detect(string, pattern, negate = negate),
    coll = str_detect(string, pattern, negate = negate),
    regex = str_detect(string, pattern, negate = negate)
  )

  idx[is.na(idx)] <- FALSE
  string[idx]
}

#' Find matching indices
#'
#' `str_which()` returns the indices of `string` where there's at least
#' one match to `pattern`. It's a wrapper around
#' `which(str_detect(x, pattern))`, and is equivalent to `grep(pattern, x)`.
#'
#' @inheritParams str_detect
#' @return An integer vector, usually smaller than `string`.
#' @export
#' @examples
#' fruit <- c("apple", "banana", "pear", "pineapple")
#' str_which(fruit, "a")
#'
#' # Elements that don't match
#' str_which(fruit, "^p", negate = TRUE)
#'
#' # Missings never match
#' str_which(c("a", NA, "b"), ".")
str_which <- function(string, pattern, negate = FALSE) {
  which(str_detect(string, pattern, negate = negate))
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/stringr/main/R/trim.R ---
#' Remove whitespace
#'
#' `str_trim()` removes whitespace from start and end of string; `str_squish()`
#' removes whitespace at the start and end, and replaces all internal whitespace
#' with a single space.
#'
#' @inheritParams str_detect
#' @param side Side on which to remove whitespace: "left", "right", or
#'   "both", the default.
#' @return A character vector the same length as `string`.
#' @export
#' @seealso [str_pad()] to add whitespace
#' @examples
#' str_trim("  String with trailing and leading white space\t")
#' str_trim("\n\nString with trailing and leading white space\n\n")
#'
#' str_squish("  String with trailing,  middle, and leading white space\t")
#' str_squish("\n\nString with excess,  trailing and leading white   space\n\n")
str_trim <- function(string, side = c("both", "left", "right")) {
  side <- arg_match(side)

  out <- switch(
    side,
    left = stri_trim_left(string),
    right = stri_trim_right(string),
    both = stri_trim_both(string)
  )
  copy_names(string, out)
}

#' @export
#' @rdname str_trim
str_squish <- function(string) {
  copy_names(string, stri_trim_both(str_replace_all(string, "\\s+", " ")))
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/stringr/main/R/trunc.R ---
#' Truncate a string to maximum width
#'
#' Truncate a string to a fixed of characters, so that
#' `str_length(str_trunc(x, n))` is always less than or equal to `n`.
#'
#' @inheritParams str_detect
#' @param width Maximum width of string.
#' @param side,ellipsis Location and content of ellipsis that indicates
#'   content has been removed.
#' @return A character vector the same length as `string`.
#' @seealso [str_pad()] to increase the minimum width of a string.
#' @export
#' @examples
#' x <- "This string is moderately long"
#' rbind(
#'   str_trunc(x, 20, "right"),
#'   str_trunc(x, 20, "left"),
#'   str_trunc(x, 20, "center")
#' )
str_trunc <- function(
  string,
  width,
  side = c("right", "left", "center"),
  ellipsis = "..."
) {
  check_number_whole(width)
  side <- arg_match(side)
  check_string(ellipsis)

  len <- str_length(string)
  too_long <- !is.na(string) & len > width
  width... <- width - str_length(ellipsis)

  if (width... < 0) {
    cli::cli_abort(
      tr_(
        "`width` ({width}) is shorter than `ellipsis` ({str_length(ellipsis)})."
      )
    )
  }

  string[too_long] <- switch(
    side,
    right = str_c(str_sub(string[too_long], 1, width...), ellipsis),
    left = str_c(
      ellipsis,
      str_sub(string[too_long], len[too_long] - width... + 1, -1)
    ),
    center = str_c(
      str_sub(string[too_long], 1, ceiling(width... / 2)),
      ellipsis,
      str_sub(string[too_long], len[too_long] - floor(width... / 2) + 1, -1)
    )
  )
  string
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/stringr/main/R/unique.R ---
#' Remove duplicated strings
#'
#' `str_unique()` removes duplicated values, with optional control over
#' how duplication is measured.
#'
#' @inheritParams str_detect
#' @inheritParams str_equal
#' @return A character vector, usually shorter than `string`.
#' @seealso [unique()], [stringi::stri_unique()] which this function wraps.
#' @examples
#' str_unique(c("a", "b", "c", "b", "a"))
#'
#' str_unique(c("a", "b", "c", "B", "A"))
#' str_unique(c("a", "b", "c", "B", "A"), ignore_case = TRUE)
#'
#' # Use ... to pass additional arguments to stri_unique()
#' str_unique(c("motley", "mötley", "pinguino", "pingüino"))
#' str_unique(c("motley", "mötley", "pinguino", "pingüino"), strength = 1)
#' @export
str_unique <- function(string, locale = "en", ignore_case = FALSE, ...) {
  check_string(locale)
  check_bool(ignore_case)

  opts <- str_opts_collator(
    locale = locale,
    ignore_case = ignore_case,
    ...
  )

  keep <- !stringi::stri_duplicated(string, opts_collator = opts)
  string[keep]
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/stringr/main/R/utils.R ---
#' Pipe operator
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

check_lengths <- function(
  string,
  pattern,
  replacement = NULL,
  error_call = caller_env()
) {
  # stringi already correctly recycles vectors of length 0 and 1
  # we just want more stringent vctrs checks for other lengths
  vctrs::vec_size_common(
    string = string,
    pattern = pattern,
    replacement = replacement,
    .call = error_call
  )
}

no_boundary <- function(call = caller_env()) {
  cli::cli_abort(tr_("{.arg pattern} can't be a boundary."), call = call)
}
no_empty <- function(call = caller_env()) {
  cli::cli_abort(
    tr_("{.arg pattern} can't be the empty string ({.code \"\"})."),
    call = call
  )
}

tr_ <- function(...) {
  enc2utf8(gettext(paste0(...), domain = "R-stringr"))
}

# copy names from `string` to output, regardless of output type
copy_names <- function(from, to) {
  nm <- names(from)
  if (is.null(nm)) {
    return(to)
  }

  if (is.matrix(to)) {
    rownames(to) <- nm
    to
  } else {
    set_names(to, nm)
  }
}

# keep names if pattern is scalar (i.e. vectorised) or same length as string.
keep_names <- function(string, pattern) {
  length(pattern) == 1L || length(pattern) == length(string)
}

preserve_names_if_possible <- function(string, pattern, out) {
  if (keep_names(string, pattern)) {
    copy_names(string, out)
  } else {
    out
  }
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/stringr/main/R/view.R ---
#' View strings and matches
#'
#' @description
#' `str_view()` is used to print the underlying representation of a string and
#' to see how a `pattern` matches.
#'
#' Matches are surrounded by `<>` and unusual whitespace (i.e. all whitespace
#' apart from `" "` and `"\n"`) are surrounded by `{}` and escaped. Where
#' possible, matches and unusual whitespace are coloured blue and `NA`s red.
#'
#' @inheritParams str_detect
#' @param match If `pattern` is supplied, which elements should be shown?
#'
#'   * `TRUE`, the default, shows only elements that match the pattern.
#'   * `NA` shows all elements.
#'   * `FALSE` shows only elements that don't match the pattern.
#'
#'   If `pattern` is not supplied, all elements are always shown.
#' @param html Use HTML output? If `TRUE` will create an HTML widget; if `FALSE`
#'   will style using ANSI escapes.
#' @param use_escapes If `TRUE`, all non-ASCII characters will be rendered
#'   with unicode escapes. This is useful to see exactly what underlying
#'   values are stored in the string.
#' @export
#' @examples
#' # Show special characters
#' str_view(c("\"\\", "\\\\\\", "fgh", NA, "NA"))
#'
#' # A non-breaking space looks like a regular space:
#' nbsp <- "Hi\u00A0you"
#' nbsp
#' # But it doesn't behave like one:
#' str_detect(nbsp, " ")
#' # So str_view() brings it to your attention with a blue background
#' str_view(nbsp)
#'
#' # You can also use escapes to see all non-ASCII characters
#' str_view(nbsp, use_escapes = TRUE)
#'
#' # Supply a pattern to see where it matches
#' str_view(c("abc", "def", "fghi"), "[aeiou]")
#' str_view(c("abc", "def", "fghi"), "^")
#' str_view(c("abc", "def", "fghi"), "..")
#'
#' # By default, only matching strings will be shown
#' str_view(c("abc", "def", "fghi"), "e")
#' # but you can show all:
#' str_view(c("abc", "def", "fghi"), "e", match = NA)
#' # or just those that don't match:
#' str_view(c("abc", "def", "fghi"), "e", match = FALSE)
str_view <- function(
  string,
  pattern = NULL,
  match = TRUE,
  html = FALSE,
  use_escapes = FALSE
) {
  rec <- vctrs::vec_recycle_common(string = string, pattern = pattern)
  string <- rec$string
  pattern <- rec$pattern

  check_bool(match, allow_na = TRUE)
  check_bool(html)
  check_bool(use_escapes)

  filter <- str_view_filter(string, pattern, match)
  out <- string[filter]
  pattern <- pattern[filter]

  if (!is.null(pattern)) {
    out <- str_replace_all(out, pattern, str_view_highlighter(html))
  }
  if (use_escapes) {
    out <- stri_escape_unicode(out)
    out <- str_replace_all(out, fixed("\\u001b"), "\u001b")
  } else {
    out <- str_view_special(out, html = html)
  }

  str_view_print(out, filter, html = html)
}

#' @rdname str_view
#' @usage NULL
#' @export
str_view_all <- function(
  string,
  pattern = NULL,
  match = NA,
  html = FALSE,
  use_escapes = FALSE
) {
  lifecycle::deprecate_warn("1.5.0", "str_view_all()", "str_view()")

  str_view(
    string = string,
    pattern = pattern,
    match = match,
    html = html,
    use_escapes = use_escapes
  )
}

str_view_filter <- function(x, pattern, match) {
  if (is.null(pattern) || inherits(pattern, "stringr_boundary")) {
    rep(TRUE, length(x))
  } else {
    if (identical(match, TRUE)) {
      str_detect(x, pattern) & !is.na(x)
    } else if (identical(match, FALSE)) {
      !str_detect(x, pattern) | is.na(x)
    } else {
      rep(TRUE, length(x))
    }
  }
}

# Helpers -----------------------------------------------------------------

str_view_highlighter <- function(html = TRUE) {
  if (html) {
    function(x) str_c("<span class='match'>", x, "</span>")
  } else {
    function(x) {
      out <- cli::col_cyan("<", x, ">")

      # Ensure styling is starts and ends within each line
      out <- cli::ansi_strsplit(out, "\n", fixed = TRUE)
      out <- map_chr(out, str_flatten, "\n")

      out
    }
  }
}

str_view_special <- function(x, html = TRUE) {
  if (html) {
    replace <- function(x) str_c("<span class='special'>", x, "</span>")
  } else {
    replace <- function(x) {
      if (length(x) == 0) {
        return(character())
      }

      cli::col_cyan("{", stri_escape_unicode(x), "}")
    }
  }

  # Highlight any non-standard whitespace characters
  str_replace_all(x, "[\\p{Whitespace}-- \n]+", replace)
}

str_view_print <- function(x, filter, html = TRUE) {
  if (html) {
    str_view_widget(x)
  } else {
    structure(x, id = which(filter), class = "stringr_view")
  }
}

str_view_widget <- function(lines) {
  check_installed(c("htmltools", "htmlwidgets"))

  lines <- str_replace_na(lines)
  bullets <- str_c(
    "<ul>\n",
    str_c("  <li><pre>", lines, "</pre></li>", collapse = "\n"),
    "\n</ul>"
  )

  html <- htmltools::HTML(bullets)
  size <- htmlwidgets::sizingPolicy(
    knitr.figure = FALSE,
    defaultHeight = pmin(10 * length(lines), 300),
    knitr.defaultHeight = "100%"
  )

  htmlwidgets::createWidget(
    "str_view",
    list(html = html),
    sizingPolicy = size,
    package = "stringr"
  )
}

#' @export
print.stringr_view <- function(x, ..., n = getOption("stringr.view_n", 20)) {
  n_extra <- length(x) - n
  if (n_extra > 0) {
    x <- x[seq_len(n)]
  }

  if (length(x) == 0) {
    cli::cli_inform(c(x = "Empty `string` provided.\n"))
    return(invisible(x))
  }

  bar <- if (cli::is_utf8_output()) "\u2502" else "|"

  id <- format(paste0("[", attr(x, "id"), "] "), justify = "right")
  indent <- paste0(cli::col_grey(id, bar), " ")
  exdent <- paste0(strrep(" ", nchar(id[[1]])), cli::col_grey(bar), " ")

  x[is.na(x)] <- cli::col_red("NA")
  x <- paste0(indent, x)
  x <- str_replace_all(x, "\n", paste0("\n", exdent))

  cat(x, sep = "\n")
  if (n_extra > 0) {
    cat("... and ", n_extra, " more\n", sep = "")
  }

  invisible(x)
}

#' @export
`[.stringr_view` <- function(x, i, ...) {
  structure(NextMethod(), id = attr(x, "id")[i], class = "stringr_view")
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/stringr/main/R/word.R ---
#' Extract words from a sentence
#'
#' @inheritParams str_detect
#' @param start,end Pair of integer vectors giving range of words (inclusive)
#'   to extract. If negative, counts backwards from the last word.
#'
#'   The default value select the first word.
#' @param sep Separator between words. Defaults to single space.
#' @return A character vector with the same length as `string`/`start`/`end`.
#' @export
#' @examples
#' sentences <- c("Jane saw a cat", "Jane sat down")
#' word(sentences, 1)
#' word(sentences, 2)
#' word(sentences, -1)
#' word(sentences, 2, -1)
#'
#' # Also vectorised over start and end
#' word(sentences[1], 1:3, -1)
#' word(sentences[1], 1, 1:4)
#'
#' # Can define words by other separators
#' str <- 'abc.def..123.4568.999'
#' word(str, 1, sep = fixed('..'))
#' word(str, 2, sep = fixed('..'))
word <- function(string, start = 1L, end = start, sep = fixed(" ")) {
  args <- vctrs::vec_recycle_common(string = string, start = start, end = end)
  string <- args$string
  start <- args$start
  end <- args$end

  breaks <- str_locate_all(string, sep)
  words <- lapply(breaks, invert_match)

  # Convert negative values into actual positions
  len <- vapply(words, nrow, integer(1))

  neg_start <- !is.na(start) & start < 0L
  start[neg_start] <- start[neg_start] + len[neg_start] + 1L

  neg_end <- !is.na(end) & end < 0L
  end[neg_end] <- end[neg_end] + len[neg_end] + 1L

  # Replace indexes past end with NA
  start[start > len] <- NA
  end[end > len] <- NA

  # To return all words when trying to extract more words than available
  start[start < 1L] <- 1

  # Extract locations
  starts <- mapply(function(word, loc) word[loc, "start"], words, start)
  ends <- mapply(function(word, loc) word[loc, "end"], words, end)

  copy_names(string, str_sub(string, starts, ends))
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/stringr/main/R/wrap.R ---
#' Wrap words into nicely formatted paragraphs
#'
#' Wrap words into paragraphs, minimizing the "raggedness" of the lines
#' (i.e. the variation in length line) using the Knuth-Plass algorithm.
#'
#' @inheritParams str_detect
#' @param width Positive integer giving target line width (in number of
#'   characters). A width less than or equal to 1 will put each word on its
#'   own line.
#' @param indent,exdent A non-negative integer giving the indent for the
#'   first line (`indent`) and all subsequent lines (`exdent`).
#' @param whitespace_only A boolean.
#'   * If `TRUE` (the default) wrapping will only occur at whitespace.
#'   * If `FALSE`, can break on any non-word character (e.g. `/`, `-`).
#' @return A character vector the same length as `string`.
#' @seealso [stringi::stri_wrap()] for the underlying implementation.
#' @export
#' @examples
#' thanks_path <- file.path(R.home("doc"), "THANKS")
#' thanks <- str_c(readLines(thanks_path), collapse = "\n")
#' thanks <- word(thanks, 1, 3, fixed("\n\n"))
#' cat(str_wrap(thanks), "\n")
#' cat(str_wrap(thanks, width = 40), "\n")
#' cat(str_wrap(thanks, width = 60, indent = 2), "\n")
#' cat(str_wrap(thanks, width = 60, exdent = 2), "\n")
#' cat(str_wrap(thanks, width = 0, exdent = 2), "\n")
str_wrap <- function(
  string,
  width = 80,
  indent = 0,
  exdent = 0,
  whitespace_only = TRUE
) {
  check_number_decimal(width)
  if (width <= 0) {
    width <- 1
  }
  check_number_whole(indent)
  check_number_whole(exdent)
  check_bool(whitespace_only)

  out <- stri_wrap(
    string,
    width = width,
    indent = indent,
    exdent = exdent,
    whitespace_only = whitespace_only,
    simplify = FALSE
  )
  out <- vapply(out, str_c, collapse = "\n", character(1))
  copy_names(string, out)
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/stringr/main/data-raw/samples.R ---
words <- rcorpora::corpora("words/common")$commonWords
fruit <- rcorpora::corpora("foods/fruits")$fruits

html <- read_html("https://harvardsentences.com")
html %>%
  html_elements("li") %>%
  html_text() %>%
  iconv(to = "ASCII//translit") %>%
  writeLines("data-raw/harvard-sentences.txt")
sentences <- readr::read_lines("data-raw/harvard-sentences.txt")

usethis::use_data(words, overwrite = TRUE)
usethis::use_data(fruit, overwrite = TRUE)
usethis::use_data(sentences, overwrite = TRUE)


# --- FILE: https://raw.githubusercontent.com/tidyverse/stringr/main/tests/testthat.R ---
library(testthat)
library(stringr)

test_check("stringr")


# --- FILE: https://raw.githubusercontent.com/tidyverse/stringr/main/tests/testthat/test-c.R ---
test_that("basic case works", {
  test <- c("a", "b", "c")

  expect_equal(str_c(test), test)
  expect_equal(str_c(test, sep = " "), test)
  expect_equal(str_c(test, collapse = ""), "abc")
})

test_that("obeys tidyverse recycling rules", {
  expect_equal(str_c(), character())

  expect_equal(str_c("x", character()), character())
  expect_equal(str_c("x", NULL), "x")

  expect_snapshot(str_c(c("x", "y"), character()), error = TRUE)
  expect_equal(str_c(c("x", "y"), NULL), c("x", "y"))
})

test_that("vectorised arguments error", {
  expect_snapshot(error = TRUE, {
    str_c(letters, sep = c("a", "b"))
    str_c(letters, collapse = c("a", "b"))
  })
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/stringr/main/tests/testthat/test-case.R ---
test_that("to_upper and to_lower have equivalent base versions", {
  x <- "This is a sentence."
  expect_identical(str_to_upper(x), toupper(x))
  expect_identical(str_to_lower(x), tolower(x))
})

test_that("to_title creates one capital letter per word", {
  x <- "This is a sentence."
  expect_equal(str_count(x, "\\W+"), str_count(str_to_title(x), "[[:upper:]]"))
})

test_that("to_sentence capitalizes just the first letter", {
  expect_identical(str_to_sentence("a Test"), "A test")
})

test_that("case conversions preserve names", {
  x <- c(C = "3", B = "2", A = "1")
  expect_equal(names(str_to_lower(x)), names(x))
  expect_equal(names(str_to_upper(x)), names(x))
  expect_equal(names(str_to_title(x)), names(x))
})

# programming cases -----------------------------------------------------------

test_that("to_camel can control case of first argument", {
  expect_equal(str_to_camel("my_variable"), "myVariable")
  expect_equal(str_to_camel("my$variable"), "myVariable")
  expect_equal(str_to_camel(" my    variable  "), "myVariable")
  expect_equal(str_to_camel("my_variable", first_upper = TRUE), "MyVariable")
})

test_that("to_kebab converts to kebab case", {
  expect_equal(str_to_kebab("myVariable"), "my-variable")
  expect_equal(str_to_kebab("MyVariable"), "my-variable")
  expect_equal(str_to_kebab("1MyVariable1"), "1-my-variable-1")
  expect_equal(str_to_kebab("My$Variable"), "my-variable")
  expect_equal(str_to_kebab(" My   Variable  "), "my-variable")
  expect_equal(str_to_kebab("testABCTest"), "test-abc-test")
  expect_equal(str_to_kebab("IlÉtaitUneFois"), "il-était-une-fois")
})

test_that("to_snake converts to snake case", {
  expect_equal(str_to_snake("myVariable"), "my_variable")
  expect_equal(str_to_snake("MyVariable"), "my_variable")
  expect_equal(str_to_snake("1MyVariable1"), "1_my_variable_1")
  expect_equal(str_to_snake("My$Variable"), "my_variable")
  expect_equal(str_to_snake(" My   Variable  "), "my_variable")
  expect_equal(str_to_snake("testABCTest"), "test_abc_test")
  expect_equal(str_to_snake("IlÉtaitUneFois"), "il_était_une_fois")
})

test_that("to_words handles common compound cases", {
  expect_equal(to_words("a_b"), "a b")
  expect_equal(to_words("a-b"), "a b")
  expect_equal(to_words("aB"), "a b")
  expect_equal(to_words("a123b"), "a 123 b")
  expect_equal(to_words("HTML"), "html")
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/stringr/main/tests/testthat/test-conv.R ---
test_that("encoding conversion works", {
  skip_on_os("windows")

  x <- rawToChar(as.raw(177))
  expect_equal(str_conv(x, "latin1"), "±")
})

test_that("check encoding argument", {
  expect_snapshot(str_conv("A", c("ISO-8859-1", "ISO-8859-2")), error = TRUE)
})

test_that("str_conv() preserves names", {
  x <- c(C = "3", B = "2", A = "1")
  expect_equal(names(str_conv(x, "UTF-8")), names(x))
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/stringr/main/tests/testthat/test-count.R ---
test_that("counts are as expected", {
  fruit <- c("apple", "banana", "pear", "pineapple")
  expect_equal(str_count(fruit, "a"), c(1, 3, 1, 1))
  expect_equal(str_count(fruit, "p"), c(2, 0, 1, 3))
  expect_equal(str_count(fruit, "e"), c(1, 0, 1, 2))
  expect_equal(str_count(fruit, c("a", "b", "p", "n")), c(1, 1, 1, 1))
})

test_that("uses tidyverse recycling rules", {
  expect_error(str_count(1:2, 1:3), class = "vctrs_error_incompatible_size")
})

test_that("can use fixed() and coll()", {
  expect_equal(str_count("x.", fixed(".")), 1)
  expect_equal(str_count("\u0131", turkish_I()), 1)
})

test_that("can count boundaries", {
  # str_count(x, boundary()) == lengths(str_split(x, boundary()))
  expect_equal(str_count("a b c", ""), 5)
  expect_equal(str_count("a b c", boundary("word")), 3)
})

test_that("str_count() preserves names", {
  x <- c(C = "3", B = "2", A = "1")
  expect_equal(names(str_count(x, ".")), names(x))
})

test_that("str_count() drops names when pattern is vector and string is scalar", {
  x1 <- c(A = "ab")
  p2 <- c("a", "b")
  expect_null(names(str_count(x1, p2)))
})

test_that("str_count() preserves names when pattern and string have same length", {
  x2 <- c(A = "ab", B = "cd")
  p2 <- c("a", "c")
  expect_equal(names(str_count(x2, p2)), names(x2))
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/stringr/main/tests/testthat/test-detect.R ---
test_that("special cases are correct", {
  expect_equal(str_detect(NA, "x"), NA)
  expect_equal(str_detect(character(), "x"), logical())
})

test_that("vectorised patterns work", {
  expect_equal(str_detect("ab", c("a", "b", "c")), c(T, T, F))
  expect_equal(str_detect(c("ca", "ab"), c("a", "c")), c(T, F))

  # negation works
  expect_equal(str_detect("ab", c("a", "b", "c"), negate = TRUE), c(F, F, T))
})

test_that("str_starts() and str_ends() match expected strings", {
  expect_equal(str_starts(c("ab", "ba"), "a"), c(TRUE, FALSE))
  expect_equal(str_ends(c("ab", "ba"), "a"), c(FALSE, TRUE))

  # negation
  expect_equal(str_starts(c("ab", "ba"), "a", negate = TRUE), c(FALSE, TRUE))
  expect_equal(str_ends(c("ab", "ba"), "a", negate = TRUE), c(TRUE, FALSE))

  # correct precedence
  expect_equal(str_starts(c("ab", "ba", "cb"), "a|b"), c(TRUE, TRUE, FALSE))
  expect_equal(str_ends(c("ab", "ba", "bc"), "a|b"), c(TRUE, TRUE, FALSE))
})

test_that("can use fixed() and coll()", {
  expect_equal(str_detect("X", fixed(".")), FALSE)
  expect_equal(str_starts("X", fixed(".")), FALSE)
  expect_equal(str_ends("X", fixed(".")), FALSE)

  expect_equal(str_detect("\u0131", turkish_I()), TRUE)
  expect_equal(str_starts("\u0131", turkish_I()), TRUE)
  expect_equal(str_ends("\u0131", turkish_I()), TRUE)
})

test_that("can't empty/boundary", {
  expect_snapshot(error = TRUE, {
    str_detect("x", "")
    str_starts("x", "")
    str_ends("x", "")
  })
})

test_that("functions use tidyverse recycling rules", {
  expect_snapshot(error = TRUE, {
    str_detect(1:2, 1:3)
    str_starts(1:2, 1:3)
    str_ends(1:2, 1:3)
    str_like(1:2, c("a", "b", "c"))
  })
})

test_that("detection functions preserve names", {
  x <- c(C = "3", B = "2", A = "1")
  expect_equal(names(str_detect(x, "[123]")), names(x))
  expect_equal(names(str_starts(x, "1")), names(x))
  expect_equal(names(str_ends(x, "1")), names(x))
  expect_equal(names(str_like(x, "%")), names(x))
  expect_equal(names(str_ilike(x, "%")), names(x))
})

test_that("detection drops names when pattern is vector and string is scalar", {
  x1 <- c(A = "ab")
  p2 <- c("a", "b")
  expect_null(names(str_detect(x1, p2)))
  expect_null(names(str_starts(x1, p2)))
  expect_null(names(str_ends(x1, p2)))
  expect_null(names(str_like(x1, p2)))
  expect_null(names(str_ilike(x1, p2)))
})

test_that("detection preserves names when pattern and string have same length", {
  x2 <- c(A = "ab", B = "cd")
  p2 <- c("a", "c")
  expect_equal(names(str_detect(x2, p2)), names(x2))
  expect_equal(names(str_starts(x2, p2)), names(x2))
  expect_equal(names(str_ends(x2, p2)), names(x2))
  expect_equal(names(str_like(x2, p2)), names(x2))
  expect_equal(names(str_ilike(x2, p2)), names(x2))
})

# str_like ----------------------------------------------------------------

test_that("str_like is case sensitive", {
  expect_true(str_like("abc", "ab%"))
  expect_false(str_like("abc", "AB%"))
  expect_snapshot(str_like("abc", regex("x")), error = TRUE)
})

test_that("ignore_case is deprecated but still respected", {
  expect_snapshot(out <- str_like("abc", "AB%", ignore_case = TRUE))
  expect_equal(out, TRUE)

  expect_warning(out <- str_like("abc", "AB%", ignore_case = FALSE))
  expect_equal(out, FALSE)
})

test_that("str_ilike works", {
  expect_true(str_ilike("abc", "ab%"))
  expect_true(str_ilike("abc", "AB%"))
  expect_snapshot(str_ilike("abc", regex("x")), error = TRUE)
})

test_that("like_to_regex generates expected regexps", {
  expect_equal(like_to_regex("ab%"), "^ab.*$")
  expect_equal(like_to_regex("ab_"), "^ab.$")

  # escaping
  expect_equal(like_to_regex("ab\\%"), "^ab\\%$")
  expect_equal(like_to_regex("ab[%]"), "^ab[%]$")
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/stringr/main/tests/testthat/test-dup.R ---
test_that("basic duplication works", {
  expect_equal(str_dup("a", 3), "aaa")
  expect_equal(str_dup("abc", 2), "abcabc")
  expect_equal(str_dup(c("a", "b"), 2), c("aa", "bb"))
  expect_equal(str_dup(c("a", "b"), c(2, 3)), c("aa", "bbb"))
})

test_that("0 duplicates equals empty string", {
  expect_equal(str_dup("a", 0), "")
  expect_equal(str_dup(c("a", "b"), 0), rep("", 2))
})

test_that("uses tidyverse recycling rules", {
  expect_error(str_dup(1:2, 1:3), class = "vctrs_error_incompatible_size")
})

test_that("uses sep argument", {
  expect_equal(str_dup("abc", 1, sep = "-"), "abc")
  expect_equal(str_dup("abc", 2, sep = "-"), "abc-abc")

  expect_equal(str_dup(c("a", "b"), 2, sep = "-"), c("a-a", "b-b"))
  expect_equal(str_dup(c("a", "b"), c(1, 2), sep = "-"), c("a", "b-b"))

  expect_equal(str_dup(character(), 1, sep = "-"), character())
  expect_equal(str_dup(character(), 2, sep = "-"), character())
})

test_that("separator must be a single string", {
  expect_snapshot(error = TRUE, {
    str_dup("a", 3, sep = 1)
    str_dup("a", 3, sep = c("-", ";"))
  })
})

test_that("str_dup() preserves names", {
  x <- c(C = "3", B = "2", A = "1")
  expect_equal(names(str_dup(x, 2)), names(x))
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/stringr/main/tests/testthat/test-equal.R ---
test_that("vectorised using TRR", {
  expect_equal(str_equal("a", character()), logical())
  expect_equal(str_equal("a", "b"), FALSE)
  expect_equal(str_equal("a", c("a", "b")), c(TRUE, FALSE))
  expect_snapshot(str_equal(letters[1:3], c("a", "b")), error = TRUE)
})

test_that("can ignore case", {
  expect_equal(str_equal("a", "A"), FALSE)
  expect_equal(str_equal("a", "A", ignore_case = TRUE), TRUE)
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/stringr/main/tests/testthat/test-escape.R ---
test_that("multiplication works", {
  expect_equal(
    str_escape(".^$|*+?{}[]()"),
    "\\.\\^\\$\\|\\*\\+\\?\\{\\}\\[\\]\\(\\)"
  )
  expect_equal(str_escape("\\"), "\\\\")
})

test_that("str_escape() preserves names", {
  x <- c(C = "3", B = "2", A = "1")
  expect_equal(names(str_escape(x)), names(x))
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/stringr/main/tests/testthat/test-extract.R ---
test_that("single pattern extracted correctly", {
  test <- c("one two three", "a b c")

  expect_equal(
    str_extract_all(test, "[a-z]+"),
    list(c("one", "two", "three"), c("a", "b", "c"))
  )

  expect_equal(
    str_extract_all(test, "[a-z]{3,}"),
    list(c("one", "two", "three"), character())
  )
})

test_that("uses tidyverse recycling rules", {
  expect_error(
    str_extract(c("a", "b"), c("a", "b", "c")),
    class = "vctrs_error_incompatible_size"
  )
  expect_error(
    str_extract_all(c("a", "b"), c("a", "b", "c")),
    class = "vctrs_error_incompatible_size"
  )
})


test_that("no match yields empty vector", {
  expect_equal(str_extract_all("a", "b")[[1]], character())
})

test_that("str_extract extracts first match if found, NA otherwise", {
  shopping_list <- c("apples x4", "bag of flour", "bag of sugar", "milk x2")
  word_1_to_4 <- str_extract(shopping_list, "\\b[a-z]{1,4}\\b")

  expect_length(word_1_to_4, length(shopping_list))
  expect_equal(word_1_to_4[1], NA_character_)
})

test_that("can extract a group", {
  expect_equal(str_extract("abc", "(.).(.)", group = 1), "a")
  expect_equal(str_extract("abc", "(.).(.)", group = 2), "c")
})

test_that("can use fixed() and coll()", {
  expect_equal(str_extract("x.x", fixed(".")), ".")
  expect_equal(str_extract_all("x.x.", fixed(".")), list(c(".", ".")))

  expect_equal(str_extract("\u0131", turkish_I()), "\u0131")
  expect_equal(str_extract_all("\u0131I", turkish_I()), list(c("\u0131", "I")))
})

test_that("can extract boundaries", {
  expect_equal(str_extract("a b c", ""), "a")
  expect_equal(
    str_extract_all("a b c", ""),
    list(c("a", " ", "b", " ", "c"))
  )

  expect_equal(str_extract("a b c", boundary("word")), "a")
  expect_equal(
    str_extract_all("a b c", boundary("word")),
    list(c("a", "b", "c"))
  )
})

test_that("str_extract() preserves names", {
  x <- c(C = "3", B = "2", A = "1")
  expect_equal(names(str_extract(x, "[0-9]")), names(x))
})

test_that("str_extract_all() preserves names on outer structure", {
  x <- c(C = "3", B = "2", A = "1")
  expect_equal(names(str_extract_all(x, "[0-9]")), names(x))
})

test_that("str_extract and extract_all handle vectorised patterns and names", {
  x1 <- c(A = "ab")
  p2 <- c("a", "b")
  expect_null(names(str_extract(x1, p2)))
  expect_null(names(str_extract_all(x1, p2)))

  x2 <- c(A = "ab", B = "cd")
  expect_equal(names(str_extract(x2, p2)), names(x2))
  expect_equal(names(str_extract_all(x2, p2)), names(x2))
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/stringr/main/tests/testthat/test-flatten.R ---
test_that("equivalent to paste with collapse", {
  expect_equal(str_flatten(letters), paste0(letters, collapse = ""))
})

test_that("collapse must be single string", {
  expect_snapshot(str_flatten("A", c("a", "b")), error = TRUE)
})

test_that("last optionally used instead of final separator", {
  expect_equal(str_flatten(letters[1:3], ", ", ", and "), "a, b, and c")
  expect_equal(str_flatten(letters[1:2], ", ", ", and "), "a, and b")
  expect_equal(str_flatten(letters[1], ", ", ", and "), "a")
})

test_that("can remove missing values", {
  expect_equal(str_flatten(c("a", NA)), NA_character_)
  expect_equal(str_flatten(c("a", NA), na.rm = TRUE), "a")
})

test_that("str_flatten_oxford removes comma iif necessary", {
  expect_equal(str_flatten_comma(letters[1:2], ", or "), "a or b")

  expect_equal(str_flatten_comma(letters[1:3], ", or "), "a, b, or c")
  expect_equal(str_flatten_comma(letters[1:3], " or "), "a, b or c")
  expect_equal(str_flatten_comma(letters[1:3]), "a, b, c")
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/stringr/main/tests/testthat/test-glue.R ---
test_that("verify wrapper is functional", {
  expect_equal(as.character(str_glue("a {b}", b = "b")), "a b")

  df <- data.frame(b = "b")
  expect_equal(as.character(str_glue_data(df, "a {b}", b = "b")), "a b")
})

test_that("verify trim is functional", {
  expect_equal(as.character(str_glue("L1\t \n  \tL2")), "L1\t \nL2")

  expect_equal(
    as.character(str_glue("L1\t \n  \tL2", .trim = FALSE)),
    "L1\t \n  \tL2"
  )
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/stringr/main/tests/testthat/test-interp.R ---
test_that("str_interp works with default env", {
  subject <- "statistics"
  number <- 7
  floating <- 6.656

  expect_equal(
    str_interp("A ${subject}. B $[d]{number}. C $[.2f]{floating}."),
    "A statistics. B 7. C 6.66."
  )

  expect_equal(
    str_interp("Pi is approximately $[.5f]{pi}"),
    "Pi is approximately 3.14159"
  )
})

test_that("str_interp works with lists and data frames.", {
  expect_equal(
    str_interp(
      "One value, ${value1}, and then another, ${value2*2}.",
      list(value1 = 10, value2 = 20)
    ),
    "One value, 10, and then another, 40."
  )

  expect_equal(
    str_interp(
      "Values are $[.2f]{max(Sepal.Width)} and $[.2f]{min(Sepal.Width)}.",
      iris
    ),
    "Values are 4.40 and 2.00."
  )
})

test_that("str_interp works with nested expressions", {
  amount <- 1337

  expect_equal(
    str_interp("Works with } nested { braces too: $[.2f]{{{2 + 2}*{amount}}}"),
    "Works with } nested { braces too: 5348.00"
  )
})

test_that("str_interp works in the absense of placeholders", {
  expect_equal(
    str_interp("A quite static string here."),
    "A quite static string here."
  )
})

test_that("str_interp fails when encountering nested placeholders", {
  msg <- "This will never see the light of day"
  num <- 1.2345

  expect_snapshot(error = TRUE, {
    str_interp("${${msg}}")
    str_interp("$[.2f]{${msg}}")
  })
})

test_that("str_interp fails when input is not a character string", {
  expect_snapshot(str_interp(3L), error = TRUE)
})

test_that("str_interp wraps parsing errors", {
  expect_snapshot(str_interp("This is a ${1 +}"), error = TRUE)
})

test_that("str_interp formats list independetly of other placeholders", {
  a_list <- c("item1", "item2", "item3")
  other <- "1"
  extract <- function(text) regmatches(text, regexpr("xx[^x]+xx", text))
  from_list <- extract(str_interp("list: xx${a_list}xx"))
  from_both <- extract(str_interp("list: xx${a_list}xx, and another ${other}"))
  expect_equal(from_list, from_both)
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/stringr/main/tests/testthat/test-length.R ---
test_that("str_length is number of characters", {
  expect_equal(str_length("a"), 1)
  expect_equal(str_length("ab"), 2)
  expect_equal(str_length("abc"), 3)
})

test_that("str_length of missing string is missing", {
  expect_equal(str_length(NA), NA_integer_)
  expect_equal(str_length(c(NA, 1)), c(NA, 1))
  expect_equal(str_length("NA"), 2)
})

test_that("str_length of factor is length of level", {
  expect_equal(str_length(factor("a")), 1)
  expect_equal(str_length(factor("ab")), 2)
  expect_equal(str_length(factor("abc")), 3)
})

test_that("str_width returns display width", {
  x <- c("\u0308", "x", "\U0001f60a")
  expect_equal(str_width(x), c(0, 1, 2))
})

test_that("length/width preserve names", {
  x <- c(C = "3", B = "2", A = "1")
  expect_equal(names(str_length(x)), names(x))
  expect_equal(names(str_width(x)), names(x))
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/stringr/main/tests/testthat/test-locate.R ---
test_that("basic location matching works", {
  expect_equal(str_locate("abc", "a")[1, ], c(start = 1, end = 1))
  expect_equal(str_locate("abc", "b")[1, ], c(start = 2, end = 2))
  expect_equal(str_locate("abc", "c")[1, ], c(start = 3, end = 3))
  expect_equal(str_locate("abc", ".+")[1, ], c(start = 1, end = 3))
})

test_that("uses tidyverse recycling rules", {
  expect_error(str_locate(1:2, 1:3), class = "vctrs_error_incompatible_size")
  expect_error(
    str_locate_all(1:2, 1:3),
    class = "vctrs_error_incompatible_size"
  )
})

test_that("locations are integers", {
  strings <- c("a b c", "d e f")
  expect_true(is.integer(str_locate(strings, "[a-z]")))

  res <- str_locate_all(strings, "[a-z]")[[1]]
  expect_true(is.integer(res))
  expect_true(is.integer(invert_match(res)))
})

test_that("both string and patterns are vectorised", {
  strings <- c("abc", "def")

  locs <- str_locate(strings, "a")
  expect_equal(locs[, "start"], c(1, NA))

  locs <- str_locate(strings, c("a", "d"))
  expect_equal(locs[, "start"], c(1, 1))
  expect_equal(locs[, "end"], c(1, 1))

  locs <- str_locate_all(c("abab"), c("a", "b"))
  expect_equal(locs[[1]][, "start"], c(1, 3))
  expect_equal(locs[[2]][, "start"], c(2, 4))
})

test_that("can use fixed() and coll()", {
  expect_equal(str_locate("x.x", fixed(".")), cbind(start = 2, end = 2))
  expect_equal(
    str_locate_all("x.x.", fixed(".")),
    list(cbind(start = c(2, 4), end = c(2, 4)))
  )

  expect_equal(str_locate("\u0131", turkish_I()), cbind(start = 1, end = 1))
  expect_equal(
    str_locate_all("\u0131I", turkish_I()),
    list(cbind(start = 1:2, end = 1:2))
  )
})

test_that("can use boundaries", {
  expect_equal(
    str_locate(" x  y", ""),
    cbind(start = 1, end = 1)
  )
  expect_equal(
    str_locate_all("abc", ""),
    list(cbind(start = 1:3, end = 1:3))
  )

  expect_equal(
    str_locate(" xy", boundary("word")),
    cbind(start = 2, end = 3)
  )
  expect_equal(
    str_locate_all(" ab  cd", boundary("word")),
    list(cbind(start = c(2, 6), end = c(3, 7)))
  )
})

test_that("str_locate() preserves row names when 1:1 with input", {
  x <- c(C = "3", B = "2", A = "1")
  expect_equal(rownames(str_locate(x, "[0-9]")), names(x))
})

test_that("str_locate_all() preserves names on outer structure", {
  x <- c(C = "3", B = "2", A = "1")
  expect_equal(names(str_locate_all(x, "[0-9]")), names(x))
})

test_that("locate handles vectorised patterns and names", {
  x1 <- c(A = "ab")
  p2 <- c("a", "b")
  expect_null(rownames(str_locate(x1, p2)))
  expect_null(names(str_locate_all(x1, p2)))

  x2 <- c(A = "ab", B = "cd")
  expect_equal(rownames(str_locate(x2, p2)), names(x2))
  expect_equal(names(str_locate_all(x2, p2)), names(x2))
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/stringr/main/tests/testthat/test-match.R ---
set.seed(1410)
num <- matrix(sample(9, 10 * 10, replace = T), ncol = 10)
num_flat <- apply(num, 1, str_c, collapse = "")

phones <- str_c(
  "(",
  num[, 1],
  num[, 2],
  num[, 3],
  ") ",
  num[, 4],
  num[, 5],
  num[, 6],
  " ",
  num[, 7],
  num[, 8],
  num[, 9],
  num[, 10]
)

test_that("empty strings return correct matrix of correct size", {
  skip_if_not_installed("stringi", "1.2.2")

  expect_equal(str_match(NA, "(a)"), matrix(NA_character_, 1, 2))
  expect_equal(str_match(character(), "(a)"), matrix(character(), 0, 2))
})

test_that("no matching cases returns 1 column matrix", {
  res <- str_match(c("a", "b"), ".")

  expect_equal(nrow(res), 2)
  expect_equal(ncol(res), 1)

  expect_equal(res[, 1], c("a", "b"))
})

test_that("single match works when all match", {
  matches <- str_match(phones, "\\(([0-9]{3})\\) ([0-9]{3}) ([0-9]{4})")

  expect_equal(nrow(matches), length(phones))
  expect_equal(ncol(matches), 4)

  expect_equal(matches[, 1], phones)

  matches_flat <- apply(matches[, -1], 1, str_c, collapse = "")
  expect_equal(matches_flat, num_flat)
})

test_that("match returns NA when some inputs don't match", {
  matches <- str_match(
    c(phones, "blah", NA),
    "\\(([0-9]{3})\\) ([0-9]{3}) ([0-9]{4})"
  )

  expect_equal(nrow(matches), length(phones) + 2)
  expect_equal(ncol(matches), 4)

  expect_equal(matches[11, ], rep(NA_character_, 4))
  expect_equal(matches[12, ], rep(NA_character_, 4))
})

test_that("match returns NA when optional group doesn't match", {
  expect_equal(str_match(c("ab", "a"), "(a)(b)?")[, 3], c("b", NA))
})

test_that("match_all returns NA when option group doesn't match", {
  expect_equal(str_match_all("a", "(a)(b)?")[[1]][1, ], c("a", "a", NA))
})

test_that("multiple match works", {
  phones_one <- str_c(phones, collapse = " ")
  multi_match <- str_match_all(
    phones_one,
    "\\(([0-9]{3})\\) ([0-9]{3}) ([0-9]{4})"
  )
  single_matches <- str_match(phones, "\\(([0-9]{3})\\) ([0-9]{3}) ([0-9]{4})")

  expect_equal(multi_match[[1]], single_matches)
})

test_that("match and match_all fail when pattern is not a regex", {
  expect_snapshot(error = TRUE, {
    str_match(phones, fixed("3"))
    str_match_all(phones, coll("9"))
  })
})

test_that("uses tidyverse recycling rules", {
  expect_error(
    str_match(c("a", "b"), c("a", "b", "c")),
    class = "vctrs_error_incompatible_size"
  )
  expect_error(
    str_match_all(c("a", "b"), c("a", "b", "c")),
    class = "vctrs_error_incompatible_size"
  )
})

test_that("match can't use other modifiers", {
  expect_snapshot(error = TRUE, {
    str_match("x", coll("y"))
    str_match_all("x", coll("y"))
  })
})

test_that("str_match() preserves row names when 1:1 with input", {
  x <- c(C = "3", B = "2", A = "1")
  expect_equal(rownames(str_match(x, "([0-9])")), names(x))
})

test_that("str_match_all() preserves names on outer structure", {
  x <- c(C = "3", B = "2", A = "1")
  expect_equal(names(str_match_all(x, "([0-9])")), names(x))
})

test_that("match handles vectorised patterns and names", {
  x1 <- c(A = "ab")
  p2 <- c("a", "b")
  expect_null(rownames(str_match(x1, p2)))
  expect_null(names(str_match_all(x1, p2)))

  x2 <- c(A = "ab", B = "cd")
  expect_equal(rownames(str_match(x2, p2)), names(x2))
  expect_equal(names(str_match_all(x2, p2)), names(x2))
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/stringr/main/tests/testthat/test-modifiers.R ---
test_that("patterns coerced to character", {
  x <- factor("a")

  expect_snapshot({
    . <- regex(x)
    . <- coll(x)
    . <- fixed(x)
  })
})

test_that("useful error message for bad type", {
  expect_snapshot(error = TRUE, {
    type(1:3)
  })
})

test_that("fallback for regex (#433)", {
  expect_equal(type(structure("x", class = "regex")), "regex")
})

test_that("ignore_case sets strength, but can override manually", {
  x1 <- coll("x", strength = 1)
  x2 <- coll("x", ignore_case = TRUE)
  x3 <- coll("x")

  expect_equal(attr(x1, "options")$strength, 1)
  expect_equal(attr(x2, "options")$strength, 2)
  expect_equal(attr(x3, "options")$strength, 3)
})

test_that("boundary has length 1", {
  expect_length(boundary(), 1)
})

test_that("subsetting preserves class and options", {
  x <- regex("a", multiline = TRUE)
  expect_equal(x[], x)
})

test_that("useful errors for NAs", {
  expect_snapshot(error = TRUE, {
    type(NA)
    type(c("a", "b", NA_character_, "c"))
  })
})

test_that("stringr_pattern methods", {
  ex <- coll(c("foo", "bar"))
  expect_true(inherits(ex[1], "stringr_pattern"))
  expect_true(inherits(ex[[1]], "stringr_pattern"))
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/stringr/main/tests/testthat/test-pad.R ---
test_that("long strings are unchanged", {
  lengths <- sample(40:100, 10)
  strings <- vapply(
    lengths,
    function(x) {
      str_c(letters[sample(26, x, replace = T)], collapse = "")
    },
    character(1)
  )

  padded <- str_pad(strings, width = 30)
  expect_equal(str_length(padded), str_length(strings))
})

test_that("directions work for simple case", {
  pad <- function(direction) str_pad("had", direction, width = 10)

  expect_equal(pad("right"), "had       ")
  expect_equal(pad("left"), "       had")
  expect_equal(pad("both"), "   had    ")
})

test_that("padding based of length works", {
  # \u4e2d is a 2-characters-wide Chinese character
  pad <- function(...) str_pad("\u4e2d", ..., side = "both")

  expect_equal(pad(width = 6), "  \u4e2d  ")
  expect_equal(pad(width = 5, use_width = FALSE), "  \u4e2d  ")
})

test_that("uses tidyverse recycling rules", {
  expect_error(
    str_pad(c("a", "b"), 1:3),
    class = "vctrs_error_incompatible_size"
  )
  expect_error(
    str_pad(c("a", "b"), 10, pad = c("a", "b", "c")),
    class = "vctrs_error_incompatible_size"
  )
})

test_that("str_pad() preserves names", {
  x <- c(C = "3", B = "2", A = "1")
  expect_equal(names(str_pad(x, 2, side = "left")), names(x))
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/stringr/main/tests/testthat/test-remove.R ---
test_that("succesfully wraps str_replace_all", {
  expect_equal(str_remove_all("abababa", "ba"), "a")
  expect_equal(str_remove("abababa", "ba"), "ababa")
})

test_that("str_remove() preserves names", {
  x <- c(C = "3", B = "2", A = "1")
  expect_equal(names(str_remove(x, "[0-9]")), names(x))
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/stringr/main/tests/testthat/test-replace.R ---
test_that("basic replacement works", {
  expect_equal(str_replace_all("abababa", "ba", "BA"), "aBABABA")
  expect_equal(str_replace("abababa", "ba", "BA"), "aBAbaba")
})

test_that("can replace multiple matches", {
  x <- c("a1", "b2")
  y <- str_replace_all(x, c("a" = "1", "b" = "2"))
  expect_equal(y, c("11", "22"))
})

test_that("even when lengths differ", {
  x <- c("a1", "b2", "c3")
  y <- str_replace_all(x, c("a" = "1", "b" = "2"))
  expect_equal(y, c("11", "22", "c3"))
})

test_that("multiple matches respects class", {
  x <- c("x", "y")
  y <- str_replace_all(x, regex(c("X" = "a"), ignore_case = TRUE))
  expect_equal(y, c("a", "y"))
})

test_that("replacement must be a string", {
  expect_snapshot(str_replace("x", "x", 1), error = TRUE)
})

test_that("replacement must be a string", {
  expect_equal(str_replace("xyz", "x", NA_character_), NA_character_)
})

test_that("can replace all types of NA values", {
  expect_equal(str_replace_na(NA), "NA")
  expect_equal(str_replace_na(NA_character_), "NA")
  expect_equal(str_replace_na(NA_complex_), "NA")
  expect_equal(str_replace_na(NA_integer_), "NA")
  expect_equal(str_replace_na(NA_real_), "NA")
})

test_that("can use fixed() and coll()", {
  expect_equal(str_replace("x.x", fixed("."), "Y"), "xYx")
  expect_equal(str_replace_all("x.x.", fixed("."), "Y"), "xYxY")

  expect_equal(str_replace("\u0131", turkish_I(), "Y"), "Y")
  expect_equal(str_replace_all("\u0131I", turkish_I(), "Y"), "YY")
})

test_that("can't replace empty/boundary", {
  expect_snapshot(error = TRUE, {
    str_replace("x", "", "")
    str_replace("x", boundary("word"), "")
    str_replace_all("x", "", "")
    str_replace_all("x", boundary("word"), "")
  })
})

# functions ---------------------------------------------------------------

test_that("can replace multiple values", {
  expect_equal(str_replace("abc", "a|c", toupper), "Abc")
  expect_equal(str_replace_all("abc", "a|c", toupper), "AbC")
})

test_that("can use formula", {
  expect_equal(str_replace("abc", "b", ~"x"), "axc")
  expect_equal(str_replace_all("abc", "b", ~"x"), "axc")
})

test_that("replacement can be different length", {
  double <- function(x) str_dup(x, 2)
  expect_equal(str_replace_all("abc", "a|c", double), "aabcc")
})

test_that("replacement is vectorised", {
  x <- c("", "a", "b", "ab", "abc", "cba")
  expect_equal(
    str_replace_all(x, "a|c", ~ toupper(str_dup(.x, 2))),
    c("", "AA", "b", "AAb", "AAbCC", "CCbAA")
  )
})

test_that("is forgiving of 0 matches with paste", {
  x <- c("a", "b", "c")
  expect_equal(str_replace_all(x, "d", ~ paste("x", .x)), x)
})

test_that("useful error if not vectorised correctly", {
  x <- c("a", "b", "c")
  expect_snapshot(
    str_replace_all(x, "a|c", ~ if (length(x) > 1) stop("Bad")),
    error = TRUE
  )
})

test_that("works with no match", {
  expect_equal(str_replace("abc", "z", toupper), "abc")
})

test_that("works with zero length match", {
  expect_equal(str_replace("abc", "$", toupper), "abc")
  expect_equal(str_replace_all("abc", "$|^", ~ rep("X", length(.x))), "XabcX")
})

test_that("replacement function must return correct type/length", {
  expect_snapshot(error = TRUE, {
    str_replace_all("x", "x", ~1)
    str_replace_all("x", "x", ~ c("a", "b"))
  })
})

# fix_replacement ---------------------------------------------------------

test_that("backrefs are correctly translated", {
  expect_equal(str_replace_all("abcde", "(b)(c)(d)", "\\1"), "abe")
  expect_equal(str_replace_all("abcde", "(b)(c)(d)", "\\2"), "ace")
  expect_equal(str_replace_all("abcde", "(b)(c)(d)", "\\3"), "ade")

  # gsub("(b)(c)(d)", "\\0", "abcde", perl=TRUE) gives a0e,
  # in ICU regex $0 refers to the whole pattern match
  expect_equal(str_replace_all("abcde", "(b)(c)(d)", "\\0"), "abcde")

  # gsub("(b)(c)(d)", "\\4", "abcde", perl=TRUE) is legal,
  # in ICU regex this gives an U_INDEX_OUTOFBOUNDS_ERROR
  expect_snapshot(str_replace_all("abcde", "(b)(c)(d)", "\\4"), error = TRUE)

  expect_equal(str_replace_all("abcde", "bcd", "\\\\1"), "a\\1e")

  expect_equal(str_replace_all("a!1!2!b", "!", "$"), "a$1$2$b")
  expect_equal(str_replace("aba", "b", "$"), "a$a")
  expect_equal(str_replace("aba", "b", "$$$"), "a$$$a")
  expect_equal(str_replace("aba", "(b)", "\\1$\\1$\\1"), "ab$b$ba")
  expect_equal(str_replace("aba", "(b)", "\\1$\\\\1$\\1"), "ab$\\1$ba")
  expect_equal(str_replace("aba", "(b)", "\\\\1$\\1$\\\\1"), "a\\1$b$\\1a")
})

test_that("$ are escaped", {
  expect_equal(fix_replacement("$"), "\\$")
  expect_equal(fix_replacement("\\$"), "\\\\$")
})

test_that("\1 converted to $1 etc", {
  expect_equal(fix_replacement("\\1"), "$1")
  expect_equal(fix_replacement("\\9"), "$9")
})

test_that("\\1 left as is", {
  expect_equal(fix_replacement("\\\\1"), "\\\\1")
})

test_that("replace functions preserve names", {
  x <- c(C = "3", B = "2", A = "1")
  expect_equal(names(str_replace(x, "[0-9]", "x")), names(x))
  expect_equal(names(str_replace_all(x, "[0-9]", "x")), names(x))
})

test_that("replace functions handle vectorised patterns and names", {
  x1 <- c(A = "ab")
  p2 <- c("a", "b")
  expect_null(names(str_replace(x1, p2, "x")))
  expect_null(names(str_replace_all(x1, p2, "x")))

  x2 <- c(A = "ab", B = "cd")
  expect_equal(names(str_replace(x2, p2, "x")), names(x2))
  expect_equal(names(str_replace_all(x2, p2, "x")), names(x2))
})

test_that("str_replace_na() preserves names", {
  y <- c(A = NA, B = "x")
  expect_equal(names(str_replace_na(y)), names(y))
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/stringr/main/tests/testthat/test-sort.R ---
test_that("digits can be sorted/ordered as strings or numbers", {
  x <- c("2", "1", "10")

  expect_equal(str_sort(x, numeric = FALSE), c("1", "10", "2"))
  expect_equal(str_sort(x, numeric = TRUE), c("1", "2", "10"))

  expect_equal(str_order(x, numeric = FALSE), c(2, 3, 1))
  expect_equal(str_order(x, numeric = TRUE), c(2, 1, 3))

  expect_equal(str_rank(x, numeric = FALSE), c(3, 1, 2))
  expect_equal(str_rank(x, numeric = TRUE), c(2, 1, 3))
})

test_that("NA can be at beginning or end", {
  x <- c("2", "1", NA, "10")

  na_end <- str_sort(x, numeric = TRUE, na_last = TRUE)
  expect_equal(tail(na_end, 1), NA_character_)

  na_start <- str_sort(x, numeric = TRUE, na_last = FALSE)
  expect_equal(head(na_start, 1), NA_character_)
})

test_that("str_sort() preserves names", {
  x <- c(C = "3", B = "2", A = "1")
  out <- str_sort(x)
  expect_equal(names(out), c("A", "B", "C"))
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/stringr/main/tests/testthat/test-split.R ---
test_that("special cases are correct", {
  expect_equal(str_split(NA, "")[[1]], NA_character_)
  expect_equal(str_split(character(), ""), list())
})

test_that("str_split functions as expected", {
  expect_equal(
    str_split(c("bab", "cac", "dadad"), "a"),
    list(c("b", "b"), c("c", "c"), c("d", "d", "d"))
  )
})

test_that("str_split() can split by special patterns", {
  expect_equal(str_split("ab", ""), list(c("a", "b")))
  expect_equal(
    str_split("this that.", boundary("word")),
    list(c("this", "that"))
  )
  expect_equal(str_split("a-b", fixed("-")), list(c("a", "b")))
  expect_equal(
    str_split("aXb", coll("X", ignore_case = TRUE)),
    list(c("a", "b"))
  )
})

test_that("boundary() can be recycled", {
  expect_equal(str_split(c("x", "y"), boundary()), list("x", "y"))
})

test_that("str_split() can control maximum number of splits", {
  expect_equal(
    str_split(c("a", "a-b"), n = 1, "-"),
    list("a", "a-b")
  )
  expect_equal(
    str_split(c("a", "a-b"), n = 3, "-"),
    list("a", c("a", "b"))
  )
})

test_that("str_split() checks its inputs", {
  expect_snapshot(error = TRUE, {
    str_split(letters[1:3], letters[1:2])
    str_split("x", 1)
    str_split("x", "x", n = 0)
  })
})

test_that("str_split_1 takes string and returns character vector", {
  expect_equal(str_split_1("abc", ""), c("a", "b", "c"))
  expect_snapshot_error(str_split_1(letters, ""))
})

test_that("str_split_fixed pads with empty string", {
  expect_equal(
    str_split_fixed(c("a", "a-b"), "-", 1),
    cbind(c("a", "a-b"))
  )
  expect_equal(
    str_split_fixed(c("a", "a-b"), "-", 2),
    cbind(c("a", "a"), c("", "b"))
  )
  expect_equal(
    str_split_fixed(c("a", "a-b"), "-", 3),
    cbind(c("a", "a"), c("", "b"), c("", ""))
  )
})

test_that("str_split_fixed check its inputs", {
  expect_snapshot(str_split_fixed("x", "x", 0), error = TRUE)
})

# str_split_i -------------------------------------------------------------

test_that("str_split_i can extract from LHS or RHS", {
  expect_equal(str_split_i(c("1-2-3", "4-5"), "-", 1), c("1", "4"))
  expect_equal(str_split_i(c("1-2-3", "4-5"), "-", -1), c("3", "5"))
})

test_that("str_split_i returns NA for absent components", {
  expect_equal(str_split_i(c("a", "b-c"), "-", 2), c(NA, "c"))
  expect_equal(str_split_i(c("a", "b-c"), "-", 3), c(NA_character_, NA))

  expect_equal(str_split_i(c("1-2-3", "4-5"), "-", -3), c("1", NA))
  expect_equal(str_split_i(c("1-2-3", "4-5"), "-", -4), c(NA_character_, NA))
})

test_that("str_split_i check its inputs", {
  expect_snapshot(error = TRUE, {
    str_split_i("x", "x", 0)
    str_split_i("x", "x", 0.5)
  })
})

test_that("split functions preserve names on outer structures", {
  x <- c(C = "3", B = "2", A = "1")
  expect_equal(names(str_split(x, "")), names(x))
  expect_equal(rownames(str_split(x, "", simplify = TRUE)), names(x))
  expect_equal(rownames(str_split_fixed(x, "", 1)), names(x))
})

test_that("str_split_i() preserves names", {
  x <- c(C = "3", B = "2", A = "1")
  expect_equal(names(str_split_i(x, " ", 1)), names(x))
})

test_that("split handles vectorised patterns and names", {
  x1 <- c(A = "ab")
  p2 <- c("a", "b")
  expect_null(names(str_split(x1, p2)))
  expect_null(rownames(str_split(x1, p2, simplify = TRUE)))
  expect_null(rownames(str_split_fixed(x1, p2, 1)))

  x2 <- c(A = "ab", B = "cd")
  expect_equal(names(str_split(x2, p2)), names(x2))
  expect_equal(rownames(str_split(x2, p2, simplify = TRUE)), names(x2))
  expect_equal(rownames(str_split_fixed(x2, p2, 1)), names(x2))
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/stringr/main/tests/testthat/test-sub.R ---
test_that("correct substring extracted", {
  alphabet <- str_c(letters, collapse = "")
  expect_equal(str_sub(alphabet, 1, 3), "abc")
  expect_equal(str_sub(alphabet, 24, 26), "xyz")
})

test_that("can extract multiple substrings", {
  expect_equal(
    str_sub_all(c("abc", "def"), list(c(1, 2), 1), list(c(1, 2), 2)),
    list(c("a", "b"), "de")
  )
})

test_that("arguments expanded to longest", {
  alphabet <- str_c(letters, collapse = "")

  expect_equal(
    str_sub(alphabet, c(1, 24), c(3, 26)),
    c("abc", "xyz")
  )

  expect_equal(
    str_sub(c("abc", "xyz"), 2, 2),
    c("b", "y")
  )
})

test_that("can supply start and end/length as a matrix", {
  x <- c("abc", "def")
  expect_equal(str_sub(x, cbind(1, end = 1)), c("a", "d"))
  expect_equal(str_sub(x, cbind(1, length = 2)), c("ab", "de"))

  expect_equal(
    str_sub_all(x, cbind(c(1, 2), end = c(2, 3))),
    list(c("ab", "bc"), c("de", "ef"))
  )

  str_sub(x, cbind(1, end = 1)) <- c("A", "D")
  expect_equal(x, c("Abc", "Def"))
})

test_that("specifying only end subsets from start", {
  alphabet <- str_c(letters, collapse = "")
  expect_equal(str_sub(alphabet, end = 3), "abc")
})

test_that("specifying only start subsets to end", {
  alphabet <- str_c(letters, collapse = "")
  expect_equal(str_sub(alphabet, 24), "xyz")
})

test_that("specifying -1 as end selects entire string", {
  expect_equal(
    str_sub("ABCDEF", c(4, 5), c(5, -1)),
    c("DE", "EF")
  )

  expect_equal(
    str_sub("ABCDEF", c(4, 5), c(-1, -1)),
    c("DEF", "EF")
  )
})

test_that("negative values select from end", {
  expect_equal(str_sub("ABCDEF", 1, -4), "ABC")
  expect_equal(str_sub("ABCDEF", -3), "DEF")
})

test_that("missing arguments give missing results", {
  expect_equal(str_sub(NA), NA_character_)
  expect_equal(str_sub(NA, 1, 3), NA_character_)
  expect_equal(str_sub(c(NA, "NA"), 1, 3), c(NA, "NA"))

  expect_equal(str_sub("test", NA, NA), NA_character_)
  expect_equal(str_sub(c(NA, "test"), NA, NA), rep(NA_character_, 2))
})

test_that("negative length or out of range gives empty string", {
  expect_equal(str_sub("abc", 2, 1), "")
  expect_equal(str_sub("abc", 4, 5), "")
})

test_that("replacement works", {
  x <- "BBCDEF"
  str_sub(x, 1, 1) <- "A"
  expect_equal(x, "ABCDEF")

  str_sub(x, -1, -1) <- "K"
  expect_equal(x, "ABCDEK")

  str_sub(x, -2, -1) <- "EFGH"
  expect_equal(x, "ABCDEFGH")

  str_sub(x, 2, -2) <- ""
  expect_equal(x, "AH")
})

test_that("replacement with NA works", {
  x <- "BBCDEF"
  str_sub(x, NA) <- "A"
  expect_equal(x, NA_character_)

  x <- "BBCDEF"
  str_sub(x, NA, omit_na = TRUE) <- "A"
  str_sub(x, 1, 1, omit_na = TRUE) <- NA
  expect_equal(x, "BBCDEF")
})

test_that("bad vectorisation gives informative error", {
  x <- "a"
  expect_snapshot(error = TRUE, {
    str_sub(x, 1:2, 1:3)
    str_sub(x, 1:2, 1:2) <- 1:3
  })
})

test_that("str_sub() preserves names", {
  x <- c(C = "3", B = "2", A = "1")
  expect_equal(names(str_sub(x, 1, 1)), names(x))
})

test_that("str_sub_all() preserves names on outer structure", {
  x <- c(C = "3", B = "2", A = "1")
  expect_equal(names(str_sub_all(x, 1, 1)), names(x))
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/stringr/main/tests/testthat/test-subset.R ---
test_that("can subset with regexps", {
  x <- c("a", "b", "c")
  expect_equal(str_subset(x, "a|c"), c("a", "c"))
  expect_equal(str_subset(x, "a|c", negate = TRUE), "b")
})

test_that("can subset with fixed patterns", {
  expect_equal(str_subset(c("i", "I"), fixed("i")), "i")
  expect_equal(
    str_subset(c("i", "I"), fixed("i", ignore_case = TRUE)),
    c("i", "I")
  )

  # negation works
  expect_equal(str_subset(c("i", "I"), fixed("i"), negate = TRUE), "I")
})

test_that("str_which is equivalent to grep", {
  expect_equal(
    str_which(head(letters), "[aeiou]"),
    grep("[aeiou]", head(letters))
  )

  # negation works
  expect_equal(
    str_which(head(letters), "[aeiou]", negate = TRUE),
    grep("[aeiou]", head(letters), invert = TRUE)
  )
})

test_that("can use fixed() and coll()", {
  expect_equal(str_subset(c("x", "."), fixed(".")), ".")
  expect_equal(str_subset(c("i", "\u0131"), turkish_I()), "\u0131")
})

test_that("can't use boundaries", {
  expect_snapshot(error = TRUE, {
    str_subset(c("a", "b c"), "")
    str_subset(c("a", "b c"), boundary())
  })
})

test_that("keep names", {
  fruit <- c(A = "apple", B = "banana", C = "pear", D = "pineapple")
  expect_identical(names(str_subset(fruit, "b")), "B")
  expect_identical(names(str_subset(fruit, "p")), c("A", "C", "D"))
  expect_identical(names(str_subset(fruit, "x")), as.character())
})

test_that("str_subset() preserves names of retained elements", {
  x <- c(C = "3", B = "2", A = "1")
  out <- str_subset(x, "[12]")
  expect_equal(names(out), c("B", "A"))
})

test_that("str_subset() never matches missing values", {
  expect_equal(str_subset(c("a", NA, "b"), "."), c("a", "b"))
  expect_identical(str_subset(NA_character_, "."), character(0))
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/stringr/main/tests/testthat/test-trim.R ---
test_that("trimming removes spaces", {
  expect_equal(str_trim("abc   "), "abc")
  expect_equal(str_trim("   abc"), "abc")
  expect_equal(str_trim("  abc   "), "abc")
})

test_that("trimming removes tabs", {
  expect_equal(str_trim("abc\t"), "abc")
  expect_equal(str_trim("\tabc"), "abc")
  expect_equal(str_trim("\tabc\t"), "abc")
})

test_that("side argument restricts trimming", {
  expect_equal(str_trim(" abc ", "left"), "abc ")
  expect_equal(str_trim(" abc ", "right"), " abc")
})

test_that("str_squish removes excess spaces from all parts of string", {
  expect_equal(str_squish("ab\t\tc\t"), "ab c")
  expect_equal(str_squish("\ta  bc"), "a bc")
  expect_equal(str_squish("\ta\t bc\t"), "a bc")
})

test_that("trimming functions preserve names", {
  x <- c(C = "3", B = "2", A = "1")
  expect_equal(names(str_trim(x)), names(x))
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/stringr/main/tests/testthat/test-trunc.R ---
test_that("NA values in input pass through unchanged", {
  expect_equal(
    str_trunc(NA_character_, width = 5),
    NA_character_
  )
  expect_equal(
    str_trunc(c("foobar", NA), 5),
    c("fo...", NA)
  )
})

test_that("truncations work for all elements of a vector", {
  expect_equal(
    str_trunc(c("abcd", "abcde", "abcdef"), width = 5),
    c("abcd", "abcde", "ab...")
  )
})

test_that("truncations work for all sides", {
  trunc <- function(direction, width) {
    str_trunc(
      "This string is moderately long",
      direction,
      width = width
    )
  }

  expect_equal(trunc("right", 20), "This string is mo...")
  expect_equal(trunc("left", 20), "...s moderately long")
  expect_equal(trunc("center", 20), "This stri...ely long")

  expect_equal(trunc("right", 3), "...")
  expect_equal(trunc("left", 3), "...")
  expect_equal(trunc("center", 3), "...")

  expect_equal(trunc("right", 4), "T...")
  expect_equal(trunc("left", 4), "...g")
  expect_equal(trunc("center", 4), "T...")

  expect_equal(trunc("right", 5), "Th...")
  expect_equal(trunc("left", 5), "...ng")
  expect_equal(trunc("center", 5), "T...g")
})

test_that("does not truncate to a length shorter than elipsis", {
  expect_snapshot(error = TRUE, {
    str_trunc("foobar", 2)
    str_trunc("foobar", 3, ellipsis = "....")
  })
})

test_that("str_trunc correctly snips rhs-of-ellipsis for truncated strings", {
  trunc <- function(width, side) {
    str_trunc(
      c("", "a", "aa", "aaa", "aaaa", "aaaaaaa"),
      width,
      side,
      ellipsis = ".."
    )
  }

  expect_equal(trunc(4, "right"), c("", "a", "aa", "aaa", "aaaa", "aa.."))
  expect_equal(trunc(4, "left"), c("", "a", "aa", "aaa", "aaaa", "..aa"))
  expect_equal(trunc(4, "center"), c("", "a", "aa", "aaa", "aaaa", "a..a"))

  expect_equal(trunc(3, "right"), c("", "a", "aa", "aaa", "a..", "a.."))
  expect_equal(trunc(3, "left"), c("", "a", "aa", "aaa", "..a", "..a"))
  expect_equal(trunc(3, "center"), c("", "a", "aa", "aaa", "a..", "a.."))

  expect_equal(trunc(2, "right"), c("", "a", "aa", "..", "..", ".."))
  expect_equal(trunc(2, "left"), c("", "a", "aa", "..", "..", ".."))
  expect_equal(trunc(2, "center"), c("", "a", "aa", "..", "..", ".."))
})

test_that("str_trunc() preserves names", {
  x <- c(C = "3", B = "2", A = "1")
  expect_equal(names(str_trunc(x, 3)), names(x))
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/stringr/main/tests/testthat/test-unique.R ---
test_that("unique values returned for strings with duplicate values", {
  expect_equal(str_unique(c("a", "a", "a")), "a")
  expect_equal(str_unique(c(NA_character_, NA_character_)), NA_character_)
})

test_that("can ignore case", {
  expect_equal(str_unique(c("a", "A"), ignore_case = TRUE), "a")
})

test_that("str_unique() preserves names of first occurrences", {
  y <- c(A = "a", A2 = "a", B = "b")
  out <- str_unique(y)
  expect_equal(names(out), c("A", "B"))
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/stringr/main/tests/testthat/test-utils.R ---
test_that("keep_names() returns logical flag based on inputs", {
  expect_true(keep_names("a", "x"))
  expect_false(keep_names("a", c("x", "y")))
  expect_true(keep_names(c("a", "b"), "x"))
  expect_true(keep_names(c("a", "b"), c("x", "y")))
})

test_that("copy_names() applies names to vectors if present", {
  expect_equal(
    copy_names(c(A = "a", B = "b"), c("x", "y")),
    c(A = "x", B = "y")
  )

  expect_equal(
    copy_names(c("a", "b"), c("x", "y")),
    c("x", "y")
  )
})

test_that("copy_names() applies rownames to matrices if present", {
  from <- c(A = "a", B = "b")
  to <- matrix(c("x", "y"), nrow = 2)

  expected <- to
  rownames(expected) <- names(from)

  expect_equal(copy_names(from, to), expected)
  expect_equal(copy_names(c("a", "b"), to), to)
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/stringr/main/tests/testthat/test-view.R ---
test_that("results are truncated", {
  expect_snapshot(str_view(words))

  # and can control with option
  local_options(stringr.view_n = 5)
  expect_snapshot(str_view(words))
})

test_that("indices come from original vector", {
  expect_snapshot(str_view(letters, "a|z", match = TRUE))
})

test_that("view highlights all matches", {
  x <- c("abc", "def", "fgh")

  expect_snapshot({
    str_view(x, "[aeiou]")
    str_view(x, "d|e")
  })
})

test_that("view highlights whitespace (except a space/nl)", {
  x <- c(" ", "\u00A0", "\n", "\t")
  expect_snapshot({
    str_view(x)

    "or can instead use escapes"
    str_view(x, use_escapes = TRUE)
  })
})

test_that("view displays message for empty vectors", {
  expect_snapshot(str_view(character()))
})

test_that("match argument controls what is shown", {
  x <- c("abc", "def", "fgh", NA)
  out <- str_view(x, "d|e", match = NA)
  expect_length(out, 4)

  out <- str_view(x, "d|e", match = TRUE)
  expect_length(out, 1)

  out <- str_view(x, "d|e", match = FALSE)
  expect_length(out, 3)
})

test_that("can match across lines", {
  local_reproducible_output(crayon = TRUE)
  expect_snapshot(str_view("a\nb\nbbb\nc", "(b|\n)+"))
})

test_that("vectorised over pattern", {
  x <- str_view("a", c("a", "b"), match = NA)
  expect_equal(length(x), 2)
})

test_that("[ preserves class", {
  x <- str_view(letters)
  expect_s3_class(x[], "stringr_view")
})

test_that("str_view_all() is deprecated", {
  expect_snapshot(str_view_all("abc", "a|b"))
})

test_that("html mode continues to work", {
  skip_if_not_installed("htmltools")
  skip_if_not_installed("htmlwidgets")

  x <- c("abc", "def", "fgh")
  expect_snapshot({
    str_view(x, "[aeiou]", html = TRUE)$x$html
    str_view(x, "d|e", html = TRUE)$x$html
  })

  # can use escapes
  x <- c(" ", "\u00A0", "\n")
  expect_snapshot({
    str_view(x, html = TRUE, use_escapes = TRUE)$x$html
  })
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/stringr/main/tests/testthat/test-word.R ---
test_that("word extraction", {
  expect_equal("walk", word("walk the moon"))
  expect_equal("walk", word("walk the moon", 1))
  expect_equal("moon", word("walk the moon", 3))
  expect_equal("the moon", word("walk the moon", 2, 3))
})

test_that("words past end return NA", {
  expect_equal(word("a b c", 4), NA_character_)
})

test_that("negative parameters", {
  expect_equal("moon", word("walk the moon", -1, -1))
  expect_equal("walk the moon", word("walk the moon", -3, -1))
  expect_equal("walk the moon", word("walk the moon", -5, -1))
})

test_that("word() preserves names", {
  x <- c(C = "3", B = "2", A = "1")
  expect_equal(names(word(x, 1)), names(x))
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/stringr/main/tests/testthat/test-wrap.R ---
test_that("wrapping removes spaces", {
  expect_equal(str_wrap(""), "")
  expect_equal(str_wrap(" "), "")
  expect_equal(str_wrap("  a  "), "a")
})

test_that("wrapping with width of 0 puts each word on own line", {
  n_returns <- letters %>%
    str_c(collapse = " ") %>%
    str_wrap(0) %>%
    str_count("\n")
  expect_equal(n_returns, length(letters) - 1)
})

test_that("wrapping at whitespace break works", {
  expect_equal(str_wrap("a/b", width = 0, whitespace_only = TRUE), "a/b")
  expect_equal(str_wrap("a/b", width = 0, whitespace_only = FALSE), "a/\nb")
})

test_that("str_wrap() preserves names", {
  x <- c(C = "3", B = "2", A = "1")
  expect_equal(names(str_wrap(x)), names(x))
})
