

# --- FILE: https://raw.githubusercontent.com/rorynolan/strex/master/R/after.R ---
#' Extract text before or after `n`th occurrence of pattern.
#'
#' Extract the part of a string which is before or after the `n`th occurrence of
#' a specified pattern, vectorized over the string.
#'
#' \itemize{ \item `str_after_first(...)` is just `str_after_nth(..., n = 1)`.
#' \item `str_after_last(...)` is just `str_after_nth(..., n = -1)`. \item
#' `str_before_first(...)` is just `str_before_nth(..., n = 1)`. \item
#' `str_before_last(...)` is just `str_before_nth(..., n = -1)`. }
#'
#' @param string A character vector.
#' @param pattern The pattern to look for.
#'
#'   The default interpretation is a regular expression, as described in
#'   [stringi::about_search_regex].
#'
#'   To match a without regular expression (i.e. as a human would), use
#'   [coll()][stringr::coll]. For details see [stringr::regex()].
#'
#' @param n A vector of integerish values. Must be either length 1 or
#'   have length equal to the length of `string`. Negative indices count from
#'   the back: while `n = 1` and `n = 2` correspond to first and second, `n =
#'   -1` and `n = -2` correspond to last and second-last. `n = 0` will return
#'   `NA`.
#'
#' @return A character vector.
#' @examples
#' string <- "abxxcdxxdexxfgxxh"
#' str_after_nth(string, "xx", 3)
#' str_before_nth(string, "e", 1:2)
#' str_before_nth(string, "xx", -3)
#' str_before_nth(string, ".", -3)
#' str_before_nth(rep(string, 2), "..x", -3)
#' str_before_first(string, "d")
#' str_before_last(string, "x")
#' string <- c("abc", "xyz.zyx")
#' str_after_first(string, ".") # using regex
#' str_after_first(string, coll(".")) # using human matching
#' str_after_last(c("xy", "xz"), "x")
#' @name before-and-after
#' @family bisectors
NULL

#' @rdname before-and-after
#' @export
str_after_nth <- function(string, pattern, n) {
  if (is_l0_char(string)) {
    return(character())
  }
  verify_string_pattern_n(string, pattern, n)
  nth_instance_indices <- str_locate_nth(string, pattern, n)
  str_sub(string, nth_instance_indices[, "end"] + 1)
}

#' @rdname before-and-after
#' @export
str_after_first <- function(string, pattern) {
  str_after_nth(string, pattern, n = 1)
}

#' @rdname before-and-after
#' @export
str_after_last <- function(string, pattern) {
  str_after_nth(string, pattern, n = -1)
}


# --- FILE: https://raw.githubusercontent.com/rorynolan/strex/master/R/alphord.R ---
#' Make string numbers comply with alphabetical order.
#'
#' If strings are numbered, their numbers may not *comply* with alphabetical
#' order, e.g. "abc2" comes after `"abc10"` in alphabetical order. We might (for
#' whatever reason) wish to change them such that they come in the order *that
#' we would like*. This function alters the strings such that they comply with
#' alphabetical order, so here `"abc2"` would be renamed to "abc02". It works on
#' file names with more than one number in them e.g. `"abc01def3"` (a string
#' with 2 numbers). All the strings in the character vector `string` must have
#' the same number of numbers, and the non-number bits must be the same.
#'
#' @inheritParams str_after_nth
#'
#' @return A character vector.
#'
#' @examples
#' string <- paste0("abc", 1:12)
#' print(string)
#' str_alphord_nums(string)
#' str_alphord_nums(c("abc9def55", "abc10def7"))
#' str_alphord_nums(c("01abc9def55", "5abc10def777", "99abc4def4"))
#' str_alphord_nums(1:10)
#' \dontrun{
#' str_alphord_nums(c("abc9def55", "abc10xyz7")) # error
#' }
#'
#' @family alphorderers
#' @export
str_alphord_nums <- function(string) {
  if (is_l0_char(string)) {
    return(character())
  }
  checkmate::assert(
    checkmate::check_numeric(string, min.len = 1),
    checkmate::check_character(string, min.len = 1)
  )
  if (is.numeric(string)) string <- as.character(string)
  have_nums <- str_detect(string, "\\d")
  if (!all(have_nums)) {
    bad_index <- match(F, have_nums)
    rlang::abort(
      c("Some of the input strings have no numbers in them.",
        x = str_glue(
          "The first bad string is string number {bad_index}, ",
          "which is '{string[bad_index]}'."
        )
      )
    )
  }
  non_nums <- str_extract_non_numerics(string)
  if (length(unique(non_nums)) > 1) {
    bad_index <- 2
    while (isTRUE(all.equal(non_nums[[1]], non_nums[[bad_index]]))) {
      bad_index <- bad_index + 1
    }
    rlang::abort(
      c("The non-number bits of every string must be the same.",
        i = str_glue(
          "The first pair of strings with different non-",
          "number bits are strings 1 and {bad_index}."
        ),
        x = str_glue("They are '{string[1]}' and '{string[bad_index]}'.")
      )
    )
  }
  nums <- str_extract_numbers(string, leave_as_string = TRUE)
  nums_lengths <- lengths(nums)
  if (length(unique(nums_lengths)) > 1) {
    bad_index <- match(F, nums_lengths == nums_lengths[1])
    rlang::abort(
      c("The strings must all have the same number of numbers.",
        x = str_glue(
          "Your string number 1 \"{string[1]}\" has ",
          "{nums_lengths[1]} numbers, whereas your string number ",
          "{bad_index} '{string[bad_index]}' has ",
          "{nums_lengths[bad_index]} numbers."
        )
      )
    )
  }
  nums <- simplify2array(nums)
  if (!is.matrix(nums)) nums <- t(nums)
  ncn <- nums %>%
    {
      array(str_length(.), dim = dim(.))
    }
  max_lengths <- int_mat_row_maxs(ncn)
  min_length <- min(ncn)
  to_prefix <- rep("0", max(max_lengths) - min_length) %>% str_c(collapse = "")
  nums <- str_c(to_prefix, nums)
  starts <- -rep(max_lengths, ncol(ncn))
  nums <- str_sub(nums, starts, -1) %>%
    split(rep(seq_len(ncol(ncn)), each = nrow(ncn)))
  num_first <- str_elem(string, 1) %>% str_can_be_numeric()
  if (length(unique(num_first)) > 1) {
    bad_index <- match(!num_first[1], num_first)
    rlang::abort(
      c(
        paste(
          "It should either be the case that all strings start with",
          "numbers or that none of them do."
        ),
        x = str_glue(
          " String number 1 '{string[1]}' ",
          "{ifelse(num_first[1], 'does', 'does not')} ",
          "start with a number whereas ",
          "string number {bad_index} '{string[bad_index]}' ",
          "{ifelse(num_first[1], 'does not', 'does')} ",
          "start with a number."
        )
      )
    )
  }
  if (num_first[1]) {
    interleaves <- interleave_chr_lsts(nums, non_nums)
  } else {
    interleaves <- interleave_chr_lsts(non_nums, nums)
  }
  stringi::stri_paste_list(interleaves)
}


# --- FILE: https://raw.githubusercontent.com/rorynolan/strex/master/R/arg-match.R ---
#' Argument Matching.
#'
#' Match `arg` against a series of candidate `choices`. `arg` _matches_ an
#' element of `choices` if `arg` is a prefix of that element.
#'
#' `ERROR`s are thrown when a match is not made and where the match is
#' ambiguous. However, sometimes ambiguities are inevitable. Consider the case
#' where `choices = c("ab", "abc")`, then there's no way to choose `"ab"`
#' because `"ab"` is a prefix for `"ab"` and `"abc"`. If this is the case, you
#' need to provide a full match, i.e. using `arg = "ab"` will get you `"ab"`
#' without an error, however `arg = "a"` will throw an ambiguity error.
#'
#' When `choices` is `NULL`, the `choices` are obtained from a default setting
#' for the formal argument `arg` of the function from which `str_match_arg` was
#' called. This is consistent with `base::match.arg()`. See the examples for
#' details.
#'
#' When `arg` and `choices` are identical and `several_ok = FALSE`, the first
#' element of `choices` is returned. This is consistent with
#' `base::match.arg()`.
#'
#' This function inspired by `RSAGA::match.arg.ext()`. Its behaviour is almost
#' identical (the difference is that `RSAGA::match.arg.ext(..., ignore.case =
#' TRUE)` always returns in all lower case; `strex::match_arg(..., ignore_case =
#' TRUE)` ignores case while matching but returns the element of `choices` in
#' its original case). `RSAGA` is a heavy package to depend upon so
#' `strex::match_arg()` is handy for package developers.
#'
#' This function is designed to be used inside of other functions. It's fine to
#' use it for other purposes, but the error messages might be a bit weird.
#'
#' @param arg A character vector (of length one unless `several_ok = TRUE`).
#' @param choices A character vector of candidate values.
#' @param index Return the index of the match rather than the match itself?
#' @param several_ok Allow `arg` to have length greater than one to match
#'   several arguments at once?
#' @param ignore_case Ignore case while matching. If this is `TRUE`, the
#'   returned value is the matched element of `choices` (with its original
#'   casing).
#'
#' @examples
#' choices <- c("Apples", "Pears", "Bananas", "Oranges")
#' match_arg("A", choices)
#' match_arg("B", choices, index = TRUE)
#' match_arg(c("a", "b"), choices, several_ok = TRUE, ignore_case = TRUE)
#' match_arg(c("b", "a"), choices,
#'   ignore_case = TRUE, index = TRUE,
#'   several_ok = TRUE
#' )
#' myword <- function(w = c("abacus", "baseball", "candy")) {
#'   w <- match_arg(w)
#'   w
#' }
#' myword("b")
#' myword()
#' myword <- function(w = c("abacus", "baseball", "candy")) {
#'   w <- match_arg(w, several_ok = TRUE)
#'   w
#' }
#' myword("c")
#' myword()
#' @family argument matchers
#' @export
str_match_arg <- function(arg, choices = NULL, index = FALSE,
                          several_ok = FALSE, ignore_case = FALSE) {
  if (is.null(choices)) {
    arg_sym <- rlang::enexpr(arg)
    null_choice_err <- FALSE
    if (!rlang::is_symbol(arg_sym)) null_choice_err <- TRUE
    if (!null_choice_err) {
      formal_args <- formals(sys.function(sys_p <- sys.parent()))
      arg_sym <- as.character(arg_sym)
      default_arg_names <- formal_args %>%
        {
          names(.)[as.logical(str_length(as.character(.)))]
        }
      if (arg_sym %in% default_arg_names) {
        choices <- eval(formal_args[[arg_sym]], envir = sys.frame(sys_p))
        if (is.character(choices)) {
          return(
            str_match_arg(arg,
              choices = choices,
              index = index,
              several_ok = several_ok,
              ignore_case = ignore_case
            )
          )
        } else {
          null_choice_err <- TRUE
        }
      } else {
        null_choice_err <- TRUE
      }
    }
    if (null_choice_err) {
      rlang::abort(
        c(
          "You used `match_arg()` without specifying a `choices` argument.",
          i = paste(
            "The only way to do this is from another function where",
            "the `arg` has a default setting.",
            "This is the same as `base::match.arg()`."
          ),
          i = paste("See the man page for `match_arg()`."),
          i = paste(
            "See the vignette on argument matching:",
            "enter `vignette('argument-matching', package = 'strex')`",
            "at the R console."
          )
        )
      )
    }
  }
  arg_sym <- tryCatch(as.character(rlang::ensym(arg)),
    error = function(e) NA_character_
  )
  str_match_arg_basic(
    arg = arg,
    choices = choices,
    index = index,
    several_ok = several_ok,
    ignore_case = ignore_case,
    arg_sym = arg_sym
  )
}

#' @rdname str_match_arg
#' @export
match_arg <- str_match_arg

str_match_arg_basic <- function(arg, choices, index, several_ok, ignore_case,
                                arg_sym) {
  checkmate::assert_character(arg, min.len = 1)
  checkmate::assert_character(choices, min.len = 1)
  checkmate::assert_flag(index)
  checkmate::assert_flag(several_ok)
  checkmate::assert_flag(ignore_case)
  checkmate::assert_character(arg, min.len = 1)
  checkmate::assert_string(arg_sym, na.ok = TRUE)
  arg_sym <- ifelse(is.na(arg_sym), "arg", arg_sym)
  first_dup <- anyDuplicated(choices)
  if (first_dup) {
    rlang::abort(
      c(
        "`choices` must not have duplicate elements.",
        str_glue(
          "Element {first_dup} of your `choices`, ",
          "'{choices[first_dup]}', is a duplicate."
        )
      )
    )
  }
  if (ignore_case) {
    lower_choices <- str_to_lower(choices)
    first_dup <- anyDuplicated(lower_choices)
    if (first_dup) {
      dupair_indices <- c(
        match(lower_choices[first_dup], lower_choices),
        first_dup
      )
      rlang::abort(
        c(
          "`choices` must not have duplicate elements.",
          i = str_glue_data(
            list(
              dupair = choices[dupair_indices],
              dupair_indices = dupair_indices
            ),
            "Since you have set `ignore_case = TRUE`, elements ",
            "{dupair_indices[1]} and {dupair_indices[2]} of your `choices`, ",
            "('{dupair[1]}' and '{dupair[2]}') are effectively duplicates."
          )
        )
      )
    }
  }
  arg_len <- length(arg)
  if (!several_ok && arg_len > 1) {
    if (isTRUE(all.equal(arg, choices))) {
      return(choices[[1]])
    }
    rlang::abort(
      c(
        str_glue("`{arg_sym}` must have length 1."),
        x = str_glue("Your `{arg_sym}` has length {arg_len}."),
        i = str_glue(
          "To use an `{arg_sym}` with length greater than one, ",
          "use `several_ok = TRUE`."
        )
      )
    )
  }
  if (ignore_case) {
    indices <- match_arg_index(str_to_lower(arg), lower_choices)
  } else {
    indices <- match_arg_index(arg, choices)
  }
  bads <- indices <= 0
  if (any(bads)) {
    first_bad_index <- match(TRUE, bads)
    first_bad_type <- indices[first_bad_index]
    stopifnot(first_bad_type %in% (-seq_len(2))) # should never happen
    if (first_bad_type == -1) {
      rlang::abort(
        c(
          str_glue(
            "`{arg_sym}` must be a prefix of exactly one element of ",
            "`choices`."
          ),
          i = str_glue(
            "Your{ifelse(length(choices) > 50, ' first 50 ', ' ')}`choices` ",
            "are {paste(head(choices, 50), collapse = ', ')}."
          ),
          x = str_glue(
            "Your `{arg_sym}`, '{arg[first_bad_index]}', is not a ",
            "prefix of any of your `choices`."
          )
        )
      )
    } else {
      if (ignore_case) {
        ambigs <- choices[
          str_starts(
            tolower(choices),
            str_c("^", tolower(arg)[first_bad_index])
          )
        ]
      } else {
        ambigs <- str_subset(choices, str_c("^", arg[first_bad_index]))
      }
      rlang::abort(
        c(
          str_glue(
            "`arg` must be a prefix of exactly one element of ",
            "`choices`."
          ),
          x = str_glue(
            "Your `arg`, '{arg[first_bad_index]}', is a ",
            "prefix of two or more elements of `choices`."
          ),
          i = str_glue(
            "The first two of these are ",
            "'{ambigs[1]}' and '{ambigs[2]}'."
          )
        )
      )
    }
  }
  if (index) {
    return(indices)
  }
  choices[indices]
}


# --- FILE: https://raw.githubusercontent.com/rorynolan/strex/master/R/before.R ---
#' @rdname before-and-after
#' @export
str_before_nth <- function(string, pattern, n) {
  if (is_l0_char(string)) {
    return(character())
  }
  verify_string_pattern_n(string, pattern, n)
  nth_instance_indices <- str_locate_nth(string, pattern, n)
  str_sub(string, 1, nth_instance_indices[, "start"] - 1)
}

#' @rdname before-and-after
#' @export
str_before_first <- function(string, pattern) {
  str_before_nth(string = string, pattern = pattern, n = 1)
}

#' @rdname before-and-after
#' @export
str_before_last <- function(string, pattern) {
  str_before_nth(string = string, pattern = pattern, n = -1)
}

#' Extract the part of a string before the last period.
#'
#' This is usually used to get the part of a file name that doesn't include the
#' file extension. It is vectorized over `string`. If there is no period in
#' `string`, the input is returned.
#'
#' @inheritParams before-and-after
#'
#' @return A character vector.
#'
#' @examples
#' str_before_last_dot(c("spreadsheet1.csv", "doc2.doc", ".R"))
#' @family bisectors
#' @export
str_before_last_dot <- function(string) {
  if (is_l0_char(string)) {
    return(character())
  }
  checkmate::assert_character(string)
  out <- tools::file_path_sans_ext(string)
  out[(string == out) & (str_elem(out, 1) == ".")] <- ""
  out
}


# --- FILE: https://raw.githubusercontent.com/rorynolan/strex/master/R/c-wrappers.R ---
#' Convert a list of character vectors to a list of numeric vectors.
#'
#' This is the same as doing `lapply(x, as.numeric)` but faster, and it allows
#' for comma handling.
#'
#' @param x A list of character vectors.
#' @inheritParams str_extract_numbers
#'
#' @return A list of numeric vectors.
#'
#' @examples
#' lst_chr_to_dbl(list(c("1", "2,000"), c("1.3", "2.2", "5.9")), big_mark_regex = "[,]")
#' @noRd
lst_chr_to_dbl <- function(x, big_mark_regex = "") {
  checkmate::assert_list(x, types = "character")
  checkmate::assert_character(big_mark_regex)
  big_mark_regex <- ifelse(big_mark_regex == "[]", "", big_mark_regex)
  assert_compatible_lengths(x, big_mark_regex)
  .Call(C_lst_chr_to_dbl, x, big_mark_regex)
}

#' Get the indices of the `choices` that are matches for `arg`.
#'
#' @inheritParams match_arg
#'
#' @return A numeric vector.
#'
#' @examples
#' match_arg_index("ab", c("book", "abacus", "pencil"))
#' @noRd
match_arg_index <- function(arg, choices) {
  checkmate::assert_character(arg, min.chars = 1, min.len = 1)
  checkmate::assert_character(choices, min.chars = 1, min.len = 1)
  .Call(C_match_arg_index, arg, choices)
}

#' Make a vector where every other element is from `x` or `y`.
#'
#' The lengths of `x` and `y` must differ by at most 1.
#' If `x` and `y` have the same length, the first element of `x` will be first
#' in the result.
#'
#' @param x,y Character vectors.
#'
#' @return A character vector.
#'
#' @examples
#' interleave_chr_vecs(c("a", "b"), c("x", "y"))
#' interleave_chr_vecs(c("a", "b", "c"), c("x", "y"))
#' interleave_chr_vecs(c("a", "b"), c("x", "y", "z"))
#' @noRd
interleave_chr_vecs <- function(x, y) {
  checkmate::assert_character(x)
  checkmate::assert_character(y)
  if (abs(length(x) - length(y)) > 1) {
    rlang::abort(
      c(
        "`x` and `y` must have lengths that differ by at most 1.",
        x = str_glue("`x` has length {length(x)}."),
        x = str_glue("`y` has length {length(y)}.")
      )
    )
  }
  .Call(C_interleave_chr_vecs, x, y)
}

#' List version of [interleave_chr_vecs].
#'
#' This is a C version of `map2(x, y, ~interleave_chr_vecs(.x, .y))`.
#'
#' @param x,y Lists of character vectors. `x` and `y` must be of equal length.
#'
#' @return A list.
#'
#' @examples
#' interleave_chr_lsts(
#'   list(c("a", "b"), c("a", "b", "c"), c("a", "b")),
#'   list(c("x", "y"), c("x", "y"), c("x", "y", "z"))
#' )
#' @noRd
interleave_chr_lsts <- function(x, y) {
  checkmate::assert_list(x, types = "character")
  checkmate::assert_list(y, types = "character")
  .Call(C_interleave_chr_lsts, x, y)
}

#' Remove empty strings from a character vector.
#'
#' Empty strings are length zero strings i.e. `""`.
#'
#' @param chr_vec A character vector.
#'
#' @return A character vector.
#'
#' @examples
#' chr_vec_remove_empties(c("", "a", "", "", "b", "c", ""))
#' @noRd
chr_vec_remove_empties <- function(chr_vec) {
  checkmate::assert_character(chr_vec)
  .Call(C_chr_vec_remove_empties, chr_vec)
}

#' Remove empty strings from a character vector.
#'
#' Empty strings are length zero strings i.e. `""`.
#'
#' @param chr_lst A list of character vectors.
#'
#' @return A list of character vectors.
#'
#' @examples
#' chr_lst_remove_empties(
#'   list(
#'     c("", "a", "", "", "b", "c", ""),
#'     c("", ""),
#'     c("xy")
#'   )
#' )
#' @noRd
chr_lst_remove_empties <- function(chr_lst) {
  checkmate::assert_list(chr_lst, types = "character")
  .Call(C_chr_lst_remove_empties, chr_lst)
}

#' Get the `n`th element of each of each element of a list of character vectors.
#'
#' @param chr_lst A list of character vectors.
#' @param n An integer vector.
#'
#' @return A character vector.
#'
#' @examples
#' chr_lst_nth_elems(list(c("a", "b"), c("x", "y", "z")), 2:3)
#' @noRd
chr_lst_nth_elems <- function(chr_lst, n) {
  checkmate::assert_list(chr_lst, types = "character")
  checkmate::assert_integerish(n)
  assert_compatible_lengths(chr_lst, n)
  .Call(C_chr_lst_nth_elems, chr_lst, as.integer(n))
}

#' Get the `n`th element of each of each element of a list of real vectors.
#'
#' @param dbl_lst A list of real vectors.
#' @param n An integer vector.
#'
#' @return A character vector.
#'
#' @examples
#' dbl_lst_nth_elems(list(c(1.2, 2.3, 3.4), c(6.7, 8.9)), 2)
#' @noRd
dbl_lst_nth_elems <- function(dbl_lst, n) {
  checkmate::assert_list(dbl_lst, types = "double")
  checkmate::assert_integerish(n)
  assert_compatible_lengths(dbl_lst, n)
  .Call(C_dbl_lst_nth_elems, dbl_lst, as.integer(n))
}

#' Get the `n`th column of each integer matrix in a list.
#'
#' @param int_mat_lst A list of integer matrices.
#' @param n An integer vector.
#'
#' @return A list.
#'
#' @examples
#' int_mat_lst_nth_cols(
#'   list(
#'     matrix(1:4, nrow = 2),
#'     matrix(9:1, nrow = 3)
#'   ),
#'   c(2, 3)
#' )
#' @noRd
int_mat_lst_nth_cols <- function(int_mat_lst, n) {
  checkmate::assert_list(int_mat_lst, "integer")
  checkmate::assert_list(int_mat_lst, "matrix")
  checkmate::assert_integerish(n)
  assert_compatible_lengths(int_mat_lst, n)
  .Call(C_int_mat_lst_nth_cols, int_mat_lst, as.integer(n))
}

#' Get the `n`th row of each integer matrix in a list.
#'
#' @param int_mat_lst A list of integer matrices.
#' @param n An integer vector.
#'
#' @return A list.
#'
#' @examples
#' int_mat_lst_nth_rows(
#'   list(
#'     matrix(1:4, nrow = 2),
#'     matrix(9:1, nrow = 3)
#'   ),
#'   c(2, 3)
#' )
#' @noRd
int_mat_lst_nth_rows <- function(int_mat_lst, n) {
  checkmate::assert_list(int_mat_lst, "integer")
  checkmate::assert_list(int_mat_lst, "matrix")
  checkmate::assert_integerish(n)
  assert_compatible_lengths(int_mat_lst, n)
  .Call(C_int_mat_lst_nth_rows, int_mat_lst, as.integer(n))
}

#' [cbind()] the elements of a list of integer vectors.
#'
#' @param int_lst A list of equal-length integer vectors.
#'
#' @return An integer matrix.
#'
#' @examples
#' int_lst_cbind(list(1:4, 6:9))
#' @noRd
int_lst_cbind <- function(int_lst) {
  checkmate::assert_list(int_lst, types = "integer")
  assert_lst_elems_common_length(int_lst)
  .Call(C_int_lst_cbind, int_lst)
}

#' [rbind()] the elements of a list of integer vectors.
#'
#' @param int_lst A list of equal-length integer vectors.
#'
#' @return An integer matrix.
#'
#' @examples
#' int_lst_rbind(list(1:4, 6:9))
#' @noRd
int_lst_rbind <- function(int_lst) {
  checkmate::assert_list(int_lst, types = "integer")
  assert_lst_elems_common_length(int_lst)
  .Call(C_int_lst_rbind, int_lst)
}

#' [cbind()] the `n`th row of each integer matrix in a list.
#'
#' @param int_mat_lst A list of integer matrices.
#' @param n An integer vector.
#'
#' @return A list.
#'
#' @examples
#' int_mat_lst_cbind_nth_rows(
#'   list(
#'     matrix(1:4, nrow = 2),
#'     matrix(6:9, nrow = 2)
#'   ),
#'   c(1, 2)
#' )
#' @noRd
int_mat_lst_cbind_nth_rows <- function(int_mat_lst, n) {
  checkmate::assert_list(int_mat_lst, "integer")
  checkmate::assert_list(int_mat_lst, "matrix")
  checkmate::assert_integerish(n)
  assert_compatible_lengths(int_mat_lst, n)
  .Call(C_int_mat_lst_cbind_nth_rows, int_mat_lst, as.integer(n))
}

#' [cbind()] the `n`th column of each integer matrix in a list.
#'
#' @param int_mat_lst A list of integer matrices.
#' @param n An integer vector.
#'
#' @return A list.
#'
#' @examples
#' int_mat_lst_cbind_nth_cols(
#'   list(
#'     matrix(1:4, nrow = 2),
#'     matrix(6:9, nrow = 2)
#'   ),
#'   c(1, 2)
#' )
#' @noRd
int_mat_lst_cbind_nth_cols <- function(int_mat_lst, n) {
  checkmate::assert_list(int_mat_lst, "integer")
  checkmate::assert_list(int_mat_lst, "matrix")
  checkmate::assert_integerish(n)
  assert_compatible_lengths(int_mat_lst, n)
  .Call(C_int_mat_lst_cbind_nth_cols, int_mat_lst, as.integer(n))
}

#' [rbind()] the `n`th column of each integer matrix in a list.
#'
#' @param int_mat_lst A list of integer matrices.
#' @param n An integer vector.
#'
#' @return A list.
#'
#' @examples
#' int_mat_lst_rbind_nth_cols(
#'   list(
#'     matrix(1:4, nrow = 2),
#'     matrix(6:9, nrow = 2)
#'   ),
#'   c(1, 2)
#' )
#' @noRd
int_mat_lst_rbind_nth_cols <- function(int_mat_lst, n) {
  checkmate::assert_list(int_mat_lst, "integer")
  checkmate::assert_list(int_mat_lst, "matrix")
  checkmate::assert_integerish(n)
  assert_compatible_lengths(int_mat_lst, n)
  .Call(C_int_mat_lst_rbind_nth_cols, int_mat_lst, as.integer(n))
}

#' [rbind()] the `n`th row of each integer matrix in a list.
#'
#' @param int_mat_lst A list of integer matrices.
#' @param n An integer vector.
#'
#' @return A list.
#'
#' @examples
#' int_mat_lst_rbind_nth_rows(
#'   list(
#'     matrix(1:4, nrow = 2),
#'     matrix(6:9, nrow = 2)
#'   ),
#'   c(1, 2)
#' )
#' @noRd
int_mat_lst_rbind_nth_rows <- function(int_mat_lst, n) {
  checkmate::assert_list(int_mat_lst, "integer")
  checkmate::assert_list(int_mat_lst, "matrix")
  checkmate::assert_integerish(n)
  assert_compatible_lengths(int_mat_lst, n)
  .Call(C_int_mat_lst_rbind_nth_rows, int_mat_lst, as.integer(n))
}

#' Fullocate each element of a list.
#'
#' Fullocation is filling in the output of a call to [stringr::str_locate()].The
#' result is a set of closed intervals whose union spans the whole interval.
#' Looking at the examples is the best way to understand this function.
#'
#' This function does not handle bad input well. The first argument must be the
#' output of a call to [stringr::str_locate_all()].
#'
#' @param int_mat_lst A list of integer matrices. The return of a call to
#'   [stringr::str_locate_all()].
#' @param start,end Integer vectors of length 1 or the same length as
#'   `int_mat_lst`. Start and end points for the fullocation interval.
#'
#' @return A list of integer matrices.
#'
#' @examples
#' int_mat_lst <- list(
#'   rbind(c(2L, 5L), 7:8),
#'   rbind(c(5L, 6L), c(7L, 10L), c(20L, 30L))
#' )
#' lst_fullocate(int_mat_lst, start = c(1, 5), end = c(10, 50))
#' @noRd
lst_fullocate <- function(int_mat_lst, start, end) {
  checkmate::assert_list(int_mat_lst, types = "integer")
  checkmate::assert_list(int_mat_lst, types = "matrix")
  start <- checkmate::assert_integerish(start, coerce = TRUE)
  end <- checkmate::assert_integerish(end, coerce = TRUE)
  assert_compatible_lengths(int_mat_lst, start)
  assert_compatible_lengths(int_mat_lst, end)
  .Call(C_lst_fullocate, int_mat_lst, start, end)
}

#' Do all elements of a list have the same length?
#'
#' @param lst A list.
#' @param l A double. The length of the list.
#'
#' @return A flag.
#'
#' @noRd
lst_elems_common_length <- function(lst, l = as.double(length(lst))) {
  checkmate::assert_list(lst)
  checkmate::assert_number(l)
  .Call(C_lst_elems_common_length, lst, as.double(l))
}

#' [rbind()] the elements of a pairlist of integer vectors.
#'
#' @param prlst A pairlist.
#'
#' @return An integer matrix.
#'
#' @examples
#' int_prlst_rbind(pairlist(1:2, 8:9, 5:4))
#' @noRd
int_prlst_rbind <- function(prlst) {
  checkmate::assert_class(prlst, "pairlist")
  .Call(C_SXP_int_prlst_rbind, prlst, length(prlst))
}

#' [cbind()] the elements of a pairlist of integer vectors.
#'
#' @param prlst A pairlist.
#'
#' @return An integer matrix.
#'
#' @examples
#' int_prlst_rbind(pairlist(1:2, 8:9, 5:4))
#' @noRd
int_prlst_cbind <- function(prlst) {
  checkmate::assert_class(prlst, "pairlist")
  .Call(C_SXP_int_prlst_cbind, prlst, length(prlst))
}

#' Find the maxs of each row of an integer matrix.
#'
#' Faster version of `apply(int_mat, 1, max)`.
#'
#' @param int_mat An integer matrix.
#'
#' @return An integer vector.
#'
#' @noRd
int_mat_row_maxs <- function(int_mat) {
  checkmate::assert_matrix(int_mat, mode = "integer")
  .Call(C_int_mat_row_maxs, int_mat)
}

#' Check if all elements of a vector are equal to a specified value value.
#'
#' @param int_vec An integer vector.
#' @param int_scalar An integer. The value.
#'
#' @return A flag.
#'
#' @noRd
int_vec_all_value <- function(int_vec, int_scalar) {
  int_vec <- checkmate::assert_integerish(int_vec, coerce = TRUE)
  int_scalar <- checkmate::assert_int(int_scalar, coerce = TRUE)
  .Call(C_int_vec_all_value, int_vec, int_scalar)
}

#' C version of `purrr::map(pattern, ~stringi::stri_detect_coll(string, .)`.
#'
#' @inheritParams stringi::stri_detect_coll
#'
#' @return A list of logical vectors. The list is the same length as `pattern`.
#'   The vectors are the same length as `string`.
#'
#' @noRd
str_detect_many_coll <- function(string, pattern) {
  checkmate::assert_character(string, min.chars = 1, min.len = 1)
  checkmate::assert_character(pattern, min.chars = 1, min.len = 1)
  .Call(C_str_detect_many_coll, string, pattern)
}

#' C version of `purrr::map(pattern, ~stringi::stri_detect_fixed(string, .)`.
#'
#' @inheritParams stringi::stri_detect_coll
#'
#' @return A list of logical vectors. The list is the same length as `pattern`.
#'   The vectors are the same length as `string`.
#'
#' @noRd
str_detect_many_fixed <- function(string, pattern) {
  checkmate::assert_character(string, min.chars = 1, min.len = 1)
  checkmate::assert_character(pattern, min.chars = 1, min.len = 1)
  .Call(C_str_detect_many_fixed, string, pattern)
}


# --- FILE: https://raw.githubusercontent.com/rorynolan/strex/master/R/camel-case.R ---
#' Split a string based on CamelCase.
#'
#' Vectorized over `string`.
#'
#' @inheritParams str_after_nth
#' @param lower Do you want the output to be all lower case (or as is)?
#'
#' @return A list of character vectors, one list element for each element of
#'   `string`.
#'
#' @references Adapted from Ramnath Vaidyanathan's answer at
#' http://stackoverflow.com/questions/8406974/splitting-camelcase-in-r.
#'
#' @examples
#' str_split_camel_case(c("RoryNolan", "NaomiFlagg", "DepartmentOfSillyHats"))
#' str_split_camel_case(c("RoryNolan", "NaomiFlagg", "DepartmentOfSillyHats",
#'   lower = TRUE
#' ))
#' @family splitters
#' @export
str_split_camel_case <- function(string, lower = FALSE) {
  if (is_l0_char(string)) {
    return(list())
  }
  checkmate::assert_character(string)
  checkmate::assert_flag(lower)
  string <- gsub("^[^[:alnum:]]+|[^[:alnum:]]+$", "", string) %>%
    gsub("(?!^)(?=[[:upper:]])", " ", ., perl = TRUE)
  if (lower) string <- str_to_lower(string)
  str_split(string, " ")
}


# --- FILE: https://raw.githubusercontent.com/rorynolan/strex/master/R/can-be-num.R ---
#' Check if a string could be considered as numeric.
#'
#' After padding is removed, could the input string be considered to be numeric,
#' i.e. could it be coerced to numeric. This function is vectorized over its one
#' argument.
#'
#' @inheritParams str_after_nth
#'
#' @return A logical vector.
#'
#' @examples
#' str_can_be_numeric("3")
#' str_can_be_numeric("5 ")
#' str_can_be_numeric(c("1a", "abc"))
#' @family type converters
#' @export
str_can_be_numeric <- function(string) {
  checkmate::assert(
    checkmate::check_character(string),
    checkmate::check_numeric(string)
  )
  !is.na(suppressWarnings(as.numeric(string)))
}


# --- FILE: https://raw.githubusercontent.com/rorynolan/strex/master/R/currency.R ---
#' Number pattern for currency.
#'
#' @return A string.
#'
#' @noRd
curr_pattern <- function() {
  num_regex(decimals = TRUE, sci = TRUE, negs = TRUE, big_mark = ",")
}

#' Helper for currency extration.
#'
#' Given string numbers, strings and amount locations, output the required
#' data frame.
#'
#' @param string_num The string number.
#' @param string A character vector.
#' @param locs An integer matrix. The amount locations.
#'
#' @return A data frame.
#'
#' @noRd
extract_curr_helper <- function(string_num, string, locs) {
  amount <- string %>%
    str_sub(locs[, 1], locs[, 2]) %>%
    str_replace_all(stringr::coll(","), "") %>%
    as.numeric()
  curr_sym_pos <- locs[, 1] - 1
  curr_sym <- str_elem(string, curr_sym_pos)
  sign_sym_pos <- ifelse(curr_sym_pos == 0, 0, curr_sym_pos - 1)
  curr_sym_sign <- ifelse(str_elem(string, sign_sym_pos) == "-", -1, 1)
  amount <- amount * curr_sym_sign
  data.frame(
    string_num = string_num, string = string,
    curr_sym = curr_sym, amount = amount,
    stringsAsFactors = FALSE
  )
}

#' Extract currency amounts from a string.
#'
#' The currency of a number is defined as the character coming before the number
#' in the string. If nothing comes before (i.e. if the number is the first thing
#' in the string), the currency is the empty string, similarly the currency can
#' be a space, comma or any manner of thing.
#'
#' These functions are vectorized over `string` and `n`.
#'
#' [str_extract_currencies()] extracts all currency amounts.
#'
#' `str_nth_currency()` just gets the `n`th currency amount from each string.
#' `str_first_currency(string)` and `str_last_currency(string)` are just
#' wrappers for `str_nth_currency(string, n = 1)` and `str_nth_currency(string,
#' n = -1)`.
#'
#' "-$2.00" and "$-2.00" are interpreted as negative two dollars.
#'
#' If you request e.g. the 5th currency amount but there are only 3 currency
#' amounts, you get an amount and currency symbol of `NA`.
#'
#' @inheritParams str_after_nth
#'
#' @return A data frame with 4 columns: `string_num`, `string`, `curr_sym` and
#'   `amount`. Every extracted currency amount gets its own row in the data
#'   frame detailing the string number and string that it was extracted from,
#'   the currency symbol and the amount.
#'
#' @examples
#' string <- c("ab3 13", "$1", "35.00 $1.14", "abc5 $3.8", "stuff")
#' str_extract_currencies(string)
#' str_nth_currency(string, n = 2)
#' str_nth_currency(string, n = -2)
#' str_nth_currency(string, c(1, -2, 1, 2, -1))
#' str_first_currency(string)
#' str_last_currency(string)
#' @name currency
#' @family currency extractors
NULL

#' @rdname currency
#' @export
str_extract_currencies <- function(string) {
  if (is_l0_char(string)) {
    return(extract_curr_helper(
      integer(), character(),
      matrix(ncol = 2, nrow = 0)
    ))
  }
  checkmate::assert_character(string)
  locs <- str_locate_all(string, curr_pattern())
  locs_lens <- lengths(locs)
  string_num <- rep(seq_along(string), locs_lens / 2)
  string <- string[string_num]
  locs <- do.call(rbind, locs)
  extract_curr_helper(string_num, string, locs)
}


#' @rdname currency
#' @export
str_nth_currency <- function(string, n) {
  if (is_l0_char(string)) {
    checkmate::assert_integerish(n)
    return(extract_curr_helper(
      integer(), character(),
      matrix(ncol = 2, nrow = 0)
    ))
  }
  verify_string_n(string, n)
  abs_n <- abs(n)
  if (length(n) == 1 && abs_n == 1) {
    if (n == 1) {
      locs <- stringi::stri_locate_first_regex(string, curr_pattern())
    } else {
      locs <- stringi::stri_locate_last_regex(string, curr_pattern())
    }
  } else {
    locs <- matrix(NA_integer_, ncol = 2, nrow = length(string))
    interim_locs <- str_locate_all(string, curr_pattern())
    interim_locs_n_matches <- lengths(interim_locs) / 2
    good <- interim_locs_n_matches >= abs_n
    n_negs <- n < 0
    if (any(n_negs)) {
      if (length(n) == 1) {
        n <- interim_locs_n_matches + n + 1
      } else {
        n[n_negs] <- interim_locs_n_matches[n_negs] + n[n_negs] + 1
      }
    }
    if (any(good)) {
      if (length(n) > 1) n <- n[good]
      locs[good, ] <- interim_locs[good] %>%
        int_mat_lst_rbind_nth_rows(n)
    }
  }
  extract_curr_helper(seq_along(string), string, locs)
}

#' @rdname currency
#' @export
str_first_currency <- function(string) {
  str_nth_currency(string, n = 1)
}

#' @rdname currency
#' @export
str_last_currency <- function(string) {
  str_nth_currency(string, n = -1)
}


# --- FILE: https://raw.githubusercontent.com/rorynolan/strex/master/R/detect.R ---
#' Detect any or all patterns.
#'
#' Vectorized over `string`.
#'
#' @param string A character vector.
#' @param pattern A character vector. The patterns to look for. Default is
#'   `stringi`-style regular expression. [stringr::coll()] and
#'   [stringr::fixed()] are also permissible.
#' @param negate A flag. If `TRUE`, inverts the result.
#'
#' @return A character vector.
#'
#' @examples
#' str_detect_all("quick brown fox", c("x", "y", "z"))
#' str_detect_all(c(".", "-"), ".")
#' str_detect_all(c(".", "-"), coll("."))
#' str_detect_all(c(".", "-"), coll("."), negate = TRUE)
#' str_detect_all(c(".", "-"), c(".", ":"))
#' str_detect_all(c(".", "-"), coll(c(".", ":")))
#' str_detect_all("xyzabc", c("a", "c", "z"))
#' str_detect_all(c("xyzabc", "abcxyz"), c(".b", "^x"))
#'
#' @export
str_detect_all <- function(string, pattern, negate = FALSE) {
  checkmate::assert_character(string)
  if (inherits(pattern, "stringr_boundary")) {
    rlang::abort("Function cannot handle a `pattern` of type 'boundary'.")
  }
  checkmate::assert_character(pattern, min.chars = 1)
  checkmate::assert_flag(negate)
  if (inherits(pattern, "stringr_coll") || inherits(pattern, "stringr_fixed")) {
    if (inherits(pattern, "stringr_coll")) {
      out <- str_detect_many_coll(string, pattern)
    } else {
      out <- str_detect_many_fixed(string, pattern)
    }
    out <- Reduce(`&`, out)
  } else {
    pattern <- pattern %>%
      str_c("(?=.*", ., ")") %>%
      str_flatten() %>%
      str_c("^", .)
    out <- stringr::str_detect(string, pattern)
  }
  if (negate) out <- !out
  out
}

#' @rdname str_detect_all
#'
#' @examples
#' str_detect_any("quick brown fox", c("x", "y", "z"))
#' str_detect_any(c(".", "-"), ".")
#' str_detect_any(c(".", "-"), coll("."))
#' str_detect_any(c(".", "-"), coll("."), negate = TRUE)
#' str_detect_any(c(".", "-"), c(".", ":"))
#' str_detect_any(c(".", "-"), coll(c(".", ":")))
#' str_detect_any(c("xyzabc", "abcxyz"), c(".b", "^x"))
#'
#' @export
str_detect_any <- function(string, pattern, negate = FALSE) {
  checkmate::assert_character(string)
  if (inherits(pattern, "stringr_boundary")) {
    rlang::abort("Function cannot handle a `pattern` of type 'boundary'.")
  }
  checkmate::assert_character(pattern, min.chars = 1)
  checkmate::assert_flag(negate)
  if (inherits(pattern, "stringr_coll") || inherits(pattern, "stringr_fixed")) {
    if (inherits(pattern, "stringr_coll")) {
      out <- str_detect_many_coll(string, pattern)
    } else {
      out <- str_detect_many_fixed(string, pattern)
    }
    out <- Reduce(`|`, out)
  } else {
    out <- str_detect(string, str_flatten(pattern, "|"))
  }
  if (negate) out <- !out
  out
}


# --- FILE: https://raw.githubusercontent.com/rorynolan/strex/master/R/elem.R ---
#' Extract a single character from a string, using its index.
#'
#' If the element does not exist, this function returns the empty string. This
#' is consistent with [stringr::str_sub()]. This function is vectorised over
#' both arguments.
#'
#' @inheritParams str_after_nth
#' @param index An integer. Negative indexing is allowed as in
#'   [stringr::str_sub()].
#'
#' @return A one-character string.
#'
#' @examples
#' str_elem(c("abcd", "xyz"), 3)
#' str_elem("abcd", -2)
#' @family single element extractors
#' @export
str_elem <- function(string, index) {
  if (is_l0_char(string)) {
    return(character())
  }
  verify_string_n(string, index, "index")
  str_sub(string, index, index)
}

#' Helper for [str_elems()] and [str_paste_elems()].
#'
#' @return A list of elements of strings. Either with one list element per input
#'   string (orientation: bycol) or one string index (for multiple strings) per
#'   list element (orientation: byrow).
#'
#' @noRd
str_elems_helper <- function(string, indices, insist_bycol = FALSE) {
  indices <- as.integer(indices)
  # The following lapplys can only be easily and efficiently replaced if Rcpp
  # starts dealing with UTF-8 strings well.
  if (!insist_bycol && length(indices) > length(string)) {
    out <- lapply(indices, function(x) str_elem(string, x))
    attr(out, "strex__str_elems_helper__orientation") <- "byrow"
  } else {
    out <- lapply(string, function(x) str_elem(x, indices))
    attr(out, "strex__str_elems_helper__orientation") <- "bycol"
  }
  out
}

#' Extract several single elements from a string.
#'
#' Efficiently extract several elements from a string. See [str_elem()] for
#' extracting single elements. This function is vectorized over the first
#' argument.
#'
#' @inheritParams str_after_nth
#' @param indices A vector of integerish values. Negative indexing is allowed as
#'   in [stringr::str_sub()].
#' @param byrow Should the elements be organised in the matrix with one row per
#'   string (`byrow = TRUE`, the default) or one column per string (`byrow =
#'   FALSE`). See examples if you don't understand.
#'
#' @return A character matrix.
#'
#' @examples
#' string <- c("abc", "def", "ghi", "vwxyz")
#' str_elems(string, 1:2)
#' str_elems(string, 1:2, byrow = FALSE)
#' str_elems(string, c(1, 2, 3, 4, -1))
#' @family single element extractors
#' @export
str_elems <- function(string, indices, byrow = TRUE) {
  checkmate::assert_flag(byrow)
  if (is_l0_char(string)) {
    out <- matrix(character(), ncol = length(indices))
    if (!byrow) out <- t(out)
    return(out)
  }
  checkmate::assert_character(string, min.len = 1)
  checkmate::assert_integerish(indices, min.len = 1)
  out <- str_elems_helper(string, indices)
  if (attr(out, "strex__str_elems_helper__orientation") == "byrow") {
    byrow <- !byrow
  }
  stringi::stri_list2matrix(out, byrow = byrow)
}

#' Extract single elements of a string and paste them together.
#'
#' This is a quick way around doing a call to [str_elems()] followed by a call
#' of `apply(..., paste)`.
#'
#' Elements that don't exist e.g. element 5 of `"abc"` are ignored.
#'
#' @inheritParams str_after_nth
#' @inheritParams str_elems
#' @param sep A string. The separator for pasting `string` elements together.
#'
#' @return A character vector.
#'
#' @examples
#' string <- c("abc", "def", "ghi", "vwxyz")
#' str_paste_elems(string, 1:2)
#' str_paste_elems(string, c(1, 2, 3, 4, -1))
#' str_paste_elems("abc", c(1, 5, 55, 43, 3))
#' @family single element extractors
#' @export
str_paste_elems <- function(string, indices, sep = "") {
  if (is_l0_char(string)) {
    return(character())
  }
  checkmate::assert_character(string, min.len = 1)
  checkmate::assert_integerish(indices, min.len = 1)
  checkmate::assert_string(sep)
  out <- str_elems_helper(string, indices, insist_bycol = TRUE)
  stringi::stri_paste_list(out, sep = sep)
}


# --- FILE: https://raw.githubusercontent.com/rorynolan/strex/master/R/extract-non-nums.R ---
#' Extract non-numerics assuming no number ambiguity.
#'
#' Sometimes the strings have ambiguous numbers in them e.g. 2.5.3. These have
#' to be dealt with by strex (which it does by returning `NA` in those cases).
#' This helper to `str_extract_non_numerics()` assumes that the input has
#' no such ambiguities.
#'
#' @param string A character vector.
#' @param num_pattern The regex defining a numer in the current context.
#'
#' @return A list of character vectors.
#'
#' @noRd
str_extract_non_nums_no_ambigs <- function(string, num_pattern) {
  stringi::stri_split_regex(string, num_pattern, omit_empty = TRUE)
}

#' Extract non-numbers from a string.
#'
#' Extract the non-numeric bits of a string where numbers are optionally defined
#' with decimals, scientific notation and thousand separators.
#'
#' \itemize{ \item `str_first_non_numeric(...)` is just
#' `str_nth_non_numeric(..., n = 1)`. \item `str_last_non_numeric(...)` is just
#' `str_nth_non_numeric(..., n = -1)`. }
#'
#' @inheritParams str_extract_numbers
#'
#' @examples
#' strings <- c(
#'   "abc123def456", "abc-0.12def.345", "abc.12e4def34.5e9",
#'   "abc1,100def1,230.5", "abc1,100e3,215def4e1,000"
#' )
#' str_extract_non_numerics(strings)
#' str_extract_non_numerics(strings, decimals = TRUE, leading_decimals = FALSE)
#' str_extract_non_numerics(strings, decimals = TRUE)
#' str_extract_non_numerics(strings, big_mark = ",")
#' str_extract_non_numerics(strings,
#'   decimals = TRUE, leading_decimals = TRUE,
#'   sci = TRUE
#' )
#' str_extract_non_numerics(strings,
#'   decimals = TRUE, leading_decimals = TRUE,
#'   sci = TRUE, big_mark = ",", negs = TRUE
#' )
#' str_extract_non_numerics(c("22", "1.2.3"), decimals = TRUE)
#' @family non-numeric extractors
#' @export
str_extract_non_numerics <- function(string, decimals = FALSE,
                                     leading_decimals = decimals, negs = FALSE,
                                     sci = FALSE, big_mark = "",
                                     commas = FALSE) {
  if (!isFALSE(commas)) {
    lifecycle::deprecate_stop(
      "2.0.0", "strex::str_extract_non_numerics(commas)",
      details = "Use the `big_mark` argument instead."
    )
  }
  checkmate::assert_character(string)
  checkmate::assert_flag(decimals)
  checkmate::assert_flag(leading_decimals)
  checkmate::assert_flag(negs)
  checkmate::assert_flag(sci)
  checkmate::assert_character(big_mark)
  if (is_l0_char(string)) {
    return(list())
  }
  num_pattern <- num_regex(
    decimals = decimals, leading_decimals = leading_decimals,
    negs = negs, sci = sci, big_mark = big_mark
  )
  ambig_pattern <- ambig_num_regex(
    decimals = decimals,
    leading_decimals = leading_decimals,
    sci = sci, big_mark = big_mark
  )
  ambigs <- num_ambigs(string,
    decimals = decimals,
    leading_decimals = leading_decimals, sci = sci,
    big_mark = big_mark
  )
  out <- vector(mode = "list", length = length(string))
  if (any(ambigs)) {
    ambig_warn(string, ambigs, ambig_regex = ambig_pattern)
    out[ambigs] <- NA_character_
    not_ambigs <- !ambigs
    out[not_ambigs] <- str_extract_non_nums_no_ambigs(
      string[not_ambigs],
      num_pattern
    )
  } else {
    out[] <- str_extract_non_nums_no_ambigs(string, num_pattern)
  }
  out
}

#' Extract the `n`th non-numeric substring from a string.
#'
#' This is a helper for `str_nth_non_numeric()` which assums non-ambiguous
#' input.
#'
#' For a detailed explanation of the number extraction, see
#' [str_extract_numbers()].
#'
#' @inheritParams str_extract_numbers
#' @inheritParams str_nth_number
#'
#' @return A character vector.
#'
#' @noRd
str_nth_non_numeric_no_ambigs <- function(string, num_pattern, n) {
  if (length(string) == 0) {
    return(character())
  }
  if (length(n) == 1 && n >= 0) {
    out <- stringi::stri_split_regex(string, num_pattern,
      n = n + 1,
      omit_empty = TRUE, simplify = TRUE
    )[, n]
    out[!str_length(out)] <- NA_character_
  } else {
    out <- stringi::stri_split_regex(string, num_pattern, omit_empty = TRUE) %>%
      chr_lst_nth_elems(n)
  }
  out
}

#' Extract the `n`th non-numeric substring from a string.
#'
#' Extract the `n`th non-numeric bit of a string where numbers are optionally
#' defined with decimals, scientific notation and thousand separators.
#' \itemize{ \item `str_first_non_numeric(...)` is just
#' `str_nth_non_numeric(..., n = 1)`. \item `str_last_non_numeric(...)` is
#' just `str_nth_non_numeric(..., n = -1)`. }
#'
#' @inheritParams str_extract_non_numerics
#' @inheritParams str_after_nth
#'
#'
#'
#' @examples
#' strings <- c(
#'   "abc123def456", "abc-0.12def.345", "abc.12e4def34.5e9",
#'   "abc1,100def1,230.5", "abc1,100e3,215def4e1,000"
#' )
#' str_nth_non_numeric(strings, n = 2)
#' str_nth_non_numeric(strings, n = -2, decimals = TRUE)
#' str_first_non_numeric(strings, decimals = TRUE, leading_decimals = FALSE)
#' str_last_non_numeric(strings, big_mark = ",")
#' str_nth_non_numeric(strings,
#'   n = 1, decimals = TRUE, leading_decimals = TRUE,
#'   sci = TRUE
#' )
#' str_first_non_numeric(strings,
#'   decimals = TRUE, leading_decimals = TRUE,
#'   sci = TRUE, big_mark = ",", negs = TRUE
#' )
#' str_first_non_numeric(c("22", "1.2.3"), decimals = TRUE)
#' @family non-numeric extractors
#' @export
str_nth_non_numeric <- function(string, n, decimals = FALSE,
                                leading_decimals = decimals, negs = FALSE,
                                sci = FALSE, big_mark = "", commas = FALSE) {
  if (!isFALSE(commas)) {
    lifecycle::deprecate_stop("2.0.0", "strex::str_nth_non_numeric(commas)",
                              details = "Use the `big_mark` argument instead.")
  }
  if (is_l0_char(string)) {
    return(character())
  }
  verify_string_n(string, n)
  checkmate::assert_flag(decimals)
  checkmate::assert_flag(leading_decimals)
  checkmate::assert_flag(negs)
  checkmate::assert_flag(sci)
  checkmate::assert_character(big_mark)
  num_pattern <- num_regex(
    decimals = decimals, leading_decimals = leading_decimals,
    negs = negs, sci = sci, big_mark = big_mark
  )
  ambig_pattern <- ambig_num_regex(
    decimals = decimals,
    leading_decimals = leading_decimals,
    sci = sci, big_mark = big_mark
  )
  ambigs <- num_ambigs(string,
    decimals = decimals,
    leading_decimals = leading_decimals, sci = sci,
    big_mark = big_mark
  )
  out <- character(length(string))
  if (any(ambigs)) {
    ambig_warn(string, ambigs, ambig_pattern)
    out[ambigs] <- NA_character_
    not_ambigs <- !ambigs
    out[not_ambigs] <- str_nth_non_numeric_no_ambigs(
      string[not_ambigs],
      num_pattern, n
    )
  } else {
    out[] <- str_nth_non_numeric_no_ambigs(string, num_pattern, n)
  }
  out
}

#' @rdname str_nth_non_numeric
#' @export
str_first_non_numeric <- function(string, decimals = FALSE,
                                  leading_decimals = decimals, negs = FALSE,
                                  sci = FALSE, big_mark = "", commas = FALSE) {
  str_nth_non_numeric(string,
    n = 1,
    decimals = decimals, leading_decimals = leading_decimals,
    negs = negs, sci = sci, big_mark = big_mark, commas = commas
  )
}

#' @rdname str_nth_non_numeric
#' @export
str_last_non_numeric <- function(string, decimals = FALSE,
                                 leading_decimals = decimals, negs = FALSE,
                                 sci = FALSE, big_mark = "") {
  str_nth_non_numeric(string,
    n = -1,
    decimals = decimals, leading_decimals = leading_decimals,
    negs = negs, sci = sci, big_mark = big_mark
  )
}


# --- FILE: https://raw.githubusercontent.com/rorynolan/strex/master/R/extract-nums.R ---
#' Create a regex pattern for finding numbers in strings.
#'
#' There are options for finding decimal numbers, negative numbers and
#' scientific notation, but the default simply finds consecutive numeric
#' characters (0123456789). See the examples to understand how these options
#' work.
#'
#' @inheritParams str_extract_numbers
#'
#' @return A string. The appropriate regex pattern for searching for the chosen
#'   types of numbers in strings.
#'
#' @examples
#' num_regex()
#' num_regex(decimals = TRUE)
#' num_regex(decimals = TRUE, leading_decimals = TRUE)
#' num_regex(negs = TRUE)
#' num_regex(
#'   decimals = TRUE, leading_decimals = TRUE,
#'   negs = TRUE, sci = TRUE
#' )
#' @noRd
num_regex <- function(decimals = FALSE, leading_decimals = decimals,
                      negs = FALSE, sci = FALSE, big_mark = "") {
  if (leading_decimals == TRUE && decimals == FALSE) {
    rlang::abort(
      c("To allow leading decimals, you need to first allow decimals.",
        i = "To allow decimals, use `decimals = TRUE`."
      )
    )
  }
  use_big_mark <- str_length(big_mark) > 0
  dec_pattern <- ifelse(use_big_mark,
    str_glue("\\d+(?:[{big_mark}]?\\d+)*(?:\\.\\d+)?"),
    "\\d+(?:\\.\\d+)?"
  )
  leading_dec_pattern <- ifelse(use_big_mark,
    str_c(
      str_glue("(?:(?:\\d+(?:[{big_mark}]?\\d+)*(?:\\.\\d+)?)|"),
      "(?:\\.?\\d+))"
    ),
    "(?:\\d+(?:\\.\\d+)?|\\.?\\d+)"
  )
  non_dec_pattern <- ifelse(use_big_mark,
    str_glue("\\d+(?:[{big_mark}]?\\d+)*"),
    "\\d+"
  )
  pattern <- non_dec_pattern
  if (decimals) {
    pattern <- ifelse(leading_decimals, leading_dec_pattern, dec_pattern)
  }
  if (sci) {
    pattern <- stringr::str_glue(
      "(?:(?:{pattern}[eE][+-]?{non_dec_pattern})|",
      "(?:{pattern}))"
    )
  }
  if (negs) pattern <- stringr::str_glue("-?(?:{pattern})")
  pattern
}

ambig_num_regex <- function(decimals = FALSE, leading_decimals = decimals,
                            sci = FALSE, big_mark = "") {
  out <- character(1)
  if (!any(decimals, leading_decimals, sci)) {
    return(out)
  }
  use_big_mark <- str_length(big_mark) > 0
  if (decimals) {
    if (use_big_mark) {
      out <- ifelse(leading_decimals,
        str_glue("\\.(?:\\d+[{big_mark}]?)+\\.\\d"),
        str_glue("(?:\\d+[{big_mark}]?)+\\.(?:\\d+[{big_mark}]?)+\\.\\d")
      )
    } else {
      out <- ifelse(leading_decimals, "\\.\\d+\\.\\d", "\\d\\.\\d+\\.\\d")
    }
  }
  if (sci) {
    if (use_big_mark) {
      sci_bit <- ifelse(
        decimals,
        str_glue(
          "\\d\\.?\\d*[eE](?:\\d+(?:[{big_mark}]\\d+)*)+",
          "(?:\\.\\d|\\.[eE]\\d|[eE]\\d)"
        ),
        str_glue("\\d[eE](?:\\d+(?:[{big_mark}]\\d+)*)+[eE]\\d")
      )
    } else {
      sci_bit <- ifelse(
        decimals,
        "\\d\\.?\\d*[eE]\\d+(?:\\.\\d|\\.[eE]\\d|[eE]\\d)",
        "\\d[eE]\\d+[eE]\\d"
      )
    }
    out <- ifelse(decimals, stringr::str_glue("({sci_bit})|({out})"), sci_bit)
  }
  out
}

num_ambigs <- function(string, decimals = FALSE, leading_decimals = decimals,
                       sci = FALSE, big_mark = "") {
  if (!any(c(decimals, sci))) {
    return(FALSE)
  }
  str_detect(string, ambig_num_regex(
    decimals = decimals, leading_decimals = leading_decimals,
    sci = sci, big_mark = big_mark
  ))
}

ambig_warn <- function(string, ambigs, ambig_regex) {
  first_offender <- match(T, ambigs) %>%
    list(., string[.])
  first_offender[[2]] <- ifelse(
    str_length(first_offender[[2]]) > 50,
    str_c(str_sub(first_offender[[2]], 1, 17), "..."),
    first_offender[[2]]
  )
  rlang::warn(
    c(
      "`NA`s introduced by ambiguity.",
      i = str_glue(
        "The first such ambiguity is in string number ",
        "{first_offender[[1]]} which is '{first_offender[[2]]}'."
      ),
      x = str_glue(
        "The offending part of that string is ",
        "'{str_extract(first_offender[[2]], ambig_regex)}'."
      )
    )
  )
}

#' Extract numbers from a string.
#'
#' Extract the numbers from a string, where decimals, scientific notation and
#' thousand separators are optionally allowed.
#'
#' If any part of a string contains an ambiguous number (e.g. `1.2.3` would be
#' ambiguous if `decimals = TRUE` (but not otherwise)), the value returned for
#' that string will be `NA` and a `warning` will be issued.
#'
#' With scientific notation, it is assumed that the exponent is not a decimal
#' number e.g. `2e2.4` is unacceptable. Thousand separators, however, are
#' acceptable in the exponent.
#'
#' Numbers outside the double precision floating point range (i.e. with absolute
#' value greater than 1.797693e+308) are read as `Inf` (or `-Inf` if they begin
#' with a minus sign). This is what `base::as.numeric()` does.
#'
#' @param string A string.
#' @param decimals Do you want to include the possibility of decimal numbers
#'   (`TRUE`) or not (`FALSE`, the default).
#' @param leading_decimals Do you want to allow a leading decimal point to be
#'   the start of a number?
#' @param negs Do you want to allow negative numbers? Note that double negatives
#'   are not handled here (see the examples).
#' @param sci Make the search aware of scientific notation e.g. 2e3 is the same
#'   as 2000.
#' @param big_mark A character. Allow this character to be used as a thousands
#'   separator. This character will be removed from between digits before they
#'   are converted to numeric. You may specify many at once by pasting them
#'   together e.g. `big_mark = ",_"` will allow both commas and underscores.
#'   Internally, this will be used inside a `[]` regex block so e.g. `"a-z"`
#'   will behave differently to `"az-"`. Most common separators (commas, spaces,
#'   underscores) should work fine.
#' @param commas Deprecated. Use `big_mark` instead.
#' @param leave_as_string Do you want to return the number as a string (`TRUE`)
#'   or as numeric (`FALSE`, the default)?
#'
#'
#' @return For `str_extract_numbers` and `str_extract_non_numerics`, a list of
#'   numeric or character vectors, one list element for each element of
#'   `string`. For `str_nth_number` and `str_nth_non_numeric`, a numeric or
#'   character vector the same length as the vector `string`.
#' @examples
#' strings <- c(
#'   "abc123def456", "abc-0.12def.345", "abc.12e4def34.5e9",
#'   "abc1,100def1,230.5", "abc1,100e3,215def4e1,000"
#' )
#' str_extract_numbers(strings)
#' str_extract_numbers(strings, decimals = TRUE)
#' str_extract_numbers(strings, decimals = TRUE, leading_decimals = TRUE)
#' str_extract_numbers(strings, big_mark = ",")
#' str_extract_numbers(strings,
#'   decimals = TRUE, leading_decimals = TRUE,
#'   sci = TRUE
#' )
#' str_extract_numbers(strings,
#'   decimals = TRUE, leading_decimals = TRUE,
#'   sci = TRUE, big_mark = ",", negs = TRUE
#' )
#' str_extract_numbers(strings,
#'   decimals = TRUE, leading_decimals = FALSE,
#'   sci = FALSE, big_mark = ",", leave_as_string = TRUE
#' )
#' str_extract_numbers(c("22", "1.2.3"), decimals = TRUE)
#' @family numeric extractors
#' @export
str_extract_numbers <- function(string,
                                decimals = FALSE, leading_decimals = decimals,
                                negs = FALSE, sci = FALSE, big_mark = "",
                                leave_as_string = FALSE, commas = FALSE) {
  if (!isFALSE(commas)) {
    lifecycle::deprecate_stop("2.0.0", "strex::str_extract_numbers(commas)",
      details = "Use the `big_mark` argument instead."
    )
  }
  checkmate::assert_character(string)
  checkmate::assert_flag(leave_as_string)
  checkmate::assert_flag(decimals)
  checkmate::assert_flag(leading_decimals)
  checkmate::assert_flag(negs)
  checkmate::assert_flag(sci)
  checkmate::assert_string(big_mark)
  if (is_l0_char(string)) {
    return(list())
  }
  pattern <- num_regex(
    decimals = decimals, leading_decimals = leading_decimals,
    negs = negs, sci = sci, big_mark = big_mark
  )
  ambig_pattern <- ambig_num_regex(
    decimals = decimals,
    leading_decimals = leading_decimals,
    sci = sci, big_mark = big_mark
  )
  ambigs <- num_ambigs(string,
    decimals = decimals,
    leading_decimals = leading_decimals, sci = sci, big_mark = big_mark
  )
  out <- vector(mode = "list", length = length(string))
  if (any(ambigs)) {
    ambig_warn(string, ambigs, ambig_regex = ambig_pattern)
    out[ambigs] <- NA_character_
    not_ambigs <- !ambigs
    out[not_ambigs] <- str_extract_all(string[not_ambigs], pattern)
  } else {
    out[] <- str_extract_all(string, pattern)
  }
  if (leave_as_string) {
    return(out)
  }
  lst_chr_to_dbl(out, big_mark_regex = str_glue("[{big_mark}]"))
}

#' Extract the `n`th number from a string.
#'
#' Extract the `n`th number from a string, where decimals, scientific notation
#' and thousand separators are optionally allowed.
#'
#' \itemize{ \item `str_first_number(...)` is just `str_nth_number(..., n = 1)`.
#' \item `str_last_number(...)` is just `str_nth_number(..., n = -1)`. }
#'
#' For a detailed explanation of the number extraction, see
#' [str_extract_numbers()].
#'
#' @inheritParams str_extract_numbers
#' @inheritParams str_after_nth
#'
#' @return A numeric vector (or a character vector if `leave_as_string = TRUE`).
#'
#' @examples
#' strings <- c(
#'   "abc123def456", "abc-0.12def.345", "abc.12e4def34.5e9",
#'   "abc1,100def1,230.5", "abc1,100e3,215def4e1,000"
#' )
#' str_nth_number(strings, n = 2)
#' str_nth_number(strings, n = -2, decimals = TRUE)
#' str_first_number(strings, decimals = TRUE, leading_decimals = TRUE)
#' str_last_number(strings, big_mark = ",")
#' str_nth_number(strings,
#'   n = 1, decimals = TRUE, leading_decimals = TRUE,
#'   sci = TRUE
#' )
#' str_first_number(strings,
#'   decimals = TRUE, leading_decimals = TRUE,
#'   sci = TRUE, big_mark = ",", negs = TRUE
#' )
#' str_last_number(strings,
#'   decimals = TRUE, leading_decimals = FALSE,
#'   sci = FALSE, big_mark = ",", negs = TRUE, leave_as_string = TRUE
#' )
#' str_first_number(c("22", "1.2.3"), decimals = TRUE)
#' @family numeric extractors
#' @export
str_nth_number <- function(string, n, decimals = FALSE,
                           leading_decimals = decimals, negs = FALSE,
                           sci = FALSE, big_mark = "",
                           leave_as_string = FALSE, commas = FALSE) {
  if (!isFALSE(commas)) {
    lifecycle::deprecate_stop("2.0.0", "strex::str_nth_number(commas)",
      details = "Use the `big_mark` argument instead."
    )
  }
  checkmate::assert_flag(leave_as_string)
  if (is_l0_char(string)) {
    return(vector(mode = ifelse(leave_as_string, "character", "numeric")))
  }
  verify_string_n(string, n)
  checkmate::assert_flag(decimals)
  checkmate::assert_flag(leading_decimals)
  checkmate::assert_flag(negs)
  checkmate::assert_flag(sci)
  checkmate::assert_string(big_mark)
  out <- character(length(string))
  if (int_vec_all_value(n, 1) || int_vec_all_value(n, -1)) {
    pattern <- num_regex(
      decimals = decimals, leading_decimals = leading_decimals,
      negs = negs, sci = sci, big_mark = big_mark
    )
    ambig_pattern <- ambig_num_regex(
      decimals = decimals,
      leading_decimals = leading_decimals,
      sci = sci, big_mark = big_mark
    )
    ambigs <- FALSE
    if (str_length(ambig_pattern)) ambigs <- str_detect(string, ambig_pattern)
    if (any(ambigs)) {
      ambig_warn(string, ambigs, ambig_regex = ambig_pattern)
      not_ambigs <- !ambigs
      out[ambigs] <- NA_character_
      if (n[[1]] == 1) {
        out[not_ambigs] <- stringi::stri_extract_first_regex(
          string[not_ambigs],
          pattern
        )
      } else {
        out[not_ambigs] <- stringi::stri_extract_last_regex(
          string[not_ambigs],
          pattern
        )
      }
    } else {
      if (n[[1]] == 1) {
        out[] <- stringi::stri_extract_first_regex(string, pattern)
      } else {
        out[] <- stringi::stri_extract_last_regex(string, pattern)
      }
    }
  } else {
    numbers <- str_extract_numbers(string,
      leave_as_string = TRUE, negs = negs, sci = sci,
      decimals = decimals,
      leading_decimals = leading_decimals, big_mark = big_mark
    )
    out <- chr_lst_nth_elems(numbers, n)
  }
  if (leave_as_string) {
    return(out)
  }
  if (str_length(big_mark)) {
    out <- str_remove_all(
      out, str_glue("[{big_mark}]")
    )
  }
  as.numeric(out)
}

#' @rdname str_nth_number
#' @export
str_first_number <- function(string, decimals = FALSE,
                             leading_decimals = decimals, negs = FALSE,
                             sci = FALSE, big_mark = "",
                             leave_as_string = FALSE, commas = FALSE) {
  str_nth_number(string,
    n = 1, leave_as_string = leave_as_string,
    decimals = decimals, leading_decimals = leading_decimals,
    negs = negs, sci = sci, big_mark = big_mark, commas = commas
  )
}

#' @rdname str_nth_number
#' @export
str_last_number <- function(string, decimals = FALSE,
                            leading_decimals = decimals, negs = FALSE,
                            sci = FALSE, big_mark = "",
                            leave_as_string = FALSE, commas = FALSE) {
  str_nth_number(string,
    n = -1, leave_as_string = leave_as_string,
    decimals = decimals, leading_decimals = leading_decimals,
    negs = negs, sci = sci, big_mark = big_mark, commas = commas
  )
}


# --- FILE: https://raw.githubusercontent.com/rorynolan/strex/master/R/give-ext.R ---
#' Ensure a file name has the intended extension.
#'
#' Say you want to ensure a name is fit to be the name of a csv file. Then, if
#' the input doesn't end with ".csv", this function will tack ".csv" onto the
#' end of it. This is vectorized over the first argument.
#'
#' @param string The intended file name.
#' @param ext The intended file extension (with or without the ".").
#' @param replace If the file has an extension already, replace it (or append
#'   the new extension name)?
#'
#' @return A string: the file name in your intended form.
#'
#' @examples
#' str_give_ext(c("abc", "abc.csv"), "csv")
#' str_give_ext("abc.csv", "pdf")
#' str_give_ext("abc.csv", "pdf", replace = TRUE)
#' @family appenders
#' @export
str_give_ext <- function(string, ext, replace = FALSE) {
  if (is_l0_char(string)) {
    return(character())
  }
  checkmate::assert_character(string)
  checkmate::assert_string(ext)
  checkmate::assert_flag(replace)
  ext <- str_match(ext, "^\\.*(.*)")[, 2]
  if (replace) {
    string <- str_before_last_dot(string)
  } else {
    correct_ext <- str_detect(string, str_c("\\.", ext, "$"))
    string[correct_ext] <- str_before_last_dot(string[correct_ext])
  }
  str_c(string, ".", ext)
}


# --- FILE: https://raw.githubusercontent.com/rorynolan/strex/master/R/locate.R ---
#' Locate the braces in a string.
#'
#' Give the positions of `(`, `)`, `[`, `]`, `\{`, `\}` within a string.
#'
#' @param string A character vector
#'
#' @return A data frame with 4 columns: `string_num`, `string`, `position` and
#'   `brace`. Every extracted brace amount gets its own row in the tibble
#'   detailing the string number and string that it was extracted from, the
#'   position in its string and the brace.
#'
#' @examples
#' str_locate_braces(c("a{](kkj)})", "ab(]c{}"))
#' @family locators
#' @export
str_locate_braces <- function(string) {
  checkmate::assert_character(string)
  if (is_l0_char(string)) {
    out <- data.frame(
      string_num = integer(),
      string = character(),
      position = integer(),
      brace = character(),
      stringsAsFactors = FALSE
    )
    return(out)
  }
  pattern <- "[\\(\\)\\[\\]\\{\\}]"
  locations <- str_locate_all(string, pattern) %>%
    int_mat_lst_nth_cols(1L)
  braces <- str_extract_all(string, pattern)
  string_num <- rep(seq_along(string), lengths(braces))
  data.frame(
    string_num = string_num, string = string[string_num],
    position = unlist(locations), brace = unlist(braces),
    stringsAsFactors = FALSE
  )
}

#' Locate the indices of the `n`th instance of a pattern.
#'
#' The `n`th instance of an pattern will cover a series of character
#' indices. These functions tell you which indices those are. These functions
#' are vectorised over all arguments.
#'
#' \itemize{ \item `str_locate_first(...)` is just `str_locate_nth(..., n = 1)`.
#' \item `str_locate_last(...)` is just `str_locate_nth(..., n = -1)`. }
#'
#' @inheritParams str_after_nth
#'
#' @return A two-column matrix. The \eqn{i}th row of this matrix gives the start
#'   and end indices of the \eqn{n}th instance of `pattern` in the \eqn{i}th
#'   element of `string`.
#'
#' @examples
#' str_locate_nth(c("abcdabcxyz", "abcabc"), "abc", 2)
#' str_locate_nth(
#'   c("This old thing.", "That beautiful thing there."),
#'   "\\w+", c(2, -2)
#' )
#' str_locate_nth("abc", "b", c(0, 1, 1, 2))
#' str_locate_first("abcxyzabc", "abc")
#' str_locate_last("abcxyzabc", "abc")
#' @family locators
#' @export
str_locate_nth <- function(string, pattern, n) {
  if (is_l0_char(string)) {
    out <- matrix(character(), ncol = 2) %>%
      magrittr::set_colnames(c("start", "end"))
    return(out)
  }
  verify_string_pattern_n(string, pattern, n)
  locs <- str_locate_all(string, pattern)
  locs_n_matches <- lengths(locs) / 2
  n_negs <- n < 0
  if (any(n_negs)) {
    if (length(n) == 1) {
      n <- locs_n_matches + n + 1
    } else {
      n[n_negs] <- locs_n_matches[n_negs] + n[n_negs] + 1
    }
  }
  out <- matrix(NA_integer_,
    nrow = max(lengths(list(string, pattern, n))), ncol = 2
  ) %>%
    magrittr::set_colnames(c("start", "end"))
  good <- (abs(n) <= locs_n_matches) & (n != 0)
  if (any(good)) {
    if (length(locs) == 1) {
      out[good, ] <- int_mat_lst_rbind_nth_rows(locs, n[good])
    } else {
      if (length(n) > 1) n <- n[good]
      out[good, ] <- int_mat_lst_rbind_nth_rows(locs[good], n)
    }
  }
  out
}

#' @rdname str_locate_nth
#' @export
str_locate_first <- function(string, pattern) {
  str_locate_nth(string, pattern, n = 1)
}

#' @rdname str_locate_nth
#' @export
str_locate_last <- function(string, pattern) {
  str_locate_nth(string, pattern, n = -1)
}


# --- FILE: https://raw.githubusercontent.com/rorynolan/strex/master/R/num-after.R ---
#' Find the `n`th number after the `m`th occurrence of a pattern.
#'
#' Given a string, a pattern and natural numbers `n` and `m`, find the `n`th
#' number after the `m`th occurrence of the pattern.
#'
#' @param n,m Vectors of integerish values. Must be either length 1 or have
#'   length equal to the length of `string`. Negative indices count from the
#'   back: while `1` and `2` correspond to first and second, `-1` and `-2`
#'   correspond to last and second-last. `0` will return `NA`.
#' @inheritParams str_after_nth
#' @inheritParams str_extract_numbers
#'
#' @return A numeric or character vector.
#'
#' @examples
#' string <- c(
#'   "abc1abc2abc3abc4abc5abc6abc7abc8abc9",
#'   "abc1def2ghi3abc4def5ghi6abc7def8ghi9"
#' )
#' str_nth_number_after_mth(string, "abc", 1, 3)
#' str_nth_number_after_mth(string, "abc", 2, 3)
#' @family numeric extractors
#' @export
str_nth_number_after_mth <- function(string, pattern, n, m,
                                     decimals = FALSE,
                                     leading_decimals = decimals,
                                     negs = FALSE, sci = FALSE, big_mark = "",
                                     leave_as_string = FALSE, commas = FALSE) {
  if (!isFALSE(commas)) {
    lifecycle::deprecate_stop(
      "2.0.0", "strex::str_nth_number_after_mth(commas)",
      details = "Use the `big_mark` argument instead."
    )
  }
  checkmate::assert_flag(leave_as_string)
  if (is_l0_char(string)) {
    return(vector(mode = ifelse(leave_as_string, "character", "numeric")))
  }
  verify_string_pattern_n_m(string, pattern, n, m)
  checkmate::assert_flag(decimals)
  checkmate::assert_flag(leading_decimals)
  checkmate::assert_flag(negs)
  checkmate::assert_flag(sci)
  checkmate::assert_string(big_mark)
  string %>%
    str_after_nth(pattern, m) %>%
    str_nth_number(n,
      decimals = decimals, leading_decimals = leading_decimals,
      negs = negs, leave_as_string = leave_as_string, sci = sci,
      big_mark = big_mark
    )
}

#' @rdname str_nth_number_after_mth
#' @examples
#' str_nth_number_after_first(string, "abc", 2)
#' @export
str_nth_number_after_first <- function(string, pattern, n, decimals = FALSE,
                                       leading_decimals = decimals,
                                       negs = FALSE, sci = FALSE,
                                       big_mark = "",
                                       leave_as_string = FALSE,
                                       commas = FALSE) {
  str_nth_number_after_mth(string, pattern,
    n = n, m = 1,
    decimals = decimals,
    leading_decimals = leading_decimals, sci = sci, big_mark = big_mark,
    negs = negs, leave_as_string = leave_as_string, commas = commas
  )
}

#' @rdname str_nth_number_after_mth
#' @examples
#' str_nth_number_after_last(string, "abc", -1)
#' @export
str_nth_number_after_last <- function(string, pattern, n,
                                      decimals = FALSE,
                                      leading_decimals = decimals,
                                      negs = FALSE, sci = FALSE,
                                      big_mark = "",
                                      leave_as_string = FALSE,
                                      commas = FALSE) {
  str_nth_number_after_mth(string, pattern,
    n = n, m = -1,
    decimals = decimals,
    leading_decimals = leading_decimals,
    negs = negs, leave_as_string = leave_as_string,
    commas = commas
  )
}

#' @rdname str_nth_number_after_mth
#' @examples
#' str_first_number_after_mth(string, "abc", 2)
#' @export
str_first_number_after_mth <- function(string, pattern, m,
                                       decimals = FALSE,
                                       leading_decimals = decimals,
                                       negs = FALSE, sci = FALSE,
                                       big_mark = "",
                                       leave_as_string = FALSE,
                                       commas = FALSE) {
  str_nth_number_after_mth(string, pattern,
    n = 1, m = m,
    decimals = decimals,
    leading_decimals = leading_decimals, sci = sci, big_mark = big_mark,
    negs = negs, leave_as_string = leave_as_string,
    commas = commas
  )
}

#' @rdname str_nth_number_after_mth
#' @examples
#' str_last_number_after_mth(string, "abc", 1)
#' @export
str_last_number_after_mth <- function(string, pattern, m,
                                      decimals = FALSE,
                                      leading_decimals = decimals,
                                      negs = FALSE, sci = FALSE,
                                      big_mark = "",
                                      leave_as_string = FALSE,
                                      commas = FALSE) {
  str_nth_number_after_mth(string, pattern,
    n = -1, m = m,
    decimals = decimals,
    leading_decimals = leading_decimals, sci = sci, big_mark = big_mark,
    negs = negs, leave_as_string = leave_as_string,
    commas = commas
  )
}

#' @rdname str_nth_number_after_mth
#' @examples
#' str_first_number_after_first(string, "abc")
#' @export
str_first_number_after_first <- function(string, pattern,
                                         decimals = FALSE,
                                         leading_decimals = decimals,
                                         negs = FALSE, sci = FALSE,
                                         big_mark = "",
                                         leave_as_string = FALSE,
                                         commas = FALSE) {
  str_nth_number_after_mth(string, pattern,
    n = 1, m = 1,
    decimals = decimals,
    leading_decimals = leading_decimals, sci = sci, big_mark = big_mark,
    negs = negs, leave_as_string = leave_as_string,
    commas = commas
  )
}

#' @rdname str_nth_number_after_mth
#' @examples
#' str_first_number_after_last(string, "abc")
#' @export
str_first_number_after_last <- function(string, pattern,
                                        decimals = FALSE,
                                        leading_decimals = decimals,
                                        negs = FALSE,
                                        sci = FALSE, big_mark = "",
                                        leave_as_string = FALSE,
                                        commas = FALSE) {
  str_nth_number_after_mth(string, pattern,
    n = 1, m = -1,
    decimals = decimals, sci = sci, big_mark = big_mark,
    leading_decimals = leading_decimals,
    negs = negs, leave_as_string = leave_as_string,
    commas = commas
  )
}

#' @rdname str_nth_number_after_mth
#' @examples
#' str_last_number_after_first(string, "abc")
#' @export
str_last_number_after_first <- function(string, pattern, decimals = FALSE,
                                        leading_decimals = decimals,
                                        negs = FALSE,
                                        sci = FALSE, big_mark = "",
                                        leave_as_string = FALSE,
                                        commas = FALSE) {
  str_nth_number_after_mth(string, pattern,
    n = -1, m = 1,
    decimals = decimals, sci = sci, big_mark = big_mark,
    leading_decimals = leading_decimals,
    negs = negs, leave_as_string = leave_as_string,
    commas = commas
  )
}

#' @rdname str_nth_number_after_mth
#' @examples
#' str_last_number_after_last(string, "abc")
#' @export
str_last_number_after_last <- function(string, pattern,
                                       decimals = FALSE,
                                       leading_decimals = decimals,
                                       negs = FALSE,
                                       sci = FALSE, big_mark = "",
                                       leave_as_string = FALSE,
                                       commas = FALSE) {
  str_nth_number_after_mth(string, pattern,
    n = -1, m = -1, decimals = decimals,
    leading_decimals = leading_decimals, sci = sci, big_mark = big_mark,
    negs = negs, leave_as_string = leave_as_string,
    commas = commas
  )
}


# --- FILE: https://raw.githubusercontent.com/rorynolan/strex/master/R/num-before.R ---
#' Find the `n`th number before the `m`th occurrence of a pattern.
#'
#' Given a string, a pattern and natural numbers `n` and `m`, find the `n`th
#' number that comes before the `m`th occurrence of the pattern.
#'
#' @inheritParams str_nth_number_after_mth
#'
#' @return A numeric or character vector.
#'
#' @examples
#' string <- c(
#'   "abc1abc2abc3abc4def5abc6abc7abc8abc9",
#'   "abc1def2ghi3abc4def5ghi6abc7def8ghi9"
#' )
#' str_nth_number_before_mth(string, "def", 1, 1)
#' str_nth_number_before_mth(string, "abc", 2, 3)
#' @family numeric extractors
#' @export
str_nth_number_before_mth <- function(string, pattern, n, m,
                                      decimals = FALSE,
                                      leading_decimals = decimals,
                                      negs = FALSE,
                                      sci = FALSE, big_mark = "",
                                      leave_as_string = FALSE,
                                      commas = FALSE) {
  if (!isFALSE(commas)) {
    lifecycle::deprecate_stop(
      "2.0.0", "strex::str_nth_number_before_mth(commas)",
      details = "Use the `big_mark` argument instead."
    )
  }
  checkmate::assert_flag(leave_as_string)
  if (is_l0_char(string)) {
    return(vector(mode = ifelse(leave_as_string, "character", "numeric")))
  }
  verify_string_pattern_n_m(string, pattern, n, m)
  checkmate::assert_flag(decimals)
  checkmate::assert_flag(leading_decimals)
  checkmate::assert_flag(negs)
  checkmate::assert_flag(sci)
  checkmate::assert_character(big_mark)
  string %>%
    str_before_nth(pattern, m) %>%
    str_nth_number(n,
      decimals = decimals, leading_decimals = leading_decimals,
      negs = negs, leave_as_string = leave_as_string, sci = sci,
      big_mark = big_mark
    )
}

#' @rdname str_nth_number_before_mth
#' @examples
#' str_nth_number_before_first(string, "def", 2)
#' @export
str_nth_number_before_first <- function(string, pattern, n,
                                        decimals = FALSE,
                                        leading_decimals = decimals,
                                        negs = FALSE,
                                        sci = FALSE, big_mark = "",
                                        leave_as_string = FALSE,
                                        commas = FALSE) {
  str_nth_number_before_mth(string, pattern,
    n = n, m = 1,
    decimals = decimals, sci = sci, big_mark = big_mark,
    leading_decimals = leading_decimals,
    negs = negs, leave_as_string = leave_as_string,
    commas = commas
  )
}

#' @rdname str_nth_number_before_mth
#' @examples
#' str_nth_number_before_last(string, "def", -1)
#' @export
str_nth_number_before_last <- function(string, pattern, n,
                                       decimals = FALSE,
                                       leading_decimals = decimals,
                                       negs = FALSE,
                                       sci = FALSE, big_mark = "",
                                       leave_as_string = FALSE,
                                       commas = FALSE) {
  str_nth_number_before_mth(string, pattern,
    n = n, m = -1,
    decimals = decimals,
    leading_decimals = leading_decimals, sci = sci, big_mark = big_mark,
    negs = negs, leave_as_string = leave_as_string,
    commas = commas
  )
}

#' @rdname str_nth_number_before_mth
#' @examples
#' str_first_number_before_mth(string, "abc", 2)
#' @export
str_first_number_before_mth <- function(string, pattern, m,
                                        decimals = FALSE,
                                        leading_decimals = decimals,
                                        negs = FALSE,
                                        sci = FALSE, big_mark = "",
                                        leave_as_string = FALSE,
                                        commas = FALSE) {
  str_nth_number_before_mth(string, pattern,
    n = 1, m = m,
    decimals = decimals,
    leading_decimals = leading_decimals, sci = sci, big_mark = big_mark,
    negs = negs, leave_as_string = leave_as_string,
    commas = commas
  )
}

#' @rdname str_nth_number_before_mth
#' @examples
#' str_last_number_before_mth(string, "def", 1)
#' @export
str_last_number_before_mth <- function(string, pattern, m,
                                       decimals = FALSE,
                                       leading_decimals = decimals,
                                       negs = FALSE,
                                       sci = FALSE, big_mark = "",
                                       leave_as_string = FALSE,
                                       commas = FALSE) {
  str_nth_number_before_mth(string, pattern,
    n = -1, m = m,
    decimals = decimals,
    leading_decimals = leading_decimals, sci = sci, big_mark = big_mark,
    negs = negs, leave_as_string = leave_as_string,
    commas = commas
  )
}

#' @rdname str_nth_number_before_mth
#' @examples
#' str_first_number_before_first(string, "def")
#' @export
str_first_number_before_first <- function(string, pattern,
                                          decimals = FALSE,
                                          leading_decimals = decimals,
                                          negs = FALSE,
                                          sci = FALSE, big_mark = "",
                                          leave_as_string = FALSE,
                                          commas = FALSE) {
  str_nth_number_before_mth(string, pattern,
    n = 1, m = 1,
    decimals = decimals,
    leading_decimals = leading_decimals, sci = sci, big_mark = big_mark,
    negs = negs, leave_as_string = leave_as_string,
    commas = commas
  )
}

#' @rdname str_nth_number_before_mth
#' @examples
#' str_first_number_before_last(string, "def")
#' @export
str_first_number_before_last <- function(string, pattern,
                                         decimals = FALSE,
                                         leading_decimals = decimals,
                                         negs = FALSE,
                                         sci = FALSE, big_mark = "",
                                         leave_as_string = FALSE,
                                         commas = FALSE) {
  str_nth_number_before_mth(string, pattern,
    n = 1, m = -1,
    decimals = decimals,
    leading_decimals = leading_decimals, sci = sci, big_mark = big_mark,
    negs = negs, leave_as_string = leave_as_string,
    commas = commas
  )
}

#' @rdname str_nth_number_before_mth
#' @examples
#' str_last_number_before_first(string, "def")
#' @export
str_last_number_before_first <- function(string, pattern,
                                         decimals = FALSE,
                                         leading_decimals = decimals,
                                         negs = FALSE,
                                         sci = FALSE, big_mark = "",
                                         leave_as_string = FALSE,
                                         commas = FALSE) {
  str_nth_number_before_mth(string, pattern,
    n = -1, m = 1,
    decimals = decimals,
    leading_decimals = leading_decimals, sci = sci, big_mark = big_mark,
    negs = negs, leave_as_string = leave_as_string,
    commas = commas
  )
}

#' @rdname str_nth_number_before_mth
#' @examples
#' str_last_number_before_last(string, "def")
#' @export
str_last_number_before_last <- function(string, pattern,
                                        decimals = FALSE,
                                        leading_decimals = decimals,
                                        negs = FALSE,
                                        sci = FALSE, big_mark = "",
                                        leave_as_string = FALSE,
                                        commas = FALSE) {
  str_nth_number_before_mth(string, pattern,
    n = -1, m = -1,
    decimals = decimals,
    leading_decimals = leading_decimals, sci = sci, big_mark = big_mark,
    negs = negs, leave_as_string = leave_as_string,
    commas = commas
  )
}


# --- FILE: https://raw.githubusercontent.com/rorynolan/strex/master/R/remove.R ---
#' Remove the quoted parts of a string.
#'
#' If any parts of a string are quoted (between quotation marks), remove those
#' parts of the string, including the quotes. Run the examples and you'll know
#' exactly how this function works.
#'
#' @param string A character vector.
#'
#' @return A character vector.
#' @examples
#' string <- "\"abc\"67a\'dk\'f"
#' cat(string)
#' str_remove_quoted(string)
#' @family removers
#' @export
str_remove_quoted <- function(string) {
  if (is_l0_char(string)) {
    return(character())
  }
  checkmate::assert_character(string)
  string <- str_replace_all(string, "(?:\".*?\")", "")
  string <- str_replace_all(string, "(?:\'.*?\')", "")
  string
}


# --- FILE: https://raw.githubusercontent.com/rorynolan/strex/master/R/singleize.R ---
#' Remove back-to-back duplicates of a pattern in a string.
#'
#' If a string contains a given pattern duplicated back-to-back a number of
#' times, remove that duplication, leaving the pattern appearing once in that
#' position (works if the pattern is duplicated in different parts of a string,
#' removing all instances of duplication). This is vectorized over string and
#' pattern.
#'
#' @inheritParams str_after_nth
#'
#' @return A character vector.
#'
#' @examples
#' str_singleize("abc//def", "/")
#' str_singleize("abababcabab", "ab")
#' str_singleize(c("abab", "cdcd"), "cd")
#' str_singleize(c("abab", "cdcd"), c("ab", "cd"))
#' @family removers
#' @export
str_singleize <- function(string, pattern) {
  if (is_l0_char(string)) {
    return(character())
  }
  verify_string_pattern(string, pattern)
  dup_patt <- str_c("(", pattern, ")+")
  str_replace_all(string, dup_patt, pattern)
}


# --- FILE: https://raw.githubusercontent.com/rorynolan/strex/master/R/split-by-nums.R ---
#' Split a string by its numeric characters assuming no number ambiguity.
#'
#' Sometimes the strings have ambiguous numbers in them e.g. 2.5.3. These have
#' to be dealt with by strex (which it does by returning `NA` in those cases).
#' This helper to `str_split_by_numbers()` assumes that the input has
#' no such ambiguities.
#'
#' @param string A character vector.
#' @param num_pattern The regex defining a numer in the current context.
#'
#' @return A list of character vectors.
#'
#' @noRd
str_split_by_numbers_no_ambigs <- function(string, num_pattern) {
  num_locs <- str_locate_all(string, num_pattern)
  fullocated_locs <- lst_fullocate(num_locs,
    start = 1, end = stringr::str_length(string)
  )
  stringi::stri_sub_all(string, fullocated_locs)
}

#' Split a string by its numeric characters.
#'
#' Break a string wherever you go from a numeric character to a non-numeric or
#' vice-versa. Keep the whole string, just split it up. Vectorised over
#' `string`.
#'
#' @inheritParams str_extract_numbers
#'
#' @return A list of character vectors.
#'
#' @examples
#' str_split_by_numbers(c("abc123def456.789gh", "a1b2c344"))
#' str_split_by_numbers("abc123def456.789gh", decimals = TRUE)
#' str_split_by_numbers(c("22", "1.2.3"), decimals = TRUE)
#' @family splitters
#' @export
str_split_by_numbers <- function(string, decimals = FALSE,
                                 leading_decimals = FALSE, negs = FALSE,
                                 sci = FALSE, big_mark = "",
                                 commas = FALSE) {
  if (!isFALSE(commas)) {
    lifecycle::deprecate_stop(
      "2.0.0", "strex::str_split_by_numbers(commas)",
      details = "Use the `big_mark` argument instead."
    )
  }
  if (is_l0_char(string)) {
    return(list())
  }
  checkmate::assert_character(string)
  checkmate::assert_flag(decimals)
  checkmate::assert_flag(leading_decimals)
  checkmate::assert_flag(negs)
  checkmate::assert_flag(sci)
  checkmate::assert_string(big_mark)
  num_pattern <- num_regex(
    decimals = decimals, leading_decimals = leading_decimals,
    negs = negs, sci = sci, big_mark = big_mark
  )
  ambig_pattern <- ambig_num_regex(
    decimals = decimals,
    leading_decimals = leading_decimals,
    sci = sci, big_mark = big_mark
  )
  ambigs <- num_ambigs(string,
    decimals = decimals,
    leading_decimals = leading_decimals, sci = sci, big_mark = big_mark
  )
  out <- vector(mode = "list", length = length(string))
  if (any(ambigs)) {
    ambig_warn(string, ambigs, ambig_pattern)
    out[ambigs] <- NA_character_
    not_ambigs <- !ambigs
    out[not_ambigs] <- str_split_by_numbers_no_ambigs(
      string[not_ambigs],
      num_pattern
    )
  } else {
    out[] <- str_split_by_numbers_no_ambigs(string, num_pattern)
  }
  out
}


# --- FILE: https://raw.githubusercontent.com/rorynolan/strex/master/R/strex-package.R ---
#' @useDynLib strex, .registration = TRUE
#' @import stringr
#' @importFrom stringi stri_write_lines
#' @importFrom magrittr '%>%'
#' @importFrom stats as.dendrogram
#' @importFrom utils head
NULL


## quiets concerns of R CMD check re: the .'s that appear in pipelines
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("."))
}

.onUnload <- function(libpath) library.dynam.unload("strex", libpath)


#' `strex`: extra string manipulation functions
#'
#' There are some things that I wish were easier with the `stringr` or `stringi`
#' packages. The foremost of these is the extraction of numbers from strings.
#' `stringr` makes you figure out the regex for yourself; `strex` takes care of
#' this for you. There are many more useful functionalities in `strex`. In
#' particular, there's a `match_arg()` function which is more flexible than the
#' base `match.arg()`. Contributions to this package are encouraged: it is
#' intended as a miscellany of string manipulation functions which cannot be
#' found in `stringi` or `stringr`.
#'
#' @name strex
#' @aliases strex-package
#' @references Rory Nolan and Sergi Padilla-Parra (2017). filesstrings: An R
#'   package for file and string manipulation. The Journal of Open Source
#'   Software, 2(14).  \doi{10.21105/joss.00260}.
"_PACKAGE"


# --- FILE: https://raw.githubusercontent.com/rorynolan/strex/master/R/to-vec.R ---
#' Convert a string to a vector of characters
#'
#' Go from a string to a vector whose \eqn{i}th element is the \eqn{i}th
#' character in the string.
#'
#' @inheritParams str_after_nth
#'
#' @return A character vector.
#'
#' @examples
#' str_to_vec("abcdef")
#' @family converters
#' @export
str_to_vec <- function(string) {
  if (is_l0_char(string)) {
    return(character())
  }
  checkmate::assert_character(string)
  strsplit(string, NULL)[[1]]
}


# --- FILE: https://raw.githubusercontent.com/rorynolan/strex/master/R/trim.R ---
#' Trim something other than whitespace
#'
#' The `stringi` and `stringr` packages let you trim whitespace, but
#' what if you want to trim something else from either (or both) side(s) of a
#' string? This function lets you select which pattern to trim and from which
#' side(s).
#'
#' @inheritParams str_after_nth
#' @param side Which side do you want to trim from? `"both"` is the
#'   default, but you can also have just either `"left"` or `"right"`
#'   (or optionally the shortened `"b"`, `"l"` and `"r"`).
#'
#' @return A string.
#'
#' @examples
#' str_trim_anything("..abcd.", ".", "left")
#' str_trim_anything("..abcd.", coll("."), "left")
#' str_trim_anything("-ghi--", "-", "both")
#' str_trim_anything("-ghi--", "-")
#' str_trim_anything("-ghi--", "-", "right")
#' str_trim_anything("-ghi--", "--")
#' str_trim_anything("-ghi--", "i-+")
#' @family removers
#'
#' @export
str_trim_anything <- function(string, pattern, side = "both") {
  if (is_l0_char(string)) {
    return(character())
  }
  verify_string_pattern(string, pattern, boundary_allowed = FALSE)
  out <- string
  checkmate::assert_string(side)
  side <- match_arg(side, c("both", "left", "right"), ignore_case = TRUE)
  type <- "regex"
  if (inherits(pattern, "stringr_fixed")) {
    type <- "fixed"
  } else if (inherits(pattern, "stringr_coll")) {
    type <- "coll"
  } else {
    bad_starts <- str_starts(pattern, "\\(*\\^")
    bad_ends <- str_ends(pattern, "\\$\\)*")
    if (any(bad_starts)) {
      rlang::abort(
        c(
          paste(
            "In `str_trim_anything()`, don't start your regular expression",
            "patterns with '^' to match the start of the string.",
            "The trimming by definition is happening at the edges."
          ),
          x = str_glue_data(
            list(
              pattern = pattern,
              first_bad = which.max(bad_starts)
            ),
            "Element {first_bad} of your pattern, ",
            "'{pattern[first_bad]}' is the first offender.",
          )
        )
      )
    } else if (any(bad_ends)) {
      rlang::abort(
        c(
          paste(
            "In `str_trim_anything()`, don't end your regular expression",
            "patterns with '$' to match the end of the string.",
            "The trimming by definition is happening at the edges."
          ),
          x = str_glue_data(
            list(pattern = pattern, first_bad = which.max(bad_ends)),
            "Element {first_bad} of your pattern, '{pattern[first_bad]}' ",
            "is the first offender."
          )
        )
      )
    }
    pattern <- str_c("(", pattern, ")+")
    pattern <- switch(side,
      left = str_c("^", pattern),
      right = str_c(pattern, "$"),
      pattern
    )
  }
  if (side == "both") {
    out <- string %>%
      str_trim_anything(pattern, "left") %>%
      str_trim_anything(pattern, "right")
  } else if (type == "regex") {
    out <- str_replace(string, pattern, "")
  } else if (side == "left") {
    starts <- which(str_starts(out, pattern))
    while (any(starts)) {
      out[starts] <- switch(type,
        fixed = stringi::stri_replace_first_fixed(
          out[starts],
          pattern[ifelse(length(pattern) == 1, 1, starts)],
          ""
        ),
        coll = stringi::stri_replace_first_coll(
          out[starts],
          pattern[ifelse(length(pattern) == 1, 1, starts)],
          ""
        )
      )
      starts <- starts[str_starts(out[starts], pattern)]
    }
  } else if (side == "right") {
    ends <- which(str_ends(out, pattern))
    while (length(ends)) {
      out[ends] <- switch(type,
        fixed = stringi::stri_replace_last_fixed(
          out[ends],
          pattern[ifelse(length(pattern) == 1, 1, ends)],
          ""
        ),
        coll = stringi::stri_replace_last_coll(
          out[ends],
          pattern[ifelse(length(pattern) == 1, 1, ends)],
          ""
        )
      )
      ends <- ends[str_ends(out[ends], pattern)]
    }
  }
  out
}


# --- FILE: https://raw.githubusercontent.com/rorynolan/strex/master/R/utils.R ---
#' Assert that two objects have compatible lengths.
#'
#' Compatible means that either both have length less than or equal to 1, or
#' both have the same length.
#'
#' @param x, y Objects
#'
#' @return `TRUE`, invisibly if the lengths are compatible. Otherwise an error
#'   is thrown.
#'
#' @noRd
assert_compatible_lengths <- function(x, y) {
  x_sym <- rlang::ensym(x)
  y_sym <- rlang::ensym(y)
  checkmate::assert_vector(x)
  checkmate::assert_vector(y)
  if (length(x) > 1 && length(y) > 1) {
    if (length(x) != length(y)) {
      rlang::abort(
        c(
          str_glue(
            "If both `{x_sym}` and `{y_sym}` have lengths greater ",
            "than 1, then their lengths must be equal."
          ),
          x = str_glue("`{x_sym}` has length {length(x)}."),
          x = str_glue("`{y_sym}` has length {length(y)}.")
        )
      )
    }
  }
  invisible(TRUE)
}

#' Assert that the elements of a list have a common length.
#'
#' @param lst A list.
#'
#' @return `TRUE` (invisibly) if the elements have a common length. Otherwise,
#'   an error is thrown.
#'
#' @noRd
assert_lst_elems_common_length <- function(lst) {
  lst_sym <- rlang::ensym(lst)
  checkmate::assert_list(lst)
  l <- length(lst)
  if (l <= 1) {
    return(invisible(TRUE))
  }
  good <- lst_elems_common_length(lst, as.double(l))
  if (!good) {
    rlang::abort(
      str_glue("Elements of `{lst_sym}` do not have a common length.")
    )
  }
  invisible(TRUE)
}

#' Generate an error due to an incompatible combination of arguemnt lengths.
#'
#' @param string A character vector.
#' @param sym Another argument to a strex function.
#' @param replacement_sym A string to replace sym in the error message.
#'
#' @noRd
err_string_len <- function(string, sym, replacement_sym = NULL) {
  sym_sym <- rlang::enexpr(sym)
  if (!is.null(replacement_sym)) {
    sym_str <- replacement_sym
  } else {
    sym_str <- as.character(sym_sym)
  }
  rlang::abort(
    c(
      str_glue(
        "When `string` has length greater than 1, `{sym_str}` ",
        "must either be length 1 or have the same length as `string`."
      ),
      x = str_glue("Your `string` has length {length(string)}."),
      x = str_glue("Your `{sym_str}` has length {length(sym)}.")
    )
  )
}

verify_string_pattern <- function(string, pattern, boundary_allowed = TRUE) {
  checkmate::assert_character(string, min.len = 1)
  checkmate::assert_flag(boundary_allowed)
  if (boundary_allowed) {
    if (inherits(pattern, "stringr_boundary")) {
      checkmate::assert_character(pattern, min.len = 0)
    } else {
      checkmate::assert_character(pattern, min.len = 1)
    }
  } else if (inherits(pattern, "stringr_boundary")) {
    rlang::abort("Function cannot handle a `pattern` of type 'boundary'.")
  } else {
    checkmate::assert_character(pattern, min.len = 1)
  }
  if (length(pattern) > 1 && length(string) > 1 &&
    length(pattern) != length(string)) {
    err_string_len(string, pattern)
  }
  invisible(TRUE)
}

verify_string_n <- function(string, n, replacement_n_sym = NULL) {
  checkmate::assert_character(string, min.len = 1)
  checkmate::assert_integerish(n, min.len = 1)
  if (length(n) > 1 && length(string) > 1 &&
    length(n) != length(string)) {
    err_string_len(string, n, replacement_n_sym)
  }
  invisible(TRUE)
}

verify_string_pattern_n <- function(string, pattern, n,
                                    replacement_n_sym = NULL) {
  if (!is.null(replacement_n_sym)) {
    n_sym_str <- replacement_n_sym
  } else {
    n_sym_str <- as.character(rlang::ensym(n))
  }
  verify_string_n(string, n, replacement_n_sym)
  verify_string_pattern(string, pattern)
  if (length(pattern) > 1 && length(n) > 1 &&
    length(pattern) != length(n)) {
    rlang::abort(
      c(
        paste(
          "If `pattern` and `n` both have length greater than 1,",
          "their lengths must be equal."
        ),
        x = str_glue("Your `pattern` has length {length(pattern)}."),
        x = str_glue("Your `{n_sym_str}` has length {length(n)}.")
      )
    )
  }
  invisible(TRUE)
}

verify_string_pattern_n_m <- function(string, pattern, n, m) {
  verify_string_pattern_n(string, pattern, n)
  checkmate::assert_integerish(m, min.len = 1)
  verify_string_pattern_n(string, pattern, m, "m")
  if (length(n) > 1 && length(m) > 1 &&
    length(n) != length(m)) {
    rlang::abort(
      c(
        paste(
          "If `n` and `m` both have length greater than 1,",
          "their lengths must be equal."
        ),
        x = str_glue("Your `n` has length {length(n)}."),
        x = str_glue("Your `m` has length {length(m)}.")
      )
    )
  }
  invisible(TRUE)
}

is_l0_char <- function(x) isTRUE(checkmate::check_character(x, max.len = 0))


# --- FILE: https://raw.githubusercontent.com/rorynolan/strex/master/junk/R/utils.R ---
#' Get the nth element of each vector in a list of numeric or character vectors.
#'
#' These are faster implementations of procedures that could very easily be done
#' with [purrr::map_dbl] or [purrr::map_chr].
#'
#' @param char_list A list of character vectors.
#' @param n The index of the element that you want from each vector. If
#'   `char_list` (or `num_list`) is of length 1, this can be any length and
#'   those indices will be extracted from `char_list[[1]]` (or `num_list[[1]]`).
#'   Otherwise, this must either be of length 1 or the same length as
#'   `char_list`. All of this is to say that the function is vectorised over
#'   this argument.
#'
#' @return A list.
#'
#' @examples
#' str_list_nth_elems_(list(c("a", "b", "c"), c("d", "f", "a")), 2)
#' num_list_nth_elems_(list(1:5, 0:2), 4)
#' @noRd
str_list_nth_elems <- function(char_list, n) {
  checkmate::assert_list(char_list, min.len = 1)
  checkmate::assert_integerish(n, min.len = 1, lower = 1)
  lcl <- length(char_list)
  ln <- length(n)
  if (lcl > 1 && ln > 1 && lcl != ln) {
    custom_stop("
      If both `char_list` and `n` have lengths greater than 1,
      then their lengths must be equal.
      ", "
      Your `char_list` has length {length(char_list)} and
      your `n` has length {length(n)}.
    ")
  }
  str_list_nth_elems_helper(char_list, n)
}

interleave_char_lists <- function(strings1, strings2) {
  checkmate::assert_list(strings1)
  checkmate::assert_list(strings2)
  checkmate::assert_true(length(strings1) == length(strings2))
  .Call("interleave_char_lists_C", strings1, strings2,
    PACKAGE = "strex"
  )
}

#' Remove empty strings from a character list.
#'
#' @param char_list A list of character vectors.
#'
#' @return A list of character vectors.
#'
#' @examples
#' str_list_remove_empties(list(c("a", "", "b"), "gg", c("", 1, "")))
#' @noRd
str_list_remove_empties <- function(char_list) {
  checkmate::assert_list(char_list)
  .Call("str_list_remove_empties_C", char_list, PACKAGE = "strex")
}


#' @rdname str_list_nth_elems
#' @param num_list A list of numeric vectors.
#' @noRd
num_list_nth_elems <- function(num_list, n) {
  checkmate::assert_list(num_list, min.len = 1)
  checkmate::assert_integerish(n, min.len = 1)
  lnl <- length(num_list)
  ln <- length(n)
  if (lnl > 1 && ln > 1 && lnl != ln) {
    custom_stop("
      If both `num_list` and `n` have lengths greater than 1,
      then their lengths must be equal.
      ", "
      Your `num_list` has length {length(num_list)} and
      your `n` has length {length(n)}.
      ")
  }
  num_list_nth_elems_(num_list, n)
}

#' Construct the bullet point bits for `custom_stop()`.
#'
#' @param string The message for the bullet point.
#'
#' @return A string with the bullet-pointed message nicely formatted for the
#'   console.
#'
#' @noRd
custom_bullet <- function(string) {
  checkmate::assert_string(string)
  string %>%
    stringr::str_replace_all("\\s+", " ") %>%
    {
      glue::glue("    * {.}")
    }
}

custom_condition_prep <- function(main_message, ..., .envir = parent.frame()) {
  checkmate::assert_string(main_message)
  main_message %<>%
    stringr::str_replace_all("\\s+", " ") %>%
    glue::glue(.envir = .envir) %>%
    stringr::str_trim()
  out <- main_message
  dots <- unlist(list(...))
  if (length(dots)) {
    if (!is.character(dots)) {
      stop("\nThe arguments in ... must all be of character type.")
    }
    dots %<>%
      vapply(glue::glue, character(1), .envir = .envir) %>%
      vapply(custom_bullet, character(1))
    out %<>% {
      glue::glue_collapse(c(., dots), sep = "\n")
    }
  }
  out
}

#' Nicely formatted error message.
#'
#' Format an error message with bullet-pointed sub-messages with nice
#' line-breaks.
#'
#' Arguments should be entered as `glue`-style strings.
#'
#' @param main_message The main error message.
#' @param ... Bullet-pointed sub-messages.
#'
#' @noRd
custom_stop <- function(main_message, ..., .envir = parent.frame()) {
  rlang::abort(custom_condition_prep(main_message, ..., .envir = .envir))
}

custom_warn <- function(main_message, ..., .envir = parent.frame()) {
  rlang::warn(custom_condition_prep(main_message, ..., .envir = .envir))
}

#' Generate an error due to an incompatible combination of arguemnt lengths.
#'
#' @param string A character vector.
#' @param sym Another argument to a strex function.
#' @param replacement_sym A string to replace sym in the error message.
#'
#' @noRd
err_string_len <- function(string, sym, replacement_sym = NULL) {
  sym_sym <- rlang::enexpr(sym)
  sym_str <- as.character(sym_sym)
  if (!is.null(replacement_sym)) sym_str <- replacement_sym
  sym_len <- length(sym)
  custom_stop(
    "
    When `string` has length greater than 1,
    `{sym_str}` must either be length 1 or have the same length as `string`.
    ",
    "Your `string` has length {length(string)}.",
    "Your `{sym_str}` has length {sym_len}."
  )
}

verify_string_pattern <- function(string, pattern, boundary_allowed = TRUE) {
  checkmate::assert_character(string, min.len = 1)
  checkmate::assert_character(pattern, min.len = 1)
  checkmate::assert_flag(boundary_allowed)
  if (length(pattern) > 1 && length(string) > 1 &&
    length(pattern) != length(string)) {
    err_string_len(string, pattern)
  }
  if (!boundary_allowed && all(c("boundary", "pattern") %in% class(pattern))) {
    "Function cannot handle a `pattern` of type 'boundary'."
  }
  invisible(TRUE)
}

verify_string_n <- function(string, n, replacement_n_sym = NULL) {
  checkmate::assert_character(string, min.len = 1)
  checkmate::assert_integerish(n, min.len = 1)
  if (length(n) > 1 && length(string) > 1 &&
    length(n) != length(string)) {
    err_string_len(string, n, replacement_n_sym)
  }
  invisible(TRUE)
}

verify_string_pattern_n <- function(string, pattern, n,
                                    replacement_n_sym = NULL) {
  verify_string_pattern(string, pattern)
  verify_string_n(string, n, replacement_n_sym)
  n_sym_str <- "n"
  if (!is.null(replacement_n_sym)) n_sym_str <- replacement_n_sym
  if (length(pattern) > 1 && length(n) > 1 &&
    length(pattern) != length(n)) {
    custom_stop(
      "
                If `pattern` and `{n_sym_str}` both have length greater than 1,
                their lengths must be equal.
                ",
      "Your `pattern` has length {length(pattern)}.",
      "Your `{n_sym_str}` has length {length(n)}."
    )
  }
  invisible(TRUE)
}

verify_string_pattern_n_m <- function(string, pattern, n, m) {
  verify_string_pattern_n(string, pattern, n)
  checkmate::assert_integerish(m, min.len = 1)
  verify_string_pattern_n(string, pattern, m, "m")
  if (length(n) > 1 && length(m) > 1 &&
    length(n) != length(m)) {
    custom_stop(
      "
                If `n` and `m` both have length greater than 1,
                their lengths must be equal.
                ",
      "Your `n` has length {length(n)}.",
      "Your `m` has length {length(m)}."
    )
  }
  invisible(TRUE)
}

is_l0_char <- function(x) isTRUE(checkmate::check_character(x, max.len = 0))


# --- FILE: https://raw.githubusercontent.com/rorynolan/strex/master/junk/junk.R ---
#' Group together close adjacent elements of a vector.
#'
#' Given a strictly increasing vector (each element is bigger than the last),
#' group together stretches of the vector where *adjacent* elements are
#' separated by at most some specified distance. Hence, each element in each
#' group has at least one other element in that group that is *close* to it. See
#' the examples.
#' @param x A strictly increasing numeric vector.
#' @param max_gap The biggest allowable gap between adjacent elements for them
#'   to be considered part of the same *group*.
#' @param check Check inputs for validity? Can be turned off for speed if you're
#'   sure your inputs are valid.
#' @return A where each element is one group, as a numeric vector.
#' @examples
#' group_close(1:10, 1)
#' group_close(1:10, 0.5)
#' group_close(c(1, 2, 4, 10, 11, 14, 20, 25, 27), 3)
#' @noRd
group_close <- function(x, max_gap = 1, check = TRUE) {
  dva <- diff(x)
  if (check) {
    checkmate::assert_numeric(x, min.len = 1)
    test <- dva > 0
    if (anyNA(test) || (!all(test))) {
      bad_index <- match(F, test)
      custom_stop(
        "`vec_ascending` must be strictly increasing.",
        "
                  Indices {bad_index} and {bad_index + 1} of `vec_ascending`
                  are respectively {vec_ascending[bad_index]} and
                  {vec_ascending[bad_index + 1]}, therefore `vec_ascending`
                  is not strictly increasing.
                  "
      )
    }
  }
  lva <- length(x)
  if (lva == 1) return(list(x))
  gaps <- dva
  big_gaps <- which(gaps > max_gap)
  nbgaps <- length(big_gaps) # number of big gaps
  if (!nbgaps) return(list(x))
  big_gaps %>% {
    split(x, rep(seq_len(nbgaps + 1), times = c(.[1], diff(c(., lva)))))
  }
}

test_that("group_close works", {
  expect_equal(unname(group_close(1:10, 1)), list(1:10))
  expect_equal(unname(group_close(1:10, 0.5)), as.list(1:10))
  expect_equal(
    unname(group_close(c(1, 2, 4, 10, 11, 14, 20, 25, 27), 3)),
    list(c(1, 2, 4), c(10, 11, 14), 20, c(25, 27))
  )
  expect_error(group_close(integer(0)))
  expect_error(group_close(rep(1, 2)))
  expect_equal(unname(group_close(0)), list(0))
  expect_equal(unname(group_close(c(0, 2))), list(0, 2))
})

#' Locate the braces in a string.
#'
#' Give the positions of `(`, `)`, `[`, `]`, `\{`, `\}` within a string.
#'
#' @param string A character vector
#'
#' @return A list of data frames, one for each member of the string character
#'   vector. Each data frame has a "position" and "brace" column which give the
#'   positions and types of braces in the given string.
#'
#' @examples
#' str_locate_braces(c("a{](kkj)})", "ab(]c{}"))
#' @export
str_locate_braces <- function(string) {
  locations <- str_locate_all(string, "[\\(\\)\\[\\]\\{\\}]") %>%
    int_lst_first_col()
  braces <- str_elems(string, locations)
  lst_df_pos_brace(locations, braces)
}

get_os <- function() {
  sysinf <- Sys.info()
  if (!is.null(sysinf)) {
    os <- sysinf["sysname"]
    if (os == "Darwin") {
      os <- "mac"
    }
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os)) {
      os <- "mac"
    }
    if (grepl("linux-gnu", R.version$os)) {
      os <- "linux"
    }
  }
  tolower(os)
}


# --- FILE: https://raw.githubusercontent.com/rorynolan/strex/master/tests/spelling.R ---
if (requireNamespace("spelling", quietly = TRUE)) {
  spelling::spell_check_test(
    vignettes = TRUE, error = FALSE,
    skip_on_cran = TRUE
  )
}


# --- FILE: https://raw.githubusercontent.com/rorynolan/strex/master/tests/testthat.R ---
library(testthat)
library(strex)

test_check("strex")


# --- FILE: https://raw.githubusercontent.com/rorynolan/strex/master/tests/testthat/test-after.R ---
test_that("str_after_nth works", {
  string <- "ab..cd..de..fg..h"
  expect_equal(str_after_nth(string, "\\.\\.", 3), "fg..h",
    ignore_attr = TRUE
  )
  expect_equal(str_after_first(string, "\\.\\."), "cd..de..fg..h",
    ignore_attr = TRUE
  )
  expect_equal(str_after_last(string, "\\.\\."), "h",
    ignore_attr = TRUE
  )
  expect_equal(str_before_first(string, "e"), "ab..cd..d",
    ignore_attr = TRUE
  )
  string <- c("abc", "xyz.zyx")
  expect_equal(str_after_first(string, "."), str_sub(string, 2))
  expect_equal(str_after_first(string, coll(".")), c(NA, "zyx"))
  expect_equal(str_after_first(character(), 1:3), character())
  expect_equal(str_after_nth("abc", "b", c(0, 1)), c(NA, "c"))
  string <- "abxxcdxxdexxfgxxh"
  expect_equal(str_after_nth(string, "e", 1:2), c("xxfgxxh", NA))
  expect_snapshot_error(str_after_nth(c("a"), c("a", "b"), 1:3))
})


# --- FILE: https://raw.githubusercontent.com/rorynolan/strex/master/tests/testthat/test-alphord.R ---
test_that("str_alphord_nums works", {
  strings <- paste0("abc", 1:12)
  expect_equal(
    str_alphord_nums(strings),
    str_c("abc", c(paste0(0, 1:9), 10:12))
  )
  expect_equal(
    str_alphord_nums(c("01abc9def55", "5abc10def777", "99abc4def4")),
    c("01abc09def055", "05abc10def777", "99abc04def004")
  )
  expect_equal(
    str_alphord_nums(c("abc9def55", "abc10def7")),
    c("abc09def55", "abc10def07")
  )
  expect_equal(
    str_alphord_nums(c("abc9def55", "abc10def777", "abc4def4")),
    c("abc09def055", "abc10def777", "abc04def004")
  )
  expect_snapshot_error(
    str_alphord_nums(c("abc9def55", "abc9def5", "abc10xyz7"))
  )
  expect_error(
    str_alphord_nums(c("abc9def55", "9abc10def7")),
    "The strings must all have the same number of numbers."
  )
  expect_snapshot_error(str_alphord_nums(c("0abc9def55g", "abc10def7g0")))
  expect_error(
    str_alphord_nums("abc"),
    "Some of the input strings have no numbers in them."
  )
  expect_equal(str_alphord_nums(1:10), c(paste0(0, 1:9), 10))
  expect_equal(str_alphord_nums(character()), character())
})


# --- FILE: https://raw.githubusercontent.com/rorynolan/strex/master/tests/testthat/test-arg-match.R ---
test_that("match_arg() works", {
  expect_equal(match_arg("ab", c("abcdef", "defgh")), "abcdef")
  expect_error(match_arg("abcdefg", c("Abcdef", "defg")), "not a prefix of any")
  expect_equal(
    match_arg("ab", c("Abcdef", "defgh"), ignore_case = TRUE),
    "Abcdef"
  )
  expect_equal(match_arg("ab", c("xyz", "Abcdef", "defgh"),
    ignore_case = TRUE, index = TRUE
  ), 2)
  choices <- c("Apples", "Pears", "Bananas", "Oranges")
  expect_equal(match_arg("A", choices), "Apples")
  expect_equal(match_arg("B", choices, index = TRUE), 3)
  expect_equal(
    match_arg(c("b", "a"), choices,
      several_ok = TRUE,
      ignore_case = TRUE
    ),
    c("Bananas", "Apples")
  )
  expect_equal(
    match_arg(c("b", "a"), choices,
      ignore_case = TRUE, index = TRUE,
      several_ok = TRUE
    ),
    c(3, 1)
  )
  choices <- c(choices, "Avocados", "Apricots")
  expect_snapshot_error(match_arg("A", choices, ignore_case = FALSE))
  x <- "a"
  expect_snapshot_error(match_arg(x, choices, ignore_case = TRUE))
  expect_error(
    match_arg(c("A", "a"), choices),
    str_c(
      "`arg` must have length 1.+",
      ". Your `arg` has length 2.+",
      ". To use an `arg` with length greater than one, use.+",
      "`several_ok = TRUE`."
    )
  )
  choices <- c(choices, "bananas")
  expect_snapshot_error(match_arg("p", choices, ignore_case = TRUE))
  choices <- c(choices, "Pears")
  expect_snapshot_error(match_arg("p", choices, ignore_case = TRUE))
  expect_equal(match_arg("ab", c("ab", "abc")), "ab")
  y <- "a"
  expect_snapshot_error(match_arg(y, as.character(1:51)))
  word <- function(w = c("abacus", "baseball", "candy")) {
    match_arg(w)
  }
  expect_equal(word("b"), "baseball")
  expect_equal(word(), "abacus")
  word <- function(w = c("abacus", "baseball", "candy")) {
    match_arg(w, several_ok = TRUE)
  }
  expect_equal(word("c"), "candy")
  expect_equal(word(), c("abacus", "baseball", "candy"))
  word <- function(w = c("abacus", "baseball", "candy")) {
    match_arg(as.character(w), several_ok = TRUE)
  }
  expect_snapshot_error(word())
  word <- function(w = 1:3) {
    match_arg(w, several_ok = TRUE)
  }
  expect_snapshot_error(word())
  word <- function(w = c("abacus", "baseball", "candy")) {
    x <- "a"
    match_arg(x, several_ok = TRUE)
  }
  expect_snapshot_error(word())
})


# --- FILE: https://raw.githubusercontent.com/rorynolan/strex/master/tests/testthat/test-before.R ---
test_that("str_before_last_dot works", {
  expect_equal(str_before_last_dot(c("spreadsheet1.csv", "doc2.doc")),
    c("spreadsheet1", "doc2"),
    ignore_attr = TRUE
  )
})

test_that("`str_before_nth()` works", {
  string <- "ab..cd..de..fg..h"
  expect_equal(str_before_nth(string, "\\.", -3), "ab..cd..de.",
    ignore_attr = TRUE
  )
  expect_equal(str_before_nth(string, ".", -3), "ab..cd..de..fg",
    ignore_attr = TRUE
  )
  expect_equal(str_before_nth(rep(string, 2), fixed("."), -3),
    rep("ab..cd..de.", 2),
    ignore_attr = TRUE
  )
  expect_equal(str_before_last(rep(string, 2), fixed(".")),
    rep("ab..cd..de..fg.", 2),
    ignore_attr = TRUE
  )
  expect_equal(str_before_last(character(), 1:3), character())
  string <- "abxxcdxxdexxfgxxh"
  expect_equal(str_before_nth(string, "e", 1:2), c("abxxcdxxd", NA))
})


# --- FILE: https://raw.githubusercontent.com/rorynolan/strex/master/tests/testthat/test-c-wrappers.R ---
test_that("`chr_lst_nth_elems()` works", {
  expect_equal(
    chr_lst_nth_elems(list(c("a", "b"), c("x", "y", "z")), 2:3),
    c("b", "z")
  )
})

test_that("`chr_lst_remove_empties()` works", {
  expect_equal(
    chr_lst_remove_empties(
      list(
        c("", "a", "", "", "b", "c", ""),
        c("", ""), c("xy")
      )
    ),
    list(c("a", "b", "c"), character(0), "xy")
  )
})

test_that("`chr_vec_remove_empties()` works", {
  expect_equal(
    chr_vec_remove_empties(c("", "a", "", "", "b", "c", "")), c("a", "b", "c")
  )
})

test_that("`dbl_lst_nth_elems()` works", {
  expect_equal(
    dbl_lst_nth_elems(list(c(1.2, 2.3, 3.4), c(6.7, 8.9)), 2),
    c(2.3, 8.9)
  )
})

test_that("`int_lst_cbind()` works", {
  expect_equal(
    int_lst_cbind(list(1:4, 6:9, c(3L, 1L, 5L, 5L))),
    cbind(1:4, 6:9, c(3L, 1L, 5L, 5L))
  )
})

test_that("`int_lst_rbind()` works", {
  expect_equal(
    int_lst_rbind(list(1:4, 6:9, c(3L, 1L, 5L, 5L))),
    rbind(1:4, 6:9, c(3L, 1L, 5L, 5L))
  )
})

test_that("`int_mat_lst_cbind_nth_cols()` works", {
  expect_equal(
    int_mat_lst_cbind_nth_cols(
      list(
        matrix(1:12, nrow = 3),
        matrix(2:13, nrow = 3),
        matrix(1:6 + 20L, ncol = 2)
      ),
      c(1, 2, 5)
    ),
    cbind(1:3, 5:7, NA_integer_)
  )
  expect_equal(
    int_mat_lst_cbind_nth_cols(
      list(
        matrix(1:12, nrow = 3),
        matrix(2:13, nrow = 3),
        matrix(1:6 + 20L, nrow = 3)
      ),
      2
    ),
    cbind(4:6, 5:7, 4:6 + 20L)
  )
  expect_equal(
    int_mat_lst_cbind_nth_cols(
      list(
        matrix(1:12, nrow = 3),
        matrix(2:13, nrow = 3),
        matrix(1:6 + 20L, ncol = 2)
      ),
      3
    ),
    cbind(7:9, 8:10, NA_integer_)
  )
  expect_equal(
    int_mat_lst_cbind_nth_cols(
      list(matrix(1:12, nrow = 3)), c(1, 2, 5)
    ),
    cbind(1:3, 4:6, NA_integer_)
  )
})

test_that("`int_mat_lst_cbind_nth_rows()` works", {
  expect_equal(
    int_mat_lst_cbind_nth_rows(
      list(
        matrix(1:12, nrow = 3),
        matrix(2:13, nrow = 3),
        matrix(1:6 + 20L, ncol = 2)
      ),
      c(1, 2, 5)
    ),
    cbind(c(1L, 4L, 7L, 10L), c(3L, 6L, 9L, 12L), NA_integer_)
  )
  expect_equal(
    int_mat_lst_cbind_nth_rows(
      list(
        matrix(1:12, nrow = 3),
        matrix(2:13, nrow = 3),
        matrix(1:8 + 20L, ncol = 4)
      ),
      2
    ),
    cbind(c(2L, 5L, 8L, 11L), c(3L, 6L, 9L, 12L), c(22L, 24L, 26L, 28L))
  )
  expect_equal(
    int_mat_lst_cbind_nth_rows(
      list(
        matrix(1:12, nrow = 3),
        matrix(2:13, nrow = 3),
        matrix(1:8 + 20L, ncol = 4)
      ),
      3
    ),
    cbind(c(3L, 6L, 9L, 12L), c(4L, 7L, 10L, 13L), NA_integer_)
  )
  expect_equal(
    int_mat_lst_cbind_nth_rows(
      list(matrix(1:12, nrow = 3)), c(1, 2, 5)
    ),
    cbind(c(1L, 4L, 7L, 10L), c(2L, 5L, 8L, 11L), NA_integer_)
  )
})

test_that("`int_mat_lst_nth_cols()` works", {
  expect_equal(
    int_mat_lst_nth_cols(
      list(matrix(1:4, nrow = 2), matrix(9:1, nrow = 3)),
      c(2, 3)
    ),
    list(3:4, 3:1)
  )
  expect_equal(
    int_mat_lst_nth_cols(
      list(matrix(9:1, nrow = 3)),
      c(2, 3)
    ),
    list(6:4, 3:1)
  )
  expect_equal(
    int_mat_lst_nth_cols(
      list(matrix(9:1, nrow = 3)),
      c(2, -9)
    ),
    list(6:4, rep(NA_integer_, 3))
  )
  expect_equal(
    int_mat_lst_nth_cols(
      list(matrix(9:1, nrow = 3), matrix(9:1, nrow = 3)),
      c(2, -9)
    ),
    list(6:4, rep(NA_integer_, 3))
  )
})

test_that("`int_mat_lst_nth_rows()` works", {
  expect_equal(
    int_mat_lst_nth_rows(
      list(matrix(1:4, nrow = 2), matrix(9:1, nrow = 3)),
      c(2, 3)
    ),
    list(c(2L, 4L), c(7L, 4L, 1L))
  )
  expect_equal(
    int_mat_lst_nth_rows(
      list(matrix(1:4, nrow = 2), matrix(9:1, nrow = 3)),
      c(2, 9)
    ),
    list(c(2L, 4L), rep(NA_integer_, 3))
  )
  expect_equal(
    int_mat_lst_nth_rows(
      list(matrix(1:4, nrow = 2)),
      c(2, 9)
    ),
    list(c(2L, 4L), rep(NA_integer_, 2))
  )
  expect_equal(
    int_mat_lst_nth_rows(
      list(matrix(1:4, nrow = 2), matrix(9:1, nrow = 3)), 2
    ),
    list(c(2L, 4L), c(8L, 5L, 2L))
  )
})

test_that("`int_mat_lst_rbind_nth_cols()` works", {
  expect_equal(
    int_mat_lst_rbind_nth_cols(
      list(
        matrix(1:12, nrow = 3),
        matrix(2:13, nrow = 3),
        matrix(1:6 + 20L, ncol = 2)
      ),
      c(1, 2, 5)
    ),
    rbind(1:3, 5:7, NA_integer_)
  )
  expect_equal(
    int_mat_lst_rbind_nth_cols(
      list(
        matrix(1:12, nrow = 3),
        matrix(2:13, nrow = 3),
        matrix(1:6 + 20L, nrow = 3)
      ),
      2
    ),
    rbind(4:6, 5:7, 4:6 + 20L)
  )
  expect_equal(
    int_mat_lst_rbind_nth_cols(
      list(
        matrix(1:12, nrow = 3),
        matrix(2:13, nrow = 3),
        matrix(1:6 + 20L, ncol = 2)
      ),
      3
    ),
    rbind(7:9, 8:10, NA_integer_)
  )
  expect_equal(
    int_mat_lst_rbind_nth_cols(
      list(matrix(1:12, nrow = 3)), c(1, 2, 5)
    ),
    rbind(1:3, 4:6, NA_integer_)
  )
})

test_that("`int_mat_lst_rbind_nth_rows()` works", {
  expect_equal(
    int_mat_lst_rbind_nth_rows(
      list(
        matrix(1:12, nrow = 3),
        matrix(2:13, nrow = 3),
        matrix(1:6 + 20L, ncol = 2)
      ),
      c(1, 2, 5)
    ),
    rbind(c(1L, 4L, 7L, 10L), c(3L, 6L, 9L, 12L), NA_integer_)
  )
  expect_equal(
    int_mat_lst_rbind_nth_rows(
      list(
        matrix(1:12, nrow = 3),
        matrix(2:13, nrow = 3),
        matrix(1:8 + 20L, ncol = 4)
      ),
      2
    ),
    rbind(c(2L, 5L, 8L, 11L), c(3L, 6L, 9L, 12L), c(22L, 24L, 26L, 28L))
  )
  expect_equal(
    int_mat_lst_rbind_nth_rows(
      list(
        matrix(1:12, nrow = 3),
        matrix(2:13, nrow = 3),
        matrix(1:8 + 20L, ncol = 4)
      ),
      3
    ),
    rbind(c(3L, 6L, 9L, 12L), c(4L, 7L, 10L, 13L), NA_integer_)
  )
  expect_equal(
    int_mat_lst_rbind_nth_rows(
      list(matrix(1:12, nrow = 3)), c(1, 2, 5)
    ),
    rbind(c(1L, 4L, 7L, 10L), c(2L, 5L, 8L, 11L), NA_integer_)
  )
})

test_that("`interleave_chr_lsts()` works", {
  expect_equal(
    interleave_chr_lsts(
      list(c("a", "b"), c("a", "b", "c"), c("a", "b")),
      list(c("x", "y"), c("x", "y"), c("x", "y", "z"))
    ),
    list(
      c("a", "x", "b", "y"),
      c("a", "x", "b", "y", "c"),
      c("x", "a", "y", "b", "z")
    )
  )
})

test_that("`interleave_chr_vecs()` works", {
  expect_equal(
    interleave_chr_vecs(c("a", "b"), c("x", "y")),
    c("a", "x", "b", "y")
  )
  expect_equal(
    interleave_chr_vecs(c("a", "b", "c"), c("x", "y")),
    c("a", "x", "b", "y", "c")
  )
  expect_equal(
    interleave_chr_vecs(c("a", "b"), c("x", "y", "z")),
    c("x", "a", "y", "b", "z")
  )
  expect_error(
    interleave_chr_vecs(as.character(1:7), as.character(1:3)),
    str_c(
      "lengths.+differ by at most 1.+",
      "x.+length 7.+y.+length 3"
    )
  )
})

test_that("`lst_chr_to_dbl()` works", {
  expect_equal(
    lst_chr_to_dbl(list(c("1", "2,000"), c("1.3", "2.2", "5.9")),
      big_mark = ","
    ),
    lapply(list(c("1", "2000"), c("1.3", "2.2", "5.9")), as.double)
  )
  expect_equal(
    lst_chr_to_dbl(list(c("1", "2,000"), c("1.3", "2.2", "5.9")),
      big_mark = c(",", "")
    ),
    lapply(list(c("1", "2000"), c("1.3", "2.2", "5.9")), as.double)
  )
})

test_that("`lst_fullocate()` works", {
  int_mat_lst <- list(
    rbind(c(2L, 5L), 7:8),
    rbind(c(5L, 6L), c(7L, 10L), c(20L, 30L))
  )
  expect_equal(
    lst_fullocate(int_mat_lst, start = c(1, 5), end = c(10, 50)),
    list(
      rbind(1L, c(2L, 5L), 6L, 7:8, 9:10),
      rbind(5:6, c(7L, 10L), c(11L, 19L), c(20L, 30L), c(31L, 50L))
    )
  )
  expect_equal(
    lst_fullocate(int_mat_lst, start = c(1, 5), end = 50),
    list(
      rbind(1L, c(2L, 5L), 6L, 7:8, c(9L, 50L)),
      rbind(5:6, c(7L, 10L), c(11L, 19L), c(20L, 30L), c(31L, 50L))
    )
  )
})

test_that("`match_arg_index()` works", {
  expect_equal(match_arg_index("ab", c("book", "abacus", "pencil")), 2)
})

test_that("int_prlst_rbind() and int_prlst_cbind() work", {
  expect_equal(
    int_prlst_rbind(pairlist(1:2, 8:9, 5:4)),
    rbind(1:2, 8:9, 5:4)
  )
  expect_equal(
    int_prlst_cbind(pairlist(1:2, 8:9, 5:4)),
    cbind(1:2, 8:9, 5:4)
  )
})


# --- FILE: https://raw.githubusercontent.com/rorynolan/strex/master/tests/testthat/test-camel-case.R ---
test_that("str_split_camel_case()` works", {
  expect_equal(
    str_split_camel_case(c(
      "RoryNolan", "NaomiFlagg",
      "DepartmentOfSillyHats"
    )),
    list(
      c("Rory", "Nolan"), c("Naomi", "Flagg"),
      c("Department", "Of", "Silly", "Hats")
    )
  )
  expect_equal(
    str_split_camel_case(
      c(
        "RoryNolan", "NaomiFlagg",
        "DepartmentOfSillyHats"
      ),
      lower = TRUE
    ),
    list(
      c("Rory", "Nolan"), c("Naomi", "Flagg"),
      c("Department", "Of", "Silly", "Hats")
    ) %>%
      lapply(str_to_lower)
  )
  expect_equal(str_split_camel_case(character()), list())
})


# --- FILE: https://raw.githubusercontent.com/rorynolan/strex/master/tests/testthat/test-can-be-num.R ---
test_that("`str_can_be_numeric()` works", {
  expect_true(str_can_be_numeric("3"))
  expect_true(str_can_be_numeric("5 "))
  expect_equal(str_can_be_numeric(c("1a", "abc")), rep(FALSE, 2))
  expect_equal(str_can_be_numeric(character()), logical())
  expect_equal(str_can_be_numeric(numeric()), logical())
})


# --- FILE: https://raw.githubusercontent.com/rorynolan/strex/master/tests/testthat/test-currency.R ---
test_that("`str_extract_currencies()` works", {
  string <- "35.00 $1.14 abc5 $3.8 77"
  expect_equal(
    str_extract_currencies(string),
    data.frame(
      string_num = 1, string = string,
      curr_sym = c("", "$", "c", "$", " "),
      amount = c(35, 1.14, 5, 3.8, 77),
      stringsAsFactors = FALSE
    )
  )
  string <- c(
    "35.00 $1.14", "abc5 $3.8 77", "-$1.5e6",
    "over £1,000"
  )
  reps <- c(2, 3, 1, 1)
  expect_equal(
    str_extract_currencies(string),
    data.frame(
      string_num = rep(seq_along(string), reps),
      string = rep(string, reps),
      curr_sym = c("", "$", "c", "$", " ", "$", "£"),
      amount = c(35, 1.14, 5, 3.8, 77, -1.5e6, 1000),
      stringsAsFactors = FALSE
    )
  )
  expect_equal(
    as.data.frame(str_extract_currencies(character())),
    data.frame(
      string_num = integer(), string = character(),
      curr_sym = character(), amount = numeric(),
      stringsAsFactors = FALSE
    )
  )
})
test_that("`str_nth_currency()` works", {
  string <- c("ab3 13", "$1")
  expect_equal(
    str_nth_currency(string, n = 2),
    data.frame(
      string_num = seq_along(string), string = string,
      curr_sym = c(" ", NA), amount = c(13, NA),
      stringsAsFactors = FALSE
    )
  )
  string <- c("35.00 $1.14", "abc5 $3.8", "stuff")
  expect_equal(str_nth_currency(string, c(
    1,
    2, 1
  )), data.frame(
    string_num = seq_along(string), string = string,
    curr_sym = c("", "$", NA), amount = c(35, 3.8, NA),
    stringsAsFactors = FALSE
  ))
  string <- c("ab3 13", "$1", "35.00 $1.14", "abc5 $3.8", "stuff")
  expect_equal(
    str_nth_currency(string, n = 2),
    data.frame(
      string_num = 1:5,
      string = c("ab3 13", "$1", "35.00 $1.14", "abc5 $3.8", "stuff"),
      curr_sym = c(" ", NA, "$", "$", NA),
      amount = c(13, NA, 1.14, 3.8, NA),
      stringsAsFactors = FALSE
    )
  )
  expect_equal(
    str_nth_currency(string, c(1, 2, 1, 2, 1)),
    data.frame(
      string_num = 1:5,
      string = c("ab3 13", "$1", "35.00 $1.14", "abc5 $3.8", "stuff"),
      curr_sym = c("b", NA, "", "$", NA),
      amount = c(3, NA, 35, 3.8, NA),
      stringsAsFactors = FALSE
    )
  )
  expect_equal(
    str_first_currency(string),
    data.frame(
      string_num = 1:5,
      string = c(
        "ab3 13", "$1", "35.00 $1.14",
        "abc5 $3.8", "stuff"
      ),
      curr_sym = c("b", "$", "", "c", NA),
      amount = c(
        3,
        1, 35, 5, NA
      ), stringsAsFactors = FALSE
    )
  )
  expect_equal(
    str_last_currency(string),
    data.frame(
      string_num = 1:5, string = c(
        "ab3 13", "$1", "35.00 $1.14",
        "abc5 $3.8", "stuff"
      ), curr_sym = c(" ", "$", "$", "$", NA),
      amount = c(13, 1, 1.14, 3.8, NA),
      stringsAsFactors = FALSE
    )
  )
  expect_snapshot_error(str_nth_currency(as.character(1:3), 1:7))
  expect_equal(as.data.frame(str_nth_currency(string, n = -2)),
    data.frame(
      string_num = seq_along(string), string,
      curr_sym = c("b", NA, "", "c", NA),
      amount = c(3, NA, 35, 5, NA),
      stringsAsFactors = FALSE
    ),
    ignore_attr = TRUE
  )
  expect_equal(
    as.data.frame(str_nth_currency(string, c(1, -2, 1, 2, -1))),
    data.frame(
      string_num = seq_along(string), string,
      curr_sym = c("b", NA, "", "$", NA),
      amount = c(3, NA, 35, 3.8, NA),
      stringsAsFactors = FALSE
    )
  )
  expect_equal(
    as.data.frame(str_nth_currency(character(), 1)),
    data.frame(
      string_num = integer(), string = character(),
      curr_sym = character(), amount = numeric(),
      stringsAsFactors = FALSE
    )
  )
})


# --- FILE: https://raw.githubusercontent.com/rorynolan/strex/master/tests/testthat/test-detect.R ---
test_that("`str_detect_all()` works", {
  expect_equal(str_detect_all("quick brown fox", c("x", "y", "z")), FALSE)
  expect_equal(str_detect_all(c(".", "-"), "."), c(TRUE, TRUE))
  expect_equal(str_detect_all(c(".", "-"), coll(".")), c(TRUE, FALSE))
  expect_equal(
    str_detect_all(c(".", "-"), coll("."), negate = TRUE),
    c(FALSE, TRUE)
  )
  expect_equal(
    str_detect_all(c(".", "-"), fixed("."), negate = TRUE),
    c(FALSE, TRUE)
  )
  expect_equal(str_detect_all(c(".", "-"), c(".", ":")), c(FALSE, FALSE))
  expect_equal(str_detect_all(c(".", "-"), coll(c(".", ":"))), c(FALSE, FALSE))
  expect_equal(str_detect_any("quick brown fox", c("x", "y", "z")), TRUE)
  expect_equal(str_detect_any(c(".", "-"), "."), c(TRUE, TRUE))
  expect_equal(str_detect_any(c(".", "-"), coll(".")), c(TRUE, FALSE))
  expect_equal(str_detect_any(c(".", "-"), fixed(".")), c(TRUE, FALSE))
  expect_equal(
    str_detect_any(c(".", "-"), coll("."), negate = TRUE),
    c(FALSE, TRUE)
  )
  expect_equal(str_detect_any(c(".", "-"), c(".", ":")), c(TRUE, TRUE))
  expect_equal(str_detect_any(c(".", "-"), coll(c(".", ":"))), c(TRUE, FALSE))
  expect_error(
    str_detect_all("quick brown fox", boundary()),
    "cannot handle.+pattern.+of type.+boundary"
  )
  expect_error(
    str_detect_any("quick brown fox", boundary()),
    "cannot handle.+pattern.+of type.+boundary"
  )
  expect_equal(
    str_detect_any(c("xyzabc", "abcxyz"), c(".b", "^x")),
    c(TRUE, TRUE)
  )
  expect_equal(
    str_detect_all(c("xyzabc", "abcxyz"), c(".b", "^x")),
    c(TRUE, FALSE)
  )
  expect_equal(str_detect_all("xyzabc", c("a", "c", "z")), TRUE)
})


# --- FILE: https://raw.githubusercontent.com/rorynolan/strex/master/tests/testthat/test-elem.R ---
test_that("str_elem() works", {
  expect_equal(str_elem(c("abcd", "xyz"), 3), c("c", "z"))
  expect_equal(str_elem("abcd", -2), "c")
  expect_equal(str_elem("abcd", 3), "c")
  expect_snapshot_error(str_elem(c("a", "b"), 1:3))
})

test_that("str_elems() works", {
  string <- c("abc", "def", "ghi", "vwxyz")
  ans <- matrix(
    c(
      "a", "b",
      "d", "e",
      "g", "h",
      "v", "w"
    ),
    ncol = 2, byrow = TRUE
  )
  expect_equal(str_elems(string, 1:2), ans)
  expect_equal(str_elems(string, 1:2, byrow = FALSE), t(ans))
  expect_equal(
    str_elems(string, c(1, 2, 3, 4, -1)),
    matrix(
      c(
        "a", "b", "c", "", "c",
        "d", "e", "f", "", "f",
        "g", "h", "i", "", "i",
        "v", "w", "x", "y", "z"
      ),
      nrow = length(string), byrow = TRUE
    )
  )
  expect_equal(str_elems(character(), 1:3), matrix(character(), ncol = 3))
  expect_equal(
    str_elems(character(), 1:3, byrow = FALSE),
    t(matrix(character(), ncol = 3))
  )
})

test_that("str_paste_elems() works", {
  string <- c("abc", "def", "ghi", "vwxyz")
  expect_equal(str_paste_elems(string, 1:2), c("ab", "de", "gh", "vw"))
  expect_equal(
    str_paste_elems(string, c(1, 2, 3, 4, -1)),
    c("abcc", "deff", "ghii", "vwxyz")
  )
  expect_equal(str_paste_elems("abc", c(1, 5, 55, 43, 3)), "ac")
  expect_equal(str_paste_elems(character(), c(1, 5, 55, 43, 3)), character())
})


# --- FILE: https://raw.githubusercontent.com/rorynolan/strex/master/tests/testthat/test-extract-non-numerics.R ---
test_that("str_extract_non_numerics() works", {
  expect_equal(
    str_extract_non_numerics("--123abc456", negs = TRUE),
    list(c("-", "abc"))
  )
  expect_equal(str_first_non_numeric("--123abc456"), "--")
  expect_equal(str_last_non_numeric("--123abc456"), "abc")
  expect_equal(str_nth_non_numeric("--123abc456", -2), "--")
  expect_snapshot_error(str_extract_non_numerics("a.23", leading_decimals = T))
  expect_equal(str_first_non_numeric("1"), NA_character_)
  expect_equal(str_last_non_numeric(c("abc", "def")), c("abc", "def"))
  expect_equal(
    str_nth_non_numeric(c("ab12bd23", "wx56yz89"), c(3, -1)),
    c(NA, "yz")
  )
  strings <- c(
    "abc123def456", "abc-0.12def.345", "abc.12e4def34.5e9",
    "abc1,100def1,230.5", "abc1,100e3,215def4e1,000"
  )
  expect_equal(
    str_extract_non_numerics(strings),
    list(c("abc", "def"), c("abc-", ".", "def."), c(
      "abc.", "e",
      "def", ".", "e"
    ), c("abc", ",", "def", ",", "."), c(
      "abc", ",",
      "e", ",", "def", "e", ","
    ))
  )
  expect_equal(
    str_extract_non_numerics(strings,
      decimals = TRUE, leading_decimals = FALSE
    ),
    list(c("abc", "def"), c("abc-", "def."), c(
      "abc.", "e", "def",
      "e"
    ), c("abc", ",", "def", ","), c(
      "abc", ",", "e", ",", "def",
      "e", ","
    ))
  )
  expect_equal(
    str_extract_non_numerics(strings, decimals = TRUE),
    list(c("abc", "def"), c("abc-", "def"), c(
      "abc", "e", "def",
      "e"
    ), c("abc", ",", "def", ","), c(
      "abc", ",", "e", ",", "def",
      "e", ","
    ))
  )
  expect_equal(
    str_extract_non_numerics(strings, big_mark = ","),
    list(c("abc", "def"), c("abc-", ".", "def."), c(
      "abc.", "e",
      "def", ".", "e"
    ), c("abc", "def", "."), c(
      "abc", "e", "def",
      "e"
    ))
  )
  expect_equal(str_extract_non_numerics(strings,
    decimals = TRUE, leading_decimals = TRUE,
    sci = TRUE
  ), list(c("abc", "def"), c("abc-", "def"), c("abc", "def"), c(
    "abc",
    ",", "def", ","
  ), c("abc", ",", ",", "def", ",")))
  expect_equal(str_extract_non_numerics(strings,
    decimals = TRUE, leading_decimals = TRUE,
    sci = TRUE, big_mark = ",", negs = TRUE
  ), list(c("abc", "def"), c("abc", "def"), c("abc", "def"), c(
    "abc",
    "def"
  ), c("abc", "def")))
  expect_equal(
    str_nth_non_numeric(strings, n = 2),
    c("def", ".", "e", ",", ",")
  )
  expect_equal(
    str_nth_non_numeric(strings, n = -2, decimals = TRUE),
    c("abc", "abc-", "def", "def", "e")
  )
  expect_equal(str_first_non_numeric(strings,
    decimals = TRUE,
    leading_decimals = FALSE
  ), c("abc", "abc-", "abc.", "abc", "abc"))
  expect_equal(
    str_last_non_numeric(strings, big_mark = ","),
    c("def", "def.", "e", ".", "e")
  )
  expect_equal(str_nth_non_numeric(strings,
    n = 1, decimals = TRUE, leading_decimals = TRUE,
    sci = TRUE
  ), c("abc", "abc-", "abc", "abc", "abc"))
  expect_equal(str_first_non_numeric(strings,
    decimals = TRUE, leading_decimals = TRUE,
    sci = TRUE, big_mark = ",", negs = TRUE
  ), c("abc", "abc", "abc", "abc", "abc"))
  expect_equal(
    suppressWarnings(str_extract_non_numerics("abc25.25.25def",
      decimals = TRUE
    )),
    list(NA_character_)
  )
  expect_equal(
    suppressWarnings(str_last_non_numeric("abc25.25.25def",
      decimals = TRUE
    )),
    NA_character_
  )
  expect_equal(str_extract_non_numerics(character()), list())
  expect_equal(str_last_non_numeric(character()), character())
})


# --- FILE: https://raw.githubusercontent.com/rorynolan/strex/master/tests/testthat/test-extract-nums.R ---
test_that("str_extract_numbers works", {
  expect_equal(
    str_extract_numbers(c("abc123abc456", "abc1.23abc456")),
    list(c(123, 456), c(1, 23, 456))
  )
  expect_equal(
    str_extract_numbers(c("abc1.23abc456", "abc1..23abc456"),
      decimals = TRUE, leading_decimals = FALSE
    ),
    list(c(1.23, 456), c(1, 23, 456))
  )
  expect_equal(
    str_extract_numbers("abc1..23abc456",
      decimals = TRUE, leading_decimals = FALSE
    ),
    list(c(1, 23, 456))
  )
  expect_equal(str_extract_numbers("abc1..23abc456",
    decimals = TRUE,
    leading_decimals = TRUE
  ), list(c(1, .23, 456)))
  expect_equal(str_extract_numbers("abc1..23abc456",
    decimals = TRUE,
    leading_decimals = TRUE,
    leave_as_string = TRUE
  ), list(c("1", ".23", "456")))
  expect_equal(str_extract_numbers("-123abc456"), list(c(123, 456)))
  expect_equal(
    str_extract_numbers("-123abc456", negs = TRUE),
    list(c(-123, 456))
  )
  expect_equal(
    str_extract_numbers("--123abc456", negs = TRUE),
    list(c(-123, 456))
  )
  expect_equal(str_extract_non_numerics("abc123abc456"), list(rep("abc", 2)))
  expect_equal(
    str_extract_non_numerics("abc1.23abc456"),
    list(c("abc", ".", "abc"))
  )
  expect_equal(
    str_extract_non_numerics("abc1.23abc456", decimals = TRUE),
    list(c("abc", "abc"))
  )
  expect_equal(
    str_extract_non_numerics("abc1..23abc456",
      decimals = TRUE,
      leading_decimals = FALSE
    ),
    list(c("abc", "..", "abc"))
  )
  expect_equal(str_extract_non_numerics("abc1..23abc456",
    decimals = TRUE,
    leading_decimals = TRUE
  ), list(c("abc", ".", "abc")))
  expect_equal(
    str_extract_non_numerics(c("-123abc456", "ab1c")),
    list(c("-", "abc"), c("ab", "c"))
  )
  expect_equal(str_extract_non_numerics("-123abc456", negs = TRUE), list("abc"))
  expect_equal(
    str_extract_non_numerics("--123abc456", negs = TRUE),
    list(c("-", "abc"))
  )
  expect_snapshot_warning(
    str_extract_numbers("abc1.2.3", decimals = TRUE)
  )
  expect_equal(suppressWarnings(
    str_extract_numbers("abc1.2.3", decimals = TRUE)
  ), list(NA_real_))
  expect_snapshot_warning(
    str_extract_numbers("ab.1.2",
      decimals = TRUE,
      leading_decimals = TRUE
    )
  )
  expect_snapshot_warning(
    str_extract_numbers("ab.1.2",
      decimals = TRUE,
      leading_decimals = TRUE
    )
  )
  expect_equal(suppressWarnings(str_extract_numbers("ab.1.2",
    decimals = TRUE,
    leading_decimals = TRUE
  )), list(NA_real_))
  expect_snapshot_warning(
    str_extract_numbers(c(rep("abc1.2.3", 2), "a1b2.2.3", "e5r6"),
      decimals = TRUE
    )
  )
  expect_equal(
    suppressWarnings(
      str_extract_numbers(c(rep("abc1.2.3", 2), "a1b2.2.3", "e5r6"),
        decimals = TRUE
      )
    ),
    c(as.list(rep(NA_real_, 3)), list(c(5, 6)))
  )
  expect_equal(str_nth_number("abc1.23abc456", 2), 23)
  expect_equal(str_first_number("abc1a2"), 1)
  expect_equal(str_last_number("akd50lkdjf0qukwjfj8"), 8)
  expect_equal(str_nth_number("abc1.23abc456", 2, leave_as_string = TRUE), "23")
  expect_equal(str_nth_number("abc1.23abc456", 2, decimals = TRUE), 456)
  expect_equal(str_nth_number("-123abc456", -2, negs = TRUE), -123)
  expect_equal(str_first_number("abc1e5"), 1)
  expect_equal(str_first_number("abc1e5", sci = TRUE), 1e5)
  expect_equal(str_first_number("abc1.4e5", sci = TRUE), 1)
  expect_equal(str_first_number("abc1.4e5", sci = TRUE, decimals = TRUE), 1.4e5)
  expect_equal(
    str_first_number("abc-1.4e5", sci = TRUE, decimals = TRUE),
    1.4e5
  )
  expect_equal(
    str_first_number("abc-1.4e5",
      sci = TRUE, decimals = TRUE,
      negs = TRUE
    ),
    -1.4e5
  )
  expect_snapshot_warning(
    expect_equal(
      str_first_number("ab.1.2",
        decimals = TRUE, leading_decimals = TRUE
      ),
      NA_real_
    )
  )
  expect_equal(
    suppressWarnings(str_first_number("ab.1.2",
      decimals = TRUE, leading_decimals = TRUE
    )),
    NA_real_
  )
  expect_equal(suppressWarnings(str_last_number("ab.1.2",
    decimals = TRUE,
    leading_decimals = TRUE
  )), NA_real_)
  expect_snapshot_error(str_extract_numbers("a.23", leading_decimals = T))
  expect_equal(str_first_number("abc"), NA_integer_)
  expect_equal(
    strex:::dbl_lst_nth_elems(list(c(1, 2)), c(-1, 3)),
    c(2, NA)
  )
  expect_equal(strex:::dbl_lst_nth_elems(list(c(1, 2), c(3, 4)), -1), c(2, 4))
  expect_equal(strex:::dbl_lst_nth_elems(list(c(1, 2), c(3, 4)), c(-1, 1)), 2:3)
  strings <- c(
    "abc123def456", "abc-0.12def.345", "abc.12e4def34.5e9",
    "abc1,100def1,230.5", "abc1,100e3,215def4e1,000"
  )
  expect_equal(
    str_nth_number(strings, n = 2),
    c(456, 12, 4, 100, 100)
  )
  expect_equal(
    str_nth_number(strings, n = -2, decimals = TRUE),
    c(123, 0.12, 34.5, 1, 1)
  )
  expect_equal(
    str_first_number(strings,
      decimals = TRUE, leading_decimals = TRUE
    ),
    c(123, 0.12, 0.12, 1, 1)
  )
  expect_equal(
    str_last_number(strings, big_mark = ","),
    c(456, 345, 9, 5, 1000)
  )
  expect_equal(str_nth_number(strings,
    n = 1, decimals = TRUE, leading_decimals = TRUE,
    sci = TRUE
  ), c(123, 0.12, 1200, 1, 1))
  expect_equal(str_first_number(strings,
    decimals = TRUE, leading_decimals = TRUE,
    sci = TRUE, big_mark = ",", negs = TRUE
  ), c(123, -0.12, 1200, 1100, Inf))
  expect_equal(str_last_number(strings,
    decimals = TRUE, leading_decimals = FALSE,
    sci = FALSE, big_mark = ",", leave_as_string = TRUE
  ), c("456", "345", "9", "1,230.5", "1,000"))
  expect_equal(
    str_extract_numbers(strings),
    list(c(123, 456), c(0, 12, 345), c(12, 4, 34, 5, 9), c(
      1, 100,
      1, 230, 5
    ), c(1, 100, 3, 215, 4, 1, 0))
  )
  expect_equal(
    str_extract_numbers(strings,
      decimals = TRUE, leading_decimals = FALSE
    ),
    list(c(123, 456), c(0.12, 345), c(12, 4, 34.5, 9), c(
      1, 100,
      1, 230.5
    ), c(1, 100, 3, 215, 4, 1, 0))
  )
  expect_equal(
    str_extract_numbers(strings,
      decimals = TRUE, leading_decimals = TRUE
    ),
    list(c(123, 456), c(0.12, 0.345), c(0.12, 4, 34.5, 9), c(
      1, 100,
      1, 230.5
    ), c(1, 100, 3, 215, 4, 1, 0))
  )
  expect_equal(
    str_extract_numbers(strings, big_mark = ","),
    list(c(123, 456), c(0, 12, 345), c(12, 4, 34, 5, 9), c(
      1100,
      1230, 5
    ), c(1100, 3215, 4, 1000))
  )
  expect_equal(str_extract_numbers(strings,
    decimals = TRUE, sci = TRUE
  ), list(c(123, 456), c(0.12, 0.345), c(1200, 3.45e+10), c(
    1, 100,
    1, 230.5
  ), c(1, 1e+05, 215, 40, 0)))
  expect_equal(str_extract_numbers(strings,
    decimals = TRUE, sci = TRUE, big_mark = ",", negs = TRUE
  ), list(c(123, 456), c(-0.12, 0.345), c(1200, 3.45e+10), c(
    1100,
    1230.5
  ), c(Inf, Inf)))
  expect_equal(str_extract_numbers(strings,
    decimals = TRUE, leading_decimals = FALSE,
    sci = FALSE, big_mark = ",", leave_as_string = TRUE
  ), list(c("123", "456"), c("0.12", "345"), c(
    "12", "4", "34.5",
    "9"
  ), c("1,100", "1,230.5"), c("1,100", "3,215", "4", "1,000")))
  expect_equal(str_extract_numbers(character()), list())
  expect_equal(str_first_number(character()), numeric())
  expect_equal(
    str_last_number(character(), leave_as_string = TRUE),
    character()
  )
})


# --- FILE: https://raw.githubusercontent.com/rorynolan/strex/master/tests/testthat/test-give-ext.R ---
test_that("`str_give_ext()` works", {
  expect_equal(str_give_ext("abc.csv", "csv"), "abc.csv")
  expect_equal(str_give_ext("abc", "csv"), "abc.csv")
  expect_equal(str_give_ext("abc.csv", "pdf"), "abc.csv.pdf")
  expect_equal(str_give_ext("abc.csv", "pdf", replace = TRUE), "abc.pdf")
  expect_equal(str_give_ext(character(), "pdf"), character())
})


# --- FILE: https://raw.githubusercontent.com/rorynolan/strex/master/tests/testthat/test-locate.R ---
test_that("`str_locate_nth()` works", {
  expect_equal(
    str_locate_first(c("abcdabcxyz", "abcabc"), "abc"),
    matrix(c(1, 3), nrow = 2, ncol = 2, byrow = TRUE) %>%
      magrittr::set_colnames(c("start", "end"))
  )
  expect_equal(
    str_locate_last(c("abcdabcxyz", "abcabc"), "abc"),
    matrix(c(5, 7, 4, 6), nrow = 2, ncol = 2, byrow = TRUE) %>%
      magrittr::set_colnames(c("start", "end"))
  )
  expect_equal(
    str_locate_nth("abc1def2abc", "abc", 3),
    matrix(NA_integer_, ncol = 2, nrow = 1) %>%
      magrittr::set_colnames(c("start", "end"))
  )
  expect_equal(
    str_locate_nth(
      c(
        "This old thing.",
        "That beautiful thing there."
      ),
      "\\w+", c(2, -2)
    ),
    matrix(c(
      6, 8,
      16, 20
    ), ncol = 2, byrow = 2) %>%
      magrittr::set_colnames(c("start", "end"))
  )
  expect_snapshot_error(str_locate_first(c("a", "b"), c("c", "d", "e")))
  expect_snapshot_error(str_locate_nth(c("a", "b"), c("a", "b"), 1:5))
  expect_equal(
    str_locate_nth("abc", "b", c(0, 1, 1, 2)),
    matrix(c(rep(NA, 2), rep(2, 4), rep(NA, 2)),
      ncol = 2, byrow = TRUE
    ) %>%
      magrittr::set_colnames(c("start", "end"))
  )
  expect_equal(
    str_locate_nth(character(0), "b", 4),
    matrix(character(0), ncol = 2) %>%
      magrittr::set_colnames(c("start", "end"))
  )
})

test_that("str_locate_braces() works", {
  string <- c("a{](kkj)})", "ab(]c{}")
  out <- str_locate_braces(string)
  expect_equal(
    as.data.frame(out),
    data.frame(
      string_num = as.integer(rep(1:2, c(6, 4))),
      string = rep(string, c(6, 4)),
      position = as.integer(c(
        2, 3, 4, 8, 9, 10,
        3, 4, 6, 7
      )),
      brace = c(
        "{", "]", "(", ")", "}", ")", "(",
        "]", "{", "}"
      ),
      stringsAsFactors = FALSE
    )
  )
  expect_equal(
    as.data.frame(str_locate_braces(character())),
    as.data.frame(out[0, ])
  )
})


# --- FILE: https://raw.githubusercontent.com/rorynolan/strex/master/tests/testthat/test-num-after.R ---
test_that("`nth_number_after_mth()` works", {
  string <- c(
    "abc1abc2abc3abc4abc5abc6abc7abc8abc9",
    "abc1def2ghi3abc4def5ghi6abc7def8ghi9"
  )
  expect_equal(str_nth_number_after_mth(string, "abc", 1, 3), c(3, 7))
  expect_equal(str_nth_number_after_mth(string, "abc", 2, 3), c(4, 8))
  expect_equal(str_nth_number_after_first(string, "abc", 2), c(2, 2))
  expect_equal(str_nth_number_after_last(string, "abc", -1), c(9, 9))
  expect_equal(str_first_number_after_mth(string, "abc", 2), c(2, 4))
  expect_equal(str_last_number_after_mth(string, "abc", 1), c(9, 9))
  expect_equal(str_first_number_after_first(string, "abc"), c(1, 1))
  expect_equal(str_first_number_after_last(string, "abc"), c(9, 7))
  expect_equal(str_last_number_after_first(string, "abc"), c(9, 9))
  expect_equal(str_last_number_after_last(string, "abc"), c(9, 9))
  expect_equal(str_last_number_after_last(character(), "abc"), numeric())
  expect_equal(
    str_last_number_after_last(character(), "abc",
      leave_as_string = TRUE
    ),
    character()
  )
  expect_snapshot_error(str_nth_number_after_mth("abc", "123", 1:2, 1:3))
})


# --- FILE: https://raw.githubusercontent.com/rorynolan/strex/master/tests/testthat/test-num-before.R ---
test_that("`nth_number_before_mth()` works", {
  string <- c(
    "abc1abc2abc3abc4def5abc6abc7abc8abc9",
    "abc1def2ghi3abc4def5ghi6abc7def8ghi9"
  )
  expect_equal(str_nth_number_before_mth(string, "def", 1, 1), c(1, 1))
  expect_equal(str_nth_number_before_mth(string, "abc", 2, 3), c(2, 2))
  expect_equal(str_nth_number_before_first(string, "def", 2), c(2, NA))
  expect_equal(str_nth_number_before_last(string, "def", -1), c(4, 7))
  expect_equal(str_first_number_before_mth(string, "abc", 2), c(1, 1))
  expect_equal(str_last_number_before_mth(string, "def", 1), c(4, 1))
  expect_equal(str_first_number_before_first(string, "def"), c(1, 1))
  expect_equal(str_first_number_before_last(string, "def"), c(1, 1))
  expect_equal(str_last_number_before_first(string, "def"), c(4, 1))
  expect_equal(str_last_number_before_last(string, "def"), c(4, 7))
  expect_equal(str_first_number_before_last(character(), "def"), numeric())
})


# --- FILE: https://raw.githubusercontent.com/rorynolan/strex/master/tests/testthat/test-remove.R ---
test_that("`str_remove_quoted()` works", {
  string <- "\"abc\"67a\'dk\'f"
  expect_equal(str_remove_quoted(string), "67af")
  expect_equal(str_remove_quoted(character()), character())
})


# --- FILE: https://raw.githubusercontent.com/rorynolan/strex/master/tests/testthat/test-singleize.R ---
test_that("`str_singleize()` works", {
  expect_equal(str_singleize("abc//def", "/"), "abc/def")
  expect_equal(str_singleize("abababcabab", "ab"), "abcab")
  expect_equal(str_singleize(c("abab", "cdcd"), "cd"), c("abab", "cd"))
  expect_equal(
    str_singleize(c("abab", "cdcd"), c("ab", "cd")),
    c("ab", "cd")
  )
  expect_equal(str_singleize(character(), "abc"), character())
})


# --- FILE: https://raw.githubusercontent.com/rorynolan/strex/master/tests/testthat/test-split-by-nums.R ---
test_that("str_split_by_numbers works", {
  expect_equal(
    str_split_by_numbers(c("abc123def456.789gh", "a1b2c344")),
    list(
      c("abc", "123", "def", "456", ".", "789", "gh"),
      c("a", 1, "b", 2, "c", 344)
    )
  )
  expect_equal(
    str_split_by_numbers("abc123def456.789gh", decimals = TRUE),
    list(c("abc", "123", "def", "456.789", "gh"))
  )
  expect_equal(str_split_by_numbers("22"), list("22"))
  expect_equal(
    suppressWarnings(str_split_by_numbers("abc25.25.25def", decimals = TRUE)),
    list(NA_character_)
  )
  expect_equal(str_split_by_numbers(character()), list())
})


# --- FILE: https://raw.githubusercontent.com/rorynolan/strex/master/tests/testthat/test-to-vec.R ---
test_that("str_to_vec works", {
  expect_equal(str_to_vec("abcdef"), c("a", "b", "c", "d", "e", "f"))
  expect_equal(str_to_vec(character()), character())
})


# --- FILE: https://raw.githubusercontent.com/rorynolan/strex/master/tests/testthat/test-trim.R ---
test_that("str_trim_anything works", {
  expect_equal(str_trim_anything("..abcd.", coll("."), "left"), "abcd.")
  expect_equal(
    str_trim_anything("..abcd.", coll("."), "left"),
    str_trim_anything("..abcd.", fixed("."), "left")
  )
  expect_equal(
    str_trim_anything("..abcd.", coll("."), "Ri"),
    str_trim_anything("..abcd.", coll("."), "r")
  )
  expect_equal(
    str_trim_anything(c("abcx", "abcy"), c("x", "y")),
    rep("abc", 2)
  )
  expect_equal(
    str_trim_anything(c("abcx", "abcy"), coll(c("x", "y"))),
    rep("abc", 2)
  )
  expect_equal(
    str_trim_anything(c("abcx", "abcy"), fixed(c("x", "y"))),
    rep("abc", 2)
  )
  expect_equal(str_trim_anything("..abcd.", ".", "left"), "")
  expect_equal(str_trim_anything("-ghi--", "-"), "ghi")
  expect_equal(str_trim_anything("-ghi--", "--"), "-ghi")
  expect_equal(str_trim_anything("-ghi--", "--", "right"), "-ghi")
  expect_equal(str_trim_anything(character(), "a"), character())
  expect_equal(str_trim_anything("-ghi--", "i-+"), "-gh")
  expect_equal(str_trim_anything("-ghi--", "-"), "ghi")
  expect_equal(str_trim_anything(c("-ghi--", "xx"), "-"), c("ghi", "xx"))
  expect_equal(str_trim_anything(c("-ghi--", "xx"), "(-)+"), c("ghi", "xx"))
  expect_equal(
    str_trim_anything(c("tttattt", "ttatt", "tat"), "t"),
    rep("a", 3)
  )
  expect_error(
    str_trim_anything("x", boundary("word")),
    "Function cannot handle a `pattern` of type 'boundary'.",
    fixed = TRUE
  )
  expect_error(
    str_trim_anything(c("a", "b"), c("a", "^a")),
    "don.+start.+reg.+ex.+with.+\\^.+Element 2.+\\^a"
  )
  expect_error(
    str_trim_anything(c("a", "b"), c("a", "a$")),
    "don.+end.+reg.+ex.+with.+\\$.+Element 2.+a\\$"
  )
})


# --- FILE: https://raw.githubusercontent.com/rorynolan/strex/master/tests/testthat/test-utils.R ---
test_that("`*_list_nth_elems()` error correctly", {
  expect_error(
    chr_lst_nth_elems(list("a", "b"), 1:3),
    str_c(
      "If both.+chr_lst.+n.+lengths greater than 1.+",
      "then.+their lengths must be equal.+",
      "chr_lst.+length 2.+n.+length 3."
    )
  )
  expect_error(
    dbl_lst_nth_elems(list(1, 2), 1:3),
    str_c(
      "If both.+dbl_lst.+n.+lengths greater than 1.+",
      "then.+lengths must be equal.+",
      "dbl_lst.+length 2.+n.+length 3."
    )
  )
})

test_that("assert_lst_elems_common_length() works", {
  lst <- list(1)
  expect_true(assert_lst_elems_common_length(lst))
  lst <- list(1, 1:2)
  expect_error(
    assert_lst_elems_common_length(lst),
    "Elements.+do not have a common length"
  )
})

test_that("verify_string_pattern() edge cases are OK", {
  expect_true(verify_string_pattern("a", boundary()))
})
