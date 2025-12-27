

# --- FILE: https://raw.githubusercontent.com/r-lib/lifecycle/main/R/aaa.R ---
on_load <- function(expr, env = parent.frame(), ns = topenv(env)) {
  expr <- substitute(expr)
  force(env)

  callback <- function() eval_bare(expr, env)
  ns$.__rlang_hook__. <- c(ns$.__rlang_hook__., list(callback))
}

run_on_load <- function(env = parent.frame()) {
  ns <- topenv(env)

  hook <- ns$.__rlang_hook__.
  env_unbind(ns, ".__rlang_hook__.")

  # FIXME: Transform to `while` loop to allow hooking into on-load
  # from an on-load hook?
  for (callback in hook) {
    callback()
  }

  ns$.__rlang_hook__. <- NULL
}


replace_from <- function(pkg, what, to = topenv(caller_env())) {
  if (what %in% getNamespaceExports(pkg)) {
    env <- ns_env(pkg)
  } else {
    env <- to
  }
  env_get(env, what, inherit = TRUE)
}


# --- FILE: https://raw.githubusercontent.com/r-lib/lifecycle/main/R/arg.R ---
#' Mark an argument as deprecated
#'
#' Signal deprecated argument by using self-documenting sentinel
#' `deprecated()` as default argument. Test whether the caller has
#' supplied the argument with `is_present()`.
#'
#' @section Magical defaults:
#'
#' We recommend importing `lifecycle::deprecated()` in your namespace
#' and use it without the namespace qualifier.
#'
#' In general, we advise against such magical defaults, i.e. defaults
#' that cannot be evaluated by the user. In the case of
#' `deprecated()`, the trade-off is worth it because the meaning of
#' this default is obvious and there is no reason for the user to call
#' `deprecated()` themselves.
#'
#' @examples
#' foobar_adder <- function(foo, bar, baz = deprecated()) {
#'   # Check if user has supplied `baz` instead of `bar`
#'   if (lifecycle::is_present(baz)) {
#'
#'     # Signal the deprecation to the user
#'     deprecate_warn("1.0.0", "foo::bar_adder(baz = )", "foo::bar_adder(bar = )")
#'
#'     # Deal with the deprecated argument for compatibility
#'     bar <- baz
#'   }
#'
#'   foo + bar
#' }
#'
#' foobar_adder(1, 2)
#' foobar_adder(1, baz = 2)
#' @export
deprecated <- function() {
  missing_arg()
}
#' @rdname deprecated
#' @param arg A `deprecated()` function argument.
#' @export
is_present <- function(arg) {
  !is_missing(maybe_missing(arg))
}


# --- FILE: https://raw.githubusercontent.com/r-lib/lifecycle/main/R/badge.R ---
#' Embed a lifecycle badge in documentation
#'
#' @description
#'
#' To include lifecycle badges in your documentation:
#'
#' 1. Call `usethis::use_lifecycle()` to copy the badge images into the
#'    `man/` folder of your package.
#'
#' 2. Call `lifecycle::badge()` inside R backticks to insert a
#'    lifecycle badge:
#'
#'     ```
#'     #' `r lifecycle::badge("experimental")`
#'     #' `r lifecycle::badge("deprecated")`
#'     #' `r lifecycle::badge("superseded")`
#'     ```
#'
#'    If the deprecated feature is a function, a good place for this
#'    badge is at the top of the topic description. If it is an argument,
#'    you can put the badge in the argument description.
#'
#' The badge is displayed as an image in the HTML version of the
#' documentation and as text otherwise.
#'
#' `lifecycle::badge()` is run by roxygen at build time so you don't need
#' to add lifecycle to `Imports:` just to use the badges. However, it's still
#' good practice to add to `Suggests:` so that it will be available to
#' package developers.
#'
#' @section Badges:
#' * `r lifecycle::badge("experimental")` `lifecycle::badge("experimental")`
#' * `r lifecycle::badge("stable")` `lifecycle::badge("stable")`
#' * `r lifecycle::badge("superseded")` `lifecycle::badge("superseded")`
#' * `r lifecycle::badge("deprecated")` `lifecycle::badge("deprecated")`
#'
#' The meaning of these stages is described in
#' `vignette("stages")`.
#'
#' @param stage A lifecycle stage as a string. Must be one of
#'   `"experimental"`, `"stable"`, `"superseded"`, or `"deprecated"`.
#' @return An `Rd` expression describing the lifecycle stage.
#'
#' @export
badge <- function(stage) {
  old <- c("maturing", "questioning", "soft-deprecated", "defunct", "retired")
  if (!stage %in% old) {
    stage <- arg_match0(
      stage,
      c("experimental", "stable", "superseded", "deprecated")
    )
  }

  url <- paste0("https://lifecycle.r-lib.org/articles/stages.html#", stage)
  html <- sprintf(
    "\\href{%s}{\\figure{%s}{options: alt='[%s]'}}",
    url,
    file.path(sprintf("lifecycle-%s.svg", stage)),
    upcase1(stage)
  )
  text <- sprintf("\\strong{[%s]}", upcase1(stage))

  sprintf("\\ifelse{html}{%s}{%s}", html, text)
}


# --- FILE: https://raw.githubusercontent.com/r-lib/lifecycle/main/R/deprecate.R ---
#' Deprecate functions and arguments
#'
#' @description
#' These functions provide three levels of verbosity for deprecated
#' functions. Learn how to use them in `vignette("communicate")`.
#'
#' * `deprecate_soft()` warns only if the deprecated function is called
#'   directly, i.e. a user is calling a function they wrote in the global
#'   environment or a developer is calling it in their package. It does not
#'   warn when called indirectly, i.e. the deprecation comes from code that
#'   you don't control.
#'
#' * `deprecate_warn()` warns unconditionally.
#'
#' * `deprecate_stop()` fails unconditionally.
#'
#' Warnings are only issued once per session to avoid overwhelming
#' the user. Control with [`options(lifecycle_verbosity)`][verbosity].
#'
#' @section Conditions:
#' * Deprecation warnings have class `lifecycle_warning_deprecated`.
#' * Deprecation errors have class `lifecycle_error_deprecated`.
#'
#' @param when A string giving the version when the behaviour was deprecated.
#' @param what A string describing what is deprecated:
#'
#'   * Deprecate a whole function with `"foo()"`.
#'   * Deprecate an argument with `"foo(arg)"`.
#'   * Partially deprecate an argument with
#'     `"foo(arg = 'must be a scalar integer')"`.
#'   * Deprecate anything else with a custom message by wrapping it in `I()`.
#'
#'   You can optionally supply the namespace: `"ns::foo()"`, but this is
#'   usually not needed as it will be inferred from the caller environment.
#'
#' @param with An optional string giving a recommended replacement for the
#'   deprecated behaviour. This takes the same form as `what`.
#' @param details In most cases the deprecation message can be
#'   automatically generated from `with`. When it can't, use `details`
#'   to provide a hand-written message.
#'
#'   `details` can either be a single string or a character vector,
#'   which will be converted to a [bulleted list][cli::cli_bullets].
#'   By default, info bullets are used. Provide a named vectors to
#'   override.
#' @param id The id of the deprecation. A warning is issued only once for each
#'   `id`. Defaults to the generated message, but you should provide a unique
#'   `id` when the message in `details` is built programmatically and depends on
#'   inputs, or when you'd like to deprecate multiple functions but warn only
#'   once for all of them. Repeated calls to `deprecate_soft()` and
#'   `deprecate_warn()` are also much faster if you supply an `id` because it
#'   avoids spending time generating the message only to immediately exit if the
#'   once per session warning has already been thrown before.
#' @param env,user_env Pair of environments that define where `deprecate_*()`
#'   was called (used to determine the package name) and where the function
#'   called the deprecating function was called (used to determine if
#'   `deprecate_soft()` should message).
#'
#'   These are only needed if you're calling `deprecate_*()` from an internal
#'   helper, in which case you should forward `env = caller_env()` and
#'   `user_env = caller_env(2)`.
#' @return `NULL`, invisibly.
#'
#' @seealso [lifecycle()]
#'
#' @examples
#' # A deprecated function `foo`:
#' deprecate_warn("1.0.0", "foo()")
#'
#' # A deprecated argument `arg`:
#' deprecate_warn("1.0.0", "foo(arg)")
#'
#' # A partially deprecated argument `arg`:
#' deprecate_warn("1.0.0", "foo(arg = 'must be a scalar integer')")
#'
#' # A deprecated function with a function replacement:
#' deprecate_warn("1.0.0", "foo()", "bar()")
#'
#' # A deprecated function with a function replacement from a
#' # different package:
#' deprecate_warn("1.0.0", "foo()", "otherpackage::bar()")
#'
#' # A deprecated function with custom message:
#' deprecate_warn(
#'   when = "1.0.0",
#'   what = "foo()",
#'   details = "Please use `otherpackage::bar(foo = TRUE)` instead"
#' )
#'
#' # A deprecated function with custom bulleted list:
#' deprecate_warn(
#'   when = "1.0.0",
#'   what = "foo()",
#'   details = c(
#'     x = "This is dangerous",
#'     i = "Did you mean `safe_foo()` instead?"
#'   )
#' )
#' @export
deprecate_soft <- function(
  when,
  what,
  with = NULL,
  details = NULL,
  id = NULL,
  env = caller_env(),
  user_env = caller_env(2)
) {
  msg <- NULL # trick R CMD check
  # Delay message generation until required, in particular if an `id`
  # is provided and we've already warned this session, then we won't ever
  # materialize this `msg`. Faster than using the more ergonomic `%<~%`.
  delayedAssign(
    "msg",
    lifecycle_message(
      when,
      what,
      with,
      details,
      env,
      signaller = "deprecate_soft"
    )
  )

  verbosity <- lifecycle_verbosity()
  direct <- is_direct(user_env)

  invisible(switch(
    verbosity,
    quiet = NULL,
    warning = ,
    default = if (direct) {
      always <- verbosity == "warning"
      trace_env <- caller_env()
      deprecate_warn0(
        msg,
        id,
        always = always,
        direct = TRUE,
        trace_env = trace_env,
        user_env = user_env
      )
    },
    error = deprecate_stop0(msg)
  ))
}

#' @rdname deprecate_soft
#' @param always If `FALSE`, the default, will warn once per session. If
#'   `TRUE`, will always warn in direct usages. Indirect usages keep
#'   warning once per session to avoid disrupting users who can't fix the
#'   issue. Only use `always = TRUE` after at least one release with
#'   the default.
#' @export
deprecate_warn <- function(
  when,
  what,
  with = NULL,
  details = NULL,
  id = NULL,
  always = FALSE,
  env = caller_env(),
  user_env = caller_env(2)
) {
  msg <- NULL # trick R CMD check
  # Delay message generation until required, in particular if an `id`
  # is provided and we've already warned this session, then we won't ever
  # materialize this `msg`. Faster than using the more ergonomic `%<~%`.
  delayedAssign(
    "msg",
    lifecycle_message(
      when,
      what,
      with,
      details,
      env,
      signaller = "deprecate_warn"
    )
  )

  verbosity <- lifecycle_verbosity()

  invisible(switch(
    verbosity,
    quiet = NULL,
    warning = ,
    default = {
      direct <- is_direct(user_env)
      always <- direct && (always || verbosity == "warning")
      trace_env <- caller_env()
      deprecate_warn0(
        msg,
        id,
        always = always,
        direct = direct,
        trace_env = trace_env,
        user_env = user_env
      )
    },
    error = deprecate_stop0(msg),
  ))
}

#' @rdname deprecate_soft
#' @export
deprecate_stop <- function(
  when,
  what,
  with = NULL,
  details = NULL,
  env = caller_env()
) {
  # No need to be lazy here, `deprecate_stop0()` will always force `msg`
  msg <- lifecycle_message(
    when,
    what,
    with,
    details,
    env,
    signaller = "deprecate_stop"
  )
  deprecate_stop0(msg)
}

# Signals -----------------------------------------------------------------

deprecate_warn0 <- function(
  msg,
  id = NULL,
  always = FALSE,
  direct = FALSE,
  call = caller_env(),
  trace_env = caller_env(),
  user_env = caller_env(2)
) {
  # declare(
  #   params(msg = lazy)
  # )

  # `msg` is passed lazily for performance reasons! Avoid evaluating it before
  # checking if we can early exit using the `id`.
  id <- id %||% paste_line(msg)
  if (!always && !needs_warning(id, call = call)) {
    return()
  }

  # Prevent warning from being displayed again this session
  env_poke(deprecation_env, id, TRUE)

  footer <- function(...) {
    footer <- NULL

    if (!direct) {
      top <- topenv(user_env)

      if (is_namespace(top)) {
        pkg <- ns_env_name(top)
        url <- pkg_url_bug(pkg)

        likely_line <- cli::format_inline(
          "The deprecated feature was likely used in the {.pkg {pkg}} package."
        )

        if (is_null(url)) {
          report_line <-
            "Please report the issue to the authors."
        } else {
          report_line <- cli::format_inline(
            "Please report the issue at {.url {url}}."
          )
        }

        footer <- c(
          footer,
          "i" = likely_line,
          " " = report_line
        )
      }
    }

    if (is_interactive()) {
      footer <- c(
        footer,
        if (!always) {
          cli::col_silver("This warning is displayed once per session.")
        },
        cli::format_inline(cli::col_silver(
          "Call {.run lifecycle::last_lifecycle_warnings()} to see where this warning was generated."
        ))
      )
    }

    footer
  }

  trace <- trace_back(bottom = trace_env)

  wrn <- new_deprecated_warning(msg, trace, footer = footer)

  # Record muffled warnings if testthat is running because it
  # muffles all warnings but we still want to examine them after a
  # run of `devtools::test()`
  maybe_push_warning <- function() {
    if (Sys.getenv("TESTTHAT_PKG") != "") {
      push_warning(wrn)
    }
  }

  withRestarts(muffleWarning = maybe_push_warning, {
    signalCondition(wrn)
    push_warning(wrn)
    warning(wrn)
  })
}

deprecate_stop0 <- function(msg) {
  cnd_signal(error_cnd(
    c("lifecycle_error_deprecated", "defunctError"),
    old = NULL,
    new = NULL,
    package = NULL,
    message = msg
  ))
}

# Messages ----------------------------------------------------------------

lifecycle_message <- function(
  when,
  what,
  with = NULL,
  details = NULL,
  env = caller_env(2),
  call = caller_env(),
  signaller = "signal_lifecycle"
) {
  check_string(when, call = call)

  if (is_null(details)) {
    details <- chr()
  } else {
    check_character(details, call = call)
  }

  what <- spec(what, env, signaller = signaller)
  msg <- lifecycle_message_what(what, when)

  if (!is_null(with)) {
    with <- spec(with, NULL, signaller = signaller)
    msg <- c(msg, "i" = lifecycle_message_with(with, what))
  }

  if (is_null(names(details))) {
    details <- set_names(details, "i")
  }

  c(msg, details)
}

lifecycle_message_what <- function(what, when) {
  if (!inherits(what$fn, "AsIs")) {
    what$fn <- fun_label(what$fn)
  }

  if (is_null(what$arg)) {
    if (what$from == "deprecate_stop") {
      sprintf(
        "%s was deprecated in %s %s and is now defunct.",
        what$fn,
        what$pkg,
        when
      )
    } else {
      sprintf(
        "%s was deprecated in %s %s.",
        what$fn,
        what$pkg,
        when
      )
    }
  } else {
    if (what$from == "deprecate_stop" && is_null(what$reason)) {
      sprintf(
        "The `%s` argument of %s was deprecated in %s %s and is now defunct.",
        what$arg,
        what$fn,
        what$pkg,
        when
      )
    } else {
      what$reason <- what$reason %||% "is deprecated"
      sprintf(
        "The `%s` argument of %s %s as of %s %s.",
        what$arg,
        what$fn,
        what$reason,
        what$pkg,
        when
      )
    }
  }
}

fun_label <- function(fn) {
  if (grepl("^`", fn)) {
    fn
  } else {
    paste0("`", fn, "()`")
  }
}

lifecycle_message_with <- function(with, what) {
  if (inherits(with$fn, "AsIs")) {
    sprintf("Please use %s instead.", with$fn)
  } else {
    if (!is_null(with$pkg) && what$pkg != with$pkg) {
      with$fn <- sprintf("%s::%s", with$pkg, with$fn)
    }

    if (is_null(with$arg)) {
      sprintf(
        "Please use `%s()` instead.",
        with$fn
      )
    } else if (what$fn == with$fn) {
      sprintf(
        "Please use the `%s` argument instead.",
        with$arg
      )
    } else {
      sprintf(
        "Please use the `%s` argument of `%s()` instead.",
        with$arg,
        with$fn
      )
    }
  }
}

# Helpers -----------------------------------------------------------------

is_direct <- function(env) {
  env_inherits_global(env) || from_testthat(env)
}

env_inherits_global <- function(env) {
  # `topenv(emptyenv())` returns the global env. Return `FALSE` in
  # that case to allow passing the empty env when the
  # soft-deprecation should not be promoted to deprecation based on
  # the caller environment.
  if (is_reference(env, empty_env())) {
    return(FALSE)
  }

  is_reference(topenv(env), global_env())
}

# TRUE if we are in unit tests and the package being tested is the
# same as the package that called
from_testthat <- function(env) {
  tested_package <- Sys.getenv("TESTTHAT_PKG")
  if (!nzchar(tested_package)) {
    return(FALSE)
  }

  top <- topenv(env)
  if (!is_namespace(top)) {
    return(FALSE)
  }

  # Test for environment names rather than reference/contents because
  # testthat clones the namespace
  identical(ns_env_name(top), tested_package)
}

needs_warning <- function(id, call = caller_env()) {
  check_string(id, call = call)
  is_null(deprecation_env[[id]])
}


# --- FILE: https://raw.githubusercontent.com/r-lib/lifecycle/main/R/expect.R ---
#' Does expression produce lifecycle warnings or errors?
#'
#' @description
#' These functions are equivalent to [testthat::expect_warning()] and
#' [testthat::expect_error()] but check specifically for lifecycle
#' warnings or errors.
#'
#' To test whether a deprecated feature still works without causing a
#' deprecation warning, set the `lifecycle_verbosity` option to
#' `"quiet"`.
#'
#' ```
#' test_that("feature still works", {
#'   withr::local_options(lifecycle_verbosity = "quiet")
#'   expect_true(my_deprecated_function())
#' })
#' ```
#'
#' @param expr Expression that should produce a lifecycle warning or
#'   error.
#' @param regexp Optional regular expression matched against the
#'   expected warning message.
#' @inheritParams testthat::expect_warning
#'
#' @details
#' `expect_deprecated()` sets the [lifecycle_verbosity][verbosity]
#' option to `"warning"` to enforce deprecation warnings which are
#' otherwise only shown once per session.
#'
#' @export
expect_deprecated <- function(expr, regexp = NULL, ...) {
  local_options(lifecycle_verbosity = "warning")

  if (!is_null(regexp) && is_na(regexp)) {
    abort("`regexp` can't be `NA`.")
  }

  testthat::expect_warning(
    {{ expr }},
    regexp = regexp,
    class = "lifecycle_warning_deprecated",
    ...
  )
}
#' @rdname expect_deprecated
#' @export
expect_defunct <- function(expr) {
  testthat::expect_error(
    {{ expr }},
    class = "lifecycle_error_deprecated"
  )
}


# --- FILE: https://raw.githubusercontent.com/r-lib/lifecycle/main/R/import-standalone-obj-type.R ---
# Standalone file: do not edit by hand
# Source: https://github.com/r-lib/rlang/blob/HEAD/R/standalone-obj-type.R
# Generated by: usethis::use_standalone("r-lib/rlang", "obj-type")
# ----------------------------------------------------------------------
#
# ---
# repo: r-lib/rlang
# file: standalone-obj-type.R
# last-updated: 2025-10-02
# license: https://unlicense.org
# imports: rlang (>= 1.1.0)
# ---
#
# ## Changelog
#
# 2025-10-02:
# - `obj_type_friendly()` now shows the dimensionality of arrays.
#
# 2024-02-14:
# - `obj_type_friendly()` now works for S7 objects.
#
# 2023-05-01:
# - `obj_type_friendly()` now only displays the first class of S3 objects.
#
# 2023-03-30:
# - `stop_input_type()` now handles `I()` input literally in `arg`.
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
#
# nocov start

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
      type <- class(x)[[1L]]
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
    if (n_dim == 0) {
      return(add_length("a list"))
    } else if (n_dim == 2) {
      if (is.data.frame(x)) {
        return("a data frame")
      } else {
        return("a list matrix")
      }
    } else {
      return(sprintf("a list %sD array", n_dim))
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

  if (n_dim == 0) {
    kind <- "vector"
  } else if (n_dim == 2) {
    kind <- "matrix"
  } else {
    kind <- sprintf("%sD array", n_dim)
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
#'   `"R6"`, or `"S7"`.
#' @noRd
obj_type_oo <- function(x) {
  if (!is.object(x)) {
    return("bare")
  }

  class <- inherits(x, c("R6", "S7_object"), which = TRUE)

  if (class[[1]]) {
    "R6"
  } else if (class[[2]]) {
    "S7"
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
  # From standalone-cli.R
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
  if (inherits(arg, "AsIs")) {
    format_arg <- identity
  } else {
    format_arg <- cli$format_arg
  }

  message <- sprintf(
    "%s must be %s, not %s.",
    format_arg(arg),
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


# --- FILE: https://raw.githubusercontent.com/r-lib/lifecycle/main/R/import-standalone-types-check.R ---
# Standalone file: do not edit by hand
# Source: https://github.com/r-lib/rlang/blob/HEAD/R/standalone-types-check.R
# Generated by: usethis::use_standalone("r-lib/rlang", "types-check")
# ----------------------------------------------------------------------
#
# ---
# repo: r-lib/rlang
# file: standalone-types-check.R
# last-updated: 2023-03-13
# license: https://unlicense.org
# dependencies: standalone-obj-type.R
# imports: rlang (>= 1.1.0)
# ---
#
# ## Changelog
#
# 2025-09-19:
# - `check_logical()` gains an `allow_na` argument (@jonthegeek, #1724)
#
# 2024-08-15:
# - `check_character()` gains an `allow_na` argument (@martaalcalde, #1724)
#
# 2023-03-13:
# - Improved error messages of number checkers (@teunbrand)
# - Added `allow_infinite` argument to `check_number_whole()` (@mgirlich).
# - Added `check_data_frame()` (@mgirlich).
#
# 2023-03-07:
# - Added dependency on rlang (>= 1.1.0).
#
# 2023-02-15:
# - Added `check_logical()`.
#
# - `check_bool()`, `check_number_whole()`, and
#   `check_number_decimal()` are now implemented in C.
#
# - For efficiency, `check_number_whole()` and
#   `check_number_decimal()` now take a `NULL` default for `min` and
#   `max`. This makes it possible to bypass unnecessary type-checking
#   and comparisons in the default case of no bounds checks.
#
# 2022-10-07:
# - `check_number_whole()` and `_decimal()` no longer treat
#   non-numeric types such as factors or dates as numbers.  Numeric
#   types are detected with `is.numeric()`.
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
#
# nocov start

# Scalars -----------------------------------------------------------------

.standalone_types_check_dot_call <- .Call

check_bool <- function(
  x,
  ...,
  allow_na = FALSE,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  if (
    !missing(x) &&
      .standalone_types_check_dot_call(
        ffi_standalone_is_bool_1.0.7,
        x,
        allow_na,
        allow_null
      )
  ) {
    return(invisible(NULL))
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

IS_NUMBER_true <- 0
IS_NUMBER_false <- 1
IS_NUMBER_oob <- 2

check_number_decimal <- function(
  x,
  ...,
  min = NULL,
  max = NULL,
  allow_infinite = TRUE,
  allow_na = FALSE,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  if (missing(x)) {
    exit_code <- IS_NUMBER_false
  } else if (
    0 ==
      (exit_code <- .standalone_types_check_dot_call(
        ffi_standalone_check_number_1.0.7,
        x,
        allow_decimal = TRUE,
        min,
        max,
        allow_infinite,
        allow_na,
        allow_null
      ))
  ) {
    return(invisible(NULL))
  }

  .stop_not_number(
    x,
    ...,
    exit_code = exit_code,
    allow_decimal = TRUE,
    min = min,
    max = max,
    allow_na = allow_na,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

check_number_whole <- function(
  x,
  ...,
  min = NULL,
  max = NULL,
  allow_infinite = FALSE,
  allow_na = FALSE,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  if (missing(x)) {
    exit_code <- IS_NUMBER_false
  } else if (
    0 ==
      (exit_code <- .standalone_types_check_dot_call(
        ffi_standalone_check_number_1.0.7,
        x,
        allow_decimal = FALSE,
        min,
        max,
        allow_infinite,
        allow_na,
        allow_null
      ))
  ) {
    return(invisible(NULL))
  }

  .stop_not_number(
    x,
    ...,
    exit_code = exit_code,
    allow_decimal = FALSE,
    min = min,
    max = max,
    allow_na = allow_na,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

.stop_not_number <- function(
  x,
  ...,
  exit_code,
  allow_decimal,
  min,
  max,
  allow_na,
  allow_null,
  arg,
  call
) {
  if (allow_decimal) {
    what <- "a number"
  } else {
    what <- "a whole number"
  }

  if (exit_code == IS_NUMBER_oob) {
    min <- min %||% -Inf
    max <- max %||% Inf

    if (min > -Inf && max < Inf) {
      what <- sprintf("%s between %s and %s", what, min, max)
    } else if (x < min) {
      what <- sprintf("%s larger than or equal to %s", what, min)
    } else if (x > max) {
      what <- sprintf("%s smaller than or equal to %s", what, max)
    } else {
      abort("Unexpected state in OOB check", .internal = TRUE)
    }
  }

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
    allow_na = FALSE,
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
    allow_na = FALSE,
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
    allow_na = FALSE,
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
    allow_na = FALSE,
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
    allow_na = FALSE,
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
    allow_na = FALSE,
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
    allow_na = FALSE,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}


# Vectors -----------------------------------------------------------------

# TODO: Figure out what to do with logical `NA` and `allow_na = TRUE`

check_character <- function(
  x,
  ...,
  allow_na = TRUE,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  if (!missing(x)) {
    if (is_character(x)) {
      if (!allow_na && any(is.na(x))) {
        abort(
          sprintf("`%s` can't contain NA values.", arg),
          arg = arg,
          call = call
        )
      }

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

check_logical <- function(
  x,
  ...,
  allow_na = TRUE,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  if (!missing(x)) {
    if (is_logical(x)) {
      if (!allow_na && any(is.na(x))) {
        abort(
          sprintf("`%s` can't contain NA values.", arg),
          arg = arg,
          call = call
        )
      }
      return(invisible(NULL))
    }
    if (allow_null && is_null(x)) {
      return(invisible(NULL))
    }
  }

  stop_input_type(
    x,
    "a logical vector",
    ...,
    allow_na = FALSE,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

check_data_frame <- function(
  x,
  ...,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  if (!missing(x)) {
    if (is.data.frame(x)) {
      return(invisible(NULL))
    }
    if (allow_null && is_null(x)) {
      return(invisible(NULL))
    }
  }

  stop_input_type(
    x,
    "a data frame",
    ...,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

# nocov end


# --- FILE: https://raw.githubusercontent.com/r-lib/lifecycle/main/R/lifecycle-package.R ---
#' @keywords internal
#' @import rlang
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL

.onLoad <- function(lib, pkg) {
  run_on_load()
}

on_load(
  local_use_cli()
)


# --- FILE: https://raw.githubusercontent.com/r-lib/lifecycle/main/R/lint.R ---
# Retrieve the lifecycle status defined in each Rd file
db_lifecycle <- function(db) {
  lifecycle_patterns <- paste0(
    "(?:",
    paste(
      collapse = "|",
      c(
        "lifecycle::badge\\([\\\\]\"",
        "rlang:::lifecycle\\([\\\\]\"",
        "list\\(\"lifecycle-",
        "https://www.tidyverse.org/lifecycle/#"
      )
    ),
    ")([\\w-]+)"
  )

  desc <- lapply(db, asNamespace("tools")$.Rd_get_metadata, "description")
  lapply(desc, function(x) {
    res <- regexpr(lifecycle_patterns, x, perl = TRUE)
    starts <- attr(res, "capture.start")
    ends <- starts + attr(res, "capture.length") - 1
    substring(x, starts, ends)
  })
}

# Retrieve the functions listed in the usage for each Rd file in the database
db_function <- function(db) {
  usage <- lapply(db, asNamespace("tools")$.Rd_get_section, "usage")
  lapply(usage, get_usage_function_names)
}

#' @param package The name of an installed package.
#' @param which The lifecycle statuses to retrieve.
#'   Include `NA` if you want to include functions without a specified lifecycle
#'   status in the results.
#' @export
#' @rdname lifecycle_linter
pkg_lifecycle_statuses <- function(
  package,
  which = c(
    "superseded",
    "deprecated",
    "questioning",
    "defunct",
    "experimental",
    "soft-deprecated",
    "retired"
  )
) {
  check_installed("vctrs")
  which <- match.arg(which, several.ok = TRUE)
  stopifnot(is_string(package))

  db <- tools::Rd_db(package)
  lc <- db_lifecycle(db)
  funs <- db_function(db)

  res <- mapply(
    function(lc, f) {
      data.frame(
        fun = f,
        lifecycle = rep(lc, length(f)),
        stringsAsFactors = FALSE
      )
    },
    lc,
    funs,
    SIMPLIFY = FALSE
  )

  res <- vctrs::vec_rbind(!!!res)

  # Filter funs without a lifecycle
  if (!NA %in% which) {
    res <- res[!is.na(res$lifecycle), ]
  }

  # Filter funs without a function name
  res <- res[nzchar(res$fun), ]

  # Filter method definitions
  res <- res[grep("[\\\\]method\\{", res$fun, invert = TRUE), ]

  # filter lifecycles not in which
  res <- res[res$lifecycle %in% which, ]

  if (nrow(res) == 0) {
    return(data.frame(
      package = character(),
      fun = character(),
      lifecycle = character()
    ))
  }

  res$package <- package

  res[c("package", "fun", "lifecycle")]
}

get_usage_function_names <- function(x) {
  if (!length(x)) {
    character(1)
  } else {
    res <- asNamespace("tools")$.parse_usage_as_much_as_possible(x)
    vapply(
      res,
      function(x) {
        if (is.call(x)) as.character(x[[1]]) else character(1)
      },
      character(1)
    )
  }
}

#' @rdname lifecycle_linter
#' @param path The directory path to the files you want to search.
#' @param pattern Any files matching this pattern will be searched. The default
#'   searches any files ending in `.R` or `.Rmd`.
#' @export
lint_lifecycle <- function(
  packages,
  path = ".",
  pattern = "(?i)[.](r|rmd|qmd|rnw|rhtml|rrst|rtex|rtxt)$",
  which = c(
    "superseded",
    "deprecated",
    "questioning",
    "defunct",
    "experimental",
    "soft-deprecated",
    "retired"
  ),
  symbol_is_undesirable = FALSE
) {
  which <- match.arg(which, several.ok = TRUE)

  check_installed(c("lintr", "vctrs", "xml2"))

  lintr::lint_dir(
    path = path,
    pattern = pattern,
    linters = lifecycle_linter(
      packages = packages,
      which = which,
      symbol_is_undesirable = symbol_is_undesirable
    )
  )
}

#' @rdname lifecycle_linter
#' @export
lint_tidyverse_lifecycle <- function(
  path = ".",
  pattern = "(?i)[.](r|rmd|qmd|rnw|rhtml|rrst|rtex|rtxt)$",
  which = c(
    "superseded",
    "deprecated",
    "questioning",
    "defunct",
    "experimental",
    "soft-deprecated",
    "retired"
  ),
  symbol_is_undesirable = FALSE
) {
  which <- match.arg(which, several.ok = TRUE)

  check_installed(c("lintr", "vctrs", "xml2", "tidyverse"))

  lint_lifecycle(
    packages = tidyverse::tidyverse_packages(),
    pattern = pattern,
    path = path,
    which = which,
    symbol_is_undesirable = symbol_is_undesirable
  )
}

#' Lint usages of functions that have a non-stable life cycle.
#'
#' - `lifecycle_linter()` creates a linter for lifecycle annotations which can be
#'   included in a `.lintr` configuration if `lintr` is used directly.
#' - `lint_lifecycle()` dynamically queries the package documentation for packages
#'   in `packages` for lifecycle annotations and then searches the directory in
#'   `path` for usages of those functions.
#' - `lint_tidyverse_lifecycle()` is a convenience function to call `lint_lifecycle()`
#'   for all the packages in the tidyverse.
#' - `pkg_lifecycle_statuses()` returns a data frame of functions with lifecycle
#'   annotations for an installed package.
#'
#' @param packages One or more installed packages to query for lifecycle statuses.
#' @param which Vector of lifecycle statuses to lint.
#' @param symbol_is_undesirable Also lint symbol usages, e.g. `lapply(x, is_na)`?
#'
#' @export
#' @examples
#' lintr::lint(
#'   text = "is_na(x)",
#'   linters = lifecycle_linter(packages = "rlang")
#' )
#' lintr::lint(
#'   text = "lapply(x, is_na)",
#'   linters = lifecycle_linter(packages = "rlang",
#'   symbol_is_undesirable = TRUE)
#' )
lifecycle_linter <- function(
  packages = tidyverse::tidyverse_packages(),
  which = c(
    "superseded",
    "deprecated",
    "questioning",
    "defunct",
    "experimental",
    "soft-deprecated",
    "retired"
  ),
  symbol_is_undesirable = FALSE
) {
  check_installed(c("lintr", "vctrs", "xml2"))

  life_cycles <- vctrs::vec_rbind(
    !!!lapply(packages, pkg_lifecycle_statuses, which = which)
  )
  bad_usages <- sprintf(
    "`%s::%s` is %s",
    life_cycles$package,
    life_cycles$fun,
    life_cycles$lifecycle
  )
  names(bad_usages) <- life_cycles$fun

  if (symbol_is_undesirable) {
    xpath <- sprintf(
      "//SYMBOL_FUNCTION_CALL[%1$s] | //SYMBOL[%1$s]",
      paste0("text() = '", names(bad_usages), "'", collapse = " or ")
    )
  } else {
    xpath <- sprintf(
      "//SYMBOL_FUNCTION_CALL[%s]",
      paste0("text() = '", names(bad_usages), "'", collapse = " or ")
    )
  }

  lintr::Linter(function(source_expression) {
    if (!lintr::is_lint_level(source_expression, "expression")) {
      return(list())
    }

    matched_nodes <- xml2::xml_find_all(
      source_expression$xml_parsed_content,
      xpath
    )
    fun_names <- lintr::get_r_string(matched_nodes)

    lintr::xml_nodes_to_lints(
      matched_nodes,
      source_expression = source_expression,
      lint_message = unname(bad_usages[fun_names])
    )
  })
}


# --- FILE: https://raw.githubusercontent.com/r-lib/lifecycle/main/R/signal.R ---
#' Signal other experimental or superseded features
#'
#' @description
#' `r badge("experimental")`
#'
#' `signal_stage()` allows you to signal life cycle stages other than
#' deprecation (for which you should use [deprecate_warn()] and friends). There
#' is no behaviour associated with this signal, it is currently purely a way to
#' express intent at the call site. In the future, we hope to replace this with
#' a standardized call to `base::declare()`.
#'
#' @param stage Life cycle stage, either `"experimental"` or `"superseded"`.
#'
#' @param what String describing what feature the stage applies too, using the
#'   same syntax as [deprecate_warn()].
#'
#' @param with An optional string giving a recommended replacement for a
#'   superseded function.
#'
#' @param env `r badge("deprecated")`
#'
#' @export
#' @examples
#' foofy <- function(x, y, z) {
#'   signal_stage("experimental", "foofy()")
#'   x + y / z
#' }
#' foofy(1, 2, 3)
signal_stage <- function(stage, what, with = NULL, env = deprecated()) {
  # Does nothing
  invisible()
}

#' Deprecated functions for signalling lifecycle stages
#'
#' @description
#' `r badge("deprecated")`
#' @name deprecated-signallers
#' @keywords internal
NULL

#' @rdname deprecated-signallers
#' @export
signal_experimental <- function(when, what, env = caller_env()) {
  deprecate_soft(
    "1.1.0",
    what = "signal_experimental()",
    with = "signal_stage()",
    id = "lifecycle_signal_experimental"
  )
  signal_stage("experimental", what)
}

#' @rdname deprecated-signallers
#' @export
signal_superseded <- function(when, what, env = caller_env()) {
  deprecate_soft(
    "1.1.0",
    what = "signal_superseded()",
    with = "signal_stage()",
    id = "lifecycle_signal_superseded"
  )
  signal_stage("superseded", what)
}


# --- FILE: https://raw.githubusercontent.com/r-lib/lifecycle/main/R/spec.R ---
spec <- function(
  spec,
  env = caller_env(),
  signaller = "signal_lifecycle",
  error_call = caller_env()
) {
  ctxt <- list(
    signaller = signaller,
    call = error_call
  )

  if (inherits(spec, "AsIs")) {
    list(
      fn = spec,
      arg = NULL,
      pkg = spec_pkg(NULL, env, ctxt = ctxt),
      reason = NULL,
      from = signaller
    )
  } else {
    what <- parse_what(spec, ctxt = ctxt)

    list(
      fn = spec_fn(what$call, ctxt = ctxt),
      arg = spec_arg(what$call, ctxt = ctxt),
      pkg = spec_pkg(what$pkg, env, ctxt = ctxt),
      reason = spec_reason(what$call, ctxt = ctxt),
      from = signaller
    )
  }
}

parse_what <- function(what, ctxt) {
  check_string(what, call = ctxt$call)

  call <- parse_expr(what)

  if (!is_call(call)) {
    what <- as_string(what)
    cli::cli_abort(
      c(
        "{.arg what} must have function call syntax.",
        "",
        " " = "# Good:",
        " " = "{ ctxt$signaller }(\"{what}()\")",
        "",
        " " = "# Bad:",
        " " = "{ ctxt$signaller }(\"{what}\")"
      ),
      call = ctxt$call,
      arg = "what"
    )
  }

  head <- call[[1]]
  if (is_call(head, "::")) {
    pkg <- as_string(head[[2]])
    call[[1]] <- head[[3]]
  } else {
    pkg <- NULL
  }

  list(pkg = pkg, call = call)
}

spec_fn <- function(call, ctxt) {
  fn <- node_car(call)

  if (!is_symbol(fn) && !is_call(fn, "$")) {
    cli::cli_abort(
      "{.arg what} must be a function or method call.",
      call = ctxt$call,
      arg = "what"
    )
  }

  # Deparse so non-syntactic names are backticked
  deparse(fn)
}

spec_arg <- function(call, ctxt) {
  arg <- node_cdr(call)

  if (is_null(arg)) {
    return(NULL)
  }

  if (length(arg) != 1L) {
    fn <- as_label(node_car(call))
    n <- length(arg)
    cli::cli_abort(
      "Function in {.arg what} ({fn}) must have 1 argument, not {n}.",
      call = ctxt$call
    )
  }

  if (is_null(node_tag(arg))) {
    as_string(node_car(arg))
  } else {
    as_string(node_tag(arg))
  }
}

spec_reason <- function(call, ctxt) {
  arg <- node_cdr(call)

  if (is_null(arg)) {
    return(NULL)
  }

  if (is_null(node_tag(arg))) {
    return(NULL)
  }

  if (is_missing(node_car(arg))) {
    return(NULL)
  }

  if (is_string(node_car(arg))) {
    return(node_car(arg))
  }

  fn <- deparse(node_car(call))
  cli::cli_abort(
    c(
      "{.arg what} must contain reason as a string on the RHS of `=`.",
      "",
      " " = "# Good:",
      " " = "{ctxt$signaller}(\"{fn}(arg = 'must be a string')\")",
      "",
      " " = "# Bad:",
      " " = "{ctxt$signaller}(\"{fn}(arg = 42)\")"
    ),
    call = ctxt$call
  )
}

spec_pkg <- function(pkg, env, ctxt) {
  if (!is_null(pkg) || is_null(env)) {
    return(pkg)
  }

  env <- topenv(env)
  if (is_reference(env, global_env())) {
    # Convenient for experimenting interactively
    return(getOption("lifecycle:::calling_package", "<NA>"))
  }

  if (is_namespace(env)) {
    return(ns_env_name(env))
  }

  cli::cli_abort(
    c(
      "Can't detect the package of the deprecated function.",
      "Please mention the namespace:",
      "",
      " " = "# Good:",
      " " = "{ ctxt$signaller }(what = \"namespace::myfunction()\")",
      "",
      " " = "# Bad:",
      " " = "{ ctxt$signaller }(what = \"myfunction()\")",
      ""
    ),
    call = ctxt$call
  )
}


# --- FILE: https://raw.githubusercontent.com/r-lib/lifecycle/main/R/utils.R ---
upcase1 <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

cat_line <- function(...) {
  cat(paste0(..., "\n", collapse = ""))
}
paste_line <- function(...) {
  paste(chr(...), collapse = "\n")
}

pkg_url_bug <- function(pkg) {
  # First check that package is installed, e.g. in case of
  # runtime-only namespace created by pkgload
  if (nzchar(system.file(package = pkg))) {
    url <- utils::packageDescription(pkg)$BugReports

    # `url` can be NULL if not part of the description
    if (is_string(url) && grepl("^https://", url)) {
      return(url)
    }
  }

  NULL
}


# --- FILE: https://raw.githubusercontent.com/r-lib/lifecycle/main/R/verbosity.R ---
#' Control the verbosity of deprecation signals
#'
#' @description
#'
#' There are 3 levels of verbosity for deprecated functions: silence,
#' warning, and error. Since the lifecycle package avoids disruptive
#' warnings, the default level of verbosity depends on the lifecycle
#' stage of the deprecated function, on the context of the caller
#' (global environment or testthat unit tests cause more warnings),
#' and whether the warning was already issued (see the help for
#' [deprecation functions][deprecate_soft]).
#'
#' You can control the level of verbosity with the global option
#' `lifecycle_verbosity`. It can be set to:
#'
#' * `"quiet"` to suppress all deprecation messages.
#' * `"default"` or `NULL` to warn once per session.
#' * `"warning"` to warn every time.
#' * `"error"` to error instead of warning.
#'
#' Note that functions calling [deprecate_stop()] invariably throw
#' errors.
#'
#' @examples
#' if (rlang::is_installed("testthat")) {
#'   library(testthat)
#'
#'   mytool <- function() {
#'     deprecate_soft("1.0.0", "mytool()")
#'     10 * 10
#'   }
#'
#'   # Forcing the verbosity level is useful for unit testing. You can
#'   # force errors to test that the function is indeed deprecated:
#'   test_that("mytool is deprecated", {
#'     rlang::local_options(lifecycle_verbosity = "error")
#'     expect_error(mytool(), class = "defunctError")
#'   })
#'
#'   # Or you can enforce silence to safely test that the function
#'   # still works:
#'   test_that("mytool still works", {
#'     rlang::local_options(lifecycle_verbosity = "quiet")
#'     expect_equal(mytool(), 100)
#'   })
#' }
#' @name verbosity
NULL

lifecycle_verbosity <- function() {
  opt <- peek_option("lifecycle_verbosity")

  if (is_null(opt)) {
    return("default")
  }

  if (!is_string(opt, c("quiet", "default", "warning", "error"))) {
    options(lifecycle_verbosity = "default")

    message <- paste(
      sep = " ",
      "The `lifecycle_verbosity` option must be set to one of:",
      "\"quiet\", \"default\", \"warning\", or \"error\".",
      "Resetting to \"default\"."
    )

    warn(message)
  }

  opt
}


# --- FILE: https://raw.githubusercontent.com/r-lib/lifecycle/main/R/warning.R ---
#' Display last deprecation warnings
#'
#' @description
#'
#' `last_lifecycle_warnings()` returns a list of all warnings that
#' occurred during the last top-level R command, along with a
#' backtrace.
#'
#' Use `print(last_lifecycle_warnings(), simplify = level)` to control
#' the verbosity of the backtrace. The `simplify` argument supports
#' one of `"branch"` (the default), `"collapse"`, and `"none"` (in
#' increasing order of verbosity).
#'
#' @examples
#' # These examples are not run because `last_lifecycle_warnings()` does not
#' # work well within knitr and pkgdown
#' \dontrun{
#'
#' f <- function() invisible(g())
#' g <- function() list(h(), i())
#' h <- function() deprecate_warn("1.0.0", "this()")
#' i <- function() deprecate_warn("1.0.0", "that()")
#' f()
#'
#' # Print all the warnings that occurred during the last command:
#' last_lifecycle_warnings()
#'
#'
#' # By default, the backtraces are printed in their simplified form.
#' # Use `simplify` to control the verbosity:
#' print(last_lifecycle_warnings(), simplify = "none")
#' }
#' @export
last_lifecycle_warnings <- function() {
  structure(
    warnings_env$warnings,
    class = c("lifecycle_warnings", "list")
  )
}

new_deprecated_warning <- function(msg, trace, ..., footer = NULL) {
  warning_cnd(
    "lifecycle_warning_deprecated",
    message = msg,
    trace = trace,
    footer = footer,
    internal = list(...)
  )
}

#' @export
print.lifecycle_warnings <- function(x, ...) {
  local_interactive(FALSE)
  print(unclass(x))
}

warnings_env <- env(empty_env())

init_warnings <- function() {
  warnings_env$last_top_frame <- NULL
  warnings_env$warnings <- list()
}
init_warnings()

push_warning <- function(wrn) {
  current <- obj_address(sys.frame(1))

  if (identical(warnings_env$last_top_frame, current)) {
    warnings_env$warnings <- c(warnings_env$warnings, list(wrn))
  } else {
    warnings_env$last_top_frame <- current
    warnings_env$warnings <- list(wrn)
  }
}


# Contains unique IDs of deprecated features so we don't warn multiple times
deprecation_env <- env(empty_env())


# --- FILE: https://raw.githubusercontent.com/r-lib/lifecycle/main/tests/testthat.R ---
library(testthat)
library(lifecycle)

test_check("lifecycle")


# --- FILE: https://raw.githubusercontent.com/r-lib/lifecycle/main/tests/testthat/helper-lifecycle.R ---
expect_lifecycle_defunct <- function(object, ...) {
  expect_error(object, class = "defunctError")
}

expect_no_warning <- function(...) {
  expect_warning(regexp = NA, ...)
}

try2 <- function(expr) {
  cat(paste0("\n", as_label2(substitute(expr)), ":\n\n"))
  cat(catch_cnd(expr, classes = "error")$message, "\n\n")
}

cat_ruler <- function(title) {
  cat(paste0("\n\n", title, "\n", strrep("=", nchar(title)), "\n\n"))
}

spec_data <- function(
  fn = NULL,
  arg = NULL,
  pkg = spec_pkg(NULL, caller_env()),
  reason = NULL,
  from = "signal_lifecycle"
) {
  list(
    fn = fn,
    arg = arg,
    pkg = pkg,
    reason = reason,
    from = from
  )
}

new_callers <- function(deprecated_feature, env = caller_env()) {
  direct <- inject(function(...) (!!deprecated_feature)(...))
  indirect <- inject(function(...) (!!deprecated_feature)(...))

  environment(direct) <- global_env()
  environment(indirect) <- ns_env("base")

  list(direct, indirect)
}


# --- FILE: https://raw.githubusercontent.com/r-lib/lifecycle/main/tests/testthat/helper-zeallot.R ---
# nocov start --- compat-zeallot --- 2020-11-23

# This drop-in file implements a simple version of zeallot::`%<-%`.
# Please find the most recent version in rlang's repository.

`%<-%` <- function(lhs, value) {
  lhs <- substitute(lhs)
  env <- caller_env()

  if (!is_call(lhs, "c")) {
    abort("The left-hand side of `%<-%` must be a call to `c()`.")
  }

  vars <- as.list(lhs[-1])

  if (length(value) != length(vars)) {
    abort("The left- and right-hand sides of `%<-%` must be the same length.")
  }

  for (i in seq_along(vars)) {
    var <- vars[[i]]
    if (!is_symbol(var)) {
      abort(paste0(
        "Element ",
        i,
        " of the left-hand side of `%<-%` must be a symbol."
      ))
    }

    env[[as_string(var)]] <- value[[i]]
  }

  invisible(value)
}

# nocov end


# --- FILE: https://raw.githubusercontent.com/r-lib/lifecycle/main/tests/testthat/test-arg.R ---
test_that("deprecated() returns the missing argument", {
  fn <- function(foo = deprecated()) is_present(foo)
  expect_false(fn())
  expect_true(fn(1))

  fn <- function(foo) is_present(foo)
  expect_false(fn())
  expect_true(fn(1))
})


# --- FILE: https://raw.githubusercontent.com/r-lib/lifecycle/main/tests/testthat/test-badge.R ---
test_that("badge doesn't change unexpected", {
  expect_snapshot(cat(badge("deprecated")))
  expect_snapshot(cat(badge("experimental")))
  expect_snapshot(cat(badge("unknown")), error = TRUE)
})


# --- FILE: https://raw.githubusercontent.com/r-lib/lifecycle/main/tests/testthat/test-deprecate.R ---
# lifecycle verbosity -----------------------------------------------------

test_that("default deprecations behave as expected", {
  on.exit(env_unbind(deprecation_env, "test"))
  local_options(lifecycle_verbosity = "default")

  deprecated_feature <- function(...) {
    deprecate_warn("1.0.0", "foo()", with = "bar()", id = "test", ...)
  }
  c(direct, indirect) %<-% new_callers(deprecated_feature)

  expect_snapshot({
    (expect_warning(direct(), class = "lifecycle_warning_deprecated"))
  })
  expect_warning(indirect(), NA)
  expect_warning(indirect(), NA)

  expect_snapshot({
    (expect_defunct(deprecate_stop("1.0.0", "foo()")))
  })
})

test_that("deprecate_warn() only warns repeatedly if always = TRUE", {
  on.exit(env_unbind(deprecation_env, "test"))
  local_options(lifecycle_verbosity = "default")

  deprecated_feature <- function(...) {
    deprecate_warn("1.0.0", "foo()", id = "test", ...)
  }
  c(direct, indirect) %<-% new_callers(deprecated_feature)

  expect_snapshot({
    direct()
    direct()
    indirect()
    indirect()
  })

  expect_snapshot({
    direct(always = TRUE)
    direct(always = TRUE)
    indirect(always = TRUE)
    indirect(always = TRUE)
  })
})

test_that("indirect usage recommends contacting authors", {
  on.exit(env_unbind(deprecation_env, c("test_base", "test_rlang")))
  local_options(lifecycle_verbosity = "default")

  deprecated_feature <- function(..., id) {
    deprecate_warn("1.0.0", "foo()", id = id, ...)
  }
  c(direct, indirect) %<-% new_callers(deprecated_feature)

  # To test for URL
  indirect_rlang <- indirect
  environment(indirect_rlang) <- ns_env("rlang")

  expect_snapshot({
    indirect(id = "test_base")
    indirect_rlang(id = "test_rlang")
  })
})

test_that("quiet suppresses _soft and _warn", {
  local_options(lifecycle_verbosity = "quiet")

  expect_warning(deprecate_soft("1.0.0", "foo()"), NA)
  expect_warning(deprecate_warn("1.0.0", "foo()"), NA)
  expect_defunct(deprecate_stop("1.0.0", "foo()"))
})

test_that("warning always warns in _soft and _warn", {
  local_options(lifecycle_verbosity = "warning")

  expect_deprecated(deprecate_warn("1.0.0", "foo()"))
  expect_defunct(deprecate_stop("1.0.0", "foo()"))
})

test_that("error coverts _soft and _warn to errors", {
  local_options(lifecycle_verbosity = "error")

  expect_defunct(deprecate_soft("1.0.0", "foo()"))
  expect_defunct(deprecate_warn("1.0.0", "foo()"))
  expect_defunct(deprecate_stop("1.0.0", "foo()"))
})

test_that("soft deprecation uses correct calling envs", {
  withr::local_envvar(TESTTHAT_PKG = "testpackage")

  # Simulate package functions available from global environment
  env <- new_environment(parent = ns_env("lifecycle"))
  local(envir = env, {
    softly <- function() {
      deprecate_soft("1.0.0", "softly()")
    }
    softly_softly <- function() {
      softly()
    }
  })
  local_bindings(!!!as.list(env), .env = global_env())

  # Calling package function directly should warn
  cnd <- catch_cnd(evalq(softly(), global_env()), "warning")
  expect_s3_class(cnd, class = "lifecycle_warning_deprecated")
  expect_match(conditionMessage(cnd), "lifecycle")

  # Calling package function indirectly from global env shouldn't
  cnd <- catch_cnd(evalq(softly_softly(), global_env()), "warning")
  expect_equal(cnd, NULL)
})

test_that("warning conditions are signaled only once if warnings are suppressed", {
  local_options(lifecycle_verbosity = "warning")

  deprecated_feature <- function(...) deprecate_warn(...)
  c(direct, indirect) %<-% new_callers(deprecated_feature)

  x <- 0L
  suppressWarnings(withCallingHandlers(
    warning = function(...) x <<- x + 1L,
    {
      direct("1.0.0", "foo()")
      indirect("1.0.0", "foo()")
    }
  ))

  expect_identical(x, 1L)
})

# messaging ---------------------------------------------------------------

test_that("what deprecation messages are readable", {
  expect_snapshot({
    cat_line(lifecycle_message("1.0.0", "foo()"))
    cat_line(lifecycle_message("1.0.0", "foo()", signaller = "deprecate_stop"))
    cat_line(lifecycle_message("1.0.0", "foo(arg)"))
    cat_line(lifecycle_message(
      "1.0.0",
      "foo(arg)",
      signaller = "deprecate_stop"
    ))
    cat_line(lifecycle_message("1.0.0", I("Use of bananas")))
    cat_line(lifecycle_message(
      "1.0.0",
      I("Use of bananas"),
      signaller = "deprecate_stop"
    ))
  })
})

test_that("replace deprecation messages are readable", {
  expect_snapshot({
    cat_line(lifecycle_message("1.0.0", "foo()", "package::bar()"))

    cat_line(lifecycle_message("1.0.0", "foo()", "bar()"))
    cat_line(lifecycle_message("1.0.0", "foo(arg1)", "foo(arg2)"))
    cat_line(lifecycle_message("1.0.0", "foo(arg)", "bar(arg)"))

    cat_line(lifecycle_message("1.0.0", I("Use of bananas"), I("apples")))
  })
})

test_that("unusual names are handled gracefully", {
  expect_snapshot({
    cat_line(lifecycle_message("1.0.0", "`foo-fy`(`qu-ux` = )"))
    cat_line(lifecycle_message("1.0.0", "`foo<-`()"))
    cat_line(lifecycle_message("1.0.0", "`+`()"))
  })
})

test_that("details uses an info bullet by default", {
  on.exit(env_unbind(deprecation_env, "test"))
  expect_snapshot({
    deprecate_warn(
      "1.0.0",
      "foo()",
      details = "Please do that instead.",
      id = "test"
    )
  })

  env_unbind(deprecation_env, "test")
  expect_snapshot({
    deprecate_warn(
      "1.0.0",
      "foo()",
      details = c("Please do that instead.", "Also know that."),
      id = "test"
    )
  })
})

test_that("can use bullets in details ", {
  on.exit(env_unbind(deprecation_env, "test"))
  expect_snapshot({
    deprecate_warn(
      "1.0.0",
      "foo()",
      details = c(
        "Unnamed",
        i = "Informative",
        x = "Error"
      ),
      id = "test"
    )
  })
})

test_that("checks input types", {
  expect_snapshot(lifecycle_message(1), error = TRUE)
  expect_snapshot(lifecycle_message("1", details = 1), error = TRUE)
})

test_that("lifecycle message is never generated when an `id` is supplied and we've already warned", {
  # This is an important test for performance reasons. Supplying an `id` makes
  # repeated calls to `deprecate_soft()` and `deprecate_warn()` much faster by
  # avoiding message generation via `lifecycle_message()`.

  on.exit(env_unbind(deprecation_env, c("test")))
  local_options(lifecycle_verbosity = "default")

  # Mock having already warned this session
  env_poke(deprecation_env, "test", TRUE)

  # These arguments are never touched again if we supply an `id`,
  # we expect silence in the snapshot
  expect_snapshot({
    deprecate_soft(
      when = stop("when"),
      what = stop("what"),
      with = stop("with"),
      details = stop("details"),
      env = stop("env"),
      id = "test"
    )
  })
  expect_snapshot({
    deprecate_warn(
      when = stop("when"),
      what = stop("what"),
      with = stop("with"),
      details = stop("details"),
      env = stop("env"),
      id = "test"
    )
  })
})

# helpers -----------------------------------------------------------------

test_that("env_inherits_global works for simple cases", {
  expect_false(env_inherits_global(empty_env()))

  env <- new_environment(parent = global_env())
  expect_true(env_inherits_global(env))
})

test_that("needs_warning works as expected", {
  on.exit(env_unbind(deprecation_env, "test"))

  expect_snapshot(needs_warning(1), error = TRUE)
  expect_true(needs_warning("test"))

  env_poke(deprecation_env, "test", TRUE)
  expect_false(needs_warning("test"))
})


# --- FILE: https://raw.githubusercontent.com/r-lib/lifecycle/main/tests/testthat/test-expect.R ---
test_that("expect_deprecated() expects lifecycle warnings", {
  fn <- function() deprecate_warn("1.0", "pkg::foo()", id = "expect-deprecated")
  expect_success(expect_deprecated(fn()))
  expect_success(expect_deprecated(fn()))
  expect_failure(expect_deprecated(NULL))
})

test_that("expect_defunct() expects lifecycle errors", {
  fn <- function() deprecate_stop("1.0", "pkg::foo()")
  expect_success(expect_defunct(fn()))
  expect_failure(expect_defunct(NULL))
})


# --- FILE: https://raw.githubusercontent.com/r-lib/lifecycle/main/tests/testthat/test-lifecycle.R ---
test_that("deprecate_soft() warns when called from global env", {
  withr::local_envvar(TESTTHAT_PKG = "testpackage")

  fn <- function(id) {
    deprecate_soft("1.0.0", "foo()", id = id)
  }
  expect_no_warning(fn("called from local env"))

  local_bindings(.env = global_env(), fn = fn)
  env_bind_lazy(
    current_env(),
    do = fn("called from global env"),
    .eval_env = global_env()
  )

  expect_deprecated(do, "foo")
})

test_that("deprecate_soft() warns when called from package being tested", {
  old <- Sys.getenv("NOT_CRAN")
  on.exit(Sys.setenv("NOT_CRAN" = old))

  Sys.setenv("NOT_CRAN" = "true")
  retired <- function() deprecate_soft("1.0.0", "foo()")
  expect_warning(retired(), "was deprecated")
})

test_that("deprecate_soft() warns when option is set", {
  retired <- function(id) deprecate_soft("1.0.0", "foo()", id = id)
  with_options(lifecycle_verbosity = "warning", {
    expect_warning(retired("rlang_test5"), "was deprecated")
  })
})

test_that("deprecate_warn() repeats warnings when option is set", {
  local_options(lifecycle_verbosity = "warning")

  retired1 <- function() deprecate_soft("1.0.0", "foo()", id = "signal repeat")
  retired2 <- function() deprecate_warn("1.0.0", "foo()", id = "warn repeat")

  expect_warning(retired1(), "was deprecated")
  expect_warning(retired2(), "was deprecated")

  expect_warning(retired1(), "was deprecated")
  expect_warning(retired2(), "was deprecated")
})

test_that("can promote lifecycle warnings to errors", {
  local_options(lifecycle_verbosity = "error")
  expect_lifecycle_defunct(deprecate_soft("1.0.0", "foo()"), "was deprecated")
  expect_lifecycle_defunct(deprecate_warn("1.0.0", "foo()"), "was deprecated")
})

test_that("soft-deprecation warnings are issued when called from child of global env as well", {
  fn <- function() deprecate_soft("1.0.0", "foo()", id = "child of global env")
  expect_warning(eval_bare(call2(fn), env(global_env())), "was deprecated")
})

test_that("deprecation warnings are not displayed again", {
  local_interactive()
  on.exit(env_unbind(deprecation_env, c("once-per-session", "unconditional")))

  # With `"default"` - warning only shown once per session
  local_options(lifecycle_verbosity = "default")

  retired <- function() {
    deprecate_warn("1.0.0", "foo()", id = "once-per-session")
  }

  # First and only time shows "once per session" message
  wrn <- catch_cnd(retired(), classes = "lifecycle_warning_deprecated")
  expect_s3_class(wrn, "lifecycle_warning_deprecated")
  expect_snapshot(wrn)

  # Calling it again gives no warning at all
  wrn <- catch_cnd(retired(), classes = "lifecycle_warning_deprecated")
  expect_null(wrn)

  # With `"warning"` - warning shows unconditionally
  local_options(lifecycle_verbosity = "warning")

  retired <- function() {
    deprecate_warn("1.0.0", "foo()", id = "unconditional")
  }

  # First time doesn't show "once per session" message because we always warn
  wrn <- catch_cnd(retired(), classes = "lifecycle_warning_deprecated")
  expect_s3_class(wrn, "lifecycle_warning_deprecated")
  expect_snapshot(wrn)

  # Calling it again shows the warning again, again without "once per session"
  wrn <- catch_cnd(retired(), classes = "lifecycle_warning_deprecated")
  expect_s3_class(wrn, "lifecycle_warning_deprecated")
  expect_snapshot(wrn)
})

test_that("the topenv of the empty env is not the global env", {
  local_options(lifecycle_verbosity = NULL)
  expect_silent(deprecate_soft(
    "1.0.0",
    "foo()",
    env = empty_env(),
    id = "topenv of empty env"
  ))
})

test_that("expect_deprecated() matches regexp", {
  expect_deprecated(
    deprecate_warn("1.0", "fn()", details = "foo.["),
    "foo.[",
    fixed = TRUE
  )

  fn <- function() {
    deprecate_soft("1.0.0", "fn()")
  }
  expect_deprecated(fn(), "fn")
  expect_deprecated(expect_failure(
    expect_deprecated(fn(), "foo")
  ))
})


# --- FILE: https://raw.githubusercontent.com/r-lib/lifecycle/main/tests/testthat/test-signal.R ---
test_that("`signal_stage()` does nothing", {
  expect_null(signal_stage("experimental", "pkg::foo(bar = 'baz')"), NULL)
})

test_that("`signal_experimental()` and `signal_superseded()` are deprecated", {
  expect_snapshot({
    signal_experimental("1.1.0", "foo()")
  })
  expect_snapshot({
    signal_superseded("1.1.0", "foo()")
  })
})


# --- FILE: https://raw.githubusercontent.com/r-lib/lifecycle/main/tests/testthat/test-spec.R ---
test_that("spec() builds feature data", {
  expect_identical(
    spec("foo()"),
    spec_data(fn = "foo")
  )
  expect_identical(
    spec("pkg::foo()"),
    spec_data(fn = "foo", pkg = "pkg")
  )

  expect_identical(
    spec("foo(bar)"),
    spec_data(fn = "foo", arg = "bar")
  )
  expect_identical(
    spec("foo(bar = )"),
    spec_data(fn = "foo", arg = "bar")
  )
  expect_identical(
    spec("pkg::foo(bar = )"),
    spec_data(fn = "foo", arg = "bar", pkg = "pkg")
  )

  expect_identical(
    spec("foo(bar = 'baz')"),
    spec_data(fn = "foo", arg = "bar", reason = "baz")
  )
  expect_identical(
    spec("pkg::foo(bar = 'baz')"),
    spec_data(fn = "foo", arg = "bar", pkg = "pkg", reason = "baz")
  )
})

test_that("spec() gives useful errors", {
  expect_snapshot(spec(1), error = TRUE)
  expect_snapshot(spec("foo"), error = TRUE)
  expect_snapshot(spec("foo()()"), error = TRUE)
  expect_snapshot(spec("foo(arg = , arg = )"), error = TRUE)
  expect_snapshot(spec("foo(arg = arg)"), error = TRUE)

  e <- new_environment()
  local_options(topLevelEnvironment = e)
  expect_snapshot(spec("foo()", env = e), error = TRUE)
})

test_that("spec() works with methods", {
  expect_identical(
    spec("A$foo()"),
    spec_data(fn = "A$foo")
  )
  expect_identical(
    spec("A$foo(bar = )"),
    spec_data(fn = "A$foo", arg = "bar")
  )

  expect_snapshot(spec("A$foo(bar = 1)"), error = TRUE)
})


# --- FILE: https://raw.githubusercontent.com/r-lib/lifecycle/main/tests/testthat/test-verbosity.R ---
test_that("verbosity option is validated", {
  opt <- with_options(lifecycle_verbosity = NULL, lifecycle_verbosity())
  expect_identical(opt, "default")

  expect_warning(
    with_options(lifecycle_verbosity = NA, lifecycle_verbosity()),
    "must be set to one of"
  )
})

test_that("verbosity option has precedence over user env (#113)", {
  mytool <- function() {
    deprecate_soft("1.0.0", "mytool()")
    10 * 10
  }

  rlang::local_options(lifecycle_verbosity = "error")

  expect_error(
    exec(mytool, .env = global_env()),
    class = "defunctError"
  )
})


# --- FILE: https://raw.githubusercontent.com/r-lib/lifecycle/main/tests/testthat/test-warning.R ---
test_that("deprecation warning is displayed with backtrace", {
  skip_on_cran()
  skip_on_os("windows")

  init_warnings()

  local_options(
    rlang_trace_top_env = current_env(),
    rlang_trace_format_srcrefs = FALSE
  )

  f <- function() g()
  g <- function() h()
  h <- function() deprecate_warn("1.0.0", "trace()")

  expect_deprecated(f())

  expect_snapshot({
    last_lifecycle_warnings()
  })
})
