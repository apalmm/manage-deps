

# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/R/adverb-auto-browse.R ---
#' Wrap a function so it will automatically `browse()` on error
#'
#' A function wrapped with `auto_browse()` will automatically enter an
#' interactive debugger using [browser()] when ever it encounters an error.
#'
#' @inheritParams safely
#' @inheritSection safely Adverbs
#' @inherit safely return
#' @family adverbs
#' @export
#' @examples
#' # For interactive usage, auto_browse() is useful because it automatically
#' # starts a browser() in the right place.
#' f <- function(x) {
#'   y <- 20
#'   if (x > 5) {
#'     stop("!")
#'   } else {
#'     x
#'   }
#' }
#' if (interactive()) {
#'   map(1:6, auto_browse(f))
#' }
#'
auto_browse <- function(.f) {
  if (is_primitive(.f)) {
    cli::cli_abort(
      "{.arg .f} must not be a primitive function.",
      arg = ".f"
    )
  }

  function(...) {
    withCallingHandlers(
      .f(...),
      error = function(e) {
        # 1: h(simpleError(msg, call))
        # 2: .handleSimpleError(function (e)  <...>
        # 3: stop(...)
        frame <- sys.frame(4)
        browse_in_frame(frame)
      },
      warning = function(e) {
        if (getOption("warn") >= 2) {
          frame <- sys.frame(7)
          browse_in_frame(frame)
        }
      }
    )
  }
}

browse_in_frame <- function(frame) {
  # ESS should problably set `.Platform$GUI == "ESS"`
  # In the meantime, check that ESSR is attached
  if (is_attached("ESSR")) {
    # Workaround ESS issue
    with_env(
      frame,
      on.exit({
        browser()
        NULL
      })
    )
    return_from(frame)
  } else {
    eval_bare(quote(browser()), env = frame)
  }
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/R/adverb-compose.R ---
#' Compose multiple functions together to create a new function
#'
#' Create a new function that is the composition of multiple functions,
#' i.e. `compose(f, g)` is equivalent to `function(...) f(g(...))`.
#'
#' @param ... Functions to apply in order (from right to left by
#'   default). Formulas are converted to functions in the usual way.
#'
#'   [Dynamic dots][rlang::dyn-dots] are supported. In particular, if
#'   your functions are stored in a list, you can splice that in with
#'   `!!!`.
#' @param .dir If `"backward"` (the default), the functions are called
#'   in the reverse order, from right to left, as is conventional in
#'   mathematics. If `"forward"`, they are called from left to right.
#' @inheritSection safely Adverbs
#' @family adverbs
#' @return A function
#' @export
#' @examples
#' not_null <- compose(`!`, is.null)
#' not_null(4)
#' not_null(NULL)
#'
#' add1 <- function(x) x + 1
#' compose(add1, add1)(8)
#'
#' fn <- compose(\(x) paste(x, "foo"), \(x) paste(x, "bar"))
#' fn("input")
#'
#' # Lists of functions can be spliced with !!!
#' fns <- list(
#'   function(x) paste(x, "foo"),
#'   \(x) paste(x, "bar")
#' )
#' fn <- compose(!!!fns)
#' fn("input")
compose <- function(..., .dir = c("backward", "forward")) {
  .dir <- arg_match0(.dir, c("backward", "forward"))

  fns <- map(list2(...), rlang::as_closure, env = caller_env())
  if (!length(fns)) {
    # Return the identity function
    return(compose(function(x, ...) x))
  }

  if (.dir == "backward") {
    n <- length(fns)
    first_fn <- fns[[n]]
    fns <- rev(fns[-n])
  } else {
    first_fn <- fns[[1]]
    fns <- fns[-1]
  }

  composed <- function() {
    env <- env(caller_env(), `_fn` = first_fn)

    first_call <- sys.call()
    first_call[[1]] <- quote(`_fn`)
    env$`_out` <- .Call(purrr_eval, first_call, env)

    call <- quote(`_fn`(`_out`))

    for (fn in fns) {
      env$`_fn` <- fn
      env$`_out` <- .Call(purrr_eval, call, env)
    }

    env$`_out`
  }
  formals(composed) <- formals(first_fn)

  structure(
    composed,
    class = c("purrr_function_compose", "function"),
    first_fn = first_fn,
    fns = fns
  )
}

#' @export
print.purrr_function_compose <- function(x, ...) {
  cat("<composed>\n")

  first <- attr(x, "first_fn")
  cat("1. ")
  print(first, ...)

  fns <- attr(x, "fns")
  for (i in seq_along(fns)) {
    cat(sprintf("\n%d. ", i + 1))
    print(fns[[i]], ...)
  }

  invisible(x)
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/R/adverb-insistently.R ---
#' Transform a function to wait then retry after an error
#'
#' @description
#' `insistently()` takes a function and modifies it to retry after given
#' amount of time whenever it errors.
#'
#' @inheritParams safely
#' @param rate A [rate][rate-helpers] object. Defaults to jittered exponential
#'   backoff.
#' @inheritParams rate_sleep
#' @seealso [httr::RETRY()] is a special case of [insistently()] for
#'   HTTP verbs.
#' @inheritSection safely Adverbs
#' @inherit safely return
#' @family adverbs
#' @export
#' @examples
#' # For the purpose of this example, we first create a custom rate
#' # object with a low waiting time between attempts:
#' rate <- rate_delay(0.1)
#'
#' # insistently() makes a function repeatedly try to work
#' risky_runif <- function(lo = 0, hi = 1) {
#'   y <- runif(1, lo, hi)
#'   if(y < 0.9) {
#'     stop(y, " is too small")
#'   }
#'   y
#' }
#'
#' # Let's now create an exponential backoff rate with a low waiting
#' # time between attempts:
#' rate <- rate_backoff(pause_base = 0.1, pause_min = 0.005, max_times = 4)
#'
#' # Modify your function to run insistently.
#' insistent_risky_runif <- insistently(risky_runif, rate, quiet = FALSE)
#'
#' set.seed(6) # Succeeding seed
#' insistent_risky_runif()
#'
#' set.seed(3) # Failing seed
#' try(insistent_risky_runif())
#'
#' # You can also use other types of rate settings, like a delay rate
#' # that waits for a fixed amount of time. Be aware that a delay rate
#' # has an infinite amount of attempts by default:
#' rate <- rate_delay(0.2, max_times = 3)
#' insistent_risky_runif <- insistently(risky_runif, rate = rate, quiet = FALSE)
#' try(insistent_risky_runif())
#'
#' # insistently() and possibly() are a useful combination
#' rate <- rate_backoff(pause_base = 0.1, pause_min = 0.005)
#' possibly_insistent_risky_runif <- possibly(insistent_risky_runif, otherwise = -99)
#'
#' set.seed(6)
#' possibly_insistent_risky_runif()
#'
#' set.seed(3)
#' possibly_insistent_risky_runif()
insistently <- function(f, rate = rate_backoff(), quiet = TRUE) {
  f <- as_mapper(f)
  check_rate(rate)
  check_bool(quiet)

  function(...) {
    rate_reset(rate)

    repeat {
      rate_sleep(rate, quiet = quiet)
      out <- capture_error(f(...), quiet = quiet)

      if (is_null(out$error)) {
        return(out$result)
      }
    }
  }
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/R/adverb-negate.R ---
#' Negate a predicate function so it selects what it previously rejected
#'
#' Negating a function changes `TRUE` to `FALSE` and `FALSE` to `TRUE`.
#'
#' @inheritParams keep
#' @inheritSection safely Adverbs
#' @family adverbs
#' @return A new predicate function.
#' @export
#' @examples
#' x <- list(x = 1:10, y = rbernoulli(10), z = letters)
#' x |> keep(is.numeric) |> names()
#' x |> keep(negate(is.numeric)) |> names()
#' # Same as
#' x |> discard(is.numeric)
negate <- function(.p) {
  compose(`!`, as_mapper(.p))
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/R/adverb-partial.R ---
#' Partially apply a function, filling in some arguments
#'
#' Partial function application allows you to modify a function by pre-filling
#' some of the arguments. It is particularly useful in conjunction with
#' functionals and other function operators.
#'
#' @details
#' `partial()` creates a function that takes `...` arguments. Unlike
#' [compose()] and other function operators like [negate()], it
#' doesn't reuse the function signature of `.f`. This is because
#' `partial()` explicitly supports NSE functions that use
#' `substitute()` on their arguments. The only way to support those is
#' to forward arguments through dots.
#'
#' Other unsupported patterns:
#'
#' - It is not possible to call `partial()` repeatedly on the same
#'   argument to pre-fill it with a different expression.
#'
#' - It is not possible to refer to other arguments in pre-filled
#'   argument.
#'
#' @param .f a function. For the output source to read well, this should be a
#'   named function.
#' @param ... named arguments to `.f` that should be partially applied.
#'
#'   Pass an empty `... = ` argument to specify the position of future
#'   arguments relative to partialised ones. See
#'   [rlang::call_modify()] to learn more about this syntax.
#'
#'   These dots support quasiquotation. If you unquote a value, it is
#'   evaluated only once at function creation time.  Otherwise, it is
#'   evaluated each time the function is called.
#' @inheritSection safely Adverbs
#' @inherit safely return
#' @family adverbs
#' @export
#' @examples
#' # Partial is designed to replace the use of anonymous functions for
#' # filling in function arguments. Instead of:
#' compact1 <- function(x) discard(x, is.null)
#'
#' # we can write:
#' compact2 <- partial(discard, .p = is.null)
#'
#' # partial() works fine with functions that do non-standard
#' # evaluation
#' my_long_variable <- 1:10
#' plot2 <- partial(plot, my_long_variable)
#' plot2()
#' plot2(runif(10), type = "l")
#'
#' # Note that you currently can't partialise arguments multiple times:
#' my_mean <- partial(mean, na.rm = TRUE)
#' my_mean <- partial(my_mean, na.rm = FALSE)
#' try(my_mean(1:10))
#'
#'
#' # The evaluation of arguments normally occurs "lazily". Concretely,
#' # this means that arguments are repeatedly evaluated across invocations:
#' f <- partial(runif, n = rpois(1, 5))
#' f
#' f()
#' f()
#'
#' # You can unquote an argument to fix it to a particular value.
#' # Unquoted arguments are evaluated only once when the function is created:
#' f <- partial(runif, n = !!rpois(1, 5))
#' f
#' f()
#' f()
#'
#'
#' # By default, partialised arguments are passed before new ones:
#' my_list <- partial(list, 1, 2)
#' my_list("foo")
#'
#' # Control the position of these arguments by passing an empty
#' # `... = ` argument:
#' my_list <- partial(list, 1, ... = , 2)
#' my_list("foo")
partial <- function(.f, ...) {
  args <- enquos(...)

  fn_expr <- enexpr(.f)
  .fn <- switch(
    typeof(.f),
    builtin = ,
    special = as_closure(.f),
    closure = .f,
    cli::cli_abort(
      "{.arg .f} must be a function, not {.obj_type_friendly { .f }}.",
      arg = ".f"
    )
  )

  env <- caller_env()
  heterogeneous_envs <- !every(args, quo_is_same_env, env)

  if (!heterogeneous_envs) {
    args <- map(args, quo_get_expr)
  }

  # Reuse function symbol if possible
  fn_sym <- if (is_symbol(fn_expr)) fn_expr else quote(.fn)

  # Pass on `...` from parent function. It should be last, this way if
  # `args` also contain a `...` argument, the position in `args`
  # prevails.
  call <- call_modify(call2(fn_sym), !!!args, ... = )

  if (heterogeneous_envs) {
    # Forward caller environment where S3 methods might be defined.
    # See design note below.
    call <- new_quosure(call, env)

    # Unwrap quosured arguments if possible
    call <- quo_invert(call)

    # Derive a mask where dots can be forwarded
    mask <- new_data_mask(env(!!fn_sym := .fn))

    fn <- function(...) {
      mask$... <- environment()$...
      eval_tidy(call, mask)
    }
  } else {
    body <- expr({
      !!fn_sym <- !!.fn
      !!call
    })
    fn <- new_function(pairlist2(... = ), body, env = env)
  }

  structure(
    fn,
    class = c("purrr_function_partial", "function"),
    body = call
  )
}

#' @export
print.purrr_function_partial <- function(x, ...) {
  cat("<partialised>\n")

  body(x) <- partialised_body(x)
  print(x, ...)
}

partialised_body <- function(x) attr(x, "body")

# For !!fn_sym <- !!.fn
utils::globalVariables("!<-")


# helpers -----------------------------------------------------------------

quo_invert <- function(call) {
  call <- duplicate(call, shallow = TRUE)

  if (is_quosure(call)) {
    rest <- quo_get_expr(call)
  } else {
    rest <- call
  }
  if (!is_call(rest)) {
    cli::cli_abort("Expected a call", .internal = TRUE)
  }

  first_quo <- NULL

  # Find first quosured argument. We unwrap constant quosures which
  # add no scoping information.
  while (!is_null(rest)) {
    elt <- node_car(rest)

    if (is_quosure(elt)) {
      if (quo_is_constant(elt)) {
        # Unwrap constant quosures
        node_poke_car(rest, quo_get_expr(elt))
      } else if (is_null(first_quo)) {
        # Record first quosured argument
        first_quo <- elt
        first_node <- rest
      }
    }

    rest <- node_cdr(rest)
  }

  if (is_null(first_quo)) {
    return(call)
  }

  # Take the wrapping quosure env as reference if there is one.
  # Otherwise, take the first quosure detected in arguments.
  if (is_quosure(call)) {
    env <- quo_get_env(call)
    call <- quo_get_expr(call)
  } else {
    env <- quo_get_env(first_quo)
  }

  rest <- first_node
  while (!is_null(rest)) {
    cur <- node_car(rest)

    if (is_quosure(cur) && is_reference(quo_get_env(cur), env)) {
      node_poke_car(rest, quo_get_expr(cur))
    }

    rest <- node_cdr(rest)
  }

  new_quosure(call, env)
}

quo_is_constant <- function(quo) {
  is_reference(quo_get_env(quo), empty_env())
}

quo_is_same_env <- function(x, env) {
  quo_env <- quo_get_env(x)
  is_reference(quo_env, env) || is_reference(quo_env, empty_env())
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/R/adverb-possibly.R ---
#' Wrap a function to return a value instead of an error
#'
#' Create a modified version of `.f` that return a default value (`otherwise`)
#' whenever an error occurs.
#'
#' @inheritParams safely
#' @inheritSection safely Adverbs
#' @inherit safely return
#' @family adverbs
#' @export
#' @examples
#' # To replace errors with a default value, use possibly().
#' list("a", 10, 100) |>
#'   map_dbl(possibly(log, NA_real_))
#'
#' # The default, NULL, will be discarded with `list_c()`
#' list("a", 10, 100) |>
#'   map(possibly(log)) |>
#'   list_c()
possibly <- function(.f, otherwise = NULL, quiet = TRUE) {
  .f <- as_mapper(.f)
  force(otherwise)
  check_bool(quiet)

  function(...) {
    tryCatch(.f(...), error = function(e) {
      if (!quiet) {
        message("Error: ", conditionMessage(e))
      }
      otherwise
    })
  }
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/R/adverb-quietly.R ---
#' Wrap a function to capture side-effects
#'
#' Create a modified version of `.f` that captures side-effects along with
#' the return value of the function and returns a list containing
#' the `result`, `output`, `messages` and `warnings`.
#'
#' @inheritParams safely
#' @inheritSection safely Adverbs
#' @inherit safely return
#' @family adverbs
#' @export
#' @examples
#' f <- function() {
#'   print("Hi!")
#'   message("Hello")
#'   warning("How are ya?")
#'   "Gidday"
#' }
#' f()
#'
#' f_quiet <- quietly(f)
#' str(f_quiet())
quietly <- function(.f) {
  .f <- as_mapper(.f)
  function(...) capture_output(.f(...))
}

capture_output <- function(code) {
  warnings <- character()
  wHandler <- function(w) {
    warnings <<- c(warnings, conditionMessage(w))
    invokeRestart("muffleWarning")
  }

  messages <- character()
  mHandler <- function(m) {
    messages <<- c(messages, conditionMessage(m))
    invokeRestart("muffleMessage")
  }

  temp <- file()
  sink(temp)
  on.exit({
    sink()
    close(temp)
  })

  result <- withCallingHandlers(
    code,
    warning = wHandler,
    message = mHandler
  )

  output <- paste0(readLines(temp, warn = FALSE), collapse = "\n")

  list(
    result = result,
    output = output,
    warnings = warnings,
    messages = messages
  )
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/R/adverb-safely.R ---
#' Wrap a function to capture errors
#'
#' Creates a modified version of `.f` that always succeeds. It returns a list
#' with components `result` and `error`. If the function succeeds, `result`
#' contains the returned value and `error` is `NULL`. If an error occurred,
#' `error` is an `error` object and `result` is either `NULL` or `otherwise`.
#'
#' # Adverbs
#' This function is called an adverb because it modifies the effect of a
#' function (a verb). If you'd like to include a function created an adverb
#' in a package, be sure to read [faq-adverbs-export].
#'
#' @param .f A function to modify, specified in one of the following ways:
#'   * A named function, e.g. `mean`.
#'   * An anonymous function, e.g. `\(x) x + 1` or `function(x) x + 1`.
#'   * A formula, e.g. `~ .x + 1`. No longer recommended.
#' @param otherwise Default value to use when an error occurs.
#' @param quiet Hide errors (`TRUE`, the default), or display them
#'   as they occur?
#' @returns A function that takes the same arguments as `.f`, but returns
#'   a different value, as described above.
#' @family adverbs
#' @export
#' @examples
#' safe_log <- safely(log)
#' safe_log(10)
#' safe_log("a")
#'
#' list("a", 10, 100) |>
#'   map(safe_log) |>
#'   transpose()
#'
#' # This is a bit easier to work with if you supply a default value
#' # of the same type and use the simplify argument to transpose():
#' safe_log <- safely(log, otherwise = NA_real_)
#' list("a", 10, 100) |>
#'   map(safe_log) |>
#'   transpose() |>
#'   simplify_all()
safely <- function(.f, otherwise = NULL, quiet = TRUE) {
  .f <- as_mapper(.f)
  force(otherwise)
  check_bool(quiet)

  function(...) capture_error(.f(...), otherwise, quiet)
}

capture_error <- function(code, otherwise = NULL, quiet = TRUE) {
  tryCatch(
    list(result = code, error = NULL),
    error = function(e) {
      if (!quiet) {
        message("Error: ", conditionMessage(e))
      }

      list(result = otherwise, error = e)
    }
  )
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/R/adverb-slowly.R ---
#' Wrap a function to wait between executions
#'
#' `slowly()` takes a function and modifies it to wait a given
#' amount of time between each call.
#'
#' @inheritParams insistently
#' @param rate A [rate][rate-helpers] object. Defaults to a constant delay.
#' @inheritSection safely Adverbs
#' @inherit safely return
#' @family adverbs
#' @export
#' @examples
#' # For these example, we first create a custom rate
#' # with a low waiting time between attempts:
#' rate <- rate_delay(0.1)
#'
#' # slowly() causes a function to sleep for a given time between calls:
#' slow_runif <- slowly(\(x) runif(1), rate = rate, quiet = FALSE)
#' out <- map(1:5, slow_runif)
slowly <- function(f, rate = rate_delay(), quiet = TRUE) {
  f <- as_mapper(f)
  check_rate(rate)
  check_bool(quiet)

  function(...) {
    rate_sleep(rate, quiet = quiet)
    f(...)
  }
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/R/arrays.R ---
#' Coerce array to list
#'
#' `array_branch()` and `array_tree()` enable arrays to be
#' used with purrr's functionals by turning them into lists. The
#' details of the coercion are controlled by the `margin`
#' argument. `array_tree()` creates an hierarchical list (a tree)
#' that has as many levels as dimensions specified in `margin`,
#' while `array_branch()` creates a flat list (by analogy, a
#' branch) along all mentioned dimensions.
#'
#' When no margin is specified, all dimensions are used by
#' default. When `margin` is a numeric vector of length zero, the
#' whole array is wrapped in a list.
#' @param array An array to coerce into a list.
#' @param margin A numeric vector indicating the positions of the
#'   indices to be to be enlisted. If `NULL`, a full margin is
#'   used. If `numeric(0)`, the array as a whole is wrapped in a
#'   list.
#' @name array-coercion
#' @export
#' @examples
#' # We create an array with 3 dimensions
#' x <- array(1:12, c(2, 2, 3))
#'
#' # A full margin for such an array would be the vector 1:3. This is
#' # the default if you don't specify a margin
#'
#' # Creating a branch along the full margin is equivalent to
#' # as.list(array) and produces a list of size length(x):
#' array_branch(x) |> str()
#'
#' # A branch along the first dimension yields a list of length 2
#' # with each element containing a 2x3 array:
#' array_branch(x, 1) |> str()
#'
#' # A branch along the first and third dimensions yields a list of
#' # length 2x3 whose elements contain a vector of length 2:
#' array_branch(x, c(1, 3)) |> str()
#'
#' # Creating a tree from the full margin creates a list of lists of
#' # lists:
#' array_tree(x) |> str()
#'
#' # The ordering and the depth of the tree are controlled by the
#' # margin argument:
#' array_tree(x, c(3, 1)) |> str()
array_branch <- function(array, margin = NULL) {
  dims <- dim(array) %||% length(array)
  margin <- margin %||% seq_along(dims)

  if (length(margin) == 0) {
    list(array)
  } else if (is.null(dim(array))) {
    if (!identical(as.integer(margin), 1L)) {
      cli::cli_abort(
        "{.arg margin} must be `NULL` or `1` with 1D arrays, not {.str {margin}}.",
        arg = "margin"
      )
    }
    as.list(array)
  } else {
    list_flatten(apply(array, margin, list))
  }
}

#' @rdname array-coercion
#' @export
array_tree <- function(array, margin = NULL) {
  dims <- dim(array) %||% length(array)
  margin <- margin %||% seq_along(dims)

  if (length(margin) > 1) {
    new_margin <- ifelse(margin[-1] > margin[[1]], margin[-1] - 1, margin[-1])
    apply(array, margin[[1]], array_tree, new_margin)
  } else {
    array_branch(array, margin)
  }
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/R/cleancall.R ---
call_with_cleanup <- function(ptr, ...) {
  .Call(cleancall_call, pairlist(ptr, ...), parent.frame())
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/R/coerce.R ---
# Used internally by map and flatten.
# Exposed here for testing
coerce <- function(x, type) {
  .Call(coerce_impl, x, type)
}

coerce_lgl <- function(x) coerce(x, "logical")
coerce_int <- function(x) coerce(x, "integer")
coerce_dbl <- function(x) coerce(x, "double")
coerce_chr <- function(x) coerce(x, "character")

# Can rewrite after https://github.com/r-lib/rlang/issues/1643
local_deprecation_user_env <- function(
  user_env = caller_env(2),
  frame = caller_env()
) {
  old <- the$deprecation_user_env
  the$deprecation_user_env <- user_env
  defer(the$deprecation_user_env <- old, frame)
}

# Lightweight equivalent of withr::defer()
defer <- function(expr, env = caller_env(), after = FALSE) {
  thunk <- as.call(list(function() expr))
  do.call(on.exit, list(thunk, TRUE, after), envir = env)
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/R/compat-obj-type.R ---
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


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/R/compat-types-check.R ---
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
  if (!is.numeric(x)) {
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


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/R/conditions.R ---
#' Error conditions for bad types
#'
#' @param x The object whose type doesn't match `expected`.
#' @param what What does `x` represent? This is used to introduce the
#'   object in the error message and should be capitalised. If `NULL`
#'   and `arg` is `NULL` as well, defaults to `"Object"`. Otherwise
#'   defaults to `arg` wrapped in backquotes.
#' @param expected,actual The expected and actual type of `x`, in
#'   friendly representation. If `actual` is not supplied, `x` is
#'   passed to `friendly_type_of()` to provide a default value.
#' @param index The index of `x` when it is an element of a vector.
#' @param ...,message,.subclass Only use these fields when creating a subclass.
#'
#' @details
#'
#' Some of the fields are expected to be in friendly representation,
#' i.e. a longer description that includes indefinite articles. For
#' example, a friendly representation of `"integer"` would be
#' `"an integer vector"`.
#'
#' Fields in pretty representation are meant for printing, not for
#' testing. They should not be relied on in unit tests as upstream
#' packages might tweak the friendly representation at any time.
#'
#' @keywords internal
#' @name purrr-conditions-type
#' @noRd
NULL

stop_bad_type <- function(
  x,
  expected,
  ...,
  what = NULL,
  arg = NULL,
  call = caller_env()
) {
  what <- what %||% what_bad_object(arg)
  cli::cli_abort(
    "{what} must be {expected}, not {.obj_type_friendly {x}}.",
    arg = arg,
    call = call
  )
}

stop_bad_element_type <- function(
  x,
  index,
  expected,
  ...,
  what = NULL,
  arg = NULL,
  call = caller_env()
) {
  what <- what_bad_element(what, arg, index)
  cli::cli_abort(
    "{what} must be {expected}, not {.obj_type_friendly {x}}.",
    arg = arg,
    call = call
  )
}

stop_bad_element_length <- function(
  x,
  index,
  expected_length,
  ...,
  what = NULL,
  arg = NULL,
  recycle = FALSE,
  call = caller_env()
) {
  what <- what_bad_element(what, arg, index)

  if (recycle) {
    expected <- sprintf("1 or %s", expected_length)
  } else {
    expected <- expected_length
  }

  cli::cli_abort(
    "{what} must have length {expected}, not {length(x)}.",
    arg = arg,
    call = call
  )
}

# Helpers -----------------------------------------------------------------

what_bad_object <- function(arg) {
  if (is_null(arg)) {
    "Object"
  } else if (is_string(arg)) {
    sprintf("`%s`", arg)
  } else {
    stop_bad_type(arg, "`NULL` or a string", arg = "arg")
  }
}

what_bad_element <- function(what, arg, index) {
  stopifnot(is_integerish(index, n = 1, finite = TRUE))

  if (is_null(arg)) {
    what <- what %||% "Element"
    sprintf("%s %d", what, index)
  } else {
    sprintf("`%s[[%d]]`", arg, index)
  }
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/R/deprec-along.R ---
#' Create a list of given length
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function was deprecated in purrr 1.0.0 since it's not related to the
#' core purpose of purrr.
#'
#' It can be useful to create an empty list that you plan to fill later. This is
#' similar to the idea of [seq_along()], which creates a vector of the same
#' length as its input.
#'
#' @param x A vector.
#' @return A list of the same length as `x`.
#' @keywords internal
#' @examples
#' x <- 1:5
#' seq_along(x)
#' list_along(x)
#' @name along
#' @rdname along
#' @export
list_along <- function(x) {
  lifecycle::deprecate_soft("1.0.0", "list_along()", I("rep_along(x, list())"))

  vector("list", length(x))
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/R/deprec-cross.R ---
#' Produce all combinations of list elements
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' These functions were deprecated in purrr 1.0.0 because they
#' are slow and buggy, and we no longer think they are the right
#' approach to solving this problem. Please use `tidyr::expand_grid()`
#' instead.
#'
#' Here is an example of equivalent usages for `cross()` and
#' `expand_grid()`:
#'
#' ```R
#' data <- list(
#'   id = c("John", "Jane"),
#'   sep = c("! ", "... "),
#'   greeting = c("Hello.", "Bonjour.")
#' )
#'
#' # With deprecated `cross()`
#' data |> cross() |> map_chr(\(...) paste0(..., collapse = ""))
#'
#' # With `expand_grid()`
#' tidyr::expand_grid(!!!data) |> pmap_chr(paste)
#' ```
#'
#' @details
#' `cross2()` returns the product set of the elements of
#' `.x` and `.y`. `cross3()` takes an additional
#' `.z` argument. `cross()` takes a list `.l` and
#' returns the cartesian product of all its elements in a list, with
#' one combination by element. `cross_df()` is like
#' `cross()` but returns a data frame, with one combination by
#' row.
#'
#' `cross()`, `cross2()` and `cross3()` return the
#' cartesian product is returned in wide format. This makes it more
#' amenable to mapping operations. `cross_df()` returns the output
#' in long format just as `expand.grid()` does. This is adapted
#' to rowwise operations.
#'
#' When the number of combinations is large and the individual
#' elements are heavy memory-wise, it is often useful to filter
#' unwanted combinations on the fly with `.filter`. It must be
#' a predicate function that takes the same number of arguments as the
#' number of crossed objects (2 for `cross2()`, 3 for
#' `cross3()`, `length(.l)` for `cross()`) and
#' returns `TRUE` or `FALSE`. The combinations where the
#' predicate function returns `TRUE` will be removed from the
#' result.
#' @seealso [expand.grid()]
#' @param .x,.y,.z Lists or atomic vectors.
#' @param .l A list of lists or atomic vectors. Alternatively, a data
#'   frame. `cross_df()` requires all elements to be named.
#' @param .filter A predicate function that takes the same number of
#'   arguments as the number of variables to be combined.
#' @return `cross2()`, `cross3()` and `cross()`
#'   always return a list. `cross_df()` always returns a data
#'   frame. `cross()` returns a list where each element is one
#'   combination so that the list can be directly mapped
#'   over. `cross_df()` returns a data frame where each row is one
#'   combination.
#' @keywords internal
#' @export
#' @examples
#' # We build all combinations of names, greetings and separators from our
#' # list of data and pass each one to paste()
#' data <- list(
#'   id = c("John", "Jane"),
#'   greeting = c("Hello.", "Bonjour."),
#'   sep = c("! ", "... ")
#' )
#'
#' data |>
#'   cross() |>
#'   map(lift(paste))
#'
#' # cross() returns the combinations in long format: many elements,
#' # each representing one combination. With cross_df() we'll get a
#' # data frame in long format: crossing three objects produces a data
#' # frame of three columns with each row being a particular
#' # combination. This is the same format that expand.grid() returns.
#' args <- data |> cross_df()
#'
#' # In case you need a list in long format (and not a data frame)
#' # just run as.list() after cross_df()
#' args |> as.list()
#'
#' # This format is often less practical for functional programming
#' # because applying a function to the combinations requires a loop
#' out <- vector("character", length = nrow(args))
#' for (i in seq_along(out))
#'   out[[i]] <- invoke("paste", map(args, i))
#' out
#'
#' # It's easier to transpose and then use invoke_map()
#' args |> transpose() |> map_chr(\(x) exec(paste, !!!x))
#'
#' # Unwanted combinations can be filtered out with a predicate function
#' filter <- function(x, y) x >= y
#' cross2(1:5, 1:5, .filter = filter) |> str()
#'
#' # To give names to the components of the combinations, we map
#' # setNames() on the product:
#' x <- seq_len(3)
#' cross2(x, x, .filter = `==`) |>
#'   map(setNames, c("x", "y"))
#'
#' # Alternatively we can encapsulate the arguments in a named list
#' # before crossing to get named components:
#' list(x = x, y = x) |>
#'   cross(.filter = `==`)
cross <- function(.l, .filter = NULL) {
  lifecycle::deprecate_warn(
    "1.0.0",
    "purrr::cross()",
    "tidyr::expand_grid()",
    details = c(i = "See <https://github.com/tidyverse/purrr/issues/768>.")
  )

  if (is_empty(.l)) {
    return(.l)
  }

  if (!is.null(.filter)) {
    .filter <- as_mapper(.filter)
  }

  n <- length(.l)
  lengths <- lapply(.l, length)
  names <- names(.l)

  factors <- cumprod(lengths)
  total_length <- factors[n]
  factors <- c(1, factors[-n])

  out <- replicate(total_length, vector("list", n), simplify = FALSE)

  for (i in seq_along(out)) {
    for (j in seq_len(n)) {
      index <- floor((i - 1) / factors[j]) %% length(.l[[j]]) + 1
      out[[i]][[j]] <- .l[[j]][[index]]
    }
    names(out[[i]]) <- names

    # Filter out unwanted elements. We set them to NULL instead of
    # completely removing them so we don't mess up the loop indexing.
    # NULL elements are removed later on.
    if (!is.null(.filter)) {
      is_to_filter <- do.call(".filter", unname(out[[i]]))
      if (!is_bool(is_to_filter)) {
        cli::cli_abort(
          "The filter function must return a single `TRUE` or `FALSE`, not {.obj_type_friendly {is_to_filter}}."
        )
      }
      if (is_to_filter) {
        out[i] <- list(NULL)
      }
    }
  }

  # Remove filtered elements
  compact(out)
}

#' @export
#' @rdname cross
cross2 <- function(.x, .y, .filter = NULL) {
  lifecycle::deprecate_warn(
    "1.0.0",
    "purrr::cross2()",
    "tidyr::expand_grid()",
    details = c(i = "See <https://github.com/tidyverse/purrr/issues/768>.")
  )
  cross(list(.x, .y), .filter = .filter)
}

#' @export
#' @rdname cross
cross3 <- function(.x, .y, .z, .filter = NULL) {
  lifecycle::deprecate_warn(
    "1.0.0",
    "purrr::cross3()",
    "tidyr::expand_grid()",
    details = c(i = "See <https://github.com/tidyverse/purrr/issues/768>.")
  )
  cross(list(.x, .y, .z), .filter = .filter)
}

#' @rdname cross
#' @export
cross_df <- function(.l, .filter = NULL) {
  lifecycle::deprecate_warn(
    "1.0.0",
    "purrr::cross_df()",
    "tidyr::expand_grid()",
    details = c(i = "See <https://github.com/tidyverse/purrr/issues/768>.")
  )
  check_installed("tibble")
  cross(.l, .filter = .filter) |>
    transpose() |>
    simplify_all() |>
    tibble::as_tibble()
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/R/deprec-invoke.R ---
#' Invoke functions.
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' These functions were superded in purrr 0.3.0 and deprecated in purrr 1.0.0.
#'
#' * `invoke()` is deprecated in favour of the simpler `exec()` function
#'   reexported from rlang. `exec()` evaluates a function call built
#'   from its inputs and supports [dynamic dots][rlang::dyn-dots]:
#'
#'   ```R
#'   # Before:
#'   invoke(mean, list(na.rm = TRUE), x = 1:10)
#'
#'   # After
#'   exec(mean, 1:10, !!!list(na.rm = TRUE))
#'   ```
#'
#' * `invoke_map()` is deprecated because it's harder to understand than the
#'   corresponding code using `map()`/`map2()` and `exec()`:
#'
#'   ```R
#'   # Before:
#'   invoke_map(fns, list(args))
#'   invoke_map(fns, list(args1, args2))
#'
#'   # After:
#'   map(fns, exec, !!!args)
#'   map2(fns, list(args1, args2), \(fn, args) exec(fn, !!!args))
#'   ```
#' @param .f For `invoke`, a function; for `invoke_map` a
#'   list of functions.
#' @param .x For `invoke`, an argument-list; for `invoke_map` a
#'   list of argument-lists the same length as `.f` (or length 1).
#'   The default argument, `list(NULL)`, will be recycled to the
#'   same length as `.f`, and will call each function with no
#'   arguments (apart from any supplied in `...`.
#' @param ... Additional arguments passed to each function.
#' @param .env Environment in which [do.call()] should
#'   evaluate a constructed expression. This only matters if you pass
#'   as `.f` the name of a function rather than its value, or as
#'   `.x` symbols of objects rather than their values.
#' @keywords internal
#' @examples
#' # was
#' invoke(runif, list(n = 10))
#' invoke(runif, n = 10)
#' # now
#' exec(runif, n = 10)
#'
#' # was
#' args <- list("01a", "01b")
#' invoke(paste, args, sep = "-")
#' # now
#' exec(paste, !!!args, sep = "-")
#'
#' # was
#' funs <- list(runif, rnorm)
#' funs |> invoke_map(n = 5)
#' funs |> invoke_map(list(list(n = 10), list(n = 5)))
#'
#' # now
#' funs |> map(exec, n = 5)
#' funs |> map2(list(list(n = 10), list(n = 5)), function(f, args) exec(f, !!!args))
#'
#' # or use pmap + a tibble
#' df <- tibble::tibble(
#'   fun = list(runif, rnorm),
#'   args = list(list(n = 10), list(n = 5))
#' )
#' df |> pmap(function(fun, args) exec(fun, !!!args))
#'
#'
#' # was
#' list(m1 = mean, m2 = median) |> invoke_map(x = rcauchy(100))
#' # now
#' list(m1 = mean, m2 = median) |> map(function(f) f(rcauchy(100)))
#'
#' @export
invoke <- function(.f, .x = NULL, ..., .env = NULL) {
  lifecycle::deprecate_warn("1.0.0", "invoke()", "exec()")

  .env <- .env %||% parent.frame()
  args <- c(as.list(.x), list(...))
  do.call(.f, args, envir = .env)
}

as_invoke_function <- function(f) {
  if (is.function(f)) {
    list(f)
  } else {
    f
  }
}

#' @rdname invoke
#' @export
invoke_map <- function(.f, .x = list(NULL), ..., .env = NULL) {
  lifecycle::deprecate_warn("1.0.0", "invoke_map()", I("map() + exec()"))

  .env <- .env %||% parent.frame()
  .f <- as_invoke_function(.f)
  map2(.f, .x, invoke, ..., .env = .env)
}
#' @rdname invoke
#' @export
invoke_map_lgl <- function(.f, .x = list(NULL), ..., .env = NULL) {
  lifecycle::deprecate_warn("1.0.0", "invoke_lgl()", I("map_lgl() + exec()"))

  .env <- .env %||% parent.frame()
  .f <- as_invoke_function(.f)
  map2_lgl(.f, .x, invoke, ..., .env = .env)
}
#' @rdname invoke
#' @export
invoke_map_int <- function(.f, .x = list(NULL), ..., .env = NULL) {
  lifecycle::deprecate_warn("1.0.0", "invoke_int()", I("map_int() + exec()"))

  .env <- .env %||% parent.frame()
  .f <- as_invoke_function(.f)
  map2_int(.f, .x, invoke, ..., .env = .env)
}
#' @rdname invoke
#' @export
invoke_map_dbl <- function(.f, .x = list(NULL), ..., .env = NULL) {
  lifecycle::deprecate_warn("1.0.0", "invoke_dbl()", I("map_dbl() + exec()"))

  .env <- .env %||% parent.frame()
  .f <- as_invoke_function(.f)
  map2_dbl(.f, .x, invoke, ..., .env = .env)
}
#' @rdname invoke
#' @export
invoke_map_chr <- function(.f, .x = list(NULL), ..., .env = NULL) {
  lifecycle::deprecate_warn("1.0.0", "invoke_chr()", I("map_chr() + exec()"))

  .env <- .env %||% parent.frame()
  .f <- as_invoke_function(.f)
  map2_chr(.f, .x, invoke, ..., .env = .env)
}
#' @rdname invoke
#' @export
invoke_map_raw <- function(.f, .x = list(NULL), ..., .env = NULL) {
  lifecycle::deprecate_warn("1.0.0", "invoke_raw()", I("map_raw() + exec()"))

  .env <- .env %||% parent.frame()
  .f <- as_invoke_function(.f)

  map2_("raw", .f, .x, invoke, ...)
}

#' @rdname invoke
#' @export
invoke_map_dfr <- function(.f, .x = list(NULL), ..., .env = NULL) {
  lifecycle::deprecate_warn(
    "1.0.0",
    "invoke_df()",
    I("map() + exec() + list_rbind()")
  )

  .env <- .env %||% parent.frame()
  .f <- as_invoke_function(.f)
  map2_dfr(.f, .x, invoke, ..., .env = .env)
}
#' @rdname invoke
#' @export
invoke_map_dfc <- function(.f, .x = list(NULL), ..., .env = NULL) {
  lifecycle::deprecate_soft(
    "1.0.0",
    "invoke_dfc()",
    I("map() + exec() + list_cbind()")
  )

  .env <- .env %||% parent.frame()
  .f <- as_invoke_function(.f)
  map2_dfc(.f, .x, invoke, ..., .env = .env)
}
#' @rdname invoke
#' @export
#' @usage NULL
invoke_map_df <- function(.f, .x = list(NULL), ..., .env = NULL) {
  lifecycle::deprecate_soft(
    "1.0.0",
    "invoke_df()",
    I("map() + exec() + list_rbind()")
  )

  .env <- .env %||% parent.frame()
  .f <- as_invoke_function(.f)
  map2_dfr(.f, .x, invoke, ..., .env = .env)
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/R/deprec-lift.R ---
#' Lift the domain of a function
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `lift_xy()` is a composition helper. It helps you compose
#' functions by lifting their domain from a kind of input to another
#' kind. The domain can be changed from and to a list (l), a vector
#' (v) and dots (d). For example, `lift_ld(fun)` transforms a
#' function taking a list to a function taking dots.
#'
#' The most important of those helpers is probably `lift_dl()`
#' because it allows you to transform a regular function to one that
#' takes a list. This is often essential for composition with purrr
#' functional tools. Since this is such a common function,
#' `lift()` is provided as an alias for that operation.
#'
#' These functions were superseded in purrr 1.0.0 because we no longer believe
#' "lifting" to be a mainstream operation, and we are striving to reduce purrr
#' to its most useful core. Superseded functions will not go away, but will only
#' receive critical bug fixes.
#'
#' @inheritParams as_vector
#' @param ..f A function to lift.
#' @param ... Default arguments for `..f`. These will be
#'   evaluated only once, when the lifting factory is called.
#' @return A function.
#' @name lift
#' @seealso [invoke()]
NULL

#' @rdname lift
#' @section from ... to `list(...)` or `c(...)`:
#'   Here dots should be taken here in a figurative way. The lifted
#'   functions does not need to take dots per se. The function is
#'   simply wrapped a function in [do.call()], so instead
#'   of taking multiple arguments, it takes a single named list or
#'   vector which will be interpreted as its arguments.  This is
#'   particularly useful when you want to pass a row of a data frame
#'   or a list to a function and don't want to manually pull it apart
#'   in your function.
#' @param .unnamed If `TRUE`, `ld` or `lv` will not
#'   name the parameters in the lifted function signature. This
#'   prevents matching of arguments by name and match by position
#'   instead.
#' @keywords internal
#' @export
#' @examples
#' ### Lifting from ... to list(...) or c(...)
#'
#' x <- list(x = c(1:100, NA, 1000), na.rm = TRUE, trim = 0.9)
#' lift_dl(mean)(x)
#' # You can also use the lift() alias for this common operation:
#' lift(mean)(x)
#' # now:
#' exec(mean, !!!x)
#'
#' # Default arguments can also be specified directly in lift_dl()
#' list(c(1:100, NA, 1000)) |> lift_dl(mean, na.rm = TRUE)()
#' # now:
#' mean(c(1:100, NA, 1000), na.rm = TRUE)
#'
#' # lift_dl() and lift_ld() are inverse of each other.
#' # Here we transform sum() so that it takes a list
#' fun <- sum |> lift_dl()
#' fun(list(3, NA, 4, na.rm = TRUE))
#' # now:
#' fun <- function(x) exec("sum", !!!x)
#' exec(sum, 3, NA, 4, na.rm = TRUE)
lift <- function(..f, ..., .unnamed = FALSE) {
  lifecycle::deprecate_warn("1.0.0", "lift()")

  force(..f)
  defaults <- list(...)
  function(.x = list(), ...) {
    if (.unnamed) {
      .x <- unname(.x)
    }
    do.call("..f", c(.x, defaults, list(...)))
  }
}

#' @rdname lift
#' @export
lift_dl <- lift

#' @rdname lift
#' @export
lift_dv <- function(..f, ..., .unnamed = FALSE) {
  lifecycle::deprecate_warn("1.0.0", "lift_dv()")

  force(..f)
  defaults <- list(...)

  function(.x, ...) {
    if (.unnamed) {
      .x <- unname(.x)
    }
    .x <- as.list(.x)
    do.call("..f", c(.x, defaults, list(...)))
  }
}

#' @rdname lift
#' @section from `c(...)` to `list(...)` or `...`:
#'   These factories allow a function taking a vector to take a list
#'   or dots instead. The lifted function internally transforms its
#'   inputs back to an atomic vector. purrr does not obey the usual R
#'   casting rules (e.g., `c(1, "2")` produces a character
#'   vector) and will produce an error if the types are not
#'   compatible. Additionally, you can enforce a particular vector
#'   type by supplying `.type`.
#' @export
#' @examples
#' ### Lifting from c(...) to list(...) or ...
#'
#' # In other situations we need the vector-valued function to take a
#' # variable number of arguments as with pmap(). This is a job for
#' # lift_vd():
#' pmap_dbl(mtcars, lift_vd(mean))
#' # now
#' pmap_dbl(mtcars, \(...) mean(c(...)))
lift_vl <- function(..f, ..., .type) {
  lifecycle::deprecate_warn("1.0.0", "lift_vl()")

  force(..f)
  defaults <- list(...)
  if (missing(.type)) {
    .type <- NULL
  }

  function(.x = list(), ...) {
    x <- as_vector_(.x, .type)
    do.call("..f", c(list(x), defaults, list(...)))
  }
}

#' @rdname lift
#' @export
lift_vd <- function(..f, ..., .type) {
  lifecycle::deprecate_warn("1.0.0", "lift_vd()")

  force(..f)
  defaults <- list(...)
  if (missing(.type)) {
    .type <- NULL
  }

  function(...) {
    x <- as_vector_(list(...), .type)
    do.call("..f", c(list(x), defaults))
  }
}

#' @rdname lift
#' @section from list(...) to c(...) or ...:
#' `lift_ld()` turns a function that takes a list into a
#' function that takes dots. `lift_vd()` does the same with a
#' function that takes an atomic vector. These factory functions are
#' the inverse operations of `lift_dl()` and `lift_dv()`.
#'
#' `lift_vd()` internally coerces the inputs of `..f` to
#' an atomic vector. The details of this coercion can be controlled
#' with `.type`.
#'
#' @export
#' @examples
#' ### Lifting from list(...) to c(...) or ...
#'
#' # This kind of lifting is sometimes needed for function
#' # composition. An example would be to use pmap() with a function
#' # that takes a list. In the following, we use some() on each row of
#' # a data frame to check they each contain at least one element
#' # satisfying a condition:
#' mtcars |> pmap_lgl(lift_ld(some, partial(`<`, 200)))
#' # now
#' mtcars |> pmap_lgl(\(...) any(c(...) > 200))
#'
lift_ld <- function(..f, ...) {
  lifecycle::deprecate_warn("1.0.0", "lift_ld()")

  force(..f)
  defaults <- list(...)
  function(...) {
    do.call("..f", c(list(list(...)), defaults))
  }
}

#' @rdname lift
#' @export
lift_lv <- function(..f, ...) {
  lifecycle::deprecate_warn("1.0.0", "lift_lv()")

  force(..f)
  defaults <- list(...)
  function(.x, ...) {
    do.call("..f", c(list(as.list(.x)), defaults, list(...)))
  }
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/R/deprec-prepend.R ---
#' Prepend a vector
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function was deprecated in purrr 1.0.0 because it's not related to the
#' core purpose of purrr.
#'
#' This is a companion to [append()] to help merging two
#' lists or atomic vectors. `prepend()` is a clearer semantic
#' signal than `c()` that a vector is to be merged at the beginning of
#' another, especially in a pipe chain.
#'
#' @param x the vector to be modified.
#' @param values to be included in the modified vector.
#' @param before a subscript, before which the values are to be appended. If
#'   `NULL`, values will be appended at the beginning even for `x` of length 0.
#' @return A merged vector.
#' @keywords internal
#' @export
#' @examples
#' x <- as.list(1:3)
#'
#' x |> append("a")
#' x |> prepend("a")
#' x |> prepend(list("a", "b"), before = 3)
#' prepend(list(), x)
prepend <- function(x, values, before = NULL) {
  lifecycle::deprecate_warn("1.0.0", "prepend()", I("append(after = 0)"))

  n <- length(x)
  stopifnot(is.null(before) || (before > 0 && before <= n))

  if (is.null(before) || before == 1) {
    c(values, x)
  } else {
    c(x[1:(before - 1)], values, x[before:n])
  }
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/R/deprec-rerun.R ---
#' Re-run expressions multiple times
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function was deprecated in purrr 1.0.0 because we believe that NSE
#' functions are not a  good fit for purrr. Also, `rerun(n, x)` can just as
#' easily be expressed as `map(1:n, \(i) x)`
#'
#' `rerun()` is a convenient way of generating sample data. It works similarly to
#' \code{\link{replicate}(..., simplify = FALSE)}.
#'
#' @param .n Number of times to run expressions
#' @param ... Expressions to re-run.
#' @return A list of length `.n`. Each element of `...` will be
#'   re-run once for each `.n`.
#'
#'   There is one special case: if there's a single unnamed input, the second
#'   level list will be dropped. In this case, `rerun(n, x)` behaves like
#'   `replicate(n, x, simplify = FALSE)`.
#' @export
#' @keywords internal
#' @examples
#' # old
#' 5 |> rerun(rnorm(5)) |> str()
#' # new
#' 1:5 |> map(\(i) rnorm(5)) |> str()
#'
#' # old
#' 5 |>
#'   rerun(x = rnorm(5), y = rnorm(5)) |>
#'   map_dbl(\(l) cor(l$x, l$y))
#' # new
#' 1:5 |>
#'   map(\(i) list(x = rnorm(5), y = rnorm(5))) |>
#'   map_dbl(\(l) cor(l$x, l$y))
rerun <- function(.n, ...) {
  deprec_rerun(.n, ..., .purrr_user_env = caller_env())

  dots <- quos(...)

  # Special case: if single unnamed argument, insert directly into the output
  # rather than wrapping in a list.
  if (length(dots) == 1 && !is_named(dots)) {
    dots <- dots[[1]]
    eval_dots <- eval_tidy
  } else {
    eval_dots <- function(x) lapply(x, eval_tidy)
  }

  out <- vector("list", .n)
  for (i in seq_len(.n)) {
    out[[i]] <- eval_dots(dots)
  }
  out
}

deprec_rerun <- function(.n, ..., .purrr_user_env) {
  n <- .n
  old <- substitute(rerun(n, ...))
  if (dots_n(...) == 1) {
    new <- substitute(map(1:n, ~...))
  } else {
    new <- substitute(map(1:n, ~ list(...)))
  }

  lifecycle::deprecate_warn(
    when = "1.0.0",
    what = "rerun()",
    with = "map()",
    details = c(
      " " = "# Previously",
      " " = expr_deparse(old),
      "",
      " " = "# Now",
      " " = expr_deparse(new)
    ),
    user_env = .purrr_user_env
  )
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/R/deprec-splice.R ---
#' Splice objects and lists of objects into a list
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function was deprecated in purrr 1.0.0 because we no longer believe that
#' this style of implicit/automatic splicing is a good idea; instead use
#' `rlang::list2()` + `!!!` or [list_flatten()].
#'
#' `splice()` splices all arguments into a list. Non-list objects and lists
#' with a S3 class are encapsulated in a list before concatenation.
#'
#' @param ... Objects to concatenate.
#' @return A list.
#' @keywords internal
#' @examples
#' inputs <- list(arg1 = "a", arg2 = "b")
#'
#' # splice() concatenates the elements of inputs with arg3
#' splice(inputs, arg3 = c("c1", "c2")) |> str()
#' list(inputs, arg3 = c("c1", "c2")) |> str()
#' c(inputs, arg3 = c("c1", "c2")) |> str()
#' @export
splice <- function(...) {
  lifecycle::deprecate_warn("1.0.0", "splice()", "list_flatten()")

  splice_if(list(...), is_bare_list)
}

splice_if <- function(.x, .p) {
  unspliced <- !where_if(.x, .p)
  out <- modify_if(.x, unspliced, list)
  list_flatten(out, name_spec = "{inner}")
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/R/deprec-utils.R ---
#' Generate random sample from a Bernoulli distribution
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function was deprecated in purrr 1.0.0 because it's not related to the
#' core purpose of purrr.
#'
#' @param n Number of samples
#' @param p Probability of getting `TRUE`
#' @return A logical vector
#' @keywords internal
#' @export
#' @examples
#' rbernoulli(10)
#' rbernoulli(100, 0.1)
rbernoulli <- function(n, p = 0.5) {
  lifecycle::deprecate_warn("1.0.0", "rbernoulli()")
  stats::runif(n) > (1 - p)
}

#' Generate random sample from a discrete uniform distribution
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function was deprecated in purrr 1.0.0 because it's not related to the
#' core purpose of purrr.
#'
#' @param n Number of samples to draw.
#' @param a,b Range of the distribution (inclusive).
#' @keywords internal
#' @export
#' @examples
#' table(rdunif(1e3, 10))
#' table(rdunif(1e3, 10, -5))
rdunif <- function(n, b, a = 1) {
  lifecycle::deprecate_warn("1.0.0", "rdunif()")

  stopifnot(is.numeric(a), length(a) == 1)
  stopifnot(is.numeric(b), length(b) == 1)

  a1 <- min(a, b)
  b1 <- max(a, b)

  sample(b1 - a1 + 1, n, replace = TRUE) + a1 - 1
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/R/deprec-when.R ---
#' Match/validate a set of conditions for an object and continue with the action
#' associated with the first valid match.
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function was deprecated in purrr 1.0.0 because it's not related to the
#' core purpose of purrr. You can pull your code out of a pipe and use regular
#' `if`/`else` statements instead.
#'
#' `when()` is a flavour of pattern matching (or an if-else abstraction) in
#' which a value is matched against a sequence of condition-action sets. When a
#' valid match/condition is found the action is executed and the result of the
#' action is returned.
#'
#' @param .   the value to match against
#' @param ... formulas; each containing a condition as LHS and an action as RHS.
#'   named arguments will define additional values.
#' @return The value resulting from the action of the first valid
#'   match/condition is returned. If no matches are found, and no default is
#'   given, NULL will be returned.
#'
# @details condition-action sets are written as formulas with conditions as
#   left-hand sides and actions as right-hand sides. A formula with only a
#   right-hand will be treated as a condition which is always satisfied. For
#   such a default case one can also omit the `~` symbol, but note that its
#   value will then be evaluated. Any named argument will be made available in
#   all conditions and actions, which is useful in avoiding repeated temporary
#   computations or temporary assignments.
#
#' Validity of the conditions are tested with `isTRUE`, or equivalently
#' with `identical(condition, TRUE)`.
#' In other words conditions resulting in more than one logical will never
#' be valid. Note that the input value is always treated as a single object,
#' as opposed to the `ifelse` function.
#'
#' @keywords internal
#' @examples
#' 1:10 |>
#'   when(
#'     sum(.) <=  50 ~ sum(.),
#'     sum(.) <= 100 ~ sum(.)/2,
#'     ~ 0
#'   )
#'
#' # now
#' x <- 1:10
#' if (sum(x) < 10) {
#'   sum(x)
#' } else if (sum(x) < 100) {
#'   sum(x) / 2
#' } else {
#'   0
#' }
#' @export
when <- function(., ...) {
  lifecycle::deprecate_warn("1.0.0", "when()", I("`if`"))

  dots <- list(...)
  names <- names(dots)
  named <- if (is.null(names)) rep(FALSE, length(dots)) else names != ""

  if (sum(!named) == 0) {
    cli::cli_abort("At least one matching condition is needed.")
  }

  is_formula <-
    vapply(dots, function(dot) identical(class(dot), "formula"), logical(1L))

  env <- new.env(parent = parent.frame())
  env[["."]] <- .

  if (sum(named) > 0) {
    for (i in which(named)) {
      env[[names[i]]] <- dots[[i]]
    }
  }

  result <- NULL
  for (i in which(!named)) {
    if (is_formula[i]) {
      action <- length(dots[[i]])
      if (action == 2 || is_true(eval(dots[[i]][[2]], env, env))) {
        result <- eval(dots[[i]][[action]], env, env)
        break
      }
    } else {
      result <- dots[[i]]
    }
  }

  result
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/R/detect.R ---
#' Find the value or position of the first match
#'
#' @inheritParams keep
#' @inheritParams map
#' @param .f A function, specified in one of the following ways:
#'
#'   * A named function, e.g. `mean`.
#'   * An anonymous function, e.g. `\(x) x + 1` or `function(x) x + 1`.
#'   * A formula, e.g. `~ .x + 1`. Use `.x` to refer to the first argument. No
#'     longer recommended.
#'   * A string, integer, or list, e.g. `"idx"`, `1`, or `list("idx", 1)` which
#'     are shorthand for `\(x) pluck(x, "idx")`, `\(x) pluck(x, 1)`, and
#'     `\(x) pluck(x, "idx", 1)` respectively. Optionally supply `.default` to
#'     set a default value if the indexed element is `NULL` or does not exist.
#' @param .dir If `"forward"`, the default, starts at the beginning of
#'   the vector and move towards the end; if `"backward"`, starts at
#'   the end of the vector and moves towards the beginning.
#' @param .default The value returned when nothing is detected.
#' @return `detect` the value of the first item that matches the
#'  predicate; `detect_index` the position of the matching item.
#'  If not found, `detect` returns `NULL` and `detect_index`
#'  returns 0.
#'
#' @seealso [keep()] for keeping all matching values.
#' @export
#' @examples
#' is_even <- function(x) x %% 2 == 0
#'
#' 3:10 |> detect(is_even)
#' 3:10 |> detect_index(is_even)
#'
#' 3:10 |> detect(is_even, .dir = "backward")
#' 3:10 |> detect_index(is_even, .dir = "backward")
#'
#'
#' # Since `.f` is passed to as_mapper(), you can supply a pluck object:
#' x <- list(
#'   list(1, foo = FALSE),
#'   list(2, foo = TRUE),
#'   list(3, foo = TRUE)
#' )
#'
#' detect(x, "foo")
#' detect_index(x, "foo")
#'
#'
#' # If you need to find all values, use keep():
#' keep(x, "foo")
#'
#' # If you need to find all positions, use map_lgl():
#' which(map_lgl(x, "foo"))
detect <- function(
  .x,
  .f,
  ...,
  .dir = c("forward", "backward"),
  .default = NULL
) {
  .f <- as_predicate(.f, ..., .mapper = TRUE)
  .dir <- arg_match0(.dir, c("forward", "backward"))

  for (i in index(.x, .dir, "detect")) {
    if (.f(.x[[i]], ...)) {
      return(.x[[i]])
    }
  }

  .default
}

#' @export
#' @rdname detect
detect_index <- function(.x, .f, ..., .dir = c("forward", "backward")) {
  .f <- as_predicate(.f, ..., .mapper = TRUE)
  .dir <- arg_match0(.dir, c("forward", "backward"))

  for (i in index(.x, .dir, "detect_index")) {
    if (.f(.x[[i]], ...)) {
      return(i)
    }
  }

  0L
}


index <- function(x, dir, right = NULL, fn) {
  idx <- seq_along(x)
  if (dir == "backward") {
    idx <- rev(idx)
  }
  idx
}

#' Does a list contain an object?
#'
#' @inheritParams map
#' @param .y Object to test for
#' @export
#' @examples
#' x <- list(1:10, 5, 9.9)
#' x |> has_element(1:10)
#' x |> has_element(3)
has_element <- function(.x, .y) {
  some(.x, identical, .y)
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/R/every-some-none.R ---
#' Do every, some, or none of the elements of a list satisfy a predicate?
#'
#' * `some()` returns `TRUE` when `.p` is `TRUE` for at least one element.
#' * `every()` returns `TRUE` when `.p` is `TRUE` for all elements.
#' * `none()` returns `TRUE` when `.p` is `FALSE` for all elements.
#'
#' @inheritParams keep
#' @param ... Additional arguments passed on to `.p`.
#' @return A logical vector of length 1.
#' @export
#' @examples
#' x <- list(0:10, 5.5)
#' x |> every(is.numeric)
#' x |> every(is.integer)
#' x |> some(is.integer)
#' x |> none(is.character)
#'
#' # Missing values are propagated:
#' some(list(NA, FALSE), identity)
#'
#' # If you need to use these functions in a context where missing values are
#' # unsafe (e.g. in `if ()` conditions), make sure to use safe predicates:
#' if (some(list(NA, FALSE), rlang::is_true)) "foo" else "bar"
every <- function(.x, .p, ...) {
  satisfies_predicate(.x, .p, ..., .purrr_predicate = "every")
}

#' @export
#' @rdname every
some <- function(.x, .p, ...) {
  satisfies_predicate(.x, .p, ..., .purrr_predicate = "some")
}

#' @export
#' @rdname every
none <- function(.x, .p, ...) {
  satisfies_predicate(.x, .p, ..., .purrr_predicate = "none")
}

satisfies_predicate <- function(
  .x,
  .p,
  ...,
  .purrr_predicate,
  .purrr_user_env = caller_env(2),
  .purrr_error_call = caller_env()
) {
  # Not using `as_predicate()` as R level predicate result checks are too slow.
  # Checks are done at the C level instead (#1169). Also, `NA` propagates
  # through these functions, which `as_predicate()` doesn't allow.
  .p <- as_mapper(.p, ...)

  # Consistent with `map()`
  .x <- vctrs_vec_compat(.x, .purrr_user_env)
  obj_check_vector(.x, arg = ".x", call = .purrr_error_call)

  n <- vec_size(.x)

  i <- 0L

  # We refer to `.p`, `.x`, `i`, `...`, and `.purrr_error_call` all from C level
  switch(
    .purrr_predicate,
    every = .Call(every_impl, environment(), n, i),
    some = .Call(some_impl, environment(), n, i),
    none = .Call(none_impl, environment(), n, i),
    abort("Unreachable", .internal = TRUE)
  )
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/R/faq.R ---
#' Best practices for exporting adverb-wrapped functions
#'
#' @description
#' Exporting functions created with purrr adverbs in your package
#' requires some precautions because the functions will contain internal
#' purrr code. This means that creating them once and for all when
#' the package is built may cause problems when purrr is updated, because
#' a function that the adverb uses might no longer exist.
#'
#' Instead, either create the modified function once per session on package
#' load or wrap the call within another function every time you use it:
#'
#' * Using the \code{\link[=.onLoad]{.onLoad()}} hook:
#'   ```
#'   #' My function
#'   #' @export
#'   insist_my_function <- function(...) "dummy"
#'
#'   my_function <- function(...) {
#'     # Implementation
#'   }
#'
#'   .onLoad <- function(lib, pkg) {
#'     insist_my_function <<- purrr::insistently(my_function)
#'   }
#'   ```
#'
#' * Using a wrapper function:
#'   ```
#'   my_function <- function(...) {
#'     # Implementation
#'   }
#'
#'   #' My function
#'   #' @export
#'   insist_my_function <- function(...) {
#'     purrr::insistently(my_function)(...)
#'   }
#'   ```
#' @keywords internal
#' @name faq-adverbs-export
NULL


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/R/head-tail.R ---
#' Find head/tail that all satisfies a predicate.
#'
#' @inheritParams map_if
#' @inheritParams map
#' @return A vector the same type as `.x`.
#' @export
#' @examples
#' pos <- function(x) x >= 0
#' head_while(5:-5, pos)
#' tail_while(5:-5, negate(pos))
#'
#' big <- function(x) x > 100
#' head_while(0:10, big)
#' tail_while(0:10, big)
head_while <- function(.x, .p, ...) {
  # Find location of first FALSE
  .p <- as_predicate(.p, ..., .mapper = TRUE)
  loc <- detect_index(.x, negate(.p), ...)
  if (loc == 0) {
    return(.x)
  }

  .x[seq_len(loc - 1)]
}

#' @export
#' @rdname head_while
tail_while <- function(.x, .p, ...) {
  .p <- as_predicate(.p, ..., .mapper = TRUE)
  # Find location of last FALSE
  loc <- detect_index(.x, negate(.p), ..., .dir = "backward")
  if (loc == 0) {
    return(.x)
  }

  .x[-seq_len(loc)]
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/R/imap.R ---
#' Apply a function to each element of a vector, and its index
#'
#' `imap(x, ...)`, an indexed map, is short hand for
#' `map2(x, names(x), ...)` if `x` has names, or `map2(x, seq_along(x), ...)`
#' if it does not. This is useful if you need to compute on both the value
#' and the position of an element.
#'
#' @param .f A function, specified in one of the following ways:
#'
#'   * A named function, e.g. `paste`.
#'   * An anonymous function, e.g. `\(x, idx) x + idx` or
#'     `function(x, idx) x + idx`.
#'   * A formula, e.g. `~ .x + .y`. Use `.x` to refer to the current element and
#'     `.y` to refer to the current index. No longer recommended.
#'
#'   `r lifecycle::badge("experimental")`
#'
#'   Wrap a function with [in_parallel()] to declare that it should be performed
#'   in parallel. See [in_parallel()] for more details.
#'   Use of `...` is not permitted in this context.
#' @inheritParams map
#' @return A vector the same length as `.x`.
#' @export
#' @family map variants
#' @examples
#' imap_chr(sample(10), paste)
#'
#' imap_chr(sample(10), \(x, idx) paste0(idx, ": ", x))
#'
#' iwalk(mtcars, \(x, idx) cat(idx, ": ", median(x), "\n", sep = ""))
imap <- function(.x, .f, ...) {
  .f <- as_mapper(.f, ...)
  map2(.x, vec_index(.x), .f, ...)
}

#' @rdname imap
#' @export
imap_lgl <- function(.x, .f, ...) {
  .f <- as_mapper(.f, ...)
  map2_lgl(.x, vec_index(.x), .f, ...)
}

#' @rdname imap
#' @export
imap_chr <- function(.x, .f, ...) {
  .f <- as_mapper(.f, ...)
  map2_chr(.x, vec_index(.x), .f, ...)
}

#' @rdname imap
#' @export
imap_int <- function(.x, .f, ...) {
  .f <- as_mapper(.f, ...)
  map2_int(.x, vec_index(.x), .f, ...)
}

#' @rdname imap
#' @export
imap_dbl <- function(.x, .f, ...) {
  .f <- as_mapper(.f, ...)
  map2_dbl(.x, vec_index(.x), .f, ...)
}

#' @rdname imap
#' @export
imap_vec <- function(.x, .f, ...) {
  .f <- as_mapper(.f, ...)
  map2_vec(.x, vec_index(.x), .f, ...)
}


#' @export
#' @rdname imap
iwalk <- function(.x, .f, ...) {
  .f <- as_mapper(.f, ...)
  walk2(.x, vec_index(.x), .f, ...)
}


vec_index <- function(x) {
  names(x) %||% seq_along(x)
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/R/keep.R ---
#' Keep/discard elements based on their values
#'
#' `keep()` selects all elements where `.p` evaluates to `TRUE`;
#' `discard()` selects all elements where `.p` evaluates to `FALSE`.
#' `compact()` discards elements where `.p` evaluates to an empty vector.
#'
#' In other languages, `keep()` and `discard()` are often called `select()`/
#' `filter()` and `reject()`/ `drop()`, but those names are already taken
#' in R. `keep()` is similar to [Filter()], but the argument order is more
#' convenient, and the evaluation of the predicate function `.p` is stricter.
#'
#' @param .x A list or vector.
#' @param .p A predicate function (i.e. a function that returns either `TRUE`
#'   or `FALSE`) specified in one of the following ways:
#'
#'   * A named function, e.g. `is.character`.
#'   * An anonymous function, e.g. `\(x) all(x < 0)` or `function(x) all(x < 0)`.
#'   * A formula, e.g. `~ all(.x < 0)`. Use `.x` to refer to the first argument.
#'     No longer recommended.
#'
#' @seealso [keep_at()]/[discard_at()] to keep/discard elements by name.
#' @param ... Additional arguments passed on to `.p`.
#' @export
#' @examples
#' rep(10, 10) |>
#'   map(sample, 5) |>
#'   keep(function(x) mean(x) > 6)
#'
#' # Or use shorthand form
#' rep(10, 10) |>
#'   map(sample, 5) |>
#'   keep(\(x) mean(x) > 6)
#'
#' # Using a string instead of a function will select all list elements
#' # where that subelement is TRUE
#' x <- rerun(5, a = rbernoulli(1), b = sample(10))
#' x
#' x |> keep("a")
#' x |> discard("a")
#'
#' # compact() discards elements that are NULL or that have length zero
#' list(a = "a", b = NULL, c = integer(0), d = NA, e = list()) |>
#'   compact()
keep <- function(.x, .p, ...) {
  where <- where_if(.x, .p, ...)
  .x[!is.na(where) & where]
}

#' @export
#' @rdname keep
discard <- function(.x, .p, ...) {
  where <- where_if(.x, .p, ...)
  .x[is.na(where) | !where]
}

#' @export
#' @rdname keep
compact <- function(.x, .p = identity) {
  .f <- as_mapper(.p)
  discard(.x, function(x) is_empty(.f(x)))
}


#' Keep/discard elements based on their name/position
#'
#' @description
#' `keep_at()` and `discard_at()` are similar to `[` or `dplyr::select()`: they
#' return the same type of data structure as the input, but only containing
#' the requested elements. (If you're looking for a function similar to
#' `[[` see [pluck()]/[chuck()]).
#'
#' @seealso [keep()]/[discard()] to keep/discard elements by value.
#' @inheritParams map_at
#' @export
#' @examples
#' x <- c(a = 1, b = 2, cat = 10, dog = 15, elephant = 5, e = 10)
#' x |> keep_at(letters)
#' x |> discard_at(letters)
#'
#' # Can also use a function
#' x |> keep_at(\(x) nchar(x) == 3)
#' x |> discard_at(\(x) nchar(x) == 3)
keep_at <- function(x, at) {
  where <- where_at(x, at, user_env = caller_env())
  x[where]
}

#' @export
#' @rdname keep_at
discard_at <- function(x, at) {
  where <- where_at(x, at, user_env = caller_env())
  x[!where]
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/R/list-combine.R ---
#' Combine list elements into a single data structure
#'
#' @description
#' * `list_c()` combines elements into a vector by concatenating them together
#'   with [vctrs::vec_c()].
#'
#' * `list_rbind()` combines elements into a data frame by row-binding them
#'   together with [vctrs::vec_rbind()].
#'
#' * `list_cbind()` combines elements into a data frame by column-binding them
#'   together with [vctrs::vec_cbind()].
#'
#' @param x A list. For `list_rbind()` and `list_cbind()` the list must
#'   only contain only data frames or `NULL`.
#' @param ptype An optional prototype to ensure that the output type is always
#'   the same.
#' @param names_to By default, `names(x)` are lost. To keep them, supply a
#'   string to `names_to` and the names will be saved into a column with that
#'   name. If `names_to` is supplied and `x` is not named, the position of
#'   the elements will be used instead of the names.
#' @param size An optional integer size to ensure that every input has the
#'   same size (i.e. number of rows).
#' @param name_repair One of `"unique"`, `"universal"`, or `"check_unique"`.
#'   See [vctrs::vec_as_names()] for the meaning of these options.
#' @inheritParams rlang::args_dots_empty
#' @export
#' @examples
#' x1 <- list(a = 1, b = 2, c = 3)
#' list_c(x1)
#'
#' x2 <- list(
#'   a = data.frame(x = 1:2),
#'   b = data.frame(y = "a")
#' )
#' list_rbind(x2)
#' list_rbind(x2, names_to = "id")
#' list_rbind(unname(x2), names_to = "id")
#'
#' list_cbind(x2)
list_c <- function(x, ..., ptype = NULL) {
  obj_check_list(x)
  check_dots_empty()

  # For `list_c()`, we don't expose `list_unchop()`'s `name_spec` arg,
  # and instead strip outer names to avoid collisions with inner names
  x <- unname(x)

  list_unchop(
    x,
    ptype = ptype,
    error_call = current_env()
  )
}

#' @export
#' @rdname list_c
list_cbind <- function(
  x,
  ...,
  name_repair = c("unique", "universal", "check_unique"),
  size = NULL
) {
  check_list_of_data_frames(x)
  check_dots_empty()

  vec_cbind(
    !!!x,
    .name_repair = name_repair,
    .size = size,
    .error_call = current_env()
  )
}

#' @export
#' @rdname list_c
list_rbind <- function(x, ..., names_to = rlang::zap(), ptype = NULL) {
  check_list_of_data_frames(x)
  check_dots_empty()

  vec_rbind(
    !!!x,
    .names_to = names_to,
    .ptype = ptype,
    .error_call = current_env()
  )
}


check_list_of_data_frames <- function(x, error_call = caller_env()) {
  obj_check_list(x, call = error_call)

  is_df_or_null <- map_lgl(x, function(x) is.data.frame(x) || is.null(x))

  if (all(is_df_or_null)) {
    return()
  }

  bad <- which(!is_df_or_null)
  cli::cli_abort(
    c(
      "Each element of {.arg x} must be either a data frame or {.code NULL}.",
      i = "Elements {bad} are not."
    ),
    arg = "x",
    call = error_call
  )
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/R/list-flatten.R ---
#' Flatten a list
#'
#' Flattening a list removes a single layer of internal hierarchy,
#' i.e. it inlines elements that are lists leaving non-lists alone.
#'
#' @param x A list.
#' @param name_spec If both inner and outer names are present, control
#'   how they are combined. Should be a glue specification that uses
#'   variables `inner` and `outer`.
#' @param name_repair One of `"minimal"`, `"unique"`, `"universal"`, or
#'   `"check_unique"`. See [vctrs::vec_as_names()] for the meaning of these
#'   options.
#' @inheritParams rlang::args_dots_empty
#' @inheritParams modify_tree
#' @return A list of the same type as `x`. The list might be shorter
#'   if `x` contains empty lists, the same length if it contains lists
#'   of length 1 or no sub-lists, or longer if it contains lists of
#'   length > 1.
#' @export
#' @examples
#' x <- list(1, list(2, 3), list(4, list(5)))
#' x |> list_flatten() |> str()
#' x |> list_flatten() |> list_flatten() |> str()
#'
#' # Flat lists are left as is
#' list(1, 2, 3, 4, 5) |> list_flatten() |> str()
#'
#' # Empty lists will disappear
#' list(1, list(), 2, list(3)) |> list_flatten() |> str()
#'
#' # Another way to see this is that it reduces the depth of the list
#' x <- list(
#'   list(),
#'   list(list())
#' )
#' x |> pluck_depth()
#' x |> list_flatten() |> pluck_depth()
#'
#' # Use name_spec to control how inner and outer names are combined
#' x <- list(x = list(a = 1, b = 2), y = list(c = 1, d = 2))
#' x |> list_flatten() |> names()
#' x |> list_flatten(name_spec = "{outer}") |> names()
#' x |> list_flatten(name_spec = "{inner}") |> names()
#'
#' # Set `is_node = is.list` to also flatten richer objects built on lists like
#' # data frames and linear models
#' df <- data.frame(x = 1:3, y = 4:6)
#' x <- list(
#'   a_string = "something",
#'   a_list = list(1:3, "else"),
#'   a_df = df
#' )
#' x |> list_flatten(is_node = is.list)
#'
#' # Note that objects that are already "flat" retain their classes
#' list_flatten(df, is_node = is.list)
list_flatten <- function(
  x,
  ...,
  is_node = NULL,
  name_spec = "{outer}_{inner}",
  name_repair = c("minimal", "unique", "check_unique", "universal")
) {
  is_node <- as_is_node(is_node)
  if (!is_node(x)) {
    cli::cli_abort("{.arg x} must be a node.")
  }

  check_dots_empty()
  check_string(name_spec)

  # Take the proxy as we restore on exit
  proxy <- vec_proxy(x)

  # Unclass S3 lists to avoid their coercion methods. Wrap atoms in a
  # list of size 1 so the elements can be concatenated in a single list.
  proxy <- map_if(proxy, is_node, unclass, .else = list)

  out <- list_unchop(
    proxy,
    ptype = list(),
    name_spec = name_spec,
    name_repair = name_repair,
    error_arg = x,
    error_call = current_env()
  )

  # Preserve input type
  vec_restore(out, x)
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/R/list-modify.R ---
#' Modify a list
#'
#' @description
#' * `list_assign()` modifies the elements of a list by name or position.
#' * `list_modify()` modifies the elements of a list recursively.
#' * `list_merge()` merges the elements of a list recursively.
#'
#' `list_modify()` is inspired by [utils::modifyList()].
#'
#' @param .x List to modify.
#' @param ... New values of a list. Use `zap()` to remove values.
#'
#'   These values should be either all named or all unnamed. When
#'   inputs are all named, they are matched to `.x` by name. When they
#'   are all unnamed, they are matched by position.
#'
#'   [Dynamic dots][rlang::dyn-dots] are supported. In particular, if your
#'   replacement values are stored in a list, you can splice that in with
#'   `!!!`.
#' @inheritParams map_depth
#' @export
#' @examples
#' x <- list(x = 1:10, y = 4, z = list(a = 1, b = 2))
#' str(x)
#'
#' # Update values
#' str(list_assign(x, a = 1))
#'
#' # Replace values
#' str(list_assign(x, z = 5))
#' str(list_assign(x, z = NULL))
#' str(list_assign(x, z = list(a = 1:5)))
#'
#' # Replace recursively with list_modify(), leaving the other elements of z alone
#' str(list_modify(x, z = list(a = 1:5)))
#'
#' # Remove values
#' str(list_assign(x, z = zap()))
#'
#' # Combine values with list_merge()
#' str(list_merge(x, x = 11, z = list(a = 2:5, c = 3)))
#'
#' # All these functions support dynamic dots features. Use !!! to splice
#' # a list of arguments:
#' l <- list(new = 1, y = zap(), z = 5)
#' str(list_assign(x, !!!l))
list_assign <- function(.x, ..., .is_node = NULL) {
  check_list(.x)
  y <- dots_list(..., .named = NULL, .homonyms = "error")

  list_recurse(.x, y, function(x, y) y, recurse = FALSE, is_node = .is_node)
}

#' @export
#' @rdname list_assign
list_modify <- function(.x, ..., .is_node = NULL) {
  check_list(.x)
  y <- dots_list(..., .named = NULL, .homonyms = "error")

  list_recurse(.x, y, function(x, y) y, is_node = .is_node)
}

#' @export
#' @rdname list_assign
list_merge <- function(.x, ..., .is_node = NULL) {
  check_list(.x)
  y <- dots_list(..., .named = NULL, .homonyms = "error")

  list_recurse(.x, y, c, is_node = .is_node)
}

list_recurse <- function(
  x,
  y,
  base_f,
  recurse = TRUE,
  error_call = caller_env(),
  is_node = NULL
) {
  is_node <- as_is_node(
    is_node,
    error_call = error_call,
    error_arg = ".is_node"
  )

  if (!is_null(names(y)) && !is_named(y)) {
    cli::cli_abort(
      "`...` arguments must be either all named or all unnamed.",
      call = error_call
    )
  }

  idx <- names(y) %||% rev(seq_along(y))

  for (i in idx) {
    x_i <- pluck(x, i)
    y_i <- pluck(y, i)

    if (is_zap(y_i)) {
      x[[i]] <- NULL
    } else if (recurse && is_node(x_i) && is_node(y_i)) {
      list_slice2(x, i) <- list_recurse(x_i, y_i, base_f)
    } else {
      list_slice2(x, i) <- base_f(x_i, y_i)
    }
  }

  x
}

check_list <- function(x, call = caller_env(), arg = caller_arg(x)) {
  if (!is.list(x)) {
    cli::cli_abort(
      "{.arg {arg}} must be a list, not {.obj_type_friendly {x}}.",
      call = call,
      arg = arg
    )
  }
}

#' Update a list with formulas
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `update_list()` was deprecated in purrr 1.0.0, because we no longer believe
#' that functions that use NSE are a good fit for purrr.
#'
#' `update_list()` handles formulas and quosures that can refer to
#' values existing within the input list. This function is deprecated
#' because we no longer believe that functions that use tidy evaluation are
#' a good fit for purrr.
#'
#' @inheritParams list_modify
#' @export
#' @keywords internal
update_list <- function(.x, ...) {
  lifecycle::deprecate_warn("1.0.0", "update_list()")

  dots <- dots_list(...)

  formulas <- map_lgl(dots, is_bare_formula, lhs = FALSE, scoped = TRUE)
  dots <- map_if(dots, formulas, as_quosure)
  dots <- map_if(dots, is_quosure, eval_tidy, data = .x)

  list_recurse(.x, dots, function(x, y) y)
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/R/list-simplify.R ---
#' Simplify a list to an atomic or S3 vector
#'
#' Simplification maintains a one-to-one correspondence between the input
#' and output, implying that each element of `x` must contain a one element
#' vector or a one-row data frame. If you don't want to maintain this
#' correspondence, then you probably want either [list_c()]/[list_rbind()] or
#' [list_flatten()].
#'
#' @param x A list.
#' @param strict What should happen if simplification fails? If `TRUE`
#'   (the default) it will error. If `FALSE` and `ptype` is not supplied,
#'   it will return `x` unchanged.
#' @param ptype An optional prototype to ensure that the output type is always
#'   the same.
#' @inheritParams rlang::args_dots_empty
#' @returns A vector the same length as `x`.
#' @export
#' @examples
#' list_simplify(list(1, 2, 3))
#'
#' # Only works when vectors are length one and have compatible types:
#' try(list_simplify(list(1, 2, 1:3)))
#' try(list_simplify(list(1, 2, "x")))
#'
#' # Unless you strict = FALSE, in which case you get the input back:
#' list_simplify(list(1, 2, 1:3), strict = FALSE)
#' list_simplify(list(1, 2, "x"), strict = FALSE)
list_simplify <- function(x, ..., strict = TRUE, ptype = NULL) {
  check_dots_empty()
  check_bool(strict)

  simplify_impl(x, strict = strict, ptype = ptype)
}

# Wrapper used by purrr functions that do automatic simplification
list_simplify_internal <- function(
  x,
  simplify = NA,
  ptype = NULL,
  error_arg = caller_arg(x),
  error_call = caller_env()
) {
  check_bool(simplify, allow_na = TRUE, call = error_call)
  if (!is.null(ptype) && isFALSE(simplify)) {
    cli::cli_abort(
      "Can't specify {.arg ptype} when `simplify = FALSE`.",
      arg = "ptype",
      call = error_call
    )
  }

  if (isFALSE(simplify)) {
    return(x)
  }

  simplify_impl(
    x,
    strict = !is.na(simplify),
    ptype = ptype,
    error_arg = error_arg,
    error_call = error_call
  )
}

simplify_impl <- function(
  x,
  strict = TRUE,
  ptype = NULL,
  error_arg = caller_arg(x),
  error_call = caller_env()
) {
  obj_check_list(x, arg = error_arg, call = error_call)

  # Handle the cases where we definitely can't simplify
  if (strict) {
    list_check_all_vectors(x, arg = error_arg, call = error_call)
    list_check_all_size(x, 1, arg = error_arg, call = error_call)
  } else {
    can_simplify <- list_all_vectors(x) && all(list_sizes(x) == 1L)

    if (!can_simplify) {
      return(x)
    }
  }

  names <- vec_names(x)
  x <- vec_set_names(x, NULL)

  out <- tryCatch(
    list_unchop(
      x,
      ptype = ptype,
      error_arg = error_arg,
      error_call = error_call
    ),
    vctrs_error_incompatible_type = function(err) {
      if (strict || !is.null(ptype)) {
        cnd_signal(err)
      } else {
        x
      }
    }
  )
  if (!is.null(out)) {
    out <- vec_set_names(out, names)
  }
  out
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/R/list-transpose.R ---
#' Transpose a list
#'
#' @description
#' `list_transpose()` turns a list-of-lists "inside-out". For instance it turns a pair of
#' lists into a list of pairs, or a list of pairs into a pair of lists. For
#' example, if you had a list of length `n` where each component had values `a`
#' and `b`, `list_transpose()` would make a list with elements `a` and
#' `b` that contained lists of length `n`.
#'
#' It's called transpose because `x[["a"]][["b"]]` is equivalent to
#' `list_transpose(x)[["b"]][["a"]]`, i.e. transposing a list flips the order of
#' indices in a similar way to transposing a matrix.
#'
#' @param x A list of vectors to transpose.
#' @param template A "template" that describes the output list. Can either be
#'   a character vector (where elements are extracted by name), or an integer
#'   vector (where elements are extracted by position). Defaults to the union
#'   of the names of the elements of `x`, or if they're not present, the
#'   union of the integer indices.
#' @param simplify Should the result be [simplified][list_simplify]?
#'   * `TRUE`: simplify or die trying.
#'   * `NA`: simplify if possible.
#'   * `FALSE`: never try to simplify, always leaving as a list.
#'
#'   Alternatively, a named list specifying the simplification by output
#'   element.
#' @param ptype An optional vector prototype used to control the simplification.
#'   Alternatively, a named list specifying the prototype by output element.
#' @param default A default value to use if a value is absent or `NULL`.
#'   Alternatively, a named list specifying the default by output element.
#' @inheritParams rlang::args_dots_empty
#' @export
#' @examples
#' # list_transpose() is useful in conjunction with safely()
#' x <- list("a", 1, 2)
#' y <- x |> map(safely(log))
#' y |> str()
#' # Put all the errors and results together
#' y |> list_transpose() |> str()
#' # Supply a default result to further simplify
#' y |> list_transpose(default = list(result = NA)) |> str()
#'
#' # list_transpose() will try to simplify by default:
#' x <- list(list(a = 1, b = 2), list(a = 3, b = 4), list(a = 5, b = 6))
#' x |> list_transpose()
#' # this makes list_tranpose() not completely symmetric
#' x |> list_transpose() |> list_transpose()
#'
#' # use simplify = FALSE to always return lists:
#' x |> list_transpose(simplify = FALSE) |> str()
#' x |>
#'   list_transpose(simplify = FALSE) |>
#'   list_transpose(simplify = FALSE) |> str()
#'
#' # Provide an explicit template if you know which elements you want to extract
#' ll <- list(
#'   list(x = 1, y = "one"),
#'   list(z = "deux", x = 2)
#' )
#' ll |> list_transpose()
#' ll |> list_transpose(template = c("x", "y", "z"))
#' ll |> list_transpose(template = 1)
#'
#' # And specify a default if you want to simplify
#' ll |> list_transpose(template = c("x", "y", "z"), default = NA)
list_transpose <- function(
  x,
  ...,
  template = NULL,
  simplify = NA,
  ptype = NULL,
  default = NULL
) {
  obj_check_list(x)
  check_dots_empty()

  if (length(x) == 0) {
    template <- integer()
  } else if (is.null(template)) {
    indexes <- map(x, vec_index)
    call <- current_env()
    withCallingHandlers(
      template <- reduce(indexes, vec_set_union),
      vctrs_error_ptype2 = function(e) {
        cli::cli_abort(
          "Can't combine named and unnamed vectors.",
          arg = template,
          call = call
        )
      }
    )
  }

  if (!is.character(template) && !is.numeric(template)) {
    cli::cli_abort(
      "{.arg template} must be a character or numeric vector, not {.obj_type_friendly {template}}.",
      arg = template
    )
  }

  simplify <- match_template(simplify, template)
  default <- match_template(default, template)
  ptype <- match_template(ptype, template)

  out <- rep_along(template, list())
  if (is.character(template)) {
    names(out) <- template
  }

  for (i in seq_along(template)) {
    idx <- template[[i]]
    res <- map(x, idx, .default = default[[i]])
    res <- list_simplify_internal(
      res,
      simplify = simplify[[i]] %||% NA,
      ptype = ptype[[i]],
      error_arg = result_index(idx)
    )
    out[[i]] <- res
  }

  out
}

result_index <- function(idx) {
  if (is.character(idx)) {
    paste0("result$", idx)
  } else {
    paste0("result[[", idx, "]]")
  }
}

match_template <- function(
  x,
  template,
  error_arg = caller_arg(x),
  error_call = caller_env()
) {
  if (is.character(template)) {
    if (is_bare_list(x) && is_named(x)) {
      extra_names <- setdiff(names(x), template)
      if (length(extra_names)) {
        cli::cli_abort(
          "{.arg {error_arg}} contains unknown names: {.str {extra_names}}.",
          arg = error_arg,
          call = error_call
        )
      }

      out <- rep_named(template, list(NULL))
      out[names(x)] <- x
      out
    } else {
      rep_named(template, list(x))
    }
  } else if (is.numeric(template)) {
    if (is_bare_list(x) && length(x) > 0) {
      if (length(x) != length(template)) {
        cli::cli_abort(
          "Length of {.arg {error_arg}} ({length(x)}) and {.arg template} ({length(template)}) must be the same when transposing by position.",
          arg = error_arg,
          call = error_call
        )
      }
      x
    } else {
      rep_along(template, list(x))
    }
  } else {
    cli::cli_abort("Invalid `template`", .internal = TRUE)
  }
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/R/lmap.R ---
#' Apply a function to list-elements of a list
#'
#' @description
#' `lmap()`, `lmap_at()` and `lmap_if()` are similar to `map()`, `map_at()` and
#' `map_if()`, except instead of mapping over `.x[[i]]`, they instead map over
#' `.x[i]`.
#'
#' This has several advantages:
#'
#' * It makes it possible to work with functions that exclusively take a list.
#' * It allows `.f` to access the attributes of the encapsulating list,
#'   like [names()].
#' * It allows `.f` to return a larger or small list than it receives
#'   changing the size of the output.
#'
#' @param .x A list or data frame.
#' @param .f A function that takes a length-1 list and returns a list (of any
#'   length.)
#' @inheritParams map_if
#' @inheritParams map_at
#' @inheritParams map
#' @return A list or data frame, matching `.x`. There are no guarantees about
#'   the length.
#' @family map variants
#' @export
#' @examples
#' set.seed(1014)
#'
#' # Let's write a function that returns a larger list or an empty list
#' # depending on some condition. It also uses the input name to name the
#' # output
#' maybe_rep <- function(x) {
#'   n <- rpois(1, 2)
#'   set_names(rep_len(x, n), paste0(names(x), seq_len(n)))
#' }
#'
#' # The output size varies each time we map f()
#' x <- list(a = 1:4, b = letters[5:7], c = 8:9, d = letters[10])
#' x |> lmap(maybe_rep) |> str()
#'
#' # We can apply f() on a selected subset of x
#' x |> lmap_at(c("a", "d"), maybe_rep) |> str()
#'
#' # Or only where a condition is satisfied
#' x |> lmap_if(is.character, maybe_rep) |> str()
lmap <- function(.x, .f, ...) {
  lmap_helper(.x, rep(TRUE, length(.x)), .f, ...)
}

#' @rdname lmap
#' @export
lmap_if <- function(.x, .p, .f, ..., .else = NULL) {
  where <- where_if(.x, .p)
  lmap_helper(.x, where, .f, ..., .else = .else)
}

#' @rdname lmap
#' @export
lmap_at <- function(.x, .at, .f, ...) {
  where <- where_at(.x, .at, user_env = caller_env())
  lmap_helper(.x, where, .f, ...)
}

lmap_helper <- function(
  .x,
  .ind,
  .f,
  ...,
  .else = NULL,
  .purrr_error_call = caller_env()
) {
  .f <- rlang::as_function(.f, call = .purrr_error_call)
  if (!is.null(.else)) {
    .else <- rlang::as_function(.else, call = .purrr_error_call)
  }

  out <- vector("list", length(.x))
  for (i in seq_along(.x)) {
    if (.ind[[i]]) {
      res <- .f(.x[i], ...)
    } else if (is.null(.else)) {
      res <- .x[i]
    } else {
      res <- .else(.x[i], ...)
    }

    if (!is.list(res)) {
      cli::cli_abort(
        "{.code .f(.x[[{i}]])} must return a list, not {.obj_type_friendly {res}}.",
        call = .purrr_error_call
      )
    }
    out[[i]] <- res
  }

  if (is.data.frame(.x)) {
    out <- lapply(out, as.data.frame)
    list_cbind(out)
  } else {
    list_flatten(out)
  }
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/R/map-depth.R ---
#' Map/modify elements at given depth
#'
#' `map_depth()` calls `map(.y, .f)` on all `.y` at the specified `.depth` in
#' `.x`. `modify_depth()` calls `modify(.y, .f)` on `.y` at the specified
#' `.depth` in `.x`.
#'
#' @inheritParams map
#' @param .depth Level of `.x` to map on. Use a negative value to
#'   count up from the lowest level of the list.
#'
#'   * `map_depth(x, 0, fun)` is equivalent to `fun(x)`.
#'   * `map_depth(x, 1, fun)` is equivalent to `x <- map(x, fun)`
#'   * `map_depth(x, 2, fun)` is equivalent to `x <- map(x, \(y) map(y, fun))`
#' @param .ragged If `TRUE`, will apply to leaves, even if they're not
#'   at depth `.depth`. If `FALSE`, will throw an error if there are
#'   no elements at depth `.depth`.
#' @inheritParams modify_tree
#' @seealso [modify_tree()] for a recursive version of `modify_depth()` that
#'   allows you to apply a function to every leaf or every node.
#' @family map variants
#' @family modify variants
#' @export
#' @examples
#' # map_depth() -------------------------------------------------
#' # Use `map_depth()` to recursively traverse nested vectors and map
#' # a function at a certain depth:
#' x <- list(a = list(foo = 1:2, bar = 3:4), b = list(baz = 5:6))
#' x |> str()
#' x |> map_depth(2, \(y) paste(y, collapse = "/")) |> str()
#'
#' # Equivalent to:
#' x |> map(\(y) map(y, \(z) paste(z, collapse = "/"))) |> str()
#'
#' # When ragged is TRUE, `.f()` will also be passed leaves at depth < `.depth`
#' x <- list(1, list(1, list(1, list(1, 1))))
#' x |> str()
#' x |> map_depth(4, \(x) length(unlist(x)), .ragged = TRUE) |> str()
#' x |> map_depth(3, \(x) length(unlist(x)), .ragged = TRUE) |> str()
#' x |> map_depth(2, \(x) length(unlist(x)), .ragged = TRUE) |> str()
#' x |> map_depth(1, \(x) length(unlist(x)), .ragged = TRUE) |> str()
#' x |> map_depth(0, \(x) length(unlist(x)), .ragged = TRUE) |> str()
#'
#' # modify_depth() -------------------------------------------------
#' l1 <- list(
#'   obj1 = list(
#'     prop1 = list(param1 = 1:2, param2 = 3:4),
#'     prop2 = list(param1 = 5:6, param2 = 7:8)
#'   ),
#'   obj2 = list(
#'     prop1 = list(param1 = 9:10, param2 = 11:12),
#'     prop2 = list(param1 = 12:14, param2 = 15:17)
#'   )
#' )
#'
#' # In the above list, "obj" is level 1, "prop" is level 2 and "param"
#' # is level 3. To apply sum() on all params, we map it at depth 3:
#' l1 |> modify_depth(3, sum) |> str()
#'
#' # modify() lets us pluck the elements prop1/param2 in obj1 and obj2:
#' l1 |> modify(c("prop1", "param2")) |> str()
#'
#' # But what if we want to pluck all param2 elements? Then we need to
#' # act at a lower level:
#' l1 |> modify_depth(2, "param2") |> str()
#'
#' # modify_depth() can be with other purrr functions to make them operate at
#' # a lower level. Here we ask pmap() to map paste() simultaneously over all
#' # elements of the objects at the second level. paste() is effectively
#' # mapped at level 3.
#' l1 |> modify_depth(2, \(x) pmap(x, paste, sep = " / ")) |> str()
map_depth <- function(
  .x,
  .depth,
  .f,
  ...,
  .ragged = .depth < 0,
  .is_node = NULL
) {
  force(.ragged)
  .depth <- check_depth(.depth, pluck_depth(.x, .is_node))
  .f <- as_mapper(.f, ...)
  .is_node <- as_is_node(.is_node)
  map_depth_rec(
    map,
    .x,
    .depth,
    .f,
    ...,
    .ragged = .ragged,
    .is_node = .is_node
  )
}

#' @rdname map_depth
#' @export
modify_depth <- function(
  .x,
  .depth,
  .f,
  ...,
  .ragged = .depth < 0,
  .is_node = NULL
) {
  force(.ragged)
  .depth <- check_depth(.depth, pluck_depth(.x, .is_node))
  .f <- as_mapper(.f, ...)
  .is_node <- as_is_node(.is_node)
  map_depth_rec(
    modify,
    .x,
    .depth,
    .f,
    ...,
    .ragged = .ragged,
    .is_node = .is_node
  )
}

map_depth_rec <- function(
  .fmap,
  .x,
  .depth,
  .f,
  ...,
  .ragged,
  .is_node,
  .purrr_error_call = caller_env()
) {
  if (.depth == 0) {
    if (identical(.fmap, map)) {
      return(.f(.x, ...))
    } else {
      .x[] <- .f(.x, ...)
      return(.x)
    }
  }

  if (!.is_node(.x)) {
    if (.ragged) {
      return(.fmap(.x, .f, ...))
    } else {
      cli::cli_abort("List not deep enough", call = .purrr_error_call)
    }
  }

  if (.depth == 1) {
    .fmap(.x, .f, ...)
  } else {
    .fmap(.x, function(x) {
      map_depth_rec(
        .fmap = .fmap,
        .x = x,
        .depth = .depth - 1,
        .f = .f,
        ...,
        .ragged = .ragged,
        .is_node = .is_node,
        .purrr_error_call = .purrr_error_call
      )
    })
  }
}

check_depth <- function(depth, max_depth, error_call = caller_env()) {
  check_number_whole(depth, call = error_call)

  if (depth < 0) {
    if (-depth > max_depth) {
      cli::cli_abort(
        "Negative {.arg .depth} ({depth}) must be greater than -{max_depth}.",
        arg = ".depth",
        call = error_call
      )
    }
    depth <- max_depth + depth
  }
  depth
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/R/map-if-at.R ---
#' Apply a function to each element of a vector conditionally
#'
#' @description
#' The functions `map_if()` and `map_at()` take `.x` as input, apply
#' the function `.f` to some of the elements of `.x`, and return a
#' list of the same length as the input.
#'
#' * `map_if()` takes a predicate function `.p` as input to determine
#'   which elements of `.x` are transformed with `.f`.
#'
#' * `map_at()` takes a vector of names or positions `.at` to specify
#'   which elements of `.x` are transformed with `.f`.
#'
#' @inheritParams map
#' @param .p A single predicate function, a formula describing such a
#'   predicate function, or a logical vector of the same length as `.x`.
#'   Alternatively, if the elements of `.x` are themselves lists of
#'   objects, a string indicating the name of a logical element in the
#'   inner lists. Only those elements where `.p` evaluates to
#'   `TRUE` will be modified.
#' @param .else A function applied to elements of `.x` for which `.p`
#' returns `FALSE`.
#' @family map variants
#' @export
#' @examples
#' # Use a predicate function to decide whether to map a function:
#' iris |> map_if(is.factor, as.character) |> str()
#'
#' # Specify an alternative with the `.else` argument:
#' iris |> map_if(is.factor, as.character, .else = as.integer) |> str()
#'
#' # Use numeric vector of positions select elements to change:
#' iris |> map_at(c(4, 5), is.numeric) |> str()
#'
#' # Use vector of names to specify which elements to change:
#' iris |> map_at("Species", toupper) |> str()
map_if <- function(.x, .p, .f, ..., .else = NULL) {
  where <- where_if(.x, .p)

  out <- vector("list", length(.x))
  out[where] <- map(.x[where], .f, ...)

  if (is_null(.else)) {
    out[!where] <- .x[!where]
  } else {
    out[!where] <- map(.x[!where], .else, ...)
  }

  set_names(out, names(.x))
}
#' @rdname map_if
#' @param .at A logical, integer, or character vector giving the elements
#'   to select. Alternatively, a function that takes a vector of names,
#'   and returns a logical, integer, or character vector of elements to select.
#'
#'   `r lifecycle::badge("deprecated")`: if the tidyselect package is
#'   installed, you can use `vars()` and tidyselect helpers to select
#'   elements.
#' @export
map_at <- function(.x, .at, .f, ..., .progress = FALSE) {
  where <- where_at(.x, .at, user_env = caller_env())

  out <- vector("list", length(.x))
  out[where] <- map(.x[where], .f, ..., .progress = .progress)
  out[!where] <- .x[!where]

  set_names(out, names(.x))
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/R/map-mapper.R ---
#' Convert an object into a mapper function
#'
#' `as_mapper` is the powerhouse behind the varied function
#' specifications that most purrr functions allow. It is an S3
#' generic. The default method forwards its arguments to
#' [rlang::as_function()].
#'
#' @param .f A function, formula, or vector (not necessarily atomic).
#'
#'   If a __function__, it is used as is.
#'
#'   If a __formula__, e.g. `~ .x + 2`, it is converted to a function.
#'   No longer recommended.
#'
#'   If __character vector__, __numeric vector__, or __list__, it is
#'   converted to an extractor function. Character vectors index by
#'   name and numeric vectors index by position; use a list to index
#'   by position and name at different levels. If a component is not
#'   present, the value of `.default` will be returned.
#' @param .default,.null Optional additional argument for extractor functions
#'   (i.e. when `.f` is character, integer, or list). Returned when
#'   value is absent (does not exist) or empty (has length 0).
#'   `.null` is deprecated; please use `.default` instead.
#' @param ... Additional arguments passed on to methods.
#' @export
#' @examples
#' as_mapper(\(x) x + 1)
#' as_mapper(1)
#'
#' as_mapper(c("a", "b", "c"))
#' # Equivalent to function(x) x[["a"]][["b"]][["c"]]
#'
#' as_mapper(list(1, "a", 2))
#' # Equivalent to function(x) x[[1]][["a"]][[2]]
#'
#' as_mapper(list(1, attr_getter("a")))
#' # Equivalent to function(x) attr(x[[1]], "a")
#'
#' as_mapper(c("a", "b", "c"), .default = NA)
as_mapper <- function(.f, ...) {
  UseMethod("as_mapper")
}

#' @export
as_mapper.default <- function(.f, ...) {
  rlang::as_function(.f)
}

#' @export
#' @rdname as_mapper
as_mapper.character <- function(.f, ..., .null, .default = NULL) {
  .default <- find_extract_default(.null, .default)
  plucker(as.list(.f), .default)
}

#' @export
#' @rdname as_mapper
as_mapper.numeric <- function(.f, ..., .null, .default = NULL) {
  .default <- find_extract_default(.null, .default)
  plucker(as.list(.f), .default)
}

#' @export
#' @rdname as_mapper
as_mapper.list <- function(.f, ..., .null, .default = NULL) {
  .default <- find_extract_default(.null, .default)
  plucker(.f, .default)
}

find_extract_default <- function(.null, .default) {
  if (!missing(.null)) {
    # warning("`.null` is deprecated; please use `.default` instead", call. = FALSE)
    .null
  } else {
    .default
  }
}

plucker <- function(i, default) {
  x <- NULL # supress global variables check NOTE
  i <- as.list(i)

  # Use metaprogramming to create function that prints nicely
  new_function(
    exprs(x = , ... = ),
    expr(pluck_raw(x, !!i, .default = !!default))
  )
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/R/map-raw.R ---
#' Functions that return raw vectors
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' These functions were deprecated in purrr 1.0.0 because they are of limited
#' use and you can now use [map_vec()] instead. They are variants of [map()],
#' [map2()], [imap()], [pmap()], and [flatten()] that return raw vectors.
#'
#' @keywords internal
#' @export
map_raw <- function(.x, .f, ...) {
  lifecycle::deprecate_warn("1.0.0", "map_raw()", "map_vec()")
  map_("raw", .x, .f, ...)
}

#' @export
#' @rdname map_raw
map2_raw <- function(.x, .y, .f, ...) {
  lifecycle::deprecate_warn("1.0.0", "map2_raw()", "map2_vec()")
  map2_("raw", .x, .y, .f, ...)
}

#' @rdname map_raw
#' @export
imap_raw <- function(.x, .f, ...) {
  lifecycle::deprecate_warn("1.0.0", "imap_raw()", "imap_vec()")
  map2_("raw", .x, vec_index(.x), .f, ...)
}

#' @export
#' @rdname map_raw
pmap_raw <- function(.l, .f, ...) {
  lifecycle::deprecate_warn("1.0.0", "pmap_raw()", "pmap_vec()")
  pmap_("raw", .l, .f, ...)
}

#' @export
#' @rdname map_raw
flatten_raw <- function(.x) {
  lifecycle::deprecate_warn("1.0.0", "flatten_raw()")
  .Call(vflatten_impl, .x, "raw")
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/R/map.R ---
#' Apply a function to each element of a vector
#'
#' @description
#' The map functions transform their input by applying a function to
#' each element of a list or atomic vector and returning an object of
#' the same length as the input.
#'
#' * `map()` always returns a list. See the [modify()] family for
#'   versions that return an object of the same type as the input.
#'
#' * `map_lgl()`, `map_int()`, `map_dbl()` and `map_chr()` return an
#'   atomic vector of the indicated type (or die trying). For these functions,
#'   `.f` must return a length-1 vector of the appropriate type.
#'
#' * `map_vec()` simplifies to the common type of the output. It works with
#'   most types of simple vectors like Date, POSIXct, factors, etc.
#'
#' * `walk()` calls `.f` for its side-effect and returns
#'   the input `.x`.
#'
#' @param .x A list or atomic vector.
#' @param .f A function, specified in one of the following ways:
#'
#'   * A named function, e.g. `mean`.
#'   * An anonymous function, e.g. `\(x) x + 1` or `function(x) x + 1`.
#'   * A formula, e.g. `~ .x + 1`. Use `.x` to refer to the first
#'     argument. No longer recommended.
#'   * A string, integer, or list, e.g. `"idx"`, `1`, or `list("idx", 1)` which
#'     are shorthand for `\(x) pluck(x, "idx")`, `\(x) pluck(x, 1)`, and
#'     `\(x) pluck(x, "idx", 1)` respectively. Optionally supply `.default` to
#'     set a default value if the indexed element is `NULL` or does not exist.
#'
#'   `r lifecycle::badge("experimental")`
#'
#'   Wrap a function with [in_parallel()] to declare that it should be performed
#'   in parallel. See [in_parallel()] for more details.
#'   Use of `...` is not permitted in this context.
#'
#' @param ... Additional arguments passed on to the mapped function.
#'
#'   We now generally recommend against using `...` to pass additional
#'   (constant) arguments to `.f`. Instead use a shorthand anonymous function:
#'
#'   ```R
#'   # Instead of
#'   x |> map(f, 1, 2, collapse = ",")
#'   # do:
#'   x |> map(\(x) f(x, 1, 2, collapse = ","))
#'   ```
#'
#'   This makes it easier to understand which arguments belong to which
#'   function and will tend to yield better error messages.
#'
#' @param .progress Whether to show a progress bar. Use `TRUE` to turn on
#'   a basic progress bar, use a string to give it a name, or see
#'   [progress_bars] for more details.
#' @returns
#' The output length is determined by the length of the input.
#' The output names are determined by the input names.
#' The output type is determined by the suffix:
#'
#' * No suffix: a list; `.f()` can return anything.
#'
#' * `_lgl()`, `_int()`, `_dbl()`, `_chr()` return a logical, integer, double,
#'   or character vector respectively; `.f()` must return a compatible atomic
#'   vector of length 1.
#'
#' * `_vec()` return an atomic or S3 vector, the same type that `.f` returns.
#'   `.f` can return pretty much any type of vector, as long as its length 1.
#'
#' * `walk()` returns the input `.x` (invisibly). This makes it easy to
#'    use in a pipe. The return value of `.f()` is ignored.
#'
#' Any errors thrown by `.f` will be wrapped in an error with class
#' [purrr_error_indexed].
#' @export
#' @family map variants
#' @seealso [map_if()] for applying a function to only those elements
#'   of `.x` that meet a specified condition.
#' @examples
#' # Compute normal distributions from an atomic vector
#' 1:10 |>
#'   map(rnorm, n = 10)
#'
#' # You can also use an anonymous function
#' 1:10 |>
#'   map(\(x) rnorm(10, x))
#'
#' # Simplify output to a vector instead of a list by computing the mean of the distributions
#' 1:10 |>
#'   map(rnorm, n = 10) |>  # output a list
#'   map_dbl(mean)           # output an atomic vector
#'
#' # Using set_names() with character vectors is handy to keep track
#' # of the original inputs:
#' set_names(c("foo", "bar")) |> map_chr(paste0, ":suffix")
#'
#' # Working with lists
#' favorite_desserts <- list(Sophia = "banana bread", Eliott = "pancakes", Karina = "chocolate cake")
#' favorite_desserts |> map_chr(\(food) paste(food, "rocks!"))
#'
#' # Extract by name or position
#' # .default specifies value for elements that are missing or NULL
#' l1 <- list(list(a = 1L), list(a = NULL, b = 2L), list(b = 3L))
#' l1 |> map("a", .default = "???")
#' l1 |> map_int("b", .default = NA)
#' l1 |> map_int(2, .default = NA)
#'
#' # Supply multiple values to index deeply into a list
#' l2 <- list(
#'   list(num = 1:3,     letters[1:3]),
#'   list(num = 101:103, letters[4:6]),
#'   list()
#' )
#' l2 |> map(c(2, 2))
#'
#' # Use a list to build an extractor that mixes numeric indices and names,
#' # and .default to provide a default value if the element does not exist
#' l2 |> map(list("num", 3))
#' l2 |> map_int(list("num", 3), .default = NA)
#'
#' # Working with data frames
#' # Use map_lgl(), map_dbl(), etc to return a vector instead of a list:
#' mtcars |> map_dbl(sum)
#'
#' # A more realistic example: split a data frame into pieces, fit a
#' # model to each piece, summarise and extract R^2
#' mtcars |>
#'   split(mtcars$cyl) |>
#'   map(\(df) lm(mpg ~ wt, data = df)) |>
#'   map(summary) |>
#'   map_dbl("r.squared")
#'
#' @examplesIf interactive() && rlang::is_installed("mirai") && rlang::is_installed("carrier")
#' # Run in interactive sessions only as spawns additional processes
#'
#' # To use parallelized map:
#' # 1. Set daemons (number of parallel processes) first:
#' mirai::daemons(2)
#'
#' # 2. Wrap .f with in_parallel():
#' mtcars |> map_dbl(in_parallel(\(x) mean(x)))
#'
#' # Note that functions from packages should be fully qualified with `pkg::`
#' # or call `library(pkg)` within the function
#' 1:10 |>
#'   map(in_parallel(\(x) vctrs::vec_init(integer(), x))) |>
#'   map_int(in_parallel(\(x) { library(vctrs); vec_size(x) }))
#'
#' # A locally-defined function (or any required variables)
#' # should be passed via ... of in_parallel():
#' slow_lm <- function(formula, data) {
#'   Sys.sleep(0.5)
#'   lm(formula, data)
#' }
#'
#' mtcars |>
#'   split(mtcars$cyl) |>
#'   map(in_parallel(\(df) slow_lm(mpg ~ disp, data = df), slow_lm = slow_lm))
#'
#' # Tear down daemons when no longer in use:
#' mirai::daemons(0)
#'
map <- function(.x, .f, ..., .progress = FALSE) {
  map_("list", .x, .f, ..., .progress = .progress)
}

#' @rdname map
#' @export
map_lgl <- function(.x, .f, ..., .progress = FALSE) {
  map_("logical", .x, .f, ..., .progress = .progress)
}

#' @rdname map
#' @export
map_int <- function(.x, .f, ..., .progress = FALSE) {
  map_("integer", .x, .f, ..., .progress = .progress)
}

#' @rdname map
#' @export
map_dbl <- function(.x, .f, ..., .progress = FALSE) {
  map_("double", .x, .f, ..., .progress = .progress)
}

#' @rdname map
#' @export
map_chr <- function(.x, .f, ..., .progress = FALSE) {
  map_("character", .x, .f, ..., .progress = .progress)
}

map_ <- function(
  .type,
  .x,
  .f,
  ...,
  .progress = FALSE,
  .purrr_user_env = caller_env(2),
  .purrr_error_call = caller_env()
) {
  .progress <- as_progress(
    .progress,
    user_env = .purrr_user_env,
    caller_env = .purrr_error_call
  )

  .x <- vctrs_vec_compat(.x, .purrr_user_env)
  vec_assert(.x, arg = ".x", call = .purrr_error_call)

  if (running_in_parallel(.f)) {
    return(mmap_(.x, .f, .progress, .type, .purrr_error_call, ...))
  }

  .f <- as_mapper(.f, ...)

  n <- vec_size(.x)
  names <- vec_names(.x)

  i <- 0L
  with_indexed_errors(
    i = i,
    names = names,
    error_call = .purrr_error_call,
    call_with_cleanup(map_impl, environment(), .type, .progress, n, names, i)
  )
}

mmap_ <- function(.x, .f, .progress, .type, error_call, ...) {
  if (...length()) {
    cli::cli_abort(
      "Can't use `...` with parallelized functions.",
      call = error_call
    )
  }

  m <- mirai::mirai_map(.x, .f)

  options <- if (isFALSE(.progress)) {
    ".stop"
  } else if (is.logical(.progress)) {
    c(".stop", ".progress")
  } else if (is.character(.progress) || is.list(.progress)) {
    list(.stop = TRUE, .progress = .progress)
  } else {
    cli::cli_abort(
      "Unknown cli progress bar configuation, see manual.",
      call = error_call
    )
  }
  x <- with_parallel_indexed_errors(
    mirai::collect_mirai(m, options = options),
    interrupt_expr = mirai::stop_mirai(m),
    error_call = error_call
  )
  if (.type != "list") {
    x <- simplify_impl(x, ptype = vector(mode = .type), error_call = error_call)
  }
  x
}

#' @rdname map
#' @param .ptype If `NULL`, the default, the output type is the common type
#'   of the elements of the result. Otherwise, supply a "prototype" giving
#'   the desired type of output.
#' @export
map_vec <- function(.x, .f, ..., .ptype = NULL, .progress = FALSE) {
  out <- map(.x, .f, ..., .progress = .progress)
  simplify_impl(out, ptype = .ptype)
}

#' @rdname map
#' @export
walk <- function(.x, .f, ..., .progress = FALSE) {
  map(.x, .f, ..., .progress = .progress)
  invisible(.x)
}

with_indexed_errors <- function(
  expr,
  i,
  names = NULL,
  error_call = caller_env()
) {
  withCallingHandlers(
    expr,
    error = function(cnd) {
      if (i == 0L) {
        # Error happened before or after loop
      } else {
        message <- c(i = "In index: {i}.")
        if (!is.null(names) && !is.na(names[[i]]) && names[[i]] != "") {
          name <- names[[i]]
          message <- c(message, i = "With name: {name}.")
        } else {
          name <- NULL
        }

        cli::cli_abort(
          message,
          location = i,
          name = name,
          parent = cnd,
          call = error_call,
          class = "purrr_error_indexed"
        )
      }
    }
  )
}

with_parallel_indexed_errors <- function(
  expr,
  interrupt_expr = NULL,
  error_call = caller_env()
) {
  withCallingHandlers(
    expr,
    error = function(cnd) {
      location <- cnd$location
      iname <- cnd$name
      cli::cli_abort(
        c(
          i = "In index: {location}.",
          i = if (length(iname) && nzchar(iname)) "With name: {iname}."
        ),
        location = location,
        name = iname,
        parent = cnd$parent,
        call = error_call,
        class = "purrr_error_indexed"
      )
    },
    interrupt = function(cnd) {
      interrupt_expr
    }
  )
}

#' Indexed errors (`purrr_error_indexed`)
#'
#' @description
#'
#' ```{r, child = "man/rmd/indexed-error.Rmd"}
#' ```
#'
#' @keywords internal
#' @name purrr_error_indexed
NULL


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/R/map2.R ---
#' Map over two inputs
#'
#' @description
#' These functions are variants of [map()] that iterate over two arguments at
#' a time.
#'
#' @param .x,.y A pair of vectors, usually the same length. If not, a vector
#'   of length 1 will be recycled to the length of the other.
#' @param .f A function, specified in one of the following ways:
#'
#'   * A named function.
#'   * An anonymous function, e.g. `\(x, y) x + y` or `function(x, y) x + y`.
#'   * A formula, e.g. `~ .x + .y`. Use `.x` to refer to the current
#'     element of `x` and `.y` to refer to the current element of `y`.
#'     No longer recommended.
#'
#'   `r lifecycle::badge("experimental")`
#'
#'   Wrap a function with [in_parallel()] to declare that it should be performed
#'   in parallel. See [in_parallel()] for more details.
#'   Use of `...` is not permitted in this context.
#' @inheritParams map
#' @inherit map return
#' @family map variants
#' @export
#' @examples
#' x <- list(1, 1, 1)
#' y <- list(10, 20, 30)
#'
#' map2(x, y, \(x, y) x + y)
#' # Or just
#' map2(x, y, `+`)
#'
#' # Split into pieces, fit model to each piece, then predict
#' by_cyl <- mtcars |> split(mtcars$cyl)
#' mods <- by_cyl |> map(\(df) lm(mpg ~ wt, data = df))
#' map2(mods, by_cyl, predict)
map2 <- function(.x, .y, .f, ..., .progress = FALSE) {
  map2_("list", .x, .y, .f, ..., .progress = .progress)
}
#' @export
#' @rdname map2
map2_lgl <- function(.x, .y, .f, ..., .progress = FALSE) {
  map2_("logical", .x, .y, .f, ..., .progress = .progress)
}
#' @export
#' @rdname map2
map2_int <- function(.x, .y, .f, ..., .progress = FALSE) {
  map2_("integer", .x, .y, .f, ..., .progress = .progress)
}
#' @export
#' @rdname map2
map2_dbl <- function(.x, .y, .f, ..., .progress = FALSE) {
  map2_("double", .x, .y, .f, ..., .progress = .progress)
}
#' @export
#' @rdname map2
map2_chr <- function(.x, .y, .f, ..., .progress = FALSE) {
  map2_("character", .x, .y, .f, ..., .progress = .progress)
}

map2_ <- function(
  .type,
  .x,
  .y,
  .f,
  ...,
  .progress = FALSE,
  .purrr_user_env = caller_env(2),
  .purrr_error_call = caller_env()
) {
  .progress <- as_progress(
    .progress,
    user_env = .purrr_user_env,
    caller_env = .purrr_error_call
  )

  .x <- vctrs_vec_compat(.x, .purrr_user_env)
  .y <- vctrs_vec_compat(.y, .purrr_user_env)

  n <- vec_size_common(.x = .x, .y = .y, .call = .purrr_error_call)
  args <- vec_recycle_common(
    .x = .x,
    .y = .y,
    .size = n,
    .call = .purrr_error_call
  )
  .x <- args$.x
  .y <- args$.y

  names <- vec_names(.x)

  .f <- as_mapper(.f, ...)

  if (running_in_parallel(.f)) {
    attributes(args) <- list(
      class = "data.frame",
      row.names = if (is.null(names)) .set_row_names(n) else names
    )
    return(mmap_(args, .f, .progress, .type, .purrr_error_call, ...))
  }

  i <- 0L
  with_indexed_errors(
    i = i,
    names = names,
    error_call = .purrr_error_call,
    call_with_cleanup(map2_impl, environment(), .type, .progress, n, names, i)
  )
}

#' @rdname map2
#' @export
map2_vec <- function(.x, .y, .f, ..., .ptype = NULL, .progress = FALSE) {
  out <- map2(.x, .y, .f, ..., .progress = .progress)
  simplify_impl(out, ptype = .ptype)
}

#' @export
#' @rdname map2
walk2 <- function(.x, .y, .f, ..., .progress = FALSE) {
  map2(.x, .y, .f, ..., .progress = .progress)
  invisible(.x)
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/R/modify-tree.R ---
#' Recursively modify a list
#'
#' `modify_tree()` allows you to recursively modify a list, supplying functions
#' that either modify each leaf or each node (or both).
#'
#' @param x A list.
#' @param ... Reserved for future use. Must be empty
#' @param leaf A function applied to each leaf.
#' @param is_node A predicate function that determines whether an element is
#'   a node (by returning `TRUE`) or a leaf (by returning `FALSE`). The
#'   default value, `NULL`, treats simple lists as nodes and everything else
#'   (including richer objects like data frames and linear models) as leaves,
#'   using [vctrs::obj_is_list()]. To recurse into all objects built on lists
#'   use [is.list()].
#' @param pre,post Functions applied to each node. `pre` is applied on the
#'   way "down", i.e. before the leaves are transformed with `leaf`, while
#'   `post` is applied on the way "up", i.e. after the leaves are transformed.
#' @family modify variants
#' @export
#' @examples
#' x <- list(list(a = 2:1, c = list(b1 = 2), b = list(c2 = 3, c1 = 4)))
#' x |> str()
#'
#' # Transform each leaf
#' x |> modify_tree(leaf = \(x) x + 100) |>  str()
#'
#' # Recursively sort the nodes
#' sort_named <- function(x) {
#'   nms <- names(x)
#'   if (!is.null(nms)) {
#'     x[order(nms)]
#'   } else {
#'     x
#'    }
#' }
#' x |> modify_tree(post = sort_named) |> str()
modify_tree <- function(
  x,
  ...,
  leaf = identity,
  is_node = NULL,
  pre = identity,
  post = identity
) {
  check_dots_empty()
  leaf <- rlang::as_function(leaf)
  is_node <- as_is_node(is_node)
  post <- rlang::as_function(post)
  pre <- rlang::as_function(pre)

  worker <- function(x) {
    if (is_node(x)) {
      out <- pre(x)
      out <- modify(out, worker)
      out <- post(out)
    } else {
      out <- leaf(x)
    }
    out
  }

  worker(x)
}

as_is_node <- function(
  f,
  error_call = caller_env(),
  error_arg = caller_arg(f)
) {
  if (is.null(f)) {
    obj_is_list
  } else {
    is_node_f <- rlang::as_function(f, call = error_call, arg = error_arg)
    as_predicate(
      is_node_f,
      .mapper = FALSE,
      .purrr_error_call = error_call,
      .purrr_error_arg = error_arg
    )
  }
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/R/modify.R ---
#' Modify elements selectively
#'
#' @description
#'
#' Unlike [map()] and its variants which always return a fixed object
#' type (list for `map()`, integer vector for `map_int()`, etc), the
#' `modify()` family always returns the same type as the input object.
#'
#' * `modify()` is a shortcut for `x[[i]] <- f(x[[i]]); return(x)`.
#'
#' * `modify_if()` only modifies the elements of `x` that satisfy a
#'   predicate and leaves the others unchanged. `modify_at()` only
#'   modifies elements given by names or positions.
#'
#' * `modify2()` modifies the elements of `.x` but also passes the
#'   elements of `.y` to `.f`, just like [map2()]. `imodify()` passes
#'   the names or the indices to `.f` like [imap()] does.
#'
#' * [modify_in()] modifies a single element in a [pluck()] location.
#'
#' @param .x A vector.
#' @param .y A vector, usually the same length as `.x`.
#' @inheritParams map2
#' @inheritParams map
#' @param .f A function specified in the same way as the corresponding map
#'   function.
#' @return An object the same class as `.x`
#'
#' @details
#'
#' Since the transformation can alter the structure of the input; it's
#' your responsibility to ensure that the transformation produces a
#' valid output. For example, if you're modifying a data frame, `.f`
#' must preserve the length of the input.
#'
#' @section Genericity:
#'
#' `modify()` and variants are generic over classes that implement
#' `length()`, `[[` and `[[<-` methods. If the default implementation
#' is not compatible for your class, you can override them with your
#' own methods.
#'
#' If you implement your own `modify()` method, make sure it satisfies
#' the following invariants:
#'
#' ```
#' modify(x, identity) === x
#' modify(x, compose(f, g)) === modify(x, g) |> modify(f)
#' ```
#'
#' These invariants are known as the [functor
#' laws](https://wiki.haskell.org/Functor#Functor_Laws) in computer
#' science.
#'
#'
#' @family map variants
#' @family modify variants
#' @examples
#' # Convert factors to characters
#' iris |>
#'   modify_if(is.factor, as.character) |>
#'   str()
#'
#' # Specify which columns to map with a numeric vector of positions:
#' mtcars |> modify_at(c(1, 4, 5), as.character) |> str()
#'
#' # Or with a vector of names:
#' mtcars |> modify_at(c("cyl", "am"), as.character) |> str()
#'
#' list(x = sample(c(TRUE, FALSE), 100, replace = TRUE), y = 1:100) |>
#'   list_transpose(simplify = FALSE) |>
#'   modify_if("x", \(l) list(x = l$x, y = l$y * 100)) |>
#'   list_transpose()
#'
#' # Use modify2() to map over two vectors and preserve the type of
#' # the first one:
#' x <- c(foo = 1L, bar = 2L)
#' y <- c(TRUE, FALSE)
#' modify2(x, y, \(x, cond) if (cond) x else 0L)
#'
#' # Use a predicate function to decide whether to map a function:
#' modify_if(iris, is.factor, as.character)
#'
#' # Specify an alternative with the `.else` argument:
#' modify_if(iris, is.factor, as.character, .else = as.integer)
#' @export
modify <- function(.x, .f, ...) {
  .f <- as_mapper(.f, ...)

  if (obj_is_list(.x)) {
    out <- map(vec_proxy(.x), .f, ...)
    vec_restore(out, .x)
  } else if (is.data.frame(.x)) {
    size <- vec_size(.x)
    out <- unclass(vec_proxy(.x))
    out <- map(out, .f, ...)
    out <- vec_recycle_common(!!!out, .size = size, .arg = "out")
    out <- new_data_frame(out, n = size)
    vec_restore(out, .x)
  } else if (vec_is(.x)) {
    map_vec(.x, .f, ..., .ptype = .x)
  } else if (is.list(.x) || is.null(.x)) {
    .x[] <- map(.x, .f, ...)
    .x
  } else {
    cli::cli_abort(
      "{.arg .x} must be a vector, list, or data frame, not {.obj_type_friendly {.x}}."
    )
  }
}

#' @rdname modify
#' @inheritParams map_if
#' @export
modify_if <- function(.x, .p, .f, ..., .else = NULL) {
  where <- where_if(.x, .p)
  .x <- modify_where(.x, where, .f, ...)

  if (!is.null(.else)) {
    .else <- as_mapper(.else, ...)
    .x <- modify_where(.x, !where, .else, ...)
  }

  .x
}

#' @rdname modify
#' @inheritParams map_at
#' @export
modify_at <- function(.x, .at, .f, ...) {
  where <- where_at(.x, .at, user_env = caller_env())
  modify_where(.x, where, .f, ...)
}

#' @rdname modify
#' @export
modify2 <- function(.x, .y, .f, ...) {
  .f <- as_mapper(.f, ...)

  if (obj_is_list(.x)) {
    out <- map2(vec_proxy(.x), .y, .f, ...)
    vec_restore(out, .x)
  } else if (is.data.frame(.x)) {
    size <- vec_size(.x)
    out <- unclass(vec_proxy(.x))
    out <- map2(out, .y, .f, ...)
    out <- vec_recycle_common(!!!out, .size = size, .arg = "out")
    out <- new_data_frame(out, n = size)
    vec_restore(out, .x)
  } else if (vec_is(.x)) {
    map2_vec(.x, .y, .f, ..., .ptype = .x)
  } else if (is.null(.x) || is.list(.x)) {
    out <- map2(.x, .y, .f, ...)
    if (length(out) > length(.x)) {
      .x <- .x[rep(1L, length(out))]
    }
    .x[] <- out
    .x
  } else {
    cli::cli_abort(
      "{.arg .x} must be a vector, list, or data frame, not {.obj_type_friendly {.x}}."
    )
  }
}

#' @rdname modify
#' @export
imodify <- function(.x, .f, ...) {
  modify2(.x, vec_index(.x), .f, ...)
}

# helpers -----------------------------------------------------------------

modify_where <- function(
  .x,
  .where,
  .f,
  ...,
  .purrr_error_call = caller_env()
) {
  if (obj_is_list(.x)) {
    out <- vec_proxy(.x)
    out[.where] <- no_zap(map(out[.where], .f, ...), .purrr_error_call)
    vec_restore(out, .x)
  } else if (is.data.frame(.x)) {
    size <- vec_size(.x)
    out <- unclass(vec_proxy(.x))
    new <- no_zap(map(out[.where], .f, ...), .purrr_error_call)
    out[.where] <- vec_recycle_common(!!!new, .size = size, .arg = "out")
    out <- new_data_frame(out, n = size)
    vec_restore(out, .x)
  } else if (vec_is(.x)) {
    .x[.where] <- map_vec(.x[.where], .f, ..., .ptype = .x)
    .x
  } else if (is.null(.x) || is.list(.x)) {
    .x[.where] <- no_zap(map(.x[.where], .f, ...), .purrr_error_call)
    .x
  } else {
    cli::cli_abort(
      "{.arg .x} must be a vector, list, or data frame, not {.obj_type_friendly {.x}}.",
      call = .purrr_error_call
    )
  }
}

no_zap <- function(x, error_call) {
  has_zap <- some(x, is_zap)
  if (!has_zap) {
    x
  } else {
    cli::cli_abort(
      "Can't use {.fn zap} to change the size of the output.",
      call = error_call
    )
  }
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/R/package-purrr.R ---
#' @keywords internal
#' @import rlang
#' @import vctrs
#' @importFrom cli cli_progress_bar
#' @importFrom lifecycle deprecated
#' @useDynLib purrr, .registration = TRUE
"_PACKAGE"

the <- new_environment()


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/R/parallelization.R ---
#' Parallelization in purrr
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' All map functions allow parallelized operation using \CRANpkg{mirai}.
#'
#' Wrap functions passed to the `.f` argument of [map()] and its variants with
#' [in_parallel()].
#'
#' [in_parallel()] is a \pkg{purrr} adverb that plays two roles:
#'  * It is a signal to purrr verbs like [map()] to go ahead and perform
#'    computations in parallel.
#'  * It helps you create self-contained functions that are isolated from your
#'    workspace. This is important because the function is packaged up
#'    (serialized) to be sent across to parallel processes. Isolation is
#'    critical for performance because it prevents accidentally sending very
#'    large objects between processes.
#'
#' For maps to actually be performed in parallel, the user must also set
#' [mirai::daemons()], otherwise they fall back to sequential processing.
#' [mirai::require_daemons()] may be used to enforce the use of parallel
#' processing. See the section 'Daemons settings' below.
#'
#' @param .f A fresh formula or function. "Fresh" here means that they should be
#'   declared in the call to [in_parallel()].
#' @param ... Named arguments to declare in the environment of the function.
#'
#' @return A 'crate' (classed function).
#'
#' @section Creating self-contained functions:
#'
#' * They should call package functions with an explicit `::` namespace. For
#'   instance `ggplot()` from the ggplot2 package must be called with its
#'   namespace prefix: `ggplot2::ggplot()`. An alternative is to use `library()`
#'   within the function to attach a package to the search path, which allows
#'   subsequent use of package functions without the explicit namespace.
#'
#' * They should declare any data they depend on. Declare data by supplying
#'   named arguments to `...`. When `.f` is an anonymous function to a
#'   locally-defined function of the form `\(x) fun(x)`, `fun` itself must be
#'   supplied to `...` in the manner of: `in_parallel(\(x) fun(x), fun = fun)`.
#'
#' * Functions (closures) supplied to `...` must themselves be self-contained,
#'   as they are modified to share the same closure as the main function. This
#'   means that all helper functions and other required variables must also be
#'   supplied as further `...` arguments. This applies only for functions
#'   directly supplied to `...`: containers (such as lists) are not
#'   recursively analysed. In other words, if you supply complex
#'   objects to `...` you're at risk of unexpectedly including large objects.
#'
#' [in_parallel()] is a simple wrapper of [carrier::crate()] and you may refer
#' to that package for more details.
#'
#' Example usage:
#' ```r
#' # The function needs to be freshly-defined, so instead of:
#' mtcars |> map_dbl(in_parallel(sum))
#' # Use an anonymous function:
#' mtcars |> map_dbl(in_parallel(\(x) sum(x)))
#'
#' # Package functions need to be explicitly namespaced, so instead of:
#' map(1:3, in_parallel(\(x) vec_init(integer(), x)))
#' # Use :: to namespace all package functions:
#' map(1:3, in_parallel(\(x) vctrs::vec_init(integer(), x)))
#'
#' fun <- function(x) { param + helper(x) }
#' helper <- function(x) { x %% 2 }
#' param <- 5
#' # Operating in parallel, locally-defined functions, including helper
#' # functions and other objects required by it, will not be found:
#' map(1:3, in_parallel(\(x) fun(x)))
#' # Use the ... argument to supply these objects:
#' map(1:3, in_parallel(\(x) fun(x), fun = fun, helper = helper, param = param))
#' ```
#'
#' @section When to use:
#'
#' Parallelizing a map using 'n' processes does not automatically lead to it
#' taking 1/n of the time. Additional overhead from setting up the parallel task
#' and communicating with parallel processes eats into this benefit, and can
#' outweigh it for very short tasks or those involving large amounts of data.
#'
#' The threshold at which parallelization becomes clearly beneficial will differ
#' according to your individual setup and task, but a rough guide would be in
#' the order of 100 microseconds to 1 millisecond for each map iteration.
#'
#' @section Daemons settings:
#'
#' How and where parallelization occurs is determined by [mirai::daemons()].
#' This is a function from the \pkg{mirai} package that sets up daemons
#' (persistent background processes that receive parallel computations) on your
#' local machine or across the network.
#'
#' Daemons must be set prior to performing any parallel map operation, otherwise
#' [in_parallel()] will fall back to sequential processing. To ensure that maps
#' are always performed in parallel, place [mirai::require_daemons()] before the
#' map.
#'
#' It is usual to set daemons once per session. You can leave them running on
#' your local machine as they consume almost no resources whilst waiting to
#' receive tasks. The following sets up 6 daemons locally:
#'
#' ```r
#' mirai::daemons(6)
#' ```
#'
#' Function arguments:
#'
#' * `n`: the number of daemons to launch on your local machine, e.g.
#'   `mirai::daemons(6)`. As a rule of thumb, for maximum efficiency this should
#'   be (at most) one less than the number of cores on your machine, leaving one
#'   core for the main R process.
#' * `url` and `remote`: used to set up and launch daemons for distributed
#'   computing over the network. See [mirai::daemons()] documentation for more
#'   details.
#'
#' Resetting daemons:
#'
#' Daemons persist for the duration of your session. To reset and tear down any
#' existing daemons:
#'
#' ```r
#' mirai::daemons(0)
#' ```
#'
#' All daemons automatically terminate when your session ends. You do not need
#' to explicitly terminate daemons in this instance, although it is still good
#' practice to do so.
#'
#' Note: if you are using parallel map within a package, do not make any
#' [mirai::daemons()] calls within your package. It should always be
#' up to the user how they wish to set up parallel processing: (i) resources are
#' only known at run-time e.g. availability of local or remote daemons, (ii)
#' packages should make use of existing daemons when already set, rather than
#' reset them, and (iii) it helps prevent inadvertently spawning too many
#' daemons when functions are used recursively within each other.
#'
#' @references
#'
#' \pkg{purrr}'s parallelization is powered by \CRANpkg{mirai}. See the
#' [mirai website](https://mirai.r-lib.org/) for more details.
#'
#' @seealso [map()] for usage examples.
#' @aliases parallelization
#' @export
#' @examplesIf interactive() && rlang::is_installed("mirai") && rlang::is_installed("carrier")
#' # Run in interactive sessions only as spawns additional processes
#'
#' default_param <- 0.5
#'
#' delay <- function(secs = default_param) {
#'   Sys.sleep(secs)
#' }
#'
#' slow_lm <- function(formula, data) {
#'   delay()
#'   lm(formula, data)
#' }
#'
#' # Example of a 'crate' returned by in_parallel(). The object print method
#' # shows the size of the crate and any objects contained within:
#' crate <- in_parallel(
#'   \(df) slow_lm(mpg ~ disp, data = df),
#'   slow_lm = slow_lm,
#'   delay = delay,
#'   default_param = default_param
#' )
#' crate
#'
#' # Use mirai::mirai() to test that a crate is self-contained
#' # by running it in a daemon and collecting its return value:
#' mirai::mirai(crate(mtcars), crate = crate) |> mirai::collect_mirai()
#'
in_parallel <- function(.f, ...) {
  parallel_pkgs_installed()
  inject(
    carrier::crate(
      !!substitute(.f),
      !!!list(...),
      .parent_env = globalenv(),
      .error_arg = ".f",
      .error_call = environment()
    )
  )
}

running_in_parallel <- function(x) {
  inherits(x, "crate") && parallel_pkgs_installed() && mirai::daemons_set()
}

parallel_pkgs_installed <- function() {
  is.logical(the$parallel_pkgs_installed) ||
    {
      check_installed(
        c("carrier", "mirai"),
        version = c("0.3.0", "2.5.1"),
        reason = "for parallel map."
      )
      the$parallel_pkgs_installed <- TRUE
    }
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/R/pluck-assign.R ---
#' Modify a pluck location
#'
#' @description
#'
#' * `assign_in()` takes a data structure and a [pluck] location,
#'   assigns a value there, and returns the modified data structure.
#'
#' * `modify_in()` applies a function to a pluck location, assigns the
#'   result back to that location with [assign_in()], and returns the
#'   modified data structure.
#'
#' @inheritParams pluck
#' @param .f A function to apply at the pluck location given by `.where`.
#' @param ... Arguments passed to `.f`.
#' @param .where,where A pluck location, as a numeric vector of
#'   positions, a character vector of names, or a list combining both.
#'   The location must exist in the data structure.
#' @seealso [pluck()]
#' @export
#' @examples
#' # Recall that pluck() returns a component of a data structure that
#' # might be arbitrarily deep
#' x <- list(list(bar = 1, foo = 2))
#' pluck(x, 1, "foo")
#'
#' # Use assign_in() to modify the pluck location:
#' str(assign_in(x, list(1, "foo"), 100))
#' # Or zap to remove it
#' str(assign_in(x, list(1, "foo"), zap()))
#'
#' # Like pluck(), this works even when the element (or its parents) don't exist
#' pluck(x, 1, "baz")
#' str(assign_in(x, list(2, "baz"), 100))
#'
#' # modify_in() applies a function to that location and update the
#' # element in place:
#' modify_in(x, list(1, "foo"), \(x) x * 200)
#'
#' # Additional arguments are passed to the function in the ordinary way:
#' modify_in(x, list(1, "foo"), `+`, 100)
modify_in <- function(.x, .where, .f, ...) {
  .where <- as.list(.where)
  .f <- rlang::as_function(.f)

  value <- .f(pluck(.x, !!!.where), ...)
  assign_in(.x, .where, value)
}
#' @rdname modify_in
#' @param value A value to replace in `.x` at the pluck location.
#'   Use `zap()` to instead remove the element.
#' @export
assign_in <- function(x, where, value) {
  n <- length(where)
  if (n == 0) {
    cli::cli_abort(
      "{.arg where} must contain at least one element.",
      arg = "where"
    )
  } else if (n > 1) {
    old <- pluck(x, where[[1]], .default = list())
    if (!is_zap(value) || !identical(old, list())) {
      value <- assign_in(old, where[-1], value)
    }
  }

  if (is_zap(value)) {
    x[[where[[1]]]] <- NULL
  } else {
    list_slice2(x, where[[1]]) <- value
  }

  x
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/R/pluck-depth.R ---
#' Compute the depth of a vector
#'
#' The depth of a vector is how many levels that you can index/pluck into it.
#' `pluck_depth()` was previously called `vec_depth()`.
#'
#' @param x A vector
#' @param is_node Optionally override the default criteria for determine an
#'   element can be recursed within. The default matches the behaviour of
#'   `pluck()` which can recurse into lists and expressions.
#' @return An integer.
#' @export
#' @examples
#' x <- list(
#'   list(),
#'   list(list()),
#'   list(list(list(1)))
#' )
#' pluck_depth(x)
#' x |> map_int(pluck_depth)
pluck_depth <- function(x, is_node = NULL) {
  if (is.null(is_node)) {
    is_node <- function(x) is.expression(x) || is.list(x)
  }
  is_node <- as_is_node(is_node)

  if (is_node(x)) {
    depths <- map_int(x, pluck_depth, is_node = is_node)
    1L + max(depths, 0L)
  } else if (is_atomic(x)) {
    1L
  } else {
    0L
  }
}

#' @export
#' @rdname pluck_depth
#' @usage NULL
vec_depth <- function(x) {
  lifecycle::deprecate_warn("1.0.0", "vec_depth()", "pluck_depth()")
  pluck_depth(x)
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/R/pluck.R ---
#' Safely get or set an element deep within a nested data structure
#'
#' @description
#' `pluck()` implements a generalised form of `[[` that allow you to index
#' deeply and flexibly into data structures. (If you're looking for an
#' equivalent of `[`, see [keep_at()].) `pluck()` always succeeds, returning
#' `.default` if the index you are trying to access does not exist or is `NULL`.
#' (If you're looking for a variant that errors, try [chuck()].)
#'
#' `pluck<-()` is the assignment equivalent, allowing you to modify an object
#' deep within a nested data structure.
#'
#' `pluck_exists()` tells you whether or not an object exists using the
#' same rules as pluck (i.e. a `NULL` element is equivalent to an absent
#' element).
#'
#' @param .x,x A vector or environment
#' @param ... A list of accessors for indexing into the object. Can be
#'   an positive integer, a negative integer (to index from the right),
#'   a string (to index into names), or an accessor function
#'   (except for the assignment variants which only support names and
#'   positions). If the object being indexed is an S4 object,
#'   accessing it by name will return the corresponding slot.
#'
#'   [Dynamic dots][rlang::dyn-dots] are supported. In particular, if
#'   your accessors are stored in a list, you can splice that in with
#'   `!!!`.
#' @param .default Value to use if target is `NULL` or absent.
#'
#' @details
#' * You can pluck or chuck with standard accessors like integer
#'   positions and string names, and also accepts arbitrary accessor
#'   functions, i.e. functions that take an object and return some
#'   internal piece.
#'
#'   This is often more readable than a mix of operators and accessors
#'   because it reads linearly and is free of syntactic
#'   cruft. Compare: \code{accessor(x[[1]])$foo} to `pluck(x, 1,
#'   accessor, "foo")`.
#'
#' * These accessors never partial-match. This is unlike `$` which
#'   will select the `disp` object if you write `mtcars$di`.
#'
#' @seealso
#' * [attr_getter()] for creating attribute getters suitable for use
#'   with `pluck()` and `chuck()`.
#' * [modify_in()] for applying a function to a plucked location.
#' * [keep_at()] is similar to `pluck()`, but retain the structure
#'   of the list instead of converting it into a vector.
#' @export
#' @examples
#' # Let's create a list of data structures:
#' obj1 <- list("a", list(1, elt = "foo"))
#' obj2 <- list("b", list(2, elt = "bar"))
#' x <- list(obj1, obj2)
#'
#' # pluck() provides a way of retrieving objects from such data
#' # structures using a combination of numeric positions, vector or
#' # list names, and accessor functions.
#'
#' # Numeric positions index into the list by position, just like `[[`:
#' pluck(x, 1)
#' # same as x[[1]]
#'
#' # Index from the back
#' pluck(x, -1)
#' # same as x[[2]]
#'
#' pluck(x, 1, 2)
#' # same as x[[1]][[2]]
#'
#' # Supply names to index into named vectors:
#' pluck(x, 1, 2, "elt")
#' # same as x[[1]][[2]][["elt"]]
#'
#' # By default, pluck() consistently returns `NULL` when an element
#' # does not exist:
#' pluck(x, 10)
#' try(x[[10]])
#'
#' # You can also supply a default value for non-existing elements:
#' pluck(x, 10, .default = NA)
#'
#' # The map() functions use pluck() by default to retrieve multiple
#' # values from a list:
#' map_chr(x, 1)
#' map_int(x, c(2, 1))
#'
#' # pluck() also supports accessor functions:
#' my_element <- function(x) x[[2]]$elt
#' pluck(x, 1, my_element)
#' pluck(x, 2, my_element)
#'
#' # Even for this simple data structure, this is more readable than
#' # the alternative form because it requires you to read both from
#' # right-to-left and from left-to-right in different parts of the
#' # expression:
#' my_element(x[[1]])
#'
#' # If you have a list of accessors, you can splice those in with `!!!`:
#' idx <- list(1, my_element)
#' pluck(x, !!!idx)
pluck <- function(.x, ..., .default = NULL) {
  check_dots_unnamed()
  pluck_raw(.x, list2(...), .default = .default)
}

#' @rdname pluck
#' @inheritParams modify_in
#' @export
`pluck<-` <- function(.x, ..., value) {
  assign_in(.x, list2(...), value)
}

#' @rdname pluck
#' @export
pluck_exists <- function(.x, ...) {
  check_dots_unnamed()

  !is_zap(pluck_raw(.x, list2(...), .default = zap()))
}

pluck_raw <- function(.x, index, .default = NULL) {
  .Call(
    pluck_impl,
    x = .x,
    index = index,
    missing = .default,
    strict = FALSE
  )
}

#' Get an element deep within a nested data structure, failing if it doesn't
#' exist
#'
#' `chuck()` implements a generalised form of `[[` that allow you to index
#' deeply and flexibly into data structures. If the index you are trying to
#' access does not exist (or is `NULL`), it will throw (i.e. chuck) an error.
#'
#' @seealso [pluck()] for a quiet equivalent.
#' @inheritParams pluck
#' @export
#' @examples
#' x <- list(a = 1, b = 2)
#'
#' # When indexing an element that doesn't exist `[[` sometimes returns NULL:
#' x[["y"]]
#' # and sometimes errors:
#' try(x[[3]])
#'
#' # chuck() consistently errors:
#' try(chuck(x, "y"))
#' try(chuck(x, 3))
chuck <- function(.x, ...) {
  check_dots_unnamed()

  .Call(
    pluck_impl,
    x = .x,
    index = list2(...),
    missing = NULL,
    strict = TRUE
  )
}

#' Create an attribute getter function
#'
#' `attr_getter()` generates an attribute accessor function; i.e., it
#' generates a function for extracting an attribute with a given
#' name. Unlike the base R `attr()` function with default options, it
#' doesn't use partial matching.
#'
#' @param attr An attribute name as string.
#'
#' @seealso [pluck()]
#' @examples
#' # attr_getter() takes an attribute name and returns a function to
#' # access the attribute:
#' get_rownames <- attr_getter("row.names")
#' get_rownames(mtcars)
#'
#' # These getter functions are handy in conjunction with pluck() for
#' # extracting deeply into a data structure. Here we'll first
#' # extract by position, then by attribute:
#' obj1 <- structure("obj", obj_attr = "foo")
#' obj2 <- structure("obj", obj_attr = "bar")
#' x <- list(obj1, obj2)
#'
#' pluck(x, 1, attr_getter("obj_attr"))  # From first object
#' pluck(x, 2, attr_getter("obj_attr"))  # From second object
#' @export
attr_getter <- function(attr) {
  force(attr)
  function(x) attr(x, attr, exact = TRUE)
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/R/pmap.R ---
#' Map over multiple input simultaneously (in "parallel")
#'
#' @description
#' These functions are variants of [map()] that iterate over multiple arguments
#' simultaneously. They are parallel in the sense that each input is processed
#' in parallel with the others, not in the sense of multicore computing, i.e.
#' they share the same notion of "parallel" as [base::pmax()] and [base::pmin()].
#'
#' @param .l A list of vectors. The length of `.l` determines the number of
#'   arguments that `.f` will be called with. Arguments will be supply by
#'   position if unnamed, and by name if named.
#'
#'   Vectors of length 1 will be recycled to any length; all other elements
#'   must be have the same length.
#'
#'   A data frame is an important special case of `.l`. It will cause `.f`
#'   to be called once for each row.
#' @param .f A function, specified in one of the following ways:
#'
#'   * A named function.
#'   * An anonymous function, e.g. `\(x, y, z) x + y / z` or
#'     `function(x, y, z) x + y / z`
#'   * A formula, e.g. `~ ..1 + ..2 / ..3`. No longer recommended.
#'
#'   `r lifecycle::badge("experimental")`
#'
#'   Wrap a function with [in_parallel()] to declare that it should be performed
#'   in parallel. See [in_parallel()] for more details.
#'   Use of `...` is not permitted in this context.
#' @inheritParams map
#' @returns
#' The output length is determined by the maximum length of all elements of `.l`.
#' The output names are determined by the names of the first element of `.l`.
#' The output type is determined by the suffix:
#'
#' * No suffix: a list; `.f()` can return anything.
#'
#' * `_lgl()`, `_int()`, `_dbl()`, `_chr()` return a logical, integer, double,
#'   or character vector respectively; `.f()` must return a compatible atomic
#'   vector of length 1.
#'
#' * `_vec()` return an atomic or S3 vector, the same type that `.f` returns.
#'   `.f` can return pretty much any type of vector, as long as it is length 1.
#'
#' * `pwalk()` returns the input `.l` (invisibly). This makes it easy to
#'    use in a pipe. The return value of `.f()` is ignored.
#'
#' Any errors thrown by `.f` will be wrapped in an error with class
#' [purrr_error_indexed].
#' @family map variants
#' @export
#' @examples
#' x <- list(1, 1, 1)
#' y <- list(10, 20, 30)
#' z <- list(100, 200, 300)
#' pmap(list(x, y, z), sum)
#'
#' # Matching arguments by position
#' pmap(list(x, y, z), function(first, second, third) (first + third) * second)
#'
#' # Matching arguments by name
#' l <- list(a = x, b = y, c = z)
#' pmap(l, function(c, b, a) (a + c) * b)
#'
#' # Vectorizing a function over multiple arguments
#' df <- data.frame(
#'   x = c("apple", "banana", "cherry"),
#'   pattern = c("p", "n", "h"),
#'   replacement = c("P", "N", "H"),
#'   stringsAsFactors = FALSE
#'   )
#' pmap(df, gsub)
#' pmap_chr(df, gsub)
#'
#' # Use `...` to absorb unused components of input list .l
#' df <- data.frame(
#'   x = 1:3,
#'   y = 10:12,
#'   z = letters[1:3]
#' )
#' plus <- function(x, y) x + y
#' \dontrun{
#' # this won't work
#' pmap(df, plus)
#' }
#' # but this will
#' plus2 <- function(x, y, ...) x + y
#' pmap_dbl(df, plus2)
#'
#' # The "p" for "parallel" in pmap() is the same as in base::pmin()
#' # and base::pmax()
#' df <- data.frame(
#'   x = c(1, 2, 5),
#'   y = c(5, 4, 8)
#' )
#' # all produce the same result
#' pmin(df$x, df$y)
#' map2_dbl(df$x, df$y, min)
#' pmap_dbl(df, min)
pmap <- function(.l, .f, ..., .progress = FALSE) {
  pmap_("list", .l, .f, ..., .progress = .progress)
}

#' @export
#' @rdname pmap
pmap_lgl <- function(.l, .f, ..., .progress = FALSE) {
  pmap_("logical", .l, .f, ..., .progress = .progress)
}
#' @export
#' @rdname pmap
pmap_int <- function(.l, .f, ..., .progress = FALSE) {
  pmap_("integer", .l, .f, ..., .progress = .progress)
}
#' @export
#' @rdname pmap
pmap_dbl <- function(.l, .f, ..., .progress = FALSE) {
  pmap_("double", .l, .f, ..., .progress = .progress)
}
#' @export
#' @rdname pmap
pmap_chr <- function(.l, .f, ..., .progress = FALSE) {
  pmap_("character", .l, .f, ..., .progress = .progress)
}

pmap_ <- function(
  .type,
  .l,
  .f,
  ...,
  .progress = FALSE,
  .purrr_user_env = caller_env(2),
  .purrr_error_call = caller_env()
) {
  .progress <- as_progress(
    .progress,
    user_env = .purrr_user_env,
    caller_env = .purrr_error_call
  )

  .l <- vctrs_list_compat(.l, error_call = .purrr_error_call)
  .l <- map(.l, vctrs_vec_compat)

  n <- vec_size_common(!!!.l, .arg = ".l", .call = .purrr_error_call)
  .l <- vec_recycle_common(
    !!!.l,
    .size = n,
    .arg = ".l",
    .call = .purrr_error_call
  )

  if (length(.l) > 0L) {
    names <- vec_names(.l[[1L]])
  } else {
    names <- NULL
  }

  .f <- as_mapper(.f, ...)

  if (running_in_parallel(.f)) {
    attributes(.l) <- list(
      names = names(.l),
      class = "data.frame",
      row.names = if (is.null(names)) .set_row_names(n) else names
    )
    return(mmap_(.l, .f, .progress, .type, .purrr_error_call, ...))
  }

  call_names <- names(.l)
  call_n <- length(.l)

  i <- 0L
  with_indexed_errors(
    i = i,
    names = names,
    error_call = .purrr_error_call,
    call_with_cleanup(
      pmap_impl,
      environment(),
      .type,
      .progress,
      n,
      names,
      i,
      call_names,
      call_n
    )
  )
}


#' @export
#' @rdname pmap
pmap_vec <- function(.l, .f, ..., .ptype = NULL, .progress = FALSE) {
  .f <- as_mapper(.f, ...)

  out <- pmap(.l, .f, ..., .progress = .progress)
  simplify_impl(out, ptype = .ptype)
}

#' @export
#' @rdname pmap
pwalk <- function(.l, .f, ..., .progress = FALSE) {
  pmap(.l, .f, ..., .progress = .progress)
  invisible(.l)
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/R/progress-bars.R ---
#' Progress bars in purrr
#'
#' @description
#' purrr's map functions have a `.progress` argument that you can use to
#' create a progress bar. `.progress` can be:
#'
#' * `FALSE`, the default: does not create a progress bar.
#' * `TRUE`: creates a basic unnamed progress bar.
#' * A string: creates a basic progress bar with the given name.
#' * A named list of progress bar parameters, as described below.
#'
#' It's good practice to name your progress bars, to make it clear what
#' calculation or process they belong to. We recommend keeping the names
#' under 20 characters, so the whole progress bar fits comfortably even on
#' on narrower displays.
#'
#' ## Progress bar parameters
#'
#' * `clear`: whether to remove the progress bar from the screen after
#'   termination. Defaults to `TRUE`.
#' * `format`: format string. This overrides the default format string of
#'   the progress bar type. It must be given for the `custom` type.
#'   Format strings may contain R expressions to evaluate in braces.
#'   They support cli [pluralization][cli::pluralization], and
#'   [styling][cli::inline-markup] and they can contain special
#'   [progress variables][cli::progress-variables].
#' * `format_done`: format string for successful termination. By default
#'   the same as `format`.
#' * `format_failed`: format string for unsuccessful termination.
#'   By default the same as `format`.
#' * `name`: progress bar name. This is by default the empty string and it
#'   is displayed at the beginning of the progress bar.
#' * `show_after`: numeric scalar. Only show the progress bar after this
#'   number of seconds. It overrides the `cli.progress_show_after`
#'   global option.
#' * `type`: progress bar type. Currently supported types are:
#'   * `iterator`: the default, a for loop or a mapping function,
#'   * `tasks`: a (typically small) number of tasks,
#'   * `download`: download of one file,
#'   * `custom`: custom type, `format` must not be `NULL` for this type.
#'   The default display is different for each progress bar type.
#'
#' ## Further documentation
#'
#' purrr's progress bars are powered by cli, so see
#' [Introduction to progress bars in cli](https://cli.r-lib.org/articles/progress.html)
#' and [Advanced cli progress bars](https://cli.r-lib.org/articles/progress-advanced.html)
#' for more details.
#'
#' @name progress_bars
NULL

as_progress <- function(
  progress,
  user_env = caller_env(2),
  caller_env = caller_env()
) {
  if (isFALSE(progress) || isTRUE(progress) || is_string(progress)) {
    progress
  } else if (is.list(progress)) {
    progress$caller <- progress$caller %||% user_env
    progress
  } else {
    stop_input_type(
      progress,
      c("TRUE", "FALSE", "a string", "a named list"),
      arg = ".progress",
      call = caller_env
    )
  }
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/R/rate.R ---
#' Create delaying rate settings
#'
#' These helpers create rate settings that you can pass to [insistently()] and
#' [slowly()]. You can also use them in your own functions with [rate_sleep()].
#'
#' @param max_times Maximum number of requests to attempt.
#' @param jitter Whether to introduce a random jitter in the waiting time.
#' @examples
#' # A delay rate waits the same amount of time:
#' rate <- rate_delay(0.02)
#' for (i in 1:3) rate_sleep(rate, quiet = FALSE)
#'
#' # A backoff rate waits exponentially longer each time, with random
#' # jitter by default:
#' rate <- rate_backoff(pause_base = 0.2, pause_min = 0.005)
#' for (i in 1:3) rate_sleep(rate, quiet = FALSE)
#' @name rate-helpers
NULL

#' @rdname rate-helpers
#' @param pause Delay between attempts in seconds.
#' @export
rate_delay <- function(pause = 1, max_times = Inf) {
  check_number_decimal(pause, allow_infinite = TRUE, min = 0)

  new_rate(
    "purrr_rate_delay",
    pause = pause,
    max_times = max_times,
    jitter = FALSE
  )
}

#' @rdname rate-helpers
#' @param pause_base,pause_cap `rate_backoff()` uses an exponential
#'   back-off so that each request waits `pause_base * 2^i` seconds,
#'   up to a maximum of `pause_cap` seconds.
#' @param pause_min Minimum time to wait in the backoff; generally
#'   only necessary if you need pauses less than one second (which may
#'   not be kind to the server, use with caution!).
#' @export
rate_backoff <- function(
  pause_base = 1,
  pause_cap = 60,
  pause_min = 1,
  max_times = 3,
  jitter = TRUE
) {
  check_number_decimal(pause_base, min = 0)
  check_number_decimal(pause_cap, allow_infinite = TRUE, min = 0)
  check_number_decimal(pause_min, allow_infinite = TRUE, min = 0)
  check_number_whole(max_times, min = 1)
  check_bool(jitter)

  new_rate(
    "purrr_rate_backoff",
    pause_base = pause_base,
    pause_cap = pause_cap,
    pause_min = pause_min,
    max_times = max_times,
    jitter = jitter
  )
}

new_rate <- function(.subclass, ..., jitter = TRUE, max_times = 3) {
  stopifnot(
    is_bool(jitter),
    is_number(max_times) || identical(max_times, Inf)
  )

  rate <- list(
    ...,
    state = env(i = 0L),
    jitter = jitter,
    max_times = max_times
  )

  structure(
    rate,
    class = c(.subclass, "purrr_rate")
  )
}
#' @rdname rate-helpers
#' @param x An object to test.
#' @export
is_rate <- function(x) {
  inherits(x, "purrr_rate")
}

#' @export
print.purrr_rate_delay <- function(x, ...) {
  cli::cli_text("<rate: delay>")
  cli::cli_bullets(c(
    " " = "Attempts: {rate_count(x)}/{x$max_times}",
    " " = "{.field pause}: {x$pause}"
  ))

  invisible(x)
}
#' @export
print.purrr_rate_backoff <- function(x, ...) {
  cli::cli_text("<rate: backoff>")

  cli::cli_bullets(c(
    " " = "Attempts: {rate_count(x)}/{x$max_times}",
    " " = "{.field pause_base}: {x$pause_base}",
    " " = "{.field pause_cap}: {x$pause_cap}",
    " " = "{.field pause_min}: {x$pause_min}"
  ))

  invisible(x)
}

#' Wait for a given time
#'
#' If the rate's internal counter exceeds the maximum number of times
#' it is allowed to sleep, `rate_sleep()` throws an error of class
#' `purrr_error_rate_excess`.
#'
#' Call `rate_reset()` to reset the internal rate counter to 0.
#'
#' @param rate A [rate][rate_backoff] object determining the waiting time.
#' @param quiet If `FALSE`, prints a message displaying how long until
#'   the next request.
#'
#' @seealso [rate_backoff()], [insistently()]
#' @keywords internal
#' @export
rate_sleep <- function(rate, quiet = TRUE) {
  stopifnot(is_rate(rate))

  i <- rate_count(rate)

  if (i > rate$max_times) {
    stop_rate_expired(rate)
  }
  if (i == rate$max_times) {
    stop_rate_excess(rate)
  }

  if (i == 0L) {
    rate_bump_count(rate)
    signal_rate_init(rate)
    return(invisible())
  }

  on.exit(rate_bump_count(rate))
  UseMethod("rate_sleep")
}

#' @export
rate_sleep.purrr_rate_backoff <- function(rate, quiet = TRUE) {
  i <- rate_count(rate)

  pause_max <- min(rate$pause_cap, rate$pause_base * 2^i)
  if (rate$jitter) {
    pause_max <- stats::runif(1, 0, pause_max)
  }

  length <- max(rate$pause_min, pause_max)
  rate_sleep_impl(rate, length, quiet)
}
#' @export
rate_sleep.purrr_rate_delay <- function(rate, quiet = TRUE) {
  rate_sleep_impl(rate, rate$pause, quiet)
}

rate_sleep_impl <- function(rate, length, quiet) {
  if (!quiet) {
    signal_rate_retry(rate, length, quiet)
  }
  Sys.sleep(length)
}

#' @rdname rate_sleep
#' @export
rate_reset <- function(rate) {
  stopifnot(is_rate(rate))

  rate$state$i <- 0L

  invisible(rate)
}

rate_count <- function(rate) {
  rate$state$i
}
rate_bump_count <- function(rate, n = 1L) {
  rate$state$i <- rate$state$i + n
  invisible(rate)
}

signal_rate_init <- function(rate) {
  signal("", "purrr_condition_rate_init", rate = rate)
}
signal_rate_retry <- function(rate, length, quiet) {
  msg <- sprintf("Retrying in %s seconds.", format(length, digits = 2))
  class <- "purrr_message_rate_retry"
  if (quiet) {
    signal(msg, class, rate = rate, length = length)
  } else {
    inform(msg, class, rate = rate, length = length)
  }
}

stop_rate_expired <- function(rate, error_call = caller_env()) {
  cli::cli_abort(
    c(
      "This `rate` object has already be run more than `max_times` allows.",
      i = "Do you need to reset it with `rate_reset()`?"
    ),
    class = "purrr_error_rate_expired",
    call = error_call
  )
}
stop_rate_excess <- function(rate, error_call = caller_env()) {
  i <- rate_count(rate)

  # Bump counter to get an expired error next time around
  rate_bump_count(rate)

  cli::cli_abort(
    "Request failed after {i} attempts.",
    class = "purrr_error_rate_excess",
    call = error_call
  )
}

check_rate <- function(rate, error_call = caller_env()) {
  if (!is_rate(rate)) {
    cli::cli_abort(
      "{.arg rate} must be a rate object, not {.obj_type_friendly {rate}}.",
      arg = "rate",
      call = error_call,
    )
  }
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/R/reduce.R ---
#' Reduce a list to a single value by iteratively applying a binary function
#'
#' @description
#'
#' `reduce()` is an operation that combines the elements of a vector
#' into a single value. The combination is driven by `.f`, a binary
#' function that takes two values and returns a single value: reducing
#' `f` over `1:3` computes the value `f(f(1, 2), 3)`.
#'
#' @inheritParams map
#' @param ... Additional arguments passed on to the reduce function.
#'
#'   We now generally recommend against using `...` to pass additional
#'   (constant) arguments to `.f`. Instead use a shorthand anonymous function:
#'
#'   ```R
#'   # Instead of
#'   x |> reduce(f, 1, 2, collapse = ",")
#'   # do:
#'   x |> reduce(\(x, y) f(x, y, 1, 2, collapse = ","))
#'   ```
#'
#'   This makes it easier to understand which arguments belong to which
#'   function and will tend to yield better error messages.
#'
#' @param .y For `reduce2()` an additional
#'   argument that is passed to `.f`. If `init` is not set, `.y`
#'   should be 1 element shorter than `.x`.
#' @param .f For `reduce()`, a 2-argument function. The function will be passed
#'   the accumulated value as the first argument and the "next" value as the
#'   second argument.
#'
#'   For `reduce2()`, a 3-argument function. The function will be passed the
#'   accumulated value as the first argument, the next value of `.x` as the
#'   second argument, and the next value of `.y` as the third argument.
#'
#'   The reduction terminates early if `.f` returns a value wrapped in
#'   a [done()].
#'
#' @param .init If supplied, will be used as the first value to start
#'   the accumulation, rather than using `.x[[1]]`. This is useful if
#'   you want to ensure that `reduce` returns a correct value when `.x`
#'   is empty. If missing, and `.x` is empty, will throw an error.
#' @param .dir The direction of reduction as a string, one of
#'   `"forward"` (the default) or `"backward"`. See the section about
#'   direction below.
#'
#' @section Direction:
#'
#' When `.f` is an associative operation like `+` or `c()`, the
#' direction of reduction does not matter. For instance, reducing the
#' vector `1:3` with the binary function `+` computes the sum `((1 +
#' 2) + 3)` from the left, and the same sum `(1 + (2 + 3))` from the
#' right.
#'
#' In other cases, the direction has important consequences on the
#' reduced value. For instance, reducing a vector with `list()` from
#' the left produces a left-leaning nested list (or tree), while
#' reducing `list()` from the right produces a right-leaning list.
#'
#' @seealso [accumulate()] for a version that returns all intermediate
#'   values of the reduction.
#' @examples
#' # Reducing `+` computes the sum of a vector while reducing `*`
#' # computes the product:
#' 1:3 |> reduce(`+`)
#' 1:10 |> reduce(`*`)
#'
#' # By ignoring the input vector (nxt), you can turn output of one step into
#' # the input for the next. This code takes 10 steps of a random walk:
#' reduce(1:10, \(acc, nxt) acc + rnorm(1), .init = 0)
#'
#' # When the operation is associative, the direction of reduction
#' # does not matter:
#' reduce(1:4, `+`)
#' reduce(1:4, `+`, .dir = "backward")
#'
#' # However with non-associative operations, the reduced value will
#' # be different as a function of the direction. For instance,
#' # `list()` will create left-leaning lists when reducing from the
#' # right, and right-leaning lists otherwise:
#' str(reduce(1:4, list))
#' str(reduce(1:4, list, .dir = "backward"))
#'
#' # reduce2() takes a ternary function and a second vector that is
#' # one element smaller than the first vector:
#' paste2 <- function(x, y, sep = ".") paste(x, y, sep = sep)
#' letters[1:4] |> reduce(paste2)
#' letters[1:4] |> reduce2(c("-", ".", "-"), paste2)
#'
#' x <- list(c(0, 1), c(2, 3), c(4, 5))
#' y <- list(c(6, 7), c(8, 9))
#' reduce2(x, y, paste)
#'
#'
#' # You can shortcircuit a reduction and terminate it early by
#' # returning a value wrapped in a done(). In the following example
#' # we return early if the result-so-far, which is passed on the LHS,
#' # meets a condition:
#' paste3 <- function(out, input, sep = ".") {
#'   if (nchar(out) > 4) {
#'     return(done(out))
#'   }
#'   paste(out, input, sep = sep)
#' }
#' letters |> reduce(paste3)
#'
#' # Here the early return branch checks the incoming inputs passed on
#' # the RHS:
#' paste4 <- function(out, input, sep = ".") {
#'   if (input == "j") {
#'     return(done(out))
#'   }
#'   paste(out, input, sep = sep)
#' }
#' letters |> reduce(paste4)
#' @export
reduce <- function(.x, .f, ..., .init, .dir = c("forward", "backward")) {
  reduce_impl(.x, .f, ..., .init = .init, .dir = .dir)
}
#' @rdname reduce
#' @export
reduce2 <- function(.x, .y, .f, ..., .init) {
  reduce2_impl(.x, .y, .f, ..., .init = .init, .left = TRUE)
}

reduce_impl <- function(
  .x,
  .f,
  ...,
  .init,
  .dir,
  .acc = FALSE,
  .purrr_error_call = caller_env()
) {
  left <- arg_match0(.dir, c("forward", "backward")) == "forward"

  out <- reduce_init(.x, .init, left = left, error_call = .purrr_error_call)
  idx <- reduce_index(.x, .init, left = left)

  if (.acc) {
    acc_out <- accum_init(out, idx, left = left)
    acc_idx <- accum_index(acc_out, left = left)
  }

  .f <- as_mapper(.f, ...)

  # Left-reduce passes the result-so-far on the left, right-reduce
  # passes it on the right. A left-reduce produces left-leaning
  # computation trees while right-reduce produces right-leaning trees.
  if (left) {
    fn <- .f
  } else {
    fn <- function(x, y, ...) .f(y, x, ...)
  }

  for (i in seq_along(idx)) {
    prev <- out
    elt <- .x[[idx[[i]]]]

    out <- forceAndCall(2, fn, out, elt, ...)

    if (is_done_box(out)) {
      return(reduce_early(out, prev, .acc, acc_out, acc_idx[[i]], left))
    }

    if (.acc) {
      acc_out[[acc_idx[[i]]]] <- out
    }
  }

  if (.acc) {
    acc_out
  } else {
    out
  }
}

reduce_early <- function(out, prev, acc, acc_out, acc_idx, left = TRUE) {
  if (is_done_box(out, empty = TRUE)) {
    out <- prev
    offset <- if (left) -1L else 1L
  } else {
    out <- unbox(out)
    offset <- 0L
  }

  if (!acc) {
    return(out)
  }

  acc_idx <- acc_idx + offset
  acc_out[[acc_idx]] <- out

  if (left) {
    acc_out[seq_len(acc_idx)]
  } else {
    acc_out[seq(acc_idx, length(acc_out))]
  }
}

reduce_init <- function(x, init, left = TRUE, error_call = caller_env()) {
  if (!missing(init)) {
    init
  } else {
    if (is_empty(x)) {
      cli::cli_abort(
        "Must supply {.arg .init} when {.arg .x} is empty.",
        arg = ".init",
        call = error_call
      )
    } else if (left) {
      x[[1]]
    } else {
      x[[length(x)]]
    }
  }
}
reduce_index <- function(x, init, left = TRUE) {
  n <- length(x)

  if (left) {
    if (missing(init)) {
      seq_len2(2L, n)
    } else {
      seq_len(n)
    }
  } else {
    if (missing(init)) {
      rev(seq_len(n - 1L))
    } else {
      rev(seq_len(n))
    }
  }
}

accum_init <- function(first, idx, left) {
  len <- length(idx) + 1L
  out <- new_list(len)

  if (left) {
    out[[1]] <- first
  } else {
    out[[len]] <- first
  }

  out
}
accum_index <- function(out, left) {
  n <- length(out)

  if (left) {
    seq_len2(2, n)
  } else {
    rev(seq_len(n - 1L))
  }
}

reduce2_impl <- function(
  .x,
  .y,
  .f,
  ...,
  .init,
  .left = TRUE,
  .acc = FALSE,
  .purrr_error_call = caller_env()
) {
  out <- reduce_init(.x, .init, left = .left, error_call = .purrr_error_call)
  x_idx <- reduce_index(.x, .init, left = .left)
  y_idx <- reduce_index(.y, NULL, left = .left)

  if (length(x_idx) != length(y_idx)) {
    cli::cli_abort(
      "{.arg .y} must have length {length(x_idx)}, not {length(y_idx)}.",
      arg = ".y",
      call = .purrr_error_call
    )
  }

  .f <- as_mapper(.f, ...)

  if (.acc) {
    acc_out <- accum_init(out, x_idx, left = .left)
    acc_idx <- accum_index(acc_out, left = .left)
  }

  for (i in seq_along(x_idx)) {
    prev <- out

    x_i <- x_idx[[i]]
    y_i <- y_idx[[i]]

    out <- forceAndCall(3, .f, out, .x[[x_i]], .y[[y_i]], ...)

    if (is_done_box(out)) {
      return(reduce_early(out, prev, .acc, acc_out, acc_idx[[i]]))
    }

    if (.acc) {
      acc_out[[acc_idx[[i]]]] <- out
    }
  }

  if (.acc) {
    acc_out
  } else {
    out
  }
}

seq_len2 <- function(start, end) {
  if (start > end) {
    return(integer(0))
  }

  start:end
}

#' Accumulate intermediate results of a vector reduction
#'
#' @description
#'
#' `accumulate()` sequentially applies a 2-argument function to elements of a
#' vector. Each application of the function uses the initial value or result
#' of the previous application as the first argument. The second argument is
#' the next value of the vector. The results of each application are
#' returned in a list. The accumulation can optionally terminate before
#' processing the whole vector in response to a `done()` signal returned by
#' the accumulation function.
#'
#' By contrast to `accumulate()`, `reduce()` applies a 2-argument function in
#' the same way, but discards all results except that of the final function
#' application.
#'
#' `accumulate2()` sequentially applies a function to elements of two lists, `.x` and `.y`.
#'
#' @inheritParams map
#'
#' @param .y For `accumulate2()` `.y` is the second argument of the pair. It
#'     needs to be 1 element shorter than the vector to be accumulated (`.x`).
#'     If `.init` is set, `.y` needs to be one element shorted than the
#'     concatenation of the initial value and `.x`.
#'
#' @param .f For `accumulate()` `.f` is 2-argument function. The function will
#'     be passed the accumulated result or initial value as the first argument.
#'     The next value in sequence is passed as the second argument.
#'
#'   For `accumulate2()`, a 3-argument function. The
#'   function will be passed the accumulated result as the first
#'   argument. The next value in sequence from `.x` is passed as the second argument. The
#'   next value in sequence from `.y` is passed as the third argument.
#'
#'   The accumulation terminates early if `.f` returns a value wrapped in
#'   a [done()].
#'
#' @param .init If supplied, will be used as the first value to start
#'   the accumulation, rather than using `.x[[1]]`. This is useful if
#'   you want to ensure that `reduce` returns a correct value when `.x`
#'   is empty. If missing, and `.x` is empty, will throw an error.
#' @param .dir The direction of accumulation as a string, one of
#'   `"forward"` (the default) or `"backward"`. See the section about
#'   direction below.
#' @param .simplify If `NA`, the default, the accumulated list of
#'   results is simplified to an atomic vector if possible.
#'   If `TRUE`, the result is simplified, erroring if not possible.
#'   If `FALSE`, the result is not simplified, always returning a list.
#' @param .ptype If `simplify` is `NA` or `TRUE`, optionally supply a vector
#'   prototype to enforce the output type.
#' @return A vector the same length of `.x` with the same names as `.x`.
#'
#'   If `.init` is supplied, the length is extended by 1. If `.x` has
#'   names, the initial value is given the name `".init"`, otherwise
#'   the returned vector is kept unnamed.
#'
#'   If `.dir` is `"forward"` (the default), the first element is the
#'   initial value (`.init` if supplied, or the first element of `.x`)
#'   and the last element is the final reduced value. In case of a
#'   right accumulation, this order is reversed.
#'
#'   The accumulation terminates early if `.f` returns a value wrapped
#'   in a [done()]. If the done box is empty, the last value is
#'   used instead and the result is one element shorter (but always
#'   includes the initial value, even when terminating at the first
#'   iteration).
#'
#' @inheritSection reduce Direction
#'
#' @seealso [reduce()] when you only need the final reduced value.
#' @examples
#' # With an associative operation, the final value is always the
#' # same, no matter the direction. You'll find it in the first element for a
#' # backward (left) accumulation, and in the last element for forward
#' # (right) one:
#' 1:5 |> accumulate(`+`)
#' 1:5 |> accumulate(`+`, .dir = "backward")
#'
#' # The final value is always equal to the equivalent reduction:
#' 1:5 |> reduce(`+`)
#'
#' # It is easier to understand the details of the reduction with
#' # `paste()`.
#' accumulate(letters[1:5], paste, sep = ".")
#'
#' # Note how the intermediary reduced values are passed to the left
#' # with a left reduction, and to the right otherwise:
#' accumulate(letters[1:5], paste, sep = ".", .dir = "backward")
#'
#' # By ignoring the input vector (nxt), you can turn output of one step into
#' # the input for the next. This code takes 10 steps of a random walk:
#' accumulate(1:10, \(acc, nxt) acc + rnorm(1), .init = 0)
#'
#' # `accumulate2()` is a version of `accumulate()` that works with
#' # 3-argument functions and one additional vector:
#' paste2 <- function(acc, nxt, sep = ".") paste(acc, nxt, sep = sep)
#' letters[1:4] |> accumulate(paste2)
#' letters[1:4] |> accumulate2(c("-", ".", "-"), paste2)
#'
#' # You can shortcircuit an accumulation and terminate it early by
#' # returning a value wrapped in a done(). In the following example
#' # we return early if the result-so-far, which is passed on the LHS,
#' # meets a condition:
#' paste3 <- function(out, input, sep = ".") {
#'   if (nchar(out) > 4) {
#'     return(done(out))
#'   }
#'   paste(out, input, sep = sep)
#' }
#' letters |> accumulate(paste3)
#'
#' # Note how we get twice the same value in the accumulation. That's
#' # because we have returned it twice. To prevent this, return an empty
#' # done box to signal to accumulate() that it should terminate with the
#' # value of the last iteration:
#' paste3 <- function(out, input, sep = ".") {
#'   if (nchar(out) > 4) {
#'     return(done())
#'   }
#'   paste(out, input, sep = sep)
#' }
#' letters |> accumulate(paste3)
#'
#' # Here the early return branch checks the incoming inputs passed on
#' # the RHS:
#' paste4 <- function(out, input, sep = ".") {
#'   if (input == "f") {
#'     return(done())
#'   }
#'   paste(out, input, sep = sep)
#' }
#' letters |> accumulate(paste4)
#'
#'
#' # Simulating stochastic processes with drift
#' \dontrun{
#' library(dplyr)
#' library(ggplot2)
#'
#' map(1:5, \(i) rnorm(100)) |>
#'   set_names(paste0("sim", 1:5)) |>
#'   map(\(l) accumulate(l, \(acc, nxt) .05 + acc + nxt)) |>
#'   map(\(x) tibble(value = x, step = 1:100)) |>
#'   list_rbind(names_to = "simulation") |>
#'   ggplot(aes(x = step, y = value)) +
#'     geom_line(aes(color = simulation)) +
#'     ggtitle("Simulations of a random walk with drift")
#' }
#' @export
accumulate <- function(
  .x,
  .f,
  ...,
  .init,
  .dir = c("forward", "backward"),
  .simplify = NA,
  .ptype = NULL
) {
  .dir <- arg_match0(.dir, c("forward", "backward"))
  .f <- as_mapper(.f, ...)

  res <- reduce_impl(.x, .f, ..., .init = .init, .dir = .dir, .acc = TRUE)
  names(res) <- accumulate_names(names(.x), .init, .dir)

  res <- list_simplify_internal(res, .simplify, .ptype)
  res
}
#' @rdname accumulate
#' @export
accumulate2 <- function(.x, .y, .f, ..., .init, .simplify = NA, .ptype = NULL) {
  res <- reduce2_impl(.x, .y, .f, ..., .init = .init, .acc = TRUE)
  res <- list_simplify_internal(res, .simplify, .ptype)
  res
}

accumulate_names <- function(nms, init, dir) {
  if (is_null(nms)) {
    return(NULL)
  }

  if (!missing(init)) {
    nms <- c(".init", nms)
  }
  if (dir == "backward") {
    nms <- rev(nms)
  }

  nms
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/R/reexport-pipe.R ---
#' Pipe operator
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/R/reexport-rlang.R ---
#' @export
rlang::set_names

#' @export
rlang::exec

#' @export
rlang::zap

#' @export
rlang::`%||%`

#' @export
rlang::done

#' @export
rlang::rep_along

# Predicates ---------------------------------------------------

#' @export
rlang::is_bare_list

#' @export
rlang::is_bare_atomic

#' @export
rlang::is_bare_vector

#' @export
rlang::is_bare_double

#' @export
rlang::is_bare_integer

#' @export
rlang::is_bare_numeric

#' @export
rlang::is_bare_character

#' @export
rlang::is_bare_logical

#' @export
rlang::is_list

#' @export
rlang::is_atomic

#' @export
rlang::is_vector

#' @export
rlang::is_integer

#' @export
rlang::is_double

#' @export
rlang::is_character

#' @export
rlang::is_logical

#' @export
rlang::is_null

#' @export
rlang::is_function

#' @export
rlang::is_scalar_list

#' @export
rlang::is_scalar_atomic

#' @export
rlang::is_scalar_vector

#' @export
rlang::is_scalar_double

#' @export
rlang::is_scalar_character

#' @export
rlang::is_scalar_logical

#' @export
rlang::is_scalar_integer

#' @export
rlang::is_empty

#' @export
rlang::is_formula


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/R/superseded-flatten.R ---
#' Flatten a list of lists into a simple vector
#'
#' @description
#' `r lifecycle::badge("superseded")`
#'
#' These functions were superseded in purrr 1.0.0 because their behaviour was
#' inconsistent. Superseded functions will not go away, but will only receive
#' critical bug fixes.
#'
#' * `flatten()` has been superseded by [list_flatten()].
#' * `flatten_lgl()`, `flatten_int()`, `flatten_dbl()`, and `flatten_chr()`
#'    have been superseded by [list_c()].
#' * `flatten_dfr()` and `flatten_dfc()` have been superseded by [list_rbind()]
#'    and [list_cbind()] respectively.
#'
#' @param .x A list to flatten. The contents of the list can be anything for
#'   `flatten()` (as a list is returned), but the contents must match the
#'   type for the other functions.
#' @return `flatten()` returns a list, `flatten_lgl()` a logical
#'   vector, `flatten_int()` an integer vector, `flatten_dbl()` a
#'   double vector, and `flatten_chr()` a character vector.
#'
#'   `flatten_dfr()` and `flatten_dfc()` return data frames created by
#'   row-binding and column-binding respectively. They require dplyr to
#'   be installed.
#' @keywords internal
#' @inheritParams map
#' @export
#' @examples
#' x <- map(1:3, \(i) sample(4))
#' x
#'
#' # was
#' x |> flatten_int() |> str()
#' # now
#' x |> list_c() |> str()
#'
#' x <- list(list(1, 2), list(3, 4))
#' # was
#' x |> flatten() |> str()
#' # now
#' x |> list_flatten() |> str()
flatten <- function(.x) {
  # in 1.0.0
  lifecycle::signal_stage("superseded", "flatten()", "list_flatten()")
  .Call(flatten_impl, .x)
}

#' @export
#' @rdname flatten
flatten_lgl <- function(.x) {
  # in 1.0.0
  lifecycle::signal_stage("superseded", "flatten_lgl()", "list_c()")
  .Call(vflatten_impl, .x, "logical")
}

#' @export
#' @rdname flatten
flatten_int <- function(.x) {
  lifecycle::signal_stage("superseded", "flatten_lgl()", "list_c()")
  .Call(vflatten_impl, .x, "integer")
}

#' @export
#' @rdname flatten
flatten_dbl <- function(.x) {
  # in 1.0.0
  lifecycle::signal_stage("superseded", "flatten_lgl()", "list_c()")
  .Call(vflatten_impl, .x, "double")
}

#' @export
#' @rdname flatten
flatten_chr <- function(.x) {
  # in 1.0.0
  lifecycle::signal_stage("superseded", "flatten_lgl()", "list_c()")
  .Call(vflatten_impl, .x, "character")
}


#' @export
#' @rdname flatten
flatten_dfr <- function(.x, .id = NULL) {
  # in 1.0.0
  lifecycle::signal_stage("superseded", "flatten_dfr()", "list_rbind()")
  check_installed("dplyr", "for `flatten_dfr()`.")

  res <- .Call(flatten_impl, .x)
  dplyr::bind_rows(res, .id = .id)
}

#' @export
#' @rdname flatten
flatten_dfc <- function(.x) {
  # in 1.0.0
  lifecycle::signal_stage("superseded", "flatten_dfc()", "list_cbind()")
  check_installed("dplyr", "for `flatten_dfc()`.")

  res <- .Call(flatten_impl, .x)
  dplyr::bind_cols(res)
}

#' @export
#' @rdname flatten
#' @usage NULL
flatten_df <- function(.x, .id = NULL) {
  # in 1.0.0
  lifecycle::signal_stage("superseded", "flatten_df()", "list_rbind()")
  check_installed("dplyr", "for `flatten_dfr()`.")

  res <- .Call(flatten_impl, .x)
  dplyr::bind_rows(res, .id = .id)
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/R/superseded-map-df.R ---
#' Functions that return data frames
#'
#' @description
#' `r lifecycle::badge("superseded")`
#'
#' These [map()], [map2()], [imap()], and [pmap()] variants return data
#' frames by row-binding or column-binding the outputs together.
#'
#' The functions were superseded in purrr 1.0.0 because their names
#' suggest they work like `_lgl()`, `_int()`, etc which require length
#' 1 outputs, but actually they return results of any size because the results
#' are combined without any size checks. Additionally, they use
#' `dplyr::bind_rows()` and `dplyr::bind_cols()` which require dplyr to be
#' installed and have confusing semantics with edge cases. Superseded
#' functions will not go away, but will only receive critical bug fixes.
#'
#' Instead, we recommend using `map()`, `map2()`, etc with [list_rbind()] and
#' [list_cbind()]. These use [vctrs::vec_rbind()] and [vctrs::vec_cbind()]
#' under the hood, and have names that more clearly reflect their semantics.
#'
#' @param .id Either a string or `NULL`. If a string, the output will contain
#'   a variable with that name, storing either the name (if `.x` is named) or
#'   the index (if `.x` is unnamed) of the input. If `NULL`, the default, no
#'   variable will be created.
#'
#'   Only applies to `_dfr` variant.
#' @keywords internal
#' @export
#' @examples
#' # map ---------------------------------------------
#' # Was:
#' mtcars |>
#'   split(mtcars$cyl) |>
#'   map(\(df) lm(mpg ~ wt, data = df)) |>
#'   map_dfr(\(mod) as.data.frame(t(as.matrix(coef(mod)))))
#'
#' # Now:
#' mtcars |>
#'   split(mtcars$cyl) |>
#'   map(\(df) lm(mpg ~ wt, data = df)) |>
#'   map(\(mod) as.data.frame(t(as.matrix(coef(mod))))) |>
#'   list_rbind()
#'
#' # for certain pathological inputs `map_dfr()` and `map_dfc()` actually
#' # both combine the list by column
#' df <- data.frame(
#'   x = c(" 13", "  15 "),
#'   y = c("  34",  " 67 ")
#' )
#'
#' # Was:
#' map_dfr(df, trimws)
#' map_dfc(df, trimws)
#'
#' # But list_rbind()/list_cbind() fail because they require data frame inputs
#' try(map(df, trimws) |> list_rbind())
#'
#' # Instead, use modify() to apply a function to each column of a data frame
#' modify(df, trimws)
#'
#' # map2 ---------------------------------------------
#'
#' ex_fun <- function(arg1, arg2){
#'   col <- arg1 + arg2
#'   x <- as.data.frame(col)
#' }
#' arg1 <- 1:4
#' arg2 <- 10:13
#'
#' # was
#' map2_dfr(arg1, arg2, ex_fun)
#' # now
#' map2(arg1, arg2, ex_fun) |> list_rbind()
#'
#' # was
#' map2_dfc(arg1, arg2, ex_fun)
#' # now
#' map2(arg1, arg2, ex_fun) |> list_cbind()
map_dfr <- function(.x, .f, ..., .id = NULL) {
  # in 1.0.0
  lifecycle::signal_stage(
    "superseded",
    "map_dfr()",
    I("`map()` + `list_rbind()`")
  )
  check_installed("dplyr", "for `map_dfr()`.")

  .f <- as_mapper(.f, ...)
  res <- map(.x, .f, ...)
  dplyr::bind_rows(res, .id = .id)
}

#' @rdname map_dfr
#' @usage NULL
#' @export
map_df <- function(.x, .f, ..., .id = NULL) {
  # in 1.0.0
  lifecycle::signal_stage(
    "superseded",
    "map_df()",
    I("`map()` + `list_rbind()`")
  )
  check_installed("dplyr", "for `map_dfr()`.")

  .f <- as_mapper(.f, ...)
  res <- map(.x, .f, ...)
  dplyr::bind_rows(res, .id = .id)
}

#' @rdname map_dfr
#' @export
map_dfc <- function(.x, .f, ...) {
  # in 1.0.0
  lifecycle::signal_stage(
    "superseded",
    "map_dfc()",
    I("`map()` + `list_cbind()`")
  )
  check_installed("dplyr", "for `map_dfc()`.")

  .f <- as_mapper(.f, ...)
  res <- map(.x, .f, ...)
  dplyr::bind_cols(res)
}

#' @rdname map_dfr
#' @export
imap_dfr <- function(.x, .f, ..., .id = NULL) {
  # in 1.0.0
  lifecycle::signal_stage(
    "superseded",
    "imap_dfr()",
    I("`imap()` + `list_rbind()`")
  )

  .f <- as_mapper(.f, ...)
  res <- map2(.x, vec_index(.x), .f, ...)
  dplyr::bind_rows(res, .id = .id)
}

#' @rdname map_dfr
#' @export
imap_dfc <- function(.x, .f, ...) {
  # in 1.0.0
  lifecycle::signal_stage(
    "superseded",
    "imap_dfc()",
    I("`imap()` + `list_cbind()`")
  )

  .f <- as_mapper(.f, ...)
  res <- map2(.x, vec_index(.x), .f, ...)
  dplyr::bind_cols(res)
}

#' @rdname map_dfr
#' @export
map2_dfr <- function(.x, .y, .f, ..., .id = NULL) {
  # in 1.0.0
  lifecycle::signal_stage(
    "superseded",
    "map2_dfr()",
    I("`map2()` + `list_rbind()`")
  )
  check_installed("dplyr", "for `map2_dfr()`.")

  .f <- as_mapper(.f, ...)
  res <- map2(.x, .y, .f, ...)
  dplyr::bind_rows(res, .id = .id)
}

#' @rdname map_dfr
#' @export
map2_dfc <- function(.x, .y, .f, ...) {
  # in 1.0.0
  lifecycle::signal_stage(
    "superseded",
    "map2_dfc()",
    I("`map2()` + `list_cbind()`")
  )
  check_installed("dplyr", "for `map2_dfc()`.")

  .f <- as_mapper(.f, ...)
  res <- map2(.x, .y, .f, ...)
  dplyr::bind_cols(res)
}

#' @rdname map_dfr
#' @export
#' @usage NULL
map2_df <- function(.x, .y, .f, ..., .id = NULL) {
  # in 1.0.0
  lifecycle::signal_stage(
    "superseded",
    "map2_df()",
    I("`map2()` + `list_rbind()`")
  )
  check_installed("dplyr", "for `map2_dfr()`.")

  .f <- as_mapper(.f, ...)
  res <- map2(.x, .y, .f, ...)
  dplyr::bind_rows(res, .id = .id)
}

#' @rdname map_dfr
#' @export
pmap_dfr <- function(.l, .f, ..., .id = NULL) {
  # in 1.0.0
  lifecycle::signal_stage(
    "superseded",
    "pmap_dfr()",
    I("`pmap()` + `list_rbind()`")
  )
  check_installed("dplyr", "for `pmap_dfr()`.")

  .f <- as_mapper(.f, ...)
  res <- pmap(.l, .f, ...)
  dplyr::bind_rows(res, .id = .id)
}

#' @rdname map_dfr
#' @export
pmap_dfc <- function(.l, .f, ...) {
  # in 1.0.0
  lifecycle::signal_stage(
    "superseded",
    "pmap_dfc()",
    I("`pmap()` + `list_cbind()`")
  )
  check_installed("dplyr", "for `pmap_dfc()`.")

  .f <- as_mapper(.f, ...)
  res <- pmap(.l, .f, ...)
  dplyr::bind_cols(res)
}

#' @rdname map_dfr
#' @export
#' @usage NULL
pmap_df <- function(.l, .f, ..., .id = NULL) {
  # in 1.0.0
  lifecycle::signal_stage(
    "superseded",
    "pmap_df()",
    I("`pmap()` + `list_rbind()`")
  )
  check_installed("dplyr", "for `pmap_dfr()`.")

  .f <- as_mapper(.f, ...)
  res <- pmap(.l, .f, ...)
  dplyr::bind_rows(res, .id = .id)
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/R/superseded-simplify.R ---
#' Coerce a list to a vector
#'
#' @description
#' `r lifecycle::badge("superseded")`
#'
#' These functions were superseded in purrr 1.0.0 in favour of
#' `list_simplify()` which has more consistent semantics based on vctrs
#' principles:
#'
#' * `as_vector(x)` is now `list_simplify(x)`
#' * `simplify(x)` is now `list_simplify(x, strict = FALSE)`
#' * `simplify_all(x)` is `map(x, list_simplify, strict = FALSE)`
#'
#' Superseded functions will not go away, but will only receive critical
#' bug fixes.
#'
#' @param .x A list of vectors
#' @param .type Can be a vector mold specifying both the type and the
#'   length of the vectors to be concatenated, such as `numeric(1)`
#'   or `integer(4)`. Alternatively, it can be a string describing
#'   the type, one of: "logical", "integer", "double", "complex",
#'   "character" or "raw".
#' @export
#' @keywords internal
#' @examples
#' # was
#' as.list(letters) |> as_vector("character")
#' # now
#' as.list(letters) |> list_simplify(ptype = character())
#'
#' # was:
#' list(1:2, 3:4, 5:6) |> as_vector(integer(2))
#' # now:
#' list(1:2, 3:4, 5:6) |> list_c(ptype = integer())
as_vector <- function(.x, .type = NULL) {
  # 1.0.0
  lifecycle::signal_stage("superseded", "as_vector()", "list_simplify()")
  as_vector_(.x, .type)
}
as_vector_ <- function(.x, .type = NULL) {
  if (can_simplify(.x, .type)) {
    unlist(.x)
  } else {
    cli::cli_abort(
      "Can't coerce {.arg .x} to a vector.",
      arg = ".x"
    )
  }
}

#' @export
#' @rdname as_vector
simplify <- function(.x, .type = NULL) {
  # 1.0.0
  lifecycle::signal_stage(
    "superseded",
    "simplify()",
    I("`list_simplify(strict = FALSE)`")
  )

  if (can_simplify(.x, .type)) {
    unlist(.x)
  } else {
    .x
  }
}

#' @export
#' @rdname as_vector
simplify_all <- function(.x, .type = NULL) {
  # 1.0.0
  lifecycle::signal_stage(
    "superseded",
    "simplify_all()",
    I("`map(xs, \\(x) list_simplify(strict = FALSE))`")
  )

  map(.x, simplify)
}


# Simplify a list of atomic vectors of the same type to a vector
#
# simplify_list(list(1, 2, 3))
can_simplify <- function(x, type = NULL) {
  is_atomic <- vapply(x, is.atomic, logical(1))
  if (!all(is_atomic)) {
    return(FALSE)
  }

  mode <- unique(vapply(x, typeof, character(1)))
  if (
    length(mode) > 1 &&
      !all(c("double", "integer") %in% mode)
  ) {
    return(FALSE)
  }

  # This can be coerced safely. If type is supplied, perform
  # additional check
  is.null(type) || can_coerce(x, type)
}

can_coerce <- function(x, type) {
  actual <- typeof(x[[1]])

  if (is_mold(type)) {
    lengths <- unique(map_int(x, length))
    if (length(lengths) > 1 || !(lengths == length(type))) {
      return(FALSE)
    } else {
      type <- typeof(type)
    }
  }

  if (actual == "integer" && type %in% c("integer", "double", "numeric")) {
    return(TRUE)
  }

  if (actual %in% c("integer", "double") && type == "numeric") {
    return(TRUE)
  }

  actual == type
}


# is a mold? As opposed to a string
is_mold <- function(type) {
  modes <- c(
    "numeric",
    "logical",
    "integer",
    "double",
    "complex",
    "character",
    "raw"
  )
  length(type) > 1 || (!type %in% modes)
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/R/superseded-transpose.R ---
#' Transpose a list.
#'
#' @description
#' `r lifecycle::badge("superseded")`
#'
#' `transpose()` turns a list-of-lists "inside-out"; it turns a pair of lists
#' into a list of pairs, or a list of pairs into pair of lists. For example,
#' if you had a list of length n where each component had values `a` and
#' `b`, `transpose()` would make a list with elements `a` and
#' `b` that contained lists of length n. It's called transpose because
#' \code{x[[1]][[2]]} is equivalent to \code{transpose(x)[[2]][[1]]}.
#'
#' This function was superseded in purrr 1.0.0 because [list_transpose()]
#' has a better name and can automatically simplify the output, as is commonly
#' needed. Superseded functions will not go away, but will only receive critical
#' bug fixes.
#'
#' @param .l A list of vectors to transpose. The first element is used as the
#'   template; you'll get a warning if a subsequent element has a different
#'   length.
#' @param .names For efficiency, `transpose()` bases the return structure on
#'   the first component of `.l` by default. Specify `.names` to override this.
#' @return A list with indexing transposed compared to `.l`.
#'
#'   `transpose()` is its own inverse, much like the transpose operation on a
#'    matrix. You can get back the original input by transposing it twice.
#' @keywords internal
#' @export
#' @examples
#' x <- map(1:5, \(i) list(x = runif(1), y = runif(5)))
#' # was
#' x |> transpose() |> str()
#' # now
#' x |> list_transpose(simplify = FALSE) |> str()
#'
#' # transpose() is useful in conjunction with safely() & quietly()
#' x <- list("a", 1, 2)
#' y <- x |> map(safely(log))
#' # was
#' y |> transpose() |> str()
#' # now:
#' y |> list_transpose() |> str()
#'
#' # Previously, output simplification required a call to another function
#' x <- list(list(a = 1, b = 2), list(a = 3, b = 4), list(a = 5, b = 6))
#' x |> transpose() |> simplify_all()
#' # Now can take advantage of automatic simplification
#' x |> list_transpose()
#'
#' # Provide explicit component names to prevent loss of those that don't
#' # appear in first component
#' ll <- list(
#'   list(x = 1, y = "one"),
#'   list(z = "deux", x = 2)
#' )
#' ll |> transpose()
#' nms <- ll |> map(names) |> reduce(union)
#' # was
#' ll |> transpose(.names = nms)
#' # now
#' ll |> list_transpose(template = nms)
#' # and can supply default value
#' ll |> list_transpose(template = nms, default = NA)
transpose <- function(.l, .names = NULL) {
  # 1.0.0
  if (!isTRUE(the$transpose_signalled)) {
    lifecycle::signal_stage("superseded", "transpose()", "list_transpose()")
    the$transpose_signalled <- TRUE
  }
  .Call(transpose_impl, .l, .names)
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/R/utils.R ---
where_at <- function(
  x,
  at,
  user_env,
  error_arg = caller_arg(at),
  error_call = caller_env()
) {
  if (is_formula(at)) {
    at <- rlang::as_function(at, arg = error_arg, call = error_call)
  }
  if (is.function(at)) {
    at <- at(names2(x))
  }

  if (is_quosures(at)) {
    lifecycle::deprecate_warn(
      when = "1.0.0",
      what = I("Using `vars()` in .at"),
      user_env = user_env
    )
    check_installed("tidyselect", "for using tidyselect in `map_at()`.")

    at <- tidyselect::vars_select(.vars = names2(x), !!!at)
  }

  if (is.numeric(at) || is.logical(at) || is.character(at)) {
    if (is.character(at)) {
      at <- intersect(at, names2(x))
    }

    loc <- vec_as_location(
      at,
      length(x),
      names2(x),
      missing = "error",
      arg = "at",
      call = error_call
    )
    seq_along(x) %in% loc
  } else {
    cli::cli_abort(
      "{.arg {error_arg}} must be a numeric vector, character vector, or function, not {.obj_type_friendly {at}}.",
      arg = error_arg,
      call = error_call
    )
  }
}

where_if <- function(.x, .p, ..., .purrr_error_call = caller_env()) {
  if (is_logical(.p)) {
    stopifnot(length(.p) == length(.x))
    .p
  } else {
    .p <- as_predicate(.p, ..., .mapper = TRUE, .purrr_error_call = NULL)
    map_(.x, .p, ..., .type = "logical", .purrr_error_call = .purrr_error_call)
  }
}

as_predicate <- function(
  .fn,
  ...,
  .mapper,
  .purrr_error_call = caller_env(),
  .purrr_error_arg = caller_arg(.fn)
) {
  force(.purrr_error_call)
  force(.purrr_error_arg)

  if (.mapper) {
    .fn <- as_mapper(.fn, ...)
  }

  function(...) {
    out <- .fn(...)

    if (!is_bool(out)) {
      cli::cli_abort(
        "{.fn { .purrr_error_arg }} must return a single `TRUE` or `FALSE`, not {.obj_type_friendly {out}}.",
        arg = .purrr_error_arg,
        call = .purrr_error_call
      )
    }

    out
  }
}

paste_line <- function(...) {
  paste(chr(...), collapse = "\n")
}

`list_slice2<-` <- function(x, i, value) {
  if (is.null(value)) {
    x[i] <- list(NULL)
  } else {
    x[[i]] <- value
  }
  x
}

vctrs_list_compat <- function(
  x,
  user_env,
  error_call = caller_env(),
  error_arg = caller_arg(x)
) {
  out <- vctrs_vec_compat(x, user_env)
  obj_check_list(out, call = error_call, arg = error_arg)
  out
}

# When we want to use vctrs, but treat lists like purrr does
#
# Treats data frames and S3 scalar lists like bare lists.
# But ensures rcrd vctrs retain their class.
vctrs_vec_compat <- function(x, user_env) {
  if (inherits(x, "by")) {
    class(x) <- NULL
  }

  if (is.null(x)) {
    list()
  } else if (is.pairlist(x)) {
    lifecycle::deprecate_soft(
      when = "1.0.0",
      what = I("Use of pairlists in purrr functions"),
      details = "Please coerce explicitly with `as.list()`",
      user_env = user_env
    )
    as.list(x)
  } else if (is.array(x) && length(dim(x)) > 1) {
    dim(x) <- NULL
    x
  } else if (is_call(x) || is.expression(x)) {
    lifecycle::deprecate_soft(
      when = "1.0.0",
      what = I("Use of calls and expressions in purrr functions"),
      details = "Please coerce explicitly with `as.list()`",
      user_env = user_env
    )
    as.list(x)
  } else if (isS4(x)) {
    set_names(lapply(seq_along(x), function(i) x[[i]]), names(x))
  } else if (is.data.frame(x) || (is.list(x) && !vec_is(x))) {
    unclass(x)
  } else {
    x
  }
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/tests/testthat.R ---
library(testthat)
library(purrr)

test_check("purrr")


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/tests/testthat/helper-map.R ---
named <- function(x) set_names(x, chr())

# Until we can reexport from rlang
vars <- function(...) rlang::quos(...)


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/tests/testthat/helper.R ---
expect_bare <- function(x, type) {
  predicate <- switch(
    type,
    logical = is_bare_logical,
    integer = is_bare_integer,
    double = is_bare_double,
    complex = is_bare_complex,
    character = is_bare_character,
    raw = is_bare_raw,
    list = is_bare_list,
  )

  expect_true(predicate(x))
}

local_name_repair_quiet <- function(frame = caller_env()) {
  local_options(rlib_name_repair_verbosity = "quiet", .frame = frame)
}
local_name_repair_verbose <- function(frame = caller_env()) {
  local_options(rlib_name_repair_verbosity = "verbose", .frame = frame)
}

local_methods <- function(..., .frame = caller_env()) {
  local_bindings(..., .env = global_env(), .frame = .frame)
}


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/tests/testthat/setup.R ---
Sys.setlocale("LC_MESSAGES", "C")


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/tests/testthat/test-adverb-auto-browse.R ---
test_that("auto_browse() not intended for primitive functions", {
  expect_snapshot(auto_browse(log)(NULL), error = TRUE)
  expect_no_error(auto_browse(identity)(NULL))
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/tests/testthat/test-adverb-compose.R ---
test_that("composed functions are applied right to left by default", {
  expect_identical(!is.null(4), compose(`!`, is.null)(4))

  set.seed(1)
  x <- sample(1:4, 100, replace = TRUE)
  expect_identical(unname(sort(table(x))), compose(unname, sort, table)(x))
})

test_that("composed functions are applied in reverse order if .dir is supplied", {
  expect_identical(compose(~ .x + 100, ~ .x * 2, .dir = "forward")(2), 204)
})

test_that("compose supports formulas", {
  round_mean <- compose(~ .x * 100, ~ round(.x, 2), ~ mean(.x, na.rm = TRUE))

  expect_s3_class(round_mean, "purrr_function_compose")
  expect_identical(round_mean(1:100), round(mean(1:100, na.rm = TRUE), 2) * 100)
})

test_that("compose() supports character vectors", {
  fn <- local({
    foobar <- function(x) paste(x, "baz")
    compose("foobar", "foobar")
  })
  expect_identical(fn("quux"), "quux baz baz")
})

test_that("can splice lists of functions", {
  fns <- list(
    ~ paste(.x, "a"),
    ~ paste(.x, "b")
  )
  fn <- compose(!!!fns)
  expect_identical(fn("c"), "c b a")
})

test_that("composed function has formals of first function called", {
  fn <- function(x, y = 1) NULL
  expect_identical(formals(compose(identity, fn)), formals(fn))
})

test_that("can compose primitive functions", {
  expect_identical(compose(is.character, as.character)(3), TRUE)
  expect_identical(compose(`-`, `/`)(4, 2), -2)
})

test_that("composed function prints informatively", {
  fn1 <- set_env(function(x) x + 1, global_env())
  fn2 <- set_env(function(x) x / 1, global_env())
  expect_snapshot({
    "Single input"
    compose(fn1)

    "Multiple inputs"
    compose(fn1, fn2)
  })
})

test_that("compose() with 0 inputs returns the identity", {
  expect_identical(compose()(mtcars), mtcars)
})

test_that("compose() with 1 input is a noop", {
  expect_identical(compose(toupper)(letters), toupper(letters))
})

test_that("compose() works with generic functions (#629)", {
  purrr__gen <- function(x) UseMethod("purrr__gen")

  local({
    purrr__gen.default <- function(x) x + 1
    expect_identical(compose(~ purrr__gen(.x))(0), 1)
    expect_identical(compose(~ purrr__gen(.x), ~ purrr__gen(.x))(0), 2)

    expect_identical(compose(purrr__gen)(0), 1)
    expect_identical(compose(purrr__gen, purrr__gen)(0), 2)
  })
})

test_that("compose() works with generic functions (#639)", {
  n_unique <- purrr::compose(length, unique)
  expect_identical(n_unique(iris$Species), 3L)
})

test_that("compose() works with argument matching functions", {
  # They inspect their dynamic context via sys.function()
  fn <- function(x = c("foo", "bar")) match.arg(x)
  expect_identical(compose(fn)("f"), "foo")
  expect_identical(compose(fn, fn)("f"), "foo")
})

test_that("compose() works with non-local exits", {
  fn <- function(x) return(x)
  expect_identical(compose(fn)("foo"), "foo")
  expect_identical(compose(fn, fn)("foo"), "foo")
  expect_identical(
    compose(~ return(paste(.x, "foo")), ~ return("bar"))(),
    "bar foo"
  )
})

test_that("compose() preserves lexical environment", {
  fn <- local({
    `_foo` <- "foo"
    function(...) `_foo`
  })
  expect_identical(compose(fn)(), "foo")
  expect_identical(compose(fn, fn)(), "foo")
})

test_that("compose() can take dots from multiple environments", {
  f <- function(...) {
    `_foo` <- "foo"
    g(`_foo`, ...)
  }
  g <- function(...) {
    `_bar` <- "bar"
    h(`_bar`, ...)
  }
  h <- function(...) {
    `_baz` <- "baz"
    fn(`_baz`, ...)
  }
  `_quux` <- "quux"

  # By value
  fn <- compose(function(...) c(...))
  expect_identical(f(`_quux`), c("baz", "bar", "foo", "quux"))

  # By expression (base)
  fn <- compose(function(...) substitute(...()))
  expect_identical(
    f(`_quux`),
    as.pairlist(exprs(`_baz`, `_bar`, `_foo`, `_quux`))
  )

  # By expression (rlang)
  fn <- compose(function(...) enquos(...))
  quos <- f(`_quux`)

  frame <- current_env()
  expect_true(is_reference(quo_get_env(quos[[4]]), frame))
  expect_false(is_reference(quo_get_env(quos[[3]]), frame))

  expect_identical(
    unname(map_chr(quos, as_name)),
    c("_baz", "_bar", "_foo", "_quux")
  )
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/tests/testthat/test-adverb-insistently.R ---
test_that("insistently() resets rate state", {
  fn <- insistently(compose(), rate_delay(1, max_times = 0))
  expect_snapshot_error(fn(), class = "purrr_error_rate_excess")
  expect_snapshot_error(fn(), class = "purrr_error_rate_excess")
})

test_that("validates inputs", {
  expect_snapshot(error = TRUE, {
    insistently(mean, 10)
  })
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/tests/testthat/test-adverb-negate.R ---
test_that("negate works with both functions and vectors", {
  true <- function(...) TRUE
  expect_equal(negate(true)(), FALSE)
  expect_equal(negate("x")(list(x = TRUE)), FALSE)

  expect_equal(negate(is.null)(TRUE), TRUE)
  expect_equal(negate(is.null)(NULL), FALSE)
})

test_that("negate() works with early returns", {
  expect_false(negate(~ return(TRUE))())
})

test_that("negate() works with generic functions and local methods", {
  is_foobar <- function(x) UseMethod("is_foobar")
  local({
    is_foobar.default <- function(x) TRUE
    expect_false(negate(is_foobar)())
  })
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/tests/testthat/test-adverb-partial.R ---
test_that("dots are correctly placed in the signature", {
  out <- partialised_body(partial(runif, n = rpois(1, 5)))
  exp <- expr(runif(n = rpois(1, 5), ...))
  expect_identical(out, exp)
})

test_that("no lazy evaluation means arguments aren't repeatedly evaluated", {
  counter <- env(n = 0)
  lazy <- partial(list, n = {
    counter$n <- counter$n + 1
    NULL
  })
  walk(1:10, ~ lazy())
  expect_identical(counter$n, 10)

  counter <- env(n = 0)
  qq <- partial(
    list,
    n = !!{
      counter$n <- counter$n + 1
      NULL
    }
  )
  walk(1:10, ~ qq())
  expect_identical(counter$n, 1)
})

test_that("partial() still works with functions using `missing()`", {
  fn <- function(x) missing(x)
  expect_false(partial(fn, x = 3)())

  fn <- function(x, y) missing(y)
  expect_true(partial(fn)())
  expect_true(partial(fn, x = 1)())
  expect_false(partial(fn, x = 1, y = 2)())
})

test_that("partialised arguments are evaluated in their environments", {
  n <- 0

  partialised <- local({
    n <- 10
    partial(list, n = n)
  })

  expect_identical(partialised(), list(n = 10))
})

test_that("partialised function is evaluated in its environment", {
  fn <- function(...) stop("tilt")

  partialised <- local({
    fn <- function(x) x
    partial(fn, x = "foo")
  })

  expect_identical(partialised(), "foo")
})

test_that("partial() matches argument with primitives", {
  minus <- partial(`-`, .y = 5)
  expect_identical(minus(1), -4)

  minus <- partial(`-`, e2 = 5)
  expect_identical(minus(1), -4)
})

test_that("partial() squashes quosures before printing", {
  foo <- function(x, y) y
  foo <- partial(foo, y = 3)

  # Reproducible environment tag
  environment(foo) <- global_env()

  expect_snapshot(foo)
})

test_that("partial() handles primitives with named arguments after `...`", {
  expect_identical(partial(min, na.rm = TRUE)(1, NA), 1)
  expect_true(is_na(partial(min, na.rm = FALSE)(1, NA)))
})

test_that("partialised function does not infloop when given the same name (#387)", {
  fn <- function(...) "foo"
  fn <- partial(fn)
  expect_identical(fn(), "foo")
})

test_that("partial() handles `... =` arguments", {
  fn <- function(...) list(...)

  default <- partial(fn, "partial")
  expect_identical(default(1), list("partial", 1))

  after <- partial(fn, "partial", ... = )
  expect_identical(after(1), list("partial", 1))

  before <- partial(fn, ... = , "partial")
  expect_identical(before(1), list(1, "partial"))
})

test_that("partial() supports substituted arguments", {
  fn <- function(x) substitute(x)
  fn <- partial(fn, letters)
  expect_identical(fn(), quote(letters))
})

test_that("partial() supports generics (#647)", {
  expect_identical(partial(mean, na.rm = TRUE)(1), 1)

  foo <- TRUE
  expect_identical(partial(mean, na.rm = foo)(1), 1)
})

test_that("partial() supports lexically defined methods in the def env", {
  local({
    mean.purrr__foobar <- function(...) TRUE
    foobar <- structure(list(), class = "purrr__foobar")

    expect_true(partial(mean, na.rm = TRUE)(foobar))
    expect_true(partial(mean, trim = letters, na.rm = TRUE)(foobar))
  })
})

test_that("substitute() works for both partialised and non-partialised arguments", {
  fn <- function(x, y) list(substitute(x), substitute(y))
  expect_identical(partial(fn, foo)(y = bar), alist(foo, bar))
})

test_that("partial() still supports quosures and multiple environments", {
  arg <- local({
    n <- 0
    quo({
      n <<- n + 1
      n
    })
  })

  x <- "foo"

  fn <- partial(list, !!arg, x = x)
  expect_identical(fn(), list(1, x = "foo"))
  expect_identical(fn(), list(2, x = "foo"))
})

test_that("partial() preserves visibility when arguments are from the same environment (#656)", {
  fn <- partial(identity, 1)
  expect_identical(withVisible(fn()), list(value = 1, visible = TRUE))

  fn <- function(x) invisible(x)
  fn <- partial(fn, 1)
  expect_identical(withVisible(fn()), list(value = 1, visible = FALSE))
})

test_that("checks inputs", {
  expect_snapshot(partial(1), error = TRUE)
})

# helpers -----------------------------------------------------------------

test_that("quo_invert() inverts quosured arguments", {
  call <- expr(list(!!quo(foo), !!quo(bar)))
  expect_identical(quo_invert(call), quo(list(foo, bar)))

  call <- expr(list(foo, !!quo(bar)))
  expect_identical(quo_invert(call), quo(list(foo, bar)))

  call <- expr(list(!!quo(foo), bar))
  expect_identical(quo_invert(call), quo(list(foo, bar)))
})

test_that("quo_invert() detects local quosures", {
  foo <- local(quo(foo))
  call <- expr(list(!!foo, !!quo(bar)))
  expect_identical(
    quo_invert(call),
    new_quosure(expr(list(foo, !!quo(bar))), quo_get_env(foo))
  )

  bar <- local(quo(bar))
  call <- expr(list(!!quo(foo), !!bar))
  expect_identical(quo_invert(call), quo(list(foo, !!bar)))
})

test_that("quo_invert() supports quosures in function position", {
  call <- expr((!!quo(list))(!!quo(foo), !!quo(bar)))
  expect_identical(quo_invert(call), quo(list(foo, bar)))

  fn <- local(quo(list))
  env <- quo_get_env(fn)
  call <- expr((!!fn)(!!quo(foo), !!new_quosure(quote(bar), env)))
  expect_identical(
    quo_invert(call),
    new_quosure(expr(list(!!quo(foo), bar)), env)
  )
})

test_that("quo_invert() supports quosures", {
  bar <- local(quo(bar))
  call <- quo(list(!!quo(foo), !!bar))
  expect_identical(quo_invert(call), quo(list(foo, !!bar)))

  foo <- quo(foo)
  call <- local(quo(list(!!foo, !!bar)))
  expect_identical(
    quo_invert(call),
    new_quosure(expr(list(!!foo, !!bar)), quo_get_env(call))
  )
})

test_that("quo_invert() unwraps constants", {
  call <- expr(foo(!!quo(NULL)))
  expect_identical(quo_invert(call), quote(foo(NULL)))

  foo <- local(quo(foo))
  call <- expr(foo(!!foo, !!quo(NULL)))
  expect_identical(
    quo_invert(call),
    new_quosure(quote(foo(foo, NULL)), quo_get_env(foo))
  )
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/tests/testthat/test-adverb-possibly.R ---
test_that("possibly returns default value on failure", {
  expect_identical(possibly(log, NA_real_)("a"), NA_real_)
})

test_that("possibly emits a message on failure if quiet = FALSE", {
  f <- function(...) stop("tilt")
  expect_message(
    {
      possibly(f, NA_real_, quiet = FALSE)()
    },
    regexp = "tilt"
  )
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/tests/testthat/test-adverb-quietly.R ---
test_that("quietly captures output", {
  f <- function() {
    cat(1)
    message(2, appendLF = FALSE)
    warning(3)
    4
  }
  expect_output(quietly(f)(), NA)
  expect_message(quietly(f)(), NA)
  expect_warning(quietly(f)(), NA)

  out <- quietly(f)()
  expect_equal(
    out,
    list(
      result = 4,
      output = "1",
      warnings = "3",
      messages = "2"
    )
  )
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/tests/testthat/test-adverb-safely.R ---
test_that("safely has NULL error when successful", {
  out <- safely(log10)(10)
  expect_equal(out, list(result = 1, error = NULL))
})

test_that("safely has NULL result on failure", {
  out <- safely(log10)("a")
  expect_equal(out$result, NULL)
  expect_equal(
    out$error$message,
    "non-numeric argument to mathematical function"
  )
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/tests/testthat/test-adverb-slowly.R ---
test_that("validates inputs", {
  expect_snapshot(error = TRUE, {
    slowly(mean, 10)
  })
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/tests/testthat/test-arrays.R ---
x <- array(1:12, c(2, 2, 3), dimnames = list(letters[1:2], LETTERS[1:2], NULL))

test_that("array_branch creates a flat list when no margin specified", {
  expect_length(array_branch(x), 12)
})

test_that("array_branch wraps array in list when margin has length 0", {
  expect_identical(array_branch(x, numeric(0)), list(x))
})

test_that("array_branch works on vectors", {
  expect_identical(array_branch(1:3), list(1L, 2L, 3L))
  expect_identical(array_branch(1:3, 1), list(1L, 2L, 3L))
})

test_that("array_branch throws an error for wrong margins on a vector", {
  expect_snapshot(array_branch(1:3, 2), error = TRUE)
})

test_that("length depends on whether list is flattened or not", {
  m1 <- c(3, 1)
  m2 <- 3
  expect_length(array_branch(x, m1), prod(dim(x)[m1]))
  expect_length(array_tree(x, m1), prod(dim(x)[m2]))
})

test_that("array_branch retains dimnames when going over one dimension", {
  expect_identical(names(array_branch(x, 1)), letters[1:2])
  expect_identical(names(array_branch(x, 2)), LETTERS[1:2])
  expect_identical(names(array_branch(x, 2:3)[[1]]), letters[1:2])
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/tests/testthat/test-coerce.R ---
test_that("can coerce to logical vectors", {
  expect_equal(coerce_lgl(c(TRUE, FALSE, NA)), c(TRUE, FALSE, NA))

  expect_equal(coerce_lgl(c(1L, 0L, NA)), c(TRUE, FALSE, NA))
  expect_snapshot(coerce_lgl(2L), error = TRUE)

  expect_equal(coerce_lgl(c(1, 0, NA)), c(TRUE, FALSE, NA))
  expect_snapshot(coerce_lgl(1.5), error = TRUE)

  expect_snapshot(coerce_lgl("true"), error = TRUE)
})

test_that("can coerce to integer vectors", {
  expect_identical(coerce_int(c(TRUE, FALSE, NA)), c(1L, 0L, NA))

  expect_identical(coerce_int(c(NA, 1L, 10L)), c(NA, 1L, 10L))

  expect_identical(coerce_int(c(NA, 1, 10)), c(NA, 1L, 10L))
  expect_snapshot(coerce_int(1.5), error = TRUE)

  expect_snapshot(coerce_int("1"), error = TRUE)
})

test_that("can coerce to double vctrs", {
  expect_identical(coerce_dbl(c(TRUE, FALSE, NA)), c(1, 0, NA))

  expect_identical(coerce_dbl(c(NA, 1L, 10L)), c(NA, 1, 10))

  expect_identical(coerce_dbl(c(NA, 1.5)), c(NA, 1.5))

  expect_snapshot(coerce_dbl("1.5"), error = TRUE)
})

test_that("can't coerce to character vectors", {
  expect_equal(coerce_chr(NA), NA_character_)

  expect_snapshot(error = TRUE, {
    expect_equal(coerce_chr(TRUE), "TRUE")
    expect_equal(coerce_chr(1L), "1")
    expect_equal(coerce_chr(1.5), "1.500000")
  })

  expect_equal(coerce_chr("x"), "x")
})

test_that("can't coerce to expressions", {
  expect_snapshot(coerce(list(1), "expression"), error = TRUE)
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/tests/testthat/test-conditions.R ---
test_that("stop_bad_type() constructs default `what`", {
  expect_snapshot(stop_bad_type(NA, "`NULL`"), error = TRUE)
  expect_snapshot(stop_bad_type(NA, "`NULL`", arg = ".foo"), error = TRUE)
  expect_snapshot(stop_bad_type(NA, "`NULL`", arg = quote(.foo)), error = TRUE)
})

test_that("stop_bad_element_type() constructs type errors", {
  expect_snapshot(stop_bad_element_type(1:3, 3, "a foobaz"), error = TRUE)
  expect_snapshot(
    stop_bad_element_type(1:3, 3, "a foobaz", actual = "a quux"),
    error = TRUE
  )
  expect_snapshot(
    stop_bad_element_type(1:3, 3, "a foobaz", arg = "..arg"),
    error = TRUE
  )
})

test_that("stop_bad_element_type() accepts `what`", {
  expect_snapshot(
    stop_bad_element_type(1:3, 3, "a foobaz", what = "Result"),
    error = TRUE
  )
})

test_that("stop_bad_element_length() constructs error message", {
  expect_snapshot(stop_bad_element_length(1:3, 8, 10), error = TRUE)
  expect_snapshot(
    stop_bad_element_length(1:3, 8, 10, arg = ".foo"),
    error = TRUE
  )
  expect_snapshot(
    stop_bad_element_length(1:3, 8, 10, arg = ".foo", what = "Result"),
    error = TRUE
  )
  expect_snapshot(
    stop_bad_element_length(
      1:3,
      8,
      10,
      arg = ".foo",
      what = "Result",
      recycle = TRUE
    ),
    error = TRUE
  )
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/tests/testthat/test-deprec-along.R ---
test_that("list-along is deprecated", {
  expect_snapshot({
    . <- list_along(1:4)
  })
})

test_that("list_along works", {
  local_options(lifecycle_verbosity = "quiet")

  x <- 1:5
  expect_identical(list_along(x), vector("list", 5))
})

test_that("rep_along works", {
  local_options(lifecycle_verbosity = "quiet")

  expect_equal(
    rep_along(c("c", "b", "a"), 1:3),
    rep_along(c("d", "f", "e"), 1:3)
  )
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/tests/testthat/test-deprec-cross.R ---
test_that("long format corresponds to expand.grid output", {
  skip_if_not_installed("tibble")
  local_options(lifecycle_verbosity = "quiet")

  x <- list(a = 1:3, b = 4:9)

  out1 <- cross_df(x)
  out2 <- expand.grid(x, KEEP.OUT.ATTRS = FALSE) |> tibble::as_tibble()

  expect_equal(out1, out2)
})

test_that("filtering works", {
  local_options(lifecycle_verbosity = "quiet")
  filter <- function(x, y) x >= y
  out <- cross2(1:3, 1:3, .filter = filter)
  expect_equal(out, list(list(1, 2), list(1, 3), list(2, 3)))
})

test_that("filtering requires a predicate function", {
  local_options(lifecycle_verbosity = "quiet")
  expect_snapshot(cross2(1:3, 1:3, .filter = ~ c(TRUE, TRUE)), error = TRUE)
})

test_that("filtering fails when filter function doesn't return a logical", {
  local_options(lifecycle_verbosity = "quiet")
  filter <- function(x, y, z) x + y + z
  expect_snapshot(cross3(1:3, 1:3, 1:3, .filter = filter), error = TRUE)
})

test_that("works with empty input", {
  local_options(lifecycle_verbosity = "quiet")
  expect_equal(cross(list()), list())
  expect_equal(cross(NULL), NULL)
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/tests/testthat/test-deprec-invoke.R ---
test_that("invoke_* is deprecated", {
  expect_snapshot({
    . <- invoke(identity, 1)
    . <- invoke_map(identity, list())
    . <- invoke_map_lgl(identity, list())
    . <- invoke_map_int(identity, list())
    . <- invoke_map_dbl(identity, list())
    . <- invoke_map_chr(identity, list())
    . <- invoke_map_raw(identity, list())
  })
})

# invoke ------------------------------------------------------------------

test_that("invoke() evaluates expressions in the right environment", {
  local_options(lifecycle_verbosity = "quiet")

  x <- letters
  f <- toupper
  expect_equal(invoke("f", quote(x)), toupper(letters))
})

test_that("invoke() follows promises to find the evaluation env", {
  local_options(lifecycle_verbosity = "quiet")

  x <- letters
  f <- toupper
  f1 <- function(y) {
    f2 <- function(z) purrr::invoke(z, quote(x))
    f2(y)
  }
  expect_equal(f1("f"), toupper(letters))
})

# invoke_map --------------------------------------------------------------

test_that("invoke_map() works with bare function", {
  local_options(lifecycle_verbosity = "quiet")

  data <- list(1:2, 3:4)

  expected <- list("1 2", "3 4")
  expect_equal(invoke_map(paste, data), expected)
  expect_equal(invoke_map("paste", data), expected)
  expect_equal(invoke_map_chr(paste, data), unlist(expected))

  expect_identical(invoke_map_dbl(`+`, data), c(3, 7))
  expect_identical(invoke_map_int(`+`, data), c(3L, 7L))
  expect_identical(invoke_map_lgl(`&&`, data), c(TRUE, TRUE))

  expect_identical(invoke_map_raw(identity, as.raw(1:3)), as.raw(1:3))
})

test_that("invoke_map() works with bare function with data frames", {
  local_options(lifecycle_verbosity = "quiet")
  skip_if_not_installed("dplyr")

  data <- list(1:2, 3:4)
  ops <- set_names(c(`+`, `-`), c("a", "b"))
  expect_identical(invoke_map_dfr(ops, data), invoke_map_dfc(ops, data))
})

test_that("invoke_map() evaluates expressions in the right environment", {
  local_options(lifecycle_verbosity = "quiet")

  shadowed_object <- letters
  shadowed_fun <- toupper
  expect_equal(
    invoke_map("shadowed_fun", list(quote(shadowed_object))),
    list(toupper(letters))
  )
})

test_that("invoke_maps doesn't rely on c() returning list", {
  local_options(lifecycle_verbosity = "quiet")

  day <- as.Date("2016-09-01")
  expect_equal(invoke_map(identity, list(day)), list(day))
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/tests/testthat/test-deprec-lift.R ---
test_that("lift_dl and lift_ld are inverses of each other", {
  options(lifecycle_verbosity = "quiet")

  expect_identical(
    sum |>
      lift_dl(.unnamed = TRUE) |>
      do.call(list(3, NA, 4, na.rm = TRUE)),
    sum |>
      lift_dl() |>
      lift_ld() |>
      exec(3, NA, 4, na.rm = TRUE)
  )
})

test_that("lift_dv is from ... to c(...)", {
  options(lifecycle_verbosity = "quiet")

  expect_equal(lift_dv(range, .unnamed = TRUE)(1:10), c(1, 10))
})

test_that("lift_vd is from c(...) to ...", {
  options(lifecycle_verbosity = "quiet")

  expect_equal(lift_vd(mean)(1, 2), 1.5)
})

test_that("lift_vl is from c(...) to list(...)", {
  options(lifecycle_verbosity = "quiet")

  expect_equal(lift_vl(mean)(list(1, 2)), 1.5)
})

test_that("lift_lv is from list(...) to c(...)", {
  options(lifecycle_verbosity = "quiet")

  glue <- function(l) {
    if (!is.list(l)) {
      stop("not a list")
    }
    do.call(paste, l)
  }
  expect_identical(lift_lv(glue)(letters), paste(letters, collapse = " "))
})


test_that("lift functions are deprecated", {
  expect_snapshot({
    . <- lift_dl(function() {})
    . <- lift_dv(function() {})
    . <- lift_vl(function() {})
    . <- lift_vd(function() {})
    . <- lift_ld(function() {})
    . <- lift_lv(function() {})
  })
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/tests/testthat/test-deprec-prepend.R ---
test_that("prepend is deprecated", {
  expect_snapshot({
    . <- prepend(1, 2)
  })
})

test_that("prepend is clearer version of merging with c()", {
  local_options(lifecycle_verbosity = "quiet")

  x <- 1:3
  expect_identical(
    x %>% prepend(4),
    x %>% c(4, .)
  )
  expect_identical(
    x %>% prepend(4, before = 3),
    x %>%
      {
        c(.[1:2], 4, .[3])
      }
  )
})

test_that("prepend appends at the beginning for empty list by default", {
  local_options(lifecycle_verbosity = "quiet")

  x <- list()
  expect_identical(
    x %>% prepend(1),
    x %>% c(1, .)
  )
})

test_that("prepend throws error if before param is neither NULL nor between 1 and length(x)", {
  local_options(lifecycle_verbosity = "quiet")

  expect_snapshot(prepend(list(), 1, before = 1), error = TRUE)
  x <- as.list(1:3)
  expect_snapshot(x %>% prepend(4, before = 0), error = TRUE)
  expect_snapshot(x %>% prepend(4, before = 4), error = TRUE)
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/tests/testthat/test-deprec-rerun.R ---
test_that("is deprecated", {
  expect_snapshot({
    . <- rerun(5, rnorm(1))
    . <- rerun(5, rnorm(1), rnorm(2))
  })
})

test_that("single unnamed arg doesn't get extra list", {
  local_options(lifecycle_verbosity = "quiet")
  expect_equal(rerun(2, 1), list(1, 1))
})

test_that("single named arg gets extra list", {
  local_options(lifecycle_verbosity = "quiet")
  expect_equal(rerun(2, a = 1), list(list(a = 1), list(a = 1)))
})

test_that("every run is different", {
  local_options(lifecycle_verbosity = "quiet")
  x <- rerun(2, runif(1))
  expect_true(x[[1]] != x[[2]])
})

test_that("rerun uses scope of expression", {
  local_options(lifecycle_verbosity = "quiet")
  f <- function(n) {
    rerun(1, x = seq_len(n))
  }

  expect_equal(f(10)[[1]]$x, 1:10)
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/tests/testthat/test-deprec-splice.R ---
test_that("predicate controls which elements get spliced", {
  x <- list(1, 2, list(3, 4))

  expect_equal(splice_if(x, ~FALSE), x)
  expect_equal(splice_if(x, is.list), list(1, 2, 3, 4))
})

test_that("splice() produces correctly named lists", {
  local_options(lifecycle_verbosity = "quiet")
  inputs <- list(arg1 = "a", arg2 = "b")

  out1 <- splice(inputs, arg3 = c("c1", "c2"))
  expect_named(out1, c("arg1", "arg2", "arg3"))

  out2 <- splice(inputs, arg = list(arg3 = 1, arg4 = 2))
  expect_named(out2, c("arg1", "arg2", "arg3", "arg4"))
})

test_that("splice is deprecated", {
  expect_snapshot({
    . <- splice()
  })
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/tests/testthat/test-deprec-utils.R ---
test_that("rdunif and rbernoulli are deprecated", {
  expect_snapshot({
    . <- rdunif(10, 1)
    . <- rbernoulli(10)
  })
})

test_that("rbernoulli is a special case of rbinom", {
  local_options(lifecycle_verbosity = "quiet")

  set.seed(1)
  x <- rbernoulli(10)

  set.seed(1)
  y <- ifelse(rbinom(10, 1, 0.5) == 1, TRUE, FALSE)

  expect_equal(x, y)
})

test_that("rdunif works", {
  local_options(lifecycle_verbosity = "quiet")

  expect_length(rdunif(100, 10), 100)
})

test_that("rdunif fails if a and b are not unit length numbers", {
  local_options(lifecycle_verbosity = "quiet")

  expect_snapshot(rdunif(1000, 1, "a"), error = TRUE)
  expect_snapshot(rdunif(1000, 1, c(0.5, 0.2)), error = TRUE)
  expect_snapshot(rdunif(1000, FALSE, 2), error = TRUE)
  expect_snapshot(rdunif(1000, c(2, 3), 2), error = TRUE)
})


# Lifecycle ---------------------------------------------------------------

test_that("%@% is an infix attribute accessor", {
  local_options(lifecycle_verbosity = "quiet")
  expect_identical(mtcars %@% "names", attr(mtcars, "names"))
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/tests/testthat/test-deprec-when.R ---
test_that("when is deprecated", {
  expect_snapshot({
    . <- when(1:5 < 3 ~ 1, ~0)
  })
})

test_that("when chooses the correct action", {
  local_options(lifecycle_verbosity = "quiet")

  x <-
    1:5 |>
    when(
      sum(.) <= 50 ~ sum(.),
      sum(.) <= 100 ~ sum(.) / 2,
      ~0
    )

  expect_equal(x, 15)

  y <-
    1:10 |>
    when(
      sum(.) <= 50 ~ sum(.),
      sum(.) <= 100 ~ sum(.) / 2,
      ~0
    )

  expect_equal(y, sum(1:10) / 2)

  z <-
    1:100 |>
    when(
      sum(.) <= 50 ~ sum(.),
      sum(.) <= 100 ~ sum(.) / 2,
      ~0
    )

  expect_equal(z, 0)
})

test_that("named arguments work with when", {
  local_options(lifecycle_verbosity = "quiet")

  x <-
    1:10 |>
    when(
      sum(.) <= x ~ sum(.) * x,
      sum(.) <= 2 * x ~ sum(.) * x / 2,
      ~0,
      x = 60
    )

  expect_equal(x, sum(1:10) * 60)
})

test_that("default values work without a formula", {
  local_options(lifecycle_verbosity = "quiet")

  x <- iris |>
    subset(Sepal.Length > 10) |>
    when(
      nrow(.) > 0 ~ .,
      head(iris, 10)
    )

  expect_equal(x, head(iris, 10))
})

test_that("error when named arguments have no matching conditions", {
  local_options(lifecycle_verbosity = "quiet")

  expect_snapshot(1:5 |> when(a = sum(.) < 5 ~ 3), error = TRUE)
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/tests/testthat/test-detect.R ---
y <- 4:10

test_that("detect functions work", {
  is_odd <- function(x) x %% 2 == 1
  expect_equal(detect(y, is_odd), 5)
  expect_equal(detect_index(y, is_odd), 2)
  expect_equal(detect(y, is_odd, .dir = "backward"), 9)
  expect_equal(detect_index(y, is_odd, .dir = "backward"), 6)
})

test_that("detect returns NULL when match not found", {
  expect_null(detect(y, function(x) x > 11))
})

test_that("detect_index returns 0 when match not found", {
  expect_equal(detect_index(y, function(x) x > 11), 0)
})

test_that("has_element checks whether a list contains an object", {
  expect_true(has_element(list(1, 2), 1))
  expect_false(has_element(list(1, 2), 3))
})

test_that("`detect()` requires a predicate function", {
  expect_snapshot(detect(list(1:2, 2), is.na), error = TRUE)
  expect_snapshot(detect_index(list(1:2, 2), is.na), error = TRUE)
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/tests/testthat/test-every-some-none.R ---
test_that("every returns TRUE if all elements are TRUE", {
  x <- list(0, 1, TRUE)
  expect_false(every(x, isTRUE))
  expect_true(every(x[3], isTRUE))
})

test_that("some returns FALSE if all elements are FALSE", {
  x <- list(1, 0, FALSE)
  expect_false(some(x, isTRUE))
  expect_true(some(x[1], negate(isTRUE)))
})

test_that("none returns TRUE if all elements are FALSE", {
  x <- list(1, 0, TRUE)
  expect_false(none(x, isTRUE))
  expect_true(none(x[1], isTRUE))
})

test_that("every() has the same behaviour as `&&` (#751)", {
  expect_false(every(list(NA, FALSE), identity))
  expect_false(every(list(FALSE, NA), identity))

  expect_identical(every(list(NA, TRUE), identity), NA)
  expect_identical(every(list(TRUE, NA), identity), NA)
  expect_identical(every(list(NA, NA), identity), NA)
})

test_that("some() has the same behaviour as `||`", {
  expect_true(some(list(TRUE, NA), identity))
  expect_true(some(list(NA, TRUE), identity))

  expect_identical(some(list(NA, FALSE), identity), NA)
  expect_identical(some(list(FALSE, NA), identity), NA)
  expect_identical(some(list(NA, NA), identity), NA)
})

test_that("every(), some(), and none() have correct empty size behavior", {
  # All pass
  expect_identical(every(list(), identity), all(list()))
  # All don't pass
  expect_identical(none(list(), identity), all(list()))
  # Any pass
  expect_identical(some(list(), identity), any(list()))
})

test_that("every(), some(), and none() work on `NULL`", {
  # All pass
  expect_identical(every(NULL, identity), all(NULL))
  # All don't pass
  expect_identical(none(NULL, identity), all(NULL))
  # Any pass
  expect_identical(some(NULL, identity), any(NULL))
})

test_that("every(), some(), and none() have correct early stopping behavior", {
  expect_identical(every(list(TRUE, FALSE, TRUE), identity), FALSE)
  expect_identical(none(list(FALSE, TRUE, FALSE), identity), FALSE)
  expect_identical(some(list(FALSE, TRUE, FALSE), identity), TRUE)
})

test_that("every(), some(), and none() have correct `NA` propagation behavior", {
  # Propagates through non-early-stopping case
  expect_identical(every(list(NA, TRUE), identity), NA)
  expect_identical(none(list(NA, FALSE), identity), NA)
  expect_identical(some(list(NA, FALSE), identity), NA)

  # Overruled by early-stopping case
  expect_identical(every(list(NA, FALSE), identity), FALSE)
  expect_identical(none(list(NA, TRUE), identity), FALSE)
  expect_identical(some(list(NA, TRUE), identity), TRUE)
})

test_that("every(), some(), and none() require logical scalar predicate results", {
  # No coercion to `TRUE` or `FALSE`
  expect_snapshot(every(list(1), function(x) 1), error = TRUE)
  expect_snapshot(some(list(1), function(x) 1), error = TRUE)
  expect_snapshot(none(list(1), function(x) 1), error = TRUE)

  # `NA` must be a logical `NA`, no coercion happens for `TRUE` or `FALSE`,
  # so we also don't coerce `NA`s of any other kind
  expect_snapshot(every(list(1), function(x) NA_integer_), error = TRUE)
  expect_snapshot(some(list(1), function(x) NA_integer_), error = TRUE)
  expect_snapshot(none(list(1), function(x) NA_integer_), error = TRUE)

  # Must be length 1
  expect_snapshot(every(list(1), function(x) c(TRUE, FALSE)), error = TRUE)
  expect_snapshot(some(list(1), function(x) c(TRUE, FALSE)), error = TRUE)
  expect_snapshot(none(list(1), function(x) c(TRUE, FALSE)), error = TRUE)

  # Attributes are allowed, we ignore them
  expect_true(every(list(1), function(x) structure(TRUE, foo = "bar")))
  expect_true(some(list(1), function(x) structure(TRUE, foo = "bar")))
  expect_false(none(list(1), function(x) structure(TRUE, foo = "bar")))

  # Classes are allowed for historical reasons.
  # We probably wouldn't consider these to be logical scalars these days.
  expect_true(every(list(1), function(x) structure(TRUE, class = "mylgl")))
  expect_true(some(list(1), function(x) structure(TRUE, class = "mylgl")))
  expect_false(none(list(1), function(x) structure(TRUE, class = "mylgl")))

  # We bypass any S3 `length()` methods!
  local_methods(length.mylgl = function(x) 2L)
  expect_true(every(list(1), function(x) structure(TRUE, class = "mylgl")))
  expect_true(some(list(1), function(x) structure(TRUE, class = "mylgl")))
  expect_false(none(list(1), function(x) structure(TRUE, class = "mylgl")))
})

test_that("every(), some(), and none() require vector `.x`", {
  expect_snapshot(every(function() 1, identity), error = TRUE)
  expect_snapshot(some(function() 1, identity), error = TRUE)
  expect_snapshot(none(function() 1, identity), error = TRUE)
})

test_that("every(), some(), and none() work on atomic vectors", {
  expect_identical(every(1:3, is.integer), TRUE)
  expect_identical(none(1:3, is.integer), FALSE)
  expect_identical(some(1:3, is.integer), TRUE)
})

test_that("every(), some(), and none() work colwise across data frames", {
  # If it naively worked off `vec_size()` then extracted elements with `[[`,
  # this would return incorrect results. This definition is consistent with
  # `map()`.
  df <- data_frame(a = 1L, b = 2)
  expect_identical(every(df, is.integer), FALSE)
  expect_identical(none(df, is.double), FALSE)
  expect_identical(some(df, is.double), TRUE)
})

test_that("every(), some(), and none() work on list scalars", {
  # For consistency with `map()`
  obj <- structure(list(1, "x"), class = "my_scalar")
  expect_identical(every(obj, is.double), FALSE)
  expect_identical(none(obj, is.character), FALSE)
  expect_identical(some(obj, is.character), TRUE)
})

test_that("every(), some(), and none() work with vctrs records", {
  x <- new_rcrd(list(x = c(1, 2, 3), y = c("a", "b", "c")))

  out <- list()
  every(x, function(elt) {
    out <<- append(out, list(elt))
    TRUE
  })
  expect_identical(out, vec_chop(x))

  out <- list()
  some(x, function(elt) {
    out <<- append(out, list(elt))
    FALSE
  })
  expect_identical(out, vec_chop(x))

  out <- list()
  none(x, function(elt) {
    out <<- append(out, list(elt))
    FALSE
  })
  expect_identical(out, vec_chop(x))
})

test_that("pairlists, expressions, and calls are deprecated but work", {
  local_options(lifecycle_verbosity = "warning")

  expect_snapshot(out <- every(expression(1, 2), is.double))
  expect_true(out)

  expect_snapshot(out <- every(pairlist(1, 2), is.double))
  expect_true(out)

  expect_snapshot(x <- every(quote(f(a, b)), is.name))
  expect_true(out)
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/tests/testthat/test-head-tail.R ---
y <- 1:100

test_that("head_while works", {
  expect_length(head_while(y, function(x) x <= 15), 15)
})

test_that("tail_while works", {
  expect_length(tail_while(y, function(x) x >= 86), 15)
})

test_that("original vector returned if predicate satisfied by all elements", {
  expect_identical(head_while(y, function(x) x <= 100), y)
  expect_identical(tail_while(y, function(x) x >= 0), y)
})

test_that("head_while and tail_while require predicate function", {
  expect_snapshot(head_while(1:3, ~NA), error = TRUE)
  expect_snapshot(tail_while(1:3, ~ c(TRUE, FALSE)), error = TRUE)
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/tests/testthat/test-imap.R ---
x <- 1:3 |> set_names()

test_that("imap is special case of map2", {
  expect_identical(imap(x, paste), map2(x, names(x), paste))
})

test_that("imap always returns a list", {
  expect_bare(imap(x, paste), "list")
})

test_that("atomic vector imap works", {
  expect_true(all(imap_lgl(x, `==`)))
  expect_length(imap_chr(x, paste), 3)
  expect_equal(imap_int(x, ~ .x + as.integer(.y)), x * 2)
  expect_equal(imap_dbl(x, ~ .x + as.numeric(.y)), x * 2)
  expect_equal(imap_vec(x, ~ .x + as.numeric(.y)), x * 2)
})

test_that("iwalk returns invisibly", {
  expect_output(iwalk(mtcars, ~ cat(.y, ": ", median(.x), "\n", sep = "")))
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/tests/testthat/test-keep.R ---
test_that("can keep/discard with logical vector", {
  expect_equal(keep(1:3, c(TRUE, FALSE, TRUE)), c(1, 3))
  expect_equal(discard(1:3, c(TRUE, FALSE, TRUE)), 2)
})

test_that("can keep/discard with predicate", {
  expect_equal(keep(1:3, ~ .x != 2), c(1, 3))
  expect_equal(discard(1:3, ~ .x != 2), c(2))
})

test_that("keep() and discard() require predicate functions", {
  expect_snapshot(error = TRUE, {
    keep(1:3, ~NA)
    discard(1:3, ~NA)
  })
})

# keep_at / discard_at ----------------------------------------------------

test_that("can keep_at/discard_at with character vector", {
  x <- list(a = 1, b = 1, c = 1)
  expect_equal(keep_at(x, "b"), list(b = 1))
  expect_equal(discard_at(x, "b"), list(a = 1, c = 1))
})

test_that("can keep_at/discard_at with function", {
  x <- list(a = 1, b = 1, c = 1)
  expect_equal(keep_at(x, ~ . == "b"), list(b = 1))
  expect_equal(discard_at(x, ~ . == "b"), list(a = 1, c = 1))
})

test_that("discard_at works when nothing discarded", {
  x <- list(a = 1, b = 1, c = 1)
  expect_equal(discard_at(x, "d"), x)
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/tests/testthat/test-list-combine.R ---
test_that("list_c() concatenates vctrs of compatible types", {
  expect_identical(list_c(list(1L, 2:3)), c(1L, 2L, 3L))
  expect_identical(list_c(list(1, 2:3)), c(1, 2, 3))

  expect_snapshot(error = TRUE, list_c(list("a", 1)))
})

test_that("list_c() can enforce ptype", {
  expect_snapshot(error = TRUE, list_c(list("a"), ptype = integer()))
})

test_that("list_c() strips outer names and preserves inner names (#997)", {
  expect_equal(list_c(list(x = 1:2, y = 3:4)), 1:4)
  expect_equal(list_c(list(c(a = 1), c(b = 2))), c(a = 1, b = 2))
})

test_that("list_cbind() column-binds compatible data frames", {
  df1 <- data.frame(x = 1:2)
  df2 <- data.frame(y = 1:2)
  df3 <- data.frame(z = 1:3)

  expect_equal(list_cbind(list(df1, df2)), data.frame(x = 1:2, y = 1:2))
  expect_snapshot(error = TRUE, {
    list_cbind(list(df1, df3))
  })
})

test_that("list_cbind() can enforce size", {
  df1 <- data.frame(x = 1:2)
  expect_snapshot(error = TRUE, {
    list_cbind(list(df1), size = 3)
  })
})

test_that("list_rbind() row-binds compatible data.frames", {
  df1 <- data.frame(x = 1)
  df2 <- data.frame(x = 2, y = 1)
  df3 <- data.frame(x = "a", stringsAsFactors = FALSE)

  expect_equal(list_rbind(list(df1, df2)), data.frame(x = 1:2, y = c(NA, 1)))

  # and names don't make a difference unless `names_to` is set
  out <- list_rbind(list(a = df1, b = df2))
  expect_equal(out, data.frame(x = c(1, 2), y = c(NA, 1)))

  expect_snapshot(error = TRUE, {
    list_rbind(list(df1, df3))
  })
})

test_that("list_rbind() can enforce ptype", {
  df1 <- data.frame(x = 1)

  expect_snapshot(error = TRUE, {
    ptype <- data.frame(x = character(), stringsAsFactors = FALSE)
    list_rbind(list(df1), ptype = ptype)
  })
})

test_that("NULLs are ignored", {
  df1 <- data.frame(x = 1)
  df2 <- data.frame(y = 1)

  expect_equal(list_c(list(1, NULL, 2)), c(1, 2))
  expect_equal(list_rbind(list(df1, NULL, df1)), vec_rbind(df1, df1))
  expect_equal(list_cbind(list(df1, NULL, df2)), vec_cbind(df1, df2))
})

test_that("empty inputs return expected output", {
  expect_equal(list_c(list()), NULL)
  expect_equal(list_c(list(NULL)), NULL)

  expect_equal(list_rbind(list()), data.frame())
  expect_equal(list_rbind(list(NULL)), data.frame())
  expect_equal(list_cbind(list()), data.frame())
  expect_equal(list_cbind(list(NULL)), data.frame())
})

test_that("assert input is a list", {
  expect_snapshot(error = TRUE, {
    list_c(1)
    list_rbind(1)
    list_cbind(1)
  })

  # and not just built on a list
  expect_snapshot(error = TRUE, {
    list_c(mtcars)
    list_rbind(mtcars)
    list_cbind(mtcars)
  })
})

test_that("assert input is list of data frames", {
  expect_snapshot(error = TRUE, {
    list_rbind(list(1, mtcars, 3))
    list_cbind(list(1, mtcars, 3))
  })
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/tests/testthat/test-list-flatten.R ---
test_that("flattening removes single layer of nesting", {
  expect_equal(list_flatten(list(list(1), list(2))), list(1, 2))
  expect_equal(list_flatten(list(list(1), list(list(2)))), list(1, list(2)))
  expect_equal(list_flatten(list(list(1), list(), list(2))), list(1, 2))
})

test_that("flattening a flat list is idempotent", {
  expect_equal(list_flatten(list(1, 2)), list(1, 2))
})

test_that("uses either inner or outer names if only one present", {
  expect_equal(list_flatten(list(x = list(1), list(y = 2))), list(x = 1, y = 2))
})

test_that("can control names if both present", {
  x <- list(a = list(x = 1))
  expect_equal(list_flatten(x), list(a_x = 1))
  expect_equal(list_flatten(x, name_spec = "{inner}"), list(x = 1))
  expect_equal(list_flatten(x, name_spec = "{outer}"), list(a = 1))
})

test_that("requires a list", {
  expect_snapshot(list_flatten(1:2), error = TRUE)
})

test_that("list_flatten() restores", {
  # This simulates a recursive list-of type
  my_num_list <- function(...) {
    new_my_num_list(list2(...))
  }
  new_my_num_list <- function(xs) {
    stopifnot(
      every(xs, function(x) {
        is_null(x) || is.numeric(x) || inherits(x, "my_num_list")
      })
    )
    new_vctr(xs, class = "my_num_list")
  }

  local_methods(
    vec_restore.my_num_list = function(x, to, ...) {
      new_my_num_list(x)
    }
  )

  xs <- my_num_list(1, 2, my_num_list(3:4))
  expect_equal(
    list_flatten(xs),
    my_num_list(1, 2, 3:4)
  )
})

test_that("list_flatten() supports strict types", {
  local_methods(
    vec_cast.list.my_strict_list = function(x, to, ...) {
      abort("Can't coerce to list.")
    }
  )

  x <- structure(list(1), class = c("my_strict_list", "list"))

  expect_equal(
    list_flatten(list(x)),
    list(1)
  )
})

test_that("list_flatten() works with vctrs::list_of()", {
  # Currently only with flat lists because list_of can't be recursive
  expect_equal(
    list_flatten(list_of(1, 2, 3)),
    list_of(1, 2, 3)
  )
})

test_that("list_flatten() honors its is_node param", {
  expect_equal(list_flatten(list(mtcars)), list(mtcars))
  expect_equal(list_flatten(list(mtcars), is_node = is.list), as.list(mtcars))
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/tests/testthat/test-list-modify.R ---
# list_assign -------------------------------------------------------------

test_that("can modify named lists by name or position", {
  expect_equal(list_assign(list(a = 1), b = 2), list(a = 1, b = 2))
  expect_equal(list_assign(list(a = 1), a = 2), list(a = 2))
  expect_equal(list_assign(list(a = 1), a = NULL), list(a = NULL))
  expect_equal(list_assign(list(a = 1, b = 2), b = zap()), list(a = 1))

  expect_equal(list_assign(list(a = 1), 2), list(a = 2))
  expect_equal(list_assign(list(a = 1, b = 2), zap()), list(b = 2))
})

test_that("can modify unnamed lists by name or position", {
  expect_equal(list_assign(list(3), 1, 2), list(1, 2))
  expect_equal(list_assign(list(3), NULL), list(NULL))
  expect_equal(list_assign(list(3), zap()), list())
  expect_equal(list_assign(list(3), zap(), zap()), list())

  expect_equal(list_assign(list(1), a = 2), list(1, a = 2))
  expect_equal(list_assign(list(1), a = NULL), list(1, a = NULL))
  expect_equal(list_assign(list(1), a = zap()), list(1))
})

test_that("doesn't replace recursively", {
  x <- list(y = list(a = 1))
  expect_equal(list_assign(x, y = list(b = 1)), list(y = list(b = 1)))
})

# list_modify -------------------------------------------------------------

test_that("named lists have values replaced by name", {
  expect_equal(list_modify(list(a = 1), b = 2), list(a = 1, b = 2))
  expect_equal(list_modify(list(a = 1), a = 2), list(a = 2))
  expect_equal(list_modify(list(a = 1), a = NULL), list(a = NULL))
  expect_equal(list_modify(list(a = 1, b = 2), b = zap()), list(a = 1))
})

test_that("unnamed lists are replaced by position", {
  expect_equal(list_modify(list(3), 1, 2), list(1, 2))
  expect_equal(list_modify(list(3), NULL), list(NULL))

  expect_equal(list_modify(list(3), zap()), list())
  expect_equal(list_modify(list(3), zap(), zap()), list())

  expect_equal(list_modify(list(1, 2, 3), 4), list(4, 2, 3))
})

test_that("can update unnamed lists with named inputs", {
  expect_identical(list_modify(list(1), a = 2), list(1, a = 2))
  expect_identical(list_modify(list(1), a = NULL), list(1, a = NULL))
  expect_identical(list_modify(list(1), a = zap()), list(1))
})

test_that("can update named lists with unnamed inputs", {
  expect_identical(list_modify(list(a = 1, b = 2), 2), list(a = 2, b = 2))
  expect_identical(list_modify(list(a = 1, b = 2), zap()), list(b = 2))
  expect_identical(
    list_modify(list(a = 1, b = 2), 2, 3, 4),
    list(a = 2, b = 3, 4)
  )
})

test_that("lists are replaced recursively", {
  expect_equal(
    list_modify(
      list(a = list(x = 1)),
      a = list(x = 2),
    ),
    list(a = list(x = 2))
  )

  expect_equal(
    list_modify(
      list(a = list(x = 1)),
      a = list(y = 2)
    ),
    list(a = list(x = 1, y = 2))
  )
})

test_that("but data.frames are not", {
  x1 <- list(x = data.frame(x = 1))
  x2 <- list(x = data.frame(y = 2))
  out <- list_modify(x1, !!!x2)
  expect_equal(out, x2)

  # unless you really want it
  out <- list_modify(x1, !!!x2, .is_node = is.list)
  expect_equal(out, list(x = data.frame(x = 1, y = 2)))
})

test_that("list_modify() validates inputs", {
  expect_snapshot(list_modify(1:3), error = TRUE)
  expect_snapshot(list_modify(list(a = 1), 2, a = 2), error = TRUE)
  expect_snapshot(list_modify(list(x = 1), x = 2, x = 3), error = TRUE)
})

test_that("list_modify() preserves class & attributes", {
  x <- structure(list(a = 1, b = 2), x = 10, class = "foo")
  expect_equal(
    list_modify(x, a = 10, b = 20),
    structure(list(a = 10, b = 20), x = 10, class = "foo")
  )
})

# list_merge --------------------------------------------------------------

test_that("list_merge concatenates values from two lists", {
  l1 <- list(x = 1:10, y = 4, z = list(a = 1, b = 2))
  l2 <- list(x = 11, z = list(a = 2:5, c = 3))
  l <- list_merge(l1, !!!l2)
  expect_equal(l$x, c(l1$x, l2$x))
  expect_equal(l$y, c(l1$y, l2$y))
  expect_equal(l$z$a, c(l1$z$a, l2$z$a))
  expect_equal(l$z$b, c(l1$z$b, l2$z$b))
  expect_equal(l$z$c, c(l1$z$c, l2$z$c))
})

test_that("list_merge concatenates without needing names", {
  l1 <- list(1:10, 4, list(1, 2))
  l2 <- list(11, 5, list(2:5, 3))
  expect_length(list_merge(l1, !!!l2), 3)
})

test_that("list_merge returns the non-empty list", {
  expect_equal(list_merge(list(3)), list(3))
  expect_equal(list_merge(list(), 2), list(2))
})

test_that("merge() validates inputs", {
  expect_snapshot(list_merge(1:3), error = TRUE)
  expect_snapshot(list_merge(list(x = 1), x = 2, x = 3), error = TRUE)
})

# update_list ------------------------------------------------------------

test_that("update_list() is deprecated", {
  expect_snapshot({
    . <- update_list(list())
  })
})

test_that("can modify element called x", {
  local_options(lifecycle_verbosity = "quiet")
  expect_equal(update_list(list(), x = 1), list(x = 1))
})

test_that("quosures and formulas are evaluated", {
  local_options(lifecycle_verbosity = "quiet")
  expect_identical(update_list(list(x = 1), y = quo(x + 1)), list(x = 1, y = 2))
  expect_identical(update_list(list(x = 1), y = ~ x + 1), list(x = 1, y = 2))
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/tests/testthat/test-list-simplify.R ---
test_that("simplifies using vctrs principles", {
  expect_identical(list_simplify(list(1, 2L)), c(1, 2))
  expect_equal(list_simplify(list("x", factor("y"))), c("x", "y"))

  x <- list(data.frame(x = 1), data.frame(y = 2))
  expect_equal(list_simplify(x), data.frame(x = c(1, NA), y = c(NA, 2)))
})

test_that("only uses outer names", {
  out <- list_simplify(list(a = 1, c(b = 1), c = c(d = 1)))
  expect_named(out, c("a", "", "c"))
})

test_that("empty lists simplify to NULL", {
  expect_equal(list_simplify(list()), NULL)
  expect_equal(list_simplify(set_names(list())), NULL)
})

test_that("ptype is enforced", {
  expect_equal(list_simplify(list(1, 2), ptype = double()), c(1, 2))
  expect_snapshot(list_simplify(list(1, 2), ptype = character()), error = TRUE)
  # even if `strict = FALSE`
  expect_snapshot(
    list_simplify(list(1, 2), ptype = character(), strict = FALSE),
    error = TRUE
  )
})

test_that("strict simplification will error", {
  expect_snapshot(error = TRUE, {
    list_simplify(list(mean))
    list_simplify(list(1, "a"))
    list_simplify(list(1, 1:2))
    list_simplify(list(data.frame(x = 1), data.frame(x = 1:2)))
    list_simplify(list(1, 2), ptype = character())
  })
})

test_that("simplification requires length-1 vectors with common type", {
  expect_equal(list_simplify(list(mean), strict = FALSE), list(mean))
  expect_equal(list_simplify(list(1, 2:3), strict = FALSE), list(1, 2:3))
  expect_equal(list_simplify(list(1, "a"), strict = FALSE), list(1, "a"))
})

# argument checking -------------------------------------------------------

test_that("list_simplify() validates inputs", {
  expect_snapshot(list_simplify(1:5), error = TRUE)
  expect_snapshot(list_simplify(list(), strict = NA), error = TRUE)
})

test_that("list_simplify_internal() validates inputs", {
  expect_snapshot(list_simplify_internal(list(), simplify = 1), error = TRUE)
  expect_snapshot(
    list_simplify_internal(list(), simplify = FALSE, ptype = integer()),
    error = TRUE
  )
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/tests/testthat/test-list-transpose.R ---
test_that("can transpose homogenous list", {
  x <- list(x = list(a = 1, b = 2), y = list(a = 3, b = 4))
  out <- list_transpose(x)
  expect_equal(out, list(a = c(x = 1, y = 3), b = c(x = 2, y = 4)))
})

test_that("can't transpose data frames", {
  df <- data.frame(x = 1:2, y = 4:5)

  # i.e. be consistent with other `list_*()` functions from purrr/vctrs
  expect_snapshot(error = TRUE, list_transpose(df))
})

test_that("transposing empty list returns empty list", {
  expect_equal(list_transpose(list()), list())
})

test_that("can use character template", {
  x <- list(list(a = 1, b = 2), list(b = 3, c = 4))
  # Default:
  expect_equal(
    list_transpose(x, default = NA),
    list(a = c(1, NA), b = c(2, 3), c = c(NA, 4))
  )

  # Change order
  expect_equal(
    list_transpose(x, template = c("b", "a"), default = NA),
    list(b = c(2, 3), a = c(1, NA))
  )
  # Remove
  expect_equal(
    list_transpose(x, template = "b", default = NA),
    list(b = c(2, 3))
  )
  # Add
  expect_equal(
    list_transpose(x, template = c("a", "b", "c"), default = NA),
    list(a = c(1, NA), b = c(2, 3), c = c(NA, 4))
  )
})

test_that("can use integer template", {
  x <- list(list(1, 2, 3), list(4, 5))
  # Default:
  expect_equal(
    list_transpose(x, default = NA),
    list(c(1, 4), c(2, 5), c(3, NA))
  )

  # Change order
  expect_equal(
    list_transpose(x, template = c(3, 2, 1), default = NA),
    list(c(3, NA), c(2, 5), c(1, 4))
  )
  # Remove
  expect_equal(
    list_transpose(x, template = 2, default = NA),
    list(c(2, 5))
  )
  # Add
  expect_equal(
    list_transpose(x, template = 1:4, default = NA),
    list(c(1, 4), c(2, 5), c(3, NA), c(NA, NA))
  )
})

test_that("integer template requires exact length of list() simplify etc", {
  x <- list(list(1, 2), list(3, 4))

  expect_snapshot(list_transpose(x, ptype = list()), error = TRUE)
  expect_snapshot(list_transpose(x, ptype = list(integer())), error = TRUE)
  expect_identical(
    list_transpose(x, ptype = list(integer(), integer())),
    list(c(1L, 3L), c(2L, 4L))
  )
})

test_that("simplification fails silently unless requested", {
  expect_equal(
    list_transpose(list(list(x = 1), list(x = "b"))),
    list(x = list(1, "b"))
  )
  expect_equal(
    list_transpose(list(list(x = 1), list(x = 2:3))),
    list(x = list(1, 2:3))
  )

  expect_snapshot(error = TRUE, {
    list_transpose(list(list(x = 1), list(x = "b")), simplify = TRUE)
    list_transpose(list(list(x = 1), list(x = 2:3)), simplify = TRUE)
  })
})

test_that("can supply `simplify` globally or individually", {
  x <- list(list(a = 1, b = 2), list(a = 3, b = 4))
  expect_equal(
    list_transpose(x, simplify = FALSE),
    list(a = list(1, 3), b = list(2, 4))
  )
  expect_equal(
    list_transpose(x, simplify = list(a = FALSE)),
    list(a = list(1, 3), b = c(2, 4))
  )
  expect_snapshot(list_transpose(x, simplify = list(c = FALSE)), error = TRUE)
})

test_that("can supply `ptype` globally or individually", {
  x <- list(list(a = 1, b = 2), list(a = 3, b = 4))
  expect_identical(
    list_transpose(x, ptype = integer()),
    list(a = c(1L, 3L), b = c(2L, 4L))
  )
  expect_identical(
    list_transpose(x, ptype = list(a = integer())),
    list(a = c(1L, 3L), b = c(2, 4))
  )
  expect_snapshot(list_transpose(x, ptype = list(c = integer())), error = TRUE)
})

test_that("can supply `default` globally or individually", {
  x <- list(list(x = 1), list(y = "a"))
  expect_equal(
    list_transpose(x, template = c("x", "y"), default = NA),
    list(x = c(1, NA), y = c(NA, "a"))
  )
  expect_equal(
    list_transpose(x, template = c("x", "y"), default = list(x = NA, y = "")),
    list(x = c(1, NA), y = c("", "a"))
  )
  expect_snapshot(list_transpose(x, default = list(c = NA)), error = TRUE)
})

test_that("validates inputs", {
  expect_snapshot(error = TRUE, {
    list_transpose(10)
    list_transpose(list(1), template = mean)
  })
})

test_that("fail mixing named and unnamed vectors", {
  test_list_transpose <- function() {
    x <- list(list(a = 1, b = 2), list(a = 3, b = 4))
    list_transpose(list(x = list(a = 1, b = 2), y = list(3, 4)))
  }
  expect_snapshot(error = TRUE, {
    test_list_transpose()
  })
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/tests/testthat/test-lmap.R ---
test_that("lmap output is list if input is list", {
  x <- list(a = 1:4, b = letters[5:7], c = 8:9, d = letters[10])
  maybe_rep <- function(x) {
    n <- rpois(1, 2)
    out <- rep_len(x, n)
    if (length(out) > 0) {
      names(out) <- paste0(names(x), seq_len(n))
    }
    out
  }
  expect_bare(lmap_at(x, "a", maybe_rep), "list")
})

test_that("lmap() returns a data frame if input is a data frame", {
  df <- data.frame(x = 1, y = 2)

  # as.data.frame() handles repeated names
  out <- lmap(df, function(x) as.data.frame(rep(x, 2)))
  expect_equal(out, data.frame(x = 1, x.1 = 1, y = 2, y.1 = 2))

  # even if we return bare lists
  out <- lmap(df, function(x) as.list(rep(x, 2)))
  expect_equal(out, data.frame(x = 1, x.1 = 1, y = 2, y.1 = 2))
})

test_that("lmap() can increase and decrease elements", {
  out <- lmap(list(0, 1, 2), ~ as.list(rep(.x, .x)))
  expect_equal(out, list(1, 2, 2))
})

test_that("lmap_at() only affects selected elements", {
  out <- lmap_at(list(0, 1, 2), c(1, 3), ~ as.list(rep(.x, .x)))
  expect_equal(out, list(1, 2, 2))

  out <- lmap_at(list(0, 1, 2), c(2, 3), ~ as.list(rep(.x, .x)))
  expect_equal(out, list(0, 1, 2, 2))
})

test_that("lmap_at can use tidyselect", {
  skip_if_not_installed("tidyselect")
  local_options(lifecycle_verbosity = "quiet")

  x <- lmap_at(mtcars, vars(tidyselect::contains("vs")), ~ .x + 10)
  expect_equal(x$vs[1], 10)
})

test_that("`.else` preserves false elements", {
  x <- list("a", 99)
  out <- lmap_if(x, is.character, ~ list(1, 2), .else = ~ list(3, 4))
  expect_equal(out, list(1, 2, 3, 4))
})

test_that("validates inputs", {
  expect_snapshot(error = TRUE, {
    lmap(list(1), ~1)
    lmap(list(1), environment())
    lmap(list(1), ~1, .else = environment())
  })
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/tests/testthat/test-map-depth.R ---
# map_depth ------------------------------------------------------------

test_that("map_depth modifies values at specified depth", {
  x1 <- list(list(list(1:3, 4:6)))

  expect_equal(map_depth(x1, 0, length), 1)
  expect_equal(map_depth(x1, 1, length), list(1))
  expect_equal(map_depth(x1, 2, length), list(list(2)))
  expect_equal(map_depth(x1, 3, length), list(list(list(3, 3))))
  expect_equal(map_depth(x1, -1, length), list(list(list(3, 3))))
  expect_snapshot(map_depth(x1, 6, length), error = TRUE)
  expect_snapshot(map_depth(x1, -5, length), error = TRUE)
})

test_that("default doesn't recurse into data frames, but can customise", {
  x <- list(data.frame(x = 1), data.frame(y = 2))
  expect_snapshot(map_depth(x, 2, class), error = TRUE)

  x <- list(data.frame(x = 1), data.frame(y = 1))
  expect_equal(
    map_depth(x, 2, class, .is_node = is.list),
    list(list(x = "numeric"), list(y = "numeric"))
  )
})

test_that("map_depth() with .ragged = TRUE operates on leaves", {
  x1 <- list(
    list(1),
    list(list(2))
  )
  exp <- list(
    list(list(2)),
    list(list(3))
  )

  expect_equal(map_depth(x1, 3, ~ . + 1, .ragged = TRUE), exp)
  expect_equal(map_depth(x1, -1, ~ . + 1, .ragged = TRUE), exp)
  # .ragged should be TRUE is .depth < 0
  expect_equal(map_depth(x1, -1, ~ . + 1), exp)
})

# modify_depth ------------------------------------------------------------

test_that("modify_depth modifies values at specified depth", {
  x1 <- list(list(list(1:3, 4:6)))

  expect_equal(modify_depth(x1, 0, length), list(1))
  expect_equal(modify_depth(x1, 1, length), list(1))
  expect_equal(modify_depth(x1, 2, length), list(list(2)))
  expect_equal(modify_depth(x1, 3, length), list(list(list(3, 3))))
  expect_equal(modify_depth(x1, -1, length), list(list(list(3, 3))))
  expect_snapshot(modify_depth(x1, 5, length), error = TRUE)
  expect_snapshot(modify_depth(x1, -5, length), error = TRUE)
})

test_that(".ragged = TRUE operates on leaves", {
  x1 <- list(
    list(1),
    list(list(2))
  )
  x2 <- list(
    list(2),
    list(list(3))
  )

  expect_equal(modify_depth(x1, 3, ~ . + 1, .ragged = TRUE), x2)
  expect_equal(modify_depth(x1, -1, ~ . + 1, .ragged = TRUE), x2)
  # .ragged should be TRUE is .depth < 0
  expect_equal(modify_depth(x1, -1, ~ . + 1), x2)
})

test_that("vectorised operations on the recursive and atomic levels yield same results", {
  x <- list(list(list(1:3, 4:6)))
  exp <- list(list(list(11:13, 14:16)))
  expect_identical(modify_depth(x, 3, `+`, 10L), exp)
  expect_snapshot(modify_depth(x, 5, `+`, 10L), error = TRUE)
})

test_that("modify_depth() treats NULLs correctly", {
  ll <- list(a = NULL, b = list(b1 = NULL, b2 = "hello"))
  expect_equal(modify_depth(ll, .depth = 2, identity, .ragged = TRUE), ll)
  expect_equal(
    modify_depth(ll, .depth = 2, is.character, .ragged = TRUE),
    list(a = NULL, b = list(b1 = FALSE, b2 = TRUE))
  )
})


# check_depth -------------------------------------------------------------

test_that("validates depth", {
  expect_snapshot(check_depth(mean), error = TRUE)
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/tests/testthat/test-map-if-at.R ---
test_that("map_if() and map_at() always return a list", {
  skip_if_not_installed("tibble")
  df <- tibble::tibble(x = 1, y = "a")
  expect_identical(map_if(df, is.character, ~"out"), list(x = 1, y = "out"))
  expect_identical(map_at(df, 1, ~"out"), list(x = "out", y = "a"))
})

test_that("map_at() works with tidyselect", {
  skip_if_not_installed("tidyselect")
  local_options(lifecycle_verbosity = "quiet")

  x <- list(a = "b", b = "c", aa = "bb")
  one <- map_at(x, vars(a), toupper)
  expect_identical(one$a, "B")
  expect_identical(one$aa, "bb")
  two <- map_at(x, vars(tidyselect::contains("a")), toupper)
  expect_identical(two$a, "B")
  expect_identical(two$aa, "BB")
})

test_that("negative .at omits locations", {
  x <- c(1, 2, 3)
  out <- map_at(x, -1, ~ .x * 2)
  expect_equal(out, list(1, 4, 6))
})

test_that("map_if requires predicate functions", {
  expect_snapshot(map_if(1:3, ~NA, ~"foo"), error = TRUE)
})

test_that("`.else` maps false elements", {
  expect_identical(
    map_if(-1:1, ~ .x > 0, paste, .else = ~"bar", "suffix"),
    list("bar", "bar", "1 suffix")
  )
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/tests/testthat/test-map-mapper.R ---
# formulas ----------------------------------------------------------------

test_that("can refer to first argument in three ways", {
  expect_equal(map_dbl(1, ~ . + 1), 2)
  expect_equal(map_dbl(1, ~ .x + 1), 2)
  expect_equal(map_dbl(1, ~ ..1 + 1), 2)
})

test_that("can refer to second arg in two ways", {
  expect_equal(map2_dbl(1, 2, ~ .x + .y + 1), 4)
  expect_equal(map2_dbl(1, 2, ~ ..1 + ..2 + 1), 4)
})

# vectors --------------------------------------------------------------

# test_that(".null generates warning", {
#   expect_warning(map(1, 2, .null = NA), "`.null` is deprecated")
# })

test_that(".default replaces absent values", {
  x <- list(
    list(a = 1, b = 2, c = 3),
    list(a = 1, c = 2),
    NULL
  )

  expect_equal(map_dbl(x, 3, .default = NA), c(3, NA, NA))
  expect_equal(map_dbl(x, "b", .default = NA), c(2, NA, NA))
})

test_that(".default only replaces NULL elements", {
  x <- list(
    list(a = 1),
    list(a = numeric()),
    list(a = NULL),
    list()
  )
  expect_equal(map(x, "a", .default = NA), list(1, numeric(), NA, NA))
})

test_that("Additional arguments are ignored", {
  expect_equal(as_mapper(function() NULL, foo = "bar", foobar), function() NULL)
})

test_that("can supply length > 1 vectors", {
  expect_identical(as_mapper(1:2)(list(list("a", "b"))), "b")
  expect_identical(as_mapper(c("a", "b"))(list(a = list("a", b = "b"))), "b")
})


# primitive functions --------------------------------------------------

test_that("primitive functions are wrapped", {
  expect_identical(as_mapper(`-`)(.y = 10, .x = 5), 5) # positional matching, not by name
  expect_identical(as_mapper(`c`)(1, 3, 5), c(1, 3, 5))
})

test_that("syntactic primitives are wrapped", {
  expect_identical(as_mapper(`[[`)(mtcars, "cyl"), mtcars$cyl)
  expect_identical(as_mapper(`$`)(mtcars, cyl), mtcars$cyl)
})


# lists ------------------------------------------------------------------

test_that("lists are wrapped", {
  mapper_list <- as_mapper(list("mpg", 5))(mtcars)
  base_list <- mtcars[["mpg"]][[5]]
  expect_identical(mapper_list, base_list)
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/tests/testthat/test-map-raw.R ---
test_that("_raw funtions are deprecated", {
  expect_snapshot({
    . <- map_raw(list(), ~.x)
    . <- map2_raw(list(), list(), ~.x)
    . <- imap_raw(list(), ~.x)
    . <- pmap_raw(list(), ~.x)
    . <- flatten_raw(list())
  })
})

test_that("_raw functions still work", {
  local_options(lifecycle_verbosity = "quiet")
  expect_equal(map_raw("a", charToRaw), charToRaw("a"))
  expect_identical(map_raw(set_names(list()), identity), named(raw()))

  expect_identical(map2_raw(set_names(list()), list(), identity), named(raw()))
  expect_equal(imap_raw(as.raw(12), rawShift), rawShift(as.raw(12), 1))

  expect_bare(pmap_raw(list(1:3), as.raw), "raw")
  expect_identical(pmap_raw(list(named(list())), identity), named(raw()))

  expect_equal(flatten_raw(list(as.raw(1))), as.raw(1))
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/tests/testthat/test-map.R ---
test_that("preserves names", {
  out <- map(list(x = 1, y = 2), identity)
  expect_equal(names(out), c("x", "y"))
})

test_that("creates simple call", {
  out <- map(1, function(x) sys.call())[[1]]
  expect_equal(out, quote(.f(.x[[i]], ...)))
})

test_that("fails on non-vectors", {
  expect_snapshot(map(environment(), identity), error = TRUE)
  expect_snapshot(map(quote(a), identity), error = TRUE)
})

test_that("works with vctrs records (#963)", {
  x <- new_rcrd(list(x = c(1, 2), y = c("a", "b")))
  out <- list(new_rcrd(list(x = 1, y = "a")), new_rcrd(list(x = 2, y = "b")))
  expect_identical(map(x, identity), out)
})

test_that("works with matrices/arrays (#970)", {
  expect_identical(
    map_int(matrix(1:4, nrow = 2), identity),
    1:4
  )
})

test_that("all inform about location of problem", {
  fail_at_3 <- function(x, bad) {
    if (x == 3) bad else x
  }

  expect_snapshot(error = TRUE, {
    map_int(1:3, ~ fail_at_3(.x, 2:1))
    map_int(1:3, ~ fail_at_3(.x, "x"))
    map(1:3, ~ fail_at_3(.x, stop("Doesn't work")))
  })

  cnd <- catch_cnd(map(1:3, ~ fail_at_3(.x, stop("Doesn't work"))))
  expect_s3_class(cnd, "purrr_error_indexed")
  expect_equal(cnd$location, 3)
  expect_equal(cnd$name, NULL)
})

test_that("error location uses name if present", {
  fail_at_3 <- function(x, bad) {
    if (x == 3) bad else x
  }

  expect_snapshot(error = TRUE, {
    map_int(c(a = 1, b = 2, c = 3), ~ fail_at_3(.x, stop("Error")))
    map_int(c(a = 1, b = 2, 3), ~ fail_at_3(.x, stop("Error")))
  })

  cnd <- catch_cnd(map(c(1, 2, c = 3), ~ fail_at_3(.x, stop("Doesn't work"))))
  expect_s3_class(cnd, "purrr_error_indexed")
  expect_equal(cnd$location, 3)
  expect_equal(cnd$name, "c")
})

test_that("0 length input gives 0 length output", {
  expect_equal(map(list(), identity), list())
  expect_equal(map(NULL, identity), list())

  expect_equal(map_lgl(NULL, identity), logical())
})

test_that("map() always returns a list", {
  expect_bare(map(mtcars, mean), "list")
})

test_that("types automatically coerced correctly", {
  expect_identical(map_lgl(c(NA, 0, 1), identity), c(NA, FALSE, TRUE))

  expect_identical(map_int(c(NA, FALSE, TRUE), identity), c(NA, 0L, 1L))
  expect_identical(map_int(c(NA, 1, 2), identity), c(NA, 1L, 2L))

  expect_identical(map_dbl(c(NA, FALSE, TRUE), identity), c(NA, 0, 1))
  expect_identical(map_dbl(c(NA, 1L, 2L), identity), c(NA, 1, 2))

  expect_identical(map_chr(NA, identity), NA_character_)
})

test_that("logical and integer NA become correct double NA", {
  expect_identical(
    map_dbl(list(NA, NA_integer_), identity),
    c(NA_real_, NA_real_)
  )
})

test_that("map forces arguments in same way as base R", {
  f_map <- map(1:2, function(i) function(x) x + i)
  f_base <- lapply(1:2, function(i) function(x) x + i)

  expect_equal(f_map[[1]](0), f_base[[1]](0))
  expect_equal(f_map[[2]](0), f_base[[2]](0))
})

test_that("walk is used for side-effects", {
  expect_output(walk(1:3, str))
})

test_that("primitive dispatch correctly", {
  local_bindings(.env = global_env(), as.character.test_class = function(x) {
    "dispatched!"
  })
  x <- structure(list(), class = "test_class")
  expect_identical(
    map(list(x, x), as.character),
    list("dispatched!", "dispatched!")
  )
})

test_that("map() with empty input copies names", {
  named_list <- named(list())
  expect_identical(map(named_list, identity), named(list()))
  expect_identical(map_lgl(named_list, identity), named(lgl()))
  expect_identical(map_int(named_list, identity), named(int()))
  expect_identical(map_dbl(named_list, identity), named(dbl()))
  expect_identical(map_chr(named_list, identity), named(chr()))
})


# map_vec -----------------------------------------------------------------

test_that("still iterates using [[", {
  df <- data.frame(x = 1, y = 2, z = 3)
  expect_equal(map_vec(df, length), c(x = 1, y = 1, z = 1))
})

test_that("requires output be length 1 and have common type", {
  expect_snapshot(error = TRUE, {
    map_vec(1:2, ~ rep(1, .x))
    map_vec(1:2, ~ if (.x == 1) factor("x") else 1)
  })
})

test_that("row-binds data frame output", {
  out <- map_vec(1:2, ~ data.frame(x = .x))
  expect_equal(out, data.frame(x = 1:2))
})

test_that("concatenates list output", {
  out <- map_vec(1:2, ~ list(.x))
  expect_equal(out, list(1, 2))
})

test_that("can enforce .ptype", {
  expect_snapshot(error = TRUE, {
    map_vec(1:2, ~ factor("x"), .ptype = integer())
  })
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/tests/testthat/test-map2.R ---
test_that("x and y mapped to first and second argument", {
  expect_equal(map2(1, 2, function(x, y) x), list(1))
  expect_equal(map2(1, 2, function(x, y) y), list(2))
})

test_that("variants return expected types", {
  x <- list(1, 2, 3)
  expect_true(is_bare_list(map2(x, 0, ~1)))
  expect_true(is_bare_logical(map2_lgl(x, 0, ~TRUE)))
  expect_true(is_bare_integer(map2_int(x, 0, ~1)))
  expect_true(is_bare_double(map2_dbl(x, 0, ~1.5)))
  expect_true(is_bare_character(map2_chr(x, 0, ~"x")))
  expect_equal(walk2(x, 0, ~"x"), x)

  x <- list(FALSE, 1L, 1)
  expect_true(is_bare_double(map2_vec(x, 0, ~.x)))
})

test_that("0 length input gives 0 length output", {
  expect_equal(map2(list(), list(), identity), list())
  expect_equal(map2(NULL, NULL, identity), list())

  expect_equal(map2_lgl(NULL, NULL, identity), logical())
})

test_that("verifies result types and length", {
  expect_snapshot(error = TRUE, {
    map2_int(1, 1, ~"x")
    map2_int(1, 1, ~ 1:2)
    map2_vec(1, 1, ~1, .ptype = character())
  })
})

test_that("works with vctrs records (#963)", {
  x <- new_rcrd(list(x = c(1, 2), y = c("a", "b")))
  out <- list(new_rcrd(list(x = 1, y = "a")), new_rcrd(list(x = 2, y = "b")))
  expect_identical(map2(x, 1, ~.x), out)
})

test_that("requires vector inputs", {
  expect_snapshot(error = TRUE, {
    map2(environment(), "a", identity)
    map2("a", environment(), "a", identity)
  })
})

test_that("recycles inputs", {
  expect_equal(map2(1:2, 1, `+`), list(2, 3))
  expect_equal(map2(integer(), 1, `+`), list())
  expect_equal(map2(NULL, 1, `+`), list())

  expect_snapshot(error = TRUE, {
    map2(1:2, 1:3, `+`)
    map2(1:2, integer(), `+`)
  })
})

test_that("only takes names from x", {
  x1 <- 1:2
  x2 <- set_names(x1, letters[1:2])
  x3 <- set_names(x1, "")

  expect_named(map2(x1, 1, `+`), NULL)
  expect_named(map2(x2, 1, `+`), c("a", "b"))
  expect_named(map2(x3, 1, `+`), c("", ""))

  # recycling them if needed (#779)
  x4 <- c(a = 1)
  expect_named(map2(x4, 1:2, `+`), c("a", "a"))
})

test_that("don't evaluate symbolic objects (#428)", {
  map2(exprs(1 + 2), NA, ~ expect_identical(.x, quote(1 + 2)))
  walk2(exprs(1 + 2), NA, ~ expect_identical(.x, quote(1 + 2)))
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/tests/testthat/test-modify-tree.R ---
test_that("can modify leaves", {
  expect_equal(
    modify_tree(c(1, 1, 1), leaf = ~ .x + 9),
    c(10, 10, 10)
  )

  expect_equal(
    modify_tree(list(1, list(1, list(1))), leaf = ~ .x + 9),
    list(10, list(10, list(10)))
  )
})

test_that("can modify nodes", {
  expect_equal(
    modify_tree(list(1, list(2, list(3))), post = list_flatten),
    list(1, 2, 3)
  )
})

test_that("default doesn't recurse into data frames, but can customise", {
  local_options(stringsAsFactors = FALSE)

  x <- list(data.frame(x = 1), data.frame(y = 2))
  expect_equal(
    modify_tree(x, leaf = class),
    list("data.frame", "data.frame")
  )
  expect_equal(
    modify_tree(x, leaf = class, is_node = is.list),
    list(data.frame(x = "numeric"), data.frame(y = "numeric"))
  )
})

test_that("leaf() is applied to non-node input", {
  expect_equal(modify_tree(1:3, leaf = identity), 1:3)
})

test_that("validates inputs", {
  expect_snapshot(error = TRUE, {
    modify_tree(list(), is_node = ~1)
    modify_tree(list(), is_node = 1)
  })
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/tests/testthat/test-modify.R ---
# Input types, ordered by apperance

test_that("modifying vectors list preserves type", {
  x1 <- vctrs::list_of(c(1, 2), c(3, 6, 9))
  x2 <- vctrs::list_of(c(2, 3), c(4, 7, 10))
  expect_equal(modify(x1, ~ .x + 1), x2)
})

test_that("modfiying data.frame preserves type and size", {
  df1 <- data.frame(x = 1:2, y = 2:1)
  expect_equal(modify(df1, ~1), data.frame(x = c(1, 1), y = c(1, 1)))
  expect_equal(modify_at(df1, 1, ~1), data.frame(x = c(1, 1), y = 2:1))
  expect_equal(
    modify2(df1, df1, ~ .x + .y),
    data.frame(x = c(2, 4), y = c(4, 2))
  )

  df2 <- new_data_frame(n = 5L)
  expect_equal(modify(df2, ~1), df2)

  expect_snapshot(error = TRUE, {
    modify(df1, ~ integer())
    modify(df1, ~ 1:4)

    modify_at(df1, 2, ~ integer())
    modify2(df1, list(1, 1:3), ~.y)
  })
})

test_that("zap gives clear error", {
  expect_snapshot(error = TRUE, {
    modify_at(1, 1, ~ zap())
    modify_at(list(1), 1, ~ zap())
    modify_at(data.frame(x = 1), 1, ~ zap())
    modify_at(lm(mpg ~ wt, data = mtcars), 1, ~ zap())
  })
})

test_that("data.frames are modified by column, not row", {
  df1 <- data.frame(x = 1:3, y = letters[1:3])
  df2 <- data.frame(x = 2:4, y = letters[1:3])

  expect_equal(modify(df1, ~ if (is.numeric(.x)) .x + 1 else .x), df2)
  expect_equal(modify_at(df1, "x", ~ .x + 1), df2)
})

test_that("modifying vectors preserves type", {
  expect_identical(modify(1:3, ~ .x + 1), 2:4)
  expect_equal(modify("a", ~ factor("b")), "b")

  expect_identical(modify_if(1:2, ~ .x %% 2 == 0, ~3), c(1L, 3L))
  expect_identical(modify_at(1:2, 2, ~3), c(1L, 3L))
  expect_identical(modify2(1:2, c(0, 1), `+`), c(1L, 3L))
})

test_that("bad type has useful error", {
  expect_snapshot(error = TRUE, {
    modify(1:3, ~"foo")
    modify_at(1:3, 1, ~"foo")
    modify_if(1:3, is_integer, ~"foo")
    modify2(1:3, "foo", ~.y)
  })
})

test_that("modifying lists preserves NULLs", {
  l <- list(a = 1, b = NULL, c = 3)
  expect_equal(modify(l, identity), l)
  expect_equal(modify_at(l, "b", identity), l)
  expect_equal(modify_if(l, is.null, identity), l)
  expect_equal(
    modify2(l, list(NULL, 1, NULL), ~.y),
    list(a = NULL, b = 1, c = NULL)
  )
})

test_that("can modify non-vector lists", {
  notlist <- function(...) structure(list(...), class = "notlist")
  x <- notlist(x = 1, y = "a")

  expect_equal(modify(x, ~2), notlist(x = 2, y = 2))
  expect_equal(modify_if(x, is.character, ~2), notlist(x = 1, y = 2))
  expect_equal(modify_at(x, "y", ~2), notlist(x = 1, y = 2))

  local_bindings(
    "[.notlist" = function(...) structure(NextMethod(), class = "notlist"),
    .env = globalenv()
  )
  expect_equal(modify2(x, list(3, 4), ~.y), notlist(x = 3, y = 4))
  expect_equal(modify2(notlist(1), list(3, 4), ~.y), notlist(3, 4))
})

test_that("modifying data frame ignores [<- methods", {
  df <- function(...) structure(data_frame(...), class = c("df", "data.frame"))
  local_bindings(
    "[<-.df" = function(...) stop("Forbidden"),
    .env = globalenv()
  )

  x <- df(x = 1, y = "x")
  expect_equal(modify(x, ~2), df(x = 2, y = 2))
  expect_equal(modify_if(x, is.character, ~2), df(x = 1, y = 2))
  expect_equal(modify_at(x, "y", ~2), df(x = 1, y = 2))
  expect_equal(modify2(x, list(2, 3), ~.y), df(x = 2, y = 3))
})

# other properties --------------------------------------------------------

test_that("`.else` modifies false elements", {
  exp <- modify_if(iris, negate(is.factor), as.integer)
  exp <- modify_if(exp, is.factor, as.character)
  expect_identical(
    modify_if(iris, is.factor, as.character, .else = as.integer),
    exp
  )

  expect_equal(
    modify_if(c(TRUE, FALSE), ~.x, ~FALSE, .else = ~TRUE),
    c(FALSE, TRUE)
  )
  expect_equal(modify_if(1:2, ~ .x == 1, ~3L, .else = ~4L), c(3, 4))
  expect_equal(
    modify_if(c(1, 10), ~ .x < 5, ~ .x * 10, .else = ~ .x / 2),
    c(10, 5)
  )
  expect_equal(
    modify_if(c("a", "b"), ~ .x == "a", ~"A", .else = ~"B"),
    c("A", "B")
  )
})

test_that("modify_at() can use tidyselect", {
  skip_if_not_installed("tidyselect")
  local_options(lifecycle_verbosity = "quiet")

  df <- data.frame(x = 1, y = 3)
  expect_equal(
    modify_at(df, vars(x), ~2),
    data.frame(x = 2, y = 3)
  )
})

test_that("imodify uses index", {
  expect_equal(imodify(list(2), ~.y), list(1))
  expect_equal(imodify(list(a = 2), ~.y), list(a = "a"))
})

# input validation --------------------------------------------------------

test_that("modify2() recycles arguments", {
  expect_equal(modify2(1:3, 1L, `+`), c(2, 3, 4))
  expect_equal(modify2(1, 1:3, `+`), c(2, 3, 4))

  expect_snapshot(error = TRUE, {
    modify2(1:3, integer(), `+`)
    modify2(1:3, 1:4, `+`)
  })
})

test_that("modify_if() requires predicate functions", {
  expect_snapshot(error = TRUE, {
    modify_if(list(1, 2), ~NA, ~"foo")
  })
})

test_that("user friendly error for non-supported cases", {
  expect_snapshot(error = TRUE, {
    modify(mean, identity)
    modify_if(mean, TRUE, identity)
    modify_at(mean, "x", identity)
    modify2(mean, 1, identity)
  })
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/tests/testthat/test-parallel.R ---
skip_if_not_installed("mirai")

test_that("All parallel map variants fall back to sequential with no daemons set", {
  expect_identical(
    map(list(x = 1, y = 2), in_parallel(\(x) list(x))),
    map(list(x = 1, y = 2), \(x) list(x))
  )
  expect_equal(map2(1, 2, in_parallel(\(x, y) x)), list(1))
  expect_identical(pmap(list(), in_parallel(~ 1)), list())
})

# set up daemons
mirai::daemons(1, dispatcher = FALSE) # ensures only 1 additional process on CRAN
on.exit(mirai::daemons(0), add = TRUE)

test_that("Can't use `...` in a parallel map", {
  expect_snapshot(error = TRUE, {
    map(list(x = 1, y = 2), in_parallel(\(x) list(x)), a = "wrong")
  })
})

test_that("Crated function environment is attached to search path", {
  # Use of `median()` without the `stats::` namespace
  expect_equal(map_dbl(1:2, in_parallel(\(x) median(c(1, 1, x)))), c(1, 1))
})

# map -----------------------------------------------------------------------

test_that("preserves names", {
  out <- map(list(x = 1, y = 2), in_parallel(\(x) identity(x)))
  expect_equal(names(out), c("x", "y"))
})

test_that("works with matrices/arrays (#970)", {
  expect_identical(
    map_int(matrix(1:4, nrow = 2), in_parallel(\(x) identity(x))),
    1:4
  )
})

test_that("all inform about location of problem", {
  skip_if_not_installed("carrier")

  expect_snapshot(error = TRUE, {
    map_int(1:3, in_parallel(\(x, bad = 2:1) if (x == 3) bad else x))
    map_int(1:3, in_parallel(\(x, bad = "x") if (x == 3) bad else x))
    map(
      1:3,
      in_parallel(\(x, bad = stop("Doesn't work")) if (x == 3) bad else x)
    )
  })

  cnd <- catch_cnd(map(
    1:3,
    in_parallel(\(x, bad = stop("Doesn't work")) if (x == 3) bad else x)
  ))
  expect_s3_class(cnd, "purrr_error_indexed")
  expect_equal(cnd$location, 3)
  expect_equal(cnd$name, NULL)
})

test_that("error location uses name if present", {
  skip_if_not_installed("carrier")

  expect_snapshot(error = TRUE, {
    map_int(
      c(a = 1, b = 2, c = 3),
      in_parallel(\(x, bad = stop("Doesn't work")) if (x == 3) bad else x)
    )
    map_int(
      c(a = 1, b = 2, 3),
      in_parallel(\(x, bad = stop("Doesn't work")) if (x == 3) bad else x)
    )
  })

  cnd <- catch_cnd(map(
    c(1, 2, c = 3),
    in_parallel(\(x, bad = stop("Doesn't work")) if (x == 3) bad else x)
  ))
  expect_s3_class(cnd, "purrr_error_indexed")
  expect_equal(cnd$location, 3)
  expect_equal(cnd$name, "c")
})

test_that("0 length input gives 0 length output", {
  expect_equal(map(list(), in_parallel(\(x) identity(x))), list())
  expect_equal(map(NULL, in_parallel(\(x) identity(x))), list())

  expect_equal(map_lgl(NULL, in_parallel(\(x) identity(x))), logical())
})

test_that("map() always returns a list", {
  expect_bare(map(mtcars, in_parallel(\(x) mean(x))), "list")
})

test_that("types automatically coerced correctly", {
  expect_identical(
    map_lgl(c(NA, 0, 1), in_parallel(\(x) identity(x))),
    c(NA, FALSE, TRUE)
  )

  expect_identical(
    map_int(c(NA, FALSE, TRUE), in_parallel(\(x) identity(x))),
    c(NA, 0L, 1L)
  )
  expect_identical(
    map_int(c(NA, 1, 2), in_parallel(\(x) identity(x))),
    c(NA, 1L, 2L)
  )

  expect_identical(
    map_dbl(c(NA, FALSE, TRUE), in_parallel(\(x) identity(x))),
    c(NA, 0, 1)
  )
  expect_identical(
    map_dbl(c(NA, 1L, 2L), in_parallel(\(x) identity(x))),
    c(NA, 1, 2)
  )

  expect_identical(map_chr(NA, in_parallel(\(x) identity(x))), NA_character_)
})

test_that("logical and integer NA become correct double NA", {
  expect_identical(
    map_dbl(list(NA, NA_integer_), in_parallel(\(x) identity(x))),
    c(NA_real_, NA_real_)
  )
})

test_that("map forces arguments in same way as base R", {
  f_map <- map(1:2, in_parallel(\(i) \(x) x + i))
  f_base <- lapply(1:2, \(i) \(x) x + i)

  expect_equal(f_map[[1]](0), f_base[[1]](0))
  expect_equal(f_map[[2]](0), f_base[[2]](0))
})

test_that("primitive dispatch correctly", {
  skip_if_not_installed("carrier")

  method <- \(x) "dispatched!"
  x <- structure(list(), class = "test_class")
  expect_identical(
    map(
      list(x, x),
      in_parallel(\(x) as.character(x), as.character.test_class = method)
    ),
    list("dispatched!", "dispatched!")
  )
})

test_that("map() with empty input copies names", {
  named_list <- named(list())
  expect_identical(
    map(named_list, in_parallel(\(x) identity(x))),
    named(list())
  )
  expect_identical(
    map_lgl(named_list, in_parallel(\(x) identity(x))),
    named(lgl())
  )
  expect_identical(
    map_int(named_list, in_parallel(\(x) identity(x))),
    named(int())
  )
  expect_identical(
    map_dbl(named_list, in_parallel(\(x) identity(x))),
    named(dbl())
  )
  expect_identical(
    map_chr(named_list, in_parallel(\(x) identity(x))),
    named(chr())
  )
})

# map_vec ------------------------------------------------------------------

test_that("still iterates using [[", {
  df <- data.frame(x = 1, y = 2, z = 3)
  expect_equal(map_vec(df, in_parallel(\(x) length(x))), c(x = 1, y = 1, z = 1))
})

test_that("requires output be length 1 and have common type", {
  expect_snapshot(error = TRUE, {
    map_vec(1:2, in_parallel(~ rep(1, .x)))
    map_vec(1:2, in_parallel(~ if (.x == 1) factor("x") else 1))
  })
})

test_that("row-binds data frame output", {
  out <- map_vec(1:2, in_parallel(~ data.frame(x = .x)))
  expect_equal(out, data.frame(x = 1:2))
})

test_that("concatenates list output", {
  out <- map_vec(1:2, in_parallel(~ list(.x)))
  expect_equal(out, list(1, 2))
})

test_that("can enforce .ptype", {
  expect_snapshot(error = TRUE, {
    map_vec(1:2, in_parallel(~ factor("x")), .ptype = integer())
  })
})

# map2 ---------------------------------------------------------------------

test_that("x and y mapped to first and second argument", {
  expect_equal(map2(1, 2, in_parallel(\(x, y) x)), list(1))
  expect_equal(map2(1, 2, in_parallel(\(x, y) y)), list(2))
})

test_that("variants return expected types", {
  x <- list(1, 2, 3)
  expect_true(is_bare_list(map2(x, 0, in_parallel(~1))))
  expect_true(is_bare_logical(map2_lgl(x, 0, in_parallel(~TRUE))))
  expect_true(is_bare_integer(map2_int(x, 0, in_parallel(~1))))
  expect_true(is_bare_double(map2_dbl(x, 0, in_parallel(~1.5))))
  expect_true(is_bare_character(map2_chr(x, 0, in_parallel(~"x"))))
  expect_equal(walk2(x, 0, in_parallel(~"x")), x)

  x <- list(FALSE, 1L, 1)
  expect_true(is_bare_double(map2_vec(x, 0, ~.x, .parallel = TRUE)))
})

test_that("0 length input gives 0 length output", {
  expect_equal(map2(list(), list(), in_parallel(\(x) identity(x))), list())
  expect_equal(map2(NULL, NULL, in_parallel(\(x) identity(x))), list())

  expect_equal(map2_lgl(NULL, NULL, in_parallel(\(x) identity(x))), logical())
})

test_that("verifies result types and length", {
  expect_snapshot(error = TRUE, {
    map2_int(1, 1, in_parallel(~"x"))
    map2_int(1, 1, in_parallel(~ 1:2))
    map2_vec(1, 1, in_parallel(~1), .ptype = character())
  })
})

test_that("works with vctrs records (#963)", {
  x <- new_rcrd(list(x = c(1, 2), y = c("a", "b")))
  out <- list(new_rcrd(list(x = 1, y = "a")), new_rcrd(list(x = 2, y = "b")))
  expect_identical(map2(x, 1, in_parallel(~.x)), out)
})

test_that("requires vector inputs", {
  expect_snapshot(error = TRUE, {
    map2(environment(), "a", in_parallel(\(x) identity(x)))
    map2("a", environment(), "a", in_parallel(\(x) identity(x)))
  })
})

test_that("recycles inputs", {
  expect_equal(map2(1:2, 1, in_parallel(\(x, y) x + y)), list(2, 3))
  expect_equal(map2(integer(), 1, in_parallel(\(x, y) x + y)), list())
  expect_equal(map2(NULL, 1, in_parallel(\(x, y) x + y)), list())

  expect_snapshot(error = TRUE, {
    map2(1:2, 1:3, in_parallel(\(x, y) x + y))
    map2(1:2, integer(), in_parallel(\(x, y) x + y))
  })
})

test_that("only takes names from x", {
  x1 <- 1:2
  x2 <- set_names(x1, letters[1:2])
  x3 <- set_names(x1, "")

  expect_named(map2(x1, 1, in_parallel(\(x, y) x + y)), NULL)
  expect_named(map2(x2, 1, in_parallel(\(x, y) x + y)), c("a", "b"))
  expect_named(map2(x3, 1, in_parallel(\(x, y) x + y)), c("", ""))

  # recycling them if needed (#779)
  x4 <- c(a = 1)
  expect_named(map2(x4, 1:2, in_parallel(\(x, y) x + y)), c("a", "a"))
})

test_that("don't evaluate symbolic objects (#428)", {
  map2(
    exprs(1 + 2),
    NA,
    in_parallel(~ testthat::expect_identical(.x, quote(1 + 2)))
  )
  walk2(
    exprs(1 + 2),
    NA,
    in_parallel(~ testthat::expect_identical(.x, quote(1 + 2)))
  )
  expect_true(TRUE) # so the test is not deemed empty and skipped
})

# pmap ----------------------------------------------------------------------

test_that(".f called with named arguments", {
  x <- list(x = 1, 2, y = 3)
  expect_equal(pmap(x, in_parallel(\(...) list(...))), list(x))
})

# no longer tested as `...` are forbidden when `.parallel = TRUE`
#test_that("... are passed after varying argumetns", {
#  out <- pmap(list(x = 1:2), list, n = 1:2, .parallel = TRUE)
#  expect_equal(out, list(
#    list(x = 1, n = 1:2),
#    list(x = 2, n = 1:2)
#  ))
#})

test_that("variants return expected types", {
  l <- list(list(1, 2, 3))
  expect_true(is_bare_list(pmap(l, in_parallel(~1))))
  expect_true(is_bare_logical(pmap_lgl(l, in_parallel(~TRUE))))
  expect_true(is_bare_integer(pmap_int(l, in_parallel(~1))))
  expect_true(is_bare_double(pmap_dbl(l, in_parallel(~1.5))))
  expect_true(is_bare_character(pmap_chr(l, in_parallel(~"x"))))
  expect_equal(pwalk(l, in_parallel(~"x")), l)

  l <- list(list(FALSE, 1L, 1))
  expect_true(is_bare_double(pmap_vec(l, in_parallel(~.x))))
})

test_that("verifies result types and length", {
  expect_snapshot(error = TRUE, {
    pmap_int(list(1), in_parallel(~"x"))
    pmap_int(list(1), in_parallel(~ 1:2))
    pmap_vec(list(1), in_parallel(~1), .ptype = character())
  })
})

test_that("0 length input gives 0 length output", {
  expect_equal(
    pmap(list(list(), list()), in_parallel(\(x) identity(x))),
    list()
  )
  expect_equal(pmap(list(NULL, NULL), in_parallel(\(x) identity(x))), list())
  expect_equal(pmap(list(), in_parallel(\(x) identity(x))), list())
  expect_equal(pmap(NULL, in_parallel(\(x) identity(x))), list())

  expect_equal(pmap_lgl(NULL, in_parallel(\(x) identity(x))), logical())
})

test_that("requires list of vectors", {
  expect_snapshot(error = TRUE, {
    pmap(environment(), in_parallel(\(x) identity(x)))
    pmap(list(environment()), in_parallel(\(x) identity(x)))
  })
})

test_that("recycles inputs", {
  expect_equal(pmap(list(1:2, 1), in_parallel(\(x, y) x + y)), list(2, 3))
  expect_equal(pmap(list(integer(), 1), in_parallel(\(x, y) x + y)), list())
  expect_equal(pmap(list(NULL, 1), in_parallel(\(x, y) x + y)), list())

  expect_snapshot(error = TRUE, {
    pmap(list(1:2, 1:3), in_parallel(\(x, y) x + y))
    pmap(list(1:2, integer()), in_parallel(\(x, y) x + y))
  })
})

test_that("only takes names from x", {
  x1 <- 1:2
  x2 <- set_names(x1, letters[1:2])
  x3 <- set_names(x1, "")

  expect_named(pmap(list(x1, x2), in_parallel(\(x, y) x + y)), NULL)
  expect_named(pmap(list(x2, x2), in_parallel(\(x, y) x + y)), c("a", "b"))
  expect_named(pmap(list(x3, x2), in_parallel(\(x, y) x + y)), c("", ""))

  # recycling them if needed (#779)
  x4 <- c(a = 1)
  expect_named(pmap(list(x4, 1:2), in_parallel(\(x, y) x + y)), c("a", "a"))
})

test_that("avoid expensive [[ method on data frames", {
  local_bindings(
    `[[.mydf` = function(x, ...) stop("Not allowed!"),
    .env = global_env()
  )

  df <- data.frame(x = 1:2, y = 2:1)
  class(df) <- c("mydf", "data.frame")

  expect_equal(
    pmap(df, in_parallel(\(...) list(...), `[[.mydf` = `[[.mydf`)),
    list(list(x = 1, y = 2), list(x = 2, y = 1))
  )
  expect_equal(
    pmap_lgl(df, in_parallel(~TRUE, `[[.mydf` = `[[.mydf`)),
    c(TRUE, TRUE)
  )
  expect_equal(pmap_int(df, in_parallel(~2, `[[.mydf` = `[[.mydf`)), c(2, 2))
  expect_equal(
    pmap_dbl(df, in_parallel(~3.5, `[[.mydf` = `[[.mydf`)),
    c(3.5, 3.5)
  )
  expect_equal(
    pmap_chr(df, in_parallel(~"x", `[[.mydf` = `[[.mydf`)),
    c("x", "x")
  )
})

test_that("pmap works with empty lists", {
  expect_identical(pmap(list(), in_parallel(~1)), list())
})

test_that("preserves S3 class of input vectors (#358)", {
  date <- as.Date("2018-09-27")
  expect_identical(pmap(list(date), in_parallel(\(x) identity(x))), list(date))
})

test_that("works with vctrs records (#963)", {
  x <- new_rcrd(list(x = c(1, 2), y = c("a", "b")))
  out <- list(new_rcrd(list(x = 1, y = "a")), new_rcrd(list(x = 2, y = "b")))
  expect_identical(pmap(list(x, 1, 1:2), in_parallel(~.x)), out)
})

test_that("don't evaluate symbolic objects (#428)", {
  pmap(
    list(exprs(1 + 2)),
    in_parallel(~ testthat::expect_identical(.x, quote(1 + 2)))
  )
  pwalk(
    list(exprs(1 + 2)),
    in_parallel(~ testthat::expect_identical(.x, quote(1 + 2)))
  )
  expect_true(TRUE) # so the test is not deemed empty and skipped
})

# imap ----------------------------------------------------------------------

test_that("atomic vector imap works", {
  x <- 1:3 |> set_names()
  expect_true(all(imap_lgl(x, in_parallel(\(x, y) x == y))))
  expect_length(imap_chr(x, in_parallel(\(...) paste(...))), 3)
  expect_equal(imap_int(x, in_parallel(~ .x + as.integer(.y))), x * 2)
  expect_equal(imap_dbl(x, in_parallel(~ .x + as.numeric(.y))), x * 2)
  expect_equal(imap_vec(x, in_parallel(~ .x + as.numeric(.y))), x * 2)
})

# map_at --------------------------------------------------------------------

test_that("map_at() works with tidyselect", {
  skip_if_not_installed("tidyselect")
  local_options(lifecycle_verbosity = "quiet")

  x <- list(a = "b", b = "c", aa = "bb")
  one <- map_at(x, vars(a), in_parallel(\(x) toupper(x)))
  expect_identical(one$a, "B")
  expect_identical(one$aa, "bb")
  two <- map_at(
    x,
    vars(tidyselect::contains("a")),
    in_parallel(\(x) toupper(x))
  )
  expect_identical(two$a, "B")
  expect_identical(two$aa, "BB")
})

test_that("negative .at omits locations", {
  x <- c(1, 2, 3)
  out <- map_at(x, -1, in_parallel(~ .x * 2))
  expect_equal(out, list(1, 4, 6))
})

# ---------------------------------------------------------------------------

mirai::daemons(0)


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/tests/testthat/test-pluck-assign.R ---
# assign_in() ----------------------------------------------------------

test_that("assign_in() doesn't assign in the caller environment", {
  x <- list(list(bar = 1, foo = 2))
  assign_in(x, list(1, "foo"), value = 20)
  expect_identical(x, list(list(bar = 1, foo = 2)))
})

test_that("assign_in() assigns", {
  x <- list(list(bar = 1, foo = 2))
  out <- assign_in(x, list(1, "foo"), value = 20)
  expect_identical(out, list(list(bar = 1, foo = 20)))
})

test_that("can assign NULL (#636)", {
  expect_equal(
    assign_in(list(x = 1, y = 2), 1, value = NULL),
    list(x = NULL, y = 2)
  )
  expect_equal(
    assign_in(list(x = 1, y = 2), "y", value = NULL),
    list(x = 1, y = NULL)
  )
})

test_that("can remove elements with zap()", {
  expect_equal(
    assign_in(list(x = 1, y = 2), 1, value = zap()),
    list(y = 2)
  )
  expect_equal(
    assign_in(list(x = 1, y = 2), "y", value = zap()),
    list(x = 1)
  )

  # And deep indexing leaves unchanged
  expect_equal(
    assign_in(list(x = 1, y = 2), c(3, 4, 5), value = zap()),
    list(x = 1, y = 2)
  )
  expect_equal(
    assign_in(list(x = 1, y = 2), c("a", "b", "c"), value = zap()),
    list(x = 1, y = 2)
  )
})

test_that("assign_in() requires at least one location", {
  x <- list("foo")
  expect_snapshot(error = TRUE, {
    assign_in(x, NULL, value = "foo")
  })
})

test_that("can modify non-existing locations", {
  expect_equal(assign_in(list(), "x", 1), list(x = 1))
  expect_equal(assign_in(list(), 2, 1), list(NULL, 1))

  expect_equal(assign_in(list(), c("x", "y"), 1), list(x = list(y = 1)))
  expect_equal(assign_in(list(), c(2, 1), 1), list(NULL, list(1)))

  expect_equal(assign_in(list(), list("x", 2), 1), list(x = list(NULL, 1)))
  expect_equal(assign_in(list(), list(1, "y"), 1), list(list(y = 1)))
})

# modify_in() ----------------------------------------------------------

test_that("modify_in() modifies in pluck location", {
  x <- list(list(bar = 1, foo = 2))

  out <- modify_in(x, list(1, "foo"), `+`, 100)
  expect_identical(out, list(list(bar = 1, foo = 102)))

  out <- modify_in(x, c(1, 1), `+`, 10)
  expect_identical(out, list(list(bar = 11, foo = 2)))
})

test_that("modify_in() doesn't require existing", {
  x <- list(list(x = 1, y = 2))
  expect_equal(modify_in(x, 2, ~10), list(list(x = 1, y = 2), 10))
  expect_equal(
    modify_in(x, list(1, "z"), ~10),
    list(list(x = 1, y = 2, z = 10))
  )
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/tests/testthat/test-pluck-depth.R ---
test_that("depth of non-vectors is 0", {
  expect_equal(pluck_depth(NULL), 0L)
  expect_equal(pluck_depth(mean), 0L)
})

test_that("depth of atomic vector is 1", {
  expect_equal(pluck_depth(1:10), 1)
  expect_equal(pluck_depth(letters), 1)
  expect_equal(pluck_depth(c(TRUE, FALSE)), 1)
})

test_that("depth of nested is depth of deepest element + 1", {
  x <- list(
    NULL,
    list(),
    list(list())
  )

  depths <- map_int(x, pluck_depth)
  expect_equal(depths, c(0, 1, 2))
  expect_equal(pluck_depth(x), 3)
})

test_that("vec_depth() is deprecated", {
  expect_snapshot({
    . <- vec_depth(list())
  })
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/tests/testthat/test-pluck.R ---
test_that("can pluck/chuck from NULL", {
  expect_equal(pluck(NULL, 1), NULL)
  expect_snapshot(chuck(NULL, 1), error = TRUE)
})

test_that("can pluck vector types ", {
  x <- list(
    lgl = c(TRUE, FALSE),
    int = 1:2,
    dbl = c(1, 2.5),
    chr = c("a", "b"),
    cpx = c(1 + 1i, 2 + 2i),
    raw = charToRaw("ab"),
    lst = list(1, 2)
  )

  expect_equal(pluck(x, "lgl", 2), FALSE)
  expect_identical(pluck(x, "int", 2), 2L)
  expect_equal(pluck(x, "dbl", 2), 2.5)
  expect_equal(pluck(x, "chr", 2), "b")
  expect_equal(pluck(x, "cpx", 2), 2 + 2i)
  expect_equal(pluck(x, "raw", 2), charToRaw("b"))
  expect_equal(pluck(x, "lst", 2), 2)
})

test_that("unsupported types have useful error", {
  expect_snapshot(error = TRUE, {
    pluck(quote(x), 1)
    pluck(quote(f(x, 1)), 1)
    pluck(expression(1), 1)
  })
})

test_that("dots must be unnamed", {
  expect_snapshot(pluck(1, a = 1), error = TRUE)
  expect_snapshot(chuck(1, a = 1), error = TRUE)
})

test_that("can pluck by position (positive and negative)", {
  x <- list("a", "b", "c")

  expect_equal(pluck(x, 1), "a")
  expect_equal(pluck(x, -1), "c")

  expect_equal(pluck(x, 0), NULL)
  expect_equal(pluck(x, 4), NULL)
  expect_equal(pluck(x, -4), NULL)
  expect_equal(pluck(x, -5), NULL)

  expect_snapshot(chuck(x, 0), error = TRUE)
  expect_snapshot(chuck(x, 4), error = TRUE)
  expect_snapshot(chuck(x, -4), error = TRUE)
  expect_snapshot(chuck(x, -5), error = TRUE)
})

test_that("special numbers don't match", {
  x <- list()

  expect_equal(pluck(x, NA_integer_), NULL)
  expect_equal(pluck(x, NA_real_), NULL)
  expect_equal(pluck(x, NaN), NULL)
  expect_equal(pluck(x, Inf), NULL)
  expect_equal(pluck(x, -Inf), NULL)

  expect_snapshot(chuck(x, NA_integer_), error = TRUE)
  expect_snapshot(chuck(x, NA_real_), error = TRUE)
  expect_snapshot(chuck(x, NaN), error = TRUE)
  expect_snapshot(chuck(x, Inf), error = TRUE)
  expect_snapshot(chuck(x, -Inf), error = TRUE)
})

test_that("can pluck by name", {
  x <- list(a = "a")

  expect_equal(pluck(x, "a"), "a")

  expect_equal(pluck(x, "b"), NULL)
  expect_equal(pluck(x, NA_character_), NULL)
  expect_equal(pluck(x, ""), NULL)

  expect_snapshot(chuck(x, "b"), error = TRUE)
  expect_snapshot(chuck(x, NA_character_), error = TRUE)
  expect_snapshot(chuck(x, ""), error = TRUE)
})

test_that("even if names don't exist", {
  x <- list("a")

  expect_equal(pluck(x, "a"), NULL)
  expect_snapshot(chuck(x, "a"), error = TRUE)
})

test_that("matches first name if duplicated", {
  x <- list(1, 2, 3, 4, 5)
  names(x) <- c("a", "a", NA, "", "b")

  expect_equal(pluck(x, "a"), 1)
})

test_that("empty and NA names never match", {
  x <- list(1, 2, 3)
  names(x) <- c("", NA, "x")

  expect_equal(pluck(x, "x"), 3)

  expect_equal(pluck(x, ""), NULL)
  expect_equal(pluck(x, NA_character_), NULL)

  expect_snapshot(chuck(x, ""), error = TRUE)
  expect_snapshot(chuck(x, NA_character_), error = TRUE)
})

test_that("require length 1 character/double vectors", {
  expect_snapshot(error = TRUE, {
    pluck(1, 1:2)
    pluck(1, integer())
    pluck(1, NULL)
    pluck(1, TRUE)
  })
})

test_that("validate index even when indexing NULL", {
  expect_snapshot(error = TRUE, {
    pluck(NULL, 1:2)
    pluck(NULL, TRUE)
  })
})

test_that("can pluck 0-length object", {
  expect_equal(pluck(list(integer()), 1), integer())
})

test_that("supports splicing", {
  x <- list(list(bar = 1, foo = 2))
  idx <- list(1, "foo")
  expect_identical(pluck(x, !!!idx), 2)
})

# functions ---------------------------------------------------------------

test_that("can pluck attributes", {
  x <- structure(
    list(
      structure(
        list(),
        x = 1
      )
    ),
    y = 2
  )

  expect_equal(pluck(x, attr_getter("y")), 2)
  expect_equal(pluck(x, 1, attr_getter("x")), 1)
})

test_that("attr_getter() uses exact (non-partial) matching", {
  x <- 1
  attr(x, "labels") <- "foo"

  expect_identical(attr_getter("labels")(x), "foo")
  expect_identical(attr_getter("label")(x), NULL)
})

test_that("attr_getter() evaluates eagerly", {
  getters <- new_list(2)
  attrs <- c("foo", "bar")
  for (i in seq_along(attrs)) {
    getters[[i]] <- attr_getter(attrs[[i]])
  }

  x <- structure(list(), foo = "foo", bar = "bar")
  expect_identical(getters[[1]](x), "foo")
})

test_that("accessors throw correct errors", {
  expect_snapshot(error = TRUE, {
    pluck(1:3, function() NULL)
    pluck(1:3, function(x, y) y)
  })
})

test_that("pluck() functions dispatch on base getters", {
  expect_identical(pluck(iris, "Species", levels), levels(iris$Species))
})

test_that("pluck() supports primitive and built-in functions (#404)", {
  x <- list(1:2)
  expect_equal(pluck(x, 1, as.character), c("1", "2"))
  expect_equal(pluck(x, 1, sum), 3)
})

# environments ------------------------------------------------------------

test_that("can pluck/chuck environment by name", {
  x <- new_environment(list(x = 10))

  expect_equal(pluck(x, "x"), 10)
  expect_equal(pluck(x, "y"), NULL)
  expect_equal(pluck(x, NA_character_), NULL)

  expect_snapshot(chuck(x, "y"), error = TRUE)
  expect_snapshot(chuck(x, NA_character_), error = TRUE)
})

test_that("environments error with invalid indices", {
  expect_snapshot(pluck(environment(), 1), error = TRUE)
  expect_snapshot(pluck(environment(), letters), error = TRUE)
})

# S4 ----------------------------------------------------------------------

newA <- methods::setClass("A", list(a = "numeric"))

test_that("can pluck/chuck from S4 objects", {
  A <- newA(a = 1)
  expect_equal(pluck(A, "a"), 1)
  expect_equal(pluck(A, "b"), NULL)
  expect_equal(pluck(A, NA_character_), NULL)

  expect_snapshot(chuck(A, "b"), error = TRUE)
  expect_snapshot(chuck(A, NA_character_), error = TRUE)
})

test_that("S4 objects error with invalid indices", {
  A <- newA(a = 1)
  expect_snapshot(pluck(A, 1), error = TRUE)
  expect_snapshot(pluck(A, letters), error = TRUE)
})

# S3 ----------------------------------------------------------------------

test_that("pluck() dispatches on vector methods", {
  new_test_pluck <- function(x) {
    structure(list(x), class = "test_pluck")
  }

  inner <- list(a = "foo", b = list("bar"))
  x <- list(new_test_pluck(inner))

  with_bindings(
    .env = global_env(),
    `[[.test_pluck` = function(x, i) .subset2(x, 1)[[i]],
    names.test_pluck = function(x) names(.subset2(x, 1)),
    length.test_pluck = function(x) length(.subset2(x, 1)),
    {
      expect_identical(pluck(x, 1, 1), "foo")
      expect_identical(pluck(x, 1, "b", 1), "bar")
      expect_identical(chuck(x, 1, 1), "foo")
      expect_identical(chuck(x, 1, "b", 1), "bar")
    }
  )

  # With faulty length() method
  with_bindings(
    .env = global_env(),
    `[[.test_pluck` = function(x, i) .subset2(x, 1)[[i]],
    length.test_pluck = function(x) NA,
    {
      expect_null(pluck(x, 1, 1))
      expect_snapshot(chuck(x, 1, 1), error = TRUE)
    }
  )

  # With faulty names() method
  with_bindings(
    .env = global_env(),
    `[[.test_pluck` = function(x, i) .subset2(x, 1)[[i]],
    names.test_pluck = function(x) NA,
    length.test_pluck = function(x) length(.subset2(x, 1)),
    {
      expect_null(pluck(x, 1, "b", 1))
      expect_snapshot(chuck(x, 1, "b", 1), error = TRUE)
    }
  )
})

# Setting -----------------------------------------------------------------

test_that("pluck<- is an alias for assign_in()", {
  x <- list(list(bar = 1, foo = 2))
  pluck(x, 1, "foo") <- 30
  expect_identical(x, list(list(bar = 1, foo = 30)))
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/tests/testthat/test-pmap.R ---
test_that(".f called with named arguments", {
  x <- list(x = 1, 2, y = 3)
  expect_equal(pmap(x, list), list(x))
})

test_that("... are passed after varying argumetns", {
  out <- pmap(list(x = 1:2), list, n = 1:2)
  expect_equal(
    out,
    list(
      list(x = 1, n = 1:2),
      list(x = 2, n = 1:2)
    )
  )
})

test_that("variants return expected types", {
  l <- list(list(1, 2, 3))
  expect_true(is_bare_list(pmap(l, ~1)))
  expect_true(is_bare_logical(pmap_lgl(l, ~TRUE)))
  expect_true(is_bare_integer(pmap_int(l, ~1)))
  expect_true(is_bare_double(pmap_dbl(l, ~1.5)))
  expect_true(is_bare_character(pmap_chr(l, ~"x")))
  expect_equal(pwalk(l, ~"x"), l)

  l <- list(list(FALSE, 1L, 1))
  expect_true(is_bare_double(pmap_vec(l, ~.x)))
})

test_that("verifies result types and length", {
  expect_snapshot(error = TRUE, {
    pmap_int(list(1), ~"x")
    pmap_int(list(1), ~ 1:2)
    pmap_vec(list(1), ~1, .ptype = character())
  })
})

test_that("0 length input gives 0 length output", {
  expect_equal(pmap(list(list(), list()), identity), list())
  expect_equal(pmap(list(NULL, NULL), identity), list())
  expect_equal(pmap(list(), identity), list())
  expect_equal(pmap(NULL, identity), list())

  expect_equal(pmap_lgl(NULL, identity), logical())
})


test_that("requires list of vectors", {
  expect_snapshot(error = TRUE, {
    pmap(environment(), identity)
    pmap(list(environment()), identity)
  })
})

test_that("recycles inputs", {
  expect_equal(pmap(list(1:2, 1), `+`), list(2, 3))
  expect_equal(pmap(list(integer(), 1), `+`), list())
  expect_equal(pmap(list(NULL, 1), `+`), list())

  expect_snapshot(error = TRUE, {
    pmap(list(1:2, 1:3), `+`)
    pmap(list(1:2, integer()), `+`)
  })
})

test_that("only takes names from x", {
  x1 <- 1:2
  x2 <- set_names(x1, letters[1:2])
  x3 <- set_names(x1, "")

  expect_named(pmap(list(x1, x2), `+`), NULL)
  expect_named(pmap(list(x2, x2), `+`), c("a", "b"))
  expect_named(pmap(list(x3, x2), `+`), c("", ""))

  # recycling them if needed (#779)
  x4 <- c(a = 1)
  expect_named(pmap(list(x4, 1:2), `+`), c("a", "a"))
})

test_that("avoid expensive [[ method on data frames", {
  local_bindings(
    `[[.mydf` = function(x, ...) stop("Not allowed!"),
    .env = global_env()
  )

  df <- data.frame(x = 1:2, y = 2:1)
  class(df) <- c("mydf", "data.frame")

  expect_equal(pmap(df, list), list(list(x = 1, y = 2), list(x = 2, y = 1)))
  expect_equal(pmap_lgl(df, ~TRUE), c(TRUE, TRUE))
  expect_equal(pmap_int(df, ~2), c(2, 2))
  expect_equal(pmap_dbl(df, ~3.5), c(3.5, 3.5))
  expect_equal(pmap_chr(df, ~"x"), c("x", "x"))
})

test_that("pmap works with empty lists", {
  expect_identical(pmap(list(), ~1), list())
})

test_that("preserves S3 class of input vectors (#358)", {
  date <- as.Date("2018-09-27")
  expect_equal(pmap(list(date), identity), list(date))
  expect_output(pwalk(list(date), print), format(date))
})

test_that("works with vctrs records (#963)", {
  x <- new_rcrd(list(x = c(1, 2), y = c("a", "b")))
  out <- list(new_rcrd(list(x = 1, y = "a")), new_rcrd(list(x = 2, y = "b")))
  expect_identical(pmap(list(x, 1, 1:2), ~.x), out)
})

test_that("don't evaluate symbolic objects (#428)", {
  pmap(list(exprs(1 + 2)), ~ expect_identical(.x, quote(1 + 2)))
  pwalk(list(exprs(1 + 2)), ~ expect_identical(.x, quote(1 + 2)))
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/tests/testthat/test-progress-bars.R ---
test_that("useful for bad progress spec", {
  # Test map() to make sure we're passing the caller env correctly
  expect_snapshot(map(1, .progress = 1), error = TRUE)
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/tests/testthat/test-rate.R ---
test_that("new_rate() creates rate objects", {
  rate <- new_rate("foo", jitter = FALSE, max_times = 10)
  expect_identical(rate$state$i, 0L)
  expect_identical(rate$max_times, 10)
  expect_false(rate$jitter)
})

test_that("can bump and reset count", {
  rate <- new_rate("foo")

  rate_bump_count(rate)
  rate_bump_count(rate)
  expect_identical(rate_count(rate), 2L)

  rate_reset(rate)
  expect_identical(rate_count(rate), 0L)
})

test_that("rates have print methods", {
  expect_snapshot({
    # Also checks infinite `max_times` prints properly
    rate_delay(20, max_times = Inf)

    rate_backoff()
  })
})

test_that("rate_delay() delays", {
  rate <- rate_delay(
    pause = 0.02,
    max_times = 3
  )

  rate_sleep(rate, quiet = FALSE)

  rate_reset(rate)

  msg <- catch_cnd(rate_sleep(rate))
  expect_true(inherits_all(msg, c("purrr_condition_rate_init", "condition")))

  msg <- catch_cnd(rate_sleep(rate, quiet = FALSE))
  expect_true(inherits_all(msg, c("purrr_message_rate_retry", "message")))
  expect_identical(msg$length, 0.02)

  msg <- catch_cnd(rate_sleep(rate, quiet = FALSE))
  expect_identical(msg$length, 0.02)

  expect_snapshot(rate_sleep(rate), error = TRUE)
  expect_snapshot(rate_sleep(rate), error = TRUE)
})

test_that("rate_backoff() backs off", {
  rate <- rate_backoff(
    pause_base = 0.02,
    pause_min = 0,
    jitter = FALSE
  )

  msg <- catch_cnd(rate_sleep(rate))
  expect_true(inherits_all(msg, c("purrr_condition_rate_init", "condition")))

  msg <- catch_cnd(rate_sleep(rate, quiet = FALSE))
  expect_true(inherits_all(msg, c("purrr_message_rate_retry", "message")))
  expect_identical(msg$length, 0.04)

  msg <- catch_cnd(rate_sleep(rate, quiet = FALSE))
  expect_identical(msg$length, 0.08)

  expect_snapshot(rate_sleep(rate), error = TRUE)
  expect_snapshot(rate_sleep(rate), error = TRUE)
})

test_that("rate_sleep() checks that rate is still valid", {
  rate <- rate_delay(1, max_times = 0)
  expect_snapshot(rate_sleep(rate), error = TRUE)
  expect_snapshot(rate_sleep(rate), error = TRUE)
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/tests/testthat/test-reduce.R ---
test_that("empty input returns init or error", {
  expect_snapshot(reduce(list()), error = TRUE)

  expect_equal(reduce(list(), `+`, .init = 0), 0)
})

test_that("first/value value used as first value", {
  expect_equal(reduce(c(1, 1), `+`), 2)
  expect_equal(reduce(c(1, 1), `+`, .init = 1), 3)
})

test_that("length 1 argument reduced with init", {
  expect_equal(reduce(1, `+`, .init = 1), 2)
})

test_that("direction of reduce determines how generated trees lean", {
  expect_identical(reduce(1:4, list), list(list(list(1L, 2L), 3L), 4L))
  expect_identical(
    reduce(1:4, list, .dir = "backward"),
    list(1L, list(2L, list(3L, 4L)))
  )
})

test_that("can shortcircuit reduction with done()", {
  x <- c(TRUE, TRUE, FALSE, TRUE, TRUE)
  out <- reduce(x, ~ if (.y) c(.x, "foo") else done(.x), .init = NULL)
  expect_identical(out, c("foo", "foo"))

  # Empty done box yields the same value as returning the
  # result-so-far (the last value) in a done box
  out2 <- reduce(x, ~ if (.y) c(.x, "foo") else done(), .init = NULL)
  expect_identical(out2, out)
})

test_that("reduce() forces arguments (#643)", {
  compose <- function(f, g) function(x) f(g(x))
  expect_identical(reduce(list(identity, identity), compose)(1), 1)
})


# accumulate --------------------------------------------------------------

test_that("accumulate passes arguments to function", {
  tt <- c("a", "b", "c")

  expect_equal(accumulate(tt, paste, sep = "."), c("a", "a.b", "a.b.c"))
  expect_equal(
    accumulate(tt, paste, sep = ".", .dir = "backward"),
    c("a.b.c", "b.c", "c")
  )

  expect_equal(
    accumulate(tt, paste, sep = ".", .init = "z"),
    c("z", "z.a", "z.a.b", "z.a.b.c")
  )
  expect_equal(
    accumulate(tt, paste, sep = ".", .dir = "backward", .init = "z"),
    c("a.b.c.z", "b.c.z", "c.z", "z")
  )
})

test_that("accumulate keeps input names", {
  input <- set_names(1:26, letters)
  expect_identical(accumulate(input, sum), set_names(cumsum(1:26), letters))
  expect_identical(
    accumulate(input, sum, .dir = "backward"),
    set_names(rev(cumsum(rev(1:26))), rev(letters))
  )
})

test_that("accumulate keeps input names when init is supplied", {
  expect_identical(accumulate(1:2, c, .init = 0L), list(0L, 0:1, 0:2))
  expect_identical(
    accumulate(0:1, c, .init = 2L, .dir = "backward"),
    list(0:2, 1:2, 2L)
  )

  expect_identical(
    accumulate(c(a = 1L, b = 2L), c, .init = 0L),
    list(.init = 0L, a = 0:1, b = 0:2)
  )
  expect_identical(
    accumulate(c(a = 0L, b = 1L), c, .init = 2L, .dir = "backward"),
    list(b = 0:2, a = 1:2, .init = 2L)
  )
})

test_that("can terminate accumulate() early", {
  tt <- c("a", "b", "c")
  paste2 <- function(x, y) {
    out <- paste(x, y, sep = ".")
    if (x == "b" || y == "b") {
      done(out)
    } else {
      out
    }
  }

  expect_equal(accumulate(tt, paste2), c("a", "a.b"))
  expect_equal(accumulate(tt, paste2, .dir = "backward"), c("b.c", "c"))

  expect_equal(accumulate(tt, paste2, .init = "z"), c("z", "z.a", "z.a.b"))
  expect_equal(
    accumulate(tt, paste2, .dir = "backward", .init = "z"),
    c("b.c.z", "c.z", "z")
  )
})

test_that("can terminate accumulate() early with an empty box", {
  tt <- c("a", "b", "c")
  paste2 <- function(x, y) {
    out <- paste(x, y, sep = ".")
    if (x == "b" || y == "b") {
      done()
    } else {
      out
    }
  }

  expect_equal(accumulate(tt, paste2), "a")
  expect_equal(accumulate(tt, paste2, .dir = "backward"), "c")

  expect_equal(accumulate(tt, paste2, .init = "z"), c("z", "z.a"))
  expect_equal(
    accumulate(tt, paste2, .dir = "backward", .init = "z"),
    c("c.z", "z")
  )

  # Init value is always included, even if done at first iteration
  expect_equal(accumulate(c("b", "c"), paste2), "b")
})

test_that("accumulate() forces arguments (#643)", {
  compose <- function(f, g) function(x) f(g(x))
  fns <- accumulate(list(identity, identity), compose)
  expect_true(every(fns, function(f) identical(f(1), 1)))
})

test_that("accumulate() uses vctrs to simplify results", {
  out <- list("foo", factor("bar")) |> accumulate(~.y)
  expect_identical(out, c("foo", "bar"))
})

test_that("accumulate() does not fail when input can't be simplified", {
  expect_identical(accumulate(list(1L, 2:3), ~.y), list(1L, 2:3))
  expect_identical(accumulate(list(1, "a"), ~.y), list(1, "a"))
})

test_that("accumulate() does fail when simpification is required", {
  expect_snapshot(accumulate(list(1, "a"), ~.y, .simplify = TRUE), error = TRUE)
})

# reduce2 -----------------------------------------------------------------

test_that("basic application works", {
  paste2 <- function(x, y, sep) paste(x, y, sep = sep)

  x <- c("a", "b", "c")
  expect_equal(reduce2(x, c("-", "."), paste2), "a-b.c")
  expect_equal(reduce2(x, c(".", "-", "."), paste2, .init = "x"), "x.a-b.c")
})

test_that("requires equal length vectors", {
  expect_snapshot(reduce2(1:3, 1, `+`), error = TRUE)
})

test_that("requires init if `.x` is empty", {
  expect_snapshot(reduce2(list()), error = TRUE)
})

test_that("reduce returns original input if it was length one", {
  x <- list(c(0, 1), c(2, 3), c(4, 5))
  expect_equal(reduce(x[1], paste), x[[1]])
})

test_that("can shortcircuit reduce2() with done()", {
  x <- c(TRUE, TRUE, FALSE, TRUE, TRUE)
  out <- reduce2(x, 1:5, ~ if (.y) c(.x, "foo") else done(.x), .init = NULL)
  expect_identical(out, c("foo", "foo"))
})

test_that("reduce2() forces arguments (#643)", {
  compose <- function(f, g, ...) function(x) f(g(x))
  fns <- reduce2(list(identity, identity), "foo", compose)
  expect_identical(fns(1), 1)
})

# accumulate2 -------------------------------------------------------------

test_that("basic accumulate2() works", {
  paste2 <- function(x, y, sep) paste(x, y, sep = sep)

  x <- c("a", "b", "c")
  expect_equal(accumulate2(x, c("-", "."), paste2), c("a", "a-b", "a-b.c"))
  expect_equal(
    accumulate2(x, c(".", "-", "."), paste2, .init = "x"),
    c("x", "x.a", "x.a-b", "x.a-b.c")
  )
})

test_that("can terminate accumulate2() early", {
  paste2 <- function(x, y, sep) {
    out <- paste(x, y, sep = sep)
    if (y == "b") {
      done(out)
    } else {
      out
    }
  }

  x <- c("a", "b", "c")
  expect_equal(accumulate2(x, c("-", "."), paste2), c("a", "a-b"))
  expect_equal(
    accumulate2(x, c(".", "-", "."), paste2, .init = "x"),
    c("x", "x.a", "x.a-b")
  )
})

test_that("accumulate2() forces arguments (#643)", {
  compose <- function(f, g, ...) function(x) f(g(x))
  fns <- accumulate2(list(identity, identity), "foo", compose)
  expect_true(every(fns, function(f) identical(f(1), 1)))
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/tests/testthat/test-superseded-flatten.R ---
test_that("input must be a list", {
  local_options(lifecycle_verbosity = "quiet")

  expect_snapshot(flatten(1), error = TRUE)
  expect_snapshot(flatten_dbl(1), error = TRUE)
})

test_that("contents of list must be supported types", {
  local_options(lifecycle_verbosity = "quiet")

  expect_snapshot(flatten(list(quote(a))), error = TRUE)
  expect_snapshot(flatten(list(expression(a))), error = TRUE)
})

test_that("each second level element becomes first level element", {
  expect_equal(flatten(list(1:2)), list(1, 2))
  expect_equal(flatten(list(1, 2)), list(1, 2))
})

test_that("can flatten all atomic vectors", {
  expect_equal(flatten(list(F)), list(F))
  expect_equal(flatten(list(1L)), list(1L))
  expect_equal(flatten(list(1)), list(1))
  expect_equal(flatten(list("a")), list("a"))
  expect_equal(flatten(list(as.raw(1))), list(as.raw(1)))
  expect_equal(flatten(list(1i)), list(1i))
})

test_that("NULLs are silently dropped", {
  expect_equal(flatten(list(NULL, NULL)), list())
  expect_equal(flatten(list(NULL, 1)), list(1))
  expect_equal(flatten(list(1, NULL)), list(1))
})

test_that("names are preserved", {
  expect_equal(flatten(list(list(x = 1), list(y = 1))), list(x = 1, y = 1))
  expect_equal(flatten(list(list(a = 1, b = 2), 3)), list(a = 1, b = 2, 3))
})

test_that("names of 'scalar' elements are preserved", {
  out <- flatten(list(a = list(1), b = list(2)))
  expect_equal(out, list(a = 1, b = 2))

  out <- flatten(list(a = list(1), b = 2:3))
  expect_equal(out, list(a = 1, 2, 3))

  out <- flatten(list(list(a = 1, b = 2), c = 3))
  expect_equal(out, list(a = 1, b = 2, c = 3))
})

test_that("child names beat parent names", {
  out <- flatten(list(a = list(x = 1), b = list(y = 2)))
  expect_equal(out, list(x = 1, y = 2))
})


# atomic flatten ----------------------------------------------------------

test_that("must be a list", {
  local_options(lifecycle_verbosity = "quiet")

  expect_snapshot(flatten_lgl(1), error = TRUE)
})

test_that("can flatten all atomic vectors", {
  expect_equal(flatten_lgl(list(F)), F)
  expect_equal(flatten_int(list(1L)), 1L)
  expect_equal(flatten_dbl(list(1)), 1)
  expect_equal(flatten_chr(list("a")), "a")
})

test_that("preserves inner names", {
  expect_equal(
    flatten_dbl(list(c(a = 1), c(b = 2))),
    c(a = 1, b = 2)
  )
})


# data frame flatten ------------------------------------------------------

test_that("can flatten to a data frame with named lists", {
  skip_if_not_installed("dplyr")

  dfs <- list(c(a = 1), c(b = 2))
  expect_equal(flatten_dfr(dfs), tibble::tibble(a = 1, b = 2))
  expect_equal(flatten_dfc(dfs), tibble::tibble(a = 1, b = 2))
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/tests/testthat/test-superseded-map-df.R ---
test_that("row and column binding work", {
  skip_if_not_installed("dplyr")
  local_name_repair_quiet()

  mtcar_mod <- mtcars |>
    split(mtcars$cyl) |>
    map(~ lm(mpg ~ wt, data = .x))

  f_coef <- function(x) as.data.frame(t(as.matrix(coef(x))))
  expect_length(mtcar_mod |> map_dfr(f_coef), 2)
  expect_length(mtcar_mod |> map_dfc(f_coef), 6)
})

test_that("data frame imap works", {
  skip_if_not_installed("dplyr")
  x <- set_names(1:3)
  expect_identical(imap_dfc(x, paste), imap_dfr(x, paste))
})

test_that("outputs are suffixes have correct type for data frames", {
  skip_if_not_installed("dplyr")
  local_name_repair_quiet()

  local_options(rlang_message_verbosity = "quiet")
  x <- 1:3
  expect_s3_class(pmap_dfr(list(x), as.data.frame), "data.frame")
  expect_s3_class(pmap_dfc(list(x), as.data.frame), "data.frame")
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/tests/testthat/test-superseded-simplify.R ---
test_that("can_simplify() understands vector molds", {
  x <- as.list(1:3)
  x2 <- c(x, list(1:3))
  expect_true(can_simplify(x, integer(1)))
  expect_false(can_simplify(x, character(1)))
  expect_false(can_simplify(x2, integer(1)))

  x3 <- list(1:2, 3:4, 5:6)
  expect_true(can_simplify(x3, integer(2)))
  expect_false(can_simplify(x, integer(2)))
})

test_that("can_simplify() understands types as strings", {
  x <- as.list(1:3)
  expect_true(can_simplify(x, "integer"))
  expect_false(can_simplify(x, "character"))
})

test_that("integer is coercible to double", {
  x <- list(1L, 2L)
  expect_true(can_simplify(x, "numeric"))
  expect_true(can_simplify(x, numeric(1)))
  expect_true(can_simplify(x, "double"))
  expect_true(can_simplify(x, double(1)))
})

test_that("numeric is an alias for double", {
  expect_true(can_simplify(list(1, 2), "numeric"))
})

test_that("double is not coercible to integer", {
  expect_false(can_simplify(list(1, 2), "integer"))
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/tests/testthat/test-superseded-transpose.R ---
test_that("input must be a list", {
  expect_snapshot(transpose(1:3), error = TRUE)
})

test_that("elements of input must be atomic vectors", {
  expect_snapshot(transpose(list(environment())), error = TRUE)
  expect_snapshot(transpose(list(list(), environment())), error = TRUE)
})

test_that("empty list returns empty list", {
  expect_equal(transpose(list()), list())
})

test_that("transpose switches order of first & second idnex", {
  x <- list(list(1, 3), list(2, 4))
  expect_equal(transpose(x), list(list(1, 2), list(3, 4)))
})

test_that("inside names become outside names", {
  x <- list(list(x = 1), list(x = 2))
  expect_equal(transpose(x), list(x = list(1, 2)))
})

test_that("outside names become inside names", {
  x <- list(x = list(1, 3), y = list(2, 4))
  expect_equal(transpose(x), list(list(x = 1, y = 2), list(x = 3, y = 4)))
})

test_that("warns if element too short", {
  x <- list(list(1, 2), list(1))
  expect_warning(out <- transpose(x), "Element 2 must be length 2, not 1")
  expect_equal(out, list(list(1, 1), list(2, NULL)))
})
test_that("warns if element too long", {
  x <- list(list(1, 2), list(1, 2, 3))
  expect_warning(out <- transpose(x), "Element 2 must be length 2, not 3")
  expect_equal(out, list(list(1, 1), list(2, 2)))
})

test_that("can transpose list of lists of  atomic vectors", {
  x <- list(list(TRUE, 1L, 1, "1"))
  expect_equal(transpose(x), list(list(TRUE), list(1L), list(1), list("1")))
})

test_that("can transpose lists of atomic vectors", {
  expect_equal(transpose(list(TRUE, FALSE)), list(list(TRUE, FALSE)))
  expect_equal(transpose(list(1L, 2L)), list(list(1L, 2L)))
  expect_equal(transpose(list(1, 2)), list(list(1, 2)))
  expect_equal(transpose(list("a", "b")), list(list("a", "b")))
})

test_that("can't transpose expressions", {
  expect_snapshot(transpose(list(expression(a))), error = TRUE)
})

# Named based matching ----------------------------------------------------

test_that("can override default names", {
  x <- list(
    list(x = 1),
    list(y = 2, x = 1)
  )
  tx <- transpose(x, c("x", "y"))

  expect_equal(
    tx,
    list(
      x = list(1, 1),
      y = list(NULL, 2)
    )
  )
})

test_that("if present, names are used", {
  x <- list(
    list(x = 1, y = 2),
    list(y = 2, x = 1)
  )
  tx <- transpose(x)

  expect_equal(tx$x, list(1, 1))
  expect_equal(tx$y, list(2, 2))
})

test_that("if missing elements, filled with NULL", {
  x <- list(
    list(x = 1, y = 2),
    list(x = 1)
  )
  tx <- transpose(x)
  expect_equal(tx$y, list(2, NULL))
})

# Position based matching -------------------------------------------------

test_that("warning if too short", {
  x <- list(
    list(1, 2),
    list(1)
  )
  expect_warning(tx <- transpose(x), "must be length 2, not 1")
  expect_equal(tx, list(list(1, 1), list(2, NULL)))
})

test_that("warning if too long", {
  x <- list(
    list(1),
    list(1, 2)
  )
  expect_warning(tx <- transpose(x), "must be length 1, not 2")
  expect_equal(tx, list(list(1, 1)))
})


# --- FILE: https://raw.githubusercontent.com/tidyverse/purrr/main/tests/testthat/test-utils.R ---
# where_at ------------------------------------------------------------

test_that("allows valid logical, numeric, and character vectors", {
  x <- list(a = 1, b = 1, c = 1)
  expect_equal(where_at(x, TRUE), c(TRUE, TRUE, TRUE))
  expect_equal(where_at(x, 1), c(TRUE, FALSE, FALSE))
  expect_equal(where_at(x, -2), c(TRUE, FALSE, TRUE))
  expect_equal(where_at(x, "b"), c(FALSE, TRUE, FALSE))
})

test_that("errors on invalid subsetting vectors", {
  x <- list(a = 1, b = 1, c = 1)
  expect_snapshot(error = TRUE, {
    where_at(x, c(FALSE, TRUE))
    where_at(x, NA_real_)
    where_at(x, 4)
  })
})

test_that("function at is passed names", {
  x <- list(a = 1, B = 1, c = 1)
  expect_equal(where_at(x, ~ .x %in% LETTERS), c(FALSE, TRUE, FALSE))
  expect_equal(where_at(x, ~ intersect(.x, LETTERS)), c(FALSE, TRUE, FALSE))
})

test_that("where_at works with unnamed input", {
  x <- list(1, 1, 1)
  expect_equal(where_at(x, letters), rep(FALSE, 3))
  expect_equal(where_at(x, ~ intersect(.x, LETTERS)), rep(FALSE, 3))
})

test_that("validates its inputs", {
  x <- list(a = 1, b = 1, c = 1)
  expect_snapshot(where_at(x, list()), error = TRUE)
})

test_that("tidyselect `at` is deprecated", {
  skip_if_not_installed("tidyselect")
  expect_snapshot({
    . <- where_at(data.frame(x = 1), vars("x"), user_env = globalenv())
  })
})


# vctrs compat ------------------------------------------------------------

test_that("arrays become vectors (#970)", {
  x <- matrix(1:4, nrow = 2)
  expect_equal(vctrs_vec_compat(x, globalenv()), 1:4)

  f <- factor(letters[1:4])
  dim(f) <- c(2, 2, 1)
  expect_equal(vctrs_vec_compat(f, globalenv()), factor(letters[1:4]))
})

test_that("pairlists, expressions, and calls are deprecated", {
  local_options(lifecycle_verbosity = "warning")

  expect_snapshot(x <- vctrs_vec_compat(expression(1, 2), globalenv()))
  expect_equal(x, list(1, 2))

  expect_snapshot(x <- vctrs_vec_compat(pairlist(1, 2), globalenv()))
  expect_equal(x, list(1, 2))

  expect_snapshot(x <- vctrs_vec_compat(quote(f(a, b = 1)), globalenv()))
  expect_equal(x, list(quote(f), quote(a), b = 1))
})

test_that("can work with S4 vector objects", {
  foo <- methods::setClass("foo1", contains = "list", where = current_env())
  on.exit(methods::removeClass("foo1", where = current_env()), add = TRUE)

  x1 <- foo(list(1, 2, 3))
  expect_equal(map(x1, identity), list(1, 2, 3))

  x2 <- foo(list(x = 1, y = 2, z = 3))
  expect_equal(map(x2, identity), list(x = 1, y = 2, z = 3))
})

test_that("preserves names of 1d arrays", {
  v <- array(list(1, 2), dim = 2, dimnames = list(c("a", "b")))
  expect_equal(map_dbl(v, identity), c(a = 1, b = 2))
})

test_that("can work with output of by", {
  df <- data.frame(x = 1:2)

  # 1d keeps names
  x <- by(df, c("a", "b"), function(df) df$x)
  expect_equal(map_dbl(x, identity), c(a = 1, b = 2))

  x <- by(df, c("a", "b"), function(df) df$x, simplify = FALSE)
  expect_equal(map_dbl(x, identity), c(a = 1, b = 2))

  # 2d loses names
  x <- by(df, list(c("a", "b"), c("a", "b")), function(df) df$x)
  expect_equal(map_dbl(x, identity), c(1, NA, NA, 2))

  x <- by(
    df,
    list(c("a", "b"), c("a", "b")),
    function(df) df$x,
    simplify = FALSE
  )
  expect_equal(map(x, identity), list(1, NULL, NULL, 2))
})

test_that("can work with lubridate periods", {
  skip_if_not_installed("lubridate")
  days <- lubridate::days(1:2)

  expect_equal(
    map(days, identity),
    list(lubridate::days(1), lubridate::days(2))
  )
})

test_that("can't work with regular S4 objects", {
  foo <- methods::setClass(
    "foo",
    slots = list(a = "integer"),
    where = global_env()
  )
  on.exit(methods::removeClass("foo", where = global_env()), add = TRUE)

  expect_snapshot(map(foo(), identity), error = TRUE)
})
