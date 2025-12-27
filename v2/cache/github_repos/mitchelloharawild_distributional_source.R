

# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/R/default.R ---
#' @export
density.dist_default <- function(x, ...){
  abort(
    sprintf("The distribution class `%s` does not support `density()`",
            class(x)[1])
  )
}

#' @export
log_density.dist_default <- function(x, ...){
  log(density(x, ...))
}

#' @export
quantile.dist_default <- function(x, p, ...){
  # abort(
  #   sprintf("The distribution class `%s` does not support `quantile()`",
  #           class(x)[1])
  # )
  stats::optim(0, function(pos){
    (p - cdf(x, pos, ...))^2
  })$par
}
#' @export
log_quantile.dist_default <- function(x, p, ...){
  quantile(x, exp(p), ...)
}
#' @export
cdf.dist_default <- function(x, q, times = 1e5,...){
  # Use Monte Carlo integration
  r <- generate(x, times = times)
  if(is.list(q)) {
    # Turn into matrix
    q <- do.call(rbind, q)
  }
  out <- numeric(NROW(q))
  for(i in seq_along(out)) {
    out[i] <- mean(apply(sweep(r, 2, q[i,]) < 0, 1, all))
  }
  return(out)
}
#' @export
log_cdf.dist_default <- function(x, q, ...){
  log(cdf(x, q, ...))
}

#' @export
generate.dist_default <- function(x, times, ...){
  vapply(stats::runif(times,0,1), quantile, numeric(1L), x = x, ...)
}

#' @export
likelihood.dist_default <- function(x, sample, ...){
  prod(vapply(sample, density, numeric(1L), x = x))
}

#' @export
log_likelihood.dist_default <- function(x, sample, ...){
  sum(vapply(sample, log_density, numeric(1L), x = x))
}

#' @export
parameters.dist_default <- function(x, ...) {
  # Reduce parameter structures to length 1 list if needed.
  lapply(unclass(x), function(z) {
    if(inherits(z, "dist_default")) wrap_dist(list(z))
    else if (tryCatch(vec_size(z), error = function(e) Inf) > 1) list(z)
    else z
  })
}

#' @export
family.dist_default <- function(object, ...) {
  substring(class(object)[1], first = 6)
}

#' @export
support.dist_default <- function(x, ...) {
  lims <- quantile(x, c(0, 1))
  closed <- if(any(is.na(lims))) {
    c(FALSE, FALSE)
  } else {
    # Default to open limits on error
    lim_dens <- tryCatch(
      suppressWarnings(density(x, lims)),
      error = function(e) c(0,0)
    )
    !near(lim_dens, 0)
  }
  new_support_region(
    list(vctrs::vec_init(generate(x, 1), n = 0L)),
    list(lims),
    list(closed)
  )
}

#' @export
mean.dist_default <- function(x, ...){
  x_sup <- support(x)
  dist_type <- field(x_sup, "x")[[1]]
  if (!is.numeric(dist_type)) return(NA_real_)
  if (is.double(dist_type)) {
    limits <- field(x_sup, "lim")[[1]]
    tryCatch(
      stats::integrate(function(at) density(x, at) * at, limits[1], limits[2])$value,
      error = function(e) NA_real_
    )
  } else {
    mean(quantile(x, stats::ppoints(1000)), na.rm = TRUE)
  }
}
#' @export
variance.dist_default <- function(x, ...){
  x <- covariance(x, ...)
  if(is.matrix(x[[1]]) && ncol(x[[1]]) > 1){
    matrix(diag(x[[1]]), nrow = 1)
  } else x
}
#' @export
covariance.dist_default <- function(x, ...){
  x_sup <- support(x)
  dist_type <- field(x_sup, "x")[[1]]
  if (!is.numeric(dist_type)) return(NA_real_)
  else if (is.matrix(dist_type)) stats::cov(generate(x, times = 1000))
  else if (is.double(dist_type)) {
    limits <- field(x_sup, "lim")[[1]]
    tryCatch(
      stats::integrate(function(at) density(x, at) * at^2, limits[1], limits[2])$value,
      error = function(e) NA_real_
    ) - mean(x)^2
  } else {
    stats::var(quantile(x, stats::ppoints(1000)), na.rm = TRUE)
  }
}

#' @export
median.dist_default <- function(x, na.rm = FALSE, ...){
  quantile(x, p = 0.5, ...)
}

#' @export
hilo.dist_default <- function(x, size = 95, ...){
  lower <- quantile(x, 0.5-size/200, ...)
  upper <- quantile(x, 0.5+size/200, ...)
  if(is.matrix(lower) && is.matrix(upper)) {
    return(
      vctrs::new_data_frame(split(
        new_hilo(drop(lower), drop(upper), size = rep_len(size, length(lower))),
      seq_along(lower)))
    )
  }
  new_hilo(lower, upper, size)
}

#' @export
hdr.dist_default <- function(x, size = 95, n = 512, ...){
  dist_x <- quantile(x, seq(0.5/n, 1 - 0.5/n, length.out = n))
  # Remove duplicate values of dist_x from less continuous distributions
  dist_x <- unique(dist_x)
  dist_y <- density(x, dist_x)
  alpha <- quantile(dist_y, probs = 1-size/100)

  crossing_alpha <- function(alpha, x, y){
    it <- seq_len(length(y) - 1)
    dd <- y - alpha
    dd <- dd[it + 1] * dd[it]
    index <- it[dd <= 0]
    # unique() removes possible duplicates if sequential dd has same value.
    # More robust approach is required.
    out <- unique(
      vapply(
        index,
        function(.x) stats::approx(y[.x + c(0,1)], x[.x + c(0,1)], xout = alpha)$y,
        numeric(1L)
      )
    )
    # Add boundary values which may exceed the crossing point.
    c(x[1][y[1]>alpha], out, x[length(x)][y[length(y)]>alpha])
  }

  # purrr::map(alpha, crossing_alpha, dist_x, dist_y)
  hdr <- crossing_alpha(alpha, dist_x, dist_y)
  lower_hdr <- seq_along(hdr)%%2==1
  new_hdr(lower = list(hdr[lower_hdr]), upper = list(hdr[!lower_hdr]), size = size)
}

#' @export
format.dist_default <- function(x, ...){
  "?"
}

#' @export
print.dist_default <- function(x, ...){
  cat(format(x, ...))
}

#' @export
dim.dist_default <- function(x){
  # Quick and dirty dimension calculation
  NCOL(generate(x, times = 1))
}

invert_fail <- function(...) stop("Inverting transformations for distributions is not yet supported.")

#' Attempt to get the inverse of f(x) by name. Returns invert_fail
#' (a function that raises an error if called) if there is no known inverse.
#' @param f string. Name of a function.
#' @noRd
get_unary_inverse <- function(f) {
  switch(f,
    sqrt = function(x) x^2,
    exp = log,
    log = function(x, base = exp(1)) base ^ x,
    log2 = function(x) 2^x,
    log10 = function(x) 10^x,
    expm1 = log1p,
    log1p = expm1,
    cos = acos,
    sin = asin,
    tan = atan,
    acos = cos,
    asin = sin,
    atan = tan,
    cosh = acosh,
    sinh = asinh,
    tanh = atanh,
    acosh = cosh,
    asinh = sinh,
    atanh = tanh,

    invert_fail
  )
}

#' Attempt to get the inverse of f(x, constant) by name. Returns invert_fail
#' (a function that raises an error if called) if there is no known inverse.
#' @param f string. Name of a function.
#' @param constant a constant value
#' @noRd
get_binary_inverse_1 <- function(f, constant) {
  force(constant)

  switch(f,
    `+` = function(x) x - constant,
    `-` = function(x) x + constant,
    `*` = function(x) x / constant,
    `/` = function(x) x * constant,
    `^` = function(x) x ^ (1/constant),

    invert_fail
  )
}

#' Attempt to get the inverse of f(constant, x) by name. Returns invert_fail
#' (a function that raises an error if called) if there is no known inverse.
#' @param f string. Name of a function.
#' @param constant a constant value
#' @noRd
get_binary_inverse_2 <- function(f, constant) {
  force(constant)

  switch(f,
    `+` = function(x) x - constant,
    `-` = function(x) constant - x,
    `*` = function(x) x / constant,
    `/` = function(x) constant / x,
    `^` = function(x) log(x, base = constant),

    invert_fail
  )
}

#' @method Math dist_default
#' @export
Math.dist_default <- function(x, ...) {
  if(dim(x) > 1) stop("Transformations of multivariate distributions are not yet supported.")

  trans <- new_function(exprs(x = ), body = expr((!!sym(.Generic))(x, !!!dots_list(...))))

  inverse_fun <- get_unary_inverse(.Generic)
  inverse <- new_function(exprs(x = ), body = expr((!!inverse_fun)(x, !!!dots_list(...))))

  vec_data(dist_transformed(wrap_dist(list(x)), trans, inverse))[[1]]
}

#' @method Ops dist_default
#' @export
Ops.dist_default <- function(e1, e2) {
  if(.Generic %in% c("-", "+") && missing(e2)){
    e2 <- e1
    e1 <- if(.Generic == "+") 1 else -1
    .Generic <- "*"
  }
  is_dist <- c(inherits(e1, "dist_default"), inherits(e2, "dist_default"))
  if(any(vapply(list(e1, e2)[is_dist], dim, numeric(1L)) > 1)){
    stop("Transformations of multivariate distributions are not yet supported.")
  }

  trans <- if(all(is_dist)) {
    if(identical(e1$dist, e2$dist)){
      new_function(exprs(x = ), expr((!!sym(.Generic))((!!e1$transform)(x), (!!e2$transform)(x))))
    } else {
      stop(sprintf("The %s operation is not supported for <%s> and <%s>", .Generic, class(e1)[1], class(e2)[1]))
    }
  } else if(is_dist[1]){
    new_function(exprs(x = ), body = expr((!!sym(.Generic))(x, !!e2)))
  } else {
    new_function(exprs(x = ), body = expr((!!sym(.Generic))(!!e1, x)))
  }

  inverse <- if(all(is_dist)) {
    invert_fail
  } else if(is_dist[1]){
    get_binary_inverse_1(.Generic, e2)
  } else {
    get_binary_inverse_2(.Generic, e1)
  }

  vec_data(dist_transformed(wrap_dist(list(e1,e2)[which(is_dist)]), trans, inverse))[[1]]
}


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/R/dist_bernoulli.R ---
#' The Bernoulli distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' Bernoulli distributions are used to represent events like coin flips
#' when there is single trial that is either successful or unsuccessful.
#' The Bernoulli distribution is a special case of the [Binomial()]
#' distribution with `n = 1`.
#'
#' @inheritParams dist_binomial
#'
#' @details
#'
#'   We recommend reading this documentation on
#'   <https://pkg.mitchelloharawild.com/distributional/>, where the math
#'   will render nicely.
#'
#'   In the following, let \eqn{X} be a Bernoulli random variable with parameter
#'   `p` = \eqn{p}. Some textbooks also define \eqn{q = 1 - p}, or use
#'   \eqn{\pi} instead of \eqn{p}.
#'
#'   The Bernoulli probability  distribution is widely used to model
#'   binary variables, such as 'failure' and 'success'. The most
#'   typical example is the flip of a coin, when  \eqn{p} is thought as the
#'   probability of flipping a head, and \eqn{q = 1 - p} is the
#'   probability of flipping a tail.
#'
#'   **Support**: \eqn{\{0, 1\}}{{0, 1}}
#'
#'   **Mean**: \eqn{p}
#'
#'   **Variance**: \eqn{p \cdot (1 - p) = p \cdot q}{p (1 - p)}
#'
#'   **Probability mass function (p.m.f)**:
#'
#'   \deqn{
#'     P(X = x) = p^x (1 - p)^{1-x} = p^x q^{1-x}
#'   }{
#'     P(X = x) = p^x (1 - p)^(1-x)
#'   }
#'
#'   **Cumulative distribution function (c.d.f)**:
#'
#'   \deqn{
#'     P(X \le x) =
#'     \left \{
#'       \begin{array}{ll}
#'         0 & x < 0 \\
#'         1 - p & 0 \leq x < 1 \\
#'         1 & x \geq 1
#'       \end{array}
#'     \right.
#'   }{
#'     P(X \le x) = (1 - p) 1_{[0, 1)}(x) + 1_{1}(x)
#'   }
#'
#'   **Moment generating function (m.g.f)**:
#'
#'   \deqn{
#'     E(e^{tX}) = (1 - p) + p e^t
#'   }{
#'     E(e^(tX)) = (1 - p) + p e^t
#'   }
#'
#' @examples
#' dist <- dist_bernoulli(prob = c(0.05, 0.5, 0.3, 0.9, 0.1))
#'
#' dist
#' mean(dist)
#' variance(dist)
#' skewness(dist)
#' kurtosis(dist)
#'
#' generate(dist, 10)
#'
#' density(dist, 2)
#' density(dist, 2, log = TRUE)
#'
#' cdf(dist, 4)
#'
#' quantile(dist, 0.7)
#'
#' @export
dist_bernoulli <- function(prob){
  prob <- vec_cast(prob, double())
  if(any((prob < 0) | (prob > 1))){
    abort("The probability of success must be between 0 and 1.")
  }
  new_dist(p = prob, class = "dist_bernoulli")
}

#' @export
format.dist_bernoulli <- function(x, digits = 2, ...){
  sprintf(
    "Bernoulli(%s)",
    format(x[["p"]], digits = digits, ...)
  )
}

#' @export
density.dist_bernoulli <- function(x, at, ...){
  stats::dbinom(at, 1, x[["p"]])
}

#' @export
log_density.dist_bernoulli <- function(x, at, ...){
  stats::dbinom(at, 1, x[["p"]], log = TRUE)
}

#' @export
quantile.dist_bernoulli <- function(x, p, ...){
  as.logical(stats::qbinom(p, 1, x[["p"]]))
}

#' @export
cdf.dist_bernoulli <- function(x, q, ...){
  stats::pbinom(q, 1, x[["p"]])
}

#' @export
generate.dist_bernoulli <- function(x, times, ...){
  as.logical(stats::rbinom(times, 1, x[["p"]]))
}

#' @export
mean.dist_bernoulli <- function(x, ...){
  x[["p"]]
}

#' @export
covariance.dist_bernoulli <- function(x, ...){
  x[["p"]]*(1-x[["p"]])
}

#' @export
skewness.dist_bernoulli <- function(x, ...) {
  p <- x[["p"]]
  q <- 1 - x[["p"]]
  (1 - (2 * p)) / sqrt(p * q)
}

#' @export
kurtosis.dist_bernoulli <- function(x, ...) {
  p <- x[["p"]]
  q <- 1 - x[["p"]]
  (1 - (6 * p * q)) / (p * q)
}


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/R/dist_beta.R ---
#' The Beta distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' @param shape1,shape2 The non-negative shape parameters of the Beta distribution.
#'
#' @seealso [stats::Beta]
#'
#' @examples
#' dist <- dist_beta(shape1 = c(0.5, 5, 1, 2, 2), shape2 = c(0.5, 1, 3, 2, 5))
#'
#' dist
#' mean(dist)
#' variance(dist)
#' skewness(dist)
#' kurtosis(dist)
#'
#' generate(dist, 10)
#'
#' density(dist, 2)
#' density(dist, 2, log = TRUE)
#'
#' cdf(dist, 4)
#'
#' quantile(dist, 0.7)
#'
#' @name dist_beta
#' @export
dist_beta <- function(shape1, shape2){
  shape1 <- vec_cast(shape1, double())
  shape2 <- vec_cast(shape2, double())
  if(any((shape1 < 0) | shape2 < 0)){
    abort("Shape parameters of a Beta distribution must be non-negative.")
  }
  new_dist(shape1 = shape1, shape2 = shape2, class = "dist_beta")
}

#' @export
format.dist_beta <- function(x, digits = 2, ...){
  sprintf(
    "Beta(%s, %s)",
    format(x[["shape1"]], digits = digits, ...),
    format(x[["shape2"]], digits = digits, ...)
  )
}

#' @export
density.dist_beta <- function(x, at, ...){
  stats::dbeta(at, x[["shape1"]], x[["shape2"]])
}

#' @export
log_density.dist_beta <- function(x, at, ...){
  stats::dbeta(at, x[["shape1"]], x[["shape2"]], log = TRUE)
}

#' @export
quantile.dist_beta <- function(x, p, ...){
  stats::qbeta(p, x[["shape1"]], x[["shape2"]])
}

#' @export
cdf.dist_beta <- function(x, q, ...){
  stats::pbeta(q, x[["shape1"]], x[["shape2"]])
}

#' @export
generate.dist_beta <- function(x, times, ...){
  stats::rbeta(times, x[["shape1"]], x[["shape2"]])
}

#' @export
mean.dist_beta <- function(x, ...){
  x[["shape1"]]/(x[["shape1"]] + x[["shape2"]])
}

#' @export
covariance.dist_beta <- function(x, ...){
  a <- x[["shape1"]]
  b <- x[["shape2"]]
  a*b/((a+b)^2*(a+b+1))
}

#' @export
skewness.dist_beta <- function(x, ...) {
  a <- x[["shape1"]]
  b <- x[["shape2"]]
  2 * (b - a) * sqrt(a + b + 1) / (a + b + 2) * sqrt(a * b)
}

#' @export
kurtosis.dist_beta <- function(x, ...) {
  a <- x[["shape1"]]
  b <- x[["shape2"]]
  num <- 6 * ((a - b)^2 * (a + b + 1) - (a * b) * (a + b + 2))
  denom <- a * b * (a + b + 2) * (a + b + 3)
  num / denom
}


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/R/dist_binomial.R ---
#' The Binomial distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' Binomial distributions are used to represent situations can that can
#' be thought as the result of \eqn{n} Bernoulli experiments (here the
#' \eqn{n} is defined as the `size` of the experiment). The classical
#' example is \eqn{n} independent coin flips, where each coin flip has
#' probability `p` of success. In this case, the individual probability of
#' flipping heads or tails is given by the  Bernoulli(p) distribution,
#' and the probability of having \eqn{x} equal results (\eqn{x} heads,
#' for example), in \eqn{n} trials is given by the Binomial(n, p) distribution.
#' The equation of the Binomial distribution is directly derived from
#' the equation of the Bernoulli distribution.
#'
#' @param size The number of trials. Must be an integer greater than or equal
#'   to one. When `size = 1L`, the Binomial distribution reduces to the
#'   Bernoulli distribution. Often called `n` in textbooks.
#' @param prob The probability of success on each trial, `prob` can be any
#'   value in `[0, 1]`.
#'
#' @details
#'
#'   We recommend reading this documentation on
#'   <https://pkg.mitchelloharawild.com/distributional/>, where the math
#'   will render nicely.
#'
#'   The Binomial distribution comes up when you are interested in the portion
#'   of people who do a thing. The Binomial distribution
#'   also comes up in the sign test, sometimes called the Binomial test
#'   (see [stats::binom.test()]), where you may need the Binomial C.D.F. to
#'   compute p-values.
#'
#'   In the following, let \eqn{X} be a Binomial random variable with parameter
#'   `size` = \eqn{n} and `p` = \eqn{p}. Some textbooks define \eqn{q = 1 - p},
#'   or called \eqn{\pi} instead of \eqn{p}.
#'
#'   **Support**: \eqn{\{0, 1, 2, ..., n\}}{{0, 1, 2, ..., n}}
#'
#'   **Mean**: \eqn{np}
#'
#'   **Variance**: \eqn{np \cdot (1 - p) = np \cdot q}{np (1 - p)}
#'
#'   **Probability mass function (p.m.f)**:
#'
#'   \deqn{
#'     P(X = k) = {n \choose k} p^k (1 - p)^{n-k}
#'   }{
#'     P(X = k) = choose(n, k) p^k (1 - p)^(n - k)
#'   }
#'
#'   **Cumulative distribution function (c.d.f)**:
#'
#'   \deqn{
#'     P(X \le k) = \sum_{i=0}^{\lfloor k \rfloor} {n \choose i} p^i (1 - p)^{n-i}
#'   }{
#'     P(X \le k) = \sum_{i=0}^k choose(n, i) p^i (1 - p)^(n-i)
#'   }
#'
#'   **Moment generating function (m.g.f)**:
#'
#'   \deqn{
#'     E(e^{tX}) = (1 - p + p e^t)^n
#'   }{
#'     E(e^(tX)) = (1 - p + p e^t)^n
#'   }
#'
#' @examples
#' dist <- dist_binomial(size = 1:5, prob = c(0.05, 0.5, 0.3, 0.9, 0.1))
#'
#' dist
#' mean(dist)
#' variance(dist)
#' skewness(dist)
#' kurtosis(dist)
#'
#' generate(dist, 10)
#'
#' density(dist, 2)
#' density(dist, 2, log = TRUE)
#'
#' cdf(dist, 4)
#'
#' quantile(dist, 0.7)
#'
#' @name dist_binomial
#' @export
dist_binomial <- function(size, prob){
  size <- vec_cast(size, integer())
  prob <- vec_cast(prob, double())
  if(any(size < 0)){
    abort("The number of observations cannot be negative.")
  }
  if(any((prob < 0) | (prob > 1))){
    abort("The probability of success must be between 0 and 1.")
  }
  new_dist(n = size, p = prob, class = "dist_binomial")
}

#' @export
format.dist_binomial <- function(x, digits = 2, ...){
  sprintf(
    "B(%s, %s)",
    format(x[["n"]], digits = digits, ...),
    format(x[["p"]], digits = digits, ...)
  )
}

#' @export
density.dist_binomial <- function(x, at, ...){
  stats::dbinom(at, x[["n"]], x[["p"]])
}

#' @export
log_density.dist_binomial <- function(x, at, ...){
  stats::dbinom(at, x[["n"]], x[["p"]], log = TRUE)
}

#' @export
quantile.dist_binomial <- function(x, p, ...){
  as.integer(stats::qbinom(p, x[["n"]], x[["p"]]))
}

#' @export
cdf.dist_binomial <- function(x, q, ...){
  stats::pbinom(q, x[["n"]], x[["p"]])
}

#' @export
generate.dist_binomial <- function(x, times, ...){
  as.integer(stats::rbinom(times, x[["n"]], x[["p"]]))
}

#' @export
mean.dist_binomial <- function(x, ...){
  x[["n"]]*x[["p"]]
}

#' @export
covariance.dist_binomial <- function(x, ...){
  x[["n"]]*x[["p"]]*(1-x[["p"]])
}

#' @export
skewness.dist_binomial <- function(x, ...) {
  n <- x[["n"]]
  p <- x[["p"]]
  q <- 1 - p
  (1 - (2 * p)) / sqrt(n * p * q)
}

#' @export
kurtosis.dist_binomial <- function(x, ...) {
  n <- x[["n"]]
  p <- x[["p"]]
  q <- 1 - p
  (1 - (6 * p * q)) / (n * p * q)
}


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/R/dist_burr.R ---
#' The Burr distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' @inheritParams actuar::dburr
#'
#' @seealso [actuar::Burr]
#'
#' @examples
#' dist <- dist_burr(shape1 = c(1,1,1,2,3,0.5), shape2 = c(1,2,3,1,1,2))
#' dist
#'
#' @examplesIf requireNamespace("actuar", quietly = TRUE)
#' mean(dist)
#' variance(dist)
#' support(dist)
#' generate(dist, 10)
#'
#' density(dist, 2)
#' density(dist, 2, log = TRUE)
#'
#' cdf(dist, 4)
#'
#' quantile(dist, 0.7)
#'
#' @name dist_burr
#' @export
dist_burr <- function(shape1, shape2, rate = 1, scale = 1/rate){
  shape1 <- vec_cast(shape1, double())
  shape2 <- vec_cast(shape2, double())
  if(any(shape1 <= 0)){
    abort("The shape1 parameter of a Burr distribution must be strictly positive.")
  }
  if(any(shape2 <= 0)){
    abort("The shape2 parameter of a Burr distribution must be strictly positive.")
  }
  if(any(rate <= 0)){
    abort("The rate parameter of a Burr distribution must be strictly positive.")
  }
  new_dist(s1 = shape1, s2 = shape2, r = 1/scale, class = "dist_burr")
}

#' @export
format.dist_burr <- function(x, digits = 2, ...){
  sprintf(
    "Burr12(%s, %s, %s)",
    format(x[["s1"]], digits = digits, ...),
    format(x[["s2"]], x[["r"]], digits = digits, ...),
    format(x[["r"]], digits = digits, ...)
  )
}

#' @export
density.dist_burr <- function(x, at, ...){
  require_package("actuar")
  actuar::dburr(at, x[["s1"]], x[["s2"]], x[["r"]])
}

#' @export
log_density.dist_burr <- function(x, at, ...){
  require_package("actuar")
  actuar::dburr(at, x[["s1"]], x[["s2"]], x[["r"]], log = TRUE)
}

#' @export
quantile.dist_burr <- function(x, p, ...){
  require_package("actuar")
  actuar::qburr(p, x[["s1"]], x[["s2"]], x[["r"]])
}

#' @export
cdf.dist_burr <- function(x, q, ...){
  require_package("actuar")
  actuar::pburr(q, x[["s1"]], x[["s2"]], x[["r"]])
}

#' @export
generate.dist_burr <- function(x, times, ...){
  require_package("actuar")
  actuar::rburr(times, x[["s1"]], x[["s2"]], x[["r"]])
}

#' @export
mean.dist_burr <- function(x, ...){
  require_package("actuar")
  actuar::mburr(1, x[["s1"]], x[["s2"]], x[["r"]])
}

#' @export
covariance.dist_burr <- function(x, ...){
  require_package("actuar")
  m1 <- actuar::mburr(1, x[["s1"]], x[["s2"]], x[["r"]])
  m2 <- actuar::mburr(2, x[["s1"]], x[["s2"]], x[["r"]])
  -m1^2 + m2
}


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/R/dist_categorical.R ---
#' The Categorical distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' Categorical distributions are used to represent events with multiple
#' outcomes, such as what number appears on the roll of a dice. This is also
#' referred to as the 'generalised Bernoulli' or 'multinoulli' distribution.
#' The Cateogorical distribution is a special case of the [Multinomial()]
#' distribution with `n = 1`.
#'
#' @param prob A list of probabilities of observing each outcome category.
#' @param outcomes The list of vectors where each value represents each outcome.
#' @details
#'
#'   We recommend reading this documentation on
#'   <https://pkg.mitchelloharawild.com/distributional/>, where the math
#'   will render nicely.
#'
#'   In the following, let \eqn{X} be a Categorical random variable with
#'   probability parameters `p` = \eqn{\{p_1, p_2, \ldots, p_k\}}.
#'
#'   The Categorical probability distribution is widely used to model the
#'   occurance of multiple events. A simple example is the roll of a dice, where
#'   \eqn{p = \{1/6, 1/6, 1/6, 1/6, 1/6, 1/6\}} giving equal chance of observing
#'   each number on a 6 sided dice.
#'
#'   **Support**: \eqn{\{1, \ldots, k\}}{{1, ..., k}}
#'
#'   **Mean**: \eqn{p}
#'
#'   **Variance**: \eqn{p \cdot (1 - p) = p \cdot q}{p (1 - p)}
#'
#'   **Probability mass function (p.m.f)**:
#'
#'   \deqn{
#'     P(X = i) = p_i
#'   }{
#'     P(X = i) = p_i
#'   }
#'
#'   **Cumulative distribution function (c.d.f)**:
#'
#'   The cdf() of a categorical distribution is undefined as the outcome categories aren't ordered.
#'
#' @examples
#' dist <- dist_categorical(prob = list(c(0.05, 0.5, 0.15, 0.2, 0.1), c(0.3, 0.1, 0.6)))
#'
#' dist
#'
#' generate(dist, 10)
#'
#' density(dist, 2)
#' density(dist, 2, log = TRUE)
#'
#' # The outcomes aren't ordered, so many statistics are not applicable.
#' cdf(dist, 0.6)
#' quantile(dist, 0.7)
#' mean(dist)
#' variance(dist)
#' skewness(dist)
#' kurtosis(dist)
#' 
#' # Some of these statistics are meaningful for ordered outcomes
#' dist <- dist_categorical(list(rpois(26, 3)), list(ordered(letters)))
#' dist
#' cdf(dist, "m")
#' quantile(dist, 0.5)
#'
#' dist <- dist_categorical(
#'   prob = list(c(0.05, 0.5, 0.15, 0.2, 0.1), c(0.3, 0.1, 0.6)),
#'   outcomes = list(letters[1:5], letters[24:26])
#' )
#'
#' generate(dist, 10)
#'
#' density(dist, "a")
#' density(dist, "z", log = TRUE)
#'
#' @export
dist_categorical <- function(prob, outcomes = NULL){
  prob <- lapply(prob, function(x) x/sum(x))
  prob <- as_list_of(prob, .ptype = double())
  if(is.null(outcomes)) {
    new_dist(p = prob, class = "dist_categorical")
  } else {
    new_dist(p = prob, x = outcomes, class = "dist_categorical")
  }
}

#' @export
format.dist_categorical <- function(x, digits = 2, ...){
  sprintf(
    "Categorical[%s]",
    format(length(x[["p"]]), digits = digits, ...)
  )
}

#' @export
density.dist_categorical <- function(x, at, ...){
  if(!is.null(x[["x"]])) at <- match(at, x[["x"]])
  at[at <= 0] <- NA_real_
  x[["p"]][at]
}

#' @export
quantile.dist_categorical <- function(x, p, ...){
  if(is.ordered(x[["x"]])) {
    return(x[["x"]][findInterval(p, cumsum(x[["p"]]))])
  }
  rep_len(NA_real_, length(p))
}

#' @export
cdf.dist_categorical <- function(x, q, ...){
  if(is.ordered(x[["x"]])) {
    return(cumsum(x[["p"]])[match(q, x[["x"]])])
  }
  rep_len(NA_real_, length(q))
}

#' @export
generate.dist_categorical <- function(x, times, ...){
  z <- sample(
    x = seq_along(x[["p"]]), size = times, prob = x[["p"]], replace = TRUE
  )
  if(is.null(x[["x"]])) return(z)
  x[["x"]][z]
}

#' @export
support.dist_categorical <- function(x, ...) {
  region <- if(is.null(x[["p"]])) seq_along(x[["p"]]) else x[["x"]]
  new_support_region(
    list(vctrs::vec_init(region, n = 0L)),
    list(region),
    list(c(TRUE, TRUE))
  )
}

#' @export
mean.dist_categorical <- function(x, ...){
  NA_real_
}

#' @export
covariance.dist_categorical <- function(x, ...){
  NA_real_
}

#' @export
skewness.dist_categorical <- function(x, ...) {
  NA_real_
}

#' @export
kurtosis.dist_categorical <- function(x, ...) {
  NA_real_
}


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/R/dist_cauchy.R ---
#' The Cauchy distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' The Cauchy distribution is the student's t distribution with one degree of
#' freedom. The Cauchy distribution does not have a well defined mean or
#' variance. Cauchy distributions often appear as priors in Bayesian contexts
#' due to their heavy tails.
#'
#' @inheritParams stats::dcauchy
#'
#' @details
#'
#'   We recommend reading this documentation on
#'   <https://pkg.mitchelloharawild.com/distributional/>, where the math
#'   will render nicely.
#'
#'   In the following, let \eqn{X} be a Cauchy variable with mean
#'   `location =` \eqn{x_0} and `scale` = \eqn{\gamma}.
#'
#'   **Support**: \eqn{R}, the set of all real numbers
#'
#'   **Mean**: Undefined.
#'
#'   **Variance**: Undefined.
#'
#'   **Probability density function (p.d.f)**:
#'
#'   \deqn{
#'     f(x) = \frac{1}{\pi \gamma \left[1 + \left(\frac{x - x_0}{\gamma} \right)^2 \right]}
#'   }{
#'     f(x) = 1 / (\pi \gamma (1 + ((x - x_0) / \gamma)^2)
#'   }
#'
#'   **Cumulative distribution function (c.d.f)**:
#'
#'   \deqn{
#'     F(t) = \frac{1}{\pi} \arctan \left( \frac{t - x_0}{\gamma} \right) +
#'       \frac{1}{2}
#'   }{
#'     F(t) = arctan((t - x_0) / \gamma) / \pi + 1/2
#'   }
#'
#'   **Moment generating function (m.g.f)**:
#'
#'   Does not exist.
#'
#' @seealso [stats::Cauchy]
#'
#' @examples
#' dist <- dist_cauchy(location = c(0, 0, 0, -2), scale = c(0.5, 1, 2, 1))
#'
#' dist
#' mean(dist)
#' variance(dist)
#' skewness(dist)
#' kurtosis(dist)
#'
#' generate(dist, 10)
#'
#' density(dist, 2)
#' density(dist, 2, log = TRUE)
#'
#' cdf(dist, 4)
#'
#' quantile(dist, 0.7)
#'
#' @name dist_cauchy
#' @export
dist_cauchy <- function(location, scale){
  location <- vec_cast(location, double())
  scale <- vec_cast(scale, double())
  if(any(scale[!is.na(scale)] <= 0)){
    abort("The scale parameter of a Cauchy distribution must strictly positive.")
  }
  new_dist(location = location, scale = scale, class = "dist_cauchy")
}

#' @export
format.dist_cauchy <- function(x, digits = 2, ...){
  sprintf(
    "Cauchy(%s, %s)",
    format(x[["location"]], digits = digits, ...),
    format(x[["scale"]], digits = digits, ...)
  )
}

#' @export
density.dist_cauchy <- function(x, at, ...){
  stats::dcauchy(at, x[["location"]], x[["scale"]])
}

#' @export
log_density.dist_cauchy <- function(x, at, ...){
  stats::dcauchy(at, x[["location"]], x[["scale"]], log = TRUE)
}

#' @export
quantile.dist_cauchy <- function(x, p, ...){
  stats::qcauchy(p, x[["location"]], x[["scale"]])
}

#' @export
cdf.dist_cauchy <- function(x, q, ...){
  stats::pcauchy(q, x[["location"]], x[["scale"]])
}

#' @export
generate.dist_cauchy <- function(x, times, ...){
  stats::rcauchy(times, x[["location"]], x[["scale"]])
}

#' @export
mean.dist_cauchy <- function(x, ...){
  NA_real_
}

#' @export
covariance.dist_cauchy <- function(x, ...){
  NA_real_
}

#' @export
skewness.dist_cauchy <- function(x, ...){
  NA_real_
}

#' @export
kurtosis.dist_cauchy <- function(x, ...){
  NA_real_
}


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/R/dist_chisq.R ---
#' The (non-central) Chi-Squared Distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' Chi-square distributions show up often in frequentist settings
#' as the sampling distribution of test statistics, especially
#' in maximum likelihood estimation settings.
#'
#' @inheritParams stats::dchisq
#'
#'
#' @details
#'
#'   We recommend reading this documentation on
#'   <https://pkg.mitchelloharawild.com/distributional/>, where the math
#'   will render nicely.
#'
#'   In the following, let \eqn{X} be a \eqn{\chi^2} random variable with
#'   `df` = \eqn{k}.
#'
#'   **Support**: \eqn{R^+}, the set of positive real numbers
#'
#'   **Mean**: \eqn{k}
#'
#'   **Variance**: \eqn{2k}
#'
#'   **Probability density function (p.d.f)**:
#'
#'   \deqn{
#'     f(x) = \frac{1}{\sqrt{2 \pi \sigma^2}} e^{-(x - \mu)^2 / 2 \sigma^2}
#'   }{
#'     f(x) = 1 / (2 \pi \sigma^2) exp(-(x - \mu)^2 / (2 \sigma^2))
#'   }
#'
#'   **Cumulative distribution function (c.d.f)**:
#'
#'   The cumulative distribution function has the form
#'
#'   \deqn{
#'     F(t) = \int_{-\infty}^t \frac{1}{\sqrt{2 \pi \sigma^2}} e^{-(x - \mu)^2 / 2 \sigma^2} dx
#'   }{
#'     F(t) = integral_{-\infty}^t 1 / (2 \pi \sigma^2) exp(-(x - \mu)^2 / (2 \sigma^2)) dx
#'   }
#'
#'   but this integral does not have a closed form solution and must be
#'   approximated numerically. The c.d.f. of a standard normal is sometimes
#'   called the "error function". The notation \eqn{\Phi(t)} also stands
#'   for the c.d.f. of a standard normal evaluated at \eqn{t}. Z-tables
#'   list the value of \eqn{\Phi(t)} for various \eqn{t}.
#'
#'   **Moment generating function (m.g.f)**:
#'
#'   \deqn{
#'     E(e^{tX}) = e^{\mu t + \sigma^2 t^2 / 2}
#'   }{
#'     E(e^(tX)) = e^(\mu t + \sigma^2 t^2 / 2)
#'   }
#'
#'
#' @seealso [stats::Chisquare]
#'
#' @examples
#' dist <- dist_chisq(df = c(1,2,3,4,6,9))
#'
#' dist
#' mean(dist)
#' variance(dist)
#' skewness(dist)
#' kurtosis(dist)
#'
#' generate(dist, 10)
#'
#' density(dist, 2)
#' density(dist, 2, log = TRUE)
#'
#' cdf(dist, 4)
#'
#' quantile(dist, 0.7)
#'
#' @name dist_chisq
#' @export
dist_chisq <- function(df, ncp = 0){
  df <- vec_cast(df, double())
  ncp <- vec_cast(ncp, double())
  if(any(df < 0)){
    abort("The degrees of freedom parameter of a Chi-Squared distribution must be non-negative.")
  }
  new_dist(df = df, ncp = ncp, class = "dist_chisq")
}

#' @export
format.dist_chisq <- function(x, digits = 2, ...){
  sprintf(
    if (is_utf8_output()) "\u1d6a\u00b2(%s)" else "x2(%s)",
    format(x[["df"]], digits = digits, ...)
  )
}

#' @export
density.dist_chisq <- function(x, at, ...){
  stats::dchisq(at, x[["df"]], x[["ncp"]])
}

#' @export
log_density.dist_chisq <- function(x, at, ...){
  stats::dchisq(at, x[["df"]], x[["ncp"]], log = TRUE)
}

#' @export
quantile.dist_chisq <- function(x, p, ...){
  stats::qchisq(p, x[["df"]], x[["ncp"]])
}

#' @export
cdf.dist_chisq <- function(x, q, ...){
  stats::pchisq(q, x[["df"]], x[["ncp"]])
}

#' @export
generate.dist_chisq <- function(x, times, ...){
  stats::rchisq(times, x[["df"]], x[["ncp"]])
}

#' @export
mean.dist_chisq <- function(x, ...){
  x[["df"]] + x[["ncp"]]
}

#' @export
covariance.dist_chisq <- function(x, ...){
  2*(x[["df"]] + 2*x[["ncp"]])
}

#' @export
skewness.dist_chisq <- function(x, ...) sqrt(8 / x[["df"]])

#' @export
kurtosis.dist_chisq <- function(x, ...) 12 / x[["df"]]


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/R/dist_degenerate.R ---
#' The degenerate distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' The degenerate distribution takes a single value which is certain to be
#' observed. It takes a single parameter, which is the value that is observed
#' by the distribution.
#'
#' @param x The value of the distribution.
#'
#' @details
#'
#'   We recommend reading this documentation on
#'   <https://pkg.mitchelloharawild.com/distributional/>, where the math
#'   will render nicely.
#'
#'   In the following, let \eqn{X} be a degenerate random variable with value
#'   `x` = \eqn{k_0}.
#'
#'   **Support**: \eqn{R}, the set of all real numbers
#'
#'   **Mean**: \eqn{k_0}
#'
#'   **Variance**: \eqn{0}
#'
#'   **Probability density function (p.d.f)**:
#'
#'   \deqn{
#'     f(x) = 1 for x = k_0
#'   }{
#'     f(x) = 1 for x = k_0
#'   }
#'   \deqn{
#'     f(x) = 0 for x \neq k_0
#'   }{
#'     f(x) = 0 for x \neq k_0
#'   }
#'
#'   **Cumulative distribution function (c.d.f)**:
#'
#'   The cumulative distribution function has the form
#'
#'   \deqn{
#'     F(x) = 0 for x < k_0
#'   }{
#'     F(x) = 0 for x < k_0
#'   }
#'   \deqn{
#'     F(x) = 1 for x \ge k_0
#'   }{
#'     F(x) = 1 for x \ge k_0
#'   }
#'
#'   **Moment generating function (m.g.f)**:
#'
#'   \deqn{
#'     E(e^{tX}) = e^{k_0 t}
#'   }{
#'     E(e^(tX)) = e^(k_0 t)
#'   }
#'
#' @examples
#' dist_degenerate(x = 1:5)
#'
#' @export
dist_degenerate <- function(x){
  vec_is(x, numeric())
  new_dist(x = x, class = "dist_degenerate")
}

#' @export
format.dist_degenerate <- function(x, ...){
  format(x[["x"]], ...)
}

#' @export
density.dist_degenerate <- function(x, at, ...){
  ifelse(at == x[["x"]], 1, 0)
}

#' @export
quantile.dist_degenerate <- function(x, p, ...){
  ifelse(p < 0 | p > 1, NaN, x[["x"]])
}

#' @export
cdf.dist_degenerate <- function(x, q, ...){
  ifelse(q >= x[["x"]], 1, 0)
}

#' @export
generate.dist_degenerate <- function(x, times, ...){
  rep(x[["x"]], times)
}

#' @export
mean.dist_degenerate <- function(x, ...){
  x[["x"]]
}

#' @export
covariance.dist_degenerate <- function(x, ...){
  0
}

#' @export
skewness.dist_degenerate <- function(x, ...) NA_real_

#' @export
kurtosis.dist_degenerate <- function(x, ...) NA_real_


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/R/dist_exponential.R ---
#' The Exponential Distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' @inheritParams stats::dexp
#'
#' @seealso [stats::Exponential]
#'
#' @examples
#' dist <- dist_exponential(rate = c(2, 1, 2/3))
#'
#' dist
#' mean(dist)
#' variance(dist)
#' skewness(dist)
#' kurtosis(dist)
#'
#' generate(dist, 10)
#'
#' density(dist, 2)
#' density(dist, 2, log = TRUE)
#'
#' cdf(dist, 4)
#'
#' quantile(dist, 0.7)
#'
#' @name dist_exponential
#' @export
dist_exponential <- function(rate){
  rate <- vec_cast(rate, double())
  if(any(rate < 0)){
    abort("The rate parameter of an Exponential distribution must be non-negative.")
  }
  new_dist(rate = rate, class = "dist_exponential")
}

#' @export
format.dist_exponential <- function(x, digits = 2, ...){
  sprintf(
    "Exp(%s)",
    format(x[["rate"]], digits = digits, ...)
  )
}

#' @export
density.dist_exponential <- function(x, at, ...){
  stats::dexp(at, x[["rate"]])
}

#' @export
log_density.dist_exponential <- function(x, at, ...){
  stats::dexp(at, x[["rate"]], log = TRUE)
}

#' @export
quantile.dist_exponential <- function(x, p, ...){
  stats::qexp(p, x[["rate"]])
}

#' @export
cdf.dist_exponential <- function(x, q, ...){
  stats::pexp(q, x[["rate"]])
}

#' @export
generate.dist_exponential <- function(x, times, ...){
  stats::rexp(times, x[["rate"]])
}

#' @export
mean.dist_exponential <- function(x, ...){
  1/x[["rate"]]
}

#' @export
covariance.dist_exponential <- function(x, ...){
  1/x[["rate"]]^2
}

#' @export
skewness.dist_exponential <- function(x, ...) 2

#' @export
kurtosis.dist_exponential <- function(x, ...) 6


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/R/dist_f.R ---
#' The F Distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' @inheritParams stats::df
#'
#' @details
#'
#'   We recommend reading this documentation on
#'   <https://pkg.mitchelloharawild.com/distributional/>, where the math
#'   will render nicely.
#'
#'   In the following, let \eqn{X} be a Gamma random variable
#'   with parameters
#'   `shape` = \eqn{\alpha} and
#'   `rate` = \eqn{\beta}.
#'
#'   **Support**: \eqn{x \in (0, \infty)}
#'
#'   **Mean**: \eqn{\frac{\alpha}{\beta}}
#'
#'   **Variance**: \eqn{\frac{\alpha}{\beta^2}}
#'
#'   **Probability density function (p.m.f)**:
#'
#'   \deqn{
#'     f(x) = \frac{\beta^{\alpha}}{\Gamma(\alpha)} x^{\alpha - 1} e^{-\beta x}
#'   }{
#'     f(x) = \frac{\beta^{\alpha}}{\Gamma(\alpha)} x^{\alpha - 1} e^{-\beta x}
#'   }
#'
#'   **Cumulative distribution function (c.d.f)**:
#'
#'   \deqn{
#'     f(x) = \frac{\Gamma(\alpha, \beta x)}{\Gamma{\alpha}}
#'   }{
#'     f(x) = \frac{\Gamma(\alpha, \beta x)}{\Gamma{\alpha}}
#'   }
#'
#'   **Moment generating function (m.g.f)**:
#'
#'   \deqn{
#'     E(e^{tX}) = \Big(\frac{\beta}{ \beta - t}\Big)^{\alpha}, \thinspace t < \beta
#'   }{
#'     E(e^(tX)) = \Big(\frac{\beta}{ \beta - t}\Big)^{\alpha}, \thinspace t < \beta
#'   }
#'
#' @seealso [stats::FDist]
#'
#' @examples
#' dist <- dist_f(df1 = c(1,2,5,10,100), df2 = c(1,1,2,1,100))
#'
#' dist
#' mean(dist)
#' variance(dist)
#' skewness(dist)
#' kurtosis(dist)
#'
#' generate(dist, 10)
#'
#' density(dist, 2)
#' density(dist, 2, log = TRUE)
#'
#' cdf(dist, 4)
#'
#' quantile(dist, 0.7)
#'
#' @name dist_f
#' @export
dist_f <- function(df1, df2, ncp = NULL){
  df1 <- vec_cast(df1, double())
  df2 <- vec_cast(df2, double())
  ncp <- vec_cast(ncp, double())
  if(any((df1 < 0) | (df2 < 0))){
    abort("The degrees of freedom parameters of the F distribution must be non-negative.")
  }
  if(is.null(ncp)){
    new_dist(df1 = df1, df2 = df2, class = "dist_f")
  } else {
    new_dist(df1 = df1, df2 = df2, ncp = ncp, class = "dist_f")
  }
}

#' @export
format.dist_f <- function(x, digits = 2, ...){
  sprintf(
    "F(%s, %s)",
    format(x[["df1"]], digits = digits, ...),
    format(x[["df2"]], digits = digits, ...)
  )
}

#' @export
density.dist_f <- function(x, at, ...){
  if(is.null(x[["ncp"]])) {
    stats::df(at, x[["df1"]], x[["df2"]])
  } else {
    stats::df(at, x[["df1"]], x[["df2"]], x[["ncp"]])
  }
}

#' @export
log_density.dist_f <- function(x, at, ...){
  if(is.null(x[["ncp"]])) {
    stats::df(at, x[["df1"]], x[["df2"]], log = TRUE)
  } else {
    stats::df(at, x[["df1"]], x[["df2"]], x[["ncp"]], log = TRUE)
  }
}

#' @export
quantile.dist_f <- function(x, p, ...){
  if(is.null(x[["ncp"]])) {
    stats::qf(p, x[["df1"]], x[["df2"]])
  } else {
    stats::qf(p, x[["df1"]], x[["df2"]], x[["ncp"]])
  }
}

#' @export
cdf.dist_f <- function(x, q, ...){
  if(is.null(x[["ncp"]])) {
    stats::pf(q, x[["df1"]], x[["df2"]])
  } else {
    stats::pf(q, x[["df1"]], x[["df2"]], x[["ncp"]])
  }
}

#' @export
generate.dist_f <- function(x, times, ...){
  if(is.null(x[["ncp"]])) {
    stats::rf(times, x[["df1"]], x[["df2"]])
  } else {
    stats::rf(times, x[["df1"]], x[["df2"]], x[["ncp"]])
  }
}

#' @export
mean.dist_f <- function(x, ...){
  df1 <- x[["df1"]]
  df2 <- x[["df2"]]
  if(df2 > 2) {
    if(is.null(x[["ncp"]])){
      df2 / (df2 - 2)
    } else {
      (df2 * (df1 + x[["ncp"]])) / (df1 * (df2 - 2))
    }
  } else {
    NA_real_
  }
}

#' @export
covariance.dist_f <- function(x, ...){
  df1 <- x[["df1"]]
  df2 <- x[["df2"]]
  if(df2 > 4) {
    if(is.null(x[["ncp"]])){
      (2 * df2^2 * (df1 + df2 - 2))/(df1*(df2-2)^2*(df2-4))
    } else {
      2*((df1 + x[["ncp"]])^2 + (df1 + 2*x[["ncp"]])*(df2 - 2))/((df2-2)^2*(df2-4)) * (df2^2/df1^2)
    }
  } else {
    NA_real_
  }
}

#' @export
skewness.dist_f <- function(x, ...) {
  df1 <- x[["df1"]]
  df2 <- x[["df2"]]
  if(!is.null(x[["ncp"]])) return(NA_real_)
  if (df2 > 6) {
    a <- (2 * df1 + df2 - 2) * sqrt(8 * (df2 - 4))
    b <- (df2 - 6) * sqrt(df1 * (df1 + df2 - 2))
    a / b
  } else {
    NA_real_
  }
}

#' @export
kurtosis.dist_f <- function(x, ...) {
  df1 <- x[["df1"]]
  df2 <- x[["df2"]]
  if(!is.null(x[["ncp"]])) return(NA_real_)
  if (df2 > 8) {
    a <- df1 * (5 * df2 - 22) * (df1 + df2 - 2) + (df2 - 4) * (df2 - 2)^2
    b <- df1 * (df2 - 6) * (df2 - 8) * (df1 + df2 - 2)
    12 * a / b
  } else {
    NA_real_
  }
}


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/R/dist_gamma.R ---
#' The Gamma distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' Several important distributions are special cases of the Gamma
#' distribution. When the shape parameter is `1`, the Gamma is an
#' exponential distribution with parameter \eqn{1/\beta}. When the
#' \eqn{shape = n/2} and \eqn{rate = 1/2}, the Gamma is a equivalent to
#' a chi squared distribution with n degrees of freedom. Moreover, if
#' we have \eqn{X_1} is \eqn{Gamma(\alpha_1, \beta)} and
#' \eqn{X_2} is \eqn{Gamma(\alpha_2, \beta)}, a function of these two variables
#' of the form \eqn{\frac{X_1}{X_1 + X_2}} \eqn{Beta(\alpha_1, \alpha_2)}.
#' This last property frequently appears in another distributions, and it
#' has extensively been used in multivariate methods. More about the Gamma
#' distribution will be added soon.
#'
#' @inheritParams stats::dgamma
#'
#' @details
#'
#'   We recommend reading this documentation on
#'   <https://pkg.mitchelloharawild.com/distributional/>, where the math
#'   will render nicely.
#'
#'   In the following, let \eqn{X} be a Gamma random variable
#'   with parameters
#'   `shape` = \eqn{\alpha} and
#'   `rate` = \eqn{\beta}.
#'
#'   **Support**: \eqn{x \in (0, \infty)}
#'
#'   **Mean**: \eqn{\frac{\alpha}{\beta}}
#'
#'   **Variance**: \eqn{\frac{\alpha}{\beta^2}}
#'
#'   **Probability density function (p.m.f)**:
#'
#'   \deqn{
#'     f(x) = \frac{\beta^{\alpha}}{\Gamma(\alpha)} x^{\alpha - 1} e^{-\beta x}
#'   }{
#'     f(x) = \frac{\beta^{\alpha}}{\Gamma(\alpha)} x^{\alpha - 1} e^{-\beta x}
#'   }
#'
#'   **Cumulative distribution function (c.d.f)**:
#'
#'   \deqn{
#'     f(x) = \frac{\Gamma(\alpha, \beta x)}{\Gamma{\alpha}}
#'   }{
#'     f(x) = \frac{\Gamma(\alpha, \beta x)}{\Gamma{\alpha}}
#'   }
#'
#'   **Moment generating function (m.g.f)**:
#'
#'   \deqn{
#'     E(e^{tX}) = \Big(\frac{\beta}{ \beta - t}\Big)^{\alpha}, \thinspace t < \beta
#'   }{
#'     E(e^(tX)) = \Big(\frac{\beta}{ \beta - t}\Big)^{\alpha}, \thinspace t < \beta
#'   }
#'
#' @seealso [stats::GammaDist]
#'
#' @examples
#' dist <- dist_gamma(shape = c(1,2,3,5,9,7.5,0.5), rate = c(0.5,0.5,0.5,1,2,1,1))
#'
#' dist
#' mean(dist)
#' variance(dist)
#' skewness(dist)
#' kurtosis(dist)
#'
#' generate(dist, 10)
#'
#' density(dist, 2)
#' density(dist, 2, log = TRUE)
#'
#' cdf(dist, 4)
#'
#' quantile(dist, 0.7)
#'
#' @name dist_gamma
#' @export
dist_gamma <- function(shape, rate = 1/scale, scale = 1/rate){
  shape <- vec_cast(shape, double())
  if(missing(rate) && missing(scale)) {
    abort("Either the `rate` or `scale` must be specified for Gamma distributions.")
  }
  rate <- vec_cast(rate, double())
  if(any(shape[!is.na(shape)] < 0)){
    abort("The shape parameter of a Gamma distribution must be non-negative.")
  }
  if(any(rate[!is.na(rate)] <= 0)){
    abort("The rate parameter of a Gamma distribution must be strictly positive.")
  }
  new_dist(shape = shape, rate = 1/scale, class = "dist_gamma")
}

#' @export
format.dist_gamma <- function(x, digits = 2, ...){
  sprintf(
    if (is_utf8_output()) "\u0393(%s, %s)" else "Gamma(%s, %s)",
    format(x[["shape"]], digits = digits, ...),
    format(x[["rate"]], digits = digits, ...)
  )
}

#' @export
density.dist_gamma <- function(x, at, ...){
  stats::dgamma(at, x[["shape"]], x[["rate"]])
}

#' @export
log_density.dist_gamma <- function(x, at, ...){
  stats::dgamma(at, x[["shape"]], x[["rate"]], log = TRUE)
}

#' @export
quantile.dist_gamma <- function(x, p, ...){
  stats::qgamma(p, x[["shape"]], x[["rate"]])
}

#' @export
cdf.dist_gamma <- function(x, q, ...){
  stats::pgamma(q, x[["shape"]], x[["rate"]])
}

#' @export
generate.dist_gamma <- function(x, times, ...){
  stats::rgamma(times, x[["shape"]], x[["rate"]])
}

#' @export
mean.dist_gamma <- function(x, ...){
  x[["shape"]] / x[["rate"]]
}

#' @export
covariance.dist_gamma <- function(x, ...){
  x[["shape"]] / x[["rate"]]^2
}

#' @export
skewness.dist_gamma <- function(x, ...) 2 / sqrt(x[["shape"]])

#' @export
kurtosis.dist_gamma <- function(x, ...) 6 / x[["shape"]]


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/R/dist_geometric.R ---
#' The Geometric Distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' The Geometric distribution can be thought of as a generalization
#' of the [dist_bernoulli()] distribution where we ask: "if I keep flipping a
#' coin with probability `p` of heads, what is the probability I need
#' \eqn{k} flips before I get my first heads?" The Geometric
#' distribution is a special case of Negative Binomial distribution.
#'
#' @inheritParams stats::dgeom
#'
#' @details
#'
#'   We recommend reading this documentation on
#'   <https://pkg.mitchelloharawild.com/distributional/>, where the math
#'   will render nicely.
#'
#'   In the following, let \eqn{X} be a Geometric random variable with
#'   success probability `p` = \eqn{p}. Note that there are multiple
#'   parameterizations of the Geometric distribution.
#'
#'   **Support**: 0 < p < 1, \eqn{x = 0, 1, \dots}
#'
#'   **Mean**: \eqn{\frac{1-p}{p}}
#'
#'   **Variance**: \eqn{\frac{1-p}{p^2}}
#'
#'   **Probability mass function (p.m.f)**:
#'
#'   \deqn{
#'     P(X = x) = p(1-p)^x,
#'    }
#'
#'   **Cumulative distribution function (c.d.f)**:
#'
#'   \deqn{
#'     P(X \le x) = 1 - (1-p)^{x+1}
#'   }
#'
#'   **Moment generating function (m.g.f)**:
#'
#'   \deqn{
#'     E(e^{tX}) = \frac{pe^t}{1 - (1-p)e^t}
#'   }{
#'     E(e^{tX}) = \frac{pe^t}{1 - (1-p)e^t}
#'   }
#'
#' @seealso [stats::Geometric]
#'
#' @examples
#' dist <- dist_geometric(prob = c(0.2, 0.5, 0.8))
#'
#' dist
#' mean(dist)
#' variance(dist)
#' skewness(dist)
#' kurtosis(dist)
#'
#' generate(dist, 10)
#'
#' density(dist, 2)
#' density(dist, 2, log = TRUE)
#'
#' cdf(dist, 4)
#'
#' quantile(dist, 0.7)
#' @name dist_geometric
#' @export
dist_geometric <- function(prob){
  prob <- vec_cast(prob, double())
  if(any((prob < 0) | (prob > 1))){
    abort("The prob parameter of an Geometric distribution must be between 0 and 1.")
  }
  new_dist(p = prob, class = "dist_geometric")
}

#' @export
format.dist_geometric <- function(x, digits = 2, ...){
  sprintf(
    "Geometric(%s)",
    format(x[["p"]], digits = digits, ...)
  )
}

#' @export
density.dist_geometric <- function(x, at, ...){
  stats::dgeom(at, x[["p"]])
}

#' @export
log_density.dist_geometric <- function(x, at, ...){
  stats::dgeom(at, x[["p"]], log = TRUE)
}

#' @export
quantile.dist_geometric <- function(x, p, ...){
  stats::qgeom(p, x[["p"]])
}

#' @export
cdf.dist_geometric <- function(x, q, ...){
  stats::pgeom(q, x[["p"]])
}

#' @export
generate.dist_geometric <- function(x, times, ...){
  stats::rgeom(times, x[["p"]])
}

#' @export
mean.dist_geometric <- function(x, ...){
  1/x[["p"]] - 1
}

#' @export
covariance.dist_geometric <- function(x, ...){
  (1 - x[["p"]])/x[["p"]]^2
}

#' @export
skewness.dist_geometric <- function(x, ...) (2 - x[["p"]]) / sqrt(1 - x[["p"]])

#' @export
kurtosis.dist_geometric <- function(x, ...) 6 + (x[["p"]]^2 / (1 - x[["p"]]))


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/R/dist_gev.R ---
#' The Generalized Extreme Value Distribution
#'
#' The GEV distribution function with parameters \eqn{\code{location} = a},
#' \eqn{\code{scale} = b} and \eqn{\code{shape} = s} is
#'
#' \deqn{F(x) = \exp\left[-\{1+s(x-a)/b\}^{-1/s}\right]}
#'
#' for \eqn{1+s(x-a)/b > 0}, where \eqn{b > 0}. If \eqn{s = 0} the distribution
#' is defined by continuity, giving
#'
#' \deqn{F(x) = \exp\left[-\exp\left(-\frac{x-a}{b}\right)\right]}
#'
#' The support of the distribution is the real line if \eqn{s = 0},
#' \eqn{x \geq a - b/s} if \eqn{s \neq 0}, and
#' \eqn{x \leq a - b/s} if \eqn{s < 0}.
#'
#' The parametric form of the GEV encompasses that of the Gumbel, Frechet and
#' reverse Weibull distributions, which are obtained for \eqn{s = 0},
#' \eqn{s > 0} and \eqn{s < 0} respectively. It was first introduced by
#' Jenkinson (1955).
#'
#' @references Jenkinson, A. F. (1955) The frequency distribution of the annual
#' maximum (or minimum) of meteorological elements. \emph{Quart. J. R. Met. Soc.},
#' \bold{81}, 158–171.
#' @param location the location parameter \eqn{a} of the GEV distribution.
#' @param scale the scale parameter \eqn{b} of the GEV distribution.
#' @param shape the shape parameter \eqn{s} of the GEV distribution.
#' @seealso \code{\link[evd]{gev}}
#' @examples
#' dist <- dist_gev(location = 0, scale = 1, shape = 0)
#' @export

dist_gev <- function(location, scale, shape) {
  location <- vctrs::vec_cast(unname(location), double())
  shape <- vctrs::vec_cast(unname(shape), double())
  scale <- vctrs::vec_cast(unname(scale), double())
  if (any(scale <= 0)) {
    stop("The scale parameter of a GEV distribution must be strictly positive")
  }
  distributional::new_dist(location = location, scale = scale, shape = shape, class = "dist_gev")
}

#' @export
format.dist_gev <- function(x, digits = 2, ...) {
  sprintf(
    "GEV(%s, %s, %s)",
    format(x[["location"]], digits = digits, ...),
    format(x[["scale"]], digits = digits, ...),
    format(x[["shape"]], digits = digits, ...)
  )
}

#' @exportS3Method distributional::log_density
log_density.dist_gev <- function(x, at, ...) {
  z <- (at - x[["location"]]) / x[["scale"]]
  if (x[["shape"]] == 0) {
    pdf <- -z - exp(-z)
  } else {
    xx <- 1 + x[["shape"]] * z
    xx[xx <= 0] <- NA_real_
    pdf <- -xx^(-1 / x[["shape"]]) - (1 / x[["shape"]] + 1) * log(xx)
    pdf[is.na(pdf)] <- -Inf
  }
  pdf - log(x[["scale"]])
}

#' @exportS3Method stats::density
density.dist_gev <- function(x, at, ...) {
  exp(log_density.dist_gev(x, at, ...))
}

#' @exportS3Method distributional::cdf
cdf.dist_gev <- function(x, q, ...) {
  z <- (q - x[["location"]]) / x[["scale"]]
  if (x[["shape"]] == 0) {
    exp(-exp(-z))
  } else {
    exp(-pmax(1 + x[["shape"]] * z, 0)^(-1 / x[["shape"]]))
  }
}

#' @exportS3Method stats::quantile
quantile.dist_gev <- function(x, p, ...) {
  if (x[["shape"]] == 0) {
    x[["location"]] - x[["scale"]] * log(-log(p))
  } else {
    x[["location"]] + x[["scale"]] * ((-log(p))^(-x[["shape"]]) - 1) / x[["shape"]]
  }
}

#' @exportS3Method distributional::generate
generate.dist_gev <- function(x, times, ...) {
  z <- stats::rexp(times)
  if (x[["shape"]] == 0) {
    x[["location"]] - x[["scale"]] * log(z)
  } else {
    x[["location"]] + x[["scale"]] * (z^(-x[["shape"]]) - 1) / x[["shape"]]
  }
}

#' @export
mean.dist_gev <- function(x, ...) {
  if (x[["shape"]] == 0) {
    x[["location"]] + x[["scale"]] * 0.57721566490153286
  } else if (x[["shape"]] < 1) {
    x[["location"]] + x[["scale"]] * (gamma(1 - x[["shape"]]) - 1) / x[["shape"]]
  } else {
    Inf
  }
}

#' @exportS3Method stats::median
median.dist_gev <- function(x, ...) {
  if (x[["shape"]] == 0) {
    x[["location"]] - x[["scale"]] * log(log(2))
  } else {
    x[["location"]] + x[["scale"]] * (log(2)^(-x[["shape"]]) - 1) / x[["shape"]]
  }
}

#' @exportS3Method distributional::covariance
covariance.dist_gev <- function(x, ...) {
  if (x[["shape"]] == 0) {
    x[["scale"]]^2 * pi^2 / 6
  } else if (x[["shape"]] < 0.5) {
    g2 <- gamma(1 - 2 * x[["shape"]])
    g1 <- gamma(1 - x[["shape"]])
    x[["scale"]]^2 * (g2 - g1^2) / x[["shape"]]^2
  } else {
    Inf
  }
}


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/R/dist_gh.R ---
#' The generalised g-and-h Distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' The generalised g-and-h distribution is a flexible distribution used to model univariate data, similar to the g-k distribution.
#' It is known for its ability to handle skewness and heavy-tailed behavior.
#'
#' @inheritParams gk::dgh
#'
#' @details
#'
#'   We recommend reading this documentation on
#'   <https://pkg.mitchelloharawild.com/distributional/>, where the math
#'   will render nicely.
#'
#'   In the following, let \eqn{X} be a g-and-h random variable with parameters
#'   `A`, `B`, `g`, `h`, and `c`.
#'
#'   **Support**: \eqn{(-\infty, \infty)}
#'
#'   **Mean**: Not available in closed form.
#'
#'   **Variance**: Not available in closed form.
#'
#'   **Probability density function (p.d.f)**:
#'
#'   The g-and-h distribution does not have a closed-form expression for its density. Instead,
#'   it is defined through its quantile function:
#'
#'   \deqn{
#'     Q(u) = A + B \left( 1 + c \frac{1 - \exp(-gz(u))}{1 + \exp(-gz(u))} \right) \exp(h z(u)^2/2) z(u)
#'   }{
#'     Q(u) = A + B * (1 + c * ((1 - exp(-g * z(u))) / (1 + exp(-g * z(u))))) * exp(h * z(u)^2/2) * z(u)
#'   }
#'
#'   where \eqn{z(u) = \Phi^{-1}(u)}
#'
#'   **Cumulative distribution function (c.d.f)**:
#'
#'   The cumulative distribution function is typically evaluated numerically due to the lack
#'   of a closed-form expression.
#'
#' @seealso [gk::dgh], [distributional::dist_gk]
#'
#' @examples
#' dist <- dist_gh(A = 0, B = 1, g = 0, h = 0.5)
#' dist
#'
#' @examplesIf requireNamespace("gk", quietly = TRUE)
#' mean(dist)
#' variance(dist)
#' support(dist)
#' generate(dist, 10)
#'
#' density(dist, 2)
#' density(dist, 2, log = TRUE)
#'
#' cdf(dist, 4)
#'
#' quantile(dist, 0.7)
#'
#' @name dist_gh
#' @export
dist_gh <- function(A, B, g, h, c = 0.8){
  A <- vec_cast(A, double())
  B <- vec_cast(B, double())
  g <- vec_cast(g, double())
  h <- vec_cast(h, double())
  c <- vec_cast(c, double())
  if(any(B <= 0)){
    abort("The B parameter (scale) of the g-and-h distribution must be strictly positive.")
  }
  new_dist(A = A, B = B, g = g, h = h, c = c, class = "dist_gh")
}

#' @export
format.dist_gh <- function(x, digits = 2, ...){
  sprintf(
    "gh(A = %s, B = %s, g = %s, h = %s%s)",
    format(x[["A"]], digits = digits, ...),
    format(x[["B"]], digits = digits, ...),
    format(x[["g"]], digits = digits, ...),
    format(x[["h"]], digits = digits, ...),
    if (x[["c"]]==0.8) "" else paste0(", c = ", format(x[["c"]], digits = digits, ...))
  )
}

#' @export
density.dist_gh <- function(x, at, ...){
  require_package("gk")
  gk::dgh(at, x[["A"]], x[["B"]], x[["g"]], x[["h"]], x[["c"]])
}

#' @export
log_density.dist_gh <- function(x, at, ...){
  require_package("gk")
  gk::dgh(at, x[["A"]], x[["B"]], x[["g"]], x[["h"]], x[["c"]], log = TRUE)
}

#' @export
quantile.dist_gh <- function(x, p, ...){
  require_package("gk")
  gk::qgh(p, x[["A"]], x[["B"]], x[["g"]], x[["h"]], x[["c"]])
}

#' @export
cdf.dist_gh <- function(x, q, ...){
  require_package("gk")
  gk::pgh(q, x[["A"]], x[["B"]], x[["g"]], x[["h"]], x[["c"]])
}

#' @export
generate.dist_gh <- function(x, times, ...){
  require_package("gk")
  gk::rgh(times, x[["A"]], x[["B"]], x[["g"]], x[["h"]], x[["c"]])
}


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/R/dist_gk.R ---
#' The g-and-k Distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' The g-and-k distribution is a flexible distribution often used to model univariate data.
#' It is particularly known for its ability to handle skewness and heavy-tailed behavior.
#'
#' @inheritParams gk::dgk
#'
#' @details
#'
#'   We recommend reading this documentation on
#'   <https://pkg.mitchelloharawild.com/distributional/>, where the math
#'   will render nicely.
#'
#'   In the following, let \eqn{X} be a g-k random variable with parameters
#'   `A`, `B`, `g`, `k`, and `c`.
#'
#'   **Support**: \eqn{(-\infty, \infty)}
#'
#'   **Mean**: Not available in closed form.
#'
#'   **Variance**: Not available in closed form.
#'
#'   **Probability density function (p.d.f)**:
#'
#'   The g-k distribution does not have a closed-form expression for its density. Instead,
#'   it is defined through its quantile function:
#'
#'   \deqn{
#'     Q(u) = A + B \left( 1 + c \frac{1 - \exp(-gz(u))}{1 + \exp(-gz(u))} \right) (1 + z(u)^2)^k z(u)
#'   }{
#'     Q(u) = A + B * (1 + c * ((1 - exp(-g * z(u))) / (1 + exp(-g * z(u))))) * (1 + z(u)^2)^k * z(u)
#'   }
#'
#'   where \eqn{z(u) = \Phi^{-1}(u)}, the standard normal quantile of u.
#'
#'   **Cumulative distribution function (c.d.f)**:
#'
#'   The cumulative distribution function is typically evaluated numerically due to the lack
#'   of a closed-form expression.
#'
#' @seealso [gk::dgk], [distributional::dist_gh]
#'
#' @examples
#' dist <- dist_gk(A = 0, B = 1, g = 0, k = 0.5)
#' dist
#'
#' @examplesIf requireNamespace("gk", quietly = TRUE)
#' mean(dist)
#' variance(dist)
#' support(dist)
#' generate(dist, 10)
#'
#' density(dist, 2)
#' density(dist, 2, log = TRUE)
#'
#' cdf(dist, 4)
#'
#' quantile(dist, 0.7)
#'
#' @name dist_gk
#' @export
dist_gk <- function(A, B, g, k, c = 0.8){
  A <- vec_cast(A, double())
  B <- vec_cast(B, double())
  g <- vec_cast(g, double())
  k <- vec_cast(k, double())
  c <- vec_cast(c, double())
  if(any(B <= 0)){
    abort("The B parameter (scale) of the gk distribution must be strictly positive.")
  }
  new_dist(A = A, B = B, g = g, k = k, c = c, class = "dist_gk")
}

#' @export
format.dist_gk <- function(x, digits = 2, ...){
  sprintf(
    "gk(A = %s, B = %s, g = %s, k = %s%s)",
    format(x[["A"]], digits = digits, ...),
    format(x[["B"]], digits = digits, ...),
    format(x[["g"]], digits = digits, ...),
    format(x[["k"]], digits = digits, ...),
    if (x[["c"]]==0.8) "" else paste0(", c = ", format(x[["c"]], digits = digits, ...))
  )
}

#' @export
density.dist_gk <- function(x, at, ...){
  require_package("gk")
  gk::dgk(at, x[["A"]], x[["B"]], x[["g"]], x[["k"]], x[["c"]])
}

#' @export
log_density.dist_gk <- function(x, at, ...){
  require_package("gk")
  gk::dgk(at, x[["A"]], x[["B"]], x[["g"]], x[["k"]], x[["c"]], log = TRUE)
}

#' @export
quantile.dist_gk <- function(x, p, ...){
  require_package("gk")
  gk::qgk(p, x[["A"]], x[["B"]], x[["g"]], x[["k"]], x[["c"]])
}

#' @export
cdf.dist_gk <- function(x, q, ...){
  require_package("gk")
  gk::pgk(q, x[["A"]], x[["B"]], x[["g"]], x[["k"]], x[["c"]])
}

#' @export
generate.dist_gk <- function(x, times, ...){
  require_package("gk")
  gk::rgk(times, x[["A"]], x[["B"]], x[["g"]], x[["k"]], x[["c"]])
}


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/R/dist_gpd.R ---
#' The Generalized Pareto Distribution
#'
#' The GPD distribution function with parameters \eqn{\code{location} = a},
#' \eqn{\code{scale} = b} and \eqn{\code{shape} = s} is
#'
#' \deqn{F(x) = 1 - \left(1+s(x-a)/b\right)^{-1/s}}
#'
#' for \eqn{1+s(x-a)/b > 0}, where \eqn{b > 0}. If \eqn{s = 0} the distribution
#' is defined by continuity, giving
#'
#' \deqn{F(x) = 1 - \exp\left(-\frac{x-a}{b}\right)}
#'
#' The support of the distribution is \eqn{x \geq a} if \eqn{s \geq 0}, and
#' \eqn{a \leq x \leq a -b/s} if \eqn{s < 0}.
#'
#' The Pickands–Balkema–De Haan theorem states that for a large class of
#' distributions, the tail (above some threshold) can be approximated by a GPD.
#'
#' @param location the location parameter \eqn{a} of the GPD distribution.
#' @param scale the scale parameter \eqn{b} of the GPD distribution.
#' @param shape the shape parameter \eqn{s} of the GPD distribution.
#' @seealso \code{\link[evd]{gpd}}
#' @examples
#' dist <- dist_gpd(location = 0, scale = 1, shape = 0)
#' @export

dist_gpd <- function(location, scale, shape) {
  location <- vctrs::vec_cast(unname(location), double())
  shape <- vctrs::vec_cast(unname(shape), double())
  scale <- vctrs::vec_cast(unname(scale), double())
  if (any(scale <= 0)) {
    stop("The scale parameter of a GPD distribution must be strictly positive")
  }
  distributional::new_dist(location = location, scale = scale, shape = shape, class = "dist_gpd")
}

#' @export
format.dist_gpd <- function(x, digits = 2, ...) {
  sprintf(
    "GPD(%s, %s, %s)",
    format(x[["location"]], digits = digits, ...),
    format(x[["scale"]], digits = digits, ...),
    format(x[["shape"]], digits = digits, ...)
  )
}

#' @exportS3Method distributional::log_density
log_density.dist_gpd <- function(x, at, ...) {
  z <- (at - x[["location"]]) / x[["scale"]]
  if (x[["shape"]] == 0) {
    pdf <- -z
  } else {
    xx <- 1 + x[["shape"]] * z
    xx[xx <= 0] <- NA_real_
    pdf <- -(1 / x[["shape"]] + 1) * log(xx)
    pdf[is.na(pdf)] <- -Inf
  }
  if (x[["shape"]] >= 0) {
    pdf[z < 0] <- -Inf
  } else {
    pdf[z < 0 | z > -1 / x[["shape"]]] <- -Inf
  }
  pdf - log(x[["scale"]])
}

#' @exportS3Method stats::density
density.dist_gpd <- function(x, at, ...) {
  exp(log_density.dist_gpd(x, at, ...))
}

#' @exportS3Method distributional::cdf
cdf.dist_gpd <- function(x, q, ...) {
  z <- pmax(q - x[["location"]], 0) / x[["scale"]]
  if (x[["shape"]] == 0) {
    1 - exp(-z)
  } else {
    1 - pmax(1 + x[["shape"]] * z, 0)^(-1 / x[["shape"]])
  }
}

#' @exportS3Method stats::quantile
quantile.dist_gpd <- function(x, p, ...) {
  if (x[["shape"]] == 0) {
    x[["location"]] - x[["scale"]] * log(1 - p)
  } else {
    x[["location"]] + x[["scale"]] * ((1 - p)^(-x[["shape"]]) - 1) / x[["shape"]]
  }
}

#' @exportS3Method distributional::generate
generate.dist_gpd <- function(x, times, ...) {
  if (x[["shape"]] == 0) {
    x[["location"]] + x[["scale"]] * stats::rexp(times)
  } else {
    quantile(x, stats::runif(times))
  }
}

#' @export
mean.dist_gpd <- function(x, ...) {
  if (x[["shape"]] < 1) {
    x[["location"]] + x[["scale"]] / (1 - x[["shape"]])
  } else {
    Inf
  }
}

#' @exportS3Method stats::median
median.dist_gpd <- function(x, ...) {
  if (x[["shape"]] == 0) {
    x[["location"]] - x[["scale"]] * log(0.5)
  } else {
    x[["location"]] + x[["scale"]] * (2^x[["shape"]] - 1) / x[["shape"]]
  }
}

#' @exportS3Method distributional::covariance
covariance.dist_gpd <- function(x, ...) {
  if (x[["shape"]] < 0.5) {
    x[["scale"]]^2 / (1 - x[["shape"]])^2 / (1 - 2 * x[["shape"]])
  } else {
    Inf
  }
}


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/R/dist_gumbel.R ---
#' The Gumbel distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' The Gumbel distribution is a special case of the Generalized Extreme Value
#' distribution, obtained when the GEV shape parameter \eqn{\xi} is equal to 0.
#' It may be referred to as a type I extreme value distribution.
#'
#' @inheritParams actuar::dgumbel
#'
#' @details
#'
#'   We recommend reading this documentation on
#'   <https://pkg.mitchelloharawild.com/distributional/>, where the math
#'   will render nicely.
#'
#'   In the following, let \eqn{X} be a Gumbel random variable with location
#'   parameter  `mu` = \eqn{\mu}, scale parameter `sigma` = \eqn{\sigma}.
#'
#'   **Support**: \eqn{R}, the set of all real numbers.
#'
#'   **Mean**: \eqn{\mu + \sigma\gamma}, where \eqn{\gamma} is Euler's
#'   constant, approximately equal to 0.57722.
#'
#'   **Median**: \eqn{\mu - \sigma\ln(\ln 2)}{\mu - \sigma ln(ln 2)}.
#'
#'   **Variance**: \eqn{\sigma^2 \pi^2 / 6}.
#'
#'   **Probability density function (p.d.f)**:
#'
#'   \deqn{f(x) = \sigma ^ {-1} \exp[-(x - \mu) / \sigma]%
#'         \exp\{-\exp[-(x - \mu) / \sigma] \}}{%
#'        f(x) = (1 / \sigma) exp[-(x - \mu) / \sigma]%
#'         exp{-exp[-(x - \mu) / \sigma]}}
#'   for \eqn{x} in \eqn{R}, the set of all real numbers.
#'
#'   **Cumulative distribution function (c.d.f)**:
#'
#'   In the \eqn{\xi = 0} (Gumbel) special case
#'   \deqn{F(x) = \exp\{-\exp[-(x - \mu) / \sigma] \}}{%
#'         F(x) = exp{ - exp[-(x - \mu) / \sigma]} }
#'   for \eqn{x} in \eqn{R}, the set of all real numbers.
#'
#' @seealso [actuar::Gumbel]
#'
#' @examples
#' dist <- dist_gumbel(alpha = c(0.5, 1, 1.5, 3), scale = c(2, 2, 3, 4))
#' dist
#'
#' @examplesIf requireNamespace("actuar", quietly = TRUE)
#' mean(dist)
#' variance(dist)
#' skewness(dist)
#' kurtosis(dist)
#' support(dist)
#' generate(dist, 10)
#'
#' density(dist, 2)
#' density(dist, 2, log = TRUE)
#'
#' cdf(dist, 4)
#'
#' quantile(dist, 0.7)
#'
#' @name dist_gumbel
#' @export
dist_gumbel <- function(alpha, scale){
  alpha <- vec_cast(alpha, double())
  scale <- vec_cast(scale, double())
  if(any(scale <= 0)){
    abort("The scale parameter of a Gumbel distribution must be strictly positive.")
  }
  new_dist(a = alpha, s = scale, class = "dist_gumbel")
}

#' @export
format.dist_gumbel <- function(x, digits = 2, ...){
  sprintf(
    "Gumbel(%s, %s)",
    format(x[["a"]], digits = digits, ...),
    format(x[["s"]], digits = digits, ...)
  )
}

#' @export
density.dist_gumbel <- function(x, at, ...){
  require_package("actuar")
  actuar::dgumbel(at, x[["a"]], x[["s"]])
}

#' @export
log_density.dist_gumbel <- function(x, at, ...){
  require_package("actuar")
  actuar::dgumbel(at, x[["a"]], x[["s"]], log = TRUE)
}

#' @export
quantile.dist_gumbel <- function(x, p, ...){
  require_package("actuar")
  actuar::qgumbel(p, x[["a"]], x[["s"]])
}

#' @export
cdf.dist_gumbel <- function(x, q, ...){
  require_package("actuar")
  actuar::pgumbel(q, x[["a"]], x[["s"]])
}

#' @export
generate.dist_gumbel <- function(x, times, ...){
  require_package("actuar")
  actuar::rgumbel(times, x[["a"]], x[["s"]])
}

#' @export
mean.dist_gumbel <- function(x, ...){
  actuar::mgumbel(1, x[["a"]], x[["s"]])
}

#' @export
covariance.dist_gumbel <- function(x, ...){
  (pi*x[["s"]])^2/6
}

#' @export
skewness.dist_gumbel <- function(x, ...) {
  zeta3 <- 1.20205690315959401459612
  (12 * sqrt(6) * zeta3) / pi^3
}

#' @export
kurtosis.dist_gumbel <- function(x, ...) 12/5


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/R/dist_hypergeometric.R ---
#' The Hypergeometric distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' To understand the HyperGeometric distribution, consider a set of
#' \eqn{r} objects, of which \eqn{m} are of the type I and
#' \eqn{n} are of the type II. A sample with size \eqn{k} (\eqn{k<r})
#' with no replacement is randomly chosen. The number of observed
#' type I elements observed in this sample is set to be our random
#' variable \eqn{X}.
#'
#' @param m The number of type I elements available.
#' @param n The number of type II elements available.
#' @param k The size of the sample taken.
#'
#'
#' @details
#'
#'   We recommend reading this documentation on
#'   <https://pkg.mitchelloharawild.com/distributional/>, where the math
#'   will render nicely.
#'
#'   In the following, let \eqn{X} be a HyperGeometric random variable with
#'   success probability `p` = \eqn{p = m/(m+n)}.
#'
#'   **Support**: \eqn{x \in { \{\max{(0, k-n)}, \dots, \min{(k,m)}}\}}
#'
#'   **Mean**: \eqn{\frac{km}{n+m} = kp}
#'
#'   **Variance**: \eqn{\frac{km(n)(n+m-k)}{(n+m)^2 (n+m-1)} =
#'   kp(1-p)(1 - \frac{k-1}{m+n-1})}
#'
#'   **Probability mass function (p.m.f)**:
#'
#'   \deqn{
#'     P(X = x) = \frac{{m \choose x}{n \choose k-x}}{{m+n \choose k}}
#'   }{
#'     P(X = x) = \frac{{m \choose x}{n \choose k-x}}{{m+n \choose k}}
#'   }
#'
#'   **Cumulative distribution function (c.d.f)**:
#'
#'   \deqn{
#'     P(X \le k) \approx \Phi\Big(\frac{x - kp}{\sqrt{kp(1-p)}}\Big)
#'  }
#'
#' @seealso [stats::Hypergeometric]
#'
#' @examples
#' dist <- dist_hypergeometric(m = rep(500, 3), n = c(50, 60, 70), k = c(100, 200, 300))
#'
#' dist
#' mean(dist)
#' variance(dist)
#' skewness(dist)
#' kurtosis(dist)
#'
#' generate(dist, 10)
#'
#' density(dist, 2)
#' density(dist, 2, log = TRUE)
#'
#' cdf(dist, 4)
#'
#' quantile(dist, 0.7)
#'
#' @name dist_hypergeometric
#' @export
dist_hypergeometric <- function(m, n, k){
  m <- vec_cast(m, integer())
  n <- vec_cast(n, integer())
  k <- vec_cast(k, integer())
  new_dist(m = m, n = n, k = k, class = "dist_hypergeometric")
}

#' @export
format.dist_hypergeometric <- function(x, digits = 2, ...){
  sprintf(
    "Hypergeometric(%s, %s, %s)",
    format(x[["m"]], digits = digits, ...),
    format(x[["n"]], digits = digits, ...),
    format(x[["k"]], digits = digits, ...)
  )
}

#' @export
density.dist_hypergeometric <- function(x, at, ...){
  stats::dhyper(at, x[["m"]], x[["n"]], x[["k"]])
}

#' @export
log_density.dist_hypergeometric <- function(x, at, ...){
  stats::dhyper(at, x[["m"]], x[["n"]], x[["k"]], log = TRUE)
}

#' @export
quantile.dist_hypergeometric <- function(x, p, ...){
  stats::qhyper(p, x[["m"]], x[["n"]], x[["k"]])
}

#' @export
cdf.dist_hypergeometric <- function(x, q, ...){
  stats::phyper(q, x[["m"]], x[["n"]], x[["k"]])
}

#' @export
generate.dist_hypergeometric <- function(x, times, ...){
  stats::rhyper(times, x[["m"]], x[["n"]], x[["k"]])
}

#' @export
mean.dist_hypergeometric <- function(x, ...){
  p <- x[["m"]]/(x[["m"]] + x[["n"]])
  x[["k"]] * p
}

#' @export
covariance.dist_hypergeometric <- function(x, ...){
  m <- x[["m"]]
  n <- x[["n"]]
  k <- x[["k"]]
  p <- m/(m + n)
  k * p * (1 - p) * ((m + n - k) / (m + n - 1))
}

#' @export
skewness.dist_hypergeometric <- function(x, ...) {
  N <- x[["n"]] + x[["m"]]
  K <- x[["m"]]
  n <- x[["k"]]

  a <- (N - 2 * K) * (N - 1)^0.5 * (N - 2 * n)
  b <- (n * K * (N - K) * (N - n))^0.5 * (N - 2)
  a / b
}

#' @export
kurtosis.dist_hypergeometric <- function(x, ...) {
  N <- x[["n"]] + x[["m"]]
  K <- x[["m"]]
  n <- x[["k"]]

  1 / (n * K * (N - K) * (N - n) * (N - 2) * (N - 3))
}


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/R/dist_inverse_exponential.R ---
#' The Inverse Exponential distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' @inheritParams actuar::dinvexp
#'
#' @seealso [actuar::InverseExponential]
#'
#' @examples
#' dist <- dist_inverse_exponential(rate = 1:5)
#' dist
#'
#' @examplesIf requireNamespace("actuar", quietly = TRUE)
#' mean(dist)
#' variance(dist)
#' support(dist)
#' generate(dist, 10)
#'
#' density(dist, 2)
#' density(dist, 2, log = TRUE)
#'
#' cdf(dist, 4)
#'
#' quantile(dist, 0.7)
#'
#' @name dist_inverse_exponential
#' @export
dist_inverse_exponential <- function(rate){
  rate <- vec_cast(rate, double())
  if(any(rate <= 0)){
    abort("The rate parameter of a Inverse Exponential distribution must be strictly positive.")
  }
  new_dist(r = rate, class = "dist_inverse_exponential")
}

#' @export
format.dist_inverse_exponential <- function(x, digits = 2, ...){
  sprintf(
    "InvExp(%s)",
    format(x[["r"]], digits = digits, ...)
  )
}

#' @export
density.dist_inverse_exponential <- function(x, at, ...){
  require_package("actuar")
  actuar::dinvexp(at, x[["r"]])
}

#' @export
log_density.dist_inverse_exponential <- function(x, at, ...){
  require_package("actuar")
  actuar::dinvexp(at, x[["r"]], log = TRUE)
}

#' @export
quantile.dist_inverse_exponential <- function(x, p, ...){
  require_package("actuar")
  actuar::qinvexp(p, x[["r"]])
}

#' @export
cdf.dist_inverse_exponential <- function(x, q, ...){
  require_package("actuar")
  actuar::pinvexp(q, x[["r"]])
}

#' @export
generate.dist_inverse_exponential <- function(x, times, ...){
  require_package("actuar")
  actuar::rinvexp(times, x[["r"]])
}

#' @export
mean.dist_inverse_exponential <- function(x, ...){
  NA_real_
}

#' @export
covariance.dist_inverse_exponential <- function(x, ...){
  NA_real_
}


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/R/dist_inverse_gamma.R ---
#' The Inverse Gamma distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' @inheritParams actuar::dinvgamma
#'
#' @seealso [actuar::InverseGamma]
#'
#' @examples
#' dist <- dist_inverse_gamma(shape = c(1,2,3,3), rate = c(1,1,1,2))
#' dist
#'
#' @examplesIf requireNamespace("actuar", quietly = TRUE)
#' mean(dist)
#' variance(dist)
#' support(dist)
#' generate(dist, 10)
#'
#' density(dist, 2)
#' density(dist, 2, log = TRUE)
#'
#' cdf(dist, 4)
#'
#' quantile(dist, 0.7)
#'
#' @name dist_inverse_gamma
#' @export
dist_inverse_gamma <- function(shape, rate = 1/scale, scale){
  shape <- vec_cast(shape, double())
  rate <- vec_cast(rate, double())
  if(any(shape <= 0)){
    abort("The shape parameter of a Inverse Gamma distribution must be strictly positive.")
  }
  if(any(rate <= 0)){
    abort("The rate/scale parameter of a Inverse Gamma distribution must be strictly positive.")
  }
  new_dist(s = shape, r = rate, class = "dist_inverse_gamma")
}

#' @export
format.dist_inverse_gamma <- function(x, digits = 2, ...){
  sprintf(
    "InvGamma(%s, %s)",
    format(x[["s"]], digits = digits, ...),
    format(1/x[["r"]], digits = digits, ...)
  )
}

#' @export
density.dist_inverse_gamma <- function(x, at, ...){
  require_package("actuar")
  actuar::dinvgamma(at, x[["s"]], x[["r"]])
}

#' @export
log_density.dist_inverse_gamma <- function(x, at, ...){
  require_package("actuar")
  actuar::dinvgamma(at, x[["s"]], x[["r"]], log = TRUE)
}

#' @export
quantile.dist_inverse_gamma <- function(x, p, ...){
  require_package("actuar")
  actuar::qinvgamma(p, x[["s"]], x[["r"]])
}

#' @export
cdf.dist_inverse_gamma <- function(x, q, ...){
  require_package("actuar")
  actuar::pinvgamma(q, x[["s"]], x[["r"]])
}

#' @export
generate.dist_inverse_gamma <- function(x, times, ...){
  require_package("actuar")
  actuar::rinvgamma(times, x[["s"]], x[["r"]])
}

#' @export
mean.dist_inverse_gamma <- function(x, ...){
  if(x[["s"]] <= 1) return(NA_real_)
  1/(x[["r"]]*(x[["s"]]-1))
}

#' @export
covariance.dist_inverse_gamma <- function(x, ...){
  if(x[["s"]] <= 2) return(NA_real_)
  1/(x[["r"]]^2*(x[["s"]]-1)^2*(x[["s"]]-2))
}


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/R/dist_inverse_gaussian.R ---
#' The Inverse Gaussian distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' @inheritParams actuar::dinvgauss
#'
#' @seealso [actuar::InverseGaussian]
#'
#' @examples
#' dist <- dist_inverse_gaussian(mean = c(1,1,1,3,3), shape = c(0.2, 1, 3, 0.2, 1))
#' dist
#'
#' @examplesIf requireNamespace("actuar", quietly = TRUE)
#' mean(dist)
#' variance(dist)
#' support(dist)
#' generate(dist, 10)
#'
#' density(dist, 2)
#' density(dist, 2, log = TRUE)
#'
#' cdf(dist, 4)
#'
#' quantile(dist, 0.7)
#'
#' @name dist_inverse_gaussian
#' @export
dist_inverse_gaussian <- function(mean, shape){
  mean <- vec_cast(mean, double())
  shape <- vec_cast(shape, double())
  if(any(mean[!is.na(mean)] <= 0)){
    abort("The mean parameter of a Inverse Gaussian distribution must be strictly positive.")
  }
  if(any(shape[!is.na(shape)] <= 0)){
    abort("The shape parameter of a Inverse Gaussian distribution must be strictly positive.")
  }
  new_dist(m = mean, s = shape, class = "dist_inverse_gaussian")
}

#' @export
format.dist_inverse_gaussian <- function(x, digits = 2, ...){
  sprintf(
    "IG(%s, %s)",
    format(x[["m"]], digits = digits, ...),
    format(x[["s"]], digits = digits, ...)
  )
}

#' @export
density.dist_inverse_gaussian <- function(x, at, ...){
  require_package("actuar")
  actuar::dinvgauss(at, x[["m"]], x[["s"]])
}

#' @export
log_density.dist_inverse_gaussian <- function(x, at, ...){
  require_package("actuar")
  actuar::dinvgauss(at, x[["m"]], x[["s"]], log = TRUE)
}

#' @export
quantile.dist_inverse_gaussian <- function(x, p, ...){
  require_package("actuar")
  actuar::qinvgauss(p, x[["m"]], x[["s"]])
}

#' @export
cdf.dist_inverse_gaussian <- function(x, q, ...){
  require_package("actuar")
  actuar::pinvgauss(q, x[["m"]], x[["s"]])
}

#' @export
generate.dist_inverse_gaussian <- function(x, times, ...){
  require_package("actuar")
  actuar::rinvgauss(times, x[["m"]], x[["s"]])
}

#' @export
mean.dist_inverse_gaussian <- function(x, ...){
  x[["m"]]
}

#' @export
covariance.dist_inverse_gaussian <- function(x, ...){
  x[["m"]]^3/x[["s"]]
}


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/R/dist_logarithmic.R ---
#' The Logarithmic distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' @inheritParams actuar::dlogarithmic
#'
#' @seealso [actuar::Logarithmic]
#'
#' @examples
#' dist <- dist_logarithmic(prob = c(0.33, 0.66, 0.99))
#' dist
#'
#' @examplesIf requireNamespace("actuar", quietly = TRUE)
#' mean(dist)
#' variance(dist)
#' support(dist)
#' generate(dist, 10)
#'
#' density(dist, 2)
#' density(dist, 2, log = TRUE)
#'
#' cdf(dist, 4)
#'
#' quantile(dist, 0.7)
#' @name dist_logarithmic
#' @export
dist_logarithmic <- function(prob){
  prob <- vec_cast(prob, double())
  if(any((prob < 0) | (prob > 1))){
    abort("The prob parameter of a Logarithmic distribution must be between 0 and 1.")
  }
  new_dist(p = prob, class = "dist_logarithmic")
}

#' @export
format.dist_logarithmic <- function(x, digits = 2, ...){
  sprintf(
    "Logarithmic(%s)",
    format(x[["p"]], digits = digits, ...)
  )
}

#' @export
density.dist_logarithmic <- function(x, at, ...){
  require_package("actuar")
  actuar::dlogarithmic(at, x[["p"]])
}

#' @export
log_density.dist_logarithmic <- function(x, at, ...){
  require_package("actuar")
  actuar::dlogarithmic(at, x[["p"]], log = TRUE)
}

#' @export
quantile.dist_logarithmic <- function(x, p, ...){
  require_package("actuar")
  actuar::qlogarithmic(p, x[["p"]])
}

#' @export
cdf.dist_logarithmic <- function(x, q, ...){
  require_package("actuar")
  actuar::plogarithmic(q, x[["p"]])
}

#' @export
generate.dist_logarithmic <- function(x, times, ...){
  require_package("actuar")
  actuar::rlogarithmic(times, x[["p"]])
}

#' @export
mean.dist_logarithmic <- function(x, ...){
  p <- x[["p"]]
  (-1/(log(1-p)))*(p/(1-p))
}

#' @export
covariance.dist_logarithmic <- function(x, ...){
  p <- x[["p"]]
  -(p^2 + p*log(1-p))/((1-p)*log(1-p))^2
}


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/R/dist_logistic.R ---
#' The Logistic distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' A continuous distribution on the real line. For binary outcomes
#' the model given by \eqn{P(Y = 1 | X) = F(X \beta)} where
#' \eqn{F} is the Logistic [cdf()] is called *logistic regression*.
#'
#' @inheritParams stats::dlogis
#'
#'
#' @details
#'
#'   We recommend reading this documentation on
#'   <https://pkg.mitchelloharawild.com/distributional/>, where the math
#'   will render nicely.
#'
#'   In the following, let \eqn{X} be a Logistic random variable with
#'   `location` = \eqn{\mu} and `scale` = \eqn{s}.
#'
#'   **Support**: \eqn{R}, the set of all real numbers
#'
#'   **Mean**: \eqn{\mu}
#'
#'   **Variance**: \eqn{s^2 \pi^2 / 3}
#'
#'   **Probability density function (p.d.f)**:
#'
#'   \deqn{
#'     f(x) = \frac{e^{-(\frac{x - \mu}{s})}}{s [1 + \exp(-(\frac{x - \mu}{s})) ]^2}
#'   }{
#'     f(x) = e^(-(t - \mu) / s) / (s (1 + e^(-(t - \mu) / s))^2)
#'   }
#'
#'   **Cumulative distribution function (c.d.f)**:
#'
#'   \deqn{
#'     F(t) = \frac{1}{1 + e^{-(\frac{t - \mu}{s})}}
#'   }{
#'     F(t) = 1 / (1 +  e^(-(t - \mu) / s))
#'   }
#'
#'   **Moment generating function (m.g.f)**:
#'
#'   \deqn{
#'     E(e^{tX}) = e^{\mu t} \beta(1 - st, 1 + st)
#'   }{
#'     E(e^(tX)) = = e^(\mu t) \beta(1 - st, 1 + st)
#'   }
#'
#'   where \eqn{\beta(x, y)} is the Beta function.
#'
#' @seealso [stats::Logistic]
#'
#' @examples
#' dist <- dist_logistic(location = c(5,9,9,6,2), scale = c(2,3,4,2,1))
#'
#' dist
#' mean(dist)
#' variance(dist)
#' skewness(dist)
#' kurtosis(dist)
#'
#' generate(dist, 10)
#'
#' density(dist, 2)
#' density(dist, 2, log = TRUE)
#'
#' cdf(dist, 4)
#'
#' quantile(dist, 0.7)
#'
#' @name dist_logistic
#' @export
dist_logistic <- function(location, scale){
  location <- vec_cast(location, double())
  scale <- vec_cast(scale, double())
  new_dist(l = location, s = scale, class = "dist_logistic")
}

#' @export
format.dist_logistic <- function(x, digits = 2, ...){
  sprintf(
    "Logistic(%s, %s)",
    format(x[["l"]], digits = digits, ...),
    format(x[["s"]], digits = digits, ...)
  )
}

#' @export
density.dist_logistic <- function(x, at, ...){
  stats::dlogis(at, x[["l"]], x[["s"]])
}

#' @export
log_density.dist_logistic <- function(x, at, ...){
  stats::dlogis(at, x[["l"]], x[["s"]], log = TRUE)
}

#' @export
quantile.dist_logistic <- function(x, p, ...){
  stats::qlogis(p, x[["l"]], x[["s"]])
}

#' @export
cdf.dist_logistic <- function(x, q, ...){
  stats::plogis(q, x[["l"]], x[["s"]])
}

#' @export
generate.dist_logistic <- function(x, times, ...){
  stats::rlogis(times, x[["l"]], x[["s"]])
}

#' @export
mean.dist_logistic <- function(x, ...){
  x[["l"]]
}

#' @export
covariance.dist_logistic <- function(x, ...){
  (x[["s"]]*pi)^2/3
}

#' @export
skewness.dist_logistic <- function(x, ...) 0

#' @export
kurtosis.dist_logistic <- function(x, ...) 6 / 5


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/R/dist_lognormal.R ---
#' The log-normal distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' The log-normal distribution is a commonly used transformation of the Normal
#' distribution. If \eqn{X} follows a log-normal distribution, then \eqn{\ln{X}}
#' would be characteristed by a Normal distribution.
#'
#' @param mu The mean (location parameter) of the distribution, which is the
#'   mean of the associated Normal distribution. Can be any real number.
#' @param sigma The standard deviation (scale parameter) of the distribution.
#'   Can be any positive number.
#'
#' @details
#'
#'   We recommend reading this documentation on
#'   <https://pkg.mitchelloharawild.com/distributional/>, where the math
#'   will render nicely.
#'
#'   In the following, let \eqn{Y} be a Normal random variable with mean
#'   `mu` = \eqn{\mu} and standard deviation `sigma` = \eqn{\sigma}. The
#'   log-normal distribution \eqn{X = exp(Y)} is characterised by:
#'
#'   **Support**: \eqn{R+}, the set of all real numbers greater than or equal to 0.
#'
#'   **Mean**: \eqn{e^(\mu + \sigma^2/2}
#'
#'   **Variance**: \eqn{(e^(\sigma^2)-1) e^(2\mu + \sigma^2}
#'
#'   **Probability density function (p.d.f)**:
#'
#'   \deqn{
#'     f(x) = \frac{1}{x\sqrt{2 \pi \sigma^2}} e^{-(\ln{x} - \mu)^2 / 2 \sigma^2}
#'   }{
#'     f(x) = 1 / (x * sqrt(2 \pi \sigma^2)) exp(-(log(x) - \mu)^2 / (2 \sigma^2))
#'   }
#'
#'   **Cumulative distribution function (c.d.f)**:
#'
#'   The cumulative distribution function has the form
#'
#'   \deqn{
#'     F(x) = \Phi((\ln{x} - \mu)/\sigma)
#'   }{
#'     F(x) = Phi((log(x) - \mu)/\sigma)
#'   }
#'
#'   Where \eqn{Phi}{Phi} is the CDF of a standard Normal distribution, N(0,1).
#'
#' @seealso [stats::Lognormal]
#'
#' @examples
#' dist <- dist_lognormal(mu = 1:5, sigma = 0.1)
#'
#' dist
#' mean(dist)
#' variance(dist)
#' skewness(dist)
#' kurtosis(dist)
#'
#' generate(dist, 10)
#'
#' density(dist, 2)
#' density(dist, 2, log = TRUE)
#'
#' cdf(dist, 4)
#'
#' quantile(dist, 0.7)
#'
#' # A log-normal distribution X is exp(Y), where Y is a Normal distribution of
#' # the same parameters. So log(X) will produce the Normal distribution Y.
#' log(dist)
#' @export
dist_lognormal <- function(mu = 0, sigma = 1){
  mu <- vec_cast(mu, double())
  sigma <- vec_cast(sigma, double())
  if(any(sigma[!is.na(sigma)] < 0)){
    abort("Standard deviation of a log-normal distribution must be non-negative")
  }
  new_dist(mu = mu, sigma = sigma, class = "dist_lognormal")
}

#' @export
format.dist_lognormal <- function(x, digits = 2, ...){
  sprintf(
    "lN(%s, %s)",
    format(x[["mu"]], digits = digits, ...),
    format(x[["sigma"]]^2, digits = digits, ...)
  )
}

#' @export
density.dist_lognormal <- function(x, at, ...){
  stats::dlnorm(at, x[["mu"]], x[["sigma"]])
}

#' @export
log_density.dist_lognormal <- function(x, at, ...){
  stats::dlnorm(at, x[["mu"]], x[["sigma"]], log = TRUE)
}

#' @export
quantile.dist_lognormal <- function(x, p, ...){
  stats::qlnorm(p, x[["mu"]], x[["sigma"]])
}
#' @export
log_quantile.dist_lognormal <- function(x, p, ...){
  stats::qlnorm(p, x[["mu"]], x[["sigma"]], log.p = TRUE)
}

#' @export
cdf.dist_lognormal <- function(x, q, ...){
  stats::plnorm(q, x[["mu"]], x[["sigma"]])
}
#' @export
log_cdf.dist_lognormal <- function(x, q, ...){
  stats::plnorm(q, x[["mu"]], x[["sigma"]], log.p = TRUE)
}

#' @export
generate.dist_lognormal <- function(x, times, ...){
  stats::rlnorm(times, x[["mu"]], x[["sigma"]])
}

#' @export
mean.dist_lognormal <- function(x, ...){
  exp(x[["mu"]] + x[["sigma"]]^2/2)
}

#' @export
covariance.dist_lognormal <- function(x, ...){
  s2 <- x[["sigma"]]^2
  (exp(s2)-1)*exp(2*x[["mu"]] + s2)
}

#' @export
skewness.dist_lognormal <- function(x, ...) {
  es2 <- exp(x[["sigma"]]^2)
  (es2+2)*sqrt(es2-1)
}

#' @export
kurtosis.dist_lognormal <- function(x, ...) {
  s2 <- x[["sigma"]]^2
  exp(4*s2) + 2*exp(3*s2) + 3*exp(2*s2) - 6
}

# make a normal distribution from a lognormal distribution using the
# specified base
normal_dist_with_base <- function(x, base = exp(1)) {
  vec_data(dist_normal(x[["mu"]], x[["sigma"]]) / log(base))[[1]]
}

#' @method Math dist_lognormal
#' @export
Math.dist_lognormal <- function(x, ...) {
  switch(.Generic,
    # Shortcuts to get Normal distribution from log-normal.
    log = normal_dist_with_base(x, ...),
    log2 = normal_dist_with_base(x, 2),
    log10 = normal_dist_with_base(x, 10),

    NextMethod()
  )
}


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/R/dist_missing.R ---
#' Missing distribution
#'
#' @description
#' `r lifecycle::badge('maturing')`
#'
#' A placeholder distribution for handling missing values in a vector of
#' distributions.
#'
#' @param length The number of missing distributions
#'
#' @name dist_missing
#'
#' @examples
#' dist <- dist_missing(3L)
#'
#' dist
#' mean(dist)
#' variance(dist)
#'
#' generate(dist, 10)
#'
#' density(dist, 2)
#' density(dist, 2, log = TRUE)
#'
#' cdf(dist, 4)
#'
#' quantile(dist, 0.7)
#'
#' @export
dist_missing <- function(length = 1) {
  vctrs::vec_rep(NA_dist_, length)
}

NA_dist_ <- structure(list(NULL), class = c("distribution", "vctrs_vctr", "list"))

#' @export
format.dist_na <- function(x, ...) {
  "NA"
}

#' @export
density.dist_na <- function(x, at, ...){
  rep_len(NA_real_, length(at))
}

#' @export
log_density.dist_na <- density.dist_na

#' @export
quantile.dist_na <- function(x, p, ...){
  rep_len(NA_real_, length(p))
}
#' @export
log_quantile.dist_na <- quantile.dist_na

#' @export
cdf.dist_na <- function(x, q, ...){
  rep_len(NA_real_, length(q))
}
#' @export
log_cdf.dist_na <- cdf.dist_na

#' @export
generate.dist_na <- function(x, times, ...){
  rep(NA_real_, times)
}

#' @export
mean.dist_na <- function(x, ...) NA_real_

#' @export
covariance.dist_na <- function(x, ...) NA_real_

#' @export
skewness.dist_na <- function(x, ...) NA_real_

#' @export
kurtosis.dist_na <- function(x, ...) NA_real_

#' @export
Math.dist_na <- function(x, ...) {
  x
}

#' @export
Ops.dist_na <- function(e1, e2) {
  dist_missing(max(length(e1), length(e2)))
}


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/R/dist_multinomial.R ---
#' The Multinomial distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' The multinomial distribution is a generalization of the binomial
#' distribution to multiple categories. It is perhaps easiest to think
#' that we first extend a [dist_bernoulli()] distribution to include more
#' than two categories, resulting in a [dist_categorical()] distribution.
#' We then extend repeat the Categorical experiment several (\eqn{n})
#' times.
#'
#' @param size The number of draws from the Categorical distribution.
#' @param prob The probability of an event occurring from each draw.
#'
#' @details
#'
#'   We recommend reading this documentation on
#'   <https://pkg.mitchelloharawild.com/distributional/>, where the math
#'   will render nicely.
#'
#'   In the following, let \eqn{X = (X_1, ..., X_k)} be a Multinomial
#'   random variable with success probability `p` = \eqn{p}. Note that
#'   \eqn{p} is vector with \eqn{k} elements that sum to one. Assume
#'   that we repeat the Categorical experiment `size` = \eqn{n} times.
#'
#'   **Support**: Each \eqn{X_i} is in \eqn{{0, 1, 2, ..., n}}.
#'
#'   **Mean**: The mean of \eqn{X_i} is \eqn{n p_i}.
#'
#'   **Variance**: The variance of \eqn{X_i} is \eqn{n p_i (1 - p_i)}.
#'     For \eqn{i \neq j}, the covariance of \eqn{X_i} and \eqn{X_j}
#'     is \eqn{-n p_i p_j}.
#'
#'   **Probability mass function (p.m.f)**:
#'
#'   \deqn{
#'     P(X_1 = x_1, ..., X_k = x_k) = \frac{n!}{x_1! x_2! ... x_k!} p_1^{x_1} \cdot p_2^{x_2} \cdot ... \cdot p_k^{x_k}
#'   }{
#'     P(X_1 = x_1, ..., X_k = x_k) = n! / (x_1! x_2! ... x_k!) p_1^x_1 p_2^x_2 ... p_k^x_k
#'   }
#'
#'   **Cumulative distribution function (c.d.f)**:
#'
#'   Omitted for multivariate random variables for the time being.
#'
#'   **Moment generating function (m.g.f)**:
#'
#'   \deqn{
#'     E(e^{tX}) = \left(\sum_{i=1}^k p_i e^{t_i}\right)^n
#'   }{
#'     E(e^(tX)) = (p_1 e^t_1 + p_2 e^t_2 + ... + p_k e^t_k)^n
#'   }
#'
#' @seealso [stats::Multinomial]
#'
#' @examples
#' dist <- dist_multinomial(size = c(4, 3), prob = list(c(0.3, 0.5, 0.2), c(0.1, 0.5, 0.4)))
#'
#' dist
#' mean(dist)
#' variance(dist)
#'
#' generate(dist, 10)
#'
#' # TODO: Needs fixing to support multiple inputs
#' # density(dist, 2)
#' # density(dist, 2, log = TRUE)
#'
#' @name dist_multinomial
#' @export
dist_multinomial <- function(size, prob){
  size <- vec_cast(size, double())
  prob <- lapply(prob, function(x) x/sum(x))
  prob <- as_list_of(prob, .ptype = double())
  new_dist(s = size, p = prob, class = "dist_multinomial")
}

#' @export
format.dist_multinomial <- function(x, digits = 2, ...){
  sprintf(
    "Multinomial(%s)[%s]",
    format(x[["s"]], digits = digits, ...),
    format(length(x[["p"]]), digits = digits, ...)
  )
}

#' @export
density.dist_multinomial <- function(x, at, ...){
  if(is.list(at)) return(vapply(at, density, numeric(1L), x = x, ...))
  stats::dmultinom(at, x[["s"]], x[["p"]])
}

#' @export
log_density.dist_multinomial <- function(x, at, ...){
  stats::dmultinom(at, x[["s"]], x[["p"]], log = TRUE)
}

#' @export
generate.dist_multinomial <- function(x, times, ...){
  t(stats::rmultinom(times, x[["s"]], x[["p"]]))
}

#' @export
mean.dist_multinomial <- function(x, ...){
  matrix(x[["s"]]*x[["p"]], nrow = 1)
}

#' @export
covariance.dist_multinomial <- function(x, ...){
  s <- x[["s"]]
  p <- x[["p"]]
  v <- numeric(length(p)^2)
  for(i in seq_along(p)){
    for(j in seq_along(p)){
      v[(i-1)*length(p) + j] <- if(i == j) s*p[i]*(1-p[j]) else -s*p[i]*p[j]
    }
  }
  list(matrix(v, nrow = length(p)))
}

#' @export
dim.dist_multinomial <- function(x){
  length(x[["p"]])
}


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/R/dist_multivariate_normal.R ---
#' The multivariate normal distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' @param mu A list of numeric vectors for the distribution's mean.
#' @param sigma A list of matrices for the distribution's variance-covariance matrix.
#'
#' @seealso [mvtnorm::dmvnorm], [mvtnorm::qmvnorm]
#'
#' @examples
#' dist <- dist_multivariate_normal(mu = list(c(1,2)), sigma = list(matrix(c(4,2,2,3), ncol=2)))
#' dimnames(dist) <- c("x", "y")
#' dist
#'
#' @examplesIf requireNamespace("mvtnorm", quietly = TRUE)
#' mean(dist)
#' variance(dist)
#' support(dist)
#' generate(dist, 10)
#'
#' density(dist, cbind(2, 1))
#' density(dist, cbind(2, 1), log = TRUE)
#'
#' cdf(dist, 4)
#'
#' quantile(dist, 0.7)
#' quantile(dist, 0.7, type = "marginal")
#'
#' @export
dist_multivariate_normal <- function(mu = 0, sigma = diag(1)){
  new_dist(mu = mu, sigma = sigma,
           dimnames = colnames(sigma[[1]]), class = "dist_mvnorm")
}

#' @export
format.dist_mvnorm <- function(x, digits = 2, ...){
  sprintf(
    "MVN[%i]",
    length(x[["mu"]])
  )
}

#' @export
density.dist_mvnorm <- function(x, at, ..., na.rm = FALSE){
  require_package("mvtnorm")
  if(is.list(at)) return(vapply(at, density, numeric(1L), x = x, ...))
  mvtnorm::dmvnorm(at, x[["mu"]], x[["sigma"]])
}

#' @export
log_density.dist_mvnorm <- function(x, at, ..., na.rm = FALSE){
  require_package("mvtnorm")
  if(is.list(at)) return(vapply(at, log_density, numeric(1L), x = x, ...))
  mvtnorm::dmvnorm(at, x[["mu"]], x[["sigma"]], log = TRUE)
}

#' @export
quantile.dist_mvnorm <- function(x, p, type = c("equicoordinate", "marginal"),
                                 ..., na.rm = FALSE){
  type <- match.arg(type)
  q <- if (type == "marginal") {
    stats::qnorm(p, mean = rep(x[["mu"]], each = length(p)),
                 sd = rep(sqrt(diag(x[["sigma"]])), each = length(p)), ...)
  } else {
    require_package("mvtnorm")
    vapply(p, function(p, ...) {
      if (p == 0) return(-Inf) else if (p == 1) return(Inf)
      mvtnorm::qmvnorm(p, ...)$quantile
    }, numeric(1L), mean = x[["mu"]], sigma = x[["sigma"]], ...)
  }

  matrix(q, nrow = length(p), ncol = dim(x))
}

#' @export
cdf.dist_mvnorm <- function(x, q, ..., na.rm = FALSE){
  if(is.list(q)) return(vapply(q, cdf, numeric(1L), x = x, ...))
  require_package("mvtnorm")
  mvtnorm::pmvnorm(upper = as.numeric(q), mean = x[["mu"]], sigma = x[["sigma"]], ...)[1]
}

#' @export
generate.dist_mvnorm <- function(x, times, ..., na.rm = FALSE){
  require_package("mvtnorm")
  mvtnorm::rmvnorm(times, x[["mu"]], x[["sigma"]], ...)
}

#' @export
mean.dist_mvnorm <- function(x, ...){
  matrix(x[["mu"]], nrow = 1)
}

#' @export
covariance.dist_mvnorm <- function(x, ...){
  # Wrap in list to preserve matrix structure
  list(x[["sigma"]])
}

#' @export
dim.dist_mvnorm <- function(x){
  length(x[["mu"]])
}


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/R/dist_negative_binomial.R ---
#' The Negative Binomial distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' A generalization of the geometric distribution. It is the number
#' of failures in a sequence of i.i.d. Bernoulli trials before
#' a specified number of successes (`size`) occur. The probability of success in
#' each trial is given by `prob`.
#'
#' @inheritParams stats::NegBinomial
#'
#' @details
#'
#'   We recommend reading this documentation on
#'   <https://pkg.mitchelloharawild.com/distributional/>, where the math
#'   will render nicely.
#'
#'   In the following, let \eqn{X} be a Negative Binomial random variable with
#'   success probability `prob` = \eqn{p} and the number of successes `size` =
#'   \eqn{r}.
#'
#'
#'   **Support**: \eqn{\{0, 1, 2, 3, ...\}}
#'
#'   **Mean**: \eqn{\frac{p r}{1-p}}
#'
#'   **Variance**: \eqn{\frac{pr}{(1-p)^2}}
#'
#'   **Probability mass function (p.m.f)**:
#'
#'   \deqn{
#'      f(k) = {k + r - 1 \choose k} \cdot (1-p)^r p^k
#'   }{
#'      f(k) = (k+r-1)!/(k!(r-1)!) (1-p)^r p^k
#'   }
#'
#'   **Cumulative distribution function (c.d.f)**:
#'
#'   Too nasty, omitted.
#'
#'   **Moment generating function (m.g.f)**:
#'
#'   \deqn{
#'      \left(\frac{1-p}{1-pe^t}\right)^r, t < -\log p
#'   }{
#'      \frac{(1-p)^r}{(1-pe^t)^r}, t < -\log p
#'   }
#'
#' @seealso [stats::NegBinomial]
#'
#' @examples
#' dist <- dist_negative_binomial(size = 10, prob = 0.5)
#'
#' dist
#' mean(dist)
#' variance(dist)
#' skewness(dist)
#' kurtosis(dist)
#' support(dist)
#'
#' generate(dist, 10)
#'
#' density(dist, 2)
#' density(dist, 2, log = TRUE)
#'
#' cdf(dist, 4)
#'
#' quantile(dist, 0.7)
#'
#' @export
dist_negative_binomial <- function(size, prob){
  size <- vec_cast(size, double())
  prob <- vec_cast(prob, double())
  if(any(prob < 0 | prob > 1)){
    abort("Probability of success must be between 0 and 1.")
  }
  new_dist(n = size, p = prob, class = "dist_negbin")
}

#' @export
format.dist_negbin <- function(x, digits = 2, ...){
  sprintf(
    "NB(%s, %s)",
    format(x[["n"]], digits = digits, ...),
    format(x[["p"]], digits = digits, ...)
  )
}

#' @export
density.dist_negbin <- function(x, at, ...){
  stats::dnbinom(at, x[["n"]], x[["p"]])
}

#' @export
log_density.dist_negbin <- function(x, at, ...){
  stats::dnbinom(at, x[["n"]], x[["p"]], log = TRUE)
}

#' @export
quantile.dist_negbin <- function(x, p, ...){
  stats::qnbinom(p, x[["n"]], x[["p"]])
}

#' @export
cdf.dist_negbin <- function(x, q, ...){
  stats::pnbinom(q, x[["n"]], x[["p"]])
}

#' @export
generate.dist_negbin <- function(x, times, ...){
  stats::rnbinom(times, x[["n"]], x[["p"]])
}

#' @export
mean.dist_negbin <- function(x, ...){
  x[["n"]] * (1 - x[["p"]]) / x[["p"]]
}

#' @export
covariance.dist_negbin <- function(x, ...){
  x[["n"]] * (1 - x[["p"]]) / x[["p"]]^2
}

#' @export
skewness.dist_negbin <- function(x, ...) {
  (1 + x[["p"]]) / sqrt(x[["p"]] * x[["n"]])
}

#' @export
kurtosis.dist_negbin <- function(x, ...) {
  6 / x[["n"]] + (1 - x[["p"]])^2 / x[["n"]] * x[["p"]]
}


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/R/dist_normal.R ---
#' The Normal distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' The Normal distribution is ubiquitous in statistics, partially because
#' of the central limit theorem, which states that sums of i.i.d. random
#' variables eventually become Normal. Linear transformations of Normal
#' random variables result in new random variables that are also Normal. If
#' you are taking an intro stats course, you'll likely use the Normal
#' distribution for Z-tests and in simple linear regression. Under
#' regularity conditions, maximum likelihood estimators are
#' asymptotically Normal. The Normal distribution is also called the
#' gaussian distribution.
#'
#' @param mu,mean The mean (location parameter) of the distribution, which is also
#'   the mean of the distribution. Can be any real number.
#' @param sigma,sd The standard deviation (scale parameter) of the distribution.
#'   Can be any positive number. If you would like a Normal distribution with
#'   **variance** \eqn{\sigma^2}, be sure to take the square root, as this is a
#'   common source of errors.
#'
#' @details
#'
#'   We recommend reading this documentation on
#'   <https://pkg.mitchelloharawild.com/distributional/>, where the math
#'   will render nicely.
#'
#'   In the following, let \eqn{X} be a Normal random variable with mean
#'   `mu` = \eqn{\mu} and standard deviation `sigma` = \eqn{\sigma}.
#'
#'   **Support**: \eqn{R}, the set of all real numbers
#'
#'   **Mean**: \eqn{\mu}
#'
#'   **Variance**: \eqn{\sigma^2}
#'
#'   **Probability density function (p.d.f)**:
#'
#'   \deqn{
#'     f(x) = \frac{1}{\sqrt{2 \pi \sigma^2}} e^{-(x - \mu)^2 / 2 \sigma^2}
#'   }{
#'     f(x) = 1 / sqrt(2 \pi \sigma^2) exp(-(x - \mu)^2 / (2 \sigma^2))
#'   }
#'
#'   **Cumulative distribution function (c.d.f)**:
#'
#'   The cumulative distribution function has the form
#'
#'   \deqn{
#'     F(t) = \int_{-\infty}^t \frac{1}{\sqrt{2 \pi \sigma^2}} e^{-(x - \mu)^2 / 2 \sigma^2} dx
#'   }{
#'     F(t) = integral_{-\infty}^t 1 / sqrt(2 \pi \sigma^2) exp(-(x - \mu)^2 / (2 \sigma^2)) dx
#'   }
#'
#'   but this integral does not have a closed form solution and must be
#'   approximated numerically. The c.d.f. of a standard Normal is sometimes
#'   called the "error function". The notation \eqn{\Phi(t)} also stands
#'   for the c.d.f. of a standard Normal evaluated at \eqn{t}. Z-tables
#'   list the value of \eqn{\Phi(t)} for various \eqn{t}.
#'
#'   **Moment generating function (m.g.f)**:
#'
#'   \deqn{
#'     E(e^{tX}) = e^{\mu t + \sigma^2 t^2 / 2}
#'   }{
#'     E(e^(tX)) = e^(\mu t + \sigma^2 t^2 / 2)
#'   }
#'
#' @seealso [stats::Normal]
#'
#' @examples
#' dist <- dist_normal(mu = 1:5, sigma = 3)
#'
#' dist
#' mean(dist)
#' variance(dist)
#' skewness(dist)
#' kurtosis(dist)
#'
#' generate(dist, 10)
#'
#' density(dist, 2)
#' density(dist, 2, log = TRUE)
#'
#' cdf(dist, 4)
#'
#' quantile(dist, 0.7)
#'
#' @export
dist_normal <- function(mu = 0, sigma = 1, mean = mu, sd = sigma){
  mean <- vec_cast(mean, double())
  sd <- vec_cast(sd, double())
  if(any(sd[!is.na(sd)] < 0)){
    abort("Standard deviation of a normal distribution must be non-negative")
  }
  new_dist(mu = mean, sigma = sd, class = "dist_normal")
}

#' @export
format.dist_normal <- function(x, digits = 2, ...){
  sprintf(
    "N(%s, %s)",
    format(x[["mu"]], digits = digits, ...),
    format(x[["sigma"]]^2, digits = digits, ...)
  )
}

#' @export
density.dist_normal <- function(x, at, ...){
  stats::dnorm(at, x[["mu"]], x[["sigma"]])
}

#' @export
log_density.dist_normal <- function(x, at, ...){
  stats::dnorm(at, x[["mu"]], x[["sigma"]], log = TRUE)
}

#' @export
quantile.dist_normal <- function(x, p, ...){
  stats::qnorm(p, x[["mu"]], x[["sigma"]])
}
#' @export
log_quantile.dist_normal <- function(x, p, ...){
  stats::qnorm(p, x[["mu"]], x[["sigma"]], log.p = TRUE)
}

#' @export
cdf.dist_normal <- function(x, q, ...){
  stats::pnorm(q, x[["mu"]], x[["sigma"]])
}
#' @export
log_cdf.dist_normal <- function(x, q, ...){
  stats::pnorm(q, x[["mu"]], x[["sigma"]], log.p = TRUE)
}

#' @export
generate.dist_normal <- function(x, times, ...){
  stats::rnorm(times, x[["mu"]], x[["sigma"]])
}

#' @export
mean.dist_normal <- function(x, ...){
  x[["mu"]]
}

#' @export
covariance.dist_normal <- function(x, ...){
  x[["sigma"]]^2
}

#' @export
skewness.dist_normal <- function(x, ...) 0

#' @export
kurtosis.dist_normal <- function(x, ...) 0

#' @export
Ops.dist_normal <- function(e1, e2){
  ok <- switch(.Generic, `+` = , `-` = , `*` = , `/` = TRUE, FALSE)
  if (!ok) {
    return(NextMethod())
  }
  if(.Generic == "/" && inherits(e2, "dist_normal")){
    abort(sprintf("Cannot divide by a normal distribution"))
  }
  if(.Generic %in% c("-", "+") && missing(e2)){
    e2 <- e1
    e1 <- if(.Generic == "+") 1 else -1
    .Generic <- "*"
  }
  if(.Generic == "-"){
    .Generic <- "+"
    e2 <- -e2
  }
  else if(.Generic == "/"){
    .Generic <- "*"
    e2 <- 1/e2
  }

  # Ops between two normals
  if(inherits(e1, "dist_normal") && inherits(e2, "dist_normal")){
    if(.Generic == "*"){
      abort(sprintf("Multiplying two normal distributions is not supported."))
    }

    e1$mu <- e1$mu + e2$mu
    e1$sigma <- sqrt(e1$sigma^2 + e2$sigma^2)
    return(e1)
  }

  # Ops between a normal and scalar
  if(inherits(e1, "dist_normal")){
    dist <- e1
    scalar <- e2
  } else {
    dist <- e2
    scalar <- e1
  }
  if(!is.numeric(scalar)){
    abort(sprintf("Cannot %s a `%s` with a normal distribution",
                  switch(.Generic, `+` = "add", `-` = "subtract", `*` = "multiply", `/` = "divide"),
                  class(scalar)))
  }

  if(.Generic == "+"){
    dist$mu <- dist$mu + scalar
  }
  else if(.Generic == "*"){
    dist$mu <- dist$mu * scalar
    dist$sigma <- dist$sigma * abs(scalar)
  }
  dist
}

#' @method Math dist_normal
#' @export
Math.dist_normal <- function(x, ...) {
  # Shortcut to get log-normal distribution from Normal.
  if(.Generic == "exp") return(vec_data(dist_lognormal(x[["mu"]], x[["sigma"]]))[[1]])
  NextMethod()
}


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/R/dist_pareto.R ---
#' The Pareto distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' @inheritParams actuar::dpareto
#'
#' @seealso [actuar::Pareto]
#'
#' @examples
#' dist <- dist_pareto(shape = c(10, 3, 2, 1), scale = rep(1, 4))
#' dist
#'
#' @examplesIf requireNamespace("actuar", quietly = TRUE)
#' mean(dist)
#' variance(dist)
#' support(dist)
#' generate(dist, 10)
#'
#' density(dist, 2)
#' density(dist, 2, log = TRUE)
#'
#' cdf(dist, 4)
#'
#' quantile(dist, 0.7)
#'
#' @name dist_pareto
#' @export
dist_pareto <- function(shape, scale){
  shape <- vec_cast(shape, double())
  scale <- vec_cast(scale, double())
  if(any(shape < 0)){
    abort("The shape parameter of a Pareto distribution must be non-negative.")
  }
  if(any(scale <= 0)){
    abort("The scale parameter of a Pareto distribution must be strictly positive.")
  }
  new_dist(shape = shape, scale = scale, class = "dist_pareto")
}

#' @export
format.dist_pareto <- function(x, digits = 2, ...){
  sprintf(
    "Pareto(%s, %s)",
    format(x[["shape"]], digits = digits, ...),
    format(x[["scale"]], digits = digits, ...)
  )
}

#' @export
density.dist_pareto <- function(x, at, ...){
  require_package("actuar")
  actuar::dpareto(at, x[["shape"]], x[["scale"]])
}

#' @export
log_density.dist_pareto <- function(x, at, ...){
  require_package("actuar")
  actuar::dpareto(at, x[["shape"]], x[["scale"]], log = TRUE)
}

#' @export
quantile.dist_pareto <- function(x, p, ...){
  require_package("actuar")
  actuar::qpareto(p, x[["shape"]], x[["scale"]])
}

#' @export
cdf.dist_pareto <- function(x, q, ...){
  require_package("actuar")
  actuar::ppareto(q, x[["shape"]], x[["scale"]])
}

#' @export
generate.dist_pareto <- function(x, times, ...){
  require_package("actuar")
  actuar::rpareto(times, x[["shape"]], x[["scale"]])
}

#' @export
mean.dist_pareto <- function(x, ...){
  actuar::mpareto(1, x[["shape"]], x[["scale"]])
}

#' @export
covariance.dist_pareto <- function(x, ...){
  actuar::mpareto(2, x[["shape"]], x[["scale"]]) - actuar::mpareto(1, x[["shape"]], x[["scale"]])^2
}


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/R/dist_percentile.R ---
#' Percentile distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' @param x A list of values
#' @param percentile A list of percentiles
#'
#' @examples
#' dist <- dist_normal()
#' percentiles <- seq(0.01, 0.99, by = 0.01)
#' x <- vapply(percentiles, quantile, double(1L), x = dist)
#' dist_percentile(list(x), list(percentiles*100))
#'
#' @export
dist_percentile <- function(x, percentile){
  x <- as_list_of(x, .ptype = double())
  percentile <- as_list_of(percentile, .ptype = double())
  new_dist(x = x, percentile = percentile, class = "dist_percentile")
}

#' @export
format.dist_percentile <- function(x, ...){
  sprintf(
    "percentile[%s]",
    length(x[["x"]])
  )
}

#' @export
density.dist_percentile <- function(x, at, ...){
  d <- density(generate(x, 1000), from = min(at), to = max(at), ..., na.rm=TRUE)
  stats::approx(d$x, d$y, xout = at)$y
}


#' @export
quantile.dist_percentile <- function(x, p, ...){
  out <- x[["x"]][match(p, x[["percentile"]])]
  out[is.na(out)] <- stats::approx(x = x[["percentile"]]/100, y = x[["x"]], xout = p[is.na(out)])$y
  out
}

#' @export
cdf.dist_percentile <- function(x, q, ...){
  stats::approx(x = x[["x"]], y = x[["percentile"]]/100, xout = q)$y
}

#' @export
generate.dist_percentile <- function(x, times, ...){
  stats::approx(x[["percentile"]], x[["x"]], xout=stats::runif(times,min(x[["percentile"]]),max(x[["percentile"]])))$y
}

#' @export
mean.dist_percentile <- function(x, ...) {
  # assumes percentile is sorted
  # probs <- x[["percentile"]]/100
  # i <- seq_along(probs)
  #
  # weights <- (probs[pmin(i+1, length(probs))] - probs[pmax(i-1, 1)]) / 2
  # sum(x[["x"]] * weights)


  # Fit a spline to the percentiles
  spline_fit <- stats::splinefun(x[["percentile"]], x[["x"]])

  # Use numerical integration to estimate the mean
  stats::integrate(spline_fit, lower = 0, upper = 1)$value
}

#' @export
support.dist_percentile <- function(x, ...) {
  new_support_region(
    list(vctrs::vec_init(x[["x"]], n = 0L)),
    list(range(x[["x"]])),
    list(!near(range(x[["percentile"]]), 0))
  )
}


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/R/dist_poisson.R ---
#' The Poisson Distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' Poisson distributions are frequently used to model counts.
#'
#' @inheritParams stats::dpois
#'
#'
#' @details
#'
#'   We recommend reading this documentation on
#'   <https://pkg.mitchelloharawild.com/distributional/>, where the math
#'   will render nicely.
#'
#'   In the following, let \eqn{X} be a Poisson random variable with parameter
#'   `lambda` = \eqn{\lambda}.
#'
#'   **Support**: \eqn{\{0, 1, 2, 3, ...\}}{{0, 1, 2, 3, ...}}
#'
#'   **Mean**: \eqn{\lambda}
#'
#'   **Variance**: \eqn{\lambda}
#'
#'   **Probability mass function (p.m.f)**:
#'
#'   \deqn{
#'     P(X = k) = \frac{\lambda^k e^{-\lambda}}{k!}
#'   }{
#'     P(X = k) = \lambda^k e^(-\lambda) / k!
#'   }
#'
#'   **Cumulative distribution function (c.d.f)**:
#'
#'   \deqn{
#'     P(X \le k) = e^{-\lambda}
#'     \sum_{i = 0}^{\lfloor k \rfloor} \frac{\lambda^i}{i!}
#'   }{
#'     P(X \le k) = e^(-\lambda)
#'     \sum_{i = 0}^k \lambda^i / i!
#'   }
#'
#'   **Moment generating function (m.g.f)**:
#'
#'   \deqn{
#'     E(e^{tX}) = e^{\lambda (e^t - 1)}
#'   }{
#'     E(e^(tX)) = e^(\lambda (e^t - 1))
#'   }
#' @seealso [stats::Poisson]
#'
#' @examples
#' dist <- dist_poisson(lambda = c(1, 4, 10))
#'
#' dist
#' mean(dist)
#' variance(dist)
#' skewness(dist)
#' kurtosis(dist)
#'
#' generate(dist, 10)
#'
#' density(dist, 2)
#' density(dist, 2, log = TRUE)
#'
#' cdf(dist, 4)
#'
#' quantile(dist, 0.7)
#'
#' @name dist_poisson
#' @export
dist_poisson <- function(lambda){
  lambda <- vec_cast(lambda, double())
  if(any(lambda < 0)){
    abort("The lambda parameter of an Poisson distribution must be non-negative.")
  }
  new_dist(l = lambda, class = "dist_poisson")
}

#' @export
format.dist_poisson <- function(x, digits = 2, ...){
  sprintf(
    "Pois(%s)",
    format(x[["l"]], digits = digits, ...)
  )
}

#' @export
density.dist_poisson <- function(x, at, ...){
  stats::dpois(at, x[["l"]])
}

#' @export
log_density.dist_poisson <- function(x, at, ...){
  stats::dpois(at, x[["l"]], log = TRUE)
}

#' @export
quantile.dist_poisson <- function(x, p, ...){
  stats::qpois(p, x[["l"]])
}

#' @export
cdf.dist_poisson <- function(x, q, ...){
  stats::ppois(q, x[["l"]])
}

#' @export
generate.dist_poisson <- function(x, times, ...){
  as.integer(stats::rpois(times, x[["l"]]))
}

#' @export
mean.dist_poisson <- function(x, ...){
  x[["l"]]
}

#' @export
covariance.dist_poisson <- function(x, ...){
  x[["l"]]
}

#' @export
skewness.dist_poisson <- function(x, ...) 1 / sqrt(x[["l"]])

#' @export
kurtosis.dist_poisson <- function(x, ...) 1 / x[["l"]]


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/R/dist_poisson_inverse_gaussian.R ---
#' The Poisson-Inverse Gaussian distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' @inheritParams actuar::dpoisinvgauss
#'
#' @seealso [actuar::PoissonInverseGaussian]
#'
#' @examples
#' dist <- dist_poisson_inverse_gaussian(mean = rep(0.1, 3), shape = c(0.4, 0.8, 1))
#' dist
#'
#' @examplesIf requireNamespace("actuar", quietly = TRUE)
#' mean(dist)
#' variance(dist)
#' support(dist)
#' generate(dist, 10)
#'
#' density(dist, 2)
#' density(dist, 2, log = TRUE)
#'
#' cdf(dist, 4)
#'
#' quantile(dist, 0.7)
#'
#' @name dist_poisson_inverse_gaussian
#' @export
dist_poisson_inverse_gaussian <- function(mean, shape){
  mean <- vec_cast(mean, double())
  shape <- vec_cast(shape, double())
  if(any(mean[!is.na(mean)] <= 0)){
    abort("The mean parameter of a Poisson-Inverse Gaussian distribution must be strictly positive.")
  }
  if(any(shape[!is.na(shape)] <= 0)){
    abort("The shape parameter of a Poisson-Inverse Gaussian distribution must be strictly positive.")
  }
  new_dist(m = mean, s = shape, class = "dist_poisson_inverse_gaussian")
}

#' @export
format.dist_poisson_inverse_gaussian <- function(x, digits = 2, ...){
  sprintf(
    "PIG(%s, %s)",
    format(x[["m"]], digits = digits, ...),
    format(x[["s"]], digits = digits, ...)
  )
}

#' @export
density.dist_poisson_inverse_gaussian <- function(x, at, ...){
  require_package("actuar")
  actuar::dpoisinvgauss(at, x[["m"]], x[["s"]])
}

#' @export
log_density.dist_poisson_inverse_gaussian <- function(x, at, ...){
  require_package("actuar")
  actuar::dpoisinvgauss(at, x[["m"]], x[["s"]], log = TRUE)
}

#' @export
quantile.dist_poisson_inverse_gaussian <- function(x, p, ...){
  require_package("actuar")
  actuar::qpoisinvgauss(p, x[["m"]], x[["s"]])
}

#' @export
cdf.dist_poisson_inverse_gaussian <- function(x, q, ...){
  require_package("actuar")
  actuar::ppoisinvgauss(q, x[["m"]], x[["s"]])
}

#' @export
generate.dist_poisson_inverse_gaussian <- function(x, times, ...){
  require_package("actuar")
  actuar::rpoisinvgauss(times, x[["m"]], x[["s"]])
}

#' @export
mean.dist_poisson_inverse_gaussian <- function(x, ...){
  x[["m"]]
}

#' @export
covariance.dist_poisson_inverse_gaussian <- function(x, ...){
  x[["m"]]/x[["s"]] * (x[["m"]]^2 + x[["s"]])
}


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/R/dist_sample.R ---
#' Sampling distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' @param x A list of sampled values.
#'
#' @examples
#' # Univariate numeric samples
#' dist <- dist_sample(x = list(rnorm(100), rnorm(100, 10)))
#'
#' dist
#' mean(dist)
#' variance(dist)
#' skewness(dist)
#' generate(dist, 10)
#'
#' density(dist, 1)
#'
#' # Multivariate numeric samples
#' dist <- dist_sample(x = list(cbind(rnorm(100), rnorm(100, 10))))
#' dimnames(dist) <- c("x", "y")
#'
#' dist
#' mean(dist)
#' variance(dist)
#' generate(dist, 10)
#' quantile(dist, 0.4) # Returns the marginal quantiles
#' cdf(dist, matrix(c(0.3,9), nrow = 1))
#'
#' @export
dist_sample <- function(x){
  vec_assert(x, list())
  x <- as_list_of(x, .ptype = vec_ptype(x[[1]]))
  new_dist(x = x, class = "dist_sample")
}

#' @export
format.dist_sample <- function(x, ...){
  sprintf(
    "sample[%s]",
    vapply(x, vec_size, integer(1L))
  )
}

#' @export
density.dist_sample <- function(x, at, ..., na.rm = TRUE){
  # Apply independently over sample variates
  if(is.matrix(x$x)) {
    abort("Multivariate sample density is not yet implemented.")
  }
  z <- numeric(length(at))
  zi <- is.finite(at)
  at <- at[zi]
  zl <- vec_size(at)

  # Shortcut if only one point in density is needed
  if(zl == 1){
    z[zi] <- density(x[["x"]], from = at, to = at, n = 1)$y
  } else if (zl > 1) {
    d <- density(x[["x"]], from = min(at), to = max(at), ..., na.rm=na.rm)
    z[zi] <- stats::approx(d$x, d$y, xout = at)$y
  }
  z
}


#' @export
quantile.dist_sample <- function(x, p, type = "marginal", ..., na.rm = TRUE){
  type <- match.arg(type)
  # Apply independently over sample variates
  if(is.matrix(x$x)) {
    # Marginal quantiles
    return(
      matrix(apply(x$x, 2, quantile, p = p, ..., na.rm = na.rm), nrow = length(p))
    )
  }
  quantile(x$x, probs = p, ..., na.rm = na.rm, names = FALSE)
}

#' @export
cdf.dist_sample <- function(x, q, ..., na.rm = TRUE){
  if(vec_size(q) > 1) return(vapply(q, cdf, numeric(1L), x = x, ...))
  if(is.matrix(x$x)) {
    return(mean(x$x <= vec_recycle(q, vec_size(x$x))))
  }
  mean(x <= q, ..., na.rm = na.rm)
  # vapply(x, function(x, q) mean(x <= q, ..., na.rm = na.rm), numeric(1L), q = q)
}

#' @export
generate.dist_sample <- function(x, times, ...){
  i <- sample.int(vec_size(x[["x"]]), size = times, replace = TRUE)
  if(is.matrix(x$x)) x$x[i,,drop = FALSE] else x$x[i]
}

#' @export
mean.dist_sample <- function(x, ...){
  if(is.matrix(x$x)) {
    matrix(colMeans(x$x, ...), nrow = 1L)
  } else {
    mean(x$x, ...)
  }
}

#' @export
median.dist_sample <- function(x, na.rm = FALSE, ...){
  if(is.matrix(x$x))
    matrix(apply(x$x, 2, median, na.rm = na.rm, ...), nrow = 1L)
  else
    median(x$x, na.rm = na.rm, ...)
}

#' @export
covariance.dist_sample <- function(x, ...){
  if(is.matrix(x$x)) stats::cov(x$x, ...) else stats::var(x$x, ...)
}

#' @export
skewness.dist_sample <- function(x, ..., na.rm = FALSE) {
  if(is.matrix(x$x)) {abort("Multivariate sample skewness is not yet implemented.")}
  n <- lengths(x, use.names = FALSE)
  x <- lapply(x, function(.) . - mean(., na.rm = na.rm))
  sum_x2 <- vapply(x, function(.) sum(.^2, na.rm = na.rm), numeric(1L), USE.NAMES = FALSE)
  sum_x3 <- vapply(x, function(.) sum(.^3, na.rm = na.rm), numeric(1L), USE.NAMES = FALSE)
  y <- sqrt(n) * sum_x3/(sum_x2^(3/2))
  y * ((1 - 1/n))^(3/2)
}

#' @export
support.dist_sample <- function(x, ...) {
  new_support_region(
    list(vctrs::vec_init(x$x, n = 0L)),
    list(range(x$x)),
    list(rep(TRUE, 2))
  )
}

#' @method Math dist_sample
#' @export
Math.dist_sample <- function(x, ...) {
  x <- .mapply(.Generic, list(parameters(x)$x, ...), MoreArgs = NULL)
  names(x) <- "x"
  enclass_dist(x, "dist_sample")
}

#' @method Ops dist_sample
#' @export
Ops.dist_sample <- function(e1, e2) {
  is_dist <- c(inherits(e1, "dist_sample"), inherits(e2, "dist_sample"))
  if(is_dist[1]) {
    e1 <- parameters(e1)$x
  }
  if(is_dist[2]) {
    e2 <- parameters(e2)$x
  }

  x <- .mapply(.Generic, list(e1, e2), MoreArgs = NULL)
  names(x) <- "x"
  enclass_dist(x, "dist_sample")
}


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/R/dist_student_t.R ---
#' The (non-central) location-scale Student t Distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' The Student's T distribution is closely related to the [Normal()]
#' distribution, but has heavier tails. As \eqn{\nu} increases to \eqn{\infty},
#' the Student's T converges to a Normal. The T distribution appears
#' repeatedly throughout classic frequentist hypothesis testing when
#' comparing group means.
#'
#' @inheritParams stats::dt
#' @param mu The location parameter of the distribution.
#'   If `ncp == 0` (or `NULL`), this is the median.
#' @param sigma The scale parameter of the distribution.
#'
#' @details
#'
#'   We recommend reading this documentation on
#'   <https://pkg.mitchelloharawild.com/distributional/>, where the math
#'   will render nicely.
#'
#'   In the following, let \eqn{X} be a **central** Students T random variable
#'   with `df` = \eqn{\nu}.
#'
#'   **Support**: \eqn{R}, the set of all real numbers
#'
#'   **Mean**: Undefined unless \eqn{\nu \ge 2}, in which case the mean is
#'     zero.
#'
#'   **Variance**:
#'
#'   \deqn{
#'     \frac{\nu}{\nu - 2}
#'   }{
#'     \nu / (\nu - 2)
#'   }
#'
#'   Undefined if \eqn{\nu < 1}, infinite when \eqn{1 < \nu \le 2}.
#'
#'   **Probability density function (p.d.f)**:
#'
#'   \deqn{
#'     f(x) = \frac{\Gamma(\frac{\nu + 1}{2})}{\sqrt{\nu \pi} \Gamma(\frac{\nu}{2})} (1 + \frac{x^2}{\nu} )^{- \frac{\nu + 1}{2}}
#'   }{
#'     f(x) = \Gamma((\nu + 1) / 2) / (\sqrt(\nu \pi) \Gamma(\nu / 2)) (1 + x^2 / \nu)^(- (\nu + 1) / 2)
#'   }
#'
#' @seealso [stats::TDist]
#'
#' @examples
#' dist <- dist_student_t(df = c(1,2,5), mu = c(0,1,2), sigma = c(1,2,3))
#'
#' dist
#' mean(dist)
#' variance(dist)
#'
#' generate(dist, 10)
#'
#' density(dist, 2)
#' density(dist, 2, log = TRUE)
#'
#' cdf(dist, 4)
#'
#' quantile(dist, 0.7)
#'
#' @name dist_student_t
#' @export
dist_student_t <- function(df, mu = 0, sigma = 1, ncp = NULL){
  df <- vec_cast(df, numeric())
  if(any(df <= 0)){
    abort("The degrees of freedom parameter of a Student t distribution must be strictly positive.")
  }
  mu <- vec_cast(mu, double())
  sigma <- vec_cast(sigma, double())
  if(any(sigma[!is.na(sigma)] <= 0)){
    abort("The scale (sigma) parameter of a Student t distribution must be strictly positive.")
  }
  new_dist(df = df, mu = mu, sigma = sigma, ncp = ncp, class = "dist_student_t")
}

#' @export
format.dist_student_t <- function(x, digits = 2, ...){
  out <- sprintf(
    "t(%s, %s, %s%s)",
    format(x[["df"]], digits = digits, ...),
    format(x[["mu"]], digits = digits, ...),
    format(x[["sigma"]], digits = digits, ...),
    if(is.null(x[["ncp"]])) "" else paste(",", format(x[["ncp"]], digits = digits, ...))
  )
}

#' @export
density.dist_student_t <- function(x, at, ...){
  ncp <- x[["ncp"]] %||% missing_arg()
  sigma <- x[["sigma"]]

  stats::dt((at - x[["mu"]])/sigma, x[["df"]], ncp) / sigma
}

#' @export
log_density.dist_student_t <- function(x, at, ...){
  ncp <- x[["ncp"]] %||% missing_arg()
  sigma <- x[["sigma"]]

  stats::dt((at - x[["mu"]])/sigma, x[["df"]], ncp, log = TRUE) - log(sigma)
}


#' @export
quantile.dist_student_t <- function(x, p, ...){
  ncp <- x[["ncp"]] %||% missing_arg()

  stats::qt(p, x[["df"]], ncp) * x[["sigma"]] + x[["mu"]]
}

#' @export
cdf.dist_student_t <- function(x, q, ...){
  ncp <- x[["ncp"]] %||% missing_arg()

  stats::pt((q - x[["mu"]])/x[["sigma"]], x[["df"]], ncp)
}

#' @export
generate.dist_student_t <- function(x, times, ...){
  ncp <- x[["ncp"]] %||% missing_arg()

  stats::rt(times, x[["df"]], ncp) * x[["sigma"]] + x[["mu"]]
}

#' @export
mean.dist_student_t <- function(x, ...){
  df <- x[["df"]]
  if(df <= 1) return(NA_real_)
  if(is.null(x[["ncp"]])){
    x[["mu"]]
  } else {
    x[["mu"]] + x[["ncp"]] * sqrt(df/2) * (gamma((df-1)/2)/gamma(df/2)) * x[["sigma"]]
  }
}

#' @export
covariance.dist_student_t <- function(x, ...){
  df <- x[["df"]]
  ncp <- x[["ncp"]]
  if(df <= 1) return(NA_real_)
  if(df <= 2) return(Inf)
  if(is.null(ncp)){
    df / (df - 2) * x[["sigma"]]^2
  } else {
    ((df*(1+ncp^2))/(df-2) - (ncp * sqrt(df/2) * (gamma((df-1)/2)/gamma(df/2)))^2) * x[["sigma"]]^2
  }
}


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/R/dist_studentized_range.R ---
#' The Studentized Range distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' Tukey's studentized range distribution, used for Tukey's
#' honestly significant differences test in ANOVA.
#'
#' @inheritParams stats::qtukey
#'
#' @details
#'
#'   We recommend reading this documentation on
#'   <https://pkg.mitchelloharawild.com/distributional/>, where the math
#'   will render nicely.
#'
#'   **Support**: \eqn{R^+}, the set of positive real numbers.
#'
#'   Other properties of Tukey's Studentized Range Distribution
#'   are omitted, largely because the distribution is not fun
#'   to work with.
#'
#' @seealso [stats::Tukey]
#'
#' @examples
#' dist <- dist_studentized_range(nmeans = c(6, 2), df = c(5, 4), nranges = c(1, 1))
#'
#' dist
#'
#' cdf(dist, 4)
#'
#' quantile(dist, 0.7)
#'
#' @name dist_studentized_range
#' @export
dist_studentized_range <- function(nmeans, df, nranges){
  nmeans <- vec_cast(nmeans, double())
  df <- vec_cast(df, double())
  new_dist(nm = nmeans, df = df, nr = nranges, class = "dist_studentized_range")
}

#' @export
format.dist_studentized_range <- function(x, digits = 2, ...){
  sprintf(
    "StudentizedRange(%s, %s, %s)",
    format(x[["nm"]], digits = digits, ...),
    format(x[["df"]], digits = digits, ...),
    format(x[["nr"]], digits = digits, ...)
  )
}

#' @export
quantile.dist_studentized_range <- function(x, p, ...){
  stats::qtukey(p, x[["nm"]], x[["df"]], x[["nr"]])
}

#' @export
cdf.dist_studentized_range <- function(x, q, ...){
  stats::ptukey(q, x[["nm"]], x[["df"]], x[["nr"]])
}


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/R/dist_uniform.R ---
#' The Uniform distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' A distribution with constant density on an interval.
#'
#' @inheritParams stats::dunif
#'
#' @details
#'
#'   We recommend reading this documentation on
#'   <https://pkg.mitchelloharawild.com/distributional/>, where the math
#'   will render nicely.
#'
#'   In the following, let \eqn{X} be a Poisson random variable with parameter
#'   `lambda` = \eqn{\lambda}.
#'
#'   **Support**: \eqn{[a,b]}{[a,b]}
#'
#'   **Mean**: \eqn{\frac{1}{2}(a+b)}
#'
#'   **Variance**: \eqn{\frac{1}{12}(b-a)^2}
#'
#'   **Probability mass function (p.m.f)**:
#'
#'   \deqn{
#'     f(x) = \frac{1}{b-a} for x \in [a,b]
#'   }{
#'     f(x) = \frac{1}{b-a} for x in [a,b]
#'   }
#'   \deqn{
#'     f(x) = 0 otherwise
#'   }{
#'     f(x) = 0 otherwise
#'   }
#'
#'   **Cumulative distribution function (c.d.f)**:
#'
#'   \deqn{
#'     F(x) = 0 for x < a
#'   }{
#'     F(x) = 0 for x < a
#'   }
#'   \deqn{
#'     F(x) = \frac{x - a}{b-a} for x \in [a,b]
#'   }{
#'     F(x) = \frac{x - a}{b-a} for x in [a,b]
#'   }
#'   \deqn{
#'     F(x) = 1 for x > b
#'   }{
#'     F(x) = 1 for x > b
#'   }
#'
#'   **Moment generating function (m.g.f)**:
#'
#'   \deqn{
#'     E(e^{tX}) = \frac{e^{tb} - e^{ta}}{t(b-a)} for t \neq 0
#'   }{
#'     E(e^(tX)) = \frac{e^{tb} - e^{ta}}{t(b-a)} for t \neq 0
#'   }
#'   \deqn{
#'     E(e^{tX}) = 1 for t = 0
#'   }{
#'     E(e^(tX)) = 1 for t = 0
#'   }
#'
#' @seealso [stats::Uniform]
#'
#' @examples
#' dist <- dist_uniform(min = c(3, -2), max = c(5, 4))
#'
#' dist
#' mean(dist)
#' variance(dist)
#' skewness(dist)
#' kurtosis(dist)
#'
#' generate(dist, 10)
#'
#' density(dist, 2)
#' density(dist, 2, log = TRUE)
#'
#' cdf(dist, 4)
#'
#' quantile(dist, 0.7)
#'
#' @name dist_uniform
#' @export
dist_uniform <- function(min, max){
  min <- vec_cast(min, double())
  max <- vec_cast(max, double())
  if(any(min > max)){
    abort("The min of a Uniform distribution must be less than max.")
  }
  new_dist(l = min, u = max, class = "dist_uniform")
}

#' @export
format.dist_uniform <- function(x, digits = 2, ...){
  sprintf(
    "U(%s, %s)",
    format(x[["l"]], digits = digits, ...),
    format(x[["u"]], digits = digits, ...)
  )
}

#' @export
density.dist_uniform <- function(x, at, ...){
  stats::dunif(at, x[["l"]], x[["u"]])
}

#' @export
log_density.dist_uniform <- function(x, at, ...){
  stats::dunif(at, x[["l"]], x[["u"]], log = TRUE)
}

#' @export
quantile.dist_uniform <- function(x, p, ...){
  stats::qunif(p, x[["l"]], x[["u"]])
}

#' @export
cdf.dist_uniform <- function(x, q, ...){
  stats::punif(q, x[["l"]], x[["u"]])
}

#' @export
generate.dist_uniform <- function(x, times, ...){
  stats::runif(times, x[["l"]], x[["u"]])
}

#' @export
mean.dist_uniform <- function(x, ...){
  (x[["u"]]+x[["l"]])/2
}

#' @export
covariance.dist_uniform <- function(x, ...){
  (x[["u"]]-x[["l"]])^2/12
}

#' @export
skewness.dist_uniform <- function(x, ...) 0

#' @export
kurtosis.dist_uniform <- function(x, ...) -6/5


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/R/dist_weibull.R ---
#' The Weibull distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' Generalization of the gamma distribution. Often used in survival and
#' time-to-event analyses.
#'
#' @inheritParams stats::dweibull
#'
#' @details
#'
#'   We recommend reading this documentation on
#'   <https://pkg.mitchelloharawild.com/distributional/>, where the math
#'   will render nicely.
#'
#'   In the following, let \eqn{X} be a Weibull random variable with
#'   success probability `p` = \eqn{p}.
#'
#'   **Support**: \eqn{R^+} and zero.
#'
#'   **Mean**: \eqn{\lambda \Gamma(1+1/k)}, where \eqn{\Gamma} is
#'   the gamma function.
#'
#'   **Variance**: \eqn{\lambda [ \Gamma (1 + \frac{2}{k} ) - (\Gamma(1+ \frac{1}{k}))^2 ]}
#'
#'   **Probability density function (p.d.f)**:
#'
#'   \deqn{
#'     f(x) = \frac{k}{\lambda}(\frac{x}{\lambda})^{k-1}e^{-(x/\lambda)^k}, x \ge 0
#'   }
#'
#'   **Cumulative distribution function (c.d.f)**:
#'
#'   \deqn{F(x) = 1 - e^{-(x/\lambda)^k}, x \ge 0}
#'
#'   **Moment generating function (m.g.f)**:
#'
#'   \deqn{\sum_{n=0}^\infty \frac{t^n\lambda^n}{n!} \Gamma(1+n/k), k \ge 1}
#'
#' @seealso [stats::Weibull]
#'
#' @examples
#' dist <- dist_weibull(shape = c(0.5, 1, 1.5, 5), scale = rep(1, 4))
#'
#' dist
#' mean(dist)
#' variance(dist)
#' skewness(dist)
#' kurtosis(dist)
#'
#' generate(dist, 10)
#'
#' density(dist, 2)
#' density(dist, 2, log = TRUE)
#'
#' cdf(dist, 4)
#'
#' quantile(dist, 0.7)
#'
#' @name dist_weibull
#' @export
dist_weibull <- function(shape, scale){
  shape <- vec_cast(shape, double())
  scale <- vec_cast(scale, double())
  if(any(shape[!is.na(shape)] < 0)){
    abort("The shape parameter of a Weibull distribution must be non-negative.")
  }
  if(any(scale[!is.na(scale)] <= 0)){
    abort("The scale parameter of a Weibull distribution must be strictly positive.")
  }
  new_dist(shape = shape, scale = scale, class = "dist_weibull")
}

#' @export
format.dist_weibull <- function(x, digits = 2, ...){
  sprintf(
    "Weibull(%s, %s)",
    format(x[["shape"]], digits = digits, ...),
    format(x[["scale"]], digits = digits, ...)
  )
}

#' @export
density.dist_weibull <- function(x, at, ...){
  stats::dweibull(at, x[["shape"]], x[["scale"]])
}

#' @export
log_density.dist_weibull <- function(x, at, ...){
  stats::dweibull(at, x[["shape"]], x[["scale"]], log = TRUE)
}

#' @export
quantile.dist_weibull <- function(x, p, ...){
  stats::qweibull(p, x[["shape"]], x[["scale"]])
}

#' @export
cdf.dist_weibull <- function(x, q, ...){
  stats::pweibull(q, x[["shape"]], x[["scale"]])
}

#' @export
generate.dist_weibull <- function(x, times, ...){
  stats::rweibull(times, x[["shape"]], x[["scale"]])
}

#' @export
mean.dist_weibull <- function(x, ...){
  x[["scale"]] * gamma(1 + 1/x[["shape"]])
}

#' @export
covariance.dist_weibull <- function(x, ...){
  x[["scale"]]^2 * (gamma(1 + 2/x[["shape"]]) - gamma(1 + 1/x[["shape"]])^2)
}

#' @export
skewness.dist_weibull <- function(x, ...) {
  mu <- mean(x)
  sigma <- sqrt(variance(x))
  r <- mu / sigma
  gamma(1 + 3/x[["shape"]]) * (x[["scale"]]/sigma)^3 - 3*r - 3^r
}

#' @export
kurtosis.dist_weibull <- function(x, ...) {
  mu <- mean(x)
  sigma <- sqrt(variance(x))
  gamma <- skewness(x)
  r <- mu / sigma
  (x[["scale"]]/sigma)^4 * gamma(1 + 4/x[["shape"]]) - 4*gamma*r -6*r^2 - r^4 - 3
}


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/R/dist_wrap.R ---
#' Create a distribution from p/d/q/r style functions
#'
#' @description
#' `r lifecycle::badge('maturing')`
#'
#' If a distribution is not yet supported, you can vectorise p/d/q/r functions
#' using this function. `dist_wrap()` stores the distributions parameters, and
#' provides wrappers which call the appropriate p/d/q/r functions.
#'
#' Using this function to wrap a distribution should only be done if the
#' distribution is not yet available in this package. If you need a distribution
#' which isn't in the package yet, consider making a request at
#' https://github.com/mitchelloharawild/distributional/issues.
#'
#' @param dist The name of the distribution used in the functions (name that is
#' prefixed by p/d/q/r)
#' @param ... Named arguments used to parameterise the distribution.
#' @param package The package from which the distribution is provided. If NULL,
#' the calling environment's search path is used to find the distribution
#' functions. Alternatively, an arbitrary environment can also be provided here.
# #' @param p,d,q,r The functions used to compute the p/d/q/r
# #' (pdf/cdf/quantile/generate)
#'
#' @examples
#' dist <- dist_wrap("norm", mean = 1:3, sd = c(3, 9, 2))
#'
#' density(dist, 1) # dnorm()
#' cdf(dist, 4) # pnorm()
#' quantile(dist, 0.975) # qnorm()
#' generate(dist, 10) # rnorm()
#'
#' library(actuar)
#' dist <- dist_wrap("invparalogis", package = "actuar", shape = 2, rate = 2)
#' density(dist, 1) # actuar::dinvparalogis()
#' cdf(dist, 4) # actuar::pinvparalogis()
#' quantile(dist, 0.975) # actuar::qinvparalogis()
#' generate(dist, 10) # actuar::rinvparalogis()
#'
#' @export
dist_wrap <- function(dist, ..., package = NULL){
  vec_assert(dist, character(), 1L)
  if(is.null(package)) {
    env <- rlang::caller_env()
  } else if (is.character(package)) {
    env <- rlang::pkg_env(package)
  } else {
    env <- as.environment(package)
  }
  par <- vec_recycle_common(dist = dist, env = list(env), ...)
  new_dist(!!!par, class = "dist_wrap")
}

#' @export
format.dist_wrap <- function(x, ...){
  sprintf(
    "%s(%s)",
    x[["dist"]],
    paste0(x[-(1:2)], collapse = ", ")
  )
}

#' @export
density.dist_wrap <- function(x, at, ...){
  fn <- get(paste0("d", x[["dist"]][[1]]), envir = x$env, mode = "function")

  # Remove distribution name and environment from parameters
  par <- x[-(1:2)]

  do.call(fn, c(list(at), par))
}

#' @export
log_density.dist_wrap <- function(x, at, ...){
  fn <- get(paste0("d", x[["dist"]][[1]]), envir = x$env, mode = "function")

  # Remove distribution name and environment from parameters
  par <- x[-(1:2)]

  # Use density(log = TRUE) if supported
  if(is.null(formals(fn)$log)){
    log(do.call(fn, c(list(at), par)))
  } else {
    do.call(fn, c(list(at), par, log = TRUE))
  }
}

#' @export
cdf.dist_wrap <- function(x, q, ...){
  fn <- get(paste0("p", x[["dist"]][[1]]), envir = x$env, mode = "function")

  # Remove distribution name and environment from parameters
  par <- x[-(1:2)]

  do.call(fn, c(list(q), par))
}

#' @export
quantile.dist_wrap <- function(x, p, ...){
  fn <- get(paste0("q", x[["dist"]][[1]]), envir = x$env, mode = "function")

  # Remove distribution name and environment from parameters
  par <- x[-(1:2)]

  do.call(fn, c(list(p), par))
}

#' @export
generate.dist_wrap <- function(x, times, ...){
  fn <- get(paste0("r", x[["dist"]][[1]]), envir = x$env, mode = "function")

  # Remove distribution name and environment from parameters
  par <- x[-(1:2)]

  do.call(fn, c(list(times), par))
}

#' @export
parameters.dist_wrap <- function(x, ...) {
  # All parameters except distribution environment
  x[-2L]
}


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/R/distribution.R ---
#' Create a new distribution
#'
#' @description
#' `r lifecycle::badge('maturing')`
#'
#' Allows extension package developers to define a new distribution class
#' compatible with the distributional package.
#'
#' @param ... Parameters of the distribution (named).
#' @param class The class of the distribution for S3 dispatch.
#' @param dimnames The names of the variables in the distribution (optional).
#'
#' @export
new_dist <- function(..., class = NULL, dimnames = NULL){
  args <- transpose(vctrs::vec_recycle_common(...))
  wrap_dist(
    lapply(args, enclass_dist, class = class),
    dimnames = dimnames
  )
}

enclass_dist <- function(x, class) {
  structure(x, class = c(class, "dist_default"))
}


wrap_dist <- function(x, dimnames = NULL){
  vctrs::new_vctr(x, vars = dimnames, class = "distribution")
}

#' @export
vec_ptype_abbr.distribution <- function(x, ...){
  "dist"
}

#' @export
format.distribution <- function(x, ...){
  x <- vec_data(x)
  out <- vapply(x, format, character(1L), ...)
  out[vapply(x, is.null, logical(1L))] <- "NA"
  out
}

#' @importFrom pillar pillar_shaft new_pillar_shaft get_max_extent
#' @export
pillar_shaft.distribution <- function(x, ...) {
  dist = format(x)
  dist_min = format(x, width = 30)

  pillar::new_pillar_shaft(
    list(dist = dist,
         dist_min = dist_min),
    width = pillar::get_max_extent(dist),
    min_width = pillar::get_max_extent(dist_min),
    class = "pillar_distribution"
  )
}

#' @export
#' @importFrom pillar new_ornament
format.pillar_distribution <- function(x, width, ...) {
  if (get_max_extent(x$dist) <= width) {
    ornament <- x$dist
  } else {
    ornament <- x$dist_min
  }

  pillar::new_ornament(ornament, align = "right")
}

#' @export
`dimnames<-.distribution` <- function(x, value){
  attr(x, "vars") <- value
  x
}
#' @export
dimnames.distribution <- function(x){
  attr(x, "vars")
}

#' @export
`[[.distribution` <- `[`

#' The probability density/mass function
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' Computes the probability density function for a continuous distribution, or
#' the probability mass function for a discrete distribution.
#'
#' @param x The distribution(s).
#' @param at The point at which to compute the density/mass.
#' @param ... Additional arguments passed to methods.
#' @param log If `TRUE`, probabilities will be given as log probabilities.
#'
#' @importFrom stats density
#' @export
density.distribution <- function(x, at, ..., log = FALSE){
  if(log) return(log_density(x, at, ...))
  at <- arg_listable(at, .ptype = NULL)
  dist_apply(x, density, at = at, ...)
}

log_density <- function(x, at, ...) {
  UseMethod("log_density")
}
#' @export
log_density.distribution <- function(x, at, ...){
  at <- arg_listable(at, .ptype = NULL)
  dist_apply(x, log_density, at = at, ...)
}

#' Distribution Quantiles
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' Computes the quantiles of a distribution.
#'
#' @inheritParams density.distribution
#' @param p The probability of the quantile.
#' @param ... Additional arguments passed to methods.
#'
#' @importFrom stats quantile
#' @export
quantile.distribution <- function(x, p, ..., log = FALSE){
  if(log) return(log_quantile(x, p, ...))
  p <- arg_listable(p, .ptype = double())
  dist_apply(x, quantile, p = p, ...)
}
log_quantile <- function(x, p, ...) {
  UseMethod("log_quantile")
}
#' @export
log_quantile.distribution <- function(x, p, ...){
  vec_assert(q, double(), 1L)
  p <- arg_listable(p, .ptype = double())
  dist_apply(x, log_quantile, p = p, ...)
}

#' The cumulative distribution function
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' @inheritParams density.distribution
#' @param q The quantile at which the cdf is calculated.
#'
#' @name cdf
#' @export
cdf <- function (x, q, ..., log = FALSE){
  if(log) return(log_cdf(x, q, ...))
  UseMethod("cdf")
}
#' @rdname cdf
#' @export
cdf.distribution <- function(x, q, ...){
  q <- arg_listable(q, .ptype = NULL)
  dist_apply(x, cdf, q = q, ...)
}
log_cdf <- function(x, q, ...) {
  UseMethod("log_cdf")
}
#' @export
log_cdf.distribution <- function(x, q, ...){
  q <- arg_listable(q, .ptype = NULL)
  dist_apply(x, log_cdf, q = q, ...)
}

#' Randomly sample values from a distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' Generate random samples from probability distributions.
#'
#' @param x The distribution(s).
#' @param times The number of samples.
#' @param ... Additional arguments used by methods.
#'
#' @export
generate.distribution <- function(x, times, ...){
  times <- vec_cast(times, integer())
  times <- vec_recycle(times, size = length(x))
  dn <- dimnames(x)
  x <- vec_data(x)
  dist_is_na <- vapply(x, is.null, logical(1L))
  x[dist_is_na] <- list(structure(list(), class = c("dist_na", "dist_default")))
  mapply(
    function(x, ...) {
      y <- generate(x, ...)
      if (is.matrix(y)) colnames(y) <- dn
      y
    }, x, times = times, ...,
    SIMPLIFY = FALSE
  )
  # dist_apply(x, generate, times = times, ...)
  # Needs work to structure MV appropriately.
}

#' The (log) likelihood of a sample matching a distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' @param x The distribution(s).
#' @param ... Additional arguments used by methods.
#'
#' @name likelihood
#' @export
likelihood <- function (x, ...){
  UseMethod("likelihood")
}

#' @rdname likelihood
#' @param sample A list of sampled values to compare to distribution(s).
#' @param log If `TRUE`, the log-likelihood will be computed.
#' @export
likelihood.distribution <- function(x, sample, ..., log = FALSE){
  if(vec_is(sample, numeric())) {
    warn("The `sample` argument of `likelihood()` should contain a list of numbers.
The same sample will be used for each distribution, i.e. `sample = list(sample)`.")
    sample <- list(sample)
  }
  if(log){
    dist_apply(x, log_likelihood, sample = sample, ...)
  } else {
    dist_apply(x, likelihood, sample = sample, ...)
  }
}

#' @rdname likelihood
#' @export
log_likelihood <- function(x, ...) {
  UseMethod("log_likelihood")
}
#' @export
log_likelihood.distribution <- function(x, sample, ...){
  dist_apply(x, log_likelihood, sample = sample, ...)
}

#' Extract the parameters of a distribution
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' @param x The distribution(s).
#' @param ... Additional arguments used by methods.
#'
#' @name parameters
#' @examples
#' dist <- c(
#'   dist_normal(1:2),
#'   dist_poisson(3),
#'   dist_multinomial(size = c(4, 3),
#'   prob = list(c(0.3, 0.5, 0.2), c(0.1, 0.5, 0.4)))
#'   )
#' parameters(dist)
#' @export
parameters <- function(x, ...) {
  UseMethod("parameters")
}

#' @rdname parameters
#' @export
parameters.distribution <- function(x, ...) {
  x <- lapply(vec_data(x), parameters)
  x <- lapply(x, function(z) data_frame(!!!z, .name_repair = "minimal"))
  vec_rbind(!!!x)
}

#' Extract the name of the distribution family
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' @param object The distribution(s).
#' @param ... Additional arguments used by methods.
#'
#' @examples
#' dist <- c(
#'   dist_normal(1:2),
#'   dist_poisson(3),
#'   dist_multinomial(size = c(4, 3),
#'   prob = list(c(0.3, 0.5, 0.2), c(0.1, 0.5, 0.4)))
#'   )
#' family(dist)
#'
#' @importFrom stats family
#' @export
family.distribution <- function(object, ...) {
  vapply(vec_data(object), family, character(1L))
}

#' Region of support of a distribution
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' @param x The distribution(s).
#' @param ... Additional arguments used by methods.
#'
#' @name support
#' @export
support <- function(x, ...) {
  UseMethod("support")
}

#' @rdname support
#' @export
support.distribution <- function(x, ...) {
  dist_apply(x, support, ...)
}

#' Mean of a probability distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' Returns the empirical mean of the probability distribution. If the method
#' does not exist, the mean of a random sample will be returned.
#'
#' @param x The distribution(s).
#' @param ... Additional arguments used by methods.
#'
#' @export
mean.distribution <- function(x, ...){
  dist_apply(x, mean, ...)
}

#' Variance
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' A generic function for computing the variance of an object.
#'
#' @param x An object.
#' @param ... Additional arguments used by methods.
#'
#' @details
#'
#' The implementation of `variance()` for numeric variables coerces the input to
#' a vector then uses [`stats::var()`] to compute the variance. This means that,
#' unlike [`stats::var()`], if `variance()` is passed a matrix or a 2-dimensional
#' array, it will still return the variance ([`stats::var()`] returns the
#' covariance matrix in that case).
#'
#' @seealso [`variance.distribution()`], [`covariance()`]
#'
#' @export
variance <- function(x, ...){
  UseMethod("variance")
}
#' @export
variance.default <- function(x, ...){
  stop(
    "The variance() method is not supported for objects of type ",
    paste(deparse(class(x)), collapse = "")
  )
}
#' @rdname variance
#' @export
variance.numeric <- function(x, ...){
  stats::var(as.vector(x), ...)
}
#' @rdname variance
#' @export
variance.matrix <- function(x, ...){
  diag(stats::cov(x, ...))
}

#' Variance of a probability distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' Returns the empirical variance of the probability distribution. If the method
#' does not exist, the variance of a random sample will be returned.
#'
#' @param x The distribution(s).
#' @param ... Additional arguments used by methods.
#'
#' @export
variance.distribution <- function(x, ...){
  dist_apply(x, variance, ...)
}

#' Covariance
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' A generic function for computing the covariance of an object.
#'
#' @param x An object.
#' @param ... Additional arguments used by methods.
#'
#' @seealso [`covariance.distribution()`], [`variance()`]
#'
#' @export
covariance <- function(x, ...){
  UseMethod("covariance")
}
#' @export
covariance.default <- function(x, ...){
  stop(
    "The covariance() method is not supported for objects of type ",
    paste(deparse(class(x)), collapse = "")
  )
}
#' @rdname variance
#' @export
covariance.numeric <- function(x, ...){
  stats::cov(x, ...)
}
#' Covariance of a probability distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' Returns the empirical covariance of the probability distribution. If the
#' method does not exist, the covariance of a random sample will be returned.
#'
#' @param x The distribution(s).
#' @param ... Additional arguments used by methods.
#'
#' @export
covariance.distribution <- function(x, ...){
  dist_apply(x, covariance, ...)
}

#' Skewness of a probability distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' @param x The distribution(s).
#' @param ... Additional arguments used by methods.
#'
#' @export
skewness <- function(x, ...) {
  UseMethod("skewness")
}
#' @rdname skewness
#' @export
skewness.distribution <- function(x, ...){
  dist_apply(x, skewness, ...)
}

#' Kurtosis of a probability distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' @param x The distribution(s).
#' @param ... Additional arguments used by methods.
#'
#' @export
kurtosis <- function(x, ...) {
  UseMethod("kurtosis")
}
#' @rdname kurtosis
#' @export
kurtosis.distribution <- function(x, ...){
  dist_apply(x, kurtosis, ...)
}

#' Median of a probability distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' Returns the median (50th percentile) of a probability distribution. This is
#' equivalent to `quantile(x, p=0.5)`.
#'
#' @param x The distribution(s).
#' @param na.rm Unused, included for consistency with the generic function.
#' @param ... Additional arguments used by methods.
#'
#' @importFrom stats median
#' @export
median.distribution <- function(x, na.rm = FALSE, ...){
  dist_apply(x, median, na.rm = na.rm, ...)
}

#' Probability intervals of a probability distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' Returns a `hilo` central probability interval with probability coverage of
#' `size`. By default, the distribution's [`quantile()`] will be used to compute
#' the lower and upper bound for a centered interval
#'
#' @param x The distribution(s).
#' @param size The size of the interval (between 0 and 100).
#' @param ... Additional arguments used by methods.
#'
#' @seealso [`hdr.distribution()`]
#'
#' @importFrom stats median
#' @export
hilo.distribution <- function(x, size = 95, ...){
  size <- arg_listable(size, .ptype = double())
  dist_apply(x, hilo, size = size, ...)
}

#' Highest density regions of probability distributions
#'
#' @description
#' `r lifecycle::badge('maturing')`
#'
#' This function is highly experimental and will change in the future. In
#' particular, improved functionality for object classes and visualisation tools
#' will be added in a future release.
#'
#' Computes minimally sized probability intervals highest density regions.
#'
#' @param x The distribution(s).
#' @param size The size of the interval (between 0 and 100).
#' @param n The resolution used to estimate the distribution's density.
#' @param ... Additional arguments used by methods.
#'
#' @export
hdr.distribution <- function(x, size = 95, n = 512, ...){
  size <- arg_listable(size, .ptype = double())
  dist_apply(x, hdr, size = size, n = n, ...)
}

#' @export
sum.distribution <- function(x, ...){
  vec_restore(list(Reduce("+", x)), x)
}

#' @method vec_arith distribution
#' @export
vec_arith.distribution <- function(op, x, y, ...){
  UseMethod("vec_arith.distribution", y)
}
#' @method vec_arith.distribution default
#' @export
vec_arith.distribution.default <- function(op, x, y, ...){
  dist_is_na <- vapply(x, is.null, logical(1L))
  x[dist_is_na] <- list(structure(list(), class = c("dist_na", "dist_default")))
  if(is_empty(y)){
    out <- lapply(vec_data(x), get(op))
  }
  else {
    x <- vec_recycle_common(x = x, y = y)
    y <- x[["y"]]
    if(is_distribution(y)) y <- vec_data(y)
    x <- x[["x"]]
    out <- .mapply(get(op),
                   dots = list(x = vec_data(x), y = y),
                   MoreArgs = NULL)
  }
  vec_restore(out, x)
}

#' @method vec_arith.numeric distribution
#' @export
vec_arith.numeric.distribution <- function(op, x, y, ...){
  x <- vec_recycle_common(x = x, y = y)
  y <- x[["y"]]
  x <- x[["x"]]
  out <- .mapply(get(op), list(x = x, y = vec_data(y)), MoreArgs = NULL)
  vec_restore(out, y)
}

#' @method vec_math distribution
#' @export
vec_math.distribution <- function(.fn, .x, ...) {
  if(.fn %in% c("is.nan", "is.infinite")) return(rep_len(FALSE, length(.x)))
  if(.fn == "is.finite") return(rep_len(TRUE, length(.x)))
  out <- lapply(vec_data(.x), get(.fn), ...)
  vec_restore(out, .x)
}

#' @export
vec_ptype2.distribution.distribution <- function(x, y, ...){
  if(!identical(dimnames(x), dimnames(y))){
    abort("Distributions must have the same `dimnames` to be combined.")
  }
  x
}
#' @export
vec_ptype2.double.distribution <- function(x, y, ...) new_dist()
#' @export
vec_ptype2.distribution.double <- function(x, y, ...) new_dist()
#' @export
vec_ptype2.integer.distribution <- function(x, y, ...) new_dist()
#' @export
vec_ptype2.distribution.integer <- function(x, y, ...) new_dist()

#' @export
vec_cast.distribution.distribution <- function(x, to, ...){
  dimnames(x) <- dimnames(to)
  x
}
#' @export
vec_cast.distribution.double <- function(x, to, ...){
  x <- dist_degenerate(x)
  dimnames(x) <- dimnames(to)
  x
}
#' @export
vec_cast.distribution.integer <- vec_cast.distribution.double
#' @export
vec_cast.character.distribution <- function(x, to, ...){
  format(x)
}

#' Test if the object is a distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' This function returns `TRUE` for distributions and `FALSE` for all other objects.
#'
#' @param x An object.
#'
#' @return TRUE if the object inherits from the distribution class.
#' @rdname is-distribution
#' @examples
#' dist <- dist_normal()
#' is_distribution(dist)
#' is_distribution("distributional")
#' @export
is_distribution <- function(x) {
    inherits(x, "distribution")
}


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/R/distributional-package.R ---
#' @keywords internal
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
#' @importFrom lifecycle deprecate_soft
#' @importFrom lifecycle deprecated
## usethis namespace: end
#' @import vctrs
#' @import rlang
NULL

# Used for generating transformation function expressions
globalVariables("x")


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/R/hdr.R ---
#' Construct hdr intervals
#'
#' @param lower,upper A list of numeric vectors specifying the region's lower
#' and upper bounds.
#' @param size A numeric vector specifying the coverage size of the region.
#'
#' @return A "hdr" vector
#'
#' @author Mitchell O'Hara-Wild
#'
#' @examples
#'
#' new_hdr(lower = list(1, c(3,6)), upper = list(10, c(5, 8)), size = c(80, 95))
#'
#' @export
new_hdr <- function(lower = list_of(.ptype = double()),
                    upper = list_of(.ptype = double()),
                    size = double()) {
  lower <- as_list_of(lower)
  upper <- as_list_of(upper)
  vec_assert(lower, list_of(.ptype = double()))
  vec_assert(upper, list_of(.ptype = double()))
  vec_assert(size, double())
  if (any(size < 0 | size > 100, na.rm = TRUE))
    abort("'size' must be between [0, 100].")


  out <- vec_recycle_common(lower = lower, upper = upper)
  .mapply(
    function(l,u) if (any(u<l, na.rm = TRUE)) abort("`upper` can't be lower than `lower`."),
    list(l = out[["lower"]], u = out[["upper"]]),
    MoreArgs = NULL
  )
  out[["level"]] <- vctrs::vec_recycle(size, vec_size(out[[1]]))

  vctrs::new_rcrd(out, class = "hdr")
}

#' Compute highest density regions
#'
#' Used to extract a specified prediction interval at a particular confidence
#' level from a distribution.
#'
#' @param x Object to create hilo from.
#' @param ... Additional arguments used by methods.
#'
#' @export
hdr <- function(x, ...){
  UseMethod("hdr")
}

#' @export
hdr.default <- function(x, ...){
  abort(sprintf(
    "Objects of type `%s` are not supported by `hdr()`, you can create a custom `hdr` with `new_hdr()`",
    class(x)
  ))
}

#' Is the object a hdr
#'
#' @param x An object.
#'
#' @export
is_hdr <- function(x) {
  inherits(x, "hdr")
}

#' @export
format.hdr <- function(x, justify = "right", ...) {
  out <- .mapply(function(l,u,s) {
    limit <- paste(
      format(l, justify = justify, ...),
      format(u, justify = justify, ...),
      sep = ", "
    )
    limit <- paste0("[", limit, "]", collapse = "")
    paste0(limit, s)
  }, list(l = field(x, "lower"), u = field(x, "upper"), s = field(x, "level")), MoreArgs = NULL)
  as.vector(out, "character")
}


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/R/hilo.R ---
#' Construct hilo intervals
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' Class constructor function to help with manually creating hilo interval
#' objects.
#'
#' @param lower,upper A numeric vector of values for lower and upper limits.
#' @param size Size of the interval between \[0, 100\].
#'
#' @return A "hilo" vector
#'
#' @author Earo Wang & Mitchell O'Hara-Wild
#'
#' @examples
#' new_hilo(lower = rnorm(10), upper = rnorm(10) + 5, size = 95)
#'
#' @export
new_hilo <- function(lower = double(), upper = double(), size = double()) {
  vec_assert(size, double())
  if (any(size < 0 | size > 100, na.rm = TRUE))
    abort("'size' must be between [0, 100].")

  out <- vec_recycle_common(lower = lower, upper = upper)
  if(vec_is(lower, double()) && vec_is(upper, double())) {
    if (any(out[["upper"]] < out[["lower"]], na.rm = TRUE)) {
      abort("`upper` can't be lower than `lower`.")
    }
  }
  len <- vec_size(out[[1]])
  out[["level"]] <- vctrs::vec_recycle(size, len)

  vctrs::new_rcrd(out, class = "hilo")
}

#' Compute intervals
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' Used to extract a specified prediction interval at a particular confidence
#' level from a distribution.
#'
#' The numeric lower and upper bounds can be extracted from the interval using
#' `<hilo>$lower` and `<hilo>$upper` as shown in the examples below.
#'
#' @param x Object to create hilo from.
#' @param ... Additional arguments used by methods.
#'
#' @examples
#' # 95% interval from a standard normal distribution
#' interval <- hilo(dist_normal(0, 1), 95)
#' interval
#'
#' # Extract the individual quantities with `$lower`, `$upper`, and `$level`
#' interval$lower
#' interval$upper
#' interval$level
#' @export
hilo <- function(x, ...){
  UseMethod("hilo")
}

#' @export
hilo.default <- function(x, ...){
  abort(sprintf(
    "Objects of type `%s` are not supported by `hilo()`, you can create a custom `hilo` with `new_hilo()`",
    class(x)
  ))
}

#' Is the object a hilo
#'
#' @param x An object.
#'
#' @export
is_hilo <- function(x) {
  inherits(x, "hilo")
}

#' @export
format.hilo <- function(x, justify = "right", ...) {
  lwr <- field(x, "lower")
  upr <- field(x, "upper")
  if(is.matrix(lwr)) {
    lwr <- if(ncol(lwr) > 1) vctrs::vec_ptype_abbr(lwr) else drop(lwr)
  }
  if(is.matrix(upr)) {
    upr <- if(ncol(upr) > 1) vctrs::vec_ptype_abbr(upr) else drop(upr)
  }
  limit <- paste(
    format(lwr, justify = justify, ...),
    format(upr, justify = justify, ...),
    sep = ", "
  )
  paste0("[", limit, "]", field(x, "level"))
}

#' @export
is.na.hilo <- function(x) {
  # both lower and upper are NA's
  x <- vec_data(x)
  is.na(x$lower) & is.na(x$upper)
}

#' @export
vec_ptype2.hilo.hilo <- function(x, y, ...){
  x
}

#' @export
vec_cast.character.hilo <- function(x, to, ...){
  sprintf(
    "[%s, %s]%s",
    as.character(x$lower), as.character(x$upper), as.character(x$level)
  )
}

#' @method vec_math hilo
#' @export
vec_math.hilo <- function(.fn, .x, ...){
  out <- vec_data(.x)
  if(.fn == "mean")
    abort("Cannot compute the mean of hilo intervals.")
  out[["lower"]] <- get(.fn)(out[["lower"]], ...)
  out[["upper"]] <- get(.fn)(out[["upper"]], ...)
  if(.fn %in% c("is.nan", "is.finite", "is.infinite"))
    return(out[["lower"]] | out[["upper"]])
  vec_restore(out, .x)
}

#' @method vec_arith hilo
#' @export
vec_arith.hilo <- function(op, x, y, ...){
  out <- dt_x <- vec_data(x)
  if(is_hilo(y)){
    abort("Intervals should not be added to other intervals, the sum of intervals is not the interval from a sum of distributions.")
  }
  else if(is_empty(y)){
    if(op == "-"){
      out[["upper"]] <- get(op)(dt_x[["lower"]])
      out[["lower"]] <- get(op)(dt_x[["upper"]])
    }
  }
  else{
    out[["lower"]] <- get(op)(dt_x[["lower"]], y)
    out[["upper"]] <- get(op)(dt_x[["upper"]], y)
  }
  vec_restore(out, x)
}

#' @method vec_arith.numeric hilo
#' @export
vec_arith.numeric.hilo <- function(op, x, y, ...){
  out <- hl <- vec_data(y)
  out[["lower"]] <- get(op)(x, hl[["lower"]])
  out[["upper"]] <- get(op)(x, hl[["upper"]])
  if(x < 0 && op %in% c("*", "/")){
    out[c("lower", "upper")] <- out[c("upper", "lower")]
  }
  vec_restore(out, y)
}

#' @importFrom utils .DollarNames
#' @export
.DollarNames.hilo <- function(x, pattern){
  utils::.DollarNames(vec_data(x), pattern)
}

#' @export
`$.hilo` <- function(x, name){
  field(x, name)
}

#' @export
`names<-.hilo` <- function(x, value) {
  # abort("A <hilo> object cannot be named.")
  x
}


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/R/inflated.R ---
#' Inflate a value of a probability distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' @param dist The distribution(s) to inflate.
#' @param prob The added probability of observing `x`.
#' @param x The value to inflate. The default of `x = 0` is for zero-inflation.
#'
#' @name dist_inflated
#' @export
dist_inflated <- function(dist, prob, x = 0){
  vec_is(dist, new_dist())
  if(prob < 0 || prob > 1){
    abort("The inflation probability must be between 0 and 1.")
  }
  new_dist(dist = dist, x = x, p = prob,
           dimnames = dimnames(dist), class = "dist_inflated")
}

#' @export
format.dist_inflated <- function(x, ...){
  sprintf(
    "%s+%s",
    format(x[["x"]]),
    format(x[["dist"]])
  )
}

#' @export
density.dist_inflated <- function(x, at, ...){
  x[["p"]]*(at==x[["x"]]) + (1-x[["p"]])*density(x[["dist"]], at, ...)
}

#' @export
quantile.dist_inflated <- function(x, p, ...){
  qt <- quantile(x[["dist"]], pmax(0, (p - x[["p"]]) / (1-x[["p"]])), ...)
  if(qt >= x[["x"]]) return(qt)
  qt <- quantile(x[["dist"]], p, ...)
  if(qt < x[["x"]]) qt else x[["x"]]
}

#' @export
cdf.dist_inflated <- function(x, q, ...){
  x[["p"]]*(q>=x[["x"]]) + (1-x[["p"]])*cdf(x[["dist"]], q, ...)
}

#' @export
generate.dist_inflated <- function(x, times, ...){
  p <- x[["p"]]
  inf <- stats::runif(times) < p
  r <- vec_init(x[["x"]], times)
  r[inf] <- x[["x"]]
  r[!inf] <- generate(x[["dist"]], sum(!inf))
  r
}

#' @export
mean.dist_inflated <- function(x, ...){
  # Can't compute if inflation value is not numeric
  if(!vec_is(x[["x"]], numeric())) return(NA_real_)

  p <- x[["p"]]
  p*x[["x"]] + (1-p)*mean(x[["dist"]])
}

#' @export
covariance.dist_inflated <- function(x, ...){
  # Can't compute if inflation value is not numeric
  if(!vec_is(x[["x"]], numeric())) return(NA_real_)
  # Can't (easily) compute if inflation value is not zero
  if(x[["x"]] != 0) return(NA_real_)

  m1 <- mean(x[["dist"]])
  v <- variance(x[["dist"]])
  m2 <- v + m1^2
  p <- x[["p"]]
  (1-p)*v + p*(1-p)*m1^2
}


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/R/mixture.R ---
#' Create a mixture of distributions
#'
#' @description
#' `r lifecycle::badge('maturing')`
#'
#' @param ... Distributions to be used in the mixture.
#' @param weights The weight of each distribution passed to `...`.
#'
#' @examples
#' dist_mixture(dist_normal(0, 1), dist_normal(5, 2), weights = c(0.3, 0.7))
#'
#' @export
dist_mixture <- function(..., weights = numeric()){
  dist <- dots_list(...)
  dn <- unique(lapply(dist, dimnames))
  dn <- if(length(dn) == 1) dn[[1]] else NULL

  vec_is(weights, numeric(), length(dist))

  if(!abs(sum(weights)-1) < sqrt(.Machine$double.eps)){
    abort("Weights of a mixture model must sum to 1.")
  }
  if(any(weights < 0)){
    abort("All weights in a mixtue model must be non-negative.")
  }
  new_dist(dist = transpose(dist), w = list(weights),
           class = "dist_mixture", dimnames = dn)
}

#' @export
format.dist_mixture <- function(x, width = getOption("width"), ...){
  dists <- unlist(lapply(x[["dist"]], format))

  dist_info <- paste0(x[["w"]], "*", dists, collapse = ", ")

  long_dist <- paste0("mixture(", dist_info, ")")
  short_dist <- paste0("mixture(n=", length(dists), ")")
  ifelse(nchar(long_dist) <= width, long_dist, short_dist)
}

#' @export
density.dist_mixture <- function(x, at, ...){
  if(NROW(at) > 1) return(vapply(at, density, numeric(1L), x = x, ...))
  sum(x[["w"]]*vapply(x[["dist"]], density, numeric(1L), at = at, ...))
}

#' @export
quantile.dist_mixture <- function(x, p, ...){
  d <- dim(x)
  if(d > 1)
    stop("quantile is not implemented for multivariate mixtures.")
  if(length(p) > 1) return(vapply(p, quantile, numeric(1L), x = x, ...))

  # Find bounds for optimisation based on range of each quantile
  dist_q <- vapply(x[["dist"]], quantile, numeric(1L), p, ..., USE.NAMES = FALSE)
  if(vctrs::vec_unique_count(dist_q) == 1) return(dist_q[1])
  if(p == 0) return(min(dist_q))
  if(p == 1) return(max(dist_q))

  # Search the cdf() for appropriate quantile
  stats::uniroot(
    function(pos) p - cdf(x, pos, ...),
    interval = c(min(dist_q), max(dist_q)),
    extendInt = "yes"
  )$root
}

#' @export
cdf.dist_mixture <- function(x, q, times = 1e5, ...){
  d <- dim(x)
  if(d == 1L) {
    if(length(q) > 1) return(vapply(q, cdf, numeric(1L), x = x, ...))
    sum(x[["w"]]*vapply(x[["dist"]], cdf, numeric(1L), q = q, ...))
  } else {
    NextMethod()
  }
}

#' @export
generate.dist_mixture <- function(x, times, ...){
  dist_idx <- .bincode(stats::runif(times), breaks = c(0, cumsum(x[["w"]])))
  r <- matrix(nrow = times, ncol = dim(x))
  for(i in seq_along(x[["dist"]])){
    r_pos <- dist_idx == i
    if(any(r_pos)) {
      r[r_pos,] <- generate(x[["dist"]][[i]], sum(r_pos), ...)
    }
  }
  r[,seq(NCOL(r)), drop = TRUE]
}

#' @export
mean.dist_mixture <- function(x, ...){
  d <- dim(x)
  m <- vapply(x[["dist"]], mean, numeric(d), ...)
  if(d == 1L) {
    sum(x[["w"]] * m)
  } else {
    matrix(x[["w"]], ncol = d, nrow = 1) %*% t(m)
  }
}

#' @export
covariance.dist_mixture <- function(x, ...){
  d <- dim(x)
  if(d == 1L) {
    m <- vapply(x[["dist"]], mean, numeric(1L), ...)
    v <- vapply(x[["dist"]], variance, numeric(1L), ...)
    m1 <- sum(x[["w"]]*m)
    m2 <- sum(x[["w"]]*(m^2 + v))
    m2 - m1^2
  } else {
    m <- lapply(x[["dist"]], mean)
    w <- as.list(x[["w"]])
    mbar <- mapply("*", m, w, SIMPLIFY = FALSE)
    mbar <- do.call("+", mbar)
    m <- lapply(m, function(u){u - mbar})
    v <- lapply(x[["dist"]], function(u){covariance(u)[[1]]})
    cov <- mapply(function(m,v,w) {w * ( t(m) %*% m + v ) }, m, v, w, SIMPLIFY = FALSE)
    list(do.call("+", cov))
  }
}

#' @export
dim.dist_mixture <- function(x){
  dim(x[["dist"]][[1]])
}


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/R/reexports.R ---
#' @importFrom generics generate
#' @export
generics::generate


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/R/support.R ---
#' Create a new support region vector
#'
#' @param x A list of prototype vectors defining the distribution type.
#' @param limits A list of value limits for the distribution.
#' @param closed A list of logical(2L) indicating whether the limits are closed.
#'
new_support_region <- function(x = numeric(), limits = list(), closed = list()) {
  vctrs::new_rcrd(list(x = x, lim = limits, closed = closed), class = "support_region")
}

#' @export
format.support_region <- function(x, digits = 3, ...) {
  type <- vapply(field(x, "x"), function(z) {
    out <- if(is.integer(z)) "Z"
    else if(is.numeric(z)) "R"
    else if(is.complex(z)) "C"
    else vec_ptype_abbr(z)
    if(is.matrix(z)) {
      if(ncol(z) > 1) {
        out <- paste(out, ncol(z), sep = "^")
      }
    }
    out
  }, FUN.VALUE = character(1L))
  brackets <- list(c("(","["), c(")","]"))
  mapply(function(type, z, closed) {
    br1 <- brackets[[1]][closed[1] + 1L]
    br2 <- brackets[[2]][closed[2] + 1L]
    fz <- sapply(z, function(x) format(x, digits = digits))
    fz <- gsub("3.14", "pi", fz, fixed = TRUE)
    if (any(is.na(z)) || all(is.infinite(z))) type
    else if (type == "Z") {
      if (identical(z, c(0L, Inf))) "N0"
      else if (identical(z, c(1L, Inf))) "N+"
      else paste0("{", z[1], ",", z[1]+1L, ",...,", z[2], "}")
    }
    else if (type == "R") paste0(br1, fz[1], ",", fz[2], br2)
    else type
  }, type, field(x, "lim"), field(x, "closed"))
}

#' @export
vec_ptype_abbr.support_region <- function(x, ...){
  "support"
}


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/R/transformed.R ---
#' Modify a distribution with a transformation
#'
#' @description
#' `r lifecycle::badge('maturing')`
#'
#' The [`density()`], [`mean()`], and [`variance()`] methods are approximate as
#' they are based on numerical derivatives.
#'
#' @param dist A univariate distribution vector.
#' @param transform A function used to transform the distribution. This
#' transformation should be monotonic over appropriate domain.
#' @param inverse The inverse of the `transform` function.
#'
#' @examples
#' # Create a log normal distribution
#' dist <- dist_transformed(dist_normal(0, 0.5), exp, log)
#' density(dist, 1) # dlnorm(1, 0, 0.5)
#' cdf(dist, 4) # plnorm(4, 0, 0.5)
#' quantile(dist, 0.1) # qlnorm(0.1, 0, 0.5)
#' generate(dist, 10) # rlnorm(10, 0, 0.5)
#'
#' @export
dist_transformed <- function(dist, transform, inverse){
  vec_is(dist, new_dist())
  if(is.function(transform)) transform <- list(transform)
  if(is.function(inverse)) inverse <- list(inverse)
  new_dist(dist = vec_data(dist),
           transform = transform, inverse = inverse,
           dimnames = dimnames(dist), class = "dist_transformed")
}

#' @export
format.dist_transformed <- function(x, ...){
  sprintf(
    "t(%s)",
    format(x[["dist"]])
  )
}

#' @export
support.dist_transformed <- function(x, ...) {
  support <- support(x[["dist"]])
  lim <- field(support, "lim")[[1]]
  lim <- suppressWarnings(x[['transform']](lim))
  if (all(!is.na(lim))) {
    lim <- sort(lim)
  }
  field(support, "lim")[[1]] <- lim
  support
}

#' @export
density.dist_transformed <- function(x, at, ...){
  inv <- function(v) suppressWarnings(x[["inverse"]](v))
  jacobian <- vapply(at, numDeriv::jacobian, numeric(1L), func = inv)
  d <- density(x[["dist"]], inv(at)) * abs(jacobian)
  limits <- field(support(x), "lim")[[1]]
  closed <- field(support(x), "closed")[[1]]
  if (!any(is.na(limits))) {
    `%less_than%` <- if (closed[1]) `<` else `<=`
    `%greater_than%` <- if (closed[2]) `>` else `>=`
    d[which(at %less_than% limits[1] | at %greater_than% limits[2])] <- 0
  }
  d
}

#' @export
cdf.dist_transformed <- function(x, q, ...){
  inv <- function(v) suppressWarnings(x[["inverse"]](v))
  p <- cdf(x[["dist"]], inv(q), ...)
  # TODO - remove null dist check when dist_na is structured correctly (revdep temp fix)
  if(!is.null(x[["dist"]]) && !monotonic_increasing(x[["transform"]], support(x[["dist"]]))) p <- 1 - p

  # TODO: Rework for support of closed limits and prevent computation
  x_sup <- support(x)
  x_lim <- field(x_sup, "lim")[[1]]
  x_cls <- field(x_sup, "closed")[[1]]
  if (!any(is.na(x_lim))) {
    p[q <= x_lim[1] & !x_cls[1]] <- 0
    p[q >= x_lim[2] & !x_cls[2]] <- 1
  }
  p
}

#' @export
quantile.dist_transformed <- function(x, p, ...){
  # TODO - remove null dist check when dist_na is structured correctly (revdep temp fix)
  if(!is.null(x[["dist"]]) && !monotonic_increasing(x[["transform"]], support(x[["dist"]]))) p <- 1 - p
  x[["transform"]](quantile(x[["dist"]], p, ...))
}

#' @export
generate.dist_transformed <- function(x, ...){
  x[["transform"]](generate(x[["dist"]], ...))
}

#' @export
mean.dist_transformed <- function(x, ...){
  mu <- mean(x[["dist"]])
  sigma2 <- variance(x[["dist"]])
  if(is.na(sigma2)){
    # warning("Could not compute the transformed distribution's mean as the base distribution's variance is unknown. The transformed distribution's median has been returned instead.")
    return(x[["transform"]](mu))
  }
  drop(
    x[["transform"]](mu) + numDeriv::hessian(x[["transform"]], mu, method.args=list(d = 0.01))/2*sigma2
  )
}

#' @export
covariance.dist_transformed <- function(x, ...){
  mu <- mean(x[["dist"]])
  sigma2 <- variance(x[["dist"]])
  if(is.na(sigma2)) return(NA_real_)
  drop(
    numDeriv::jacobian(x[["transform"]], mu)^2*sigma2 + (numDeriv::hessian(x[["transform"]], mu, method.args=list(d = 0.01))*sigma2)^2/2
  )
}

#' @method Math dist_transformed
#' @export
Math.dist_transformed <- function(x, ...) {
  trans <- new_function(exprs(x = ), body = expr((!!sym(.Generic))((!!x$transform)(x), !!!dots_list(...))))

  inverse_fun <- get_unary_inverse(.Generic)
  inverse <- new_function(exprs(x = ), body = expr((!!x$inverse)((!!inverse_fun)(x, !!!dots_list(...)))))

  vec_data(dist_transformed(wrap_dist(list(x[["dist"]])), trans, inverse))[[1]]
}

#' @method Ops dist_transformed
#' @export
Ops.dist_transformed <- function(e1, e2) {
  if(.Generic %in% c("-", "+") && missing(e2)){
    e2 <- e1
    e1 <- if(.Generic == "+") 1 else -1
    .Generic <- "*"
  }
  is_dist <- c(inherits(e1, "dist_default"), inherits(e2, "dist_default"))
  trans <- if(all(is_dist)) {
    if(identical(e1$dist, e2$dist)){
      new_function(exprs(x = ), expr((!!sym(.Generic))((!!e1$transform)(x), (!!e2$transform)(x))))
    } else {
      stop(sprintf("The %s operation is not supported for <%s> and <%s>", .Generic, class(e1)[1], class(e2)[1]))
    }
  } else if(is_dist[1]){
    new_function(exprs(x = ), body = expr((!!sym(.Generic))((!!e1$transform)(x), !!e2)))
  } else {
    new_function(exprs(x = ), body = expr((!!sym(.Generic))(!!e1, (!!e2$transform)(x))))
  }

  inverse <- if(all(is_dist)) {
    invert_fail
  } else if(is_dist[1]){
    inverse_fun <- get_binary_inverse_1(.Generic, e2)
    new_function(exprs(x = ), body = expr((!!e1$inverse)((!!inverse_fun)(x))))
  } else {
    inverse_fun <- get_binary_inverse_2(.Generic, e1)
    new_function(exprs(x = ), body = expr((!!e2$inverse)((!!inverse_fun)(x))))
  }

  vec_data(dist_transformed(wrap_dist(list(list(e1,e2)[[which(is_dist)[1]]][["dist"]])), trans, inverse))[[1]]
}

monotonic_increasing <- function(f, support) {
  # Shortcut for identity function (used widely in ggdist)
  if(!is.primitive(f) && identical(body(f), as.name(names(formals(f))))) {
    return(TRUE)
  }

  # Currently assumes (without checking, #9) monotonicity of f over the domain
  x <- f(field(support, "lim")[[1]])
  isTRUE(x[[2L]] >= x[[1L]])
}


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/R/truncated.R ---
#' Truncate a distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' Note that the samples are generated using inverse transform sampling, and the
#' means and variances are estimated from samples.
#'
#' @param dist The distribution(s) to truncate.
#' @param lower,upper The range of values to keep from a distribution.
#'
#' @name dist_truncated
#'
#' @examples
#' dist <- dist_truncated(dist_normal(2,1), lower = 0)
#'
#' dist
#' mean(dist)
#' variance(dist)
#'
#' generate(dist, 10)
#'
#' density(dist, 2)
#' density(dist, 2, log = TRUE)
#'
#' cdf(dist, 4)
#'
#' quantile(dist, 0.7)
#'
#' if(requireNamespace("ggdist")) {
#' library(ggplot2)
#' ggplot() +
#'   ggdist::stat_dist_halfeye(
#'     aes(y = c("Normal", "Truncated"),
#'         dist = c(dist_normal(2,1), dist_truncated(dist_normal(2,1), lower = 0)))
#'   )
#' }
#'
#' @export
dist_truncated <- function(dist, lower = -Inf, upper = Inf){
  vec_is(dist, new_dist())
  vec_is(lower, numeric())
  vec_is(upper, numeric())
  if(any(lower >= upper)){
    abort("The `lower` truncation bound must be lower than the `upper` bound.")
  }
  new_dist(dist = dist, lower = lower, upper = upper,
           dimnames = dimnames(dist), class = "dist_truncated")
}

#' @export
format.dist_truncated <- function(x, ...){
  sprintf(
    "%s[%s,%s]",
    format(x[["dist"]]),
    x[["lower"]],
    x[["upper"]]
  )
}

#' @export
density.dist_truncated <- function(x, at, ...){
  in_lim <- at >= x[["lower"]] & at <= x[["upper"]]
  cdf_upr <- cdf(x[["dist"]], x[["upper"]])
  cdf_lwr <- cdf(x[["dist"]], x[["lower"]])
  out <- numeric(length(at))
  out[in_lim] <- density(x[["dist"]], at = at[in_lim], ...)/(cdf_upr - cdf_lwr)
  out
}

#' @export
quantile.dist_truncated <- function(x, p, ...){
  F_lwr <- cdf(x[["dist"]], x[["lower"]])
  F_upr <- cdf(x[["dist"]], x[["upper"]])
  qt <- quantile(x[["dist"]], F_lwr + p * (F_upr - F_lwr), ...)
  pmin(pmax(x[["lower"]], qt), x[["upper"]])
}

#' @export
cdf.dist_truncated <- function(x, q, ...){
  cdf_upr <- cdf(x[["dist"]], x[["upper"]])
  cdf_lwr <- cdf(x[["dist"]], x[["lower"]])
  out <- numeric(length(q))
  q_lwr <- q < x[["lower"]] # out[q_lwr <- q < x[["lower"]]] <- 0
  out[q_upr <- q > x[["upper"]]] <- 1
  q_mid <- !(q_lwr|q_upr)
  out[q_mid] <- (cdf(x[["dist"]], q = q[q_mid], ...) - cdf_lwr)/(cdf_upr - cdf_lwr)
  out
}

#' @export
mean.dist_truncated <- function(x, ...) {
  if(inherits(x$dist, "dist_sample")) {
    y <- x$dist[[1]]
    mean(y[y >= x$lower & y <= x$upper])
  } else if(inherits(x$dist, "dist_normal")) {
    mu <- x$dist$mu
    s <- x$dist$sigma
    a <- (x$lower - mu) / s
    b <- (x$upper - mu) / s
    mu + (stats::dnorm(a) - stats::dnorm(b))/(stats::pnorm(b) - stats::pnorm(a))*s
  } else {
    NextMethod()
  }
}


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/R/utils.R ---
transpose <- function(.l) {
  if(is_empty(.l)) return(.l)
  inner_names <- names(.l[[1L]])
  result <- lapply(seq_along(.l[[1L]]), function(i) {
    lapply(.l, .subset2, i)
  })
  set_names(result, inner_names)
}

transpose_c <- function(.l) {
  stopifnot(is_list_of(.l))
  .ptype <- vec_init(attr(.l, "ptype"), 1L)
  if(is_empty(.l)) return(.l)
  inner_names <- names(.l[[1L]])
  .l <- vec_recycle_common(!!!.l)
  result <- lapply(seq_along(.l[[1L]]), function(i) {
    unname(vec_c(!!!lapply(.l, vec_slice, i), .ptype = .ptype))
  })
  set_names(result, inner_names)
}

split_matrix_rows <- function(x) {
  lapply(seq_len(nrow(x)), function(i) x[i,,drop=FALSE])
}

# Declare a function's argument as allowing list inputs for mapping values
arg_listable <- function(x, .ptype) {
  if(is.list(x)) {
    x <- as_list_of(as.list(x), .ptype)
    if(is.matrix(attr(x, "ptype"))) {
      x <- lapply(x, split_matrix_rows)
      x <- as_list_of(x, .ptype)
    }
    if(is.null(names(x))) {
      names(x) <- vec_as_names(character(vec_size(x)), repair = "unique")
    }
  } else if(is.matrix(x)) {
    x <- split_matrix_rows(x)
  } else {
    vec_assert(x, .ptype)
  }
  # Declares list arguments to be unpacked for dist_apply()
  class(x) <- c("arg_listable", class(x))
  x
}

validate_recycling <- function(x, arg) {
  if(is_list_of(arg)) return(lapply(arg, validate_recycling, x = x))
  if(!any(vec_size(arg) == c(1, vec_size(x)))) {
    abort(
      sprintf("Cannot recycle input of size %i to match the distributions (size %i).",
              vec_size(arg), vec_size(x)
      )
    )
  }
}

dist_apply <- function(x, .f, ...){
  dn <- dimnames(x)
  x <- vec_data(x)
  dist_is_na <- vapply(x, is.null, logical(1L))
  x[dist_is_na] <- list(structure(list(), class = c("dist_na", "dist_default")))

  args <- dots_list(...)
  is_arg_listable <- vapply(args, inherits, FUN.VALUE = logical(1L), "arg_listable")
  unpack_listable <- multi_arg <- FALSE
  if(any(is_arg_listable)) {
    if(sum(is_arg_listable) > 1) abort("Only distribution argument can be unpacked at a time.\nThis shouldn't happen, please report a bug at https://github.com/mitchelloharawild/distributional/issues/")
    arg_pos <- which(is_arg_listable)

    if(unpack_listable <- is_list_of(args[[arg_pos]])) {
      validate_recycling(x, args[[arg_pos]])
      .unpack_names <- names(args[[arg_pos]])
      args[[arg_pos]] <- transpose_c(args[[arg_pos]])
    } else if (multi_arg <- (length(args[[arg_pos]]) > 1)){
      args[[arg_pos]] <- list(unclass(args[[arg_pos]]))
    }
  }

  out <- do.call(mapply, c(.f, list(x), args, SIMPLIFY = FALSE, USE.NAMES = FALSE))
  # out <- mapply(.f, x, ..., SIMPLIFY = FALSE, USE.NAMES = FALSE)

  if(unpack_listable) {
    # TODO - update and repair multivariate distribution i/o with unpacking
    out <- as_list_of(out)
    if (rbind_mat <- is.matrix(attr(out, "ptype"))) {
      out <- as_list_of(lapply(out, split_matrix_rows))
    }
    out <- transpose_c(out)
    if(rbind_mat) {
      out <- lapply(out, function(x) `colnames<-`(do.call(rbind, x), dn))
    }
    names(out) <- .unpack_names
    out <- new_data_frame(out, n = vec_size(x))
  # } else if(length(out[[1]]) > 1) {
  #   out <- suppressMessages(vctrs::vec_rbind(!!!out))
  } else if(multi_arg) {
    if(length(dn) > 1) out <- lapply(out, `colnames<-`, dn)
  } else {
    out <- vec_c(!!!out)
    out <- if(vec_is_list(out))
      lapply(out, set_matrix_dimnames, dn = dn)
    else
      set_matrix_dimnames(out, dn)
  }
  out
}

set_matrix_dimnames <- function(x, dn) {
  if((is.matrix(x) || is.data.frame(x)) && !is.null(dn)){
    # Set dimension names
    colnames(x) <- dn
  }
  x
}

# inlined from https://github.com/r-lib/cli/blob/master/R/utf8.R
is_utf8_output <- function() {
  opt <- getOption("cli.unicode", NULL)
  if (!is_null(opt)) {
    isTRUE(opt)
  } else {
    l10n_info()$`UTF-8` && !is_latex_output()
  }
}

is_latex_output <- function() {
  if (!("knitr" %in% loadedNamespaces())) {
    return(FALSE)
  }
  get("is_latex_output", asNamespace("knitr"))()
}

require_package <- function(pkg){
  if(!requireNamespace(pkg, quietly = TRUE)){
    abort(
      sprintf('The `%s` package must be installed to use this functionality. It can be installed with install.packages("%s")', pkg, pkg)
    )
  }
}

restore_rng <- function(expr, seed = NULL) {
  old_seed <- .GlobalEnv$.Random.seed
  # Set new temporary seed
  set.seed(seed)
  # Restore previous seed
  on.exit(.GlobalEnv$.Random.seed <- old_seed)

  expr
}

near <- function(x, y) {
  tol <- .Machine$double.eps^0.5
  abs(x - y) < tol
}


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/tests/testthat.R ---
library(testthat)
library(distributional)

test_check("distributional")


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/tests/testthat/setup-tests.R ---
context("Configure test options")
options(cli.unicode = FALSE)


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/tests/testthat/test-apply.R ---
test_that("Recycling rules and output for applying multiple inputs over multiple univariate distributions", {
  # <dist> is a distribution vector of length 10
  dist <- dist_named <- dist_normal(1:10, 1:10)
  dimnames(dist_named) <- "name"

  # p = 0.5:          apply p across all elements of <dist> (recycling p onto <dist>)
  #                   Returns a vector of length 10
  expect_equal(
    quantile(dist, 0.5),
    qnorm(0.5, 1:10, 1:10)
  )
  expect_equal(
    quantile(dist_named, 0.5),
    qnorm(0.5, 1:10, 1:10)
  )

  # p = c(0.5, 0.9):  Cannot recycle p (length 2) onto <dist> (length 10)
  #                   Returns a list containing values for each p.
  expect_equal(
    quantile(dist, c(0.5, 0.9)),
    mapply({function(mean, sd) qnorm(c(0.5, 0.9), mean, sd)},
           mean = 1:10, sd = 1:10, SIMPLIFY = FALSE)
  )
  expect_equal(
    quantile(dist_named, c(0.5, 0.9)),
    mapply({function(mean, sd) qnorm(c(0.5, 0.9), mean, sd)},
           mean = 1:10, sd = 1:10, SIMPLIFY = FALSE)
  )

  # p = ppoints(10):  apply each p to each element of <dist> (no recycling)
  #                   Returns a list for each distribution with the 10 quantiles.
  expect_equal(
    quantile(dist, ppoints(10)),
    mapply({function(mean, sd) qnorm(ppoints(10), mean, sd)},
           mean = 1:10, sd = 1:10, SIMPLIFY = FALSE)
  )
  expect_equal(
    quantile(dist_named, ppoints(10)),
    mapply({function(mean, sd) qnorm(ppoints(10), mean, sd)},
           mean = 1:10, sd = 1:10, SIMPLIFY = FALSE)
  )

  # p = list(0.5):    equivalent to p = 0.5, but returns a list output
  #                   Returns a tibble with 1 vector column of length 10
  expect_equal(
    quantile(dist, list(a = 0.5)),
    new_data_frame(list(a = quantile(dist, 0.5)))
  )
  expect_equal(
    quantile(dist_named, list(a = 0.5)),
    new_data_frame(list(a = quantile(dist, 0.5)))
  )

  # p = list(c(0.5, 0.9)):
  #                   Cannot recycle p[[1]] (length 2) onto <dist> (length 10)
  #                   Returns an error.
  expect_error(
    quantile(dist, list(a = c(0.5, 0.9))),
    "Cannot recycle input"
  )
  expect_error(
    quantile(dist_named, list(a = c(0.5, 0.9))),
    "Cannot recycle input"
  )

  # p = list(p1, 0.5): equivalent to df(quantile(<dist>, p1), quantile(<dist>, 0.5)).
  #                   Returns a tibble with 2 vector columns of length 10
  #                   Names of p are used in output.
  expect_equal(
    quantile(dist, list(a=ppoints(10), b=0.5)),
    new_data_frame(list(a = qnorm(ppoints(10), 1:10, 1:10), b = quantile(dist, 0.5)))
  )
  expect_equal(
    quantile(dist_named, list(a=ppoints(10), b=0.5)),
    new_data_frame(list(a = qnorm(ppoints(10), 1:10, 1:10), b = quantile(dist, 0.5)))
  )
})

test_that("Recycling rules and output for applying multiple inputs over multiple multivariate distributions", {
  # <dist> is a bivariate distribution vector of length 2
  dist <- dist_named <- dist_multivariate_normal(mu = list(c(1,2), c(3,5)),
                                   sigma = list(matrix(c(4,2,2,3), ncol=2),
                                                matrix(c(5,1,1,4), ncol=2)))
  dimnames(dist_named) <- c("a", "b")

  expect_equal(
    quantile(dist, 0.5, type = "marginal"),
    matrix(c(1,3,2,5), nrow = 2)
  )
  expect_equal(
    quantile(dist_named, 0.5, type = "marginal"),
    matrix(c(1,3,2,5), nrow = 2, dimnames = list(NULL, c("a", "b")))
  )

  expect_equal(
    quantile(dist, c(0.5, 0.9), type = "marginal"),
    list(matrix(c(1, 3.5631031310892, 2, 4.21971242404268), ncol = 2),
         matrix(c(3, 5.86563641722901, 5, 7.5631031310892), ncol = 2))
  )
  expect_equal(
    quantile(dist_named, c(0.5, 0.9), type = "marginal"),
    list(matrix(c(1, 3.5631031310892, 2, 4.21971242404268), ncol = 2, dimnames = list(NULL, c("a", "b"))),
         matrix(c(3, 5.86563641722901, 5, 7.5631031310892), ncol = 2, dimnames = list(NULL, c("a", "b"))))
  )

  expect_equal(
    quantile(dist, c(0.5, 0.9, 0.95), type = "marginal"),
    list(
      matrix(c(1, 3.5631031310892, 4.28970725390294, 2, 4.21971242404268, 4.84897005289389),
             ncol = 2),
      matrix(c(3, 5.86563641722901, 6.67800452290057, 5, 7.5631031310892, 8.28970725390294),
             ncol = 2)
    )
  )
  expect_equal(
    quantile(dist_named, c(0.5, 0.9, 0.95), type = "marginal"),
    list(
      matrix(c(1, 3.5631031310892, 4.28970725390294, 2, 4.21971242404268, 4.84897005289389),
             ncol = 2, dimnames = list(NULL, c("a","b"))),
      matrix(c(3, 5.86563641722901, 6.67800452290057, 5, 7.5631031310892, 8.28970725390294),
             ncol = 2, dimnames = list(NULL, c("a", "b")))
    )
  )

  expect_equal(
    quantile(dist, list(single = 0.5, varied = c(0.8, 0.3))),
    new_data_frame(
      list(single = quantile(dist, 0.5), varied = rbind(quantile(dist[1], 0.8), quantile(dist[2], 0.3)))
    )
  )
  expect_equal(
    quantile(dist_named, list(single = 0.5, varied = c(0.8, 0.3))),
    new_data_frame(
      list(single = quantile(dist_named, 0.5), varied = rbind(quantile(dist_named[1], 0.8), quantile(dist_named[2], 0.3)))
    )
  )

  skip_if_not_installed("mvtnorm")
  expect_equal(
    density(dist, cbind(2, 3)),
    c(0.046649277604197, 0.0215708514518913)
  )
  expect_equal(
    density(dist_named, cbind(2, 3)),
    c(0.046649277604197, 0.0215708514518913)
  )

  expect_equal(
    mean(dist),
    matrix(c(1,3,2,5), nrow = 2)
  )
  expect_equal(
    mean(dist_named),
    matrix(c(1,3,2,5), nrow = 2, dimnames = list(NULL, c("a", "b")))
  )

  expect_equal(
    covariance(dist),
    list(
      matrix(c(4,2,2,3), nrow = 2),
      matrix(c(5,1,1,4), nrow = 2)
    )
  )
  expect_equal(
    covariance(dist_named),
    list(
      matrix(c(4,2,2,3), nrow = 2, dimnames = list(NULL, c("a", "b"))),
      matrix(c(5,1,1,4), nrow = 2, dimnames = list(NULL, c("a", "b")))
    )
  )
})


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/tests/testthat/test-dist-bernoulli.R ---
test_that("Bernoulli distribution", {
  dist <- dist_bernoulli(0.4)

  expect_equal(format(dist), "Bernoulli(0.4)")

  # quantiles
  expect_equal(quantile(dist, 0.6), FALSE)
  expect_equal(quantile(dist, 0.61), TRUE)

  # pdf
  expect_equal(density(dist, 0), stats::dbinom(0, 1, 0.4))
  expect_equal(density(dist, 1), stats::dbinom(1, 1, 0.4))

  # cdf
  expect_equal(cdf(dist, 0), stats::pbinom(0, 1, 0.4))
  expect_equal(cdf(dist, 1), stats::pbinom(1, 1, 0.4))

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.6)), 0.6, tolerance = 1e-3)

  # stats
  expect_equal(mean(dist), 0.4)
  expect_equal(variance(dist), 0.4*(1-0.4))
})


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/tests/testthat/test-dist-beta.R ---
test_that("Beta distribution", {
  dist <- dist_beta(3, 4)

  expect_equal(format(dist), "Beta(3, 4)")

  # quantiles
  expect_equal(quantile(dist, 0.1), qbeta(0.1, 3, 4))
  expect_equal(quantile(dist, 0.5), qbeta(0.5, 3, 4))

  # pdf
  expect_equal(density(dist, 0), dbeta(0, 3, 4))
  expect_equal(density(dist, 3), dbeta(3, 3, 4))

  # cdf
  expect_equal(cdf(dist, 0), pbeta(0, 3, 4))
  expect_equal(cdf(dist, 3), pbeta(3, 3, 4))

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.4)), 0.4, tolerance = 1e-3)

  # stats
  expect_equal(mean(dist), 3/(3+4))
  expect_equal(variance(dist), 3*4/((3+4)^2*(3+4+1)))
})


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/tests/testthat/test-dist-burr.R ---
test_that("Burr distribution", {

  dist <- dist_burr(2, 3)

  expect_equal(format(dist), "Burr12(2, 3, 1)")

  # Require package installed
  skip_if_not_installed("actuar", "2.0.0")

  # quantiles
  expect_equal(quantile(dist, 0.1), actuar::qburr(0.1, 2, 3))
  expect_equal(quantile(dist, 0.5), actuar::qburr(0.5, 2, 3))

  # pdf
  expect_equal(density(dist, 0), actuar::dburr(0, 2, 3))
  expect_equal(density(dist, 3), actuar::dburr(3, 2, 3))

  # cdf
  expect_equal(cdf(dist, 0), actuar::pburr(0, 2, 3))
  expect_equal(cdf(dist, 3), actuar::pburr(3, 2, 3))

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.4)), 0.4, tolerance = 1e-3)

  # stats
  expect_equal(mean(dist), actuar::mburr(1, 2, 3))
  expect_equal(variance(dist), actuar::mburr(2, 2, 3) - actuar::mburr(1, 2, 3)^2)
})


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/tests/testthat/test-dist-categorical.R ---
test_that("Categorical distribution", {
  dist <- dist_categorical(list(c(0.4, 0.2, 0.3, 0.1)))

  expect_equal(format(dist), "Categorical[4]")

  # quantiles
  expect_true(all(is.na(quantile(dist, 0.5))))
  expect_true(all(is.na(quantile(dist, 0.2))))
  expect_equal(quantile(dist, c(0.1, 0.9)), list(c(NA_real_, NA_real_)))

  # pdf
  expect_equal(density(dist, -1), NA_real_)
  expect_equal(density(dist, 0), NA_real_)
  expect_equal(density(dist, 1), 0.4)
  expect_equal(density(dist, 2), 0.2)
  expect_equal(density(dist, 5), NA_real_)
  expect_equal(density(dist, 3:5), list(c(0.3, 0.1, NA_real_)))

  # cdf
  expect_true(all(is.na(cdf(dist, 1))))
  expect_equal(cdf(dist, 1:2), list(c(NA_real_, NA_real_)))

  # stats
  expect_true(all(is.na(mean(dist))))
  expect_true(all(is.na(variance(dist))))
})


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/tests/testthat/test-dist-cauchy.R ---
test_that("Cauchy distribution", {
  dist <- dist_cauchy(-2, 1)

  expect_equal(format(dist), "Cauchy(-2, 1)")

  # quantiles
  expect_equal(quantile(dist, 0.1), stats::qcauchy(0.1, -2, 1))
  expect_equal(quantile(dist, 0.5), stats::qcauchy(0.5, -2, 1))

  # pdf
  expect_equal(density(dist, 0), stats::dcauchy(0, -2, 1))
  expect_equal(density(dist, 3), stats::dcauchy(3, -2, 1))

  # cdf
  expect_equal(cdf(dist, 0), stats::pcauchy(0, -2, 1))
  expect_equal(cdf(dist, 3), stats::pcauchy(3, -2, 1))

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.4246)), 0.4246, tolerance = 1e-3)

  # stats
  expect_equal(mean(dist), NA_real_)
  expect_equal(variance(dist), NA_real_)
})


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/tests/testthat/test-dist-chisq.R ---
test_that("Chisq distribution", {
  dist <- dist_chisq(9)

  expect_equal(format(dist), "x2(9)")

  # quantiles
  expect_equal(quantile(dist, 0.1), stats::qchisq(0.1, 9))
  expect_equal(quantile(dist, 0.5), stats::qchisq(0.5, 9))

  # pdf
  expect_equal(density(dist, 0), stats::dchisq(0, 9))
  expect_equal(density(dist, 3), stats::dchisq(3, 9))

  # cdf
  expect_equal(cdf(dist, 0), stats::pchisq(0, 9))
  expect_equal(cdf(dist, 3), stats::pchisq(3, 9))

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.4246)), 0.4246, tolerance = 1e-3)

  # stats
  expect_equal(mean(dist), 9)
  expect_equal(variance(dist), 2*9)
})


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/tests/testthat/test-dist-degenerate.R ---
test_that("Degenerate distribution", {
  dist <- dist_degenerate(1)

  expect_equal(
    dist,
    vec_cast(1, new_dist())
  )

  expect_equal(format(dist), "1")

  # quantiles
  expect_equal(quantile(dist, 0), 1)
  expect_equal(quantile(dist, 0.5), 1)
  expect_equal(quantile(dist, 1), 1)

  # pdf
  expect_equal(density(dist, 1), 1)
  expect_equal(density(dist, 0.5), 0)
  expect_equal(density(dist, 0.99999), 0)

  # cdf
  expect_equal(cdf(dist, 0), 0)
  expect_equal(cdf(dist, 1), 1)
  expect_equal(cdf(dist, 0.9999), 0)

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 1)), 1, tolerance = 1e-3)
  expect_equal(cdf(dist, quantile(dist, 0)), 1, tolerance = 1e-3)

  # stats
  expect_equal(mean(dist), 1)
  expect_equal(variance(dist), 0)
})


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/tests/testthat/test-dist-exponential.R ---
test_that("Exponential distribution", {
  dist <- dist_exponential(2)

  expect_equal(format(dist), "Exp(2)")

  # quantiles
  expect_equal(quantile(dist, 0.1), stats::qexp(0.1, 2))
  expect_equal(quantile(dist, 0.5), stats::qexp(0.5, 2))

  # pdf
  expect_equal(density(dist, 0), stats::dexp(0, 2))
  expect_equal(density(dist, 3), stats::dexp(3, 2))

  # cdf
  expect_equal(cdf(dist, 0), stats::pexp(0, 2))
  expect_equal(cdf(dist, 3), stats::pexp(3, 2))

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.4246)), 0.4246, tolerance = 1e-3)

  # stats
  expect_equal(mean(dist), 1/2)
  expect_equal(variance(dist), 1/2^2)
})


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/tests/testthat/test-dist-f.R ---
test_that("F distribution", {
  dist <- dist_f(5, 2)

  expect_equal(format(dist), "F(5, 2)")

  # quantiles
  expect_equal(quantile(dist, 0.1), stats::qf(0.1, 5, 2))
  expect_equal(quantile(dist, 0.5), stats::qf(0.5, 5, 2))

  # pdf
  expect_equal(density(dist, 0), stats::df(0, 5, 2))
  expect_equal(density(dist, 3), stats::df(3, 5, 2))

  # cdf
  expect_equal(cdf(dist, 0), stats::pf(0, 5, 2))
  expect_equal(cdf(dist, 3), stats::pf(3, 5, 2))

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.4246)), 0.4246, tolerance = 1e-3)

  # stats
  expect_equal(mean(dist), NA_real_)
  expect_equal(variance(dist), NA_real_)
  dist <- dist_f(5, 5)
  expect_equal(mean(dist), 5/(5-2))
  expect_equal(variance(dist), 2*5^2*(5+5-2)/(5*(5-2)^2*(5-4)))
})


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/tests/testthat/test-dist-gamma.R ---
test_that("Gamma distribution", {
  dist <- dist_gamma(7.5, 2)

  expect_equal(format(dist), "Gamma(7.5, 2)")

  # quantiles
  expect_equal(quantile(dist, 0.1), stats::qgamma(0.1, 7.5, 2))
  expect_equal(quantile(dist, 0.5), stats::qgamma(0.5, 7.5, 2))

  # pdf
  expect_equal(density(dist, 0), stats::dgamma(0, 7.5, 2))
  expect_equal(density(dist, 3), stats::dgamma(3, 7.5, 2))

  # cdf
  expect_equal(cdf(dist, 0), stats::pgamma(0, 7.5, 2))
  expect_equal(cdf(dist, 3), stats::pgamma(3, 7.5, 2))

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.4246)), 0.4246, tolerance = 1e-3)

  # stats
  expect_equal(mean(dist), 7.5/2)
  expect_equal(variance(dist), 7.5/2^2)
})


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/tests/testthat/test-dist-geometric.R ---
test_that("Geometric distribution", {
  dist <- dist_geometric(0.4)

  expect_equal(format(dist), "Geometric(0.4)")

  # quantiles
  expect_equal(quantile(dist, 0.6), stats::qgeom(0.6, 0.4))
  expect_equal(quantile(dist, 0.9), stats::qgeom(0.9, 0.4))

  # pdf
  expect_equal(density(dist, 0), stats::dgeom(0, 0.4))
  expect_equal(density(dist, 5), stats::dgeom(5, 0.4))

  # cdf
  expect_equal(cdf(dist, 0), stats::pgeom(0, 0.4))
  expect_equal(cdf(dist, 10), stats::pgeom(10, 0.4))

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.64)), 0.64, tolerance = 1e-3)

  # stats
  expect_equal(mean(dist), 1/0.4 - 1)
  expect_equal(variance(dist), 1/0.4^2 - 1/0.4)
})


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/tests/testthat/test-dist-gh.R ---
test_that("g-and-h distribution", {
  # Define g-and-h distribution parameters
  A <- 0
  B <- 1
  g <- 0
  h <- 0.5
  c <- 0.8
  dist <- dist_gh(A, B, g, h, c)

  # Check formatting
  expect_equal(format(dist), "gh(A = 0, B = 1, g = 0, h = 0.5)")

  # Require package installed
  skip_if_not_installed("gk", "0.1.0")

  # quantiles
  expect_equal(quantile(dist, 0.1), gk::qgh(0.1, A, B, g, h, c))
  expect_equal(quantile(dist, 0.5), gk::qgh(0.5, A, B, g, h, c))

  # pdf
  expect_equal(density(dist, 0), gk::dgh(0, A, B, g, h, c))
  expect_equal(density(dist, 3), gk::dgh(3, A, B, g, h, c))

  # cdf
  expect_equal(cdf(dist, 0), gk::pgh(0, A, B, g, h, c))
  expect_equal(cdf(dist, 3), gk::pgh(3, A, B, g, h, c))

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.4)), 0.4, tolerance = 1e-3)

  # Generate random samples
  set.seed(123)
  samples <- generate(dist, 10)
  set.seed(123)
  expect_equal(samples[[1L]], gk::rgh(10, A, B, g, h, c))
})


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/tests/testthat/test-dist-gk.R ---
test_that("g-k distribution", {
  # Define g-k distribution parameters
  A <- 0
  B <- 1
  g <- 0
  k <- 0.5
  c <- 0.8
  dist <- dist_gk(A, B, g, k, c)

  # Check formatting
  expect_equal(format(dist), "gk(A = 0, B = 1, g = 0, k = 0.5)")

  # Require package installed
  skip_if_not_installed("gk", "0.1.0")

  # quantiles
  expect_equal(quantile(dist, 0.1), gk::qgk(0.1, A, B, g, k, c))
  expect_equal(quantile(dist, 0.5), gk::qgk(0.5, A, B, g, k, c))

  # pdf
  expect_equal(density(dist, 0), gk::dgk(0, A, B, g, k, c))
  expect_equal(density(dist, 3), gk::dgk(3, A, B, g, k, c))

  # cdf
  expect_equal(cdf(dist, 0), gk::pgk(0, A, B, g, k, c))
  expect_equal(cdf(dist, 3), gk::pgk(3, A, B, g, k, c))

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.4)), 0.4, tolerance = 1e-3)

  # Generate random samples
  set.seed(123)
  samples <- generate(dist, 10)
  set.seed(123)
  expect_equal(samples[[1L]], gk::rgk(10, A, B, g, k, c))
})


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/tests/testthat/test-dist-gumbel.R ---
test_that("Gumbel distribution", {
  dist <- dist_gumbel(1, 2)

  expect_equal(format(dist), "Gumbel(1, 2)")

  # Require package installed
  skip_if_not_installed("actuar", "2.0.0")

  # quantiles
  expect_equal(quantile(dist, 0.1), actuar::qgumbel(0.1, 1, 2))
  expect_equal(quantile(dist, 0.5), actuar::qgumbel(0.5, 1, 2))

  # pdf
  expect_equal(density(dist, 0), actuar::dgumbel(0, 1, 2))
  expect_equal(density(dist, 3), actuar::dgumbel(3, 1, 2))

  # cdf
  expect_equal(cdf(dist, 0), actuar::pgumbel(0, 1, 2))
  expect_equal(cdf(dist, 3), actuar::pgumbel(3, 1, 2))

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.4)), 0.4, tolerance = 1e-3)

  # stats
  expect_equal(mean(dist), actuar::mgumbel(1, 1, 2))
  expect_equal(variance(dist), actuar::mgumbel(2, 1, 2) - actuar::mgumbel(1, 1, 2)^2)
})


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/tests/testthat/test-dist-hypergeometric.R ---
test_that("Hypergeometric distribution", {
  dist <- dist_hypergeometric(500, 50, 100)

  expect_equal(format(dist), "Hypergeometric(500, 50, 100)")

  # quantiles
  expect_equal(quantile(dist, 0.1), stats::qhyper(0.1, 500, 50, 100))
  expect_equal(quantile(dist, 0.5), stats::qhyper(0.5, 500, 50, 100))

  # pdf
  expect_equal(density(dist, 0), stats::dhyper(0, 500, 50, 100))
  expect_equal(density(dist, 3), stats::dhyper(3, 500, 50, 100))

  # cdf
  expect_equal(cdf(dist, 0), stats::phyper(0, 500, 50, 100))
  expect_equal(cdf(dist, 3), stats::phyper(3, 500, 50, 100))

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.4246)), 0.4246, tolerance = 1e-3)

  # stats
  p <- 500/(500+50)
  expect_equal(mean(dist), 100*p)
  expect_equal(variance(dist), 100*p*(1-p)*(500+50-100)/(500+50-1))
})


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/tests/testthat/test-dist-inverse-exponential.R ---
test_that("Inverse Exponential distribution", {
  dist <- dist_inverse_exponential(5)

  expect_equal(format(dist), "InvExp(5)")

  # Require package installed
  skip_if_not_installed("actuar", "2.0.0")

  # quantiles
  expect_equal(quantile(dist, 0.1), actuar::qinvexp(0.1, 5))
  expect_equal(quantile(dist, 0.5), actuar::qinvexp(0.5, 5))

  # pdf
  expect_equal(density(dist, 0), actuar::dinvexp(0, 5))
  expect_equal(density(dist, 3), actuar::dinvexp(3, 5))

  # cdf
  expect_equal(cdf(dist, 0), actuar::pinvexp(0, 5))
  expect_equal(cdf(dist, 3), actuar::pinvexp(3, 5))

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.4)), 0.4, tolerance = 1e-3)

  # stats
  expect_equal(mean(dist), NA_real_) # dput(actuar::minvexp(1, 5))
  expect_equal(variance(dist), NA_real_) # dput(actuar::minvexp(2, 5) - actuar::minvexp(1, 5)^2)
})


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/tests/testthat/test-dist-inverse-gamma.R ---
test_that("Inverse Gamma distribution", {
  dist <- dist_inverse_gamma(3, 2)

  expect_equal(format(dist), "InvGamma(3, 0.5)")

  # Require package installed
  skip_if_not_installed("actuar", "2.0.0")

  # quantiles
  expect_equal(quantile(dist, 0.1), actuar::qinvgamma(0.1, 3, 2))
  expect_equal(quantile(dist, 0.5), actuar::qinvgamma(0.5, 3, 2))

  # pdf
  expect_equal(density(dist, 0), actuar::dinvgamma(0, 3, 2))
  expect_equal(density(dist, 3), actuar::dinvgamma(3, 3, 2))

  # cdf
  expect_equal(cdf(dist, 0), actuar::pinvgamma(0, 3, 2))
  expect_equal(cdf(dist, 3), actuar::pinvgamma(3, 3, 2))

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.4)), 0.4, tolerance = 1e-3)

  # stats
  expect_equal(mean(dist), (1/2) / (3 - 1))
  expect_equal(median(dist), actuar::qinvgamma(0.5, 3, 2))
  expect_equal(median(dist[[1]]), actuar::qinvgamma(0.5, 3, 2))
  expect_equal(variance(dist), (1/2)^2/((3-1)^2*(3-2)))
})


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/tests/testthat/test-dist-inverse-gaussian.R ---
test_that("Inverse Gaussian distribution", {
  dist <- dist_inverse_gaussian(3, .2)

  expect_equal(format(dist), "IG(3, 0.2)")

  # Require package installed
  skip_if_not_installed("actuar", "2.0.0")

  # quantiles
  expect_equal(quantile(dist, 0.1), actuar::qinvgauss(0.1, 3, .2))
  expect_equal(quantile(dist, 0.5), actuar::qinvgauss(0.5, 3, .2))

  # pdf
  expect_equal(density(dist, 0), actuar::dinvgauss(0, 3, .2))
  expect_equal(density(dist, 3), actuar::dinvgauss(3, 3, .2))

  # cdf
  expect_equal(cdf(dist, 0), actuar::pinvgauss(0, 3, .2))
  expect_equal(cdf(dist, 3), actuar::pinvgauss(3, 3, .2))

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.4)), 0.4, tolerance = 1e-3)

  # stats
  expect_equal(mean(dist), actuar::minvgauss(1, 3, .2))
  expect_equal(variance(dist), actuar::minvgauss(2, 3, .2) - actuar::minvgauss(1, 3, .2)^2)
})


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/tests/testthat/test-dist-logarithmic.R ---
test_that("Logarithmic distribution", {
  dist <- dist_logarithmic(0.66)

  expect_equal(format(dist), "Logarithmic(0.66)")

  # Require package installed
  skip_if_not_installed("actuar", "2.0.0")

  # quantiles
  expect_equal(quantile(dist, 0.5), actuar::qlogarithmic(0.5, 0.66))
  expect_equal(quantile(dist, 0.99), actuar::qlogarithmic(0.99, 0.66))

  # pdf
  expect_equal(density(dist, 1), actuar::dlogarithmic(1, 0.66))
  expect_equal(density(dist, 9), actuar::dlogarithmic(9, 0.66))

  # cdf
  expect_equal(cdf(dist, 3), actuar::plogarithmic(3, 0.66))
  expect_equal(cdf(dist, 12), actuar::plogarithmic(12, 0.66))

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.9963064)), 0.9963064, tolerance = 1e-3)

  # stats
  expect_equal(mean(dist), -1/log(1-0.66)*(0.66/(1-0.66)))
  expect_equal(variance(dist), -(0.66^2 + 0.66*log(1-0.66))/((1-0.66)^2*log(1-0.66)^2))
})


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/tests/testthat/test-dist-logistic.R ---
test_that("Logistic distribution", {
  dist <- dist_logistic(5, 2)

  expect_equal(format(dist), "Logistic(5, 2)")

  # quantiles
  expect_equal(quantile(dist, 0.1), stats::qlogis(0.1, 5, 2))
  expect_equal(quantile(dist, 0.5), stats::qlogis(0.5, 5, 2))

  # pdf
  expect_equal(density(dist, 0), stats::dlogis(0, 5, 2))
  expect_equal(density(dist, 3), stats::dlogis(3, 5, 2))

  # cdf
  expect_equal(cdf(dist, 0), stats::plogis(0, 5, 2))
  expect_equal(cdf(dist, 3), stats::plogis(3, 5, 2))

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.4246)), 0.4246, tolerance = 1e-3)

  # stats
  expect_equal(mean(dist), 5)
  expect_equal(variance(dist), (2*pi)^2/3)
})


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/tests/testthat/test-dist-lognormal.R ---
test_that("Log-normal distribution", {
  # defaults
  dist <- dist_lognormal()
  expect_equal(mean(dist), exp(1/2))
  expect_equal(variance(dist), (exp(1)-1)*exp(1))

  # display
  expect_s3_class(dist, "distribution")
  expect_output(print(dist), "lN\\(0, 1\\)")
  expect_output(print(dist_lognormal(numeric())), "<distribution\\[0\\]>")

  # error checking
  expect_error(
    dist_lognormal(0, -1),
    "non-negative"
  )
  expect_silent(
    dist_lognormal(mu = 0, sigma = 1)
  )

  # density
  expect_equal(
    density(dist, 0),
    dlnorm(0, mean = 0, sd = 1)
  )
  # cdf
  expect_equal(
    cdf(dist, 5),
    plnorm(5, mean = 0, sd = 1)
  )
  # quantile
  expect_equal(
    quantile(dist, 0.1),
    qlnorm(0.1, mean = 0, sd = 1)
  )
  # generate
  expect_equal(
    {
      set.seed(0)
      generate(dist, 10)
    },
    {
      set.seed(0)
      mapply(function(m, s) rlnorm(10, m, s), m = 0, s = 1, SIMPLIFY = FALSE)
    }
  )
})


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/tests/testthat/test-dist-missing.R ---
test_that("Missing distribution", {
  dist <- dist_missing()

  expect_equal(format(dist), "NA")

  # quantiles
  expect_equal(quantile(dist, c(0.1, 0.9)), list(c(NA_real_, NA_real_)))

  # pdf
  expect_equal(density(dist, 1:2), list(c(NA_real_, NA_real_)))

  # cdf
  expect_equal(cdf(dist, 1:2), list(c(NA_real_, NA_real_)))

  # stats
  expect_equal(mean(dist), NA_real_)
  expect_equal(variance(dist), NA_real_)
})


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/tests/testthat/test-dist-multinomial.R ---
test_that("Multinomial distribution", {
  p <- c(0.3, 0.5, 0.2)
  dist <- dist_multinomial(size = 4, prob = list(p))
  dimnames(dist) <- c("a", "b", "c")

  expect_equal(format(dist), "Multinomial(4)[3]")

  # quantiles

  # pdf
  expect_equal(density(dist, cbind(1, 2, 1)), dmultinom(c(1, 2, 1), 4, p))

  # cdf

  # F(Finv(a)) ~= a

  # stats
  expect_equal(
    mean(dist),
    matrix(c(1.2, 2, 0.8), nrow = 1, dimnames = list(NULL, c("a", "b", "c")))
  )

  expect_equal(
    covariance(dist),
    list(
      matrix(
        c(0.84, -0.6, -0.24, -0.6, 1, -0.4, -0.24, -0.4, 0.64),
        nrow = 3, dimnames = list(NULL, c("a", "b", "c"))
      )
    )
  )
})


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/tests/testthat/test-dist-multivariate-normal.R ---
test_that("Multivariate normal distribution", {
  mu <- c(1,2)
  sigma <- matrix(c(4,2,2,3), ncol=2)
  dist <- dist_multivariate_normal(mu = list(mu), sigma = list(sigma))
  dimnames(dist) <- c("a", "b")

  expect_equal(format(dist), "MVN[2]")

  # stats
  expect_equal(
    mean(dist),
    matrix(c(1,2), nrow = 1, dimnames = list(NULL, c("a", "b")))
  )
  expect_equal(covariance(dist), list(`colnames<-`(sigma, dimnames(dist))))

  # quantiles
  expect_equal(
    quantile(dist, 0.1, type = "marginal"),
    matrix(c(qnorm(0.1, mu[1], sqrt(sigma[1,1])), qnorm(0.1, mu[2], sqrt(sigma[2,2]))),
           nrow = 1, dimnames = list(NULL, c("a", "b")))
  )

  skip_if_not_installed("mvtnorm")
  expect_equivalent(
    quantile(dist, 0.1, type = "equicoordinate"),
    rep(mvtnorm::qmvnorm(0.1, mean = mu, sigma = sigma)$quantile, 2)
  )

  # pdf
  expect_equal(density(dist, cbind(1, 2)), mvtnorm::dmvnorm(c(1, 2), mean = mu, sigma = sigma))
  expect_equal(density(dist, cbind(-3, 4)), mvtnorm::dmvnorm(c(-3, 4), mean = mu, sigma = sigma))

  # cdf
  expect_equivalent(cdf(dist, cbind(1, 2)), mvtnorm::pmvnorm(upper = c(1,2), mean = mu, sigma = sigma))
  expect_equivalent(cdf(dist, cbind(-3, 4)), mvtnorm::pmvnorm(upper = c(-3, 4), mean = mu, sigma = sigma))

  # F(Finv(a)) ~= a
  # expect_equal(cdf(dist, list(as.numeric(quantile(dist, 0.53)))), 0.53, tolerance = 1e-3)
})


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/tests/testthat/test-dist-negative-binomial.R ---
test_that("Negative Binomial distribution", {
  dist <- dist_negative_binomial(10, 0.4)

  expect_equal(format(dist), "NB(10, 0.4)")

  # quantiles
  expect_equal(quantile(dist, 0.6), stats::qnbinom(0.6, 10, 0.4))
  expect_equal(quantile(dist, 0.61), stats::qnbinom(0.61, 10, 0.4))

  # pdf
  expect_equal(density(dist, 0), stats::dnbinom(0, 10, 0.4))
  expect_equal(density(dist, 1), stats::dnbinom(1, 10, 0.4))

  # cdf
  expect_equal(cdf(dist, 0), stats::pnbinom(0, 10, 0.4))
  expect_equal(cdf(dist, 1), stats::pnbinom(1, 10, 0.4))

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.6358)), 0.6358, tolerance = 1e-3)

  # stats
  expect_equal(mean(dist), 0.6*10/(1-0.6))
  expect_equal(variance(dist), 0.6*10/(1-0.6)^2)
})


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/tests/testthat/test-dist-normal.R ---
test_that("Normal distribution", {
  # defaults
  dist <- dist_normal()
  expect_equal(mean(dist), 0)
  expect_equal(variance(dist), 1)

  # display
  expect_s3_class(dist, "distribution")
  expect_output(print(dist), "N\\(0, 1\\)")
  expect_output(print(dist_normal(numeric())), "<distribution\\[0\\]>")

  # error checking
  expect_error(
    dist_normal(0, -1),
    "non-negative"
  )
  expect_silent(
    dist_normal(mu = 0L, sigma = 1L)
  )

  mu <- rnorm(10)
  sigma <- abs(rnorm(10))
  dist <- dist_normal(mu, sigma)

  # summary statistics
  expect_equal(mean(dist), mu)
  expect_equal(median(dist), mu)
  expect_equal(median(dist[[1]]), mu[[1]])
  expect_equal(variance(dist), sigma^2)

  # math
  expect_equal(mean(dist*3+1), mu*3+1)
  expect_equal(variance(dist*3+1), (sigma*3)^2)
  expect_equal(mean(dist + dist), mean(dist) + mean(dist))
  expect_equal(variance(dist + dist), variance(dist) + variance(dist))
  expect_equal(dist_normal() * 2, dist_normal(0, 2))

  # density
  expect_equal(
    density(dist, 0),
    dnorm(0, mean = mu, sd = sigma)
  )
  # cdf
  expect_equal(
    cdf(dist, 5),
    pnorm(5, mean = mu, sd = sigma)
  )
  # quantile
  expect_equal(
    quantile(dist, 0.1),
    qnorm(0.1, mean = mu, sd = sigma)
  )
  # generate
  expect_equal(
    {
      set.seed(0)
      generate(dist, 10)
    },
    {
      set.seed(0)
      mapply(function(m, s) rnorm(10, m, s), m = mu, s = sigma, SIMPLIFY = FALSE)
    }
  )
})


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/tests/testthat/test-dist-pareto.R ---
test_that("Pareto distribution", {
  dist <- dist_pareto(10, 1)

  expect_equal(format(dist), "Pareto(10, 1)")

  # Require package installed
  skip_if_not_installed("actuar", "2.0.0")

  # quantiles
  expect_equal(quantile(dist, 0.1), actuar::qpareto(0.1, 10, 1))
  expect_equal(quantile(dist, 0.5), actuar::qpareto(0.5, 10, 1))

  # pdf
  expect_equal(density(dist, 0), actuar::dpareto(0, 10, 1))
  expect_equal(density(dist, 3), actuar::dpareto(3, 10, 1))

  # cdf
  expect_equal(cdf(dist, 0), actuar::ppareto(0, 10, 1))
  expect_equal(cdf(dist, 3), actuar::ppareto(3, 10, 1))

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.4)), 0.4, tolerance = 1e-3)

  # stats
  expect_equal(mean(dist), actuar::mpareto(1, 10, 1))
  expect_equal(variance(dist), actuar::mpareto(2, 10, 1) - actuar::mpareto(1, 10, 1)^2)
})


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/tests/testthat/test-dist-percentile.R ---
test_that("Negative Binomial distribution", {
  dist <- dist_normal(0, 1)
  percentiles <- seq(0.01, 0.99, by = 0.01)
  x <- vapply(percentiles, quantile, double(1L), x = dist)
  dist <- dist_percentile(list(x), list(percentiles*100))

  expect_equal(format(dist), "percentile[99]")

  # quantiles
  expect_equal(quantile(dist, 0.6), stats::qnorm(0.6, 0, 1))
  expect_equal(quantile(dist, 0.61), stats::qnorm(0.61, 0, 1))

  # pdf

  # cdf
  expect_equal(cdf(dist, 0), stats::pnorm(0, 0, 1))
  expect_equal(cdf(dist, 1), stats::pnorm(1, 0, 1), tolerance = 1e-3)

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.6)), 0.6, tolerance = 1e-3)

  # stats
})


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/tests/testthat/test-dist-poisson-inverse-gaussian.R ---
test_that("Poisson Inverse Gaussian distribution", {
  dist <- dist_poisson_inverse_gaussian(0.1, 0.8)

  expect_equal(format(dist), "PIG(0.1, 0.8)")

  # Require package installed
  skip_if_not_installed("actuar", "2.0.0")

  # quantiles
  expect_equal(quantile(dist, 0.1), actuar::qpig(0.1, 0.1, 0.8))
  expect_equal(quantile(dist, 0.5), actuar::qpig(0.5, 0.1, 0.8))

  # pdf
  expect_equal(density(dist, 0), actuar::dpig(0, 0.1, 0.8))
  expect_equal(density(dist, 3), actuar::dpig(3, 0.1, 0.8))

  # cdf
  expect_equal(cdf(dist, 0), actuar::ppig(0, 0.1, 0.8))
  expect_equal(cdf(dist, 3), actuar::ppig(3, 0.1, 0.8))

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.994)), 0.994, tolerance = 1e-3)

  # stats
  expect_equal(mean(dist), 0.1)
  expect_equal(variance(dist), 0.1/0.8*(0.1^2 + 0.8))
})


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/tests/testthat/test-dist-sample.R ---
test_that("Emperical/sample distribution", {
  x <- generate(dist_normal(0, 1), 100)
  dist <- dist_sample(x)

  expect_equal(format(dist), "sample[100]")

  # quantiles
  expect_equal(quantile(dist, 0.6), unname(quantile(x[[1]], 0.6)))
  expect_equal(quantile(dist, 0.24), unname(quantile(x[[1]], 0.24)))

  # pdf

  # cdf

  # F(Finv(a)) ~= a

  # stats
  expect_equal(mean(dist), mean(x[[1]]))
  expect_equal(median(dist), median(x[[1]]))
  expect_equal(median(dist[[1]]), median(x[[1]]))
  expect_equal(variance(dist), var(x[[1]]))

  # transform
  expect_equal(
    dist,
    dist + 1 - 1
  )
})

test_that("CDF of degenerate dist_sample() is correct", {
  expect_equal(cdf(dist_sample(list(2)), 2)[[1]], 1)
})


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/tests/testthat/test-dist-student-t.R ---
test_that("Student T distribution", {
  dist <- dist_student_t(5)

  expect_equal(format(dist), "t(5, 0, 1)")

  # quantiles
  expect_equal(quantile(dist, 0.1), stats::qt(0.1, 5))
  expect_equal(quantile(dist, 0.5), stats::qt(0.5, 5))

  # pdf
  expect_equal(density(dist, 0), stats::dt(0, 5))
  expect_equal(density(dist, 3), stats::dt(3, 5))

  # cdf
  expect_equal(cdf(dist, 0), stats::pt(0, 5))
  expect_equal(cdf(dist, 3), stats::pt(3, 5))

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.4246)), 0.4246, tolerance = 1e-3)

  # stats
  expect_equal(mean(dist), 0)
  expect_equal(variance(dist), 5 / (5-2))
})

test_that("Noncentral Student t distribution", {
  dist <- dist_student_t(8, ncp = 6)

  expect_equal(format(dist), "t(8, 0, 1, 6)")

  # quantiles
  expect_equal(quantile(dist, 0.1), stats::qt(0.1, 8, ncp = 6))
  expect_equal(quantile(dist, 0.5), stats::qt(0.5, 8, ncp = 6))

  # pdf
  expect_equal(density(dist, 0), stats::dt(0, 8, ncp = 6))
  expect_equal(density(dist, 3), stats::dt(3, 8, ncp = 6))

  # cdf
  expect_equal(cdf(dist, 0), stats::pt(0, 8, ncp = 6))
  expect_equal(cdf(dist, 3), stats::pt(3, 8, ncp = 6))

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.4246)), 0.4246, tolerance = 1e-3)

  # stats
  expect_equal(mean(dist), 2 * gamma(7/2))
  expect_equal(variance(dist), 148/3 - 4 * gamma(7/2)^2)
})

test_that("Location-scale Student t distribution", {
  dist <- dist_student_t(5, 2, 3)

  expect_equal(format(dist), "t(5, 2, 3)")

  # quantiles
  expect_equal(quantile(dist, 0.1), stats::qt(0.1, 5)*3 + 2)
  expect_equal(quantile(dist, 0.5), stats::qt(0.5, 5)*3 + 2)

  # pdf
  expect_equal(density(dist, 0), stats::dt(-2/3, 5)/3)
  expect_equal(density(dist, 3), stats::dt((3-2)/3, 5)/3)

  # cdf
  expect_equal(cdf(dist, 0), stats::pt(-2/3, 5))
  expect_equal(cdf(dist, 3), stats::pt((3-2)/3, 5))

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.4246)), 0.4246, tolerance = 1e-3)

  # stats
  expect_equal(mean(dist), 2)
  expect_equal(variance(dist), 5 / (5-2) * 3^2)
})

test_that("Noncentral location-scale Student t distribution", {
  dist <- dist_student_t(5, 2, 3, ncp = 6)

  expect_equal(format(dist), "t(5, 2, 3, 6)")

  # quantiles
  expect_equal(quantile(dist, 0.1), stats::qt(0.1, 5, 6)*3 + 2)
  expect_equal(quantile(dist, 0.5), stats::qt(0.5, 5, 6)*3 + 2)

  # pdf
  expect_equal(density(dist, 0), stats::dt(-2/3, 5, 6)/3)
  expect_equal(density(dist, 3), stats::dt((3-2)/3, 5, 6)/3)

  # cdf
  expect_equal(cdf(dist, 0), stats::pt(-2/3, 5, 6))
  expect_equal(cdf(dist, 3), stats::pt((3-2)/3, 5, 6))

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.4246)), 0.4246, tolerance = 1e-3)

  # stats
  expect_equal(mean(dist), 2 + 6 * sqrt(5/2) * gamma(2)/gamma(5/2) * 3)
  expect_equal(variance(dist), ((5*(1+6^2))/(5-2) - (6 * sqrt(5/2) * (gamma((5-1)/2)/gamma(5/2)))^2)*3^2)
})


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/tests/testthat/test-dist-studentised-range.R ---
test_that("Studentized Range distribution", {
  dist <- dist_studentized_range(6, 5, 1)

  expect_equal(format(dist), "StudentizedRange(6, 5, 1)")

  # quantiles
  expect_equal(quantile(dist, 0.1), stats::qtukey(0.1, 6, 5, 1))
  expect_equal(quantile(dist, 0.5), stats::qtukey(0.5, 6, 5, 1))

  # pdf

  # cdf
  expect_equal(cdf(dist, 0), stats::ptukey(0, 6, 5, 1))
  expect_equal(cdf(dist, 3), stats::ptukey(3, 6, 5, 1))

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.4246)), 0.4246, tolerance = 1e-3)
})


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/tests/testthat/test-dist-uniform.R ---
test_that("Uniform distribution", {
  dist <- dist_uniform(-2, 4)

  expect_equal(format(dist), "U(-2, 4)")

  # quantiles
  expect_equal(quantile(dist, 0.1), stats::qunif(0.1, -2, 4))
  expect_equal(quantile(dist, 0.5), stats::qunif(0.5, -2, 4))

  # pdf
  expect_equal(density(dist, 0), stats::dunif(0, -2, 4))
  expect_equal(density(dist, 3), stats::dunif(3, -2, 4))

  # cdf
  expect_equal(cdf(dist, 0), stats::punif(0, -2, 4))
  expect_equal(cdf(dist, 3), stats::punif(3, -2, 4))

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.4246)), 0.4246, tolerance = 1e-3)

  # stats
  expect_equal(mean(dist), 0.5*(-2 + 4))
  expect_equal(variance(dist), (4+2)^2/12)
})


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/tests/testthat/test-dist-weibull.R ---
test_that("Weibull distribution", {
  dist <- dist_weibull(1.5, 1)

  expect_equal(format(dist), "Weibull(1.5, 1)")

  # quantiles
  expect_equal(quantile(dist, 0.1), stats::qweibull(0.1, 1.5, 1))
  expect_equal(quantile(dist, 0.5), stats::qweibull(0.5, 1.5, 1))

  # pdf
  expect_equal(density(dist, 0), stats::dweibull(0, 1.5, 1))
  expect_equal(density(dist, 3), stats::dweibull(3, 1.5, 1))

  # cdf
  expect_equal(cdf(dist, 0), stats::pweibull(0, 1.5, 1))
  expect_equal(cdf(dist, 3), stats::pweibull(3, 1.5, 1))

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.4246)), 0.4246, tolerance = 1e-3)

  # stats
  expect_equal(mean(dist), 1 * gamma(1 + 1/1.5))
  expect_equal(variance(dist), 1^2 * (gamma(1 + 2/1.5) - gamma(1 + 1/1.5)^2))
})


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/tests/testthat/test-distribution.R ---
test_that("is_distribution", {
  expect_false(is_distribution(iris))
  expect_true(is_distribution(dist_normal()))
  expect_false(is_distribution(NULL))
  expect_false(is_distribution(0))

  df <- data.frame(a = 1:10, b = dist_poisson(1:10), c = dist_normal(1:10))
  expect_true(all(sapply(df, is_distribution) == c(FALSE, TRUE, TRUE)))
})

test_that("variance() works correctly on vectors/matrices of different dimension", {
  x = 1:8

  expect_equal(variance(x), 6)
  expect_equal(variance(matrix(x, nrow = 2)), rep(0.5, 4))
})

test_that("variance() throws an error on non-numeric objects", {
  expect_error(variance("foo"))
})


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/tests/testthat/test-hilo.R ---
test_that("hilo", {
  # defaults
  hl <- new_hilo()
  expect_length(hl, 0)

  # display
  expect_output(print(hl), "<hilo\\[0\\]>")
  expect_output(print(new_hilo(0,1,95)), "\\[0, 1\\]95")

  dist <- dist_normal()

  # hilo.distribution
  hl <- hilo(dist, 95)
  expect_length(hl, 1)
  expect_equal(hl, new_hilo(qnorm(0.025), qnorm(0.975), 95))

  # vec_math.hilo
  expect_equal(is.na(hl), FALSE)
  expect_equal(is.nan(hl), FALSE)

  # vec_arith.hilo
  expect_equal(
    hl*3+1,
    new_hilo(qnorm(0.025)*3+1, qnorm(0.975)*3+1, 95)
  )
  expect_equal(-hl, hl)
  expect_equal(-3*hl, hl/(1/3))
})


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/tests/testthat/test-inflated.R ---
test_that("Check zero inflation", {
  dist <- dist_inflated(dist_poisson(6), 0.33)

  expect_equal(format(dist), "0+Pois(6)")

  # quantiles
  expect_equal(quantile(dist, 0.1), 0)
  expect_equal(quantile(dist, 0.5), 4)

  # pdf
  expect_equal(density(dist, 0), 0.33 + 0.67*dpois(0, 6))
  expect_equal(density(dist, 3), 0.67*dpois(3, 6))

  # cdf
  expect_equal(cdf(dist, 0), 0.33 + 0.67*ppois(0, 6))
  expect_equal(cdf(dist, 3), 0.33 + 0.67*ppois(3, 6))

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.52)), 0.52, tolerance = 1e-3)

  # stats
  expect_equal(mean(dist), 0.67*6)
  expect_equal(variance(dist), 0.67*6 + (0.33/0.67)*(0.67*6)^2)
})

test_that("Check non-zero inflation", {
  dist <- dist_inflated(dist_poisson(6), 0.33, 2)

  expect_equal(format(dist), "2+Pois(6)")

  # quantiles
  expect_equal(quantile(dist, 0), 0)
  expect_equal(quantile(dist, 0.1), 2)
  expect_equal(quantile(dist, 0.33), 2)
  expect_equal(quantile(dist, 0.5), 4)

  # pdf
  expect_equal(density(dist, 0), 0.67*dpois(0, 6))
  expect_equal(density(dist, 2), 0.33 + 0.67*dpois(2, 6))
  expect_equal(density(dist, 3), 0.67*dpois(3, 6))

  # cdf
  expect_equal(cdf(dist, 0), 0.67*ppois(0, 6))
  expect_equal(cdf(dist, 2), 0.33 + 0.67*ppois(2, 6))
  expect_equal(cdf(dist, 3), 0.33 + 0.67*ppois(3, 6))

  # stats
  expect_equal(mean(dist), 0.33*2 + 0.67*6)
  # expect_equal(variance(d), ???)
})


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/tests/testthat/test-issues.R ---
test_that("is.na() on <dist>[[1]] (#29)", {
  x <- c(dist_normal(0,1), NA)
  expect_equal(
    is.na(x),
    c(FALSE, TRUE)
  )

  expect_equal(
    is.na(x[[1]]),
    FALSE
  )

  expect_equal(
    is.na(x[[2]]),
    TRUE
  )
})


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/tests/testthat/test-mixture.R ---
test_that("Mixture of Normals", {
  dist <- dist_mixture(dist_normal(0, 1), dist_normal(10, 4), weights = c(0.5, 0.5))

  # format
  expect_equal(format(dist), "mixture(0.5*N(0, 1), 0.5*N(10, 16))")

  # quantiles
  expect_equal(quantile(dist, 0.5), 2, tolerance = 1e-5)
  expect_equal(quantile(dist, 0.1), -0.854, tolerance = 1e-3)

  # pdf
  expect_equal(density(dist, 0), 0.5 * dnorm(0) + 0.5 * dnorm(0, 10, 4))
  expect_equal(density(dist, 3), 0.5 * dnorm(3) + 0.5 * dnorm(3, 10, 4))

  # cdf
  expect_equal(cdf(dist, 0), 0.5 * pnorm(0) + 0.5 * pnorm(0, 10, 4))
  expect_equal(cdf(dist, 3), 0.5 * pnorm(3) + 0.5 * pnorm(3, 10, 4))

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.5)), 0.5, tolerance = 1e-6)

  expect_equal(mean(dist), 5)
  expect_equal(variance(dist), 33.5)
})

test_that("Mixture of different distributions", {
  dist <- dist_mixture(dist_normal(0, 1), dist_student_t(10), weights = c(0.3, 0.7))

  # format
  expect_equal(format(dist), "mixture(0.3*N(0, 1), 0.7*t(10, 0, 1))")

  # quantiles
  expect_equal(quantile(dist, 0.5), 0, tolerance = 1e-5)
  expect_equal(quantile(dist, 0.1), -1.343, tolerance = 1e-3)

  # pdf
  expect_equal(density(dist, 0), 0.3 * dnorm(0) + 0.7 * dt(0, 10))
  expect_equal(density(dist, 3), 0.3 * dnorm(3) + 0.7 * dt(3, 10))

  # cdf
  expect_equal(cdf(dist, 0), 0.3 * pnorm(0) + 0.7 * pt(0, 10))
  expect_equal(cdf(dist, 3), 0.3 * pnorm(3) + 0.7 * pt(3, 10))

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.5)), 0.5, tolerance = 1e-6)

  expect_equal(mean(dist), 0)
  expect_equal(variance(dist), 1.175)
})

test_that("Mixture of point masses", {
  dist <- dist_mixture(dist_degenerate(1), dist_degenerate(2), dist_degenerate(3), weights = c(0.1, 0.2, 0.7))

  # format
  expect_equal(format(dist, width = 10), "mixture(n=3)")
  expect_equal(format(dist), "mixture(0.1*1, 0.2*2, 0.7*3)")

  # quantiles
  expect_equal(quantile(dist, c(0, 0.1, 0.3, 1))[[1]], c(1, 1:3), tolerance = .Machine$double.eps^0.25)

  # pmf
  expect_equal(density(dist, 1:3)[[1]], c(0.1, 0.2, 0.7))

  # cdf
  expect_equal(cdf(dist, 1:3)[[1]], c(0.1, 0.3, 1))

  # mean
  expect_equal(mean(dist), 2.6)
})

test_that("Mixture of multivariate distributions", {
  mu1 <- c(0, 0)
  mu2 <- c(1, 2)
  mu3 <- c(-10, -10)
  mu4 <- c(10, 10)
  sigma1 <- diag(2)
  sigma2 <- matrix(c(4, 2, 2, 3), 2, 2)
  w1 <- 0.3
  w2 <- 1 - w1
  w3 <- 0.5
  w4 <- 1 - w3
  # Two mixtures of bivariate normals
  dist <- c(
    dist1 = dist_mixture(
      dist_multivariate_normal(mu = list(mu1), sigma = list(sigma1)),
      dist_multivariate_normal(mu = list(mu2), sigma = list(sigma2)),
      weights = c(w1, w2)
    ),
    dist2 = dist_mixture(
      dist_multivariate_normal(mu = list(mu3), sigma = list(sigma1)),
      dist_multivariate_normal(mu = list(mu4), sigma = list(sigma1)),
      weights = c(w3, w4)
    )
  )
  # Mean
  expect_equal(mean(dist), rbind(w1 * mu1 + w2 * mu2, w3 * mu3 + w4 * mu4))
  # Quantile
  expect_error(quantile(dist, 0.5))
  # CDF
  at <- matrix(rnorm(4), 2, 2)
  cdf_dist <- cdf(dist, q = at)
  expect_equal(length(cdf_dist), 2L)
  expect_equal(lengths(cdf_dist), c(2L, 2L))
  # Density
  skip_if_not_installed("mvtnorm")
  expect_equal(
    density(dist, at),
    list(
      c(w1 * mvtnorm::dmvnorm(at, mean = mu1, sigma = sigma1) +
        w2 * mvtnorm::dmvnorm(at, mean = mu2, sigma = sigma2)),
      c(w3 * mvtnorm::dmvnorm(at, mean = mu3, sigma = sigma1) +
        w4 * mvtnorm::dmvnorm(at, mean = mu4, sigma = sigma1))
    )
  )
  # Mixture equivalent to multivariate normal
  dist <- dist_multivariate_normal(mu = list(mu2), sigma = list(sigma2))
  mdist <- dist_mixture(dist, dist, weights = c(w1, w2))
  expect_equal(mean(dist), mean(mdist))
  expect_equal(covariance(dist), covariance(mdist))
  expect_equal(unname(density(dist, rbind(mu1))), density(mdist, rbind(mu1)))
  set.seed(1)
  expect_equal(cdf(dist, rbind(c(0, 0))), cdf(mdist, rbind(c(0, 0))),
    tolerance = 0.001
  )
})

test_that("Mixture with bad weights",{

  expect_error(
    dist_mixture(dist_normal(0, 1), dist_normal(10, 4), weights = c(0.6, 0.5))
  )

  expect_error(
    dist_mixture(dist_normal(0, 1), dist_normal(10, 4), weights = c(0.4, 0.5))
  )

  dist <- dist_mixture(dist_normal(0, 1),
               dist_normal(10, 4),
               dist_normal(5, 2),
               weights = c(0.2177682569661155698,
                           0.0491619687405063024,
                           0.7330697742933780514))
  expect_equal(
    class(dist)[1],
    "distribution")

})


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/tests/testthat/test-support.R ---
test_that("support gives the correct bounds", {
  s <- support(c(dist_normal(),
               dist_gamma(1, 1),
               dist_gamma(2, 1),
               dist_lognormal(),
               dist_beta(1, 1),
               dist_beta(1, 2),
               dist_beta(2, 1),
               dist_beta(2, 2),
               exp(dist_wrap('norm')),
               2*atan(dist_normal())))
  out <- unname(format(s))
  expect_equal(out, c("R","[0,Inf)","(0,Inf)","(0,Inf)","[0,1]","[0,1)","(0,1]","(0,1)","(0,Inf)","(-pi,pi)"))
})


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/tests/testthat/test-transformations.R ---
test_that("hilo of transformed distributions", {
  expect_identical(
    hilo(exp(dist_poisson(3))),
    exp(hilo((dist_poisson(3))))
  )
})

test_that("chains of transformations", {
  expect_identical(
    hilo(dist_student_t(5)),
    hilo(log(exp(dist_student_t(5))))
  )

  expect_output(
    print(exp(dist_student_t(5))-1),
    "t\\(t\\(5, 0, 1\\)\\)"
  )
})

test_that("handling of transformation arguments", {
  expect_identical(
    hilo(logb(dist_uniform(0, 100), base = 10)),
    logb(hilo(dist_uniform(0, 100)), base = 10)
  )

  expect_identical(
    hilo(10^logb(dist_uniform(0, 100), base = 10)),
    10^logb(hilo(dist_uniform(0, 100)), base = 10)
  )
})

test_that("LogNormal distributions", {
  dist <- dist_transformed(dist_normal(0, 0.5), exp, log)
  ln_dist <- dist_lognormal(0, 0.5)

  # Test exp() shortcut
  expect_identical(
    exp(dist_normal(0, 0.5)),
    ln_dist
  )
  expect_identical(
    log(ln_dist),
    dist_normal(0, 0.5)
  )

  # Test log() shortcut with different bases
  expect_equal(log(dist_lognormal(0, log(3)), base = 3), dist_normal(0, 1))
  expect_equal(log2(dist_lognormal(0, log(2))), dist_normal(0, 1))
  expect_equal(log10(dist_lognormal(0, log(10))), dist_normal(0, 1))

  # format
  expect_equal(format(dist), sprintf("t(%s)", format(dist_normal(0, 0.5))))

  # quantiles
  expect_equal(
    quantile(dist, c(0.1, 0.5)),
    quantile(ln_dist, c(0.1, 0.5))
  )

  # pdf
  expect_equal(
    density(dist, c(1, 20)),
    density(ln_dist, c(1, 20))
  )

  # cdf
  expect_equal(
    cdf(dist, c(4, 90)),
    cdf(ln_dist, c(4, 90))
  )

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.372)), 0.372, tolerance = 1e-3)

  # stats (approximate due to bias adjustment method)
  expect_equal(mean(dist), exp(0.25/2), tolerance = 0.01)
  expect_equal(variance(dist), (exp(0.25) - 1)*exp(0.25), tolerance = 0.1)
})

test_that("inverses are applied automatically", {
  dist <- dist_gamma(1,1)
  log2dist <- log(dist, base = 2)
  log2dist_t <- dist_transformed(dist, log2, function(x) 2 ^ x)

  expect_equal(density(log2dist, 0.5), density(log2dist_t, 0.5))
  expect_equal(cdf(log2dist, 0.5), cdf(log2dist_t, 0.5))
  expect_equal(quantile(log2dist, 0.5), quantile(log2dist_t, 0.5))

  # test multiple transformations that get stacked together by dist_transformed
  explogdist <- exp(log(dist))
  expect_equal(density(dist, 0.5), density(explogdist, 0.5))
  expect_equal(cdf(dist, 0.5), cdf(explogdist, 0.5))
  expect_equal(quantile(dist, 0.5), quantile(explogdist, 0.5))

  # test multiple transformations created by operators (via Ops)
  explog2dist <- 2 ^ log2dist
  expect_equal(density(dist, 0.5), density(explog2dist, 0.5))
  expect_equal(cdf(dist, 0.5), cdf(explog2dist, 0.5))
  expect_equal(quantile(dist, 0.5), quantile(explog2dist, 0.5))

  # basic set of inverses
  expect_equal(density(sqrt(dist^2), 0.5), density(dist, 0.5))
  expect_equal(density(exp(log(dist)), 0.5), density(dist, 0.5))
  expect_equal(density(10^(log10(dist)), 0.5), density(dist, 0.5))
  expect_equal(density(expm1(log1p(dist)), 0.5), density(dist, 0.5))
  expect_equal(density(cos(acos(dist)), 0.5), density(dist, 0.5))
  expect_equal(density(sin(asin(dist)), 0.5), density(dist, 0.5))
  expect_equal(density(tan(atan(dist)), 0.5), density(dist, 0.5))
  expect_equal(density(cosh(acosh(dist + 1)) - 1, 0.5), density(dist, 0.5))
  expect_equal(density(sinh(asinh(dist)), 0.5), density(dist, 0.5))
  expect_equal(density(tanh(atanh(dist)), 0.5), density(dist, 0.5))

  expect_equal(density(dist + 1 - 1, 0.5), density(dist, 0.5))
  expect_equal(density(dist * 2 / 2, 0.5), density(dist, 0.5))

  # inverting a gamma distribution
  skip_if_not_installed("actuar")
  expect_equal(density(1/dist_gamma(4, 3), 0.5), density(dist_inverse_gamma(4, 1/3), 0.5))
  expect_equal(density(1/(1/dist_gamma(4, 3)), 0.5), density(dist_gamma(4, 3), 0.5))

})

test_that("transformed distributions' density is 0 outside of the support region", {
  dist <- dist_wrap('norm')
  expect_equal(density(exp(dist), 0)[[1]], 0)
  expect_equal(density(exp(dist), -1)[[1]], 0)

  dist <- dist_wrap('gamma', shape = 1, rate = 1)
  expect_equal(density(exp(dist), 0)[[1]], 0)
  expect_equal(density(exp(dist), 1)[[1]], 1)
})


test_that("transformed distributions' cdf is 0/1 outside of the support region", {
  dist <- dist_wrap('norm')
  expect_equal(cdf(exp(dist), 0)[[1]], 0)
  expect_equal(cdf(exp(dist), -1)[[1]], 0)
  expect_equal(cdf(-1*exp(dist), 0)[[1]], 1)
  expect_equal(cdf(-1*exp(dist), 2)[[1]], 1)
})

test_that("unary negation operator works", {
  dist <- dist_normal(1,1)
  expect_equal(density(-dist, 0.5), density(dist, -0.5))

  dist <- dist_wrap('norm', mean = 1)
  expect_equal(density(-dist, 0.5), density(dist, -0.5))

  dist <- dist_student_t(3, mu = 1)
  expect_equal(density(-dist, 0.5), density(dist, -0.5))
})

test_that("transformed distributions pdf integrates to 1", {
  dist_names <- c('norm', 'gamma', 'beta', 'chisq', 'exp',
                  'logis', 't', 'unif', 'weibull')
  dist_args <- list(list(mean = 1, sd = 1), list(shape = 2, rate = 1),
                    list(shape1 = 3, shape2 = 5), list(df = 5),
                    list(rate = 1),
                    list(location = 1.5, scale = 1), list(df = 10),
                    list(min = 0, max = 1), list(shape = 3, scale = 1))
  names(dist_args) <- dist_names
  dist <- lapply(dist_names, function(x) do.call(dist_wrap, c(x, dist_args[[x]])))
  dist <- do.call(c, dist)
  dfun <- function(x, id, transform) density(get(transform)(dist[id]), x)[[1]]
  twoexp <- function(x) 2^x
  square <- function(x) x^2
  mult2 <- function(x) 2*x
  identity <- function(x) x
  tol <- 1e-5
  for (i in 1:length(dist)) {
    expect_equal(integrate(dfun, -Inf, Inf, id = i, transform = 'identity')$value, 1, tolerance = tol)
    expect_equal(integrate(dfun, -Inf, Inf, id = i, transform = 'exp')$value, 1, tolerance = tol)
    expect_equal(integrate(dfun, -Inf, Inf, id = i, transform = 'twoexp')$value, 1, tolerance = tol)
    expect_equal(integrate(dfun, -Inf, Inf, id = i, transform = 'mult2')$value, 1, tolerance = tol)
    lower_bound <- field(support(dist[[i]]), "lim")[[1]][1]
    if (near(lower_bound, 0)) {
      expect_equal(integrate(dfun, -Inf, 5, id = i, transform = 'log')$value, 1, tolerance = tol)
      expect_equal(integrate(dfun, -Inf, Inf, id = i, transform = 'square')$value, 1, tolerance = tol)
    }
  }
})


test_that("monotonically decreasing transformations (#100)", {
  dist <- dist_lognormal()

  expect_equal(
    quantile(-dist, 0.2), -quantile(dist, 1 - 0.2)
  )
  expect_equal(
    quantile(1/dist, 0.2), 1/quantile(dist, 1 - 0.2)
  )
  expect_equal(
    quantile(-1/dist, 0.7), -1/quantile(dist, 0.7)
  )

  expect_equal(
    cdf(-dist, -2), 1 - cdf(dist, 2)
  )
  expect_equal(
    cdf(1/dist, 2), 1 - cdf(dist, 1/2)
  )
  expect_equal(
    cdf(-1/dist, -2), cdf(dist, 1/2)
  )
})


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/tests/testthat/test-truncated.R ---
test_that("Truncated Normal distributions", {
  dist <- dist_truncated(dist_normal(0, 1), -5, 5)

  # format
  expect_equal(format(dist), sprintf("%s[-5,5]", format(dist_normal(0,1))))

  # quantiles
  expect_equal(
    quantile(dist, 0.1),
    -1.28155025885944 #dput(extraDistr::qtnorm(0.1, 0, 1, -5, 5))
  )
  expect_equal(
    quantile(dist, 0.5),
    -1.39145821233588e-16 #dput(extraDistr::qtnorm(0.5, 0, 1, -5, 5))
  )

  # pdf
  expect_equal(
    density(dist, 0),
    0.398942509116427, #dput(extraDistr::dtnorm(0, 0, 1, -5, 5))
  )
  expect_equal(
    density(dist, 3),
    0.00443185095273209, #dput(extraDistr::dtnorm(3, 0, 1, -5, 5))
  )

  # cdf
  expect_equal(cdf(dist, 0), 0.5)
  expect_equal(
    cdf(dist, 3),
    0.998650387846205, #dput(extraDistr::ptnorm(3, 0, 1, -5, 5))
  )

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.3)), 0.3, tolerance = 1e-6)

  # stats
  # expect_equal(mean(dist), ???)
  # expect_equal(variance(dist), ???)
})

test_that("Truncated Binomial distributions", {
  dist <- dist_truncated(dist_binomial(100, 0.83), 76, 86)

  # format
  expect_equal(format(dist), sprintf("%s[76,86]", format(dist_binomial(100,0.83))))

  # quantiles
  expect_equal(
    quantile(dist, 0.1),
    79 #dput(extraDistr::qtbinom(0.1, 100, 0.83, 76, 86))
  )
  expect_equal(
    quantile(dist, 0.5),
    82 #dput(extraDistr::qtbinom(0.5, 100, 0.83, 76, 86))
  )

  # pdf
  expect_equal(density(dist, 75), 0)
  expect_equal(density(dist, 87), 0)
  expect_equal(
    density(dist, 80),
    0.094154977726162, #dput(extraDistr::dtbinom(80, 100, 0.83, 76, 86))
  )
  expect_equal(
    density(dist, 85),
    0.123463609708811, #dput(extraDistr::dtbinom(85, 100, 0.83, 76, 86))
  )

  # cdf
  expect_equal(cdf(dist, 0), 0)
  expect_equal(cdf(dist, 76), 0)
  expect_equal(cdf(dist, 86), 1)
  expect_equal(
    cdf(dist, 80),
    0.259185477677455, #dput(extraDistr::ptbinom(80, 100, 0.83, 76, 86))
  )

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.372)), 0.372, tolerance = 1e-3)

  # stats
  # expect_equal(mean(dist), ???)
  # expect_equal(variance(dist), ???)
})


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/tests/testthat/test_gev.R ---
test_that("GEV", {
  dist <- dist_gev(location = c(0, .5, 0), scale = c(1, 2, 3), shape = c(0, 0.1, 1.1))
  euler <- 0.57721566490153286 # Euler's constant
  # Mean
  expect_equal(mean(dist), c(
    euler,
    0.5 + 2 * (gamma(0.9) - 1) / 0.1, # location + scale*(gamma(1 - shape) - 1)/shape
    Inf # Since shape >= 1
  ), tolerance = 0.0001)
  # Median
  expect_equal(median(dist), c(
    -log(log(2)),
    0.5 + 2 * (log(2)^(-0.1) - 1) / 0.1, # location + scale*(log(2)^(-shape) - 1)/shape
    3 * (log(2)^(-1.1) - 1) / 1.1 # location + scale*(log(2)^(-shape) - 1)/shape
  ), tolerance = 0.0001)
  expect_equal(median(dist), quantile(dist, 0.5))
  # Variance
  expect_equal(distributional::variance(dist), c(
    pi^2 / 6,
    2^2 * (gamma(1 - 2 * 0.1) - gamma(1 - 0.1)^2) / 0.1^2, # scale^2 * (g2 - g1^2)/shape^2
    Inf # since shape >= 0.5
  ), tolerance = 0.0001)
  # Density
  at <- (0:20) / 10
  expect_equal(density(dist, at), list(
    evd::dgev(at, loc = 0, scale = 1, shape = 0),
    evd::dgev(at, loc = 0.5, scale = 2, shape = 0.1),
    evd::dgev(at, loc = 0, scale = 3, shape = 1.1)
  ))
  # CDF
  expect_equal(distributional::cdf(dist, at), list(
    evd::pgev(at, loc = 0, scale = 1, shape = 0),
    evd::pgev(at, loc = 0.5, scale = 2, shape = 0.1),
    evd::pgev(at, loc = 0, scale = 3, shape = 1.1)
  ))
  # Quantiles
  p <- (1:19) / 20
  expect_equal(quantile(dist, p = p), list(
    evd::qgev(p = p, loc = 0, scale = 1, shape = 0),
    evd::qgev(p = p, loc = 0.5, scale = 2, shape = 0.1),
    evd::qgev(p = p, loc = 0, scale = 3, shape = 1.1)
  ))
  # Generate
  set.seed(123)
  rand_dist <- distributional::generate(dist, times = 1e6)
  expect_equal(unlist(lapply(rand_dist[1:2], mean)),
    mean(dist)[1:2],
    tolerance = 0.01
  )
  expect_equal(unlist(lapply(rand_dist[1:2], var)),
    distributional::variance(dist)[1:2],
    tolerance = 0.01
  )
  expect_equal(unlist(lapply(rand_dist, median)),
    median(dist),
    tolerance = 0.01
  )
})


# --- FILE: https://raw.githubusercontent.com/mitchelloharawild/distributional/main/tests/testthat/test_gpd.R ---
test_that("GPD", {
  dist <- dist_gpd(location = c(0, .5, 0), scale = c(1, 2, 3), shape = c(0, 0.1, 1.1))
  # Mean
  expect_equal(mean(dist), c(
    1,
    0.5 + 2 / 0.9, # location + scale/(1 - shape)
    Inf # Since shape >= 1
  ), tolerance = 0.0001)
  # Median
  expect_equal(median(dist), c(
    -log(0.5),
    0.5 + 2 * (2^0.1 - 1) / 0.1, # location + scale * (2^shape - 1) / shape
    3 * (2^1.1 - 1) / 1.1 # location + scale * (2^shape - 1) / shape
  ), tolerance = 0.0001)
  expect_equal(median(dist), quantile(dist, 0.5))
  # Variance
  expect_equal(distributional::variance(dist), c(
    1,
    2^2 / 0.9^2 / (1 - 2 * 0.1), # scale^2 / (1 - shape)^2 / (1 - 2 * shape)
    Inf # since shape >= 0.5
  ), tolerance = 0.0001)
  # Density
  at <- (0:20) / 10 + 1e-8 # Avoiding being on the boundary where evd gives wrong result
  expect_equal(density(dist, at), list(
    evd::dgpd(at, loc = 0, scale = 1, shape = 0),
    evd::dgpd(at, loc = 0.5, scale = 2, shape = 0.1),
    evd::dgpd(at, loc = 0, scale = 3, shape = 1.1)
  ))
  # CDF
  expect_equal(distributional::cdf(dist, at), list(
    evd::pgpd(at, loc = 0, scale = 1, shape = 0),
    evd::pgpd(at, loc = 0.5, scale = 2, shape = 0.1),
    evd::pgpd(at, loc = 0, scale = 3, shape = 1.1)
  ))
  # Quantiles
  p <- (1:19) / 20
  expect_equal(quantile(dist, p = p), list(
    evd::qgpd(p = p, loc = 0, scale = 1, shape = 0),
    evd::qgpd(p = p, loc = 0.5, scale = 2, shape = 0.1),
    evd::qgpd(p = p, loc = 0, scale = 3, shape = 1.1)
  ))
  # Generate
  set.seed(123)
  rand_dist <- distributional::generate(dist, times = 1e6)
  expect_equal(unlist(lapply(rand_dist[1:2], mean)),
    mean(dist)[1:2],
    tolerance = 0.01
  )
  expect_equal(unlist(lapply(rand_dist[1:2], var)),
    distributional::variance(dist)[1:2],
    tolerance = 0.01
  )
  expect_equal(unlist(lapply(rand_dist, median)),
    median(dist),
    tolerance = 0.01
  )
})
