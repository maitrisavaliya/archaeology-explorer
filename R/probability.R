# Distributions, ECDF, and CLT simulation from scratch.

# ---------- Normal ----------
my_dnorm <- function(x, mean = 0, sd = 1) {
  stopifnot(sd > 0)
  z <- (x - mean) / sd
  exp(-0.5 * z^2) / (sd * sqrt(2 * pi))
}

# CDF via the series  Phi(z) = 1/2 + phi(z) * sum_{k>=0} z^(2k+1) / (1*3*5*...*(2k+1))
# Accurate to ~1e-15 for |z| <= 8; beyond that returns 0 or 1.
.phi_std <- function(z) {
  if (is.na(z)) return(NA_real_)
  if (z >  8) return(1)
  if (z < -8) return(0)
  term <- z; total <- z; k <- 0
  while (abs(term) > 1e-17 * abs(total) && k < 1000) {
    k <- k + 1
    term <- term * z^2 / (2 * k + 1)
    total <- total + term
  }
  0.5 + total * exp(-0.5 * z^2) / sqrt(2 * pi)
}

my_pnorm <- function(q, mean = 0, sd = 1) {
  stopifnot(sd > 0)
  vapply((q - mean) / sd, .phi_std, numeric(1))
}

# Quantile function via bisection on the CDF
my_qnorm <- function(p, mean = 0, sd = 1, tol = 1e-12) {
  stopifnot(sd > 0)
  one <- function(pp) {
    if (is.na(pp) || pp < 0 || pp > 1) return(NA_real_)
    if (pp == 0) return(-Inf)
    if (pp == 1) return(Inf)
    lo <- -10; hi <- 10
    while (hi - lo > tol) {
      mid <- (lo + hi) / 2
      if (.phi_std(mid) < pp) lo <- mid else hi <- mid
    }
    (lo + hi) / 2
  }
  mean + sd * vapply(p, one, numeric(1))
}

# ---------- Binomial ----------
my_dbinom <- function(k, size, prob) {
  stopifnot(prob >= 0, prob <= 1, size >= 0)
  ifelse(k < 0 | k > size | k != floor(k), 0,
         choose(size, k) * prob^k * (1 - prob)^(size - k))
}

my_pbinom <- function(q, size, prob) {
  vapply(q, function(qq) {
    if (qq < 0) return(0)
    if (qq >= size) return(1)
    sum(my_dbinom(0:floor(qq), size, prob))
  }, numeric(1))
}

# ---------- Poisson ----------
my_dpois <- function(k, lambda) {
  stopifnot(lambda > 0)
  ifelse(k < 0 | k != floor(k), 0, exp(-lambda + k * log(lambda) - lgamma(k + 1)))
}

my_ppois <- function(q, lambda) {
  vapply(q, function(qq) {
    if (qq < 0) return(0)
    sum(my_dpois(0:floor(qq), lambda))
  }, numeric(1))
}

# ---------- Exponential ----------
my_dexp <- function(x, rate = 1) ifelse(x < 0, 0, rate * exp(-rate * x))
my_pexp <- function(q, rate = 1) ifelse(q < 0, 0, 1 - exp(-rate * q))

# ---------- Empirical CDF ----------
# F_n(t) = (1/n) * #{x_i <= t}
my_ecdf <- function(x) {
  x <- sort(x[!is.na(x)]); n <- length(x)
  function(t) vapply(t, function(tt) sum(x <= tt) / n, numeric(1))
}

# ---------- Central Limit Theorem simulation ----------
# Draws `reps` samples of size n from `rgen`, returns the vector of sample means.
clt_means <- function(rgen, n, reps = 5000, seed = 1) {
  set.seed(seed)
  replicate(reps, my_mean_base(rgen(n)))
}
my_mean_base <- function(x) sum(x) / length(x)

