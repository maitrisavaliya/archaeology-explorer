# Descriptive statistics implemented from scratch.

.clean <- function(x, na.rm = FALSE) {
  stopifnot(is.numeric(x))
  if (na.rm) x <- x[!is.na(x)]
  if (anyNA(x)) return(NULL)
  x
}

# Mean: (1/n) * sum(x_i)
my_mean <- function(x, na.rm = FALSE) {
  x <- .clean(x, na.rm); if (is.null(x)) return(NA_real_)
  sum(x) / length(x)
}

# Sample variance: sum((x_i - xbar)^2) / (n - 1)
my_var <- function(x, na.rm = FALSE) {
  x <- .clean(x, na.rm); if (is.null(x)) return(NA_real_)
  n <- length(x)
  if (n < 2) return(NA_real_)
  sum((x - my_mean(x))^2) / (n - 1)
}

my_sd <- function(x, na.rm = FALSE) sqrt(my_var(x, na.rm))

# Quantile, Type 7 (R default): h = (n - 1)p + 1, linear interpolation
my_quantile <- function(x, probs = c(0, .25, .5, .75, 1), na.rm = FALSE) {
  x <- .clean(x, na.rm); if (is.null(x)) return(rep(NA_real_, length(probs)))
  stopifnot(all(probs >= 0 & probs <= 1))
  x <- sort(x); n <- length(x)
  h  <- (n - 1) * probs + 1
  lo <- floor(h); hi <- ceiling(h)
  q  <- x[lo] + (h - lo) * (x[hi] - x[lo])
  names(q) <- paste0(probs * 100, "%")
  q
}

my_median <- function(x, na.rm = FALSE) unname(my_quantile(x, 0.5, na.rm))

my_iqr <- function(x, na.rm = FALSE) {
  q <- my_quantile(x, c(.25, .75), na.rm)
  unname(q[2] - q[1])
}

my_range <- function(x, na.rm = FALSE) {
  x <- .clean(x, na.rm); if (is.null(x)) return(c(NA_real_, NA_real_))
  c(min = min(x), max = max(x))
}

# Skewness (moment-based): m3 / m2^(3/2), m_k = (1/n) * sum((x - xbar)^k)
my_skewness <- function(x, na.rm = FALSE) {
  x <- .clean(x, na.rm); if (is.null(x)) return(NA_real_)
  d <- x - my_mean(x)
  m2 <- mean(d^2); m3 <- mean(d^3)
  m3 / m2^1.5
}

# Excess kurtosis: m4 / m2^2 - 3
my_kurtosis <- function(x, na.rm = FALSE) {
  x <- .clean(x, na.rm); if (is.null(x)) return(NA_real_)
  d <- x - my_mean(x)
  mean(d^4) / mean(d^2)^2 - 3
}

# Mode (most frequent value; returns all ties)
my_mode <- function(x, na.rm = FALSE) {
  if (na.rm) x <- x[!is.na(x)]
  tab <- table(x)
  as.numeric(names(tab)[tab == max(tab)])
}

# Standard error of the mean: s / sqrt(n)
my_se <- function(x, na.rm = FALSE) {
  x <- .clean(x, na.rm); if (is.null(x)) return(NA_real_)
  my_sd(x) / sqrt(length(x))
}

# One-call summary table
describe <- function(x, na.rm = TRUE) {
  data.frame(
    n        = sum(!is.na(x)),
    mean     = my_mean(x, na.rm),
    median   = my_median(x, na.rm),
    sd       = my_sd(x, na.rm),
    se       = my_se(x, na.rm),
    min      = my_range(x, na.rm)[["min"]],
    q1       = unname(my_quantile(x, .25, na.rm)),
    q3       = unname(my_quantile(x, .75, na.rm)),
    max      = my_range(x, na.rm)[["max"]],
    iqr      = my_iqr(x, na.rm),
    skewness = my_skewness(x, na.rm),
    kurtosis = my_kurtosis(x, na.rm)
  )
}