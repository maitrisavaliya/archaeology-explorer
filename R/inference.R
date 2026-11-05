
# Regularized incomplete beta I_x(a, b) via Lentz continued fraction
.betacf <- function(x, a, b) {
  eps <- 1e-15; fpmin <- 1e-300
  qab <- a + b; qap <- a + 1; qam <- a - 1
  c <- 1; d <- 1 - qab * x / qap
  if (abs(d) < fpmin) d <- fpmin
  d <- 1 / d; h <- d
  for (m in 1:1000) {
    m2 <- 2 * m
    aa <- m * (b - m) * x / ((qam + m2) * (a + m2))
    d <- 1 + aa * d; if (abs(d) < fpmin) d <- fpmin
    c <- 1 + aa / c; if (abs(c) < fpmin) c <- fpmin
    d <- 1 / d; h <- h * d * c
    aa <- -(a + m) * (qab + m) * x / ((a + m2) * (qap + m2))
    d <- 1 + aa * d; if (abs(d) < fpmin) d <- fpmin
    c <- 1 + aa / c; if (abs(c) < fpmin) c <- fpmin
    d <- 1 / d; del <- d * c; h <- h * del
    if (abs(del - 1) < eps) break
  }
  h
}

.ibeta <- function(x, a, b) {
  if (x <= 0) return(0)
  if (x >= 1) return(1)
  bt <- exp(lgamma(a + b) - lgamma(a) - lgamma(b) + a * log(x) + b * log1p(-x))
  if (x < (a + 1) / (a + b + 2)) bt * .betacf(x, a, b) / a
  else 1 - bt * .betacf(1 - x, b, a) / b
}

# Regularized lower incomplete gamma P(a, x): series (x < a+1) or continued fraction
.pgamma_reg <- function(a, x) {
  if (x <= 0) return(0)
  eps <- 1e-15; fpmin <- 1e-300
  lg <- -x + a * log(x) - lgamma(a)
  if (x < a + 1) {
    ap <- a; del <- 1 / a; s <- del
    for (i in 1:2000) {
      ap <- ap + 1; del <- del * x / ap; s <- s + del
      if (abs(del) < abs(s) * eps) break
    }
    s * exp(lg)
  } else {
    b <- x + 1 - a; c <- 1 / fpmin; d <- 1 / b; h <- d
    for (i in 1:2000) {
      an <- -i * (i - a); b <- b + 2
      d <- an * d + b; if (abs(d) < fpmin) d <- fpmin
      c <- b + an / c; if (abs(c) < fpmin) c <- fpmin
      d <- 1 / d; del <- d * c; h <- h * del
      if (abs(del - 1) < eps) break
    }
    1 - exp(lg) * h
  }
}

# Distributions 
# Student t:  P(|T| > |t|) = I_{df/(df+t^2)}(df/2, 1/2)
my_pt <- function(q, df) {
  vapply(q, function(t) {
    if (is.infinite(t)) return(as.numeric(t > 0))
    tail <- 0.5 * .ibeta(df / (df + t^2), df / 2, 0.5)
    if (t > 0) 1 - tail else tail
  }, numeric(1))
}

my_pchisq <- function(q, df) vapply(q, function(v) .pgamma_reg(df / 2, v / 2), numeric(1))

# F:  P(F <= f) = I_{d1 f/(d1 f + d2)}(d1/2, d2/2)
my_pf <- function(q, df1, df2) {
  vapply(q, function(f) if (f <= 0) 0 else .ibeta(df1 * f / (df1 * f + df2), df1 / 2, df2 / 2),
         numeric(1))
}

# Generic quantile by bisection on a monotone CDF
.qbisect <- function(p, cdf, lo, hi, tol = 1e-10) {
  vapply(p, function(pp) {
    if (is.na(pp) || pp < 0 || pp > 1) return(NA_real_)
    if (pp == 0) return(lo); if (pp == 1) return(hi)
    a <- lo; b <- hi
    if (is.infinite(a)) { a <- -1; while (cdf(a) > pp) a <- a * 2 }
    if (is.infinite(b)) { b <- 1;  while (cdf(b) < pp) b <- b * 2 }
    while (b - a > tol * max(1, abs(a), abs(b))) {
      m <- (a + b) / 2
      if (cdf(m) < pp) a <- m else b <- m
    }
    (a + b) / 2
  }, numeric(1))
}

my_qt     <- function(p, df)       .qbisect(p, function(x) my_pt(x, df), -Inf, Inf)
my_qchisq <- function(p, df)       .qbisect(p, function(x) my_pchisq(x, df), 0, Inf)
my_qf     <- function(p, df1, df2) .qbisect(p, function(x) my_pf(x, df1, df2), 0, Inf)

#  t-tests 
# One-sample, paired, Welch (default), or pooled two-sample t-test.
my_t_test <- function(x, y = NULL, mu = 0, paired = FALSE, var_equal = FALSE,
                      alternative = c("two.sided", "less", "greater"),
                      conf_level = 0.95) {
  alternative <- match.arg(alternative)
  alternative <- match.arg(alternative)
  if (paired) {
    stopifnot(!is.null(y), length(x) == length(y))
    ok <- !is.na(x) & !is.na(y)
    x <- x[ok] - y[ok]; y <- NULL
  } else {
    x <- x[!is.na(x)]
    if (!is.null(y)) y <- y[!is.na(y)]
  }

  if (is.null(y)) {
    n <- length(x); est <- my_mean(x); se <- sqrt(my_var(x) / n); df <- n - 1
    method <- if (paired) "Paired t-test" else "One-sample t-test"
  } else {
    n1 <- length(x); n2 <- length(y)
    v1 <- my_var(x); v2 <- my_var(y)
    est <- my_mean(x) - my_mean(y)
    if (var_equal) {
      df <- n1 + n2 - 2
      sp2 <- ((n1 - 1) * v1 + (n2 - 1) * v2) / df
      se <- sqrt(sp2 * (1 / n1 + 1 / n2)); method <- "Two-sample t-test (pooled)"
    } else {
      se2 <- v1 / n1 + v2 / n2; se <- sqrt(se2)
      df <- se2^2 / ((v1 / n1)^2 / (n1 - 1) + (v2 / n2)^2 / (n2 - 1))
      method <- "Welch two-sample t-test"
    }
  }

  tstat <- (est - mu) / se
  p <- switch(alternative,
              two.sided = 2 * my_pt(-abs(tstat), df),
              less      = my_pt(tstat, df),
              greater   = 1 - my_pt(tstat, df))
  ci <- switch(alternative,
    two.sided = est + c(-1, 1) * my_qt(1 - (1 - conf_level) / 2, df) * se,
    less      = c(-Inf, est + my_qt(conf_level, df) * se),
    greater   = c(est - my_qt(conf_level, df) * se, Inf))

  structure(list(method = method, estimate = est, null_value = mu, se = se,
                 statistic = tstat, df = df, p_value = p, conf_int = ci,
                 conf_level = conf_level, alternative = alternative),
            class = "my_htest")
}

print.my_htest <- function(x, ...) {
  cat(x$method, "\n")
  cat(sprintf("estimate = %.4f | t = %.4f | df = %.2f | p = %.4g\n",
              x$estimate, x$statistic, x$df, x$p_value))
  cat(sprintf("%g%% CI: [%.4f, %.4f]\n", 100 * x$conf_level, x$conf_int[1], x$conf_int[2]))
  invisible(x)
}

# ================= Confidence intervals =================
my_ci_mean <- function(x, level = 0.95) {
  x <- x[!is.na(x)]; n <- length(x)
  m <- my_mean(x); se <- sqrt(my_var(x) / n)
  m + c(lower = -1, upper = 1) * my_qt(1 - (1 - level) / 2, n - 1) * se
}

# Wilson score interval for a proportion
my_ci_prop <- function(k, n, level = 0.95) {
  z <- my_qnorm(1 - (1 - level) / 2); p <- k / n
  centre <- (p + z^2 / (2 * n)) / (1 + z^2 / n)
  half   <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / (1 + z^2 / n)
  c(lower = centre - half, upper = centre + half)
}

# CI for variance via chi-square
my_ci_var <- function(x, level = 0.95) {
  x <- x[!is.na(x)]; n <- length(x); s2 <- my_var(x); a <- 1 - level
  c(lower = (n - 1) * s2 / my_qchisq(1 - a / 2, n - 1),
    upper = (n - 1) * s2 / my_qchisq(a / 2, n - 1))
}

# ================= Resampling =================
# Percentile bootstrap CI for any statistic
boot_ci <- function(x, stat = my_mean, B = 5000, level = 0.95, seed = 1) {
  set.seed(seed)
  x <- x[!is.na(x)]
  reps <- replicate(B, stat(sample(x, length(x), replace = TRUE)))
  a <- 1 - level
  list(estimate = stat(x),
       ci = my_quantile(reps, c(a / 2, 1 - a / 2)),
       replicates = reps)
}

# Two-sample permutation test (default statistic: difference in means)
perm_test <- function(x, y, stat = function(a, b) my_mean(a) - my_mean(b),
                      B = 10000, seed = 1) {
  set.seed(seed)
  x <- x[!is.na(x)]; y <- y[!is.na(y)]
  obs <- stat(x, y); pooled <- c(x, y); n1 <- length(x)
  reps <- replicate(B, { idx <- sample.int(length(pooled), n1); stat(pooled[idx], pooled[-idx]) })
  list(observed = obs,
       p_value = (1 + sum(abs(reps) >= abs(obs))) / (B + 1),
       replicates = reps)
}

# ================= Chi-square test of independence =================
my_chisq_test <- function(tab) {
  tab <- as.matrix(tab); n <- sum(tab)
  expected <- outer(rowSums(tab), colSums(tab)) / n
  stat <- sum((tab - expected)^2 / expected)
  df <- (nrow(tab) - 1) * (ncol(tab) - 1)
  list(statistic = stat, df = df, p_value = 1 - my_pchisq(stat, df), expected = expected)
}

# ================= One-way ANOVA =================
my_anova <- function(y, g) {
  ok <- !is.na(y) & !is.na(g); y <- y[ok]; g <- factor(g[ok])
  n <- length(y); k <- nlevels(g); grand <- my_mean(y)
  ni <- tapply(y, g, length); mi <- tapply(y, g, my_mean)
  ssb <- sum(ni * (mi - grand)^2); ssw <- sum((y - mi[as.integer(g)])^2)
  f <- (ssb / (k - 1)) / (ssw / (n - k))
  list(F = f, df = c(k - 1, n - k), p_value = 1 - my_pf(f, k - 1, n - k),
       ss_between = ssb, ss_within = ssw)
}