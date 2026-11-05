library(testthat)

root <- if (file.exists("R/descriptive.R")) "." else ".."
for (f in c("descriptive", "probability", "inference", "regression"))
  source(file.path(root, "R", paste0(f, ".R")))

# ---------- helper: compare, assert, and log max abs error ----------
.log <- new.env(); .log$rows <- list()

check <- function(name, mine, ref, tol = 1e-8) {
  mine <- as.numeric(mine); ref <- as.numeric(ref)
  expect_equal(length(mine), length(ref), info = paste(name, "- length mismatch"))
  d <- abs(mine - ref)
  d[which(mine == ref)] <- 0                      # Inf == Inf
  err <- if (length(d)) max(d) else NA_real_
  .log$rows[[length(.log$rows) + 1]] <-
    data.frame(check = name, max_abs_error = err, tolerance = tol)
  expect_true(is.finite(err) && err < tol,
              info = sprintf("%s: max abs error %.3e (tol %.0e)", name, err, tol))
}

check_htest <- function(label, mine, ref, est_ref, tol = 1e-9, tol_ci = 1e-7) {
  check(paste(label, "| statistic"), mine$statistic, ref$statistic, tol)
  check(paste(label, "| df"),        mine$df,        ref$parameter, tol)
  check(paste(label, "| p-value"),   mine$p_value,   ref$p.value,   tol)
  check(paste(label, "| CI"),        mine$conf_int,  ref$conf.int,  tol_ci)
  check(paste(label, "| estimate"),  mine$estimate,  est_ref,       tol)
}

# ---------- shared test data ----------
set.seed(42)
x      <- rnorm(60, 10, 3)
x_na   <- c(x[1:50], NA, x[51:60])
y      <- 2 + 0.5 * x + rnorm(60)
skew_x <- rexp(60)
a      <- rnorm(25, 10, 2)
b      <- rnorm(30, 11, 3)
d_pair <- a + rnorm(25, 0.3, 1)
x_s    <- rnorm(30, 0, 1)
y_s    <- rnorm(30, 1.5, 1)
dat    <- data.frame(y = y, x = x, w = rexp(60),
                     g = factor(sample(c("a", "b", "c"), 60, TRUE)))
probs  <- seq(0, 1, 0.1)

# =====================================================================
test_that("descriptive statistics match base R", {
  check("mean",     my_mean(x),   mean(x),   1e-12)
  check("variance", my_var(x),    var(x),    1e-12)
  check("sd",       my_sd(x),     sd(x),     1e-12)
  check("median",   my_median(x), median(x), 1e-12)
  check("IQR",      my_iqr(x),    IQR(x),    1e-12)
  check("range",    my_range(x),  range(x),  1e-12)
  check("quantile (type 7)", my_quantile(x, probs), quantile(x, probs, type = 7), 1e-12)
  check("std error", my_se(x), sd(x) / sqrt(length(x)), 1e-12)

  # NA handling
  expect_true(is.na(my_mean(x_na)))
  check("mean na.rm", my_mean(x_na, TRUE), mean(x_na, na.rm = TRUE), 1e-12)
  check("var na.rm",  my_var(x_na, TRUE),  var(x_na, na.rm = TRUE),  1e-12)

  # mode, edge cases, errors
  expect_equal(my_mode(c(1, 2, 2, 3, 3)), c(2, 3))
  expect_true(is.na(my_var(1)))
  expect_error(my_mean("a"))

  # describe() table
  dd <- describe(x_na)
  check("describe | n",    dd$n,    sum(!is.na(x_na)), 1e-12)
  check("describe | mean", dd$mean, mean(x_na, na.rm = TRUE), 1e-12)
  check("describe | q1",   dd$q1,   quantile(x_na, .25, na.rm = TRUE, type = 7), 1e-12)
})

test_that("skewness and kurtosis behave correctly", {
  expect_equal(my_skewness(-5:5), 0)                 # symmetric -> 0
  expect_equal(my_kurtosis(c(-1, 1, -1, 1)), -2)     # two-point distribution
  expect_gt(my_skewness(skew_x), 0.5)                # right-skewed sample
  skip_if_not_installed("e1071")
  check("skewness vs e1071", my_skewness(skew_x), e1071::skewness(skew_x, type = 1), 1e-10)
  check("kurtosis vs e1071", my_kurtosis(skew_x), e1071::kurtosis(skew_x, type = 1), 1e-10)
})

# =====================================================================
test_that("normal distribution matches base R", {
  z <- seq(-7, 7, by = 0.05)
  check("dnorm", my_dnorm(z), dnorm(z), 1e-14)
  check("pnorm", my_pnorm(z), pnorm(z), 1e-12)
  check("pnorm(mean=5, sd=2)", my_pnorm(seq(-3, 13, .5), 5, 2), pnorm(seq(-3, 13, .5), 5, 2), 1e-12)
  p <- c(1e-6, .001, .025, .1, .5, .9, .975, .999)
  check("qnorm", my_qnorm(p), qnorm(p), 1e-8)
  check("pnorm(qnorm(p)) round-trip", my_pnorm(my_qnorm(p)), p, 1e-12)
  expect_equal(my_qnorm(c(0, 1)), c(-Inf, Inf))
  expect_error(my_dnorm(0, sd = -1))
})

test_that("discrete and exponential distributions match base R", {
  k <- 0:20
  check("dbinom", my_dbinom(k, 20, .3), dbinom(k, 20, .3), 1e-13)
  qb <- c(-1, 0, 3.7, 10, 20, 25)
  check("pbinom", my_pbinom(qb, 20, .3), pbinom(qb, 20, .3), 1e-12)
  check("dpois",  my_dpois(0:30, 4.5), dpois(0:30, 4.5), 1e-13)
  qp <- c(-1, 0, 5, 20)
  check("ppois",  my_ppois(qp, 4.5), ppois(qp, 4.5), 1e-12)
  t <- seq(-1, 5, .25)
  check("dexp", my_dexp(t, 1.7), dexp(t, 1.7), 1e-14)
  check("pexp", my_pexp(t, 1.7), pexp(t, 1.7), 1e-14)
})

test_that("ECDF matches base R (including ties)", {
  grid <- seq(min(x) - 1, max(x) + 1, length.out = 200)
  check("ecdf", my_ecdf(x)(grid), ecdf(x)(grid), 1e-14)
  xr <- round(x)
  check("ecdf with ties", my_ecdf(xr)(unique(xr)), ecdf(xr)(unique(xr)), 1e-14)
})

test_that("CLT simulation converges to theory", {
  m <- clt_means(function(n) rexp(n, 1), n = 30, reps = 5000)
  expect_lt(abs(my_mean(m) - 1), 0.02)
  expect_lt(abs(my_sd(m) - 1 / sqrt(30)), 0.01)
})

# =====================================================================
test_that("t, chi-square and F CDFs match base R", {
  q <- seq(-6, 6, by = 0.25)
  for (df in c(1, 2, 5, 10, 30, 100))
    check(sprintf("pt df=%g", df), my_pt(q, df), pt(q, df), 1e-10)
  expect_equal(my_pt(c(-Inf, Inf), 5), c(0, 1))

  qc <- c(.01, .5, 1, 3, 5, 10, 20, 50, 100)
  for (df in c(1, 2, 5, 10, 50))
    check(sprintf("pchisq df=%g", df), my_pchisq(qc, df), pchisq(qc, df), 1e-10)

  qf <- c(0, .1, .5, 1, 2, 5, 10, 50)
  for (dfs in list(c(1, 1), c(3, 10), c(5, 20), c(10, 50), c(2, 100)))
    check(sprintf("pf df=(%g,%g)", dfs[1], dfs[2]),
          my_pf(qf, dfs[1], dfs[2]), pf(qf, dfs[1], dfs[2]), 1e-10)
})

test_that("t, chi-square and F quantiles match base R", {
  p <- c(.001, .025, .5, .9, .975, .999)
  for (df in c(2, 5, 30))
    check(sprintf("qt df=%g", df), my_qt(p, df), qt(p, df), 1e-6)
  pc <- c(.01, .05, .5, .95, .99)
  for (df in c(1, 5, 20))
    check(sprintf("qchisq df=%g", df), my_qchisq(pc, df), qchisq(pc, df), 1e-6)
  check("qf (4, 25)", my_qf(pc, 4, 25), qf(pc, 4, 25), 1e-6)
  check("pt(qt(p)) round-trip", my_pt(my_qt(p, 7), 7), p, 1e-9)
})

# =====================================================================
test_that("t-tests match t.test()", {
  r <- t.test(a, mu = 9)
  check_htest("one-sample", my_t_test(a, mu = 9), r, r$estimate)

  r <- t.test(a, mu = 9, alternative = "less")
  check_htest("one-sample less", my_t_test(a, mu = 9, alternative = "less"), r, r$estimate)

  r <- t.test(a, mu = 9, alternative = "greater")
  check_htest("one-sample greater", my_t_test(a, mu = 9, alternative = "greater"), r, r$estimate)

  r <- t.test(a, b)
  check_htest("Welch", my_t_test(a, b), r, r$estimate[1] - r$estimate[2])

  r <- t.test(a, b, var.equal = TRUE)
  check_htest("pooled", my_t_test(a, b, var_equal = TRUE), r, r$estimate[1] - r$estimate[2])

  r <- t.test(a, d_pair, paired = TRUE)
  check_htest("paired", my_t_test(a, d_pair, paired = TRUE), r, r$estimate)

  a2 <- a; a2[3] <- NA
  r <- t.test(a2, d_pair, paired = TRUE)
  check_htest("paired with NA", my_t_test(a2, d_pair, paired = TRUE), r, r$estimate)

  r <- t.test(a, b, conf.level = 0.99)
  check("Welch 99% CI", my_t_test(a, b, conf_level = 0.99)$conf_int, r$conf.int, 1e-7)
})

test_that("confidence intervals are correct", {
  check("CI mean", my_ci_mean(a), t.test(a)$conf.int, 1e-7)
  check("CI mean 90%", my_ci_mean(a, .90), t.test(a, conf.level = .90)$conf.int, 1e-7)

  check("Wilson CI (k=17, n=40)", my_ci_prop(17, 40),
        prop.test(17, 40, correct = FALSE)$conf.int, 1e-7)
  check("Wilson CI (k=0, n=25)", my_ci_prop(0, 25),
        prop.test(0, 25, correct = FALSE)$conf.int, 1e-7)

  n <- length(a); s2 <- var(a)
  check("CI variance", my_ci_var(a),
        c((n - 1) * s2 / qchisq(.975, n - 1), (n - 1) * s2 / qchisq(.025, n - 1)), 1e-6)
})

test_that("bootstrap and permutation tests behave", {
  bt <- boot_ci(a, B = 4000)
  check("bootstrap point estimate", bt$estimate, mean(a), 1e-12)
  expect_length(bt$replicates, 4000)
  expect_true(bt$ci[1] < mean(a) && mean(a) < bt$ci[2])
  w_ratio <- diff(bt$ci) / diff(t.test(a)$conf.int)
  expect_gt(w_ratio, 0.8); expect_lt(w_ratio, 1.2)
  expect_length(boot_ci(a, stat = my_median, B = 500)$replicates, 500)

  pt_res <- perm_test(x_s, y_s, B = 10000)
  check("perm observed stat", pt_res$observed, mean(x_s) - mean(y_s), 1e-12)
  expect_lt(pt_res$p_value, 0.001)               # strong effect
  expect_lt(t.test(x_s, y_s)$p.value, 0.001)
  pn <- perm_test(a, a)                          # identical samples: no evidence
  expect_gt(pn$p_value, 0.9)
})

test_that("chi-square test and one-way ANOVA match base R", {
  tab <- matrix(c(20, 15, 30, 25, 22, 18), nrow = 2)
  r <- chisq.test(tab, correct = FALSE)
  m <- my_chisq_test(tab)
  check("chisq statistic", m$statistic, r$statistic, 1e-10)
  check("chisq df",        m$df,        r$parameter, 1e-12)
  check("chisq p-value",   m$p_value,   r$p.value,   1e-10)
  check("chisq expected",  m$expected,  r$expected,  1e-10)

  s <- summary(aov(y ~ g, data = dat))[[1]]
  m <- my_anova(dat$y, dat$g)
  check("ANOVA F",       m$F,          s[["F value"]][1], 1e-9)
  check("ANOVA df",      m$df,         s$Df,              1e-12)
  check("ANOVA p-value", m$p_value,    s[["Pr(>F)"]][1],  1e-9)
  check("ANOVA SS",      c(m$ss_between, m$ss_within), s[["Sum Sq"]], 1e-8)
})

# =====================================================================
test_that("correlation matches base R", {
  check("covariance", my_cov(x, y), cov(x, y), 1e-12)
  check("Pearson",    my_cor(x, y), cor(x, y), 1e-12)
  xr <- round(x); yr <- round(y)
  check("rank (ties averaged)", my_rank(xr), rank(xr, ties.method = "average"), 1e-12)
  check("Spearman (with ties)", my_spearman(xr, yr), cor(xr, yr, method = "spearman"), 1e-12)

  ct <- cor.test(x, y)
  tt <- cor_tstat(my_cor(x, y), length(x))
  check("cor t-statistic", tt, ct$statistic, 1e-9)
  check("cor p-value", 2 * my_pt(-abs(tt), length(x) - 2), ct$p.value, 1e-12)

  y_na <- y; y_na[5] <- NA
  check("Pearson with NA", my_cor(x, y_na), cor(x, y_na, use = "complete.obs"), 1e-12)
})

test_that("OLS matches lm()", {
  fit <- my_lm(y ~ x + w, dat); ref <- lm(y ~ x + w, dat); s <- summary(ref)
  cf  <- fit$coefficients
  check("lm coefficients", cf$estimate,  coef(ref),           1e-8)
  check("lm std errors",   cf$std_error, s$coefficients[, 2], 1e-8)
  check("lm t-values",     cf$t_value,   s$coefficients[, 3], 1e-8)
  check("lm p-values",     cf$p_value,   s$coefficients[, 4], 1e-8)
  check("lm R2",           fit$r_squared,     s$r.squared,      1e-10)
  check("lm adj R2",       fit$adj_r_squared, s$adj.r.squared,  1e-10)
  check("lm sigma",        fit$sigma,         s$sigma,          1e-10)
  check("lm F-statistic",  fit$f_stat,        s$fstatistic[1],  1e-8)
  check("lm fitted",       fit$fitted,        fitted(ref),      1e-8)
  check("lm residuals",    fit$residuals,     resid(ref),       1e-8)
  expect_output(print(fit), "Residual SE")

  # factor predictor
  fit2 <- my_lm(y ~ x + g, dat); ref2 <- lm(y ~ x + g, dat)
  check("lm factor coefficients", fit2$coefficients$estimate, coef(ref2), 1e-8)
  check("lm factor p-values", fit2$coefficients$p_value,
        summary(ref2)$coefficients[, 4], 1e-8)

  # missing data
  dat_na <- dat; dat_na$x[c(3, 10)] <- NA
  fit3 <- my_lm(y ~ x + w, dat_na); ref3 <- lm(y ~ x + w, dat_na)
  expect_equal(fit3$n, nobs(ref3))
  check("lm with NA coefficients", fit3$coefficients$estimate, coef(ref3), 1e-8)
})

test_that("predictions and diagnostics match base R", {
  fit <- my_lm(y ~ x + w, dat); ref <- lm(y ~ x + w, dat)
  nd  <- data.frame(x = c(7, 10, 13), w = c(.2, 1, 2))

  check("predict point", predict_my_lm(fit, nd), predict(ref, nd), 1e-8)
  pr <- predict(ref, nd, interval = "confidence", level = 0.95)
  mp <- predict_my_lm(fit, nd, level = 0.95)
  check("predict CI fit", mp$fit, pr[, "fit"], 1e-8)
  check("predict CI lwr", mp$lwr, pr[, "lwr"], 1e-6)
  check("predict CI upr", mp$upr, pr[, "upr"], 1e-6)

  dg <- lm_diagnostics(fit)
  check("standardized residuals", dg$std_resid, rstandard(ref), 1e-8)
  check("leverage",               dg$leverage,  hatvalues(ref), 1e-10)
})

# =====================================================================
test_that("everything works on real archaeological data (DartPoints)", {
  skip_if_not_installed("archdata")
  utils::data("DartPoints", package = "archdata", envir = environment())
  dp  <- DartPoints
  top <- names(sort(table(dp$Name), decreasing = TRUE))[1:2]
  g1  <- dp$Length[dp$Name == top[1]]; g2 <- dp$Length[dp$Name == top[2]]

  r <- t.test(g1, g2)
  check_htest("DartPoints Welch", my_t_test(g1, g2), r, r$estimate[1] - r$estimate[2])

  check("DartPoints Length describe mean", describe(dp$Length)$mean,
        mean(dp$Length, na.rm = TRUE), 1e-12)

  s <- summary(aov(Length ~ Name, dp))[[1]]
  check("DartPoints ANOVA F", my_anova(dp$Length, dp$Name)$F, s[["F value"]][1], 1e-8)

  fit <- my_lm(Weight ~ Length + Width, dp); ref <- lm(Weight ~ Length + Width, dp)
  check("DartPoints lm coefficients", fit$coefficients$estimate, coef(ref), 1e-7)
  check("DartPoints lm p-values", fit$coefficients$p_value,
        summary(ref)$coefficients[, 4], 1e-7)
})

# ---------- write validation table for the Quarto report ----------
tab <- do.call(rbind, .log$rows)
dir.create(file.path(root, "data", "clean"), showWarnings = FALSE, recursive = TRUE)
write.csv(tab, file.path(root, "data", "clean", "validation_table.csv"), row.names = FALSE)
cat(sprintf("\n%d numerical checks | worst max-abs-error: %.2e (%s)\n",
            nrow(tab), max(tab$max_abs_error), tab$check[which.max(tab$max_abs_error)]))