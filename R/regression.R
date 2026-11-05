
# Covariance: sum((x - xbar)(y - ybar)) / (n - 1)
my_cov <- function(x, y) {
  stopifnot(length(x) == length(y))
  ok <- !is.na(x) & !is.na(y); x <- x[ok]; y <- y[ok]
  sum((x - sum(x) / length(x)) * (y - sum(y) / length(y))) / (length(x) - 1)
}

# Pearson r = cov(x, y) / (sd_x * sd_y)
my_cor <- function(x, y) {
  ok <- !is.na(x) & !is.na(y); x <- x[ok]; y <- y[ok]
  my_cov(x, y) / sqrt(my_cov(x, x) * my_cov(y, y))
}

# Ranks with ties averaged
my_rank <- function(x) {
  o <- order(x); r <- numeric(length(x)); i <- 1; n <- length(x)
  while (i <= n) {
    j <- i
    while (j < n && x[o[j + 1]] == x[o[i]]) j <- j + 1
    r[o[i:j]] <- (i + j) / 2
    i <- j + 1
  }
  r
}

# Spearman = Pearson on ranks
my_spearman <- function(x, y) {
  ok <- !is.na(x) & !is.na(y)
  my_cor(my_rank(x[ok]), my_rank(y[ok]))
}

# t statistic for H0: rho = 0  ->  t = r * sqrt((n-2)/(1-r^2)), df = n-2
cor_tstat <- function(r, n) r * sqrt((n - 2) / (1 - r^2))

# ---------- OLS via normal equations ----------
# beta_hat = (X'X)^{-1} X'y
# Var(beta_hat) = sigma2_hat (X'X)^{-1},  sigma2_hat = RSS / (n - p)
my_lm <- function(formula, data) {
  mf <- model.frame(formula, data)          # handles NA dropping + factors
  y  <- model.response(mf)
  X  <- model.matrix(formula, mf)
  n  <- nrow(X); p <- ncol(X)

  XtX_inv <- solve(crossprod(X))
  beta    <- as.vector(XtX_inv %*% crossprod(X, y))
  names(beta) <- colnames(X)

  fitted <- as.vector(X %*% beta)
  resid  <- as.vector(y - fitted)
  rss    <- sum(resid^2)
  tss    <- sum((y - sum(y) / n)^2)
  df     <- n - p
  sigma2 <- rss / df
  se     <- sqrt(diag(XtX_inv) * sigma2)
  tval   <- beta / se
  pval   <- if (exists("my_pt")) 2 * (1 - my_pt(abs(tval), df)) else rep(NA_real_, p)

  r2     <- 1 - rss / tss
  adj_r2 <- 1 - (rss / df) / (tss / (n - 1))
  fstat  <- ((tss - rss) / (p - 1)) / sigma2

  structure(list(
    coefficients = data.frame(estimate = beta, std_error = se,
                              t_value = tval, p_value = pval,
                              row.names = names(beta)),
    fitted = fitted, residuals = resid, X = X, y = y,
    XtX_inv = XtX_inv, sigma = sqrt(sigma2), df = df, n = n, p = p,
    rss = rss, tss = tss, r_squared = r2, adj_r_squared = adj_r2,
    f_stat = fstat, formula = formula
  ), class = "my_lm")
}

print.my_lm <- function(x, ...) {
  cat("Formula:", deparse(x$formula), "\n\n")
  print(round(x$coefficients, 5))
  cat(sprintf("\nResidual SE: %.4f on %d df\nR2: %.4f | Adj R2: %.4f | F: %.3f\n",
              x$sigma, x$df, x$r_squared, x$adj_r_squared, x$f_stat))
  invisible(x)
}

# Predictions with optional confidence interval for the mean response
# (needs my_qt from inference.R for intervals)
predict_my_lm <- function(fit, newdata, level = NULL) {
  Xn   <- model.matrix(delete.response(terms(fit$formula)), newdata)
  pred <- as.vector(Xn %*% fit$coefficients$estimate)
  if (is.null(level)) return(pred)
  se_fit <- sqrt(rowSums((Xn %*% fit$XtX_inv) * Xn) * fit$sigma^2)
  tcrit  <- my_qt(1 - (1 - level) / 2, fit$df)
  data.frame(fit = pred, lwr = pred - tcrit * se_fit, upr = pred + tcrit * se_fit)
}

# Diagnostics data frame (ready for ggplot residual plots)
lm_diagnostics <- function(fit) {
  h <- rowSums((fit$X %*% fit$XtX_inv) * fit$X)             # leverage
  data.frame(fitted = fit$fitted, residual = fit$residuals,
             std_resid = fit$residuals / (fit$sigma * sqrt(1 - h)),
             leverage = h)
}