---
title: "Archaeology Data Science Explorer"
subtitle: "Statistical methods implemented from scratch in R and validated against base R"
author: "Maitri Savaliya"
date: today
format:
  html:
    toc: true
    toc-depth: 2
    number-sections: true
    code-fold: true
    code-tools: true
    theme: cosmo
    embed-resources: true
execute:
  warning: false
  message: false
---

# Overview

This report implements core statistical methods **from first principles** in R, validates each against R's built-in functions, and applies them to real archaeological data (`DartPoints` from the `archdata` package: measurements of stone projectile points from Fort Hood, Texas).

Each section gives the mathematics, the function from this repository, and an application. Every number and figure is regenerated on render.

```{r setup}
library(ggplot2)
theme_set(theme_minimal(base_size = 12))

for (f in c("descriptive", "probability", "inference", "regression"))
  source(file.path("..", "R", paste0(f, ".R")))

library(archdata)
data(DartPoints)
dp <- DartPoints

# the two most common point types, used for group comparisons
top <- names(sort(table(dp$Name), decreasing = TRUE))[1:2]
g1  <- dp$Length[dp$Name == top[1]]
g2  <- dp$Length[dp$Name == top[2]]
```

```{r}
#| label: tbl-data
#| tbl-cap: "Dataset summary"
knitr::kable(data.frame(
  observations = nrow(dp),
  variables    = ncol(dp),
  point_types  = nlevels(factor(dp$Name)),
  missing_Length = sum(is.na(dp$Length)),
  missing_Weight = sum(is.na(dp$Weight))
))
```

# Descriptive statistics

## Mathematics

For observations $x_1,\dots,x_n$:

$$\bar{x}=\frac1n\sum_{i=1}^n x_i,\qquad s^2=\frac{1}{n-1}\sum_{i=1}^n (x_i-\bar{x})^2,\qquad \mathrm{SE}=\frac{s}{\sqrt n}$$

The divisor $n-1$ (Bessel's correction) makes $s^2$ an unbiased estimator of $\sigma^2$, because $\bar{x}$ is fitted to the same data and the deviations from it are systematically smaller than deviations from the true mean.

Quantiles use R's default (type 7): for probability $p$, let $h=(n-1)p+1$ and interpolate linearly between the order statistics $x_{(\lfloor h\rfloor)}$ and $x_{(\lceil h\rceil)}$.

Skewness and excess kurtosis are standardized central moments, with $m_k=\frac1n\sum(x_i-\bar{x})^k$:

$$g_1=\frac{m_3}{m_2^{3/2}},\qquad g_2=\frac{m_4}{m_2^{2}}-3$$

## Application

```{r}
#| label: tbl-describe
#| tbl-cap: "Summary of point length (mm), computed with `describe()`"
knitr::kable(describe(dp$Length), digits = 3)
```

```{r}
#| label: fig-hist
#| fig-cap: "Distribution of dart point length with the empirical CDF (right) from `my_ecdf()`."
#| fig-width: 9
#| fig-height: 3.6
L <- dp$Length[!is.na(dp$Length)]
Fn <- my_ecdf(L)
grid <- seq(min(L), max(L), length.out = 300)

p1 <- ggplot(data.frame(L), aes(L)) +
  geom_histogram(aes(y = after_stat(density)), bins = 20, fill = "grey75", colour = "white") +
  stat_function(fun = my_dnorm, args = list(mean = my_mean(L), sd = my_sd(L)),
                colour = "firebrick", linewidth = 1) +
  labs(x = "Length (mm)", y = "Density", title = "Histogram + fitted normal")

p2 <- ggplot(data.frame(x = grid, F = Fn(grid)), aes(x, F)) +
  geom_step() + labs(x = "Length (mm)", y = expression(F[n](x)), title = "Empirical CDF")

gridExtra::grid.arrange(p1, p2, ncol = 2)
```

Skewness is `r round(my_skewness(L), 2)` and excess kurtosis is `r round(my_kurtosis(L), 2)`. A positive skew means a longer right tail than a normal curve would have, which matters for the normality assumptions in later sections.

# Probability and the Central Limit Theorem

## Mathematics

The normal density and CDF are

$$\varphi(x)=\frac{1}{\sigma\sqrt{2\pi}}e^{-\frac{(x-\mu)^2}{2\sigma^2}},\qquad
\Phi(z)=\frac12+\varphi(z)\sum_{k=0}^{\infty}\frac{z^{2k+1}}{1\cdot3\cdot5\cdots(2k+1)}$$

The series for $\Phi$ is evaluated until terms fall below machine precision. Quantiles are found by bisection on the CDF. The binomial and Poisson pmfs are

$$P(X=k)=\binom nk p^k(1-p)^{n-k},\qquad P(X=k)=\frac{\lambda^k e^{-\lambda}}{k!}$$

**Central Limit Theorem.** If $X_i$ are i.i.d. with mean $\mu$ and finite variance $\sigma^2$, then

$$\sqrt n\,\frac{\bar{X}_n-\mu}{\sigma}\;\xrightarrow{d}\;\mathcal N(0,1)$$

## Simulation

The source distribution below is deliberately skewed (Exponential(1), $\mu=\sigma=1$). Sample means are drawn with `clt_means()`.

```{r}
#| label: fig-clt
#| fig-cap: "Sampling distribution of the mean of Exponential(1) draws. The red curve is the CLT prediction $\\mathcal{N}(1, 1/\\sqrt{n})$."
#| fig-width: 9
#| fig-height: 5
sizes <- c(1, 5, 30, 100)
df <- do.call(rbind, lapply(sizes, function(n)
  data.frame(n = factor(paste("n =", n), levels = paste("n =", sizes)),
             xbar = clt_means(function(k) rexp(k, 1), n, reps = 4000))))

curves <- do.call(rbind, lapply(sizes, function(n) {
  xs <- seq(0, 3.5, length.out = 300)
  data.frame(n = factor(paste("n =", n), levels = paste("n =", sizes)),
             x = xs, y = my_dnorm(xs, 1, 1 / sqrt(n)))
}))

ggplot(df, aes(xbar)) +
  geom_histogram(aes(y = after_stat(density)), bins = 40, fill = "grey75", colour = "white") +
  geom_line(data = curves, aes(x, y), colour = "firebrick", linewidth = 0.9) +
  coord_cartesian(xlim = c(0, 3.5)) +
  facet_wrap(~n, scales = "free_y") +
  labs(x = "sample mean", y = "density")
```

```{r}
#| label: tbl-clt
#| tbl-cap: "Empirical vs. theoretical standard error of the sample mean"
knitr::kable(do.call(rbind, lapply(sizes, function(n) {
  m <- clt_means(function(k) rexp(k, 1), n, reps = 4000)
  data.frame(n = n, empirical_sd = my_sd(m), theory_sd = 1 / sqrt(n),
             skewness = my_skewness(m))
})), digits = 3)
```

The empirical standard deviation tracks $\sigma/\sqrt n$ at every $n$, and the skewness of the sampling distribution shrinks toward 0, which is the CLT in action.

# Correlation and linear regression

## Mathematics

Sample covariance and Pearson's correlation:

$$\mathrm{cov}(x,y)=\frac{1}{n-1}\sum(x_i-\bar x)(y_i-\bar y),\qquad r=\frac{\mathrm{cov}(x,y)}{s_x s_y}$$

Spearman's $\rho$ is Pearson's $r$ applied to the ranks (ties receive average ranks), so it measures monotone rather than linear association. Under $H_0:\rho=0$,

$$t=r\sqrt{\frac{n-2}{1-r^2}}\sim t_{n-2}$$

**Ordinary least squares.** For $y=X\beta+\varepsilon$ with $X\in\mathbb R^{n\times p}$, minimizing $\lVert y-X\beta\rVert^2$ gives the normal equations:

$$\hat\beta=(X^\top X)^{-1}X^\top y,\qquad
\widehat{\mathrm{Var}}(\hat\beta)=\hat\sigma^2 (X^\top X)^{-1},\qquad
\hat\sigma^2=\frac{\mathrm{RSS}}{n-p}$$

with $R^2=1-\mathrm{RSS}/\mathrm{TSS}$ and coefficient tests $t_j=\hat\beta_j/\mathrm{SE}(\hat\beta_j)\sim t_{n-p}$. The confidence interval for the mean response at $x_0$ is

$$x_0^\top\hat\beta\;\pm\;t_{n-p,\,1-\alpha/2}\;\hat\sigma\sqrt{x_0^\top (X^\top X)^{-1}x_0}$$

## Application: does point length predict weight?

```{r}
#| label: tbl-cor
#| tbl-cap: "Association between Length and Weight"
r  <- my_cor(dp$Length, dp$Weight)
n  <- sum(complete.cases(dp$Length, dp$Weight))
tt <- cor_tstat(r, n)
knitr::kable(data.frame(
  pearson  = r,
  spearman = my_spearman(dp$Length, dp$Weight),
  t_stat   = tt,
  p_value  = 2 * my_pt(-abs(tt), n - 2)
), digits = 4)
```

```{r}
#| label: fit
fit <- my_lm(Weight ~ Length, dp)
fit
```

```{r}
#| label: fig-reg
#| fig-cap: "OLS fit from `my_lm()` with 95% confidence band from `predict_my_lm()`."
#| fig-width: 6.5
#| fig-height: 4.2
nd  <- data.frame(Length = seq(min(dp$Length), max(dp$Length), length.out = 100))
band <- cbind(nd, predict_my_lm(fit, nd, level = 0.95))

ggplot(dp, aes(Length, Weight)) +
  geom_point(alpha = 0.6) +
  geom_ribbon(data = band, aes(y = fit, ymin = lwr, ymax = upr), alpha = 0.25, fill = "steelblue") +
  geom_line(data = band, aes(y = fit), colour = "steelblue", linewidth = 1) +
  labs(x = "Length (mm)", y = "Weight (g)")
```

Length explains `r round(100 * fit$r_squared, 1)`% of the variance in weight ($R^2$ = `r round(fit$r_squared, 3)`). The slope is `r round(fit$coefficients["Length", "estimate"], 3)` g per mm (SE `r round(fit$coefficients["Length", "std_error"], 3)`).

## Diagnostics

```{r}
#| label: fig-diag
#| fig-cap: "Residuals vs. fitted values (left) and standardized residual Q-Q plot (right). Curvature or a funnel shape signals a violated assumption."
#| fig-width: 9
#| fig-height: 3.6
dg <- lm_diagnostics(fit)
dg <- dg[order(dg$std_resid), ]
dg$theoretical <- my_qnorm((seq_len(nrow(dg)) - 0.5) / nrow(dg))

a1 <- ggplot(lm_diagnostics(fit), aes(fitted, residual)) +
  geom_point(alpha = 0.6) + geom_hline(yintercept = 0, linetype = 2) +
  labs(x = "Fitted", y = "Residual")
a2 <- ggplot(dg, aes(theoretical, std_resid)) +
  geom_point(alpha = 0.6) + geom_abline(slope = 1, intercept = 0, colour = "firebrick") +
  labs(x = "Theoretical quantile", y = "Standardized residual")
gridExtra::grid.arrange(a1, a2, ncol = 2)
```

Weight typically scales with volume, so a funnel in the residuals would be unsurprising here. If it appears, a log transform of both variables is the natural next model, and `my_lm()` accepts `log(Weight) ~ log(Length)` unchanged.

```{r}
#| label: tbl-multi
#| tbl-cap: "Multiple regression: Weight ~ Length + Width + Thickness"
my_lm(Weight ~ Length + Width + Thickness, dp)$coefficients |>
  knitr::kable(digits = 4)
```

# Hypothesis testing and confidence intervals

## Mathematics

**One- and two-sample $t$-tests.** For a one-sample test of $H_0:\mu=\mu_0$,

$$t=\frac{\bar x-\mu_0}{s/\sqrt n}\sim t_{n-1}$$

For two independent samples, Welch's statistic does not assume equal variances:

$$t=\frac{\bar x_1-\bar x_2}{\sqrt{s_1^2/n_1+s_2^2/n_2}},\qquad
\nu=\frac{\left(\frac{s_1^2}{n_1}+\frac{s_2^2}{n_2}\right)^2}{\frac{(s_1^2/n_1)^2}{n_1-1}+\frac{(s_2^2/n_2)^2}{n_2-1}}$$

The $t$, $\chi^2$ and $F$ CDFs come from the regularized incomplete beta and gamma functions, evaluated with continued fractions:

$$P(T\le t)=1-\tfrac12 I_{\nu/(\nu+t^2)}\!\left(\tfrac\nu2,\tfrac12\right)\ \ (t>0)$$

A $(1-\alpha)$ confidence interval is $\hat\theta\pm t_{\nu,1-\alpha/2}\,\mathrm{SE}$, and it means that **95% of intervals built this way cover the true parameter**, not that the parameter has 95% probability of lying in this particular interval.

**Resampling.** The bootstrap approximates the sampling distribution of any statistic by resampling the data with replacement. The permutation test builds the null distribution by shuffling group labels, so it needs no distributional assumption.

## Application: are the two most common point types different in length?

```{r}
#| label: tbl-compare
#| tbl-cap: !expr 'paste0("Length of ", top[1], " (n = ", length(g1), ") vs ", top[2], " (n = ", length(g2), ")")'
tt  <- my_t_test(g1, g2)
bt  <- boot_ci(g1, B = 5000)
pm  <- perm_test(g1, g2, B = 10000)

knitr::kable(data.frame(
  method   = c("Welch t-test", "Permutation test"),
  estimate = c(tt$estimate, pm$observed),
  p_value  = c(tt$p_value, pm$p_value)
), digits = 4)
```

```{r}
#| label: fig-groups
#| fig-cap: "Length by point type, and the permutation null distribution for the difference in means."
#| fig-width: 9
#| fig-height: 3.6
sub <- dp[dp$Name %in% top, ]
b1 <- ggplot(sub, aes(Name, Length, fill = Name)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA, show.legend = FALSE) +
  geom_jitter(width = 0.1, alpha = 0.6) + labs(x = NULL, y = "Length (mm)")
b2 <- ggplot(data.frame(d = pm$replicates), aes(d)) +
  geom_histogram(bins = 40, fill = "grey75", colour = "white") +
  geom_vline(xintercept = pm$observed, colour = "firebrick", linewidth = 1) +
  labs(x = "difference in means under H0", y = "count")
gridExtra::grid.arrange(b1, b2, ncol = 2)
```

```{r}
#| label: tbl-ci
#| tbl-cap: !expr 'paste0("95% confidence intervals for mean length of ", top[1])'
knitr::kable(data.frame(
  method = c("t interval", "Bootstrap percentile"),
  lower  = c(my_ci_mean(g1)[1], bt$ci[1]),
  upper  = c(my_ci_mean(g1)[2], bt$ci[2])
), digits = 3)
```

## Does the test control the error rate? A simulation

If the implementation is correct, then under $H_0$ the $p$-values should be $\mathrm{Uniform}(0,1)$, so the rejection rate at $\alpha=0.05$ should be about 5%.

```{r}
#| label: fig-pvals
#| fig-cap: "p-values from 1,000 Welch tests on data where $H_0$ is true. A flat histogram means the test is well calibrated."
#| fig-width: 6
#| fig-height: 3.5
set.seed(7)
pvals <- replicate(1000, my_t_test(rnorm(15, 0, 1), rnorm(20, 0, 2))$p_value)

ggplot(data.frame(p = pvals), aes(p)) +
  geom_histogram(breaks = seq(0, 1, 0.05), fill = "grey75", colour = "white") +
  geom_hline(yintercept = 1000 * 0.05, linetype = 2, colour = "firebrick") +
  labs(x = "p-value", y = "count")
```

Empirical type I error at $\alpha=0.05$: **`r round(mean(pvals < 0.05), 3)`**.

## Categorical association and ANOVA

```{r}
#| label: anova
an <- my_anova(dp$Length, dp$Name)
```

A one-way ANOVA across all point types gives $F(`r an$df[1]`, `r an$df[2]`) =$ `r round(an$F, 2)`, $p =$ `r signif(an$p_value, 3)`, using

$$F=\frac{\mathrm{SS}_{\text{between}}/(k-1)}{\mathrm{SS}_{\text{within}}/(n-k)}$$

# Validation against base R

Every function above is checked against its base R counterpart in `tests/validate.R` (`mean`, `var`, `quantile`, `pnorm`, `qt`, `t.test`, `lm`, `chisq.test`, `aov`, and others). The suite records the maximum absolute error of each comparison.

```{r}
#| label: tbl-validation
#| tbl-cap: "Worst-case absolute error per check (all pass their tolerance)"
v <- read.csv(file.path("..", "data", "clean", "validation_table.csv"))
v$max_abs_error <- format(v$max_abs_error, scientific = TRUE, digits = 2)
v$tolerance     <- format(v$tolerance, scientific = TRUE, digits = 1)
knitr::kable(v)
```

```{r}
#| label: fig-validation
#| fig-cap: "Maximum absolute error of each from-scratch function relative to base R (log scale)."
#| fig-height: 7
v2 <- read.csv(file.path("..", "data", "clean", "validation_table.csv"))
v2$max_abs_error <- pmax(v2$max_abs_error, 1e-17)   # show exact matches on a log axis
v2$check <- factor(v2$check, levels = rev(v2$check))
ggplot(v2, aes(max_abs_error, check)) +
  geom_point() +
  geom_point(aes(x = tolerance), shape = 4, colour = "firebrick") +
  scale_x_log10() +
  labs(x = "max absolute error (red × = tolerance)", y = NULL)
```

# Limitations and extensions

- The $t$, $\chi^2$ and $F$ quantiles use bisection, which is simple and reliable but slower than the algorithms in base R.
- The normal-theory methods assume independent observations and approximately normal errors; the diagnostics and the resampling methods are there to check this.
- Natural extensions: weighted least squares, robust regression, multiple-testing corrections (Bonferroni, Benjamini–Hochberg), and a Bayesian version of the group comparison.

# Reproducibility

```{r}
sessionInfo()
```