# Archaeology Statistics Explorer

An interactive R Shiny app for exploring archaeological-style data and learning how statistical methods work. Explore a sample collection or upload a CSV, visualize probability distributions, simulate the Central Limit Theorem, study relationships between measurements, and run common hypothesis tests.

The app is also a companion to the statistical methods implemented from scratch in `R/`. Its analyses call those project functions so users can explore the results of the implementations directly.

## Features

### Explore

- Starts with reproducible synthetic archaeology-themed data.
- Accepts user-provided CSV files.
- Summarizes numeric columns with count, mean, median, standard deviation, quartiles, range, skewness, and kurtosis.
- Shows category counts, a variable distribution plot, and a preview of the data.

### Distributions/CLT

- Visualizes density or probability mass and cumulative distribution for normal, binomial, Poisson, and exponential distributions.
- Allows distribution parameters to be changed interactively.
- Simulates sample means from those population distributions and displays their sampling distribution, mean, and standard deviation.

### Correlation and Regression

- Plots two selected numeric variables with an ordinary least squares fit.
- Reports Pearson and Spearman correlations, R-squared, adjusted R-squared, and model coefficients.

### Hypothesis Tests

- One-sample, Welch or pooled two-sample, and paired t-tests.
- Chi-square test of independence for two categorical variables.
- One-way ANOVA for a numeric outcome and categorical groups.
- Displays test statistics, p-values, estimates, and confidence intervals where applicable.

## Run locally

Open the project folder in RStudio. Restore the project-specific R packages if needed, then launch the app:

```r
renv::restore()
shiny::runApp("app")
```

Packages are managed by [`renv`](https://rstudio.github.io/renv/) and their versions are recorded in `renv.lock`.

## Use your own data

In the app's **Data source** panel, choose a CSV file with a header row. The app reads it as a data frame. Numeric columns are offered for numeric summaries, correlations, regression, and t-tests. Character, factor, and logical columns are offered as categories for chi-square tests and ANOVA.

For paired tests, each row is treated as one matched pair. Analyses omit incomplete observations where appropriate. Correlation and regression require two distinct numeric columns with at least three complete observations and non-zero variance.

The built-in sample is synthetic demonstration data, not a collection of measured archaeological finds.

## Deploy to shinyapps.io

`rsconnect` is included in the project environment. First create a shinyapps.io account and copy its account name, token, and secret from the account dashboard. Configure the account locally in the R console; **do not commit or share the token or secret**:

```r
rsconnect::setAccountInfo(
  name = "YOUR-ACCOUNT",
  token = "YOUR-TOKEN",
  secret = "YOUR-SECRET"
)
```

The Shiny app uses source files in the project-level `R/` directory, so include those files in the deployment bundle:

```r
rsconnect::deployApp(
  appDir = ".",
  appFiles = c(
    "app/app.R",
    file.path("R", c("descriptive.R", "probability.R", "inference.R", "regression.R"))
  ),
  appPrimaryDoc = "app/app.R",
  appName = "archaeology-explorer"
)
```

This deploys the app to the shinyapps.io account configured on your machine. Check the app's access settings in shinyapps.io after deployment; public deployment makes the app available to anyone with its URL.

## Project layout

| Path | Purpose |
| --- | --- |
| `app/app.R` | Shiny interface, visual styling, and reactive app logic |
| `R/descriptive.R` | Descriptive statistics and summaries |
| `R/probability.R` | Probability distributions, ECDF, and CLT simulation |
| `R/inference.R` | Distribution functions, confidence intervals, t-tests, chi-square test, and ANOVA |
| `R/regression.R` | Covariance, correlations, linear regression, predictions, and diagnostics |
| `tests/validate.R` | Numerical validation tests for statistical functions |
| `report/report.qmd` | Reproducible statistical methods report |
| `renv.lock` | Locked R package versions for the project |

## Validate the statistical functions

With the project's R environment activated, run:

```r
testthat::test_file("tests/validate.R")
```

The validation suite compares the custom implementations against base R or reference packages. One strict numerical round-trip check, `pt(qt(...))`, currently exceeds its configured tolerance; this does not prevent the Shiny app from running.
