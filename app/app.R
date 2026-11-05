app_root <- if (file.exists(file.path("R", "descriptive.R"))) {
  "."
} else {
  ".."
}

for (file in c("descriptive.R", "probability.R", "inference.R", "regression.R")) {
  source(file.path(app_root, "R", file), local = TRUE)
}

set.seed(42)
n_demo <- 180
demo_length <- rnorm(n_demo, mean = 52, sd = 12)
demo_data <- data.frame(
  Site = factor(sample(c("North Ridge", "River Bend", "Cedar Flats"), n_demo, TRUE)),
  Artifact = factor(sample(c("Point", "Scraper", "Flake"), n_demo, TRUE)),
  Period = factor(sample(c("Early", "Middle", "Late"), n_demo, TRUE)),
  Length = demo_length,
  Width = 4 + 0.38 * demo_length + rnorm(n_demo, sd = 4),
  Depth = rnorm(n_demo, mean = 18, sd = 5)
)

numeric_columns <- function(data) {
  names(data)[vapply(data, is.numeric, logical(1))]
}

categorical_columns <- function(data) {
  names(data)[vapply(data, function(x) is.factor(x) || is.character(x) || is.logical(x),
                     logical(1))]
}

theme <- bslib::bs_theme(
  version = 5,
  bootswatch = "minty",
  primary = "#355d50",
  secondary = "#b66b4b",
  success = "#527a62",
  bg = "#f5f3ed",
  fg = "#26352f"
)

ui <- shiny::fluidPage(
  theme = theme,
  shiny::tags$head(
    shiny::tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    shiny::tags$style(shiny::HTML("
      :root {
        --ink: #26352f;
        --muted: #748078;
        --forest: #355d50;
        --clay: #b66b4b;
        --paper: #f5f3ed;
        --line: #e7e4dc;
        --card: #fffefa;
      }
      body { background: var(--paper); color: var(--ink); font-size: 15px; }
      .container-fluid { max-width: 1500px; padding: 28px clamp(16px, 4vw, 58px) 56px; }
      .hero {
        position: relative; overflow: hidden; padding: clamp(26px, 5vw, 54px);
        margin-bottom: 26px; border-radius: 22px; color: #f8f6ef;
        background: linear-gradient(118deg, #27453c 0%, #355d50 60%, #58745d 100%);
        box-shadow: 0 16px 38px rgba(38, 53, 47, .12);
      }
      .hero:after {
        content: ''; position: absolute; width: 310px; height: 310px; right: -80px; top: -150px;
        border: 1px solid rgba(255,255,255,.16); border-radius: 50%;
        box-shadow: 0 0 0 28px rgba(255,255,255,.035), 0 0 0 58px rgba(255,255,255,.025);
      }
      .hero-kicker, .section-kicker {
        text-transform: uppercase; letter-spacing: .16em; font-size: 11px; font-weight: 700;
      }
      .hero-kicker { color: #d5c6a9; margin-bottom: 12px; }
      .hero h1 {
        margin: 0 0 12px; font-family: Georgia, 'Times New Roman', serif;
        color: #faf8f1; font-size: clamp(32px, 5vw, 54px);
        font-weight: 500; letter-spacing: -.035em;
      }
      .hero-copy { max-width: 650px; color: rgba(248,246,239,.8); font-size: 16px; line-height: 1.7; margin: 0; }
      .hero-tag {
        display: inline-block; margin-top: 22px; padding: 7px 12px; border-radius: 30px;
        border: 1px solid rgba(255,255,255,.25); color: #f3e8d6; font-size: 12px;
      }
      .workspace { display: grid; grid-template-columns: 280px minmax(0, 1fr); gap: 24px; align-items: start; }
      .data-card, .content-card {
        background: var(--card); border: 1px solid var(--line); border-radius: 16px;
        box-shadow: 0 5px 18px rgba(38,53,47,.035);
      }
      .data-card { padding: 22px; position: sticky; top: 20px; }
      .data-card h3 { margin: 0 0 7px; font: 500 23px Georgia, 'Times New Roman', serif; }
      .data-card-copy { color: var(--muted); font-size: 13px; line-height: 1.6; margin-bottom: 18px; }
      .data-status { padding: 12px; border-radius: 10px; background: #f1f4ef; color: #456251; font-size: 12px; line-height: 1.55; }
      .data-status pre { margin: 0; color: inherit; font: inherit; white-space: pre-wrap; overflow-wrap: anywhere; }
      .control-label { color: #46564d; font-size: 12px; font-weight: 700; letter-spacing: .035em; }
      .form-control, .form-select { border-color: #dedfd7; border-radius: 9px; box-shadow: none; }
      .form-control:focus, .form-select:focus { border-color: #6d947e; box-shadow: 0 0 0 3px rgba(83,125,99,.12); }
      .btn { border-radius: 9px; font-weight: 600; }
      .analysis-area { min-width: 0; }
      .nav-tabs {
        display: flex; flex-wrap: wrap; gap: 7px; border: 0; padding: 6px;
        margin-bottom: 20px; border-radius: 13px; background: #eae9e1;
      }
      .nav-tabs .nav-link {
        border: 0 !important; border-radius: 9px !important; color: #68746d;
        padding: 11px 15px; font-size: 13px; font-weight: 650; transition: .18s ease;
      }
      .nav-tabs .nav-link:hover { color: var(--forest); background: rgba(255,255,255,.55); }
      .nav-tabs .nav-link.active { background: var(--card) !important; color: var(--forest) !important; box-shadow: 0 2px 6px rgba(38,53,47,.08); }
      .tab-content { min-width: 0; }
      .tab-intro { margin: 4px 0 20px; }
      .section-kicker { color: var(--clay); margin-bottom: 6px; }
      .tab-intro h2 { margin: 0 0 6px; font: 500 29px Georgia, 'Times New Roman', serif; letter-spacing: -.02em; }
      .tab-intro p { margin: 0; color: var(--muted); line-height: 1.6; }
      .content-card { padding: 22px; margin-bottom: 18px; min-width: 0; }
      .content-card h3 { margin: 0 0 14px; font: 500 20px Georgia, 'Times New Roman', serif; }
      .content-card h4 { margin: 18px 0 12px; font: 500 18px Georgia, 'Times New Roman', serif; }
      .content-card .shiny-plot-output, .content-card .plotly { max-width: 100%; }
      .shiny-input-container { max-width: 100%; }
      .dataTables_wrapper { overflow-x: auto; }
      table.dataTable thead th { color: #53645a; font-size: 12px; text-transform: uppercase; letter-spacing: .04em; }
      .table > :not(caption) > * > * { padding: .68rem .75rem; border-bottom-color: var(--line); }
      .result-box { padding: 16px; border-left: 3px solid var(--clay); background: #f8f4ec; border-radius: 0 10px 10px 0; }
      .app-footer { padding-top: 20px; color: #88918a; font-size: 12px; text-align: center; }
      @media (max-width: 900px) {
        .workspace { grid-template-columns: 1fr; }
        .data-card { position: static; }
        .nav-tabs { gap: 3px; }
        .nav-tabs .nav-link { padding: 9px 10px; }
      }
      @media (max-width: 540px) {
        .container-fluid { padding: 14px 12px 32px; }
        .hero { border-radius: 16px; padding: 26px 22px; margin-bottom: 16px; }
        .content-card { padding: 16px; }
        .nav-tabs { display: grid; grid-template-columns: 1fr 1fr; }
        .nav-tabs .nav-link { text-align: center; }
      }
    "))
  ),
  shiny::div(
    class = "hero",
    shiny::div(class = "hero-kicker", "Field notes · Data laboratory"),
    shiny::h1("Archaeology Statistics Explorer"),
    shiny::p(class = "hero-copy",
      "Explore patterns in archaeological data, experiment with probability, and learn what statistical tests can tell you."),
    shiny::span(class = "hero-tag", "A hands-on learning space for data and discovery")
  ),
  shiny::div(
    class = "workspace",
    shiny::tags$aside(
      class = "data-card",
      shiny::div(class = "section-kicker", "Your workspace"),
      shiny::h3("Data source"),
      shiny::p(class = "data-card-copy",
        "Begin with the sample collection, or bring in a CSV of your own."),
      shiny::fileInput("data_file", "Upload a CSV file", accept = ".csv",
                       buttonLabel = "Browse", placeholder = "No file selected"),
      shiny::div(class = "data-status", shiny::verbatimTextOutput("data_status"))
    ),
    shiny::tags$main(
      class = "analysis-area",
      shiny::tabsetPanel(
        id = "main_tabs",
        shiny::tabPanel(
          "Explore",
          shiny::div(class = "tab-intro",
            shiny::div(class = "section-kicker", "01 · Get to know your data"),
            shiny::h2("Explore the collection"),
            shiny::p("Choose a variable to see its shape, summary, and a quick preview of your records.")
          ),
          shiny::fluidRow(
            shiny::column(4, shiny::div(class = "content-card",
              shiny::h3("Select a variable"), shiny::uiOutput("explore_variable_ui"))),
            shiny::column(8, shiny::div(class = "content-card",
              shiny::h3("At a glance"), shiny::tableOutput("explore_summary")))
          ),
          shiny::div(class = "content-card",
            shiny::h3("Distribution of values"),
            plotly::plotlyOutput("explore_plot", height = "330px")),
          shiny::div(class = "content-card",
            shiny::h3("A peek at the records"),
            DT::DTOutput("data_preview"))
        ),
        shiny::tabPanel(
          "Distributions/CLT",
          shiny::div(class = "tab-intro",
            shiny::div(class = "section-kicker", "02 · Probability in action"),
            shiny::h2("Distributions & the CLT"),
            shiny::p("Shape a probability curve, then watch sample averages settle into a distribution.")
          ),
          shiny::div(class = "content-card",
            shiny::h3("Explore a probability distribution"),
            shiny::fluidRow(
              shiny::column(4,
                shiny::selectInput("distribution", "Distribution",
                  choices = c("Normal", "Binomial", "Poisson", "Exponential")),
                shiny::uiOutput("distribution_parameters")
              ),
              shiny::column(8,
                plotly::plotlyOutput("distribution_plot", height = "290px"),
                plotly::plotlyOutput("distribution_cdf_plot", height = "230px")
              )
            )
          ),
          shiny::div(class = "content-card",
            shiny::h3("A sampling experiment"),
            shiny::p(class = "data-card-copy",
              "Repeatedly sample from a population and compare the resulting sample means."),
            shiny::fluidRow(
              shiny::column(4,
                shiny::selectInput("clt_distribution", "Population distribution",
                  choices = c("Normal", "Binomial", "Poisson", "Exponential")),
                shiny::uiOutput("clt_parameters"),
                shiny::numericInput("clt_n", "Observations per sample", 30, min = 2, max = 500),
                shiny::numericInput("clt_reps", "Number of samples", 1000, min = 100, max = 5000),
                shiny::numericInput("clt_seed", "Random seed", 1, min = 0, max = 999999)
              ),
              shiny::column(8,
                shiny::plotOutput("clt_plot", height = "320px"),
                shiny::tableOutput("clt_summary")
              )
            )
          )
        ),
        shiny::tabPanel(
          "Correlation and Regression",
          shiny::div(class = "tab-intro",
            shiny::div(class = "section-kicker", "03 · Relationships"),
            shiny::h2("Correlation & regression"),
            shiny::p("Compare two measurements and fit a straight line to summarize their relationship.")
          ),
          shiny::fluidRow(
            shiny::column(4, shiny::div(class = "content-card",
              shiny::h3("Choose measurements"),
              shiny::uiOutput("cor_x_ui"), shiny::uiOutput("cor_y_ui"))),
            shiny::column(8, shiny::div(class = "content-card",
              shiny::h3("Relationship between variables"),
              plotly::plotlyOutput("cor_plot", height = "340px")))
          ),
          shiny::div(class = "content-card",
            shiny::h3("Correlation & model fit"),
            shiny::tableOutput("cor_summary"),
            shiny::h4("Regression coefficients"),
            DT::DTOutput("regression_table"))
        ),
        shiny::tabPanel(
          "Hypothesis Tests",
          shiny::div(class = "tab-intro",
            shiny::div(class = "section-kicker", "04 · Evidence & uncertainty"),
            shiny::h2("Hypothesis tests"),
            shiny::p("Select a test, set its inputs, and review the estimate, test statistic, and p-value.")
          ),
          shiny::div(class = "content-card",
            shiny::h3("Set up your analysis"),
            shiny::selectInput("test_type", "Test",
              choices = c("One-sample t-test", "Two-sample t-test", "Paired t-test",
                          "Chi-square test of independence", "One-way ANOVA")),
            shiny::uiOutput("test_controls")),
          shiny::div(class = "content-card",
            shiny::h3("Your results"),
            shiny::div(class = "result-box",
              shiny::tableOutput("test_result"),
              shiny::verbatimTextOutput("test_details")))
        )
      )
    )
  ),
  shiny::div(class = "app-footer", "Built for curious minds · Archaeology Statistics Explorer")
)

server <- function(input, output, session) {
  data <- shiny::reactive({
    if (is.null(input$data_file)) {
      demo_data
    } else {
      utils::read.csv(input$data_file$datapath, stringsAsFactors = FALSE,
                      check.names = FALSE)
    }
  })

  output$data_status <- shiny::renderText({
    if (is.null(input$data_file)) {
      sprintf("Using built-in sample data (%d rows, %d columns).",
              nrow(demo_data), ncol(demo_data))
    } else {
      sprintf("Using %s (%d rows, %d columns).",
              input$data_file$name, nrow(data()), ncol(data()))
    }
  })

  output$explore_variable_ui <- shiny::renderUI({
    columns <- names(data())
    shiny::validate(shiny::need(length(columns) > 0, "The data has no columns to explore."))
    shiny::selectInput("explore_variable", "Variable", choices = columns)
  })

  output$explore_summary <- shiny::renderTable({
    selected <- input$explore_variable
    shiny::req(selected, selected %in% names(data()))
    x <- data()[[selected]]
    if (is.numeric(x)) {
      shiny::validate(shiny::need(any(!is.na(x)), "This variable has no non-missing values."))
      describe(x)
    } else {
      counts <- sort(table(x, useNA = "ifany"), decreasing = TRUE)
      data.frame(value = names(counts), count = as.integer(counts), row.names = NULL)
    }
  }, rownames = FALSE)

  output$explore_plot <- plotly::renderPlotly({
    selected <- input$explore_variable
    shiny::req(selected, selected %in% names(data()))
    x <- data()[[selected]]
    if (is.numeric(x)) {
      shiny::validate(shiny::need(any(!is.na(x)), "This variable has no non-missing values."))
      plotly::plot_ly(x = x, type = "histogram", nbinsx = 30,
                      marker = list(color = "#2c7fb8")) |>
        plotly::layout(xaxis = list(title = selected), yaxis = list(title = "Count"))
    } else {
      counts <- as.data.frame(table(x, useNA = "ifany"))
      names(counts) <- c("value", "count")
      plotly::plot_ly(counts, x = ~value, y = ~count, type = "bar",
                      marker = list(color = "#2c7fb8")) |>
        plotly::layout(xaxis = list(title = selected), yaxis = list(title = "Count"))
    }
  })

  output$data_preview <- DT::renderDT({
    DT::datatable(utils::head(data(), 10), options = list(scrollX = TRUE, pageLength = 10))
  })

  output$distribution_parameters <- shiny::renderUI({
    switch(input$distribution,
      "Normal" = shiny::tagList(
        shiny::numericInput("dist_mean", "Mean", 0),
        shiny::numericInput("dist_sd", "Standard deviation", 1, min = 0.001)
      ),
      "Binomial" = shiny::tagList(
        shiny::numericInput("dist_size", "Number of trials", 20, min = 1, max = 500, step = 1),
        shiny::numericInput("dist_prob", "Success probability", 0.5, min = 0, max = 1)
      ),
      "Poisson" = shiny::numericInput("dist_lambda", "Rate (lambda)", 4, min = 0.001),
      "Exponential" = shiny::numericInput("dist_rate", "Rate", 1, min = 0.001)
    )
  })

  output$clt_parameters <- shiny::renderUI({
    switch(input$clt_distribution,
      "Normal" = shiny::tagList(
        shiny::numericInput("clt_mean", "Population mean", 0),
        shiny::numericInput("clt_sd", "Population standard deviation", 1, min = 0.001)
      ),
      "Binomial" = shiny::tagList(
        shiny::numericInput("clt_size", "Population trials", 20, min = 1, max = 500, step = 1),
        shiny::numericInput("clt_prob", "Population success probability", 0.5, min = 0, max = 1)
      ),
      "Poisson" = shiny::numericInput("clt_lambda", "Population rate (lambda)", 4, min = 0.001),
      "Exponential" = shiny::numericInput("clt_rate", "Population rate", 1, min = 0.001)
    )
  })

  distribution_values <- shiny::reactive({
    switch(input$distribution,
      "Normal" = {
        shiny::req(input$dist_mean, input$dist_sd)
        shiny::validate(shiny::need(input$dist_sd > 0, "Standard deviation must be positive."))
        x <- seq(input$dist_mean - 4 * input$dist_sd,
                 input$dist_mean + 4 * input$dist_sd, length.out = 400)
        list(x = x, density = my_dnorm(x, input$dist_mean, input$dist_sd),
             cdf = my_pnorm(x, input$dist_mean, input$dist_sd), discrete = FALSE)
      },
      "Binomial" = {
        shiny::req(input$dist_size, input$dist_prob)
        size <- as.integer(input$dist_size)
        x <- 0:size
        list(x = x, density = my_dbinom(x, size, input$dist_prob),
             cdf = my_pbinom(x, size, input$dist_prob), discrete = TRUE)
      },
      "Poisson" = {
        shiny::req(input$dist_lambda)
        shiny::validate(shiny::need(input$dist_lambda > 0, "Lambda must be positive."))
        x <- 0:ceiling(input$dist_lambda + 5 * sqrt(input$dist_lambda) + 10)
        list(x = x, density = my_dpois(x, input$dist_lambda),
             cdf = my_ppois(x, input$dist_lambda), discrete = TRUE)
      },
      "Exponential" = {
        shiny::req(input$dist_rate)
        shiny::validate(shiny::need(input$dist_rate > 0, "Rate must be positive."))
        x <- seq(0, 8 / input$dist_rate, length.out = 400)
        list(x = x, density = my_dexp(x, input$dist_rate),
             cdf = my_pexp(x, input$dist_rate), discrete = FALSE)
      }
    )
  })

  output$distribution_plot <- plotly::renderPlotly({
    values <- distribution_values()
    if (values$discrete) {
      plotly::plot_ly(x = values$x, y = values$density, type = "bar",
                      marker = list(color = "#2c7fb8"))
    } else {
      plotly::plot_ly(x = values$x, y = values$density, type = "scatter", mode = "lines",
                      line = list(color = "#2c7fb8"))
    } |>
      plotly::layout(title = "Probability density / mass",
                     xaxis = list(title = "Value"), yaxis = list(title = "Density / mass"))
  })

  output$distribution_cdf_plot <- plotly::renderPlotly({
    values <- distribution_values()
    plotly::plot_ly(x = values$x, y = values$cdf, type = "scatter", mode = "lines",
                    line = list(color = "#d95f0e")) |>
      plotly::layout(title = "Cumulative distribution",
                     xaxis = list(title = "Value"), yaxis = list(title = "Cumulative probability"))
  })

  clt_means_result <- shiny::reactive({
    shiny::req(input$clt_n, input$clt_reps, input$clt_seed)
    n <- as.integer(input$clt_n)
    reps <- as.integer(input$clt_reps)
    shiny::validate(
      shiny::need(n >= 2, "Sample size must be at least 2."),
      shiny::need(reps >= 100, "Use at least 100 simulated samples.")
    )
    generator <- switch(input$clt_distribution,
      "Normal" = {
        shiny::req(input$clt_mean, input$clt_sd)
        shiny::validate(shiny::need(input$clt_sd > 0, "Population SD must be positive."))
        function(n) stats::rnorm(n, input$clt_mean, input$clt_sd)
      },
      "Binomial" = {
        shiny::req(input$clt_size, input$clt_prob)
        function(n) stats::rbinom(n, as.integer(input$clt_size), input$clt_prob)
      },
      "Poisson" = {
        shiny::req(input$clt_lambda)
        shiny::validate(shiny::need(input$clt_lambda > 0, "Population lambda must be positive."))
        function(n) stats::rpois(n, input$clt_lambda)
      },
      "Exponential" = {
        shiny::req(input$clt_rate)
        shiny::validate(shiny::need(input$clt_rate > 0, "Population rate must be positive."))
        function(n) stats::rexp(n, input$clt_rate)
      }
    )
    values <- clt_means(generator, n, reps, as.integer(input$clt_seed))
    list(values = values, mean = my_mean(values), sd = my_sd(values), n = n, reps = reps)
  })

  output$clt_plot <- shiny::renderPlot({
    result <- clt_means_result()
    graphics::hist(result$values, probability = TRUE, breaks = 30,
                   col = "#9ecae1", border = "white",
                   main = "Distribution of sample means", xlab = "Sample mean")
    if (is.finite(result$sd) && result$sd > 0) {
      curve(my_dnorm(x, result$mean, result$sd), add = TRUE,
            col = "#d95f0e", lwd = 2)
    }
  })

  output$clt_summary <- shiny::renderTable({
    result <- clt_means_result()
    data.frame(
      `Observations per sample` = result$n,
      `Simulated samples` = result$reps,
      `Mean of sample means` = result$mean,
      `SD of sample means` = result$sd,
      check.names = FALSE
    )
  }, digits = 4, rownames = FALSE)

  output$cor_x_ui <- shiny::renderUI({
    columns <- numeric_columns(data())
    shiny::validate(shiny::need(length(columns) >= 2, "Upload at least two numeric columns."))
    shiny::selectInput("cor_x", "X variable", choices = columns)
  })

  output$cor_y_ui <- shiny::renderUI({
    columns <- numeric_columns(data())
    shiny::validate(shiny::need(length(columns) >= 2, "Upload at least two numeric columns."))
    shiny::selectInput("cor_y", "Y variable", choices = columns,
                       selected = columns[min(2, length(columns))])
  })

  correlation_data <- shiny::reactive({
    shiny::req(input$cor_x, input$cor_y)
    shiny::validate(shiny::need(input$cor_x != input$cor_y,
                                "Choose different variables for X and Y."))
    frame <- data.frame(x = data()[[input$cor_x]], y = data()[[input$cor_y]])
    frame <- stats::na.omit(frame)
    shiny::validate(
      shiny::need(nrow(frame) > 2, "At least three complete observations are required."),
      shiny::need(stats::var(frame$x) > 0 && stats::var(frame$y) > 0,
                  "Both variables must have non-zero variance.")
    )
    frame
  })

  regression_fit <- shiny::reactive({
    frame <- correlation_data()
    my_lm(y ~ x, data = frame)
  })

  output$cor_plot <- plotly::renderPlotly({
    frame <- correlation_data()
    fit <- regression_fit()
    line_data <- data.frame(x = range(frame$x))
    line_data$y <- predict_my_lm(fit, line_data)
    plotly::plot_ly(frame, x = ~x, y = ~y, type = "scatter", mode = "markers",
                    name = "Observations", marker = list(color = "#2c7fb8")) |>
      plotly::add_trace(data = line_data, x = ~x, y = ~y, type = "scatter",
                        mode = "lines", inherit = FALSE, name = "my_lm fit",
                        line = list(color = "#d95f0e")) |>
      plotly::layout(xaxis = list(title = input$cor_x), yaxis = list(title = input$cor_y))
  })

  output$cor_summary <- shiny::renderTable({
    frame <- correlation_data()
    fit <- regression_fit()
    data.frame(
      `Pearson correlation (my_cor)` = my_cor(frame$x, frame$y),
      `Spearman correlation (my_spearman)` = my_spearman(frame$x, frame$y),
      `R-squared (my_lm)` = fit$r_squared,
      `Adjusted R-squared` = fit$adj_r_squared,
      check.names = FALSE
    )
  }, digits = 4, rownames = FALSE)

  output$regression_table <- DT::renderDT({
    DT::datatable(regression_fit()$coefficients, rownames = TRUE,
                  options = list(dom = "t"))
  })

  output$test_controls <- shiny::renderUI({
    dat <- data()
    nums <- numeric_columns(dat)
    cats <- categorical_columns(dat)
    test_type <- input$test_type
    if (test_type %in% c("One-sample t-test", "Two-sample t-test", "Paired t-test")) {
      shiny::validate(shiny::need(length(nums) > 0, "Upload at least one numeric column."))
      controls <- switch(test_type,
        "One-sample t-test" = shiny::selectInput("test_x", "Numeric variable", choices = nums),
        "Two-sample t-test" = shiny::tagList(
          shiny::selectInput("test_x", "First numeric variable", choices = nums),
          shiny::selectInput("test_y", "Second numeric variable", choices = nums,
                             selected = nums[min(2, length(nums))]),
          shiny::checkboxInput("var_equal", "Assume equal variances", FALSE)
        ),
        "Paired t-test" = shiny::tagList(
          shiny::selectInput("test_x", "First paired variable", choices = nums),
          shiny::selectInput("test_y", "Second paired variable", choices = nums,
                             selected = nums[min(2, length(nums))])
        )
      )
      shiny::tagList(
        controls,
        if (test_type == "One-sample t-test")
          shiny::numericInput("test_mu", "Null mean", 0),
        shiny::selectInput("test_alternative", "Alternative hypothesis",
          choices = c("two.sided", "less", "greater")),
        shiny::numericInput("test_confidence", "Confidence level", 0.95,
                            min = 0.5, max = 0.999, step = 0.01)
      )
    } else if (test_type == "Chi-square test of independence") {
      shiny::validate(shiny::need(length(cats) >= 2, "Upload at least two categorical columns."))
      shiny::tagList(
        shiny::selectInput("test_x", "First categorical variable", choices = cats),
        shiny::selectInput("test_y", "Second categorical variable", choices = cats,
                           selected = cats[min(2, length(cats))])
      )
    } else {
      shiny::validate(
        shiny::need(length(nums) > 0, "Upload a numeric column for the outcome."),
        shiny::need(length(cats) > 0, "Upload a categorical column for the groups.")
      )
      shiny::tagList(
        shiny::selectInput("test_x", "Numeric outcome", choices = nums),
        shiny::selectInput("test_group", "Group variable", choices = cats)
      )
    }
  })

  test_result <- shiny::reactive({
    dat <- data()
    type <- input$test_type
    if (type %in% c("One-sample t-test", "Two-sample t-test", "Paired t-test")) {
      shiny::req(input$test_x, input$test_alternative, input$test_confidence)
      level <- input$test_confidence
      shiny::validate(shiny::need(level > 0 && level < 1,
                                  "Confidence level must be between 0 and 1."))
      x <- dat[[input$test_x]]
      if (type == "One-sample t-test") {
        shiny::req(input$test_mu)
        x <- x[!is.na(x)]
        shiny::validate(shiny::need(length(x) >= 2, "At least two values are required."))
        result <- my_t_test(x, mu = input$test_mu, alternative = input$test_alternative,
                            conf_level = level)
      } else {
        shiny::req(input$test_y)
        shiny::validate(shiny::need(input$test_x != input$test_y,
                                    "Choose different variables for the two samples."))
        y <- dat[[input$test_y]]
        if (type == "Paired t-test") {
          ok <- !is.na(x) & !is.na(y)
          x <- x[ok]
          y <- y[ok]
          shiny::validate(shiny::need(length(x) >= 2, "At least two complete pairs are required."))
          result <- my_t_test(x, y, paired = TRUE, alternative = input$test_alternative,
                              conf_level = level)
        } else {
          x <- x[!is.na(x)]
          y <- y[!is.na(y)]
          shiny::validate(shiny::need(length(x) >= 2 && length(y) >= 2,
                                      "Each sample must have at least two values."))
          result <- my_t_test(x, y, var_equal = isTRUE(input$var_equal),
                              alternative = input$test_alternative, conf_level = level)
        }
      }
      list(type = type, result = result)
    } else if (type == "Chi-square test of independence") {
      shiny::req(input$test_x, input$test_y)
      shiny::validate(shiny::need(input$test_x != input$test_y,
                                  "Choose different categorical variables."))
      frame <- stats::na.omit(dat[c(input$test_x, input$test_y)])
      shiny::validate(shiny::need(nrow(frame) > 0, "No complete observations are available."))
      tab <- table(frame[[1]], frame[[2]])
      shiny::validate(shiny::need(all(dim(tab) >= 2), "Each variable needs at least two categories."))
      list(type = type, result = my_chisq_test(tab))
    } else {
      shiny::req(input$test_x, input$test_group)
      frame <- dat[c(input$test_x, input$test_group)]
      frame <- frame[stats::complete.cases(frame), , drop = FALSE]
      shiny::validate(
        shiny::need(nrow(frame) > 2, "At least three complete observations are required."),
        shiny::need(length(unique(frame[[2]])) >= 2, "At least two groups are required.")
      )
      list(type = type, result = my_anova(frame[[1]], frame[[2]]))
    }
  })

  output$test_result <- shiny::renderTable({
    item <- test_result()
    result <- item$result
    if (inherits(result, "my_htest")) {
      data.frame(
        Test = result$method,
        Estimate = result$estimate,
        Statistic = result$statistic,
        `Degrees of freedom` = result$df,
        `p-value` = result$p_value,
        `Confidence interval lower` = result$conf_int[1],
        `Confidence interval upper` = result$conf_int[2],
        check.names = FALSE
      )
    } else if (item$type == "Chi-square test of independence") {
      data.frame(Test = item$type, Statistic = result$statistic,
                 `Degrees of freedom` = result$df, `p-value` = result$p_value,
                 check.names = FALSE)
    } else {
      data.frame(Test = item$type, Statistic = result$F,
                 `Numerator df` = result$df[1], `Denominator df` = result$df[2],
                 `p-value` = result$p_value, check.names = FALSE)
    }
  }, digits = 5, rownames = FALSE)

  output$test_details <- shiny::renderPrint({
    print(test_result()$result)
  })
}

shiny::shinyApp(ui, server)