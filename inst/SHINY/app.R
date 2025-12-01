# Shiny MLE Explorer
# Single-file Shiny app (app.R)
# Demonstrates maximum likelihood estimation for multiple univariate distributions
# Distributions included: Normal, Exponential, Gamma, Poisson, Binomial, Beta (optional)

library(shiny)
library(ggplot2)
library(stats)

# --- helper functions: log-likelihoods and mle wrappers ---

nll_normal <- function(params, x){
  mu <- params[1]
  sigma <- params[2]
  if(sigma <= 0) return(1e12)
  -sum(dnorm(x, mean = mu, sd = sigma, log = TRUE))
}

mle_normal <- function(x){
  mu_hat <- mean(x)
  sigma_hat <- sqrt(sum((x - mu_hat)^2)/length(x)) # MLE uses 1/n
  list(par = c(mu = mu_hat, sigma = sigma_hat), nll = nll_normal(c(mu_hat, sigma_hat), x))
}

nll_exp <- function(params, x){
  lambda <- params[1]
  if(lambda <= 0) return(1e12)
  -sum(dexp(x, rate = lambda, log = TRUE))
}

mle_exp <- function(x){
  lambda_hat <- 1/mean(x)
  list(par = c(lambda = lambda_hat), nll = nll_exp(lambda_hat, x))
}

nll_gamma <- function(params, x){
  shape <- params[1]; rate <- params[2]
  if(shape <= 0 || rate <= 0) return(1e12)
  -sum(dgamma(x, shape = shape, rate = rate, log = TRUE))
}

mle_gamma <- function(x){
  # use method of moments as start
  m <- mean(x); v <- var(x)
  shape0 <- m^2 / v
  rate0 <- m / v
  opt <- optim(par = c(shape0, rate0), fn = nll_gamma, x = x, method = "L-BFGS-B", lower = c(1e-6,1e-6))
  list(par = c(shape = opt$par[1], rate = opt$par[2]), nll = opt$value, conv = opt$convergence)
}

nll_poisson <- function(params, x){
  lambda <- params[1]
  if(lambda <= 0) return(1e12)
  -sum(dpois(x, lambda = lambda, log = TRUE))
}

mle_poisson <- function(x){
  lambda_hat <- mean(x)
  list(par = c(lambda = lambda_hat), nll = nll_poisson(lambda_hat, x))
}

nll_binom <- function(params, x, size){
  p <- params[1]
  if(p <= 0 || p >= 1) return(1e12)
  -sum(dbinom(x, size = size, prob = p, log = TRUE))
}

mle_binom <- function(x, size){
  p_hat <- sum(x) / (length(x) * size)
  list(par = c(p = p_hat), nll = nll_binom(p_hat, x, size))
}

nll_beta <- function(params, x){
  a <- params[1]; b <- params[2]
  if(a <= 0 || b <= 0) return(1e12)
  -sum(dbeta(x, shape1 = a, shape2 = b, log = TRUE))
}

mle_beta <- function(x){
  # method of moments start
  m <- mean(x); v <- var(x)
  common <- m * (1 - m) / v - 1
  a0 <- max(m * common, 1e-3)
  b0 <- max((1 - m) * common, 1e-3)
  opt <- optim(par = c(a0, b0), fn = nll_beta, x = x, method = "L-BFGS-B", lower = c(1e-6,1e-6))
  list(par = c(alpha = opt$par[1], beta = opt$par[2]), nll = opt$value, conv = opt$convergence)
}

# a utility to compute AIC
compute_aic <- function(nll, k) {
  2 * (nll) + 2 * k
}

# --- UI ---
ui <- fluidPage(
  titlePanel("Shiny MLE Explorer — univariate distributions"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("mode", "Data source:", choices = c("Simulate" = "sim", "Upload CSV" = "upload"), selected = "sim"),
      conditionalPanel(
        condition = "input.mode == 'sim'",
        selectInput("dist", "Choose distribution to simulate / fit:",
                    choices = c("Normal", "Exponential", "Gamma", "Poisson", "Binomial", "Beta"), selected = "Normal"),
        numericInput("n", "Sample size (n):", value = 200, min = 1, step = 1),
        numericInput("seed", "Random seed (optional, 0 for random):", value = 123, step = 1),
        hr(),
        # params UI for simulation (changes depending on distribution)
        uiOutput("params_ui"),
        actionButton("simulate", "Simulate data")
      ),
      conditionalPanel(
        condition = "input.mode == 'upload'",
        fileInput("file1", "Upload CSV (one column of observations)", accept = c('.csv', '.txt')),
        textInput("colname", "Column name (leave blank to use first column):", value = ""),
        helpText("For Binomial, upload counts and set 'Binomial size' below.")
      ),
      hr(),
      conditionalPanel(
        condition = "input.dist == 'Binomial'",
        numericInput("binom_size", "Binomial 'size' (trials per observation):", value = 10, min = 1)
      ),
      hr(),
      checkboxInput("show_prof", "Show profile likelihood / 1D trace plot", value = TRUE),
      width = 3
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Data & Fit",
                 br(),
                 plotOutput("plot_main", height = "500px"),
                 br(),
                 tableOutput("est_table")
        ),
        tabPanel("Log-likelihood",
                 br(),
                 verbatimTextOutput("ll_text"),
                 plotOutput("prof_plot", height = "350px")
        ),
        tabPanel("Raw data",
                 br(),
                 tableOutput("raw_head")
        )
      )
    )
  )
)

# --- Server ---
server <- function(input, output, session){

  # dynamic UI for parameters
  output$params_ui <- renderUI({
    switch(input$dist,
           "Normal" = tagList(
             numericInput("mu", "True mu:", value = 0),
             numericInput("sigma", "True sigma (>0):", value = 1, min = 1e-6)
           ),
           "Exponential" = numericInput("lambda", "True rate (lambda):", value = 1, min = 1e-6),
           "Gamma" = tagList(
             numericInput("shape", "True shape:", value = 2, min = 1e-6),
             numericInput("rate", "True rate:", value = 1, min = 1e-6)
           ),
           "Poisson" = numericInput("lambda_p", "True lambda:", value = 3, min = 0),
           "Binomial" = tagList(
             numericInput("p", "True p:", value = 0.3, min = 0, max = 1),
             numericInput("size_sim", "Size per observation (for simulation):", value = 10, min = 1)
           ),
           "Beta" = tagList(
             numericInput("alpha", "True alpha:", value = 2, min = 1e-6),
             numericInput("beta", "True beta:", value = 5, min = 1e-6)
           )
    )
  })

  # reactive: dataset
  data_r <- reactiveVal(NULL)

  observeEvent(input$simulate, {
    if(input$seed != 0) set.seed(input$seed)
    n <- as.integer(input$n)
    dist <- input$dist
    x <- NULL
    if(dist == "Normal"){
      x <- rnorm(n, mean = input$mu, sd = input$sigma)
    } else if(dist == "Exponential"){
      x <- rexp(n, rate = input$lambda)
    } else if(dist == "Gamma"){
      x <- rgamma(n, shape = input$shape, rate = input$rate)
    } else if(dist == "Poisson"){
      x <- rpois(n, lambda = input$lambda_p)
    } else if(dist == "Binomial"){
      x <- rbinom(n, size = input$size_sim, prob = input$p)
    } else if(dist == "Beta"){
      x <- rbeta(n, shape1 = input$alpha, shape2 = input$beta)
    }
    data_r(x)
  })

  # when uploading
  observeEvent(input$file1, {
    req(input$file1)
    df <- read.csv(input$file1$datapath)
    if(nchar(input$colname) > 0 && input$colname %in% names(df)){
      x <- df[[input$colname]]
    } else {
      x <- df[[1]]
    }
    data_r(x)
  })

  output$raw_head <- renderTable({
    x <- data_r()
    if(is.null(x)) return(NULL)
    head(data.frame(x = x), 20)
  })

  # fit the chosen distribution
  fit_r <- reactive({
    x <- data_r()
    req(x)
    dist <- input$dist
    if(dist == "Normal") return(mle_normal(x))
    if(dist == "Exponential") return(mle_exp(x))
    if(dist == "Gamma") return(mle_gamma(x))
    if(dist == "Poisson") return(mle_poisson(x))
    if(dist == "Binomial") return(mle_binom(x, size = input$binom_size))
    if(dist == "Beta") return(mle_beta(x))
  })

  output$est_table <- renderTable({
    x <- data_r(); req(x)
    fit <- fit_r(); req(fit)
    pars <- fit$par
    nll <- fit$nll
    k <- length(pars)
    aic <- compute_aic(nll, k)
    data.frame(Parameter = names(pars), Estimate = unname(pars), row.names = NULL, check.names = FALSE) |>
      rbind(data.frame(Parameter = "NegLogLike", Estimate = nll, row.names = NULL, check.names = FALSE)) |>
      rbind(data.frame(Parameter = "AIC", Estimate = aic, row.names = NULL, check.names = FALSE))
  })

  output$plot_main <- renderPlot({
    x <- data_r(); req(x)
    fit <- fit_r(); req(fit)
    dist <- input$dist

    df <- data.frame(x = x)

    if(dist %in% c("Normal", "Exponential", "Gamma", "Beta")){
      p <- ggplot(df, aes(x = x)) + geom_histogram(aes(y = ..density..), bins = 30, color = "black", fill = "grey90") + theme_minimal()
      if(dist == "Normal"){
        mu <- fit$par['mu']; sigma <- fit$par['sigma']
        p <- p + stat_function(fun = function(xx) dnorm(xx, mean = mu, sd = sigma), size = 1.2)
      } else if(dist == "Exponential"){
        lambda <- fit$par['lambda']
        p <- p + stat_function(fun = function(xx) dexp(xx, rate = lambda), size = 1.2)
      } else if(dist == "Gamma"){
        shape <- fit$par['shape']; rate <- fit$par['rate']
        p <- p + stat_function(fun = function(xx) dgamma(xx, shape = shape, rate = rate), size = 1.2)
      } else if(dist == "Beta"){
        alpha <- fit$par['alpha']; beta <- fit$par['beta']
        p <- p + stat_function(fun = function(xx) dbeta(xx, shape1 = alpha, shape2 = beta), size = 1.2)
      }
      print(p + ggtitle(paste0(dist, " fit — histogram + fitted density")))
    } else if(dist == "Poisson"){
      # barplot of counts with fitted Poisson PMF
      tab <- as.data.frame(table(x))
      tab$x <- as.integer(as.character(tab$x))
      lambda <- fit$par['lambda']
      tab$pmf <- dpois(tab$x, lambda = lambda)
      p <- ggplot(tab, aes(x = factor(x), y = Freq)) + geom_bar(stat = 'identity', fill = 'grey90', color = 'black') + theme_minimal()
      p <- p + geom_point(aes(x = factor(x), y = pmf * length(x)), color = 'red', size = 2) +
        ggtitle(paste0('Poisson counts (bars) and fitted PMF scaled to n (red points)')) + xlab('x')
      print(p)
    } else if(dist == "Binomial"){
      tab <- as.data.frame(table(x))
      tab$x <- as.integer(as.character(tab$x))
      p_hat <- fit$par['p']; size <- input$binom_size
      tab$pmf <- dbinom(tab$x, size = size, prob = p_hat)
      p <- ggplot(tab, aes(x = factor(x), y = Freq)) + geom_bar(stat = 'identity', fill = 'grey90', color = 'black') + theme_minimal()
      p <- p + geom_point(aes(x = factor(x), y = pmf * length(x)), color = 'blue', size = 2) +
        ggtitle('Binomial counts and fitted PMF scaled to n') + xlab('x')
      print(p)
    }
  })

  output$ll_text <- renderPrint({
    fit <- fit_r(); req(fit)
    cat('Negative log-likelihood at MLE:', format(signif(fit$nll, 6)), '\n')
    cat('Parameter estimates:\n')
    print(fit$par)
    if(!is.null(fit$conv)) cat('Optimizer convergence code:', fit$conv, '\n')
  })

  output$prof_plot <- renderPlot({
    if(!isTRUE(input$show_prof)) return(NULL)
    x <- data_r(); req(x)
    fit <- fit_r(); req(fit)
    dist <- input$dist

    # For 1-parameter distributions: show negative log-likelihood over a grid
    if(dist == 'Exponential'){
      lam_grid <- seq(max(1e-6, fit$par['lambda'] * 0.2), fit$par['lambda'] * 2.5, length.out = 200)
      nlls <- sapply(lam_grid, function(l) nll_exp(l, x))
      plot(lam_grid, nlls, type = 'l', xlab = 'lambda', ylab = 'NegLogLike', main = 'Profile: Exponential')
      abline(v = fit$par['lambda'], col = 'red')
    } else if(dist == 'Normal'){
      mu_grid <- seq(mean(x) - 2*sd(x), mean(x) + 2*sd(x), length.out = 150)
      nlls <- sapply(mu_grid, function(mu) nll_normal(c(mu, fit$par['sigma']), x))
      plot(mu_grid, nlls, type = 'l', xlab = 'mu', ylab = 'NegLogLike', main = 'Profile (mu) — Normal')
      abline(v = fit$par['mu'], col = 'red')
    } else if(dist == 'Gamma'){
      shape_grid <- seq(max(1e-3, fit$par['shape']*0.2), fit$par['shape']*2.5, length.out = 150)
      nlls <- sapply(shape_grid, function(s) nll_gamma(c(s, fit$par['rate']), x))
      plot(shape_grid, nlls, type = 'l', xlab = 'shape', ylab = 'NegLogLike', main = 'Profile (shape) — Gamma')
      abline(v = fit$par['shape'], col = 'red')
    } else if(dist == 'Poisson'){
      lam_grid <- seq(max(1e-6, fit$par['lambda']*0.2), fit$par['lambda']*2.5, length.out = 200)
      nlls <- sapply(lam_grid, function(l) nll_poisson(l, x))
      plot(lam_grid, nlls, type = 'l', xlab = 'lambda', ylab = 'NegLogLike', main = 'Profile: Poisson')
      abline(v = fit$par['lambda'], col = 'red')
    } else if(dist == 'Binomial'){
      p_grid <- seq(1e-4, 1-1e-4, length.out = 200)
      nlls <- sapply(p_grid, function(p) nll_binom(p, x, size = input$binom_size))
      plot(p_grid, nlls, type = 'l', xlab = 'p', ylab = 'NegLogLike', main = 'Profile: Binomial')
      abline(v = fit$par['p'], col = 'red')
    } else if(dist == 'Beta'){
      a_grid <- seq(max(1e-3, fit$par['alpha']*0.2), fit$par['alpha']*2.5, length.out = 150)
      nlls <- sapply(a_grid, function(a) nll_beta(c(a, fit$par['beta']), x))
      plot(a_grid, nlls, type = 'l', xlab = 'alpha', ylab = 'NegLogLike', main = 'Profile (alpha) — Beta')
      abline(v = fit$par['alpha'], col = 'red')
    }
  })

}

# Run the app
shinyApp(ui, server)
