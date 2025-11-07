#' ntickets
#'
#' @param N An integer; number of available seats
#' @param gamma An integer; "pain" factor; probability of overbooking
#' @param p An integer; probability a passenger will show who bought a ticket
#' @importFrom graphics abline
#' @importFrom stats optimize pbinom
#'
#' @returns 2 plots and a list of values, including n for both the discrete and continuous cases, and the inputs of the function
#' @export
#'
#' @examples
#' ntickets(400, 0.02, 0.95)
ntickets <- function(N, gamma, p){
  trials <- as.integer(N / 10 + 10 * exp(-5 * (p - 1))) # Found by inspection
  startN <- as.integer(N + 10 * (1 / p^2 - 1)) # Found by inspection
  endN <- trials + startN
  # Discrete
  xx <- startN:(startN + trials)
  values1 <- (abs(pbinom(N, xx, p) - 1 + gamma))
  nd <- xx[which.min(values1)]
  height1 <- min(values1)

  # Continous
  values2 <- function(n) abs(pnorm(N + 0.5, mean = n * p, sd = sqrt(n * p * (1 - p))) - 1 + gamma)
  opt <- optimize(values2, c(startN, endN))
  nc <- round(opt$minimum, 8)
  height2 <- opt$objective

  # Plot
  layout(matrix(1:2, nrow = 2, ncol = 1))
  # Discrete Plot
  title1 <- paste0("Objective vs. n to find optimal tickets sold\n(", nd, ") gamma = ", gamma, " N = ", N, " discrete")
  plot(xx, values1,
       main = title1,
       xlab = "n",
       ylab = "Objective",
       col = "blue",
       pch = 16,
       cex = 0.8)
  abline(v = nd, col = "red")
  abline(h = height1, col = "red")
  # Continuous Plot
  title2 <- paste0("Objective vs. n to find optimal tickets sold\n(", nc, ") gamma = ", gamma, " N = ", N, " continuous")
  x <- NULL # prevents build note; not assigning a value to x is intentional, but R doesn't know this
  curve(values2(x), from = startN + 0.5, to = endN + 0.5,
        main = title2,
        xlab = "n",
        ylab = "Objective",
        col = "blue")
  abline(v = nc, col = "red")
  abline(h = height2, col = "red")

  # Returning Values
  outputs <- list(nd = nd, nc = nc, gamma = gamma, N = N, p = p)
  invisible(outputs) # in case being used in RMD, have it print values but allow them to be accessible, using invisible() to prevent doubling
  print(outputs)
}
