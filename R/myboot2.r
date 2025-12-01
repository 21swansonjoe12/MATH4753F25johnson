#' My Bootstrap
#'
#' @param iter An integer; number of iterations
#' @param x A vector; sample data to bootstrap
#' @param fun A string; function for desired estimated statistic
#' @param alpha An integer; confidence value
#' @param cx An integer; modifies the 'cex' param from graphical function 'par'
#' @param ... Additional arguments to be used for the returned histogram
#' @importFrom graphics segments
#' @importFrom stats quantile
#'
#' @returns A histogram and a list of the confidence interval, function, data, and x statistic
#' @export
#'
#' @examples
#' myboot2(iter = 100000, x = rnorm(20, 0, 1), fun = "var", alpha = 0.01, cx = 1.2, xlab = "Variance")
myboot2 <- function(iter = 10000, x, fun = "mean", alpha = 0.05, cx = 1.5, ...){
  n = length(x)   #sample size
  y = sample(x, n * iter, replace = TRUE) # A. Takes the sample x and makes iter samples of size n by using replacement (bootstrapping)
  rs.mat = matrix(y, nrow = n, ncol = iter, byrow = TRUE)
  xstat = apply(rs.mat, 2, fun) # xstat is a vector and will have iter values in it
  ci = quantile(xstat, c(alpha/2, 1 - alpha/2)) # B. Finds the corresponding quantiles for the probabilities alpha/2 and 1-alpha/2, which in this case are the outer limits of the confidence interval
  # A histogram follows
  # The object para will contain the parameters used to make the histogram
  para = hist(xstat, freq = FALSE, las = 1,
              main = paste("Histogram of Bootstrap sample statistics", "\n", "alpha=", alpha, " iter=", iter, sep = ""),
              ...)

  #mat will be a matrix that contains the data, this is done so that I can use apply()
  mat = matrix(x, nrow = length(x), ncol = 1, byrow = TRUE)

  #pte is the point estimate
  #This uses whatever fun is
  pte = apply(mat, 2, fun)
  abline(v = pte, lwd = 3, col = "Black")# Vertical line
  segments(ci[1], 0, ci[2], 0, lwd = 4)      #Make the segment for the ci
  text(ci[1], 0, paste("(", round(ci[1], 2), sep = ""), col = "Red", cex = cx)
  text(ci[2], 0, paste(round(ci[2], 2), ")", sep = ""), col = "Red", cex = cx)

  # plot the point estimate 1/2 way up the density
  text(pte, max(para$density)/2, round(pte, 2), cex = cx)

  invisible(list(ci = ci, fun = fun, x = x, xstat = xstat))# Some output to use if necessary
}
