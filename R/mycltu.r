#' mycltu
#'
#' @param n An integer; sample size
#' @param iter An integer; number of samples/iterations
#' @param a An integer; binomial distribution parameter a
#' @param b An integer; binomial distribution parameter b
#' @importFrom graphics lines
#' @importFrom stats density dunif runif
#'
#' @returns A histogram of sample means
#' @export
#'
#' @examples
#' mycltu(20, 100000, a = 5, b = 15)
mycltu <- function(n, iter, a = 0, b = 10){
  x = NULL # prevents build error
  y = runif(n * iter, a, b) # iter samples of size n with params a and b
  data = matrix(y, nrow = n, ncol = iter, byrow = TRUE) # matrix of the data. columns are each sample
  w = apply(data, 2, mean) # finds the mean of each sample (uses mean() on the columns (2) of data)
  param = hist(w, plot = FALSE) # values from the histogram of w
  ymax = max(param$density) # returns the highest density
  ymax = 1.1 * ymax # a little more than the max for visibility
  hist(w, freq = FALSE, ylim = c(0, ymax), main = paste("Histogram of sample mean", "\n", "sample size= ", n, sep = ""), xlab = "Sample mean") # histogram of w
  lines(density(w), col = "Blue", lwd = 3) # density of w as a line
  curve(dnorm(x, mean = (a + b)/2, sd = (b - a)/(sqrt(12 * n))), add = TRUE, col = "Red", lty = 2, lwd = 3) # theoretical curve. sd is the squareroot of the variance
  curve(dunif(x,a,b), add = TRUE, lwd = 4) # uniform density plot
}
