#' My N Curve
#'
#' @param mu An integer, mean
#' @param sigma An integer, sd
#' @param a An integer, lower tail
#' @importFrom graphics curve polygon text
#' @importFrom stats dnorm pnorm
#'
#' @returns A normal curve plot with the area of the lower tail overlayed, and the average and standard deviation as a list
#' @export
#'
#' @examples
#' myncurve(0, 1, 1)
myncurve = function(mu, sigma, a){
  low <- mu - 3 * sigma
  x <- NULL # Does nothing but tell devcheck that this is ok
  curve(dnorm(x,
              mean = mu,
              sd = sigma),
        xlim = c(low, mu + 3 * sigma))
  prob <- round(pnorm(a, mean = mu, sd = sigma), 4)
  xcurve <- seq(low, a, length = 1000)
  ycurve <- dnorm(xcurve, mu, sigma)
  polygon(x = c(low, xcurve, a), y = c(0, ycurve, 0), col = "pink")
  text(mean(c(low,a)), 0.5*dnorm(mean(c(low,a)), mu, sigma), paste0("Area=", prob))
  list(mu = mu, sigma = sigma, a = a)
}
