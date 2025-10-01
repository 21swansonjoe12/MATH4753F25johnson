#' Birthday
#'
#' @param x A numeric vector or integer
#'
#' @returns Probability that 2 or more people in the random sample of size x have a common birthday
#' @export
#'
#' @examples
#' birthday(20:24)
birthday <- function(x) {
  1 - exp(lchoose(365,x) + lfactorial(x) - x*log(365))
}
