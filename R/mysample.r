#' My Sample
#'
#' @param n An integer
#' @param iter An integer
#' @param time An integer
#' @importFrom grDevices rainbow
#'
#' @returns A boxplot for each iteration
#' @export
#'
#' @examples
#' mysample(10000)
mysample = function(n, iter = 10, time = 0.5){
  for(i in 1:iter){ # assign each loop of i to be 1 through iter
    #make a sample
    s = sample(1:10, n, replace = TRUE) # sample of options 1 through 10, choosen n times with replacement
    # turn the sample into a factor
    sf = factor(s, levels = 1:10) # used for tabulation later (assigns "names" to each value to later see how many of the same value exist)
    #make a barplot
    barplot(table(sf)/n, beside = TRUE, col = rainbow(10), # tabulate the sample factor and divide by n to get the relative frequency
            main = paste("Example sample()", " iteration ", i, " n= ", n,sep=""),
            ylim = c(0,0.2)
    )

    #release the table
    Sys.sleep(time) # delay for time seconds to prevent lag and whatnot
  }
}
