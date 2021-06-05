
#' Mode of of integers vector
#'
#' @param v  vector of integers
#'
#' @return  integer with the mode value of the given vector
#' @export
#'
#' @examples modeR(rpois(n = 50, lambda = 10))
modeR <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}



