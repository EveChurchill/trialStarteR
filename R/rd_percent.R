#' Returns the percentage of n in N to the nearest integer'
#'
#' @description
#' `rd_percent` returns the percentage of n in N to the nearest integer
#'
#'
#' @param n (numeric) the number of items with a certain characteristic from the group
#'
#' @param N (numeric) the number of items in the group
#'
#' @returns (numeric) returns a percentage of n in N to the nearest integer
#'
#'  @examples rd_percent(3, 10)
#' @export

rd_percent<-function(numerator.n, denominator.N){
  return(round(100*numerator.n/denominator.N, 0))
}
