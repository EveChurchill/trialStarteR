#' Returns summary statistics to specified decimal places '
#'
#' @description
#' `numerical_summary_rounded`  Returns summary statistics of numerical data,
#'  if the number of decimal places is not specified, the standard is one d.p.
#'
#'
#' @param data (vector) Vector of continuous data (can include NAs).
#'
#' @param decimal_places (numeric) the number of decimal places
#'
#' @returns (list) returns the summary object
#'
#'  @examples numerical_summary_rounded(c(0, 1, 2, 3, 4, 5, NA))
#' @export


numerical_summary_rounded<-function(data, decimal_places=1){
  return(round(as.numeric(summary(data)), decimal_places))
}
