#' Returns standard deviation (removes NAs) to one decimal point'
#'
#' @description
#' `sd_rounding_rm_na`  Returns standard deviation to one decimal point and ignores NAs within the data
#'
#'
#' @param data (vector) Vector of continuous data (can include NAs).
#'
#' @returns (numeric) returns the standard deviation to one decimal place
#'
#'  @examples sd_rounding_rm_na(c(0, 1, 2, 3, 4, 5, NA))
#' @export

sd_rounding_rm_na<-function(data){
  return(round(sd(data, na.rm = TRUE), 1))
}
