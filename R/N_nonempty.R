#' Returns the number of non-empty items in a vector'
#'
#' @description
#' `rd_percent` returns the number of non-empty items in a vector
#'
#'
#' @param data.vector (vector) a vector either numeric or other
#'
#' @returns (numeric) returns the number of non-empty items
#'
#' @export

N_nonempty<-function(data.vector){
  return(sum(!is.na(data.vector)))
}
