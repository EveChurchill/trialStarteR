#' Returns summary of categorical data '
#'
#' @description
#' `categorical_summary_rm_na`  Returns summary of categorical data
#'
#'
#' @param data (vector) Vector of categorical data (can include NAs- these will be ignored).
#'
#' @returns (list) returns the summary object
#'
#' @examples categorical_summary_rm_na(as.factor(c(rep('cow', 2), rep('sheep',5), rep('pig', 3), rep(NA, 4))))
#' @export

categorical_summary_rm_na<-function(data){
  data<-data[!is.na(data)]
  return(
    summary(data)[!is.na(names(summary(data))) & names(summary(data))!=""]
  )
}
