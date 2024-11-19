#' Finding unique non-NA elements
#'
#' @description
#' `uniq_nonNA` returns a list of unique items within a list. These unique items
#' will not include NAs. This function is primarly used for identifying unique IDs within
#' the filtered segments of the master df created by the construct_master_dataframe
#'
#' @param item.list (vector) vector of items which can include NAs
#'
#'
#' @returns (vector) a vector of unique objects within the inputed 'item.list'
#'
#'  @examples master.df<-construct_analyses_dataframe(variable.details.df, blinded='y', "identification_log")
#' @export

uniq_nonNA<-function(item.list){
  return(unique(item.list[!is.na(item.list)]))
}
