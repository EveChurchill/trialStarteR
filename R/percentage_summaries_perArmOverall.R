#' Returns percentages of items in each arm and overall'
#'
#' @description
#' `percentage_summaries_perArmOverall`  Returns the percentage of people with a certain characteristic in each arm and overall to one decimal point'
#'
#' @param n (vector) Vector of the number of people/items with a certain characteristic in each arm and overall. This should be inputted as c(control.n, intervention1.n, ..., overall.n)
#'
#' @param arm.n (numeric) The number of arms in the study
#'
#' @param population.list.obj (list) a list object of the same length as the
#'   number of arms. Each list element is a character vector containing the
#'   screening numbers of participants in an arm in the relevant analysis
#'   population.
#'
#' @returns (vector) returns the list of percentages to one decimal place in the format c(control.percentage, intervention1.percentage, ..., overall.percentage)
#'
#'  @examples percentage_summaries_perArmOverall(c(2, 5, 3, 10))
#' @export
percentage_summaries_perArmOverall<-function(n, arm.n=N.Arms, population.list.obj=itt){
  percents.arms<-c()
  for (i in 1:N.Arms){
    percents.arms<-append(percents.arms, rd_percent(n[i], length(population.list.obj[[i]])))
  }
  i=i+1
  percents.arms<-append(percents.arms, rd_percent(n[i], sum(unlist(lapply(population.list.obj, length)))))
  return(percents.arms)
}
