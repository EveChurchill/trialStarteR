#' Create summaries from categorical data'
#'
#' @description
#' `cat.summ_to_string` create descriptive summaries for categorical data.
#' This function is used in the summary table functions. It out puts the categorical
#' summaries as a list of lines you would see in a baseline summary table e.g.
#' Male, 54 (96%) etc
#'
#' @param dataframe_object (dataframe) A dataframe you wish to summarise
#'
#' @param categorical.data.name (string) Categorical variable which is a column name
#'
#' @param population.list.obj (list) List object of the participant IDs for people
#' in each arm.
#'
#' @param by_arm (boolean) Default TRUE. Whether to provide these summaries by
#' arm and overall
#'
#' @returns (list)  Returns the descriptive summaries in the form of a list
#' which can be inputted straight into a summary table
#'
#'  @examples sex_summary<-cont.summ_to_string(core.dataframe,
#'                                             continuous.data.name = 'sex_demographics')
#' @export
cat.summ_to_string<-function(categorical.data.name,
                             dataframe_object,
                             population.list.obj = itt,
                             by_arm = T){

  var.data<-as.factor(unlist(dataframe_object[dataframe_object[, c(id_cols[1])] %in% unlist(population.list.obj) , c(categorical.data.name)]))

  if (by_arm == T) {

    func.n.arms<-N.Arms
    arm.data<-list()
    for (i in 1:func.n.arms){
      arm.data[[i]]<-var.data[dataframe_object[, c(id_cols[1])] %in% population.list.obj[[i]] ]
    }
    arm.data[[i+1]]<-var.data[dataframe_object[, c(id_cols[1])] %in% unlist(population.list.obj)]

    n.obsv<-unlist(lapply(arm.data, N_nonempty))

    summs<-lapply(arm.data, categorical_summary_rm_na)

    row.text.vector<-list();label.n=1

    row.text.vector[[label.n]]<-c(
      paste0(
        attr(core[[categorical.data.name]], 'label'),
        ': N(%)'),
      paste(
        unlist(n.obsv),
        ' (',
        percentage_summaries_perArmOverall(
          n.obsv,
          arm.n = func.n.arms,
          population.list.obj = population.list.obj),
        '%)', sep=""))

    label.n=label.n+1
    for (level in names(summs[[max(length(summs))]])) {
      if (unlist(summs)[which(names(unlist(summs))==level)][max(length(summs))]!=0) {
        row.text.vector[[label.n]]<-c(paste0('\\hspace{0.25cm} ', level),
                                      paste(
                                        unlist(summs)[which(names(unlist(summs))==level)],
                                        ' (',

                                        percentage_summaries_perArmOverall(
                                          unlist(summs)[which(names(unlist(summs))==level)],
                                          arm.n = func.n.arms,
                                          population.list.obj = population.list.obj)[
                                            !is.na(percentage_summaries_perArmOverall(
                                              unlist(summs)[which(names(unlist(summs))==level)],
                                              arm.n = func.n.arms,
                                              population.list.obj = population.list.obj))
                                          ],
                                        '%)', sep=""))
        label.n=label.n+1
      }
    }

  } else {

    func.n.arms = 1
    n.obsv<-N_nonempty(var.data)
    summs<-list()
    summs[[func.n.arms]]<-categorical_summary_rm_na(var.data)
    row.text.vector<-list();label.n=1

    row.text.vector[[label.n]]<-c(
      paste0(
        attr(core[[categorical.data.name]], 'label'),
        ': N'),
      unlist(n.obsv)
    )

    label.n=label.n+1
    for (level in names(summs[[max(length(summs))]])) {
      if (unlist(summs)[which(names(unlist(summs))==level)][max(length(summs))]!=0) {
        row.text.vector[[label.n]]<-c(paste0('\\hspace{0.25cm} ', level),
                                      unlist(summs)[which(names(unlist(summs))==level)]
        )
        label.n=label.n+1
      }

    }
  }



  return(row.text.vector)
}
