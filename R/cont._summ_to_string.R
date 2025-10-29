#' Create summaries from continuous data'
#'
#' @description
#' `cont.summ_to_string` create descriptive summaries for continuous data.
#' This function is used in the summary table functions. It out puts the continuous
#' summaries as a list of lines you would see in a baseline summary table e.g.
#' mean (sd) or min, max etc
#'
#' @param dataframe_object (dataframe) A dataframe you wish to summarise
#'
#' @param continuous.data.name (string) Continuous variable which is a column name
#'
#' @param population.list.obj (list) List object of the participant IDs for people
#' in each arm.
#'
#' @param include_mad (boolean) Default FALSE. Whether to include the median
#' absolute deviation alongside the median and IQR
#'
#' @param by_arm (boolean) Default TRUE. Whether to provide these summaries by
#' arm and overall
#'
#' @returns (list)  Returns the descriptive summaries in the form of a list
#' which can be inputted straight into a summary table
#'
#'  @examples age_summary<-cont.summ_to_string(core.dataframe,
#'                                             continuous.data.name = 'age_demographics')
#' @export
cont.summ_to_string<-function(dataframe_object = .,
                              continuous.data.name,
                              population.list.obj = itt,
                              id_cols = c('screening', 'event_name'),
                              include_mad = F,
                              dec_places = 1,
                              by_arm = T){

  var.data<-as.numeric(dataframe_object[ ,continuous.data.name])

  if (by_arm == T) {
    arm.data<-list()
    for (i in 1:N.Arms){
      arm.data[[i]]<-var.data[dataframe_object[, c(id_cols[1])] %in% population.list.obj[[i]] ]
    }
    arm.data[[i+1]]<-var.data[dataframe_object[, c(id_cols[1])] %in% unlist(population.list.obj)]

    arm.data.lengths<-unlist(lapply(arm.data, length))
    n.obsv<-lapply(arm.data, N_nonempty)

    sds<-lapply(arm.data, sd_rounding_rm_na)
    mads<-paste0('[',lapply(lapply(arm.data, mad, na.rm=TRUE), round, dec_places), ']')

    summs<-lapply(arm.data, numerical_summary_rounded)
    summs.lengths.cs<-c(0, cumsum(unlist(lapply(summs, length))))[1:(N.Arms+1)]
  } else {

    n.obsv<-N_nonempty(var.data)

    sds<-sd_rounding_rm_na(var.data)

    mads<-paste0('[', round(mad(var.data,na.rm=TRUE), dec_places), ']')

    summs<-numerical_summary_rounded(var.data)

    summs.lengths.cs<-0
  }

  mns<-unlist(summs)[4+summs.lengths.cs]
  mdns<-unlist(summs)[3+summs.lengths.cs]
  iqrs<-paste0(
    " (",
    unlist(summs)[2+summs.lengths.cs],
    "-",
    unlist(summs)[5+summs.lengths.cs],
    ")")
  min.v<-unlist(summs)[1+summs.lengths.cs]
  max.v<-unlist(summs)[6+summs.lengths.cs]

  mns.sds<-paste0(mns, ' (', sds, ')')
  if (include_mad==T) {
    med.iqr<-paste0(mdns, iqrs, ' ', mads)
  } else {
    med.iqr<-paste0(mdns, iqrs)
  }
  min.max<-paste0(min.v, ', ', max.v)

  return(list(n.obsv, mns.sds, med.iqr, min.max))
}
