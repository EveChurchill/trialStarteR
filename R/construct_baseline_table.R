#' Creates the baseline characteristic table for reporting
#'
#' @description
#' `construct_baseline_table` returns a dataframe which summarises the baseline characteristics per arm and overall
#' specified within the AnalysisVariablesDetails.xlsx. With mean, median, sd, IQR, min and max for numerical variables,
#' as well as counts of the number of each non-empty item in both numeric and categorical variables including each group within
#' the categorical variables.
#'
#' @param trial.data (dataframe) the dataframe containing all the trial data - ideally created by the data_construction function in this package.
#'
#' @param variable.details.df (dataframe) the dataframe within the inputs folder which specifies which variables to be seen in the baseline table - typically called variable.details.df
#'
#' @returns (dataframe) returns a dataframe which summarises the baseline characteristics
#'
#' @export

construct_baseline_table<-function(trial.data, variable.details.df=variable.details.df){

  characteristic_data<-trial.data[trial.data$event_name=='Baseline', ]

  all.variables.ordered<-variable.details.df$VariableProspectName[
    variable.details.df$baseline_yn=='y'
  ]

  all.variables.ordered<-suffix.replacement_ref.masterDF(all.variables.ordered)

  #Get continous and categorical variables ready for summaries and check and modify duplicate names
  continuous<-variable.details.df$VariableProspectName[variable.details.df$DataType=='continuous']
  continuous<-continuous[continuous %in% all.variables.ordered]
  continuous<-ifelse(duplicated(continuous), paste(continuous, continuous.dfs, sep=""), continuous)

  categorical<-variable.details.df$VariableProspectName[variable.details.df$DataType=='categorical']
  categorical<-categorical[categorical %in% all.variables.ordered]
  categorical<-ifelse(duplicated(categorical), paste(categorical, categorical.dfs, sep=""), categorical)

  #Get baseline screening separate for ease
  characteristic_data<-characteristic_data[, c('screening', categorical, continuous)]

  # Dataframe for results ---------------------------------------------------

  columnnames.summaries<-c(
    'Test/Form',
    'Summary Measure',
    arm.names,
    "Overall"
  )

  summary_table.presented<-as.data.frame(matrix(
    nrow=4*length(continuous) + 1*length(categorical),
    ncol=length(columnnames.summaries)))


  colnames(summary_table.presented)<-columnnames.summaries
  #Get summaries between arms and overall

  # Continuous Summaries ----------------------------------------------------
  cont.summ_to_string<-function(continuous.data.name, dataframe.name){
    var.data<-as.numeric(dataframe.name[ ,continuous.data.name])

    arm.data<-list()
    for (i in 1:N.Arms){
      arm.data[[i]]<-var.data[dataframe.name$screening %in% itt[[i]] ]
    }
    arm.data[[i+1]]<-var.data

    arm.data.lengths<-unlist(lapply(arm.data, length))
    arm.data.lengths.cs<-c(0, cumsum(arm.data.lengths))[1:(N.Arms+1)]
    n.obsv<-lapply(arm.data, N_nonempty)

    sds<-lapply(arm.data, sd_rounding_rm_na)

    summs<-lapply(arm.data, numerical_summary_rounded)
    summs.lengths.cs<-c(0, cumsum(unlist(lapply(summs, length))))[1:(N.Arms+1)]

    mns<-unlist(summs)[4+summs.lengths.cs]
    mdns<-unlist(summs)[3+summs.lengths.cs]
    iqrs<-paste(
      paste(
        "(",
        unlist(summs)[2+summs.lengths.cs],
        "-",
        unlist(summs)[5+summs.lengths.cs],
        ")",
        sep=""), sep="")
    min.v<-unlist(summs)[1+summs.lengths.cs]
    max.v<-unlist(summs)[6+summs.lengths.cs]
    return(list(n.obsv, mns, mdns, sds, iqrs, min.v, max.v))
  }

  cat.summ_to_string<-function(categorical.data.name, dataframe.name, item.text.name){
    var.data<-as.factor(unlist(dataframe.name[ , c(categorical.data.name)]))

    arm.data<-list()
    for (i in 1:N.Arms){
      arm.data[[i]]<-var.data[dataframe.name$screening %in% itt[[i]] ]
    }
    arm.data[[i+1]]<-var.data

    arm.data.lengths<-unlist(lapply(arm.data, length))
    arm.data.lengths.cs<-c(0, cumsum(arm.data.lengths))[1:(N.Arms+1)]

    n.obsv<-lapply(arm.data, N_nonempty)

    summs<-lapply(arm.data, categorical_summary_rm_na)
    summs.lengths.cs<-c(0, cumsum(unlist(lapply(summs, length))))[1:(N.Arms+1)]
    row.text.vector<-list();label=1

    row.text.vector[[label]]<-c(
      item.text.name, 'N (%)',
      paste(
        unlist(n.obsv),
        ' (',
        percentage_summaries_perArmOverall(as.numeric(unlist(n.obsv)), itt),
        '%)', sep=""))
    label=label+1
    for (level in names(summs[[N.Arms+1]])) {
      row.text.vector[[label]]<-c(level,
                                           'N (%)',
                                           paste(
                                             unlist(summs)[which(names(unlist(summs))==level)],
                                             ' (',
                                             percentageN.summaries(
                                               unlist(summs)[which(names(unlist(summs))==level)], itt),
                                             '%)', sep=""))
      label=label+1
    }

    return(row.text.vector)
  }

  #Add
  row.n=1
  for (variable in all.variables.ordered) {

    #For continuous variables
    if (variable %in% continuous) {
      data_to_log<-cont.summ_to_string(variable, characteristic_data)

      item.name<-variable.details.df$VariableTextName[variable.details.df$VariableProspectName==variable]

      #Append all to summary_table.presented
      summary_table.presented[row.n , ]<-c(item.name, 'N (%)',
                                           paste(data_to_log[[1]], ' (', percentage_summaries_perArmOverall(unlist(data_to_log[[1]])) , '%)', sep=""))

      summary_table.presented[row.n+1 , ]<-c('', "  Mean (SD)", paste(data_to_log[[2]], ' (', data_to_log[[4]], ')', sep=""))
      summary_table.presented[row.n+2 , ]<-c('', "  Median (IQR)", paste(data_to_log[[3]], data_to_log[[5]], sep=" "))
      summary_table.presented[row.n+3, ]<-c('', "  Minimum, Maximum", paste(data_to_log[[6]], ', ',data_to_log[[7]], sep=""))

      row.n=row.n+4

      #For categorical variables
    } else if (variable %in% categorical){
      # Categorical Summaries ----------------------------------------------------

      item.name<-variable.details.df$VariableTextName[variable.details.df$VariableProspectName==variable]
      row.text<-cat.summ_to_string(variable, characteristic_data, item.name)

      for (item in 1:length(row.text)) {
        summary_table.presented[row.n , ]<-row.text[[item]]
        row.n=row.n+1
      }

    } else {
      next
    }
  }


  return(summary_table.presented)
}

