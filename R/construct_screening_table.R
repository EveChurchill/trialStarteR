#' Creates the screening characteristic table for reporting
#'
#' @description
#'   `construct_screening_table` returns a data frame which summarises the
#'   screening characteristics specified within the
#'   AnalysisVariablesDetails.xlsx. With mean, median, sd, IQR, min and max for
#'   numerical variables, as well as counts of the number of each non-empty item
#'   in both numeric and categorical variables including each group within the
#'   categorical variables.
#'
#' @param trial.data (data frame) the data frame containing all the trial data -
#'   ideally created by the data_construction function in this package.
#'
#' @param var.spec (data frame) the data frame within the inputs folder which
#'   specifies which variables to be seen in the screening table - typically
#'   called variable.details.df
#'
#' @param id_cols (vector) the names of the id columns as a character vector.
#'   Default is `c("screening", "event_name", "event_id")`.
#'
#' @param population.list.obj (list) a list object of the same length as the
#'   number of arms. Each list element is a character vector containing the
#'   screening numbers of participants in an arm in the relevant analysis
#'   population.
#'
#' @returns (data frame) returns a data frame which summarises the screening
#'   characteristics
#'
#' @export
construct_screening_table<-function(trial.data,
                                    var.spec=variable.details.df,
                                    id_cols=c("screening", "event_name", "event_id"),
                                    population.list.obj=itt) {

  all.variables.ordered<-var.spec$VariableProspectName[var.spec$screening_yn=='y']

  all.variables.ordered<-suffix_replacement_refInput(all.variables.ordered, variable.details.df)

    if ('all' %in% all.variables.ordered) {
    stop('All variables in a dataframe are included in the baseline table. Please specify individually and rerun trial framework')
  }


  duplicated.vars<-all.variables.ordered[duplicated(all.variables.ordered)]
  for (v in duplicated.vars) {
    all.variables.ordered[all.variables.ordered==v]<-paste(v,
                                                           var.spec$DfProspectName[
                                                             var.spec$VariableProspectName==v],
                                                           sep='_'
                                                           )
  }

  #Get continous and categorical variables ready for summaries and check and modify duplicate names
  continuous<-var.spec$VariableProspectName[var.spec$DataType=='continuous']
  continuous<-continuous[continuous %in% all.variables.ordered]
  continuous<-ifelse(duplicated(continuous),
                     paste(continuous, var.spec$DfProspectName[var.spec$VariableProspectName==continuous], sep="."),
                     continuous)

  categorical<-var.spec$VariableProspectName[var.spec$DataType=='categorical']
  categorical<-categorical[categorical %in% all.variables.ordered]
  categorical<-ifelse(duplicated(categorical),
                     paste(categorical, var.spec$DfProspectName[var.spec$VariableProspectName==categorical], sep="."),
                     categorical)

  #Get baseline screening seperate for ease
  characteristic_data<-trial.data[trial.data[ ,c(id_cols[2])]=='Screening', c(id_cols[1], categorical, continuous)]

  # Dataframe for results ---------------------------------------------------

  columnnames.summaries<-c(
    'Test/Form',
    'Summary',
    'N'
  )

  screening.characteristics.table<-as.data.frame(matrix(
    nrow=3*length(continuous) + 1*length(categorical),
    ncol=length(columnnames.summaries)))


  colnames(screening.characteristics.table)<-columnnames.summaries
  #Get summaries between arms and overall

  # Continuous Summaries ----------------------------------------------------
  #Add
  row.n=1
  for (variable in all.variables.ordered) {

    #For continuous variables
    if (variable %in% continuous) {
      variable.data<-as.numeric(unlist(characteristic_data[ , c(variable)]))

      data_to_log<-numerical_summary_rounded(variable.data)

      item.name<-var.spec$VariableTextName[var.spec$VariableProspectName==variable]

      #Append all to screening.characteristics.table
      screening.characteristics.table[row.n , ]<-c(item.name, 'n', N_nonempty(variable.data))

      screening.characteristics.table[row.n+1 , ]<-c('', "  Mean (SD)", paste(data_to_log[4], ' (', sd_rounding_rm_na(variable.data), ')', sep=""))
      screening.characteristics.table[row.n+2 , ]<-c('', "  Median (IQR)", paste(data_to_log[3], ' (',data_to_log[2], ', ',data_to_log[5], ')', sep=""))
      screening.characteristics.table[row.n+3, ]<-c('', "  Min, Max", paste(data_to_log[1], ', ',data_to_log[6], sep=""))

      row.n=row.n+4


      #For categorical variables
    } else if (variable %in% categorical){
    # Categorical Summaries ----------------------------------------------------
      variable.data<-as.factor(unlist(characteristic_data[ , c(variable)]))

      #total data available
      n.o<-N_nonempty(variable.data)

      #factor percentages in each arm
      summ_o<-categorical_summary_rm_na(variable.data)

      #Append all to screening.characteristics.table
      item.name<-var.spec$VariableTextName[var.spec$VariableProspectName==variable]

      screening.characteristics.table[row.n , ]<-c(item.name, 'n', n.o)

      for (level in names(summ_o)) {
        row.n=row.n+1
        screening.characteristics.table[row.n , ]<-c('', paste('  ',level, ' - n (%)',sep=""),
                                        paste(as.numeric(summ_o[level]),
                                          ' (',
                                         rd_percent(summ_o[level], n.o),
                                          '%)', sep=""))
      }
      row.n=row.n+1
    } else {
      next
    }
  }
  return(screening.characteristics.table)
}
