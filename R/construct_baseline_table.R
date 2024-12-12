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
#' @param var.spec (dataframe) the dataframe within the inputs folder which specifies which variables to be seen in the baseline table - typically called variable.details.df
#'
#' @returns (dataframe) returns a dataframe which summarises the baseline characteristics
#'
#' @export

construct_baseline_table<-function(trial.data, var.spec=variable.details.df, population.list.obj=itt, fill_baselineNAs_screening_data='n'){
  if (fill_baselineNAs_screening_data=='n'){
    characteristic_data<-trial.data[trial.data$event_name=='Baseline', ]

    all.variables.ordered<-var.spec$VariableProspectName[
      var.spec$baseline_yn=='y'
    ]
  } else {
    characteristic_data<-trial.data[trial.data$event_name=='Baseline', ]

    all.variables.ordered<-var.spec$VariableProspectName[
      var.spec$baseline_yn=='y' | var.spec$baseline_yn=='y']

    #replace any missing data with screening data where possible

    for (id in characteristic_data$screening){
      #check for all baseline related fields
      if(any(characteristic_data$event_name=='Baseline participants' & characteristic_data$screening==id)) {

        missing_data.indiv<-which(
          is.na(characteristic_data[characteristic_data$screening==id, ])
        )

        characteristic_data[characteristic_data$screening==id, missing_data.indiv]<-
          trial.data[trial.data$event_name=='Baseline participants' & trial.data$screening==id, missing_data.indiv]
      }

      #check for all screening fields
      if(any(trial.data$event_name=='Screening' & trial.data$screening==id)) {

        missing_data.indiv<-which(
          is.na(characteristic_data[characteristic_data$screening==id, ])
        )

        characteristic_data[characteristic_data$screening==id, missing_data.indiv]<-
          trial.data[trial.data$event_name=='Screening' & trial.data$screening==id, missing_data.indiv]
      }

    }
  }

  all.variables.ordered<-suffix_replacement_refInput(all.variables.ordered)
  
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
  categorical<-ifelse(duplicated(categorical), paste(categorical, var.spec$DfProspectName[var.spec$VariableProspectName==categorical], sep=""), categorical)

  #Get baseline screening separate for ease
  characteristic_data<-characteristic_data[, c('screening', categorical, continuous)]

  # Dataframe for results ---------------------------------------------------

  columnnames.summaries<-c(
    'Category',
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
      arm.data[[i]]<-var.data[dataframe.name$screening %in% population.list.obj[[i]] ]
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

  cat.summ_to_string<-function(categorical.data.name, dataframe.name, item.text.name, cat.table.name){
    var.data<-as.factor(unlist(dataframe.name[ , c(categorical.data.name)]))

    arm.data<-list()
    for (i in 1:N.Arms){
      arm.data[[i]]<-var.data[dataframe.name$screening %in% population.list.obj[[i]] ]
    }
    arm.data[[i+1]]<-var.data

    arm.data.lengths<-unlist(lapply(arm.data, length))
    arm.data.lengths.cs<-c(0, cumsum(arm.data.lengths))[1:(N.Arms+1)]

    n.obsv<-lapply(arm.data, N_nonempty)

    summs<-lapply(arm.data, categorical_summary_rm_na)
    summs.lengths.cs<-c(0, cumsum(unlist(lapply(summs, length))))[1:(N.Arms+1)]
    row.text.vector<-list();label=1

    row.text.vector[[label]]<-c(
      cat.table.name,
      item.text.name, 'N (%)',
      paste(
        unlist(n.obsv),
        ' (',
        percentage_summaries_perArmOverall(as.numeric(unlist(n.obsv)), population.list.obj),
        '%)', sep=""))
    label=label+1
    for (level in names(summs[[N.Arms+1]])) {
      row.text.vector[[label]]<-c(cat.table.name, level,
                                           'N (%)',
                                           paste(
                                             unlist(summs)[which(names(unlist(summs))==level)],
                                             ' (',
                                             percentage_summaries_perArmOverall(
                                               unlist(summs)[which(names(unlist(summs))==level)], id.list.object),
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

      item.name<-var.spec$VariableTextName[var.spec$VariableProspectName==variable]
      var.category.name<-ifelse(is.na(var.spec$Category[var.spec$VariableProspectName==variable]),
                                '',
                                var.spec$Category[var.spec$VariableProspectName==variable])
      #Append all to summary_table.presented
      summary_table.presented[row.n , ]<-c(var.category.name, item.name, 'N (%)',
                                           paste(data_to_log[[1]], ' (', percentage_summaries_perArmOverall(unlist(data_to_log[[1]])) , '%)', sep=""))

      summary_table.presented[row.n+1 , ]<-c(var.category.name,'', "  Mean (SD)", paste(data_to_log[[2]], ' (', data_to_log[[4]], ')', sep=""))
      summary_table.presented[row.n+2 , ]<-c(var.category.name,'', "  Median (IQR)", paste(data_to_log[[3]], data_to_log[[5]], sep=" "))
      summary_table.presented[row.n+3, ]<-c(var.category.name, '',"  Min, Max", paste(data_to_log[[6]], ', ',data_to_log[[7]], sep=""))

      row.n=row.n+4

      #For categorical variables
    } else if (variable %in% categorical){
      # Categorical Summaries ----------------------------------------------------

      item.name<-var.spec$VariableTextName[var.spec$VariableProspectName==variable]
      var.category.name<-ifelse(is.na(var.spec$Category[var.spec$VariableProspectName==variable]),
                          '',
                          var.spec$Category[var.spec$VariableProspectName==variable])
      row.text<-cat.summ_to_string(variable, characteristic_data, item.name, var.category.name)

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

