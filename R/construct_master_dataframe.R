#' Master dataframe construction
#'
#' @description
#' `construct_master_dataframe` returns a master dataframe which can be used
#' for primary analyses, or as a base dataset for sharing .
#'
#' @details
#' From the dataframes read in by Read-data.R, this function will produce a
#' dataframe of all participants (from screening onwards) with the corresponding
#' variables as specified in Inputs/AnalysisVariablesDetails.xlsx.
#'
#' Any constant characteristics for each participant will be displayed in its own
#' column, for example: sex, ethnicity
#'
#' Any date variables will need to be manipualted if another format other than the PROSPECT format is needed.
#'
#' Adverse events will also be included to this dataset (if specified within
#' the input xlsx)
#'
#' If blinded='y', a dummy randomisation will be implemented. If blinded='n',
#' the randomisation allocation will be taken from PROSPECT.
#'
#'
#'
#'
#' @param variable.details.df (dataframe) should be the dataframe in the Inputs folder.
#' An example can be seen in the template folder
#'
#' @param blinded (character) This should be y or n
#'
#' @param name.of.screening.df (string) This is the name of the PROSPECT csv file which
#' contains the details of all participants screened. In some trials, this is the
#' "identification_log" or simply "screening_log".
#'
#' @returns (dataframe) master dataframe to be used for analyses or as a basis
#'  for sharing
#'
#'  @examples master.df<-construct_master_dataframe(variable.details.df, blinded='y', "identification_log", number.arms=N.Arms)
#' @export


construct_master_dataframe<-function(variable.details.df, blinded='y', name.of.screening.df, number.arms=N.Arms) {

  single_occ.var<-c(); single_occ.var.df<-c()

  #Get the dataframe names in prospect
  req.dataframes<-unique(variable.details.df$DfProspectName)

  #These columns will be used to map the dataframes together when merging
  id_cols<-c("screening", "event_id","event_name")

  #Visit completion is always available,
  #so this is the first dataframe to be included
  main.df<-merge(visit_completion[ , c(id_cols, 'visit_dt')],
                 get(name.of.screening.df)[, id_cols], by=id_cols, all=TRUE)

  #Loop through each dataframe and add the specified columns to the main df
  for (df.text.name in req.dataframes) {

    #If there isn't any data in the df, then skip
    if (any(dim(get(df.text.name))==0)){
      next
    } else { #If there is data, then merge to main

      #Find corresponding variables
      df_spec_cols<-variable.details.df$VariableProspectName[variable.details.df$DfProspectName==df.text.name]

      #If all
      if (any(df_spec_cols=='all')) {
        result<-merge_or_insert(main.df,df.text.name, colnames(get(df.text.name)), single_occ.var, single_occ.var.df)
        main.df<-as.data.frame(result[[1]]); single_occ.var<-result[[2]]; single_occ.var.df<-result[[3]]
      #If there is a variable name including suffix, all variables containing the text after suffix are needed
      } else if (any(grepl('suffix', df_spec_cols))) {
        df_spec_cols<-suffix_replacement_refInput(df_spec_cols)

        result<-merge_or_insert(main.df,df.text.name, c(id_cols,df_spec_cols), single_occ.var, single_occ.var.df)
        main.df<-as.data.frame(result[[1]]); single_occ.var<-result[[2]]; single_occ.var.df<-result[[3]]

      } else {
        if (!('event_name' %in% colnames(get(df.text.name)))){
          id_cols=c("screening", "event_id")
        }
        result<-merge_or_insert(main.df,df.text.name, c(id_cols,df_spec_cols), single_occ.var, single_occ.var.df)
        main.df<-as.data.frame(result[[1]]); single_occ.var<-result[[2]]; single_occ.var.df<-result[[3]]
      }
    }
  }

  #Add in adverse events





  #Add in randomisation allocation to main df - rand_arm is dummy, otherwise randomisation$rand_arm
  if (blinded=='y'){
    # Dummy Randomisation (if blinded)-----------------------------------------------------
    set.seed(2602)
    rand_arm<-sample(1:number.arms, length(unique(main.df$screening)), replace = TRUE, prob=rep(1/number.arms, number.arms))
    main.df$rand_arm<-insert_vectors_with_single_screeningID(main.df, rand_arm, unique(main.df$screening))

  } else {
    main.df$rand_arm<-insert_vectors_with_single_screeningID(main.df, randomisation$rand_arm, randomisation$screening)
    #Add in randomisation date to main df
    main.df$rand_dt<-insert_vectors_with_single_screeningID(main.df, randomisation$rand_dt, randomisation$screening)
  }
  for (i in 1:length(single_occ.var)){
    var<-single_occ.var[i]
    df.text.name<-single_occ.var.df[i]
    main.df<-cbind(main.df, insert_vectors_with_single_screeningID(
      main.df,
      get(df.text.name)[!duplicated(get(df.text.name)$screening) ,var],
      get(df.text.name)[!duplicated(get(df.text.name)$screening), c('screening')])
    )
    colnames(main.df)<-ifelse(colnames(main.df)!=tail(colnames(main.df), 1),
                              colnames(main.df),
                              var)
  }


  #Add site in

  main.df$site<-insert_vectors_with_single_screeningID(main.df,
                                                          visit_completion$site[!duplicated(visit_completion$screening)],
                                                          visit_completion$screening[!duplicated(visit_completion$screening)])
  return(main.df)
}


