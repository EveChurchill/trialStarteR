#' Master dataframe construction
#'
#' @description
#'   `construct_master_dataframe` returns a master data frame which can be used
#'   for primary analyses, or as a base dataset for sharing.
#'
#' @details
#'   From the dataframes read in by Read-data.R, this function will produce a
#'   dataframe of all participants (from screening onwards) with the
#'   corresponding variables as specified in
#'   Inputs/AnalysisVariablesDetails.xlsx.
#'
#'    Any constant characteristics for each participant will be displayed in its
#'    own column, for example: sex, ethnicity.
#'
#'   Any date variables will need to be manipulated if another format other than
#'   the PROSPECT format is needed.
#'
#'   Adverse events will also be included to this dataset (if specified within
#'   the input xlsx)
#'
#'   If blinded='y', a dummy randomisation will be implemented. If blinded='n',
#'   the randomisation allocation will be taken from PROSPECT.
#'
#'   If there are duplicate variable names, the df names will be appended to
#'   them e.g. variable.dfname
#'
#' @param variable.details.df (data frame) should be the data frame in the
#'   Inputs folder. An example can be seen in the template folder.
#'
#' @param blinded (character) This should be y or n
#'
#' @param id_cols (vector) the names of the id columns as a character vector.
#'   Default is `c("screening", "event_name", "event_id")`.
#'
#' @param name.of.visit.df (string) This is the name of the PROSPECT csv
#'   file which contains the details of all visits. In some
#'   trials, this is `"visit_completion"` or `"events"`. Default is
#'   `"visit_completion"`.
#'
#' @param name.of.screening.df (string) This is the name of the PROSPECT csv
#'   file which contains the details of all participants screened. In some
#'   trials, this is the "identification_log" or simply "screening_log".
#'
#' @param number.arms (integer) the number of arms in the trial.
#'
#' @param field.description.df (data frame) the data frame containing the field
#'   labels. Default is `fields`, this is often read in from Fields.csv.
#'
#' @returns (data frame) master data frame to be used for analyses or as a basis
#'  for sharing
#'
#' @examples
#' \dontrun{
#' master.df <- construct_master_dataframe(variable.details.df, blinded = 'y', "identification_log", number.arms=N.Arms)
#' }
#'
#' @export
construct_master_dataframe<-function(variable.details.df,
                                     blinded='y',
                                     id_cols=c("screening", "event_name", "event_id"),
                                     name.of.visit.df='visit_completion',
                                     name.of.screening.df,
                                     number.arms=N.Arms,
                                    field.description.df=fields,
                                    cds_labels_df=lookups) {

    #Keep only fields which are in the variable.details.df
  field.description.df$Form <- stringr::str_remove(field.description.df$Form, ".csv") %>%
    stringr::str_replace_all("( - )| ", "_") %>%
    stringr::str_remove_all("\\(|\\)|-") %>%
    stringr::str_to_lower()
  
  field.description.df$Form<-trimws(gsub("[[:punct:][:space:]]+", "_", field.description.df$Form), which = 'left', whitespace = '_')
  
  field.description.df<-field.description.df[field.description.df$Form %in% variable.details.df$DfProspectName, ]
  
  renamed<-c()
  duplicated_names<-variable.details.df$VariableProspectName[
    duplicated(variable.details.df$VariableProspectName)
  ]
  #Get the dataframe names in prospect
  req.dataframes<-unique(variable.details.df$DfProspectName)
  
  #These columns will be used to map the dataframes together when merging
  id_cols<-id_cols[(id_cols %in% colnames(get(name.of.visit.df))) & (id_cols %in% colnames(get(name.of.screening.df)))]
  
  #Check visit df and screening df for other variables to add
  visit_cols<-suffix_replacement_refInput(variable.details.df$VariableProspectName[variable.details.df$DfProspectName==name.of.visit.df], variable.details.df)
  screening_cols<-suffix_replacement_refInput(variable.details.df$VariableProspectName[variable.details.df$DfProspectName==name.of.screening.df], variable.details.df)
  
  single_occ.var<-screening_cols; single_occ.var.df<-rep(name.of.screening.df, length(screening_cols))
  #Visit completion is always available in newer trials,
  #so this is the first dataframe to be included
  main.df<-merge(get(name.of.visit.df)[ , c(id_cols, visit_cols)],
                 get(name.of.screening.df)[, c(id_cols)], by=id_cols, all=TRUE)
  
  #Loop through each dataframe and add the specified columns to the main df
  for (df.text.name in req.dataframes[!(req.dataframes %in% c(name.of.visit.df, name.of.screening.df))]) {
    
    #If there isn't any data in the df, then skip
    if (any(dim(get(df.text.name))==0)){
      next
    } else { #If there is data, then merge to main
      
      #Find corresponding variables
      df_spec_cols<-variable.details.df$VariableProspectName[variable.details.df$DfProspectName==df.text.name]
      
      #Check for duplicated variable names within specified inputs
      duplicated.column<-ifelse(all(grepl('all', df_spec_cols)),
                                
                                colnames(get(df.text.name))[
                                  length(standard.set.column)+which(
                                    colnames(get(df.text.name))[
                                      (length(standard.set.column)+1):length(colnames(get(df.text.name)))
                                    ] %in% variable.details.df$VariableProspectName)],
                                
                                df_spec_cols[which(
                                  df_spec_cols %in% variable.details.df$VariableProspectName[
                                    duplicated(variable.details.df$VariableProspectName)
                                  ])]
      )
      
      if (!purrr::is_empty(duplicated.column) & !is.na(duplicated.column)) {
        
        df<-get(df.text.name)[order(get(df.text.name)[ , c(id_cols[1])]), ]
        
        col.idx<-which(colnames(df)==duplicated.column)
        
        colnames(df)[col.idx]<-paste(duplicated.column,
                                     df.text.name,
                                     sep='_')
        
        colnames(master[[df.text.name]])[col.idx]<-paste(duplicated.column,
                                                         df.text.name,
                                                         sep='_')
        
        df_spec_cols[df_spec_cols==duplicated.column]<-paste(duplicated.column,
                                                             df.text.name,
                                                             sep='_')
        renamed<-append(renamed, paste(duplicated.column,
                                       df.text.name,
                                       sep='_'))
      } else {
        df<-get(df.text.name)[order(get(df.text.name)[ , c(id_cols[1])]), ]
      }
      
      
      #If all
      if (all(df_spec_cols=='all')) {
        result<-merge_or_insert(main.df,
                                df.text.name,
                                df,
                                id_cols,
                                colnames(df)[!colnames(df) %in% standard.set.column],
                                single_occ.var, single_occ.var.df)
        main.df<-as.data.frame(result[[1]]); single_occ.var<-result[[2]]; single_occ.var.df<-result[[3]]
        
        #If there is a variable name including suffix, all variables containing the text after suffix are needed
      } else if (any(grepl('suffix', df_spec_cols))) {
        df_spec_cols<-suffix_replacement_refInput(df_spec_cols, variable.details.df)
        
        result<-merge_or_insert(main.df, df.text.name, df, id_cols, df_spec_cols, single_occ.var, single_occ.var.df)
        main.df<-as.data.frame(result[[1]]); single_occ.var<-result[[2]]; single_occ.var.df<-result[[3]]
        
      } else {
        result<-merge_or_insert(main.df, df.text.name, df, id_cols, df_spec_cols, single_occ.var, single_occ.var.df)
        main.df<-as.data.frame(result[[1]]); single_occ.var<-result[[2]]; single_occ.var.df<-result[[3]]
      }
    }
  }
  
  #Add in adverse events
  
  
  
  
  
  #Add in randomisation allocation to main df - rand_arm is dummy, otherwise randomisation$rand_arm
  if (blinded=='y'){
    # Dummy Randomisation (if blinded)-----------------------------------------------------
    set.seed(2602)
    rand_arm<-sample(1:number.arms, length(unique(main.df[ , c(id_cols[1])])), replace = TRUE, prob=rep(1/number.arms, number.arms))
    main.df$rand_arm<-insert_vectors_with_single_screeningID(main.df, id_cols, rand_arm, unique(main.df[ , c(id_cols[1])]))
    set.seed(2602)
    main.df$rand_dt<-insert_vectors_with_single_screeningID(main.df,
                                                            id_cols,
                                                            sample(
                                                              seq(
                                                                as.Date('2024-01-01'),
                                                                as.Date('2024-12-31'),
                                                                by="day"),
                                                              length(unique(get(name.of.visit.df)[ , c(id_cols[1])]))),
                                                            unique(get(name.of.visit.df)[ , c(id_cols[1])]))
    cds = cds_labels_df$code[cds_labels_df$field=='rand_arm']
    lbls = cds_labels_df$label[cds_labels_df$field=='rand_arm']
    
    main.df$rand_arm <- as.factor(ifelse(is.na(main.df$rand_arm), NA, lbls[match(main.df$rand_arm, cds)]))
  } else {
    main.df$rand_arm<-insert_vectors_with_single_screeningID(main.df, id_cols, randomisation$rand_arm, randomisation[ , c(id_cols[1])])
    cds = cds_labels_df$code[cds_labels_df$field=='rand_arm']
    lbls = cds_labels_df$label[cds_labels_df$field=='rand_arm']
    
    main.df$rand_arm <- as.factor(ifelse(is.na(main.df$rand_arm), NA, lbls[match(main.df$rand_arm, cds)]))
    #Add in randomisation date to main df
    main.df$rand_dt<-insert_vectors_with_single_screeningID(main.df, id_cols, randomisation$rand_dt, randomisation[ , c(id_cols[1])])
  }
  
  for (i in 1:length(single_occ.var)){
    var<-single_occ.var[i]
    
    if (var %in% renamed){
      df.text.name<-single_occ.var.df[i]
      original_var<-stringr::str_remove(var, paste('_', df.text.name, sep=''))
      main.df<-cbind(main.df, insert_vectors_with_single_screeningID(
        main.df,
        id_cols,
        get(df.text.name)[!duplicated(get(df.text.name)[ , c(id_cols[1])]) ,original_var],
        get(df.text.name)[!duplicated(get(df.text.name)[ , c(id_cols[1])]), c(id_cols[1])])
      )
      colnames(main.df)<-ifelse(colnames(main.df)!=tail(colnames(main.df), 1),
                                colnames(main.df),
                                var)
      
    } else {
      df.text.name<-single_occ.var.df[i]
      main.df<-cbind(main.df, insert_vectors_with_single_screeningID(
        main.df,
        id_cols,
        get(df.text.name)[!duplicated(get(df.text.name)[ , c(id_cols[1])]) ,var],
        get(df.text.name)[!duplicated(get(df.text.name)[ , c(id_cols[1])]), c(id_cols[1])])
      )
      colnames(main.df)<-ifelse(colnames(main.df)!=tail(colnames(main.df), 1),
                                colnames(main.df),
                                var)
    }
  }
  main.df<-main.df[, !grepl('site.', colnames(main.df))]
  
  #Add site in
  main.df$site<-insert_vectors_with_single_screeningID(
    main.df, id_cols,
    get(name.of.screening.df)[!duplicated(get(name.of.screening.df)[ , c(id_cols[1])]) , c('site')],
    get(name.of.screening.df)[!duplicated(get(name.of.screening.df)[ , c(id_cols[1])]), c(id_cols[1])])
  
  main.df<-main.df[ ,c(id_cols, 'site', single_occ.var, colnames(main.df)[!colnames(main.df) %in% c(id_cols, 'site', single_occ.var)])]
  #Return updated dataframes  to global environment
  list2env(master[names(master)!='lookups'], envir = .GlobalEnv)
  
  
  #Convert lookups form name to R environment name
  cds_labels_df$form<-stringr::str_remove(cds_labels_df$form, ".csv") %>%
    stringr::str_replace_all("( - )| ", "_") %>%
    stringr::str_remove_all("\\(|\\)|-") %>%
    stringr::str_to_lower()
  
  cds_labels_df$form<-trimws(gsub("[[:punct:][:space:]]+", "_", cds_labels_df$form), which = 'left', whitespace = '_')
  cds_labels_df$form[grepl('study_completion_discontinuation', cds_labels_df$form)]<-'study_completion__discontinuation'
  #Make sure all labels are wrangles with cds_labels_df
  for (name in names(main.df)[!(names(main.df) %in% c(standard.set.column, 'rand_arm'))]) {
    
    #If the name is in cds_labels_df and the code is not all NA
    if (name %in% cds_labels_df$field & !all(is.na(cds_labels_df$code[cds_labels_df$field==name]))) {
      #If there are no stated levels in the data column, implement them from cds_labels_df
      if (is.null(levels(main.df[,c(name)]))) {
        
        if (!all(is.na(variable.details.df$DfProspectName[variable.details.df$VariableProspectName==name][1]))){
          cds = cds_labels_df$code[cds_labels_df$field==name & cds_labels_df$form==variable.details.df$DfProspectName[variable.details.df$VariableProspectName==name][1]]
          lbls = cds_labels_df$label[cds_labels_df$field==name & cds_labels_df$form==variable.details.df$DfProspectName[variable.details.df$VariableProspectName==name][1]]
        } else {
          cds = cds_labels_df$code[cds_labels_df$field==name]
          lbls =  cds_labels_df$label[cds_labels_df$field==name]
        }
        
        
        main.df[, c(name)] <- as.factor(ifelse(is.na(main.df[,c(name)]), NA, lbls[match(main.df[, c(name)], cds)]))
      }
      #whether there are levels or not, add a label to the column
      attr(main.df[, c(name)], 'label') <- field.description.df$Label[field.description.df$Identifier==name][1]
      
      #If not in lookups but in field.description.df, label with that description
    } else if (name %in% field.description.df$Identifier) {
      attr(main.df[, c(name)], 'label') <- field.description.df$Label[field.description.df$Identifier==name][1]
      
      #If not in fields or lookups, then may have been entered as a suffix which has been flagged
    } else if (name %in% suffix_replacement_refInput(variable.details.df$VariableProspectName[grepl('suffix', variable.details.df$VariableProspectName)], variable.details.df)) {
      #Find the relevent identfier to be found in lookups
      for (pattern in stringr::str_remove_all(variable.details.df$VariableProspectName[grepl('suffix', variable.details.df$VariableProspectName)], 'suffix')) {
        if (grepl(pattern, name)) {
          matching.lookups.field=trimws(gsub("[[:punct:][:space:]]+", "_", pattern), which = 'both', whitespace = '_')
          break
        }
      }
      #Label column with appropriate flag name and code name
      attr(main.df[, c(name)], 'label') <- cds_labels_df$label[cds_labels_df$field==matching.lookups.field & cds_labels_df$code==stringr::str_remove(name, pattern)]
      
      #If still can't be found then might be a duplicated namne in the main dataframe so then will have prospect file name appended
      #Of split off different pieces and check the reconstructed variable name for reconstructed prospect file name
    } else {
      name.splt<-stringr::str_split(name, '_')[[1]]
      for (i in 1:length(name.splt)) {
        n=paste(name.splt[1:i], collapse='_')
        n.df<-paste(
          name.splt[
            ifelse(i==length(name.splt), i, (i+1)):length(name.splt)
          ], collapse='_')
        if (n %in% cds_labels_df$field) {
          if (!all(is.na(cds_labels_df$code[cds_labels_df$field==n]))) {
            main.df[, c(name)] <- factor(main.df[, c(name)],
                                         levels = cds_labels_df$code[cds_labels_df$field==n & cds_labels_df$form==n.df],
                                         labels = cds_labels_df$label[cds_labels_df$field==n & cds_labels_df$form==n.df]
            )
            attr(main.df[, c(name)], 'label') <- field.description.df$Label[field.description.df$Identifier==n &
                                                                              field.description.df$Form==n.df]
          }}
      }
    }
  }
  
  return(main.df)
}


