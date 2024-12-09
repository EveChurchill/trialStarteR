#' Decide whether to merge or insert variables
#'
#' @description
#' `merge_or_insert` inserts new columns for constant variables over time, and
#' merges variables which vary over time. It returns the inputted dataframe post-change,
#' and any variables to be inserted with their corresponding prospect df name.
#'
#' @details
#' Three returned items: the dataframe, the variables to be inserted, the variables' dataframe name in PROSPECT.
#'
#' Either the dataframe will be returned unchanged with the list of variables and a list of their respective PROSPECT df names.
#' Or the dataframe will be returned with the variables merged into the dataframe, and the lists will be unchanged from input.
#'
#' Adverse events will be appended on to end of the dataframe with the event_names being 'Adverse Event 1' , 'Adverse Event 2' etc.
#' These numbers correspond to the number of adverse events each individual has recorded within the dataframe.
#'
#' @param main.df (dataframe) This the dataframe to which the variables will be added.
#'
#' @param df.text.name (string) This is the string name of PROSPECT dataframe where the variables can be found
#'
#' @param variable_s (vector) This is a list of the variable(s) to add to the dataframe
#'
#' @param single_occ.var (vector) This is the accumulating list of variables to be inserted at the end of all merges.
#'
#' @param single_occ.var.df (vector) This is the respective PROSPECT dataframe names of single_occ.var
#'
#' @returns (dataframe, vector, vector) returns either updated dataframe with unchanged lists, or unchanged dataframe with updated lists
#'
#'  @examples result<-merge_or_insert(main.df,df.text.name, colnames(get(df.text.name)), single_occ.var, single_occ.var.df)
#'            main.df<-as.data.frame(result[[1]]); single_occ.var<-result[[2]]; single_occ.var.df<-result[[3]]
#' @export



merge_or_insert<-function(main.df, df.text.name, df, variable_s, single_occ.var, single_occ.var.df){
    if (all(df$event_name==df$event_name[1]) & !(grepl('adverse', df.text.name))){
      single_occ.var=append(single_occ.var, variable_s[!(variable_s %in% standard.set.column)])
      single_occ.var.df=append(single_occ.var.df, rep(df.text.name, length(variable_s[!(variable_s %in% standard.set.column)])))
      
    } else if (grepl('adverse', df.text.name)) {
      
      ae_event_name<-c()
      for (id in unique(df$screening)){
        ae_event_name<-append(ae_event_name,
                              paste('Adverse Event', 1:sum(df$screening==id), sep=" "))
      }
      df$event_name<-ae_event_name
      
      #+ Adverse Events
      main.df<- dplyr::bind_rows(main.df,
                                 
                                 df[ , variable_s])
      
      
    } else {
      main.df<-merge(main.df,
                     df[ , variable_s],
                     by=c("screening", "event_id", 'event_name'), all = TRUE)
    }
  return(list(main.df, single_occ.var, single_occ.var.df))
}

