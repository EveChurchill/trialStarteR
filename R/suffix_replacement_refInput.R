#' Extracts variables from specified data featuring a 'suffix'
#'
#' @description
#' `suffix_replacement_refInput` returns the variable names which are standalone names and
#'  any names which correspond with the 'suffix' text.
#'
#'
#' @details
#' 'suffix' in a variable name denotes that whatever other text alongside suffix will be used to collect variable names in the specified dataframe
#' For example 'suffix_fa' will collect all variable names which include _fa
#' This is used when inputting variables from the AnalysisVariablesDetails.xlsx which names the variables to be in the master dataframe.
#'
#' @param column.name.vector (vector) Vector of variable names of a certain dataframe.
#' One or more elements with contain 'suffix' as part of the variable name. E.g.  c('consent_yn', 'no_cons_rsn_suffix',...)
#'
#' @returns (vector) returns a vector with all variable names including the original non-'suffix' elements and
#'  all variable names within that dataframe with the specified text inside the 'suffix' variable name.
#'  E.g. c('consent_yn', 'no_cons_rsn_not_int', 'no_cons_rsn_not_avail', ...)
#'
#'  @examples df_spec_cols<-suffix_replacement_refInput(df_spec_cols)
#' @export

suffix_replacement_refInput<-function(column.name.vector){
  if (any(grepl('suffix', column.name.vector))){
    df.text.names<-variable.details.df$DfProspectName[match(column.name.vector, variable.details.df$VariableProspectName)]
  
    new.column.names<-c()
    for (i in 1:length(column.name.vector)) {
      if (grepl('suffix', column.name.vector[i])) {
        s.t<-stringr::str_remove_all(column.name.vector[i], 'suffix')
        df.text.name=df.text.names[i]
        new.column.names<-append(new.column.names, colnames(get(df.text.name))[grepl(s.t, colnames(get(df.text.name)))])
      } else {
        new.column.names<-append(new.column.names, column.name.vector[i])
      }
    }
  } else {
    new.column.names<-column.name.vector
  }
  return(new.column.names)
}
