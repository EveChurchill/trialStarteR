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
    df.text.name<-variable.details.df$DfProspectName[variable.details.df$VariableProspectName==column.name.vector[1]]
    suffix.idx<-which(grepl('suffix', column.name.vector))

    #Remove suffix from the text
    suffix_text<-stringr::str_remove_all(column.name.vector[suffix.idx], 'suffix')
    #Updated variables
    column.name.vector<-column.name.vector[-c(suffix.idx)]
    for (s.t in suffix_text) {
      column.name.vector<-append(column.name.vector, colnames(get(df.text.name))[grepl(s.t, colnames(get(df.text.name)))])
    }
  }
  return(column.name.vector)
}
