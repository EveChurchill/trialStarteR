#' Add a description of the variable to the dataframe column'
#'
#' @description
#' `mutate_dict_duplicates` adds a description (found in the specified data dictionary)
#' to the column for use in constructing the central dataframe. Adding descriptions
#' to each variable is an important part of data sharing and reproducibility.
#'
#' @param dictionary_df (dataframe) The dataframe which needs the variable names
#' mutated in order to match with the central dataframe
#'
#' @param dupl_n (vector) A vector of the duplicated names within the selected
#' CRFs to be mutated.
#'
#' @returns (dataframe)  Returns the dataframe with the duplicated names amended
#' to distinct naming that follows the central dataframe nomenclature.
#'
#'  @examples lookups<-mutate_dict_duplicates(dupl_n = c('age', 'sex', 'wd_dt'))
#' @export

mutate_dict_duplicates<-function(dictionary_df = lookups, dupl_n = duplicated_names) {
  if (!'form' %in% colnames(dictionary_df)) {
    original_colnames=colnames(dictionary_df)
    colnames(dictionary_df)[original_colnames==c('Form', 'Identifier')]<-c('form', 'field')
  } else {
    original_colnames=colnames(dictionary_df)
  }

  dictionary_df <-
    dictionary_df %>%
    rowwise() %>%
    mutate(
      field = ifelse(field %in% dupl_n,
                     paste(field, form, sep='_'),
                     field)
    )


  colnames(dictionary_df)=original_colnames
  return(dictionary_df)
}
