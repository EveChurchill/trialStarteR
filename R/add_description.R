#' Add a description of the variable to the dataframe column'
#'
#' @description
#' `add_description` adds a description (found in the specified data dictionary)
#' to the column for use in constructing the central dataframe. Adding descriptions
#' to each variable is an important part of data sharing and reproducibility.
#'
#' @param col_object (column from dataframe) The column from the dataframe which
#'                                            needs a description
#'
#' @param col_name (string) The column name to be found in the dictionary
#'
#' @param description_dictionary (dataframe) The data dictionary where the
#'        description of the variable can be found
#'
#' @param ticked (boolean) Default: FALSE. If a variable is a 'flag' or not.
#'
#' @returns (dataframe)  Returns the column with same attributes as before with
#' added descriptive label.
#'
#'  @examples core$sex<-add_description(core$sex,
#'                                      col_name='sex',
#'                                      description_dictionary = fields)
#' @export

add_description<-function(col_object, col_name, description_dictionary, ticked = F) {
  if (ticked == T) {
    attributes(col_object)[['label']] = description_dictionary$Type[description_dictionary$Label==col_name]
  } else {
    attributes(col_object)[['label']] = description_dictionary$Label[description_dictionary$Identifier==col_name]
  }
  return(col_object)
}
