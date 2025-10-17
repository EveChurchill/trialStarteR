#' Converts a dataframe with CRF names into R compatible version'
#'
#' @description
#' `renameCRF_toR`  Returns the dataframe with R compatible converted CRF names.
#'                  This is particiularly helpful when reading things in from the
#'                   lookups file or any other data dictionary.
#'
#' @param df (dataframe) The dataframe to be converted
#'
#' @param form.column.name (string) The column name containing the CRF form names
#'
#' @returns (dataframe)  Returns a dataframe from the converted CRF names.
#'
#'  @examples renameCRF_toR(fields,
#'                           form.column.name = 'Form')
#' @export
renameCRF_toR<-function(df = .,
                        form.column.name = 'form') {

  stopifnot(form.column.name %in% colnames(df))


  #Transform form names into R appropriate names
  df[[form.column.name]] <- df[[form.column.name]] %>%
    stringr::str_remove(".csv") %>%
    stringr::str_replace_all("( - )| ", "_") %>%
    stringr::str_remove_all("[^([[:alnum:]]|_)]|\\(|\\)") %>%
    stringr::str_to_lower()

  df[[
    ifelse(form.column.name=='form', 'field', 'Identifier')
  ]]<-df[[
    ifelse(form.column.name=='form', 'field', 'Identifier')
  ]] %>%
    stringr::str_remove_all('\\[calculated\\] ')
  return(df)
}
