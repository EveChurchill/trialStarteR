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
                        form.column.name = 'form',
                        subform.column.name = 'subform'
                       ) {

  
  
  stopifnot(form.column.name %in% colnames(df))
  
  colnames(df)[match(
    c(form.column.name, subform.column.name),
    colnames(df)
  )] <- c('Form', 'Subform')
  
  
  #Transform form names into R appropriate names
  df = df %>% 
    rowwise() %>%
    mutate(R_dfName = 
             case_when(
               !(Subform =='') ~ paste(
                 Form, Subform,
                 sep='_'), 
               .default = Form) %>%
             str_replace_all("( - )| ", "_") %>%
             str_remove_all("^[[:digit:]]+") %>%
             str_remove_all("\\(|\\)|-|/") %>%
             str_to_lower()
    )
  
  colnames(df)[match(
    c('Form', 'Subform'),
    colnames(df)
  )] <- c(form.column.name, subform.column.name)

  return(df)
}
