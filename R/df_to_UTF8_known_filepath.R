#' Converts a dataframe into UTF-8 from a known filepath'
#'
#' @description
#' `df_to_UTF8_known_filepath`  Returns the dataframe with encoding
#' of UTF-8 instead the dataframe's previous encoding.
#'
#' @param df (dataframe) The dataframe to be converted
#'
#' @param file.path (string) File path to location of the csv to be read in
#'
#' @returns (dataframe)  Returns a dataframe from the file path with encoding
#' of UTF-8 instead the file's previous encoding.
#'
#'  @examples df_to_UTF8_known_filepath(lookups,
#'                                      file.path = paste0(get_data, '/Lookups.csv'))
#' @export
df_to_UTF8_known_filepath<-function(df = .,
                                    file.path) {

  best_encoding_match<-readr::guess_encoding(file.path)$encoding[1]
  return(df %>%
           mutate_if(is.character,
                     function(col) iconv(col,
                                         from=best_encoding_match,
                                         to="UTF-8")
           )
  )
}
