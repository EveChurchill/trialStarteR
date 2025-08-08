#' Reads in csv files to UTF-8'
#'
#' @description
#' `read_csv_to_UTF8`  Returns a dataframe from the file path with encoding
#' of UTF-8 instead the file's previous encoding.
#'
#' @param file.path (string) File path to location of the csv to be read in
#'
#' @returns (dataframe)  Returns a dataframe from the file path with encoding
#' of UTF-8 instead the file's previous encoding.
#'
#'  @examples read_csv_to_UTF8(paste0(get_data, '/Lookups.csv'))
#' @export
read_csv_to_UTF8<-function(file.path = .,
                           ...) {
  return(df_to_UTF8_known_filepath(
    df = utils::read.csv(file.path, stringsAsFactors = FALSE, ...),
    filepath = file.path))
}
