#' Fill missing data at a timepoint with data from another specified timepoint'
#'
#' @description
#' `fill_missing` fills in NA values where possible from another specified
#' event
#'
#' @param multievent_data (dataframe) A dataframe you wish to summarise
#'
#' @param focus_var (string) Variable to have NAs filled
#'
#' @param event_col (string) Column name of event names. Default 'event_name'
#'
#' @param target_event (string) Event whose NAs to be replaced
#'
#' @param replacement_event (string) Event who can replace NAs with available data
#'
#' @returns (column)  Returns the more complete column
#'
#'  @examples core_baseline<-fill_missing(core.dataframe,
#'                                        focus_var = 'age_demographics'
#'                                        )
#' @export

fill_missing<-function(multievent_data = .,
                       focus_var,
                       event_col = 'event_name',
                       target_event = 'Baseline',
                       replacement_event='Screening') {

  return(multievent_data[multievent_data[, event_col]==target_event, c('screening' ,
                                                                       focus_var)
  ] %>%
    filter(is.na(!!sym(focus_var))) %>%
    full_join(
      multievent_data[multievent_data[, event_col]==replacement_event, c( 'screening'  ,
                                                                          focus_var)],
      by=c('screening')

    ) %>%
    select(!screening) %>%
    mutate(
      best = coalesce(!!!syms(colnames(.)))
    ) %>%
    select(best))
}
