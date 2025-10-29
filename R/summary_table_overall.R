#' Creates a summary table by arm and overall for reporting
#'
#' @description
#'   `summary_table_overall` returns a dataframe which summarises the
#'   specified characteristics overall only with no percentages. With
#'   mean, median, sd, IQR, min and max for numerical variables, as well as counts of the number
#'   of each non-empty item in both numeric and categorical variables
#'   including each group within the categorical variables. There is also an
#'   option to add the median absolute deviation to the continuous summaries.
#'   This function was made with the screening data in mind as this table is
#'   overall only and does not provide percentages because not all screened
#'   participants will answer each question e.g. reason not consented
#'
#' @param data_to_summarise (dataframe) the dataframe containing all the trial data -
#'   ideally created by the construct_central_dataframe function in this package.
#'
#' @param summary_variables (vector) the variables to be summarised within this table
#'
#' @param id_cols (vector) the names of the id columns as a character vector.
#'   Default is `c("screening", "event_name", "event_id")`.
#'
#' @param population.list.obj (list) a list object of the same length as the
#'   number of arms. Each list element is a character vector containing the
#'   screening numbers of participants in an arm in the relevant analysis
#'   population.
#'#'
#' @returns (dataframe) returns a dataframe which summarises the specified
#'    characteristics by arm and overall
#'
#' @export
summary_table_overall <- function(
    data_to_summarise = .,
    summary_variables = analysis_variable_df$`Screening-Summary-Variables`,
    population.list.obj = itt,
    id_cols = c("screening", "event_name")
) {

  summary_variables<-summary_variables[!is.na(summary_variables)]

  # Initialize the results table
  summary_table.presented <- data.frame(
    Measure = character(),
    Overall = character(),
    stringsAsFactors = FALSE
  )

  # Loop through each variable to summarize
  for (variable in summary_variables) {

    # Check if the variable exists and handle potential duplicates interactively
    if (!variable %in% colnames(data_to_summarise)) {
      duplicates <- colnames(data_to_summarise)[grepl(variable, colnames(data_to_summarise))]
      if (length(duplicates) == 0) {
        warning(paste("Variable '", variable, "' not found. Skipping.", sep = ""))
        next
      } else if (length(duplicates) > 1) {
        cat('Which of these should be used for the summary table? (Enter the number of your choice)\n')
        cat(paste0(1:length(duplicates), ') ', duplicates, collapse = '\n'))
        choice <- readline(prompt = "Choice: ")
        variable <- duplicates[as.numeric(choice)]
      }
    }

    # Get variable label, if available
    item_name <- attr(data_to_summarise[[variable]], 'label')
    if (is.null(item_name)) {
      item_name <- variable # Fallback to using the variable name
      warning(paste("Label for '", variable, "' not found. Using variable name instead.", sep = ""))

    }

    # Summarize continuous variables
    if (is.numeric(data_to_summarise[[variable]])) {
      data_to_log <- cont.summ_to_string(
        continuous.data.name = variable,
        dataframe_object = data_to_summarise,
        by_arm = FALSE
      )

      rows_to_add <- data.frame(
        Measure = c(
          paste0(item_name, ': N'),
          "\\hspace{0.25cm} Mean (SD)",
          "\\hspace{0.25cm} Median (IQR)",
          "\\hspace{0.25cm} Min, Max"
        ),
        Overall = c(data_to_log[[1]], data_to_log[[2]], data_to_log[[3]], data_to_log[[4]]),
        stringsAsFactors = FALSE
      )

      summary_table.presented <- rbind(summary_table.presented, rows_to_add)

      # Summarize categorical variables
    } else if (!is.null(levels(data_to_summarise[[variable]]))) {
      row_text <- cat.summ_to_string(
        categorical.data.name = variable,
        dataframe_object = data_to_summarise,
        by_arm = FALSE
      )

      rows_to_add <- data.frame(
        Measure = sapply(row_text, `[[`, 1),
        Overall = sapply(row_text, `[[`, 2),
        stringsAsFactors = FALSE
      )

      summary_table.presented <- rbind(summary_table.presented, rows_to_add)
    }
  }

  return(summary_table.presented)
}
