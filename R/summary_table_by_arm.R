#' Creates a summary table by arm and overall for reporting
#'
#' @description
#'   `summary_table_by_arm` returns a dataframe which summarises the
#'   specified characteristics per arm and overall. With mean, median, sd,
#'   IQR, min and max for numerical variables, as well as counts of the number
#'   of each non-empty item in both numeric and categorical variables
#'   including each group within the categorical variables. There is also an
#'   option to add the median absolute deviation to the continuous summaries.
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
summary_table_by_arm <- function(
    data_to_summarise = .,
    summary_variables = analysis_variable_df$`Baseline-Summary-Variables`,
    population.list.obj = itt,
    id_cols = c("screening", "event_name")
) {

  # Initialize the results table with correct column names and types
  column_names <- c('Measure', arm.names, 'Overall')
  summary_table.presented <- data.frame(
    matrix(nrow = 0, ncol = length(column_names)),
    stringsAsFactors = FALSE
  )
  colnames(summary_table.presented) <- column_names

  # Loop through each variable to summarize
  for (variable in summary_variables) {

    # Check if the variable exists and handle potential duplicates interactively
    if (!variable %in% colnames(data_to_summarise)) {
      duplicates <- colnames(data_to_summarise)[grepl(variable, colnames(data_to_summarise))]
      if (length(duplicates) == 0) {
        warning(paste("Variable '", variable, "' not found. Skipping.", sep = ""))
        next
      } else if (length(duplicates) > 1) {
        cat('Which of these should be used for the baseline table? (Enter the number of your choice)\n')
        cat(paste0(1:length(duplicates), ') ', duplicates, collapse = '\n'))
        choice <- readline(prompt = "Choice: ")
        variable <- duplicates[as.numeric(choice)]
      }
    }

    # Get variable label, with a fallback to the variable name
    item_name <- attr(data_to_summarise[[variable]], 'label')
    if (is.null(item_name)) {
      item_name <- variable
      warning(paste("Label for '", variable, "' not found. Using coded variable name instead.", sep = ""))
    }

    # Summarize continuous variables
    if (is.numeric(data_to_summarise[[variable]])) {
      data_to_log <- cont.summ_to_string(
        continuous.data.name = variable,
        dataframe_object = data_to_summarise,
        by_arm = TRUE
      )

      # Prepare data for new rows
      counts <- unlist(data_to_log[[1]])
      percentages <- percentage_summaries_perArmOverall(counts, population.list.obj = population.list.obj)

      # Combine counts and percentages into a formatted string
      n_perc_string <- paste0(counts, ' (', percentages, '%)')

      rows_to_add <- data.frame(
        Measure = c(
          paste0(item_name, ': N (%)'),
          "\\hspace{0.25cm} Mean (SD)",
          "\\hspace{0.25cm} Median (IQR)",
          "\\hspace{0.25cm} Min, Max"
        ),
        stringsAsFactors = FALSE
      )

      rows_to_add <- cbind(rows_to_add,
                           t(data.frame(
                             n_perc_string,
                             data_to_log[[2]],
                             data_to_log[[3]],
                             data_to_log[[4]]
                           )))

      colnames(rows_to_add) <- column_names
      summary_table.presented <- rbind(summary_table.presented, rows_to_add)

      # Summarize categorical variables
    } else if (!is.null(levels(data_to_summarise[[variable]]))) {
      row_text <- cat.summ_to_string(
        categorical.data.name = variable,
        dataframe_object = data_to_summarise,
        by_arm = TRUE
      )

      rows_to_add <- do.call(rbind, row_text)
      colnames(rows_to_add) <- column_names
      summary_table.presented <- rbind(summary_table.presented, rows_to_add)
    } else {
      warning(paste("Variable '", variable, "' is not numeric or a factor. Skipping.", sep = ""))
      next
    }
  }

  return(summary_table.presented)
}
