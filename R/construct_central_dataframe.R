#' Central dataframe construction
#'
#' @description
#'   `construct_central_dataframe` returns a central data frame which can be used
#'   as a base dataset for primary analyses or for sharing.
#'
#' @details
#'   From master list created by Read-data.R, this function will produce a
#'   dataframe of all participants (from screening onwards) with all the
#'   variables from the CRFs specified in
#'   Inputs/AnalysisVariablesDetails.xlsx.
#'
#'    Any constant characteristics for each participant will be displayed in its
#'    own column, for example: sex, ethnicity.
#'
#'   Adverse events can also be included to this dataset when
#'   include.adverse = TRUE (no need to specify).
#'
#'   If blinded='y', a dummy randomisation will be implemented for those
#'   participants with a baseline event. If blinded='n', the
#'   randomisation allocation will be taken from PROSPECT.
#'
#'   If there are duplicate variable names, the dataframe names will be appended to
#'   them e.g. age_demographics
#'
#' @param analysis_variable_df (data frame) should be the data frame in the
#'   Inputs folder. An example can be seen in the template folder.
#'
#' @param dataframe_list (list) the master list produced by Read-data.R
#'
#' @param number.arms (integer) the number of arms in the trial.
#'
#' @param visit_df_name (string) This is the name of the PROSPECT csv
#'   file which contains the details of all visits. In some
#'   trials, this is `"visit_completion"` or `"events"`. Default is
#'   `"visit_completion"`.
#'
#' @param screening_df_name (string) This is the name of the PROSPECT csv
#'   file which contains the details of all participants screened. In some
#'   trials, this is the "identification_log" or simply "screening_log".
#'
#' @param id_cols (vector) the names of the id columns as a character vector.
#'   Standard is `c("screening", "event_name")` and any additional variables
#'   to be used as an identifier (so will be used when merging dataframes)
#'   should be added AFTER the standard.
#'
#' @param field.description.df (data frame) the data frame containing the field
#'   labels. Default is `fields`, this is often read in from Fields.csv.
#'
#' @param include.adverse (boolean) whether to include adverse events as seperate
#'    or as part of the central dataframe. Default is FALSE.
#'
#' @param blinded (character) This should be y or n
#'
#' @returns (list of 2 items: dataframe and list of dataframes) central dataframe
#'  to be used as a base for analyses or sharing AND the revised master list -
#'  some variables renamed where duplicated.
#'
#' @examples
#' \dontrun{
#' central <- construct_central_dataframe(
#'    analysis_variable_df = .,
#'    dataframe_list = master,
#'    number.arms = N.Arms,
#'    id_cols = id_cols,
#'    field_df = fields,
#'    include.adverse = T,
#'    blinded = 'n')
#' }
#'
#' @export
################################################################################
## Function name: core dataset construction
################################################################################

construct_central_dataframe<-function(
    analysis_variable_df = .,
    dataframe_list = master,
    visit_df_name = 'visit_completion',
    screening_df_name = 'identification_log',
    number.arms = N.Arms,
    id_cols = c('screening' , 'event_name'),
    field_df = fields,
    include.adverse = F, #default is F,
    blinded = 'n'
) {




  # Main body of function ---------------------------------------------------

rlog::log_info("Converting PROSPECT df names to compatible versions")
field_df <- renameCRF_toR(field_df,  
                          form.column.name = "Form",
                          subform.column.name = 'Subform')
req_dataframes <- uniq_nonNA(analysis_variable_df$`Required-CRF-Name`)
if (include.adverse) {
  req_dataframes <- append(req_dataframes, field_df$Form[grepl("adverse", 
                                                               field_df$Form, ignore.case = T)][1])
}
field_df <- field_df %>% filter(Form %in% req_dataframes)
rlog::log_info("Checking for duplicated names")
duplicated_names <- apply(as.data.frame(table(field_df$Identifier)) %>% 
                            filter(!Var1 %in% standard.set.column & Freq > 1) %>% 
                            select(Var1), as.character, MARGIN = 1)
id_cols <- id_cols[(id_cols %in% colnames(dataframe_list[[visit_df_name]])) & 
                     (id_cols %in% colnames(dataframe_list[[screening_df_name]]))]
if (length(id_cols) < 2) {
  stop("Missing/Unavailable unique identifier column")
}
rlog::log_info("Creating base dataframe")
main_df <- expand.grid(screening = dataframe_list[[visit_df_name]][dataframe_list[[visit_df_name]][, 
                                                                                                   c("event_name")] == timepoints[1], c("screening")], event_name = timepoints) %>% 
  rbind(data.frame(screening = dataframe_list[[screening_df_name]][, 
                                                                   c("screening")], event_name = rep("Screening", dim(dataframe_list[[screening_df_name]])[1]))) %>% 
  arrange(order(screening))
rlog::log_info("Adding longitudinal data")
single_occ_var <- c("site")
single_occ_var_df <- c(screening_df_name)
for (df.text.name in req_dataframes) {
  if (any(dim(dataframe_list[[df.text.name]]) == 0)) {
    next
  } else {
    corresponding.labels <- fields$Label[match(colnames(dataframe_list[[df.text.name]]), 
                                               fields$Identifier)]
    df_spec_cols <- colnames(dataframe_list[[df.text.name]])[!colnames(dataframe_list[[df.text.name]]) %in% 
                                                               standard.set.column & !(is.na(corresponding.labels) | 
                                                                                         grepl("name", corresponding.labels, ignore.case = T) | 
                                                                                         grepl("assessor", corresponding.labels, ignore.case = T))]
    df.duplicated.column <- df_spec_cols[df_spec_cols %in% 
                                           duplicated_names]
    if (any(!purrr::is_empty(df.duplicated.column) & 
            !is.na(df.duplicated.column))) {
      colnames(dataframe_list[[df.text.name]]) <- ifelse(colnames(dataframe_list[[df.text.name]]) %in% 
                                                           df.duplicated.column, paste(df.duplicated.column, 
                                                                                       df.text.name, sep = "_"), colnames(dataframe_list[[df.text.name]]))
      df_spec_cols <- colnames(dataframe_list[[df.text.name]])[!colnames(dataframe_list[[df.text.name]]) %in% 
                                                                 standard.set.column & !grepl("_sig", colnames(dataframe_list[[df.text.name]]))]
    }
    merge.ids <- id_cols[(id_cols %in% colnames(dataframe_list[[df.text.name]])) & 
                           (id_cols %in% colnames(main_df))]
    
    if (all(dataframe_list[[df.text.name]][, c("event_name")] == 
            dataframe_list[[df.text.name]][, c("event_name")][1]) & 
        !(grepl("adverse", df.text.name))) {
      single_occ_var = append(single_occ_var, df_spec_cols)
      single_occ_var_df = append(single_occ_var_df, 
                                 rep(df.text.name, length(df_spec_cols)))
      
    } else if (grepl("adverse", df.text.name)) {
      ae_event_name <- c()
      for (id in unique(dataframe_list[[df.text.name]][, 
                                                       c("screening")])) {
        ae_event_name <- append(ae_event_name, paste("Adverse Event", 
                                                     1:sum(dataframe_list[[df.text.name]][, c("screening")] == 
                                                             id), sep = " "))
      }
      dataframe_list[[df.text.name]][, c("event_name")] <- ae_event_name
      dataframe_list[[df.text.name]]$visit_dt <- df$rep_dt
      main_df <- dplyr::bind_rows(main_df, dataframe_list[[df.text.name]][, 
                                                                          c(merge.ids, "visit_dt", df_spec_cols)])
    } else {
      main_df <- merge(main_df, dataframe_list[[df.text.name]][, 
                                                               c(merge.ids, df_spec_cols)], by = merge.ids, 
                       all = TRUE)
    }
  }
}
rlog::log_info("Adding characteristic data")
single_occ_data_list <- purrr::map2(single_occ_var, single_occ_var_df, 
                                    function(char_var, df_name) {
                                      df <- dataframe_list[[df_name]]
                                      if (char_var %in% colnames(df) & !char_var %in% colnames(main_df)) {
                                        return(df %>% dplyr::select(screening, !!char_var))
                                      }
                                      else {
                                        return(NULL)
                                      }
                                    }) %>% purrr::compact()
combined_single_occ <- purrr::reduce(single_occ_data_list, 
                                     dplyr::full_join, by = "screening")
main_df <- main_df %>% dplyr::left_join(combined_single_occ, 
                                        by = "screening")
rlog::log_info("Organising dataframe")
main_df<-main_df[ ,!grepl('\\.y', colnames(main_df))]
colnames(main_df)<-colnames(main_df) %>% str_remove_all('\\.x')
main_df <- main_df[, c(id_cols, "site", single_occ_var, colnames(main_df)[
  !colnames(main_df) %in% c(id_cols, "site", single_occ_var)
])
]
main_df <- main_df[, !duplicated(colnames(main_df))]
rlog::log_info("Dummy randomising if needed")
if (blinded == "y") {
  set.seed(2602)
  screening_ids <- unique(dataframe_list[[visit_df_name]][["screening"]])
  rand_lookup <- tibble(screening = screening_ids, rand_arm = sample(1:number.arms, 
                                                                     length(screening_ids), replace = TRUE), rand_dt = dataframe_list[[visit_df_name]][["visit_dt"]][match(screening_ids, 
                                                                                                                                                                           dataframe_list[[visit_df_name]][["screening"]])])
} else {
  rand_lookup <- randomisation %>% dplyr::select(screening, 
                                                 rand_arm, rand_dt)
}
main_df <- main_df %>% dplyr::full_join(rand_lookup, by = "screening")
rlog::log_info("Labelling and converting columns")
main_df <- label_n_convert(main_df, dupl_n = duplicated_names)

  return(list(main_df,
              dataframe_list
  )
  )
}
