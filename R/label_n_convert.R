#' Label columns descriptively and convert to the correct type'
#'
#' @description
#' `label_n_convert` labels and converts each column in the dataframe
#' according to its type in the data dictionary.
#'
#' @param main.df (dataframe) A dataframe you wish to add labels and convert the
#' column types. It is used within the construct_central_dataframe to provide
#' clarity on variables
#'
#' @param dupl_n (string) Any duplicated column names
#'
#' @param label.dictionary (dataframe) The data dictionary where the
#'        label of the variable can be found
#'
#' @param type.dictionary (dataframe) The data dictionary where the
#'        type of the variable can be found
#'
#' @returns (dataframe)  Returns the labeled and converted appropriately dataframe
#'
#'  @examples core<-label_n_convert(unlabelled.core.dataframe)
#' @export

label_n_convert<-function(
    main.df = .,
    dupl_n = duplicated_names,
    label.dictionary = lookups,
    type.dictionary = fields
) {

    label.dictionary<-mutate_dict_duplicates(
      renameCRF_toR(label.dictionary,
                    form.column.name = 'form',
                    subform.column.name = 'subform'),
      dupl_n
    )
    
    type.dictionary<-mutate_dict_duplicates(
      renameCRF_toR(type.dictionary,
                    form.column.name = 'Form',
                    subform.column.name = 'Subform'),
      dupl_n, variableName_col = 'Identifier'
    )

  for (col in colnames(main.df)[!colnames(main.df) %in% standard.set.column]) {


    if (col %in% type.dictionary$Identifier) {
      col_labels = label.dictionary[label.dictionary$field==col, ][1, ]
      col_type = type.dictionary$Type[type.dictionary$Identifier==col][1]
      suffix=NA
    } else {
      #  if no exact match found, probably a flag variable
      stem = stringr::str_split(col, '_')
      flag_var = paste(head(unlist(stem), -1), collapse='_') #Flag variable most likely listed under
      #the column name without the last _xyz

      suffix = stringr::str_remove(col, paste0(flag_var, '_'))

      col_type.df = type.dictionary[
        #Find the most likely flag for the identifier
        stringdist::stringdist(flag_var, type.dictionary$Identifier)==
          min(stringdist::stringdist(flag_var, type.dictionary$Identifier)) &
          #and for the options set
          stringdist::stringdist(flag_var, type.dictionary$Options)==
          min(stringdist::stringdist(flag_var, type.dictionary$Options)),
      ]
      if (any(dim(col_type.df)==0)) {
        next
      } else {
        col_labels = label.dictionary[label.dictionary$field==flag_var &
                                        label.dictionary$form==col_type.df$Form, ]
        col_type=col_type.df$Type[1]
      }
    }



    #If enum -> factor, use options to set levels ------------------------------
    rlog::log_info(paste0('Labelling and converting ', col))
    if (col_type=='Enum') {
      if (is.factor(main.df[[col]])) {
        main.df[[col]]<-add_description(main.df[[col]],
                                        col_name = col,
                                        description_dictionary = type.dictionary)
      } else {
        main.df[[col]]<-add_description(factor(
          col_labels$label[match(
            main.df[[col]],
            col_labels$code
          )]
          ,
          levels = col_labels$label
        ),
        col_name = col,
        description_dictionary = type.dictionary)
      }

    } else if (col_type=='Integer') {

      #If integer ----------------------------------------------------------------
      if (is.integer(main.df[[col]])) {
        main.df[[col]]<-add_description(main.df[[col]],
                                        col_name = col,
                                        description_dictionary = type.dictionary)
      } else {
        main.df[[col]]<-add_description(as.integer(main.df[[col]]),
                                        col_name = col,
                                        description_dictionary = type.dictionary)
      }


    } else if (col_type %in% c('String', 'Text')) {

      #If string or text, as character and label ---------------------------------
      if (is.character(main.df[[col]])) {
        main.df[[col]]<-add_description(main.df[[col]],
                                        col_name = col,
                                        description_dictionary = type.dictionary)
      } else {
        main.df[[col]]<-add_description(as.character(main.df[[col]]),
                                        col_name = col,
                                        description_dictionary = type.dictionary)
      }

    } else if (col_type %in% c('Date')) {

      #If date, as.Date, ---------------------------------------------------------
      if (is.Date(main.df[[col]])) {
        main.df[[col]]<-add_description(main.df[[col]],
                                        col_name = col,
                                        description_dictionary = type.dictionary)
      } else {
        main.df[[col]]<-add_description(as.Date(main.df[[col]]),
                                        col_name = col,
                                        description_dictionary = type.dictionary)
      }

    } else if (col_type=='Flag') {

      # If flag, then have to do some wrangling of strings
      if (is.factor(main.df[[col]])) {
        main.df[[col]]<-add_description(main.df[[col]],
                                        col_name = suffix,
                                        description_dictionary = label.dictionary)
      } else {
        main.df[[col]]<-add_description(as.factor(main.df[[col]]),
                                        col_name = suffix,
                                        description_dictionary = label.dictionary)
      }

    } else if ( col_type=='Decimal') {

      #If decimal -> numeric -----------------------------------------------------
      if (is.numeric(main.df[[col]])) {
        main.df[[col]]<-add_description(main.df[[col]],
                                        col_name = col,
                                        description_dictionary = type.dictionary)
      } else {
        main.df[[col]]<-add_description(as.numeric(main.df[[col]]),
                                        col_name = col,
                                        description_dictionary = type.dictionary)
      }
    } else {

      #If boolean or partialdate or time or etc (leave conversion but apply description)
      main.df[[col]]<-add_description(main.df[[col]],
                                      col_name = col,
                                      description_dictionary = type.dictionary)
    }
  }

  return(main.df)

}
