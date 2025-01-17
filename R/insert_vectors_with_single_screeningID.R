#' Allocates single ID occurrence dataframe vector to dataframe with multiple events
#'
#' @description
#' `insert_vectors_with_single_screeningID` returns a vector to be added as a column
#' to a dataframe which has multiple occurences of the same IDs from a source with single occurences of IDs.
#'
#' @details
#' If the input_dataframe's screening number does not occur in the correspond.indivID.vector, then NA will be in its place.
#'
#' @param input_dataframe (dataframe) This the dataframe that we want to add the variables to.
#'
#' @param vector_to_allocate (vector) This is the variable's data vector we want to add to the dataframe.
#'
#' @param correspond.indivID.vector (vector) This is the corresponding ID list for vector_to_allocate
#'
#' @returns (vector) returns a vector with the variable's elements in the appropriate indices for that screening ID.
#'
#'  @examples main.df$site<-insert_vectors_with_single_screeningID(main.df,
#'                                       visit_completion$site[!duplicated(visit_completion$screening)],
#'                                       visit_completion$screening[!duplicated(visit_completion$screening)])
#' @export

insert_vectors_with_single_screeningID<-function(input_dataframe, vector_to_allocate, correspond.indivID.vector ) {
  allocated<-c()
  for (i in input_dataframe[,c(id_cols[1])]) {
    if (i %in% correspond.indivID.vector) {
      allocated<-append(allocated, vector_to_allocate[which(correspond.indivID.vector==i)])
    } else {
      allocated<-append(allocated, NA)
    }
  }
  return(allocated)
}

