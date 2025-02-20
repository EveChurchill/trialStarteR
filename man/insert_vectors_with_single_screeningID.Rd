% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/insert_vectors_with_single_screeningID.R
\name{insert_vectors_with_single_screeningID}
\alias{insert_vectors_with_single_screeningID}
\title{Allocates single ID occurrence dataframe vector to dataframe with multiple
  events}
\usage{
insert_vectors_with_single_screeningID(
  input_dataframe,
  id_cols,
  vector_to_allocate,
  correspond.indivID.vector
)
}
\arguments{
\item{input_dataframe}{(data frame) This the data frame that we want to add
the variables to.}

\item{id_cols}{(vector) the names of the id columns as a character vector.
Default is `c("screening", "event_name", "event_id")`.}

\item{vector_to_allocate}{(vector) This is the variable's data vector we want
to add to the data frame.}

\item{correspond.indivID.vector}{(vector) This is the corresponding ID list
for vector_to_allocate}
}
\value{
(vector) returns a vector with the variable's elements in the
  appropriate indices for that screening ID.

 @examples main.df$site<-insert_vectors_with_single_screeningID(main.df,
                                      visit_completion$site[!duplicated(visit_completion$screening)],
                                      visit_completion$screening[!duplicated(visit_completion$screening)])
}
\description{
`insert_vectors_with_single_screeningID` returns a vector to be added as a
  column to a dataframe which has multiple occurences of the same IDs from a
  source with single occurences of IDs.
}
\details{
If the input_dataframe's screening number does not occur in the correspond.
  indivID.vector, then NA will be in its place.
}
