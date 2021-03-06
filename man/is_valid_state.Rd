% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/is_valid_state.R
\name{is_valid_state}
\alias{is_valid_state}
\title{Returns TRUE if the given state is a valid state spelling}
\usage{
is_valid_state(state, missing_as_na = T)
}
\arguments{
\item{state}{character string of the state to check}

\item{missing_as_na}{logical, whether NA should return NA (vs. FALSE)}
}
\value{
logical, TRUE if the given state is a valid state spelling
}
\description{
Returns TRUE if the given state is a valid state spelling given our state 
spelling naming conventions. Under our conventions, spellings include the 
50 states spelled out (e.g. "Alabama"), "District of Columbia", the five 
populated U.S. territories, and "Not Available" (e.g. for federal facilities 
without a known state designation). Includes a parameter to return FALSE vs. NA 
if the given state spelling is NA.
}
\examples{
is_valid_state("DC")
is_valid_state(c("DC", NA, "District of Columbia"))
}
