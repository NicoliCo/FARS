% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_summarize_years}
\alias{fars_summarize_years}
\title{Summarize FARS Data for Years}
\usage{
fars_summarize_years(years)
}
\arguments{
\item{years}{List of years. Each year will be coerced to an integer with \code{\link[=as.integer]{as.integer()}}}
}
\value{
A tibble containing the number of entries in the FARS data for each month of the given years. First column
is the month, and subsequent columns for each year. If a year is
invalid, then a warning message is issued.
}
\description{
This function takes as input a list of years and returns a summary of the data from
US National Highway Traffis Safety
Administration's
\href{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars}{Fatality Anlaysis Reporting System}
(FARS) for those years.
}
\examples{
library(FARS)
fars_summarize_years(c(2013,2014,2015))

}
