% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/labelled_to_factors.R
\name{labelled_to_factors}
\alias{labelled_to_factors}
\title{Convert all \code{labelled}-class columns to factors.}
\usage{
labelled_to_factors(labels_df)
}
\arguments{
\item{labels_df}{a data.frame containing some columns of class labelled}
}
\value{
Returns a data.frame.
}
\description{
Takes a data.frame, checks for columns that are class \code{labelled} from the \code{haven} package, and converts them to factor class.
}
\examples{
# not run
# haven::read_spss(filepath) \%>\% labelled_to_factor()
}
