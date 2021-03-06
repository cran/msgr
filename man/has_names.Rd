% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predicates.R
\name{has_names}
\alias{has_names}
\title{Checks whether the variable has names}
\usage{
has_names(x, nm)
}
\arguments{
\item{x}{(any) The object to test}

\item{nm}{(character, optional) The names to check for. If not specified then the function
checks for any names.}
}
\value{
TRUE if x has any names, FALSE otherwise
}
\description{
Checks whether the variable has names
}
\examples{
x <- list(a = 1, b = 2)

has_names(x, "a")
has_names(x, c("a", "b"))

has_names(x, "c")

}
