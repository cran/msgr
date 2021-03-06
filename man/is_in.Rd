% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predicates.R
\name{is_in}
\alias{is_in}
\title{Checks whether all elements of one variable are in another}
\usage{
is_in(x, y)
}
\arguments{
\item{x}{(any) The object with elements to test}

\item{y}{(any) The object with elements to test against}
}
\value{
TRUE if all elements in x are in y, FALSE otherwise
}
\description{
Checks whether all elements of one variable are in another
}
\examples{
is_in("a", letters)
is_in(c("a", "b", "c"), letters)

is_in(1, LETTERS)
is_in(1:2, LETTERS)

}
