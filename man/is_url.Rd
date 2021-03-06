% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predicates.R
\name{is_url}
\alias{is_url}
\title{Checks whether the variable is a valid URL}
\usage{
is_url(x)
}
\arguments{
\item{x}{(any) The object to test}
}
\value{
TRUE if x is a valid URL, FALSE otherwise
}
\description{
Checks whether the variable is a valid URL
}
\examples{
is_url("http://something.com")
is_url("https://google.com")

is_url(1)
is_url("foo")
is_url(NA)

}
