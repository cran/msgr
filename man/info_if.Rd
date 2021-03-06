% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/messages.R
\name{info_if}
\alias{info_if}
\title{Display a message, and record in a log file, if a condition is true.}
\usage{
info_if(
  condition,
  ...,
  level = 1,
  msg_level = getOption("msgr.level"),
  msg_types = getOption("msgr.types"),
  log_path = getOption("msgr.log_path")
)
}
\arguments{
\item{condition}{(boolean) The condition to check.}

\item{...}{(strings) message to be displayed or written to file.}

\item{level}{(integer, optional) The level of the message, from 1 to 10. Default: 1.}

\item{msg_level}{(integer, optional) The maximum level of messages to output. Default: set
in the option \code{"msgr.level"}.}

\item{msg_types}{(character, optional) The type to write or display. Must either NULL or one
or more from "INFO", "WARNING" or "ERROR". Default: set in the option \code{"msgr.types"}.}

\item{log_path}{(string, optional) The file path to the text log file. If set to "", then no
logs are written. Default: set in the option \code{"msgr.log_path"}.}
}
\value{
A string is return invisibly containing the message.
}
\description{
This function calls the \code{\link[=info]{info()}} function to display a message if the specified condition
is true. If a message is not specified then a generic message is displayed.
}
\examples{
# Use info_if() to create conditional timed messages
info_if(2 > 1, "Condition is true so this message is shown")
info_if(1 > 2, "Condition is false so this message is not shown")

# As with info() a level can be set
info_if(2 > 1, "This is a level 2 message, so not shown by default", level = 2)

# Set default level in options to determine what is shown
options(msgr.level = 2)
info_if(2 > 1, "This is a level 2 message, so is shown now", level = 2)

# Set message types in options to determine what is shown
options(msgr.types = c("WARNING", "ERROR"))
info_if(2 > 1, "This is message, so will not be shown now")

}
