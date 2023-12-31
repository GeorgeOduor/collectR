% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_db_actions.R
\name{makereactivetrigger}
\alias{makereactivetrigger}
\title{Creates a reactive trigger}
\usage{
makereactivetrigger()
}
\value{
A list with two functions: depend() and trigger()
}
\description{
This function creates a reactive trigger using a reactiveValues object. The trigger can be used to notify observers that a reactive expression should be re-executed.
}
\examples{
dbtrigger <- makereactivetrigger()
observeEvent(dbtrigger$depend(), {
    # code to re-execute when trigger is called
})
}
