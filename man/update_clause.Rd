% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_db_actions.R
\name{update_clause}
\alias{update_clause}
\title{Update rows in a database table}
\usage{
update_clause(con, table, what, by, where, is, exec = TRUE)
}
\arguments{
\item{con}{A database connection object created using the \code{DBI} package.}

\item{table}{The name of the table to update.}

\item{what}{A vector of column names to update.}

\item{by}{A vector of values to set for each column in \code{what}.}

\item{where}{The column name for the condition.}

\item{is}{The value of the condition.}
}
\description{
This function generates an SQL update statement to modify rows in a
database table.
}
