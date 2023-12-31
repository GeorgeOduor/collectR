% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_clientprofile.R
\name{debtor_info}
\alias{debtor_info}
\title{Get KYC information for a debtor}
\usage{
debtor_info(outstandingfile, accid, loanbk, feedback, type)
}
\arguments{
\item{accid}{The AccountID of the debtor.}

\item{db_con}{The database connection object.}
}
\value{
A tibble containing the KYC information for the debtor, including columns: \code{Client_Name}, \code{AccountID}, \code{LoanSeries}, and \code{Mobile_No}.
}
\description{
This function retrieves KYC (Know Your Customer) information for a debtor based on the provided \code{AccountID} and optional \code{LoanSeries}.
}
