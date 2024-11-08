% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/oracle_connection.R
\name{dbGetQueryOracle}
\alias{dbGetQueryOracle}
\alias{dbSendQueryOracle}
\title{Runs a query in an Oracle database and reports Oracle errors with line and column number for debugging when those occur.
Returns a data.frame with the data, or a list containing data on the error arising.}
\usage{
dbGetQueryOracle(
  connection,
  querystring,
  ...,
  returndata = TRUE,
  timeit = FALSE,
  rowlimit = NA
)

dbSendQueryOracle(connection, querystring, ...)
}
\arguments{
\item{connection}{JDBCConnection object created separately.}

\item{querystring}{Query or statement to execute, not terminated by a semicolon unless a PL/SQL statement.}

\item{...}{Arguments to fill in holes in querystring if querystring is a prepared statement.}

\item{returndata}{Default TRUE for dbGetQueryOracle, should be FALSE for statements with no returned data.}

\item{timeit}{Default FALSE, turn to TRUE to enable profiling.}

\item{rowlimit}{Default NA, number of rows to return}
}
\value{
None
}
\description{
Runs a query in an Oracle database and reports Oracle errors with line and column number for debugging when those occur.
Returns a data.frame with the data, or a list containing data on the error arising.
}
\examples{
\dontrun{
dbGetQueryOracle(con, query)
dbSendQueryOracle(con, statement)
dbGetQueryOracle(con, statement, returndata = FALSE)
dbGetQueryOracle(con, "select ? from dual union select ? from dual", "cow", "hat", timeit = TRUE)
}


}
