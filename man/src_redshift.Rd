% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/src_redshift.R
\name{src_redshift}
\alias{src_redshift}
\title{Connect to Amazon Redshift.}
\usage{
src_redshift(dbname = NULL, host = NULL, port = NULL, user = NULL,
  password = NULL, ...)
}
\arguments{
\item{dbname}{Database name}

\item{host, port}{Host name and port number of database}

\item{user, password}{User name and password (if needed)}

\item{...}{Other arguments passed on to the underlying
database connector, \code{dbConnect}.}
}
\description{
Use \code{src_redshift} to connect to an existing Redshift database.
}
\details{
This function works the same as other source functions found in dplyr. For
more information see \code{\link{src_postgres}} or the dplyr database vignette
\url{https://cran.r-project.org/web/packages/dplyr/vignettes/databases.html}
}
\examples{
\dontrun{
rs <- src_redshift(dbname = "n", host = "h", post = "5439", user = "u", password = "p")
dat <- tbl(rs, 'tablename')
}
}

