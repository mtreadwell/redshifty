#' @import dplyr
#' @import RRedshiftSQL
#' @import DBI
NULL

# defined to avoid importing infix from another package
`%||%` <- function(x, y) if (is.null(x)) y else x

#' Connect to Amazon Redshift.
#'
#' Use \code{src_redshift} to connect to an existing Redshift database.
#'
#' This function works the same as other source functions found in dplyr. For
#' more information see \code{\link{src_postgres}} or the dplyr database vignette
#' \url{https://cran.r-project.org/web/packages/dplyr/vignettes/databases.html}
#'
#' @param dbname Database name
#' @param host,port Host name and port number of database
#' @param user,password User name and password (if needed)
#' @param ... Other arguments passed on to the underlying
#'   database connector, \code{dbConnect}.
#' @export
#' @examples
#' \dontrun{
#' rs <- src_redshift(dbname = "n", host = "h", post = "5439", user = "u", password = "p")
#' dat <- tbl(rs, 'tablename')
#' }
src_redshift <- function(dbname = NULL, host = NULL, port = NULL, user = NULL,
                         password = NULL, ...) {
  if (!requireNamespace("RRedshiftSQL", quietly = TRUE)) {
    stop("RRedshiftSQL package required to connect to redshift db", call. = FALSE)
  }

  con <- DBI::dbConnect(RRedshiftSQL::RedshiftSQL(), host = host %||% "", dbname = dbname %||% "",
                        user = user, password = password %||% "", port = port %||% "", ...)

  info <- DBI::dbGetInfo(con)

  src_sql("redshift", con, info = info)
}

#' @method src_desc src_redshift
#' @export
src_desc.src_redshift <- function(x) {
  info <- x$info
  host <- if (info$host == "") "localhost" else info$host

  paste0("redshift ", info$serverVersion, " [", info$user, "@",
         host, ":", info$port, "/", info$dbname, "]")
}

#' @method tbl src_redshift
#' @export
tbl.src_redshift <- function(src, from, ...) {
  tbl_sql("redshift", src = src, from = from, ...)
}

#' @method sql_translate_env RedshiftSQLConnection
#' @export
sql_translate_env.RedshiftSQLConnection <- function(con) {
  sql_variant(
    base_scalar,
    sql_translator(.parent = base_agg,
                   n = function() sql("count(*)"),
                   cor = sql_prefix("corr"),
                   cov = sql_prefix("covar_samp"),
                   sd =  sql_prefix("stddev_samp"),
                   var = sql_prefix("var_samp"),
                   all = sql_prefix("bool_and"),
                   any = sql_prefix("bool_or"),
                   paste = function(x) build_sql(x, collapse = ' || ')
    ),
    sql_translator(.parent = base_win,
                   lag = function(x, n = 1L, order = NULL) {
                     win_over(
                       build_sql("LAG", list(x, n)),
                       win_current_order(),
                       order %||% win_current_order()
                     )
                   },
                   lead = function(x, n = 1L, order = NULL) {
                     win_over(
                       build_sql("LEAD", list(x, n)),
                       win_current_order(),
                       order %||% win_current_order()
                     )
                   },
                   median = function(x) {
                     win_over(
                       build_sql("MEDIAN", list(x)),
                       win_current_group()
                     )
                   }
    )
  )
}

#' @method db_explain RedshiftSQLConnection
#' @export
db_explain.RedshiftSQLConnection <- function(con, sql, ...) {
  exsql <- dplyr::build_sql("EXPLAIN ", sql)
  expl <- DBI::dbGetQuery(con, exsql)

  paste(expl[[1]], collapse = "\n")
}
