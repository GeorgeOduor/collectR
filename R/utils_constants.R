#' constants
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#' @import pool
#' @import RPostgres
#' @noRd
db_con  <-
    DBI::dbConnect(
        drv = RPostgres::Postgres(),
        dbname = 'debtcollection',
        host = 'localhost',
        user = 'George',
        password = '5'
    )
