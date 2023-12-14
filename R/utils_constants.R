#' constants
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#' @import pool
#' @import RPostgres
#' @noRd
db_con  <- DBI::dbConnect(drv = RPostgres::Postgres(),
                   dbname = 'debtcollection',
                   host = 'localhost',
                   user = 'George',
                   password = '5')


max_ <- list(name = "Max",type = "max")

min_ <- list(name = "Min",type = "min")

avg_ <- list(type = "average",name = "AVG")

# testing constants
debt_collector <- "Eva"
year_val = 2023
month_val = 5
channel_selected <- 'All'
# reportDate = rollback(rollback(Sys.Date()))
