#' Creates a reactive trigger
#'
#' @description This function creates a reactive trigger using a reactiveValues object. The trigger can be used to notify observers that a reactive expression should be re-executed.
#'
#' @return A list with two functions: depend() and trigger()
#' @examples
#' dbtrigger <- makereactivetrigger()
#' observeEvent(dbtrigger$depend(), {
#'     # code to re-execute when trigger is called
#' })
makereactivetrigger <- function() {
    rv <- reactiveValues(a = 0)
    list(
        depend = function() {
            rv$a
            invisible()
        },
        trigger = function() {
            rv$a <- isolate(rv$a + 1)
        }
    )
}

#' Generate an SQL insert statement for a table


#' Generate a DELETE SQL clause
#'
#' @description This function generates a SQL DELETE clause for a given table and WHERE condition, which deletes rows from the table that match the condition.
#'
#' @param con The database connection object.
#' @param table The name of the table to delete rows from.
#' @param where The column name used to specify the WHERE condition.
#' @param is The value to match in the WHERE condition.
#' @param exec Logical value indicating whether to execute the generated SQL or return it as a string.
#'
#' @return If \code{exec = TRUE}, this function returns the result of the executed SQL DELETE statement. If \code{exec = FALSE}, this function returns a character string representing the SQL DELETE statement.
#'

delete_clause <- function(con, table, where, is, all=FALSE,exec = TRUE) {

    # Quote the table name and value
    table_quoted <- paste0('"', table, '"')
    is_quoted <- if (is.character(is)) glue::glue("'{is}'") else is
    where_quoted <- if (is.character(where)) glue::glue('"{where}"') else where

    # Generate the SQL delete statement
    where_clause <- sprintf("%s = %s", where_quoted, is_quoted)
    sql <- sprintf("DELETE FROM %s WHERE %s", table_quoted, where_clause)
    sql_all <- sprintf("DELETE FROM %s", table_quoted)

    # Execute or return the query string
    if (exec) {
        if (all) {
            DBI::dbExecute(con, sql_all)
        }else{
            DBI::dbExecute(con, sql)
        }
    } else {
        if (all) {
            print(sql_all)
        }else{
            print(sql)
        }
    }

}
# delete_clause(db_con,table = "t_Outstanding",where = "id",
#               is = 185137,exec = F)
# delete_clause(db_con,'t_Outstanding',where = 'allocation_id',
#               is = 185137,all = F,exec = F)
#' Generate SQL insert statement
#'
#' @description Generates an SQL insert statement for a given table and values.
#'
#' @param table The name of the table to insert the values into. Must be quoted if the table name contains uppercase letters, spaces or special characters.
#' @param values A list of values to be inserted into the table.
#' @param type An integer indicating the type of insert statement to generate. Type 1 is a standard insert statement where all values are inserted into the table without specifying columns. Type 2 is an insert statement that specifies the columns to insert the values into.
#' @param cols A vector of column names to be specified in the insert statement. Only used if type = 2.
#' @param exec A logical value indicating whether or not the insert statement should be executed immediately. If set to TRUE, the insert statement will be executed using the RPostgreSQL::dbExecute function.
#' @return A string containing the SQL insert statement.
#' @import DBI
insert_clause <- function(con,table, values=list(...), type=1, cols=NULL, exec=T) {
    con <-
        DBI::dbConnect(
            drv = RPostgres::Postgres(),
            dbname = 'debtcollection',
            host = 'localhost',
            user = get_golem_config("username","database"),
            password = get_golem_config("pswd","database")
        )
    on.exit(dbDisconnect(con))
    if (type == 1) {
        x <- sprintf("INSERT INTO %s VALUES ('%s')", table, paste0(values, collapse = "','"))
    } else {
        x <- sprintf("INSERT INTO %s (%s) VALUES ('%s')", table, paste0(cols, collapse = ","), paste0(values, collapse = "','"))
    }

    if (exec) {
        x <- gsub(sprintf("INSERT INTO %s", table), sprintf("INSERT INTO \"%s\"", table), x)
        dbSendStatement(con, x)
        # close the database connection
    }else{
       print(x)
    }
}

#' Update rows in a database table
#'
#' @description This function generates an SQL update statement to modify rows in a
#' database table.
#'
#' @param con A database connection object created using the \code{DBI} package.
#' @param table The name of the table to update.
#' @param what A vector of column names to update.
#' @param by A vector of values to set for each column in \code{what}.
#' @param where The column name for the condition.
#' @param is The value of the condition.
#'
#' @return NULL

update_clause <- function(con, table, what, by, where, is, exec = TRUE) {
    # Check that the input values are of the correct type and length
    stopifnot(length(what) == length(by))

    # Quote the column names and values
    table_quoted = paste0('"', table, '"')
    what_quoted = paste0('"', what, '"')
    by_quoted = lapply(by, function(x) { if (is.character(x)) glue::glue("'{x}'") else x })
    where_quoted = if (is.character(is)) glue::glue("'{is}'") else is

    # Generate the SQL update statement
    set_clause = paste0(what_quoted, " = ", by_quoted, collapse = ", ")
    where_clause = sprintf("%s = %s", where, where_quoted)
    sql = sprintf("UPDATE \"%s\" SET %s WHERE %s", table, set_clause, where_clause)

    # Execute or return the query string
    if (exec) {
        DBI::dbExecute(con, sql)
    } else {
        print(sql)
    }

}
