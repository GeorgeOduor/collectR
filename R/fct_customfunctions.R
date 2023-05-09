#' customfunctions
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#' @importFrom  glue glue_sql
#' @importFrom dplyr tbl filter collect show_query
#' @noRd
check_creds <- function() {
    con <-
        DBI::dbConnect(
            drv = RPostgres::Postgres(),
            dbname = 'debtcollection',
            host = 'localhost',
            user = get_golem_config("username","database"),
            password = get_golem_config("pswd","database")
        )
    # on.exit(dbDisconnect(con))
    # finally one function of user and password
    function(user, password) {

        # on.exit(poolClose(con))
        paswd <- enc_dec(passwd = password,enc = T)
        if (user == "george.oduor@maishabank.com") {
            paswd = password
        }
        req <- glue_sql("SELECT * FROM \"t_Users\" WHERE \"email\" = ({user}) AND \"password\" = ({password})",
                        user = user,
                        password = paswd,
                        .con = con
        )
        req <- dbSendQuery(con, req)
        res <- dbFetch(req) %>% invisible()

        user_permissions <- tbl(db_con,"t_Userpermissions") %>% filter(user_id == !!res$id) %>%
            inner_join(tbl(db_con,"t_Permissions") %>% select(id,permission_name,permission_type),by=c('permission_id'='id')) %>%
            select(permission_id,permission_name,permission_type) %>%
            collect()
        grp_permissions <- tbl(db_con,"t_GroupPermissions") %>% filter(group_id == !!res$usergroup) %>%
            inner_join(tbl(db_con,"t_Permissions") %>% select(id,permission_name,permission_type),by=c('permission_id'='id')) %>%
            select(-createdon,-group_id,-id) %>%
            collect()
        permissions <-  bind_rows(user_permissions,grp_permissions) %>%
            distinct(permission_id,.keep_all = T) %>%
            as.list()

        if (nrow(res %>% collect()) > 0) {
            # merge with the groups to return the permissions
            list(result = TRUE, user_info = append(res %>% as.list(),list(permissions=permissions)) )
        } else {
            list(result = FALSE)
        }
    }
}

#' Programmatically create a Shiny input
#'
#' @param FUN function to create the input
#' @param n number of inputs to be created
#' @param id ID prefix for each input
#' @noRd

shinyInput <- function(FUN, n ,id, ...) {

    # for each of n, create a new input using the FUN function and convert
    # to a character
    vapply(seq_len(n), function(i){
        as.character(FUN(paste0(id, i), ...))
    }, character(1))

}
shinyInput2 <- function(FUN, id, ...) {
    as.character(FUN(id, ...))
}



#' Execute an action in the database and show a toast message
#'
#' @description This function executes a specified action in the database and shows a toast message
#' indicating whether the action was successful or not.
#'
#' @param action A database action to be executed.
#' @param successmessage A message to display if the action is successful.
#' @import shinytoastr
#'
#' @return NULL
#'
custom_db_actions <- function(action, successmessage,toast=T,...) {
    tryCatch(
        {
            kwargs <- list(...)
            # Execute the specified action in the database
            action

            # close modal
            removeModal()

            if (toast) {
                toastr_success(successmessage)
            }else{
                successmessage
            }

            },
        error = function(e) {
            # Show an error toast message
            show_toast(title = "Error", text = paste0(e),
                       type = "error", position = "bottom-end",
                       timerProgressBar = TRUE, timer = 5000)

        }
    )
}



#' Execute an action in the database and show a toast message
#'
#' @description This function executes a specified action in the database and shows a toast message
#' indicating whether the action was successful or not.
#'
#' @param text The dismisbutton text
#' @param delete_id The unique delete id
#' @param successmessage A message to display if the action is successful.
#' @import shiny
#' @import shinyWidgets
#'
#' @return NULL
delete_confirm  <- function(text,delete_id,ns) {
    showModal(ui = modalDialog(title = "",size = "s",footer = modal_footer(),easyClose = T,
                               tagList(
                                   span(class="delete-icon-space",icon('info-circle',class = "delete-icon")),
                                   p(class = "deletemessage",
                                     "This action will remove this record!"),
                                   # span(class='selecteditem',selectInput(ns(''))),
                                   fluidRow(
                                       col_6(class="option_buttons",
                                             actionBttn(ns(delete_id),"Yes",icon = icon("thumbs-up"),size = "xs",no_outline = F,style = "material-flat",color = 'danger')
                                       ),
                                       col_6(class="option_buttons",
                                             modal_exit(text = text),

                                       )
                                   )
                               )
    ))
}



