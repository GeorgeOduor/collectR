#' Edit UI Function
#'
#' This function generates a modal dialog with input fields to edit the data of a given \code{what} (permissions or users) and returns it.
#'
#' @param what A character string indicating the type of data to be edited (permissions or users).
#' @param ns A character string with the namespace for Shiny input and output objects.
#' @param data A data frame with the data to be edited.
#'
#' @return A modal dialog with input fields to edit the data of a given \code{what} (permissions or users).
edit_permission_ui <- function(what, ns, data,title =  "Edit Permission Details") {
    showModal(switch (what,
            "new_group" = {
                modalDialog(
                    title = title,
                    easyClose = TRUE,
                    size = "s",
                    tagList(
                        textInput(ns("permission_name"),"Permission Name",placeholder = "Enter New Permission here",value = data$permission_name),
                        selectInput(ns("permission_type"),"Permission Type",choices = c("Application Access", "User Action"),selected = data$permission_type),
                        actionBttn(ns("save_permission"),label = "Save",icon = icon("save"),color = "success",style = "material-flat",size = "xs",name = "save_permissions",block = TRUE)
                    ),
                    footer = modal_footer()
                )
            },
            "edit_permission" = {
                modalDialog(
                    title = title,
                    easyClose = TRUE,
                    size = "s",
                    tagList(
                        textInput(ns("permission_name"),"Permission Name",placeholder = "Enter New Permission here",value = data$permission_name),
                        selectInput(ns("permission_type"),"Permission Type",choices = c("Application Access", "User Action"),selected = data$permission_type),
                        actionBttn(ns("save_permission"),label = "Save",icon = icon("save"),color = "success",style = "material-flat",size = "xs",name = "save_permissions",block = TRUE)
                    ),
                    footer = modal_footer()
                )
            },
            "new_permissions" = {
                modalDialog(
                    title = title,
                    easyClose = TRUE,
                    size = "s",
                    tagList(
                        textInput(ns("permission_name"),"Permission Name",placeholder = "Enter New Permission here"),
                        selectInput(ns("permission_type"),"Permission Type",choices = c("Application Access", "User Action")),
                        actionBttn(ns("save_new_permission"),label = "Save",icon = icon("save"),color = "success",style = "material-flat",size = "xs",name = "save_permissions",block = TRUE)
                    ),
                    footer = modal_footer()
                )
            }
    ))
}

#' Show modal for editing group settings
#'
#' @description This function displays a modal dialog for editing the settings of a group.
#'
#' @param fun The function to be executed when the Save button is clicked.
#' @param group_auth A character vector of group-level authorization settings.
#' @param user_auth A character vector of user-level authorization settings.
#'
#' @return A modal dialog containing input fields for editing group settings.
#'
group_settings <- function(ns,fun,group_auth,user_auth,title,value=NULL,selected_perms=NULL) {
    showModal(ui = modalDialog(easyClose = T,
                               tagList(
                                   col_12(
                                       if (fun %in% c('edit_group_permission','save_new_group')) {
                                           tagList(
                                               textInput(ns("new_group"),"Group Name",value = value),
                                               h4("Permissions")
                                           )
                                       }
                                   ),
                                   col_12(bucket_list(
                                       header = "",#class = "ranklists",
                                       # add_rank_list(
                                       #     text = "Modules",
                                       #     labels = group_auth,
                                       #     input_id = ns('permissions_modules')
                                       # ),
                                       if (fun %in% c('edit_group_permission','save_new_group')) {
                                           add_rank_list(
                                               text = "Modules",
                                               labels = group_auth,
                                               input_id = ns('permissions_modules')
                                           )
                                       }else{
                                           add_rank_list(
                                               text = "User Permissions",
                                               labels = user_auth,
                                               input_id = ns('user_permissions')
                                           )
                                       },
                                       add_rank_list(
                                           text = "Selected Permissions",
                                           labels = selected_perms,
                                           input_id = ns('permissions_selected')
                                       )
                                   ),
                                   col_12(class="submit",
                                          actionBttn(ns(fun),"Save",icon = icon("save"),size = "xs",block = T,style = "material-flat",color = 'success')
                                   )
                                   )),
                               title = title,
                               fluidRow(class="group-info-edit",),size = "m",
                               footer = modal_footer()
    ))
}


#' Get new group permissions
#'
#' @description This function returns a data frame with the new permissions for a given group.
#'
#' @import dplyr
#' @import tidyr
#' @import dbplyr
#'
#' @param selected_permissions A character vector with the selected permissions.
#' @param group_name A character vector with the name of the group.
#'
#' @return A data frame with the new permissions for the given group.
#'
get_new_group_permissions <- function(selected_permissions,group_name) {

    selected_permissions <- selected_permissions %>%
        enframe() %>%
        mutate(group = group_name)

    new_permissions <- tbl(db_con,'t_Permissions') %>%
        select(permission_id = id,permission_name) %>%
        collect() %>%
        distinct(.keep_all = T) %>%
        inner_join(selected_permissions,by=c("permission_name"="value")) %>%
        select(-name) %>% #print()
        inner_join(collect(tbl(db_con,"t_UserGroups") %>%
                               filter(name == !!group_name)),
                   by=c("group"="name")) %>%
        # select(-group,-createdon) %>%
        select(group_id = id,permission_id)

    return(new_permissions)
}

get_new_user_permissions <- function(permissions_selected,new_user) {
    tryCatch(
    expr = {
        user = tbl(db_con,"t_Users") %>%
            filter(email == !!new_user) %>%
            pull(id)
        if (!is.null(permissions_selected) && length(user) == 1) {

            df <- permissions_selected %>%
                enframe() %>%
                inner_join(tbl(db_con,'t_Permissions') %>%
                               collect(),
                           by=c('value'='permission_name')) %>%
                select(permission_id=id) %>%
                mutate(user_id = user)
            # success_ui


        }
    },error = function(e){print(e)},
    warning = function(w){print(w)},
    finally = {}
    )
}

#' Edit group permissions
#'
#' This function edits the permissions of a given group based on the selected row and the list of permissions selected.
#'
#' @import DBI
#'
#' @param selected_row A character vector with the selected row to edit.
#' @param permissions_selected A character vector with the selected permissions.
#' @param group_name A character vector with the name of the group to edit.
#'
#' @return Nothing is returned. The function modifies the database table 't_GroupPermissions'.
#'
edit_group_permission <- function(groups,selected_row,permissions_selected,group_name) {

    selected_row <- as.numeric(strsplit(selected_row, "_")[[1]][3])
    group_id <- groups %>% filter(rowid == selected_row) %>% pull(id)
    new_permissions <- get_new_group_permissions(selected_permissions = permissions_selected,
                                                 group_name = group_name)
    #
    if (nrow(new_permissions) > 0) {
        # first remove existing details
        delete_clause(
            con = db_con,
            table = "t_GroupPermissions",
            where = "group_id",
            is = group_id,
            exec = T
        )
        custom_db_actions(
            action = dbWriteTable(
                db_con,
                name = "t_GroupPermissions",
                value = new_permissions,
                append = T
            ),
            successmessage = "Group Data Updated Successfully"
        )
    }
}

# tbl(db_con,"t_Users") %>% select(id,usergroup) %>% inner_join(tbl(db_con,"t_GroupPermissions"),by=c('usergroup' = 'group_id'))
#' Create a user permissions form for a web application.
#'
#' @description This function creates an interactive user permissions form for a web application using the Shiny package in R. The form allows the user to select their permissions for each module or group and displays the selected permissions in a separate input. The form includes a "Save" button that calls a specified function when clicked.
#'
#' @param ns A string used to create unique names for the form inputs to avoid naming conflicts.
#' @param group_auth A vector of strings representing the modules or groups that the user can access.
#' @param user_auth A vector of strings representing the permissions that the user has for each module or group.
#' @param selected_perms A vector of strings representing the permissions that have been selected by the user.
#' @param fun A string representing the name of the function that will be called when the user clicks the "Save" button.
#'
#' @return A shiny UI object that can be used to render the user permissions form.
#' @import shiny
user_permissions <- function(ns,group_auth,user_auth,selected_perms=NULL,fun="edit_user_details") {
    tagList(
        bucket_list(
            header = "",
            #class = "ranklists",
            add_rank_list(
                text = "User Permissions",
                labels = user_auth,
                input_id = ns('user_permissions')
            ),
            add_rank_list(
                text = "Selected Permissions",
                labels = selected_perms,
                input_id = ns('permissions_selected')
            )
        ),
        col_12(
            class = "submit",
            actionBttn(
                ns(fun),
                "Save",
                icon = icon("save"),
                size = "xs",
                block = T,
                style = "material-flat",
                color = 'success'
            )
        )

    )
}

#' Display a modal dialog for managing user details
#'
#' @description This function displays a modal dialog for adding or editing user details,
#' permissions, and password management settings.
#'
#' @param ns The namespace of the Shiny app.
#' @param type The type of user details to display ('new' or 'edit').
#' @param ... Additional arguments to pass to the function.
#' @return A Shiny modal dialog.
#'
user_details_man <- function(ns,type="new",agent="internal",...) {
    kwargs <- list(...)
    new_ui <- tabsetPanel(
        type = 'pills',
        tabPanel(title = "User details",
                 div(
                     class = 'tabcontent',
                     col_6(textInput(
                         ns("first_name"),
                         "First Name",
                         placeholder = "John",
                         width = "100%",value = kwargs$first_name
                     )),
                     col_6(textInput(
                         ns("last_name"),
                         "Last Name",
                         placeholder = "Doe",
                         width = "100%",value = kwargs$last_name
                     )),
                     col_12(
                         textInput(
                             ns("mobileno"),
                             "Mobile Number",
                             placeholder = "2547XXXXXXXX",
                             width = "100%",value = kwargs$mobile
                         ),
                         textInput(
                             ns("email"),
                             "Email",
                             placeholder = "john.doe@example.com",
                             width = "100%",value = kwargs$email
                         ),
                         selectInput(
                             ns("user_category"),
                             "User Auth Category",
                             width = "100%",
                             choices = kwargs$choices,
                             selected = kwargs$user_category
                         )
                     )
                 )),
        tabPanel(title = "Permissions",
                 tagList(# uiOutput(ns("permissions_list_ui"))
                     user_permissions(ns,
                                      group_auth =  NULL,
                                      fun=kwargs$fun,
                                      selected_perms = kwargs$selected_perms,
                                      user_auth = kwargs$user_auth)
                 )
        ),
        if (type == "edit") {
            tabPanel(title = "Password Management",
                     radioGroupButtons(
                         inputId = ns("paswd_manager"),
                         label = "",
                         choices = c("Reset Password","Force password Change"),
                         status = "success",size = "xs"
                     ),
                     uiOutput(ns('password_manager_ui'))
            )
        }
    )
    external_dca <- tagList(
        textInput(ns("agent_name"),label = "DCA Name",value = kwargs$name,width = '100%'),
        fluidRow(
            class = "dca_contacts",
            col_10(class = "dca_contacts_list",
                   div(class = 'title-row', h4("Contacts")),
                   dca_contact_ui(ns)
                   ),
            col_2(class="add_remove",
                  actionBttn(ns("add_another"),"Add",icon = icon('plus'),style = 'material-flat',size = "xs",color = "success",block = T),
                  actionBttn(ns("save_dca"),"Save",icon = icon('plus'),style = 'material-flat',size = "xs",color = "success",block = T)
                  )
            )

    )
    modalDialog(
        if (type == "new") {
            tagList(
                tabsetPanel(type = "pills",
                    tabPanel(title = "Internal Debt Collectors",icon = icon('users'),
                             div(class = "tab_content",new_ui)
                             ),
                    tabPanel(title = "External Debt Collectors",icon = icon('briefcase'),
                             div(class = "tab_content",external_dca)
                             )
                )
            )
        }else{
            new_ui
        },
        title = ifelse(type=='new',"Add new User Details","Edit User Details"),
        footer = modal_footer(),
        size = "l",
        easyClose = T
    )
}


dca_contact_ui <- function(ns,id=0) {
    tagList(
        col_12(class=paste0("item-list text-focus-in list-item-",id),
            col_10(
                col_3(textInput(ns(paste0('dca_contact_name_',id)),"Name",width = '100%')),
                  col_3(textInput(ns(paste0('dca_mobilenumber_',id)),"Mobile Number",width = '100%')),
                  col_4(textInput(ns(paste0('dca_emailaddress_',id)),"Email Address",width = '100%')),
                  col_2(textInput(ns(paste0('dca_ismaincontact_',id)),"Main Contact?",width = '100%',placeholder  = "Yes/No"))
                ),
            col_2(class = "btns",
                  HTML(shinyInput2(
                      actionButton,
                      id = paste0("remove-", id),
                      label = "",
                      class = "removebutton",
                      onclick = 'Shiny.setInputValue(\"settings_1-remove\", this.id, {priority: \"event\"})',
                      icon = icon("minus")
                  ))
                  # actionBttn(ns(paste0("remove-",id)),"",icon = icon('minus'),style = 'material-circle',size = "xs",color = "danger",block = F)
            )
        )
    )
}

agent_contact_change_ui <- function(ns,contact_info,type="edit",agent_id=NULL) {
    modalDialog(title = ifelse(type == "edit",paste(contact_info[['name']],"Contact Information"),"New Contact Information"),
                footer = modal_footer(),size = "m",easyClose = T,
                fluidRow(class = 'agent_edit_container',
                         col_9(
                             if (type == "edit") {
                                 col_6(textInput(ns('contact_id'),"Agent ID",value = contact_info[['id']],width = "100%"))
                             }else{
                                 col_6(textInput(ns('agent_id'),"Agent ID",value = agent_id,width = "100%"))
                             },
                             col_6(textInput(ns('agent_name'),"Agent Name",value = contact_info[['name']],width = "100%")),
                             col_6(textInput(ns('mobilenumber'),"Agent Number",value = contact_info[['mobilenumber']],width = "100%")),
                             col_6(textInput(ns('emailaddress'),"Agent Email Address",value = contact_info[['emailaddress']],width = "100%")),
                             col_6(textInput(ns('ismain_contact'),"Is main Contact",value = contact_info[['ismain_contact']],width = "100%"))),
                         col_3(
                             actionBttn(ns(paste0("save_contact_",type)),"Submit",icon = icon("save"),style = "material-flat",color = 'success',size = 'xs',block = T)
                         )
                )
    )
}

#' Collect formatted contact information from a list
#'
#' This function takes a list of contact information and the number of contacts in the list,
#' and returns a data frame containing the formatted contact information for all contacts in the list.
#'
#' @param input A list containing contact information.
#' @param id An integer specifying the number of contacts in the list.
#'
#' @return A data frame containing the formatted contact information for all contacts in the list.
#' @importFrom reshape2 dcast
#' @examples
collect_contacts <- function(input,id) {
    out <- seq(0,id) %>%
        map_df(.f = function(x){
            contact_name <- input[[paste0("dca_contact_name_", x)]]
            mobile_number <- input[[paste0("dca_mobilenumber_", x)]]
            email_address <- input[[paste0("dca_emailaddress_", x)]]
            ismain_contact <- input[[paste0("dca_ismaincontact_", x)]]
            out <- list(
                'name' = contact_name,
                'mobilenumber' = mobile_number,
                'emailaddress' = email_address,
                'ismain_contact' = ismain_contact
                ) %>%
                lapply(FUN = function(x){
                ifelse(is.null(x),NA,x)
                })%>%
                enframe() %>%
                dcast(.~name,value.var = 'value') %>%
                mutate_all(unlist)%>%
                mutate(
                    created_on = as.character(Sys.Date()),
                    ismain_contact = ifelse(tolower(ismain_contact) == "yes",T,F)) %>%
                select(-".") %>%
                filter(!is.na(contact_name),!is.na(emailaddress),emailaddress != "")
        })%>%
        distinct(emailaddress,.keep_all=T)
    return(out)
}
#' Encrypt or decrypt a password
#'
#' @description This function encrypts or decrypts a given password using the \code{safer} package
#' and a key obtained from the \code{get_golem_config} function.
#'
#' @param passwd A character string containing the password to be encrypted or decrypted.
#' @param enc A logical value indicating whether to encrypt or decrypt the password.
#'   The default value is \code{TRUE}.
#' @return A character string containing the encrypted or decrypted password.
#'
enc_dec <- function(passwd, enc = TRUE) {

    # Validate input
    if (!is.character(passwd)) {
        stop("passwd argument must be a character string.")
    }
    if (!is.logical(enc)) {
        stop("enc argument must be a logical value.")
    }

    # Get encryption key
    key <- get_golem_config("key", "hashkey")

    # Encrypt or decrypt password
    if (enc) {
        encrypted_passwd <- safer::encrypt_string(passwd, key)
        return(encrypted_passwd)
    } else {
        decrypted_passwd <- safer::decrypt_string(passwd, key)
        return(decrypted_passwd)
    }
}

password_manager <- function(action,...) {
    kwargs <- list(...)
    switch (action,
        "new_pass" = {
            enc_dec(kwargs$random_pass,enc = T)
        },
        "new_pass_message" = {
            showModal(ui = modalDialog(
                fluidRow(class="body",
                    col_12(p(class="set-pwd","Temporary Password",
                             span(class="pull-right paswd",kwargs$initial_password)))
                ),
                title = "Success!",
                footer = modal_footer(),size = "s",
                easyClose = T
            ))
        }
    )
}

#' Generate a random password
#'
#' @description This function generates a random password of a specified length. The password contains
#' alphanumeric characters and some special characters.
#'
#' @param length An integer specifying the length of the password. The default value is 8.
#' @return A character string containing the random password.
#' @examples
#' passwd_generator(length = 12)
#' passwd_generator()

passwd_generator <- function(length = 8) {
    random_pass <-
        paste0(sample(c(0:9, letters, LETTERS, unlist(
            strsplit('!@#$%^&*_+', split = "")
        )), length,replace = T), collapse = "")
    return(random_pass)
}

