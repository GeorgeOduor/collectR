#' settings UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import shiny
#' @importFrom shinydashboardPlus box accordion accordionItem
#' @importFrom shinyWidgets pickerInput actionBttn
#' @import DT
#' @import sortable
#' @import shinyjqui
#'
mod_settings_ui <- function(id){
  ns <- NS(id)
  tagList(

    box(title = 'Application and Settings',status = "success",solidHeader = T,
        height = "400px",width=12,class="application-settings",
        col_2(
          class="settings_panel",
          radioGroupButtons(
            inputId = ns("modules"),
            label = "",
            choices = c("Permisions","Groups", "Users", "Collection Utils","CBS Authentication"),width = "100%",
            status = "success",direction = "vertical",size = "xs",justified = T,individual = T
          )
        ),
        col_10(
          uiOutput(ns('modles_ui'))
        )
    )

  )
}

#' settings Server Functions
#' @importFrom dplyr `%>%` tbl distinct collect bind_cols row_number arrange select pull inner_join
#' @importFrom tibble as_tibble tibble enframe rowid_to_column
#' @importFrom shinyWidgets show_toast ask_confirmation
#' @importFrom shinydashboardPlus accordion accordionItem
#' @importFrom purrr map_df map
#'
#' @noRd
mod_settings_server <- function(id,authentication){
  moduleServer( id, function(input, output, session){
    # database connection
    ns <- session$ns
    dbtrigger <- makereactivetrigger()
    # all permissions info ----
    permisions <- reactive({
      dbtrigger$depend()
      as_tibble(tbl(db_con,"t_Permissions")) %>%
        arrange(id) %>%
        select(-c(created_by,createdon,modified_by,modified_on)) %>%
        bind_cols(
          tibble(Edit = shinyInput(FUN = actionButton,
                                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4",
                                   n = nrow(.),id = 'edit_permission_',icon=icon("edit"),
                                   class="action-button bttn bttn-material-flat bttn-xs bttn-success bttn-no-outline",
                                   label = "",onclick = 'Shiny.setInputValue(\"settings_1-edit_permissions\", this.id, {priority: \"event\"})'),
                 Delete = shinyInput(FUN = actionButton,
                                     style="color: #fff; background-color: red; border-color: #2e6da4",
                                     n = nrow(.),id = 'delete_permision_',icon = icon("trash"),
                                     class = "action-button bttn bttn-material-flat bttn-xs bttn-success bttn-no-outline",
                                     label = "",onclick = 'Shiny.setInputValue(\"settings_1-delete_permissions\", this.id, {priority: \"event\"})')
          )
        )
    })
    # # add new permission
    observeEvent(input$add_permision,{
      edit_permission_ui("new_permissions",ns,NULL,"New Permission")
    }, ignoreInit = TRUE)
    # submit the new permison
    observeEvent(input$save_new_permission, {
      user <- authentication()$email
      isolate(custom_db_actions(
        insert_clause(con = db_con,table = "t_Permissions",
                      cols = c("permission_name","permission_type","created_by"),
                      values = c(input$permission_name, input$permission_type,
                                 user),exec = T,type = 2),
        "Permission Added Successfully"
      ))
      dbtrigger$trigger()
    }, ignoreInit = TRUE)
    # edit permissions
    permissions_data <- reactiveValues(value = NULL)
    observeEvent(input$edit_permissions,{
      selected_row <- as.numeric(strsplit(input$edit_permissions,"_")[[1]][3])
      permisions <- permisions() %>% filter(row_number() == selected_row) %>% as.list()
      edit_permission_ui("edit_permission",ns,permisions)
    }, ignoreInit = TRUE)
    observeEvent(input$save_permission,{
      selected_row <- as.numeric(strsplit(input$edit_permissions,"_")[[1]][3])
      permisions <- permisions() %>% filter(row_number() == selected_row) %>% as.list()
      custom_db_actions(
        action = update_clause(
          con   = db_con,
          table = "t_Permissions",
          what  = c("permission_name", "permission_type","modified_by","modified_on"),
          by    = c(input$permission_name, input$permission_type,"George",as.character(Sys.time())),
          where = "id",
          is = permisions$id,
          exec = T
        ),
        successmessage = "New Permission Updated Successfully",
        input = input$save_permission,
        session = session
      )
      dbtrigger$trigger()
    }, ignoreInit = TRUE)
    # delete permissions
    observeEvent(input$delete_permissions,{
      selected_row <- as.numeric(strsplit(input$delete_permissions,"_")[[1]][3])
      permisions <- permisions()  %>% filter(row_number() == selected_row) %>% as.list()
      # delete_confirm  <- function(text) {}
      delete_confirm(text = span(icon('thumbs-down'), "No"),
                     delete_id = "delete_permission",
                     ns = ns)
    },ignoreInit = T)
    # confirmdeletion
    observeEvent(input$delete_permission,{
      selected_row <- as.numeric(strsplit(input$delete_permissions,"_")[[1]][3])
      permisions <- permisions()  %>% filter(row_number() == selected_row) %>% as.list()
        custom_db_actions(
          delete_clause(
            con = db_con,
            table = "t_Permissions",
            where = "id",
            is = permisions$id,
            exec = T
          ),
          "Permission Deleted Successfully"
        )
        dbtrigger$trigger()
      #
    })

    # render permisions
    output$permisions_dt <- renderDT({
      permisions() #%>% select(-id)
    }, escape = F,selection = "none",options = list(
      columnDefs = list(list(className = 'dt-center', targets = c(1,4:5)))
    ))
    #
    # get available groups ----
    groups = reactive({
      dbtrigger$depend()
      as_tibble(tbl(db_con,"t_UserGroups")) %>%
        rowid_to_column() %>%
        bind_cols(
          tibble(Edit = shinyInput(FUN = actionButton,
                                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4",
                                   n = nrow(.),id = 'edit_group_',icon=icon("edit"),
                                   class="action-button bttn bttn-material-flat bttn-xs bttn-success bttn-no-outline",
                                   label = "",onclick = 'Shiny.setInputValue(\"settings_1-edit_group\", this.id, {priority: \"event\"})'),
                 Delete = shinyInput(FUN = actionButton,
                                     style="color: #fff; background-color: red; border-color: #2e6da4",
                                     n = nrow(.),id = 'delete_group_',icon = icon("trash"),
                                     class = "action-button bttn bttn-material-flat bttn-xs bttn-success bttn-no-outline",
                                     label = "",onclick = 'Shiny.setInputValue(\"settings_1-delete_group\", this.id, {priority: \"event\"})')
          )
        )

    })
    # render Groups Data
    output$groups_data <- renderDT({groups()}, escape = F, selection = "none",options = list(
      columnDefs = list(list(className = 'dt-center', targets = c(1,4:5)))
    ))
    # # add new group
    observeEvent(input$add_group,{
      permisions <- tbl(db_con,"t_Permissions")
      # selected_row <- as.numeric(strsplit(input$edit_button,"_")[[1]][3])
      group_auth <- permisions %>% filter(permission_type == "Application Access") %>% pull(permission_name) %>% unique()
      user_auth <- permisions %>% filter(permission_type != "Application Access") %>% pull(permission_name)
      group_settings(ns,
                     fun = "save_new_group",
                     group_auth = group_auth,
                     user_auth = user_auth,
                     title = "Add New Group Information")
    }, ignoreInit = TRUE)
    # submit the new group
    observeEvent(input$save_new_group, {
      # add group
      req(input$new_group)
      custom_db_actions(
        insert_clause(con = db_con,table = "t_UserGroups",
                      cols = c("name"),
                      values = c(input$new_group),exec = T,type = 2),
        "Group Added Successfully"
      )
      # assign permissions

      new_permissions <- get_new_group_permissions(
        selected_permissions = input$permissions_selected,
        group_name = input$new_group
      )

      dbWriteTable(db_con,name = "t_GroupPermissions",value=new_permissions,append=T)
      dbtrigger$trigger()
    }, ignoreInit = TRUE)
    # edit groups
    observeEvent(input$edit_group,{
      selected_row <- as.numeric(strsplit(input$edit_group,"_")[[1]][3])
      group_id <- groups() %>% filter(rowid == selected_row) %>% pull(id)
      group_permisions <- tbl(db_con,"t_GroupPermissions") %>% filter(group_id == !!group_id)
      permissions <- tbl(db_con,"t_Permissions") %>%
        inner_join(select(group_permisions,id = permission_id))

      group_auth <- tbl(db_con,"t_Permissions") %>% filter(permission_type == "Application Access") %>% pull(permission_name) %>% unique()
      user_auth <- tbl(db_con,"t_Permissions") %>% filter(permission_type != "Application Access") %>% pull(permission_name) %>% unique()

      group_settings(ns,
                     fun = "edit_group_permission",
                     group_auth = group_auth,
                     user_auth = user_auth ,
                     selected_perms = permissions %>% pull(permission_name) %>% unique(),
                     value = filter(groups(),rowid == selected_row)$name,
                     title = "Edit Group Information")

    })
    observeEvent(input$edit_group_permission,{
      edit_group_permission(groups(),
                            selected_row = input$edit_group,
                            group_name = input$new_group,
                            permissions_selected = input$permissions_selected)
      dbtrigger$trigger()
    })
    # delete groups
    observeEvent(input$delete_group,{
      # selected_row <- as.numeric(strsplit(input$edit_group, "_")[[1]][3])
      # group_id <- groups() %>% filter(rowid == selected_row) %>% pull(id)
      delete_confirm(text = span(icon('thumbs-down'), "No"),
                     delete_id = "delete_group_item",
                     ns = ns)
    })
    observeEvent(input$delete_group_item,{
      selected_row <- as.numeric(strsplit(input$delete_group, "_")[[1]][3])
      group_id <- groups() %>% filter(rowid == selected_row) %>% pull(id)
      custom_db_actions(action =  delete_clause(con = db_con,table = "t_UserGroups",
                                                where = "id",is = group_id,exec = T
      ),successmessage = "Group Deleted Successfuly")
      dbtrigger$trigger()
    })
    # users----
    users = reactive({
      dbtrigger$depend()
      tbl(db_con,"t_Users") %>%
        select(id,first_name,last_name,email,mobileno) %>%
        as_tibble() %>%
        rowid_to_column() %>%
        bind_cols(
          tibble(View = shinyInput(FUN = actionButton,
                                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4",
                                   n = nrow(.),id = 'edit_group_',icon=icon("eye"),
                                   class="action-button bttn bttn-material-flat bttn-xs bttn-success bttn-no-outline",
                                   label = "",onclick = 'Shiny.setInputValue(\"settings_1-edit_user\", this.id, {priority: \"event\"})'),
                 Delete = shinyInput(FUN = actionButton,
                                     style="color: #fff; background-color: red; border-color: #2e6da4",
                                     n = nrow(.),id = 'delete_group_',icon = icon("trash"),
                                     class = "action-button bttn bttn-material-flat bttn-xs bttn-success bttn-no-outline",
                                     label = "",onclick = 'Shiny.setInputValue(\"settings_1-delete_user\", this.id, {priority: \"event\"})')
          )
        )

    })
    agents = reactive({
      dbtrigger$depend()
      tbl(db_con,"t_Agents") %>%
        select(id,name,start) %>%
        as_tibble() %>%
        rowid_to_column() %>%
        bind_cols(
          tibble(View = shinyInput(FUN = actionButton,
                                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4",
                                   n = nrow(.),id = 'edit_agent_',icon=icon("eye"),
                                   class="action-button bttn bttn-material-flat bttn-xs bttn-success bttn-no-outline",
                                   label = "",onclick = 'Shiny.setInputValue(\"settings_1-edit_agent\", this.id, {priority: \"event\"})'),
                 Delete = shinyInput(FUN = actionButton,
                                     style="color: #fff; background-color: red; border-color: #2e6da4",
                                     n = nrow(.),id = 'delete_agent_',icon = icon("trash"),
                                     class = "action-button bttn bttn-material-flat bttn-xs bttn-success bttn-no-outline",
                                     label = "",onclick = 'Shiny.setInputValue(\"settings_1-delete_agent\", this.id, {priority: \"event\"})')
          )
        )

    })
    agent_contacts = reactive({
      # req(input$agent_name)
      selected_row  <- as.numeric(strsplit(input$edit_agent,"_")[[1]][3])
      agent_info  <- agents() %>% filter(row_number() == selected_row) %>% as.list()
      dbtrigger$depend()
      tbl(db_con,"t_Agent_Contacts") %>%
        filter(agent_id == !!agent_info$id) %>%
        select(id,agent_id,name,mobilenumber,emailaddress,ismain_contact) %>%
        as_tibble() %>%
        rowid_to_column() %>%
        bind_cols(
          tibble(View = shinyInput(FUN = actionButton,
                                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4",
                                   n = nrow(.),id = 'edit_agent_contact_',icon=icon("eye"),
                                   class="action-button bttn bttn-material-flat bttn-xs bttn-success bttn-no-outline",
                                   label = "",onclick = 'Shiny.setInputValue(\"settings_1-edit_agent_contact\", this.id, {priority: \"event\"})'),
                 Delete = shinyInput(FUN = actionButton,
                                     style="color: #fff; background-color: red; border-color: #2e6da4",
                                     n = nrow(.),id = 'delete_agent_contact_',icon = icon("trash"),
                                     class = "action-button bttn bttn-material-flat bttn-xs bttn-success bttn-no-outline",
                                     label = "",onclick = 'Shiny.setInputValue(\"settings_1-delete_agent_contact\", this.id, {priority: \"event\"})')
          )
        )

    })
    # add new users
    observeEvent(input$add_user,{
      permisions <- tbl(db_con,"t_Permissions")
      user_auth  <- permisions %>%
        filter(permission_type != "Application Access")%>%
        pull(permission_name) %>%
        unique()
      choices = tbl(db_con,"t_UserGroups") %>% pull(name)
      showModal(ui = user_details_man(ns,type="new",
                                      user_auth = user_auth,
                                      fun = "save_new_user",
                                      choices = choices))

    })
    observeEvent(input$save_new_user,{

      custom_db_actions(
        action = {
          user_group       <- tbl(db_con,'t_UserGroups') %>% filter(name == !!input$user_category) %>% pull(id)
          initial_password <- passwd_generator()
          insert_clause(con = db_con, table = "t_Users",
                        cols = c("first_name", "last_name", "email","start","usergroup","password","pass_reset","mobileno"),
                        values = c(input$first_name, input$last_name, input$email,
                                   as.character(Sys.Date()),
                                   user_group,
                                   password_manager("new_pass",random_pass=initial_password),
                                   TRUE,
                                   input$mobileno
                        ),
                        exec = T, type = 2)
          permissions_selected <- get_new_user_permissions(input$permissions_selected,
                                                           input$email)
          dbWriteTable(conn = db_con,name = "t_Userpermissions",value = permissions_selected,append=T)
          },
      toast = F,
      successmessage = password_manager("new_pass_message",ns=ns,
                                        initial_password = initial_password)
      )
      # send message to the newly created user
      print(paste("Dear ",input$first_name,",\n your new pass is ",initial_password))
      email_push(
        to = input$email,
        from = "dwd",
        message = account_creation(input$email,initial_password),
        Sender = "Geoge",
        Recipient = input$first_name,
        Subject = "NEW ACCOUNT CREATED",
        sig = NULL,
        test = T
      )
      dbtrigger$trigger()
    })
    # user edit details UI
    observeEvent(input$edit_user,{
      usergroups <- tbl(db_con,"t_UserGroups")
      permisions <- tbl(db_con,"t_Permissions")
      selected_row  <- as.numeric(strsplit(input$edit_user,"_")[[1]][3])
      user_info     <- users() %>% filter(row_number() == selected_row) %>% as.list()
      # user category
      user_category <- tbl(db_con,"t_Users") %>%
        filter(id == !!user_info$id) %>%
        select(customer_id = id,usergroup) %>%
        inner_join(tbl(db_con,"t_GroupPermissions"),by=c('usergroup' = 'group_id')) %>%
        inner_join(usergroups %>% select(-createdon),
                   by=c('usergroup' = 'id'))

       auth = left_join(permisions %>%
                  filter(permission_type != 'Application Access'),
                  tbl(db_con,'t_Userpermissions') %>%select(-id) %>%
                    filter(user_id == !!user_info$id) ,
                  by=c('id'='permission_id')) %>%
        mutate(permission_selected = ifelse(is.na(user_id),"notselected","selected")) %>%
        select(permission_selected,permission_name)

       user_auth  <- auth %>%
         filter(permission_selected == "notselected") %>%
         pull(permission_name) %>%
         unique()
       selected_perms  <- auth %>%
         filter(permission_selected == "selected") %>%
         pull(permission_name) %>%
         unique()
      # selected_perms
      showModal(
        ui = user_details_man(
          ns,
          user_auth = user_auth,
          selected_perms = selected_perms,
          first_name = user_info$first_name,
          last_name = user_info$last_name,
          email = user_info$email,
          mobileno = user_info$mobileno,
          fun = "edit_user_details",
          user_category = user_category %>% pull(name),
          choices = usergroups %>% pull(name),
          type = "edit"
          ))
    })
    observeEvent(input$edit_user_details,{
      custom_db_actions(action = {
        selected_row  <- as.numeric(strsplit(input$edit_user,"_")[[1]][3])
        user_info     <- users() %>% filter(row_number() == selected_row) %>% as.list()
        groupid <- tbl(db_con,"t_UserGroups") %>%
          filter(name == !!input$user_category) %>%
          pull(id)
        print(input$mobileno)
        update_clause(con = db_con,table = "t_Users",
                      what = c("first_name", "last_name", "email","usergroup","mobileno"),
                      by = c(input$first_name,
                             input$last_name,
                             input$email,
                             groupid,
                             input$mobileno),where = "id",is = user_info$id,exec = T)

        delete_clause(con = db_con,table = "t_Userpermissions",where = "user_id",is = user_info$id,exec = T)
        permissions_selected <-get_new_user_permissions(input$permissions_selected, input$email)
        dbWriteTable(db_con,"t_Userpermissions",permissions_selected,append=T)
        },toast = T,successmessage = "User Updated Successfully!")
      dbtrigger$trigger()
    })
    # settings_1-generate_random_pass
    observeEvent(input$generate_random_pass,{
      reset_pass <- passwd_generator()
      selected_row  <- as.numeric(strsplit(input$edit_user,"_")[[1]][3])
      user_info     <- users() %>% filter(row_number() == selected_row) %>% as.list()
      update_clause(
        con   = db_con,
        table = "t_Users",
        what  = c("password", "pass_reset"),
        by    = c(password_manager("new_pass",random_pass=reset_pass),TRUE),
        where = "id",
        is = user_info$id,
        exec = T
      )
      dbtrigger$trigger()
      password_manager("new_pass_message",initial_password=reset_pass)
      email_push(
        to = input$email,from = "dwd",message = pass_reset(reset_pass),Sender = "Geoge",
        Recipient = input$first_name,Subject = "PASSWORD RESET",sig = NULL,test = T
      )

    })
    # change_password
    observeEvent(input$change_password,{
      print("Here")
      custom_db_actions(action = {
        selected_row  <- as.numeric(strsplit(input$edit_user,"_")[[1]][3])
        user_info     <- users() %>% filter(row_number() == selected_row) %>% as.list()
        update_clause(
          con   = db_con,
          table = "t_Users",
          what  = c("pass_reset"),
          by    = c(TRUE),
          where = "id",
          is = user_info$id,
          exec = T
        )
      },toast = T,successmessage = paste0(user_info$first_name ," will be required to enter a new password."))
      dbtrigger$trigger()
      })
    # delete user
    observeEvent(input$delete_user,{
      # selected_row <- as.numeric(strsplit(input$edit_group, "_")[[1]][3])
      # group_id <- groups() %>% filter(rowid == selected_row) %>% pull(id)
      delete_confirm(text = span(icon('thumbs-down'), "No"),
                     delete_id = "delete_user_details",
                     ns = ns)
    })
    observeEvent(input$delete_user_details,{
      selected_row <- as.numeric(strsplit(input$delete_user, "_")[[1]][3])
      user_info   <- users() %>% filter(row_number() == selected_row)

      custom_db_actions(action =  delete_clause(con = db_con,table = "t_Users",
                                                where = "id",is = user_info$id,exec = T
      ),successmessage = "User Deleted Successfuly")
      dbtrigger$trigger()
    })
    #
    # Agents edit details UI----
    observeEvent(input$edit_agent,{
      usergroups <- tbl(db_con,"t_UserGroups")
      permisions <- tbl(db_con,"t_Permissions")
      selected_row  <- as.numeric(strsplit(input$edit_agent,"_")[[1]][3])
      agent_info    <- agents() %>% filter(row_number() == selected_row) %>% as.list()
      # selected_perms
      showModal(
        ui = modalDialog(title = "Edit Agent Details",footer = modal_footer(),
                         size = 'm',easyClose = T,
                         fluidRow(class="agent_edit_container",
                           col_12(
                             col_3(textInput(ns('agent_id'),"DCA IS",
                                             value = agent_info$id,
                                             width = "100%")),
                             col_6(textInput(ns('agent_name'),"DCA Name",
                                                  value = agent_info$name,
                                                  width = "100%")),
                             col_3(
                               actionBttn(ns("add_contact"),"New Contact",style = "material-flat",status = "success" ,icon = icon("plus")
                               )
                             )
                             ),
                           col_12(
                             class="dca_contacts",DTOutput(ns("agent_contacts"))
                           )
                           # col_3()
                           )
                         )
        )
    })
    observeEvent(input$edit_agent_contact,{
        selected_row  <- as.numeric(gsub("[a-z]|_","","edit_agent_contact"))
        contact_info     <- agent_contacts() %>% filter(row_number() == selected_row) %>% as.list()
        showModal(ui = agent_contact_change_ui(ns,contact_info))
      })
    observeEvent(input$save_contact_edit,{
      agent_name     = input$agent_name
      mobilenumber   = input$mobilenumber
      emailaddress   = input$emailaddress
      ismain_contact = input$ismain_contact
      contact_id       = as.numeric(input$contact_id)
      custom_db_actions(action = {
        update_clause(db_con, table = "t_Agent_Contacts",
                      what = c("name", "mobilenumber", "emailaddress", "ismain_contact"),
                      by = c(agent_name, mobilenumber, emailaddress, ismain_contact),
                      where = "id",
                      is = contact_id, exec = T)
      },successmessage = paste(input$agent_name,"contact details updated successfully!")
      )
      dbtrigger$trigger()
    })
    observeEvent(input$add_contact,{
      selected_row  <- as.numeric(gsub("[a-z]|_","","edit_agent_contact"))
      # contact_info     <- agent_contacts() %>% filter(row_number() == selected_row) %>% as.list()
      # print(contact_info)
      showModal(ui = agent_contact_change_ui(ns,list(),"new",input$agent_id))
    })
    observeEvent(input$save_contact_new,{
      agent_name     = input$agent_name
      mobilenumber   = input$mobilenumber
      emailaddress   = input$emailaddress
      ismain_contact = ifelse(input$ismain_contact == "yes",T,F)
      agent_id     = as.numeric(input$agent_id)
      custom_db_actions(
        action = {
          insert_clause(db_con, table = "t_Agent_Contacts",
                        values = c(agent_id,agent_name,mobilenumber,emailaddress,ismain_contact,as.character(Sys.Date())),
                      cols = c("agent_id","name","mobilenumber","emailaddress","ismain_contact","created_on"), type = 2,
                      exec = T)},
        successmessage = paste(agent_name,"contact information added successfully!")
        )
      dbtrigger$trigger()

    })
    observeEvent(input$delete_agent_contact,{
      delete_confirm(text = span(icon('thumbs-down'), "No"),
                     delete_id = "delete_agent_contact_details",
                     ns = ns)
    })
    observeEvent(input$delete_agent_contact_details,{
      selected_row <- as.numeric(gsub("[a-z]|_","","delete_agent_contact"))
      # contact_info <- agent_contacts() %>% filter(row_number() == selected_row) %>% as.list()
      delete_clause(con = db_con,
                    table = "t_Agent_Contacts",
                    where = "id",is = input$agent_id)
      dbtrigger$trigger()
      removeModal()
    })
    # delete_ agent
    observeEvent(input$delete_agent,{
         delete_confirm(text = span(icon('thumbs-down'), "No"),
                     delete_id = "delete_agent_details",
                     ns = ns)
    })
    observeEvent(input$delete_agent_details,{
      selected_row  <- as.numeric(gsub("[a-z]|_","",input$delete_agent))
      agents <- agents() %>%  filter(row_number() == selected_row) %>% as.list()
      custom_db_actions(action =  delete_clause(con = db_con,table = "t_Agents",
                                                where = "id",is = agents$id,exec = T
      ),successmessage = "User Deleted Successfuly")
      dbtrigger$trigger()
    })
    # render users
    output$users_data <- renderDT({users() %>% select(-id,-rowid)},escape = F, selection = "none",options = list(
      columnDefs = list(list(className = 'dt-center', targets = c(1,4:5)))
    ))
    # render agents
    output$agents_data <- renderDT({agents() %>% select(-id,-rowid)},escape = F, selection = "none",options = list(
      columnDefs = list(list(className = 'dt-center', targets = c(1,3:4)))
    ))
    # render agents contacts
    output$agent_contacts <- renderDT({agent_contacts() %>% select(-id,-rowid)},
                                      escape = F, selection = "none",extensions = 'FixedColumns',
                                      options = list(
                                        fixedColumns = list(rightColumns = 2),
                                        scrollX = TRUE,
                                        columnDefs = list(list(className = 'dt-center', targets = c(1,3:4)))
    ))
    # user password reset
    output$password_manager_ui <- renderUI({
      switch (input$paswd_manager,
              # 'Change Password' = {div(class="text-focus-in pass_reset",
              #                          col_8(textInput(ns("new_password"),"New Password",width = "100%")),
              #                          col_4(span(class="pull-right",
              #                                     actionBttn(ns("save_new_pass"), "Save Password",
              #                                                size = "xs",icon = icon("save"),
              #                                                block = F, style = "material-flat", color = 'success')
              #                          )))},
              'Reset Password' = {
                div(class="text-focus-in pass_reset",
                    col_8(p("This will action will generate a new random passwprd to be used for new logins.")),
                    col_4(span(class="pull-right",
                               actionBttn(ns("generate_random_pass"), "Reset Password",
                                          size = "xs",
                                          block = F, style = "material-flat", color = 'success')
                    )))
              },
              'Force password Change' = {
                div(class="text-focus-in pass_reset",
                    col_8(p("This will force the user to create a new password upon successfull Login.")),
                    col_4(span(class="pull-right",
                               actionBttn(ns("change_password"), "Change Password",
                                          size = "xs",
                                          block = F, style = "material-flat", color = 'success')
                    )))
              }
      )

    })
    # dynamicaly add dca contact entry UI
    # Insert a new contact UI when the "add_another" button is clicked
    observeEvent(input$add_another, {
      # startAnim(session, paste0(".list-item-", gsub("remove-", "", input$remove)),
      #           'fadeInRightBig')
      insertUI(selector = ".dca_contacts_list",
               # where = 'afterEnd',
               ui = dca_contact_ui(ns, input$add_another))
    })
    # Remove the corresponding contact UI when the "remove" button is clicked
    observeEvent(input$remove, {
      removeUI(
        selector = paste0(".list-item-",gsub("remove-","",input$remove)),
        immediate = FALSE
      )
    })
    observeEvent(input$save_dca,{
      custom_db_actions(
        action = {
        # save the agent name to respective table
        req(input$agent_name)
        insert_clause(db_con,
                      table = 't_Agents',
                      values = c(input$agent_name,as.character(Sys.Date())),
                      cols = c("name","start"),exec = T,type = 2)
        # save contacts
        tryCatch(
          expr = {
            contacts <- collect_contacts(input,input$add_another)
          if (nrow(contacts) > 0) {
            agent_id <- tbl(db_con,"t_Agents") %>%
              filter(name == !!input$agent_name) %>%
              pull(id)
            contacts <- contacts %>%
              mutate(agent_id = agent_id)
            dbWriteTable(db_con,"t_Agent_Contacts",contacts,append=T)
          }
        },
          error = function(e){
            delete_clause(db_con,table = "t_Agents",where = "name",
                          is = input$agent_name,exec = T)
            stop(e)
          }
        )
      },
      successmessage = paste(input$agent_name,"details saved successfully!")
      )
      dbtrigger$trigger()

    })
    # collection utils ----

    # render respective UI -----
    output$modles_ui <- renderUI({
      switch (input$modules,
              'Permisions' = {
                tagList(
                  fluidRow(class = "title_row",
                           p(class = "title_text text-focus-in mr-5","All Available Permissions")
                  ),
                  col_12(class='settings_panel',
                         col_10(class="s",DTOutput(ns('permisions_dt'))),
                         col_2(class="settings_panel-actionpanel",
                               actionBttn(ns("add_permision"),"Add New",icon = icon("plus"),size = "xs",block = T,style = "material-flat",color = 'success',data = list(table = "t_Permissions"))
                         )
                  )
                )
              },
              'Groups' = {
                tagList(
                  fluidRow(class = "title_row",
                           p(class = "title_text text-focus-in mr-5","Group Permissions")
                  ),
                  col_12(class='settings_panel',
                         col_10(class="s",DTOutput(ns('groups_data'))),
                         col_2(class="settings_panel-actionpanel",
                               actionBttn(ns("add_group"),"Add New",icon = icon("plus"),size = "xs",block = T,style = "material-flat",color = 'success',name="new_group")
                         ))
                )
              },
              'Users' = {
                col_12(class="settings_panel",
                       col_10(
                         accordion(id="usercategories",
                                   accordionItem(title = "Internal Debt Collectors",status = "success",solidHeader = T,collapsed = F,
                                                 div(class = "users_data_view",DTOutput(ns("users_data")))),
                                   accordionItem(title = "External Debt Collectors",status = 'success',solidHeader = T,collapsed = T,
                                                 div(class = "users_data_view",DTOutput(ns("agents_data")))
                                                 ))
                       ),
                       col_2(class = "settings_panel-actionpanel",
                             actionBttn(ns("add_user"),"Add New",icon = icon("plus"),size = "xs",block = T,
                                        style = "material-flat",color = 'success',name="new_group")
                             )
                       )
              },
              'Collection Utils' = {
                accordion(id = "collection_utils",
                          accordionItem(title = "SMS Templates",status = "success",solidHeader = T,collapsed = F,

                                        ),
                          accordionItem(title = "Customer Feedback Classification",status = "success",solidHeader = T,collapsed = T)
                          )
              },
              'CBS Authentication' = {"cbs UI"},
      )
    })

  })
}

## To be copied in the UI
#

## To be copied in the server
#
