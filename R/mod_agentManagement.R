
#' @importFrom shiny hr selectInput bootstrapPage fluidRow  moduleServer  NS  h1  div  icon  tagList tabsetPanel tabPanel p span
#' @importFrom shinydashboard box
#' @importFrom shinyWidgets actionBttn pickerInput checkboxGroupButtons radioGroupButtons
#' @importFrom shinydashboard box
#' @importFrom shinyjs disable addClass enable
#' @title agentManagement_UI
#' @noRd
agentManagement_UI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(class = "title_row",
             p(class = "title_text text-focus-in mr-5","Debt Collection Agents Management")
             ),
    fluidRow(
      col_2(div(id='agents_listing',box(title = "Agents/Agencies",width = 12,status = "success",solidHeader = T,
                                        class="agentslisting",height = "auto",
                                        selectInput(ns("agents"),label = "",
                                                    choices = list(
                                                      "Internal Debt Collectors"=c("George Oduor Wamaya",
                                                                                   "Kevin Otieno Wamaya"),
                                                      "External Debt Collectors"=c("Spectrum Network",
                                                                                   "Care Recoveries")
                                                    ),selectize = F,size = 25)
      ))),
      col_8(
        div(id = 'new_agent_details'),
        div(id = 'agent_details',
            existing_agent_details_ui(ns)
            )
      ),

      col_2(div(id="agent_actions",box(title = "",solidHeader = T,status = "success",width = 12,class="agentsactions",
                                       actionBttn(ns("add_new"),"Add New",style = "material-flat",size = "xs",color = "success")

      )))
    )
  )
}


#' @noRd
#' @importFrom shinyanimate startAnim
#' @importFrom shiny updateSelectizeInput selectizeInput
agentManagement_Server <- function(id) {
  moduleServer(id,function(input, output, session) {
    ns <- session$ns
    agents = tbl(db_con,"t_Users")
    # agent_info ----
      output$agent_info <- renderUI({
        tryCatch(
        expr = {
          tagList(
            col_10(class = "agent_details_list",
              # selected_agent <- ,
              agents %>%
                filter(email == !!input$internal_agent_search) %>%
                select(-password,-pass_reset,-id) %>%
                collect() %>%
                collect() %>%
                as.list() %>%
                named_to_li(class = "agent_details_list text-focus-in")
          ),
          col_2(
            actionBttn(ns("submit_agent"),"Submit",icon = icon('add'),
                       style = "material-flat",block = T,
                       size = "xs",color = "success")

          ))
        },
          error = function(e){
            shinydashboardPlus::loadingState()
          }
        )
      })
    # agent_details_ui ----
    output$agent_details_ui <- renderUI({
      existing_agent_details_ui(ns)
    })
      # agent submit ----
      observeEvent(input$submit_agent,{
      })
      # toggle ui ----
    observeEvent(input$add_new,{
      showModal(ui = modalDialog(
        new_agent_details_ui(ns,agents),
        title = "Add New Debt Collector",
        footer = modal_footer(),size = "m",easyClose = T
      ))
    })

    }
  )
}

# copy to main.R in the box section
# app/view/mod_agentManagement

