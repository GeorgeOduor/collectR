



#' @importFrom shiny fileInput fluidRow bootstrapPage  moduleServer  NS  h1  div  icon  tagList p
#' @importFrom shinydashboard box
#' @importFrom shinyWidgets actionBttn


#' @noRd
performanceUpdate_UI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(class = "title_row",
             p(class = "title_text text-focus-in mr-5","Daily Performance Update")
    ),
    box(title = "Data Upload",width = 12,solidHeader = T,status = "success",class="agentsactions",height = "400px",
        col_2(class="actionpoints",
          fileInput(ns("loanbook"),label = "Upload LoanBook",accept = c(".rdata",".rds",".xlsx",".csv")),

          actionBttn(ns("submit_upload"),"Submit Upload",style="material-flat",size="xs",status='success')
        )
        )
    # box(title = "data",width = 8,solidHeader = T,status = "success",class="agentsactions")
  )
}

#' @noRd
performanceUpdate_Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      234 + "ddfdf"
    }
  )
}

# copy to main.R in the box section
# app/view/mod_performanceUpdate

