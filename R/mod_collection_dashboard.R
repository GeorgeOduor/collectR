#' @importFrom  shiny renderTable tableOutput selectInput bootstrapPage fluidRow  moduleServer  NS  h1  div h4  icon  tagList p
#' @importFrom  shinydashboard infoBox valueBox
#' @importFrom  shinydashboardPlus box descriptionBlock
#' @importFrom  shinyWidgets pickerInput actionBttn radioGroupButtons

#' @noRd
collection_dashboard_UI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(class = "header",
             col_1(class="filter_details ",filterui(ns)),
             col_3(class="filter_details text-focus-in","Recovery Dashboard:Full View"),
             col_2(class="filter_details text-focus-in","Products: All"),
             col_1(class="filter_details text-focus-in","Year: 2023"),
             col_2(class="filter_details text-focus-in","Month/Quarter: Feb"),
             col_2(class="filter_details","Comparing to: None"),
             col_1(class="filter_details last",actionBttn(ns("tour"),"",icon = icon("info"),style = "jelly",color = "success",size = "xs"))
    ),
      fluidRow(class="key-kpis top-kpis",
               infoBox(title = "Outsourced Amount",value = "Ksh 250 M",subtitle = change_icon(),width = 3),
               infoBox(title = "Outsourced Count",value = "45,023",subtitle = change_icon(),width = 3),
               infoBox(title = "Recovered Amount",value = "Ksh 250 M",subtitle = change_icon(),width = 3),
               infoBox(title = "Recovery Rate",value = "45%",subtitle = change_icon(),width = 3)
               ),
      fluidRow(class="charts",
               col_8(box(class="trend_analysis",title = "Collection Trend",status = "success",width = 12,solidHeader = T,height = "200px",
                                 fluidRow(class="chart_controls",
                                          radioGroupButtons(
                                   inputId = c("trend-chart-options"),
                                   label = "",
                                   choices = c("Amount Recovered", "Recovery Rate"),
                                   justified = FALSE,size = "xs",status = "success",
                                 ))
               )),
               col_4(box(class="recovery_summary",title = "Recovery Rate by team",status = "success",width = 12,solidHeader = T,height = "200px",
                                 col_6(
                                   descriptionBlock(
                                     number = "17%",
                                     numberColor = "green",
                                     numberIcon = icon("caret-up"),
                                     header = "$35,210.43",
                                     text = "Internal Agents",
                                     rightBorder = TRUE,
                                     marginBottom = FALSE
                                   )),
                                 col_6(
                                   descriptionBlock(
                                     number = "18%",
                                     numberColor = "red",
                                     numberIcon = icon("caret-down"),
                                     header = "1200",
                                     text = "External Agents",
                                     rightBorder = FALSE,
                                     marginBottom = FALSE
                                   )
                                 )
               ),
               col_12(class="allocation_count_stats",
                             col_6(class="allocation_count",
                                           portfolio_bd2(value = "4,234",rate = "",title = "Number of Accounts Outsourced")),
                             col_6(
                               col_12(class="full-recoverd",
                                              portfolio_bd(value = "999,999,999",rate = "-10% from last month",title = "Fully Recovered")),
                               col_12(class="partial-recovered",
                                              portfolio_bd(value = "999,999,999",rate = "-10% from last month",title = "Partially Recovered")),
                             )
               ))
               )
  )
}

#' @noRd
collection_dashboard_Server <- function(id) {
  moduleServer(id,function(input, output, session) {
        output$feedbk <- renderTable(
          shinipsum::random_table(ncol = 3,nrow = 9,type = "numchar")
        )
    }
  )
}

# copy to main.R in the box section
# app/view/mod_collection_dashboard

