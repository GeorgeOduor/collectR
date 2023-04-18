#' @importFrom shiny fluidRow bootstrapPage  moduleServer  NS  h1  div  icon  tagList p hr
#' @importFrom shinyWidgets actionBttn radioGroupButtons
#' @importFrom shinydashboardPlus box
#' @noRd
dca_dashboard_UI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(class = "header",
             col_2(class="filter_details text-focus-in","Call Agent: All"),
             col_2(class="filter_details text-focus-in","Products: All"),
             col_1(class="filter_details text-focus-in","Year: 2023"),
             col_2(class="filter_details text-focus-in","Month/Quarter: Feb"),
             col_2(class="filter_details text-focus-in","Comparing to: None"),
             col_2(class="filter_details",filterui(ns)),
             col_1(class="filter_details last",actionBttn(ns("tour"),"",icon = icon("info"),style = "jelly",color = "success",size = "xs"))
    ),
    fluidRow(class="main stats",
             col_6(
               box(title = "DCA Collection trend",status = "success",class="trend", solidHeader = T,width = 12,
                   fluidRow(class="chart_controls",radioGroupButtons(
                     inputId = c("trend-chart-options"),
                     label = "",
                     choices = c("Amount Recovered", "Recovery Rate"),
                     justified = FALSE,size = "xs",status = "success",
                   ))),
               col_12(class="collection_activity",
                              p(class="sub-topic","DCA Recovery Cohorts"),
                              custom_panel(width = 6,title = "Current week recovery",value = "99,999",change_icon()),
                              custom_panel(width = 6,title = "Current month recovery",value = "99,999",change_icon())
                              # custom_panel(width = 6,title = "Contacted Cases",value = "99,999",change_icon()),
                              # custom_panel(width = 6,title = "Contact Conversion Rate",value = "40%",caretstats=NULL),
               )
             ),
             col_4(
               box(title = "DCA Recovery Summary",status = "success",solidHeader = T,width = 12,
                   col_6(class = "portfolio top-left",portfolio_bd(value = "999,999,999",rate = "-10% from last month",title = "Outsourced")),
                   col_6(class = "portfolio top-right",portfolio_bd(value = "999,999,999",rate = "-10% from last month",title = "Recovered ")),
                   col_6(class = "portfolio bottom-right",portfolio_bd(value = "999,999,999",rate = "-10% from last month",title = "Outstanding ")),
                   col_6(class = "portfolio bottom-left",portfolio_bd(value = "999,999,999",rate = "-10% from last month",title = "Recovery Rate"))
               ),
               #col_12(p(class="sub-topic","Recovery Speed")),
               col_12(class="cohort_analysis",
                              col_6(class="left-panel",portfolio_bd(value = "9%",rate = "-10% from last month",title = "Average Recovery Days")
                              ),
                              col_6(portfolio_bd(value = "999,999,999",rate = "-10% from last month",title = "Averaage Recovery per day")))
             ),
             col_2(
               box(title = "Collection Feedback",solidHeader = T,status = "success",width = 12,
                   portfolio_bd(value = "999,999,999",rate = "-10% from last month",title = "PTPs"),
                   hr(class="divide"),
                   portfolio_bd(value = "999,999,999",rate = "-10% from last month",title = "PTP Rate"),
                   hr(class="divide"),
                   portfolio_bd(value = "999,999,999",rate = "-10% from last month",title = "Contact Conversion Rate"))
               ),
             col_3(
               div(class = "colection_vs_targets",
                   portfolio_bd2(value = "4,234",rate = "",title = "Number of Accounts Outsourced")
                   )
             ),
             col_3(
               div(class = "colection_vs_targets",
                   portfolio_bd2(value = "34",rate = "",title = "Number of Accounts Recovered")
                   )
             )
             )
  )
}

#' @noRd
dca_dashboard_Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {

    }
  )
}

# copy to main.R in the box section
# app/view/mod_dca_dashboard

