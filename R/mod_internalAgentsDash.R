#' @importFrom   shiny observeEvent renderTable h3 hr tableOutput selectInput bootstrapPage fluidRow  moduleServer  NS  h1  div h4  icon  tagList p
#' @importFrom   shinydashboard infoBox valueBox
#' @importFrom   shinydashboardPlus box descriptionBlock
#' @importFrom   shinyWidgets radioGroupButtons pickerInput actionBttn dropdown dropdownButton tooltipOptions
#' @import   echarts4r
#' @import   kableExtra
#' @importFrom   dplyr `%>%`
#' @noRd
internalAgentsDash_UI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(class = "header",
      col_2(class="filter_details text-focus-in","Call Agent: All"),
      col_2(class="filter_details text-focus-in","Products: All"),
      col_1(class="filter_details text-focus-in","Year: 2023"),
      col_2(class="filter_details text-focus-in","Month/Quarter: Feb"),
      col_2(class="filter_details text-focus-in","Comparing to: None"),
      col_2(class="filter_details ",filterui(ns)),
      col_1(class="filter_details last",actionBttn(ns("tour"),"",icon = icon("info"),style = "jelly",color = "success",size = "xs"))
      ),
    col_12(class = "filter-section",
                         fluidRow(class='top-kpis',
                           col_3(class = "agent_kpis first",
                                         custom_ui("Call Agents Performance Dashboard",
                                                           actionBttn(ns("downloadreport"),"Download",icon = icon("download"),style = "material-flat",color = "success",size = "xs"))
                           ),
                           col_3(class = "agent_kpis",custom_value_box(title = "Allocation Amount",
                                                                                       value = "250000",
                                                                                       trend = "-5",
                                                                                       trend_text="last 5 days",
                                                                                       trend_icon = "arrow-down",
                                                                                       main_icon ="coins")
                           ),
                           col_3(class = "agent_kpis",custom_value_box(title = "Collected Amount",
                                                                                       value = "250000",
                                                                                       trend = "-5",
                                                                                       trend_text="last 5 days",
                                                                                       trend_icon = "arrow-down",
                                                                                       main_icon ="coins")
                           ),
                           col_3(class = "agent_kpis",custom_value_box(title = "Recovery Rate",
                                                                                       value = "25%",
                                                                                       trend = "-5",
                                                                                       trend_text="last 5 days",
                                                                                       trend_icon = "arrow-down",
                                                                                       main_icon ="coins")
                           )
                         ),
                         # collection metrics
                         fluidRow(
                           col_6(box(title = "Agent Collection trend",status = "success",solidHeader = T,width = 12,
                                             fluidRow(class="chart_controls",radioGroupButtons(
                                               inputId = c("trend-chart-options"),
                                               label = "",
                                               choices = c("Amount Recovered", "Recovery Rate"),
                                               justified = FALSE,size = "xs",status = "success",
                                             )),
                                             echarts4rOutput(ns("agent_trend"),height = "229px"),
                                             height="230px",class="trend-section"
                                             ),
                                         box(title = "Portfolio breakdown",status = "success",solidHeader = T,width = 12,
                                             col_3(class = "portfolio watch",portfolio_bd(value = "999,999,999",rate = "-10% from last month",title = "Watch")),
                                             col_3(class = "portfolio substandard",portfolio_bd(value = "999,999,999",rate = "-10% from last month",title = "Substandard")),
                                             col_3(class = "portfolio doubtful",portfolio_bd(value = "999,999,999",rate = "-10% from last month",title = "Doubtful")),
                                             col_3(class = "portfolio loss",portfolio_bd(value = "999,999,999",rate = "-10% from last month",title = "Loss"))
                                         )),
                           col_3(class="midle_panel",
                                         box(title = "Customer Feedback",status = "success",solidHeader = T,width = 12,
                                             tableOutput(ns("agents_feedback"))
                                         )
                           ),
                           col_3(
                             col_12(class="collection_activity",
                                            p(class="sub-topic","Agent Collection Activity"),
                                            custom_panel(width = 6,title = "Allocated Cases",value = "99,999",change_icon()),
                                            custom_panel(width = 6,title = "Recovered Cases",value = "99,999",change_icon()),
                                            custom_panel(width = 6,title = "Contacted Cases",value = "99,999",change_icon()),
                                            custom_panel(width = 6,title = "Contact Conversion Rate",value = "40%",caretstats=NULL),
                             ),
                             hr(),
                             box(class="recovery_summary",title = "Slippages",status = "success",width = 12,solidHeader = T,height = "200px",
                                 col_6(
                                   descriptionBlock(
                                     number = "17%",
                                     numberColor = "green",
                                     numberIcon = icon("caret-up"),
                                     header = "Ksh 35,210.43",
                                     text = "Amount",
                                     rightBorder = TRUE,
                                     marginBottom = FALSE
                                   )),
                                 col_6(
                                   descriptionBlock(
                                     number = "18%",
                                     numberColor = "red",
                                     numberIcon = icon("caret-down"),
                                     header = "13%",
                                     text = "Slippage Rate",
                                     rightBorder = FALSE,
                                     marginBottom = FALSE
                                   )
                                 )
                             ))
                         )
          )
  )
}

#' @noRd
internalAgentsDash_Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      # render echars
      output$agent_trend <-  echarts4r$renderEcharts4r(
        rhino::rhinos %>%
          echarts4r$group_by(Species) %>%
          echarts4r$e_chart(x = Year) %>%
          echarts4r$e_line(Population) %>%
          echarts4r$e_x_axis(Year) %>%
          echarts4r$e_tooltip() %>%
          echarts4r$e_dims(height = "229px")
      )
      output$agents_feedback <- function(){
        feed <- readRDS("app/static/feedback.rds")
        kableExtra$kable(class= "feedback_table",feed) %>%
          kableExtra$kable_styling()
      }
      # observeEvent(input$call_agent,{
      #   print(input$call_agent)
      # })
      output$portfolio_breakdown <- renderTable(
        shinipsum::random_table(4,ncol = 3,type = "numchar")
      )

    }
  )
}

# copy to main.R in the box section
# app/view/mod_internalAgentsDash

