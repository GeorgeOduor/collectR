#' @importFrom  shiny renderTable tableOutput selectInput bootstrapPage fluidRow  moduleServer  NS  h1  div h4  icon  tagList p
#' @importFrom  shinydashboard infoBox valueBox
#' @importFrom  shinydashboardPlus box descriptionBlock
#' @importFrom  shinyWidgets pickerInput actionBttn radioGroupButtons
#' @importFrom plotly plotlyOutput

#' @noRd
collection_dashboard_UI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(class = "header",
             # col_1(class="filter_details ",filterui(ns)),
             col_2(class="filter_details text-focus-in",
                   pickerInput(ns('team'),label = "Category",
                                choices = c("All","Internal Agents","External Agents"),width = "fit",
                                selected = NULL,
                                inline = T,options = list(size=5),
                                multiple = F)
                   ),
             col_3(class="filter_details text-focus-in",
                   pickerInput(ns('agnets'),label = "Agents",
                               choices = c("All","Purple Royal Recoveries","Care Recoveries"),width = "fit",
                               selected = NULL,
                               inline = T,options = list(size=5),
                               multiple = F)),
             col_3(class="filter_details text-focus-in",
                   pickerInput(ns('product'),label = "Channel:",
                               choices = NULL,width = "fit",
                               selected = NULL,
                               inline = T,options = list(size=5),
                               multiple = F)
                   ),
             col_3(class="filter_details text-focus-in",
                   dateRangeInput(ns('date_period'),label = "From",separator = "to",
                                  min = as.Date("2023-01-01"),max = rollback(Sys.Date()))
                   )
             # col_2(class="filter_details text-focus-in","Month/Quarter: Feb"),
             # col_2(class="filter_details","Comparing to: None"),
             # col_2(class="filter_details ",actionBttn(ns("download_report"),"Download",icon = icon("download"),style = "material-flat",color = "success",size = "xs")),
             # col_1(class="filter_details last",actionBttn(ns("tour"),"",icon = icon("info"),style = "jelly",color = "success",size = "xs"))
    ),
      fluidRow(class="key-kpis top-kpis",
               infoBox(title = "Outsourced Amount",value = withLoader(textOutput(ns('outsourced_amount')),'html','loader4',proxy.height = '20px'),subtitle = change_icon(),width = 3),
               infoBox(title = "Outsourced Count",value = withLoader(textOutput(ns('outsourced_count')),'html','loader4',proxy.height = '20px'),subtitle = change_icon(),width = 3),
               infoBox(title = "Recovered Amount",value = withLoader(textOutput(ns('recovered_amount')),'html','loader4',proxy.height = '20px'),subtitle = change_icon(),width = 3),
               infoBox(title = "Recovery Rate",value = withLoader(textOutput(ns('recovery_rate')),'html','loader4',proxy.height = '20px'),subtitle = change_icon(),width = 3)
               ),
      fluidRow(class="charts",
               col_8(class="trend_analysis",
                     fluidRow(class="chartcontrols",
                              col_6(
                                span(id="custombox_title","Collection Trend")
                              ),col_6(class='button_col',radioGroupButtons(
                                inputId = ns("trend_chart_options"),
                                label = "",
                                choices = c("Amount Recovered", "Recovery Rate"),
                                justified = FALSE,size = "xs",status = "success",
                              ))),
                     fluidRow(class = "trend_charts",

                              withLoader(uiOutput(ns('collection_trend')),'html','loader4',proxy.height = "50px")
                              )

               ),
               col_4(
               col_12(class="allocation_count_stats",
                             col_6(class="allocation_count",
                                   tags$div(
                                     class = "dashboard-section",
                                     tags$h3(
                                       class = "portfolio-title2",
                                       "PTP Rate"
                                     ),
                                     hr(class = "divider"),
                                     withLoader(uiOutput(ns('ptp_rate_overal')),"html","loader4",proxy.height = "100px")
                                   )
                             ),
                             col_6(
                               col_12(class="full-recoverd",
                                              portfolio_bd(value = withLoader(textOutput(ns('full_recovery')),"html","loader4",proxy.height = "30px"),rate = "",title = span(class='spec_recovery_stats',"Fully Recovered"))),
                               col_12(class="partial-recovered",
                                              portfolio_bd(value = withLoader(textOutput(ns('partial_recovery')),"html","loader4",proxy.height = "30px"),rate = "",title = span(class='spec_recovery_stats',"Partially Recovered"))),
                             )
               ), box(class="recovery_summary",title = "Collection Agents Activity",status = "success",width = 12,solidHeader = T,height = "200px",
                      col_6(
                        withLoader(uiOutput(ns('contacted_cases')),'html',"loader4",proxy.height = "80px")
                      ),
                      col_6(
                        withLoader(uiOutput(ns('contacted_conversion_rate')),'html',"loader4",proxy.height = "80px")

                      )
               ))
               )
  )
}

#' @noRd
collection_dashboard_Server <- function(id,authentication) {
  moduleServer(id,function(input, output, session) {
    ns <- session$ns
    # datasets used here ===========
    outstanding_file <- tbl(db_con,'t_Outstanding')
    endmonth_collections <- tbl(db_con, 't_EndOfMonthCollectionTrend')
    dates_channels <- endmonth_collections %>% summarise(StartDate= min(RepaymentDate),endDate=max(RepaymentDate))
    agents <- reactive({
      req(input$category)
      tbl(db_con, 't_EndOfMonthCollectionTrend') %>%
        filter(Input$category)
    })
# update channel input===========
    updatePickerInput(session,inputId = 'product',choices = c("All",outstanding_file %>% pull(Channel) %>% unique()),selected = "All")
# update date range input ============
    updateDateRangeInput(session, "date_period",
                         start = rollback(rollback(Sys.Date()))+1,
                         end = Sys.Date(),
                         min = dates_channels %>% pull(StartDate),
                         max = dates_channels %>% pull(endDate)
                         )
        output$feedbk <- renderTable(
          shinipsum::random_table(ncol = 3,nrow = 9,type = "numchar")
        )
  # render outsourced amount ==============
        output$outsourced_amount <- renderText({
          req(input$product,input$date_period)
          top_kpis <- get_key_kpis(NULL,year(input$date_period[2]),month(input$date_period[2]),input$product)
          paste0("Ksh ",format(top_kpis$Outsource_Amount,big.mark=","))
          })
  # render outsourced amount ==============
        output$outsourced_count <- renderText({
          req(input$product,input$date_period)
          top_kpis <- get_key_kpis(NULL,year(input$date_period[2]),month(input$date_period[2]),input$product)
          format(top_kpis$Outsource_Count,big.mark=",")
          })
  # render recvered amount ==============
        output$recovered_amount <- renderText({
          req(input$product,input$date_period)
          top_kpis <- get_key_kpis(NULL,year(input$date_period[2]),month(input$date_period[2]),input$product)
          paste0("Ksh ",format(top_kpis$Recovered_Amount,big.mark=","))
          })
# render recovery rate ==============
        output$recovery_rate <- renderText({
          req(input$product,input$date_period)
          top_kpis <- get_key_kpis(NULL,year(input$date_period[2]),month(input$date_period[2]),input$product)
          paste0(round(top_kpis$Recovered_Amount/top_kpis$Outsource_Amount*100),"%")
        })
  # render recovery listing plot =====

        output$recovery_trend <- renderEcharts4r({
          req(input$product,input$date_period)
          recovery_listing(NULL,input$product,year(input$date_period[2]),month(input$date_period[2])) %>%
            collect() %>%
            e_chart(RepaymentDate) %>%
            e_line(`Amount Recovered`,smooth=T) %>%
            e_tooltip(trigger = "item")%>%
            e_dims(height = "300px") %>%
            e_axis_labels("Day","Daily Amount Recovered") %>%
            e_mark_point(data = max_) %>%
            e_mark_point(data = min_)
        })
        output$collection_trend <- renderUI({
          switch (input$trend_chart_options,
                  "Amount Recovered" = {echarts4rOutput(ns('recovery_trend'))},
                  "Recovery Rate" = {}
          )
        })
        # render team specific tecovery=====
        # Contact Conversion Rate
        output$contacted_conversion_rate <- renderUI({
          req(input$date_period)
          team_stats <- get_key_kpis(NULL,year(input$date_period[2]),month(input$date_period[2]),input$product,team_val = "ExternalDebtCollector")
#
#           contact_summary <- tbl(db_con,'t_ClientFeedBackSummary') %>%
#             filter(
#               year(ReportDate) == !!year(input$date_period[2]),
#               month(ReportDate) == !!month(input$date_period[2]),
#               Channel == input$product
#             )
          descriptionBlock(
            number = paste0(team_stats$Recovery_Rate,"%"),
            numberColor = "black",
            # numberIcon = icon("caret-up"),
            header = format(team_stats$Recovered_Amount,big.mark = ","),
            text = "Contact Conversion Rate",
            rightBorder = FALSE,
            marginBottom = FALSE
          )
        })
        # contacted_cases
        output$contacted_cases <- renderUI({
          req(input$date_period)
          team_stats <- get_key_kpis(NULL,year(input$date_period[2]),
                                     month(input$date_period[2]),input$product,
                                     team_val = "InternalDebtCollector")
          descriptionBlock(
            number = paste0(team_stats$Recovery_Rate,"%"),
            numberColor = "black",
            # numberIcon = icon("caret-up"),
            header = paste(format(team_stats$Recovered_Amount,big.mark = ",")),
            text = "Contacted Cases",
            rightBorder = TRUE,
            marginBottom = FALSE
          )
        })
        # render fully recovered amount ===========
        output$full_recovery <- renderText({
          req(input$date_period)
          top_kpis <- get_key_kpis(NULL,year(input$date_period[2]),month(input$date_period[2]),input$product)
          paste("Ksh",format(top_kpis$Fully_Paid_Amount,big.mark=","))
        })
        # partial recovery amount ===========
        output$partial_recovery <- renderText({
          req(input$date_period)
          top_kpis <- get_key_kpis(NULL,year(input$date_period[2]),month(input$date_period[2]),input$product)
          paste("Ksh",format(top_kpis$Partially_Paid_Amount,big.mark=","))
        })
        # overal ptp Rate ======
        output$ptp_rate_overal <- renderUI({
          req(input$product)
          ptp_stats <- tbl(db_con,'t_PTPRateSummary') %>%
            filter(
              Channel == !!input$product,
              year(as.Date(ReportDate)) == !!year(input$date_period[2]),
              month(as.Date(ReportDate)) == !!month(input$date_period[2]),
              as.Date(ReportDate) == max(as.Date(ReportDate))
            ) %>%
            # head(1) %>%
            collect()

          ptp_stats <- ptp_stats %>%
            summarise_if(is.numeric,sum) %>%
            mutate(
              PTPRate = round(PTPRate/nrow(ptp_stats)*100)
            ) %>%
            as.list()
          tagList(col_12(class="percent_section",
                         tags$h1(
                           class = "portfolio-value2",
                           paste0(round(ptp_stats$PTPRate ),"%")
                         )
          ),
          col_6(class="count_section",
                tags$span(class='ptp_count_title',"Count"),
                tags$span(class='ptp_count',format(ptp_stats$Count,big.mark="," )),
          ),
          col_6(class="amount_section",
                tags$span(class='ptp_amount_title',"Amount(Ksh)"),
                tags$span(class='ptp_amount',format(ptp_stats$CurrentBalance,big.mark="," )),
          ))
        })
#
    }
  )
}

# copy to main.R in the box section
# app/view/mod_collection_dashboard

