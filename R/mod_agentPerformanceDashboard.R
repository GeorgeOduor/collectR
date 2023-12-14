#' @importFrom shiny tabsetPanel tabPanel observeEvent renderTable h3 hr tableOutput selectInput bootstrapPage fluidRow  moduleServer  NS  h1  div h4  icon  tagList p
#' @importFrom shinydashboard infoBox valueBox
#' @importFrom shinydashboardPlus box descriptionBlock
#' @importFrom lubridate month
#' @importFrom shinyWidgets radioGroupButtons pickerInput actionBttn dropdown dropdownButton tooltipOptions animateOptions
#' @import  echarts4r
#' @import kableExtra
#' @import shinycustomloader
#' @importFrom dplyr `%>%` mutate

#' @noRd
agentPerformanceDashboard_UI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(class = "header",

             col_3(class='dasgboard-title',
                   span(class="text-focus-in","Debt Collection Performance")),
             col_9(header_ui(ns))

    ),
    col_12(class = "filter-section",
           fluidRow(class='top-kpis',
                    col_3(class = "agent_kpis",
                          custom_value_box(title = "Outsource Amount",
                                           value = withLoader(textOutput(ns('outsource_amount')),"html","loader4",proxy.height = 20),
                                           trend =  "",
                                           trend_text="",
                                           trend_icon = "",
                                           main_icon ="coins"
                                           )
                    ),
                    col_3(class = "agent_kpis",
                          custom_value_box(title = "Recovered Amount",
                                           value = withLoader(textOutput(ns('recovered_Amount')),"html","loader4",proxy.height = 20),
                                           trend =  "",
                                           trend_text="",
                                           trend_icon = "",
                                           main_icon ="coins"
                          )
                    ),
                    col_3(class = "agent_kpis",
                          custom_value_box(title = "Recovered Cases",
                                           value = withLoader(textOutput(ns('recovered_Count')),"html","loader4",proxy.height = 20),
                                           trend =  "",
                                           trend_text="",
                                           trend_icon = "",
                                           main_icon ="coins"
                          )
                    ),
                    col_3(class = "agent_kpis",
                          custom_value_box(title = "Recovery Rate",
                                           value = withLoader(textOutput(ns('recovery_Rate')),"html","loader4",proxy.height = 20),
                                           trend =  "",
                                           trend_text="",
                                           trend_icon = "",
                                           main_icon ="coins"
                                           )
                          )
                    ),
           # collection metrics
           fluidRow(
             col_5(
               tabsetPanel(
                 type = 'pills',
                 tabPanel(title = "Amount Recovered",
                          div(class="collection_trend",
                              withLoader(echarts4rOutput(ns('agent_trend')),"html",loader = "loader4",proxy.height = '120px')
                          )
                 ),
                 #tabPanel(title = "Recovery Rate",
                 #        div(class="collection_trend",echarts4rOutput(ns('agent_trend_recovery_rate')))
                 #       ),
                 tabPanel(title = "Loan Series",
                          div(class="collection_trend",
                              withLoader(DTOutput(ns('loan_series_performance')),"html","loader4",proxy.height = '100px')
                          ) )
               ) %>% tagAppendAttributes(class="collection_trendui"),
               box(title = "Recovery breakdown",status = "success",solidHeader = T,width = 12,class="collectionbreakdown",
                   withLoader(uiOutput(ns('collection_breakdown')),"html","loader4",proxy.height = "100px")
                   # col_4(class = "portfolio",portfolio_bd(value = "5",rate = "",title = "My Team Ranking"))
               )),
             col_5(class="midle_panel",
                   # box(title = "Customer Feedback",status = "success",solidHeader = T,width = 12,class="agent-feedback",
                   #     tableOutput(ns("agents_feedback1"))
                   # ),
                   col_12(class = "agent-feedback",
                          tabsetPanel(
                            type = 'pills',
                            tabPanel(title = "My Team Ranking",
                                     div(class="team_rank",withLoader(reactableOutput(ns("performance_table")),"html","loader4",proxy.height = "150px"))
                            ),
                            tabPanel(title = "Customer Feedback",
                                     div(class = 'team_rank',withLoader(tableOutput(ns("agents_feedback1")),"html","loader4",proxy.height = "150px"))
                            )
                          )),
                   box(class="recovery_summary_slippage",title = "Slippages",
                       status = "success",width = 4,solidHeader = T,height = "200px",
                       tagList(
                         withLoader(uiOutput(ns('slipages_out')),"html","loader4",proxy.height = "80px")

                       )
                   ),
                   box(class="recovery_summary_slippage",title = "First-Call Resolution",
                       status = "success",width = 4,solidHeader = T,height = "200px",
                       tagList(
                         withLoader(uiOutput(ns('fcr_output')),"html","loader4",proxy.height = "80px")
                       )),
                   box(class="recovery_summary_slippage",title = "PTP Rate",
                       status = "success",width = 4,solidHeader = T,height = "200px",
                       tagList(
                         withLoader(uiOutput(ns('ptp_out')),"html","loader4",proxy.height = "80px")
                       ))

             ),
             col_2(class = "collection_activity-agents",
                   tags$div(
                     class = glue("col-sm-12"),
                     tags$div(
                       id = id,
                       class = "info-box",
                       tags$div(
                         class = "info-box-content",
                         tags$div(class = "infobox-title-custom", "Contacted Cases"),
                         tags$div(class = "infobox-value-custom",withLoader(textOutput(ns('contacted_cases')),type = 'html',loader = 'loader4',proxy.height = "50px")),
                         tags$div(class = "caretstats","")
                       )
                     )
                   ),
                   tags$div(
                     class = glue("col-sm-12"),
                     tags$div(
                       id = id,
                       class = "info-box",
                       tags$div(
                         class = "info-box-content",
                         tags$div(class = "infobox-title-custom","Contact Conversion Rate" ),
                         tags$div(class = "infobox-value-custom",withLoader(textOutput(ns('contact_conversion')),type = 'html',loader = 'loader4',proxy.height = "50px")),
                         tags$div(class = "caretstats","")
                       )
                     )
                   ),
                   # withLoader(uiOutput(ns("contacted_cases")),"html","loader4",proxy.height = "100px"),
                   col_12(class="collection_activity",
                          collection_time_UI(withLoader(textOutput(ns('collection_days')),"html","loader4",proxy.height = "90px"), "Average Recovery Time(Days)")

                   )
             )
           )
    )
  )
}

#' @noRd
#' @importFrom purrr map2
agentPerformanceDashboard_Server <- function(id,authentication,w) {
  moduleServer(id,function(input, output, session) {
    ns <- session$ns
    auth_res <- authentication()
    debt_coll = "Eva"
    outstanding_file <- reactive({
      tbl(db_con,'t_Outstanding') %>%
        filter(Debt_Collector == debt_coll)
    })
    allocation_file <- tbl(db_con,'t_Allocation') %>% distinct(AllocationDate) %>% mutate(Year = substr(as.character(AllocationDate),1,4)) %>% distinct(Year) %>% pull(Year)

    key_kpis <- reactive({
      req(input$product)
      get_key_kpis(input$agents,year_val = input$report_year,month_val = which(input$months == month.abb),product = input$product,team_val = gsub("\\s|s$", "", input$category))
    })

    agents <- reactive({
      req(input$category)
      find_agent_listing(input$category,auth_res)
    })
    teams <- reactive({find_teams(auth_res)})
    # dynamicaly update search inputs ================
    updatePickerInput(session,inputId = 'category',choices = teams(),selected = teams()[1])

   observe({
     # category

     # Agents
     updatePickerInput(session,inputId = 'agents',choices = agents())
     # channel
     updatePickerInput(session,inputId = 'product',choices = c("All",outstanding_file() %>% pull(Channel) %>% unique()),selected = "All")
     # year
     years = allocation_file
     updateNumericInput(session,"report_year",value = max(years),min = min(years),max = max(years),step = 1)

   })
    #render outsource amount =====
    output$outsource_amount <- renderText({
      paste("Ksh",format( key_kpis()$Outsource_Amount,big.mark=","))
    })
    #render outsource amount =====
    output$recovered_Amount <- renderText({
      paste("Ksh",format( key_kpis()$Recovered_Amount,big.mark=","))
    })
    #render outsource amount =====
    output$recovered_Count <- renderText({
      format( key_kpis()$Recovered_Count,big.mark=",")
    })
    #render outsource amount =====
    output$recovery_Rate <- renderText({
      paste0( key_kpis()$Recovery_Rate,"%")
    })
    # render key kpis ==============
    # output$key_kpis_UI <- renderUI({
    #   req(input$product)
    #   top_kpis_UI(key_kpis())
    # })
    # render daily collection chart ===========
    output$agent_trend <- renderEcharts4r({
      req(input$product)
      listing_data <- recovery_listing(input$agents,input$product,
                                       input$report_year,
                                       which(input$months == month.abb),
                                       team_val = input$category)
      # print(listing_data)
      listing_data %>%
        collect() %>%
        e_chart(RepaymentDate) %>%
        e_line(`Amount Recovered`,smooth=T) %>%
        e_tooltip(trigger = "item")%>%
        e_dims(height = "270px") %>%
        e_axis_labels("Day","Daily Amount Recovered") %>%
        e_mark_point(data = max_) %>%
        e_mark_point(data = min_)
    })
    # render performance by loan series ==============
    output$loan_series_performance <- renderDT(
      retrieve_agent_summary(channel_selected = input$product,
                             debt_collector = input$agents,
                             year_val = input$report_year,
                             month_val = which(input$months == month.abb),
                             team_val = gsub("\\s|s$", "", input$category)),
      options = list(scrollX = TRUE)
    )
    # render agent performance ranking table ==========
    output$performance_table <- renderReactable({
      req(input$product)
      table_df <- performance_table(
        input$product,
        input$report_year,
        which(input$months == month.abb),
        gsub("\\s|s$", "", input$category))
      show_performance_ranks(table_df)
    })
    # render feedback data =========
    output$agents_feedback1 <- function(){
      req(input$product)
      feedbac_data(channel_selected = input$product,
                   debt_collector = input$agents,
                   year_val = input$report_year,
                   month_val = which(input$months == month.abb),
                   team_val = gsub("\\s|s$", "", input$category)) %>%
        mutate(
          "Outsourced Amount('000)" = round(Outsourced_Amount/1000),
          "Amount Recovered('000)" = round(Amount_Recovered/1000),
        ) %>%
        # rename(
        #   'Recovery Rate' = RecoveryRate
        # ) %>%
        select(-Outsourced_Amount,-Amount_Recovered,-Count,-RecoveryRate) %>%
        collect() %>%
        kable(class= "feedback_table") %>%
        kable_styling()
    }
    # render contacted cases ===========
   output$contacted_cases <- renderText({
     tryCatch(
     expr = {

       data <- feedbac_data(input$product,input$agents,input$report_year,
                            which(input$months == month.abb),gsub("\\s|s$", "", input$category))%>%
         collect() %>%
         adorn_totals('row') %>%
         mutate(RecoveryRate = round(Amount_Recovered /Outsourced_Amount,2)*100 ) %>%
         select(Count,RecoveryRate) %>%
         tail(1) %>%
         as.list()
       format(data$Count,big.mark = ",")

     },
       error = function(e){

       },
       warning = function(w){

       },
       finally = {

       }
     )
   })
   # render cotact conversion rate cases ===========
   output$contact_conversion <- renderText({
     tryCatch(
     expr = {
       data <- feedbac_data(input$product,input$agents,input$report_year,
                            which(input$months == month.abb),gsub("\\s|s$", "", input$category))%>%
         collect() %>%
         adorn_totals('row') %>%
         mutate(RecoveryRate = round(Amount_Recovered /Outsourced_Amount,2)*100 ) %>%
         select(Count,RecoveryRate) %>%
         tail(1) %>%
         as.list()
       paste0(ifelse(is.na(data$RecoveryRate),0,data$RecoveryRate),"%")
     },
       error = function(e){

       },
       warning = function(w){

       },
       finally = {

       }
     )
   })
    # render collection breakdown ============
    output$collection_breakdown <- renderUI({
      req(input$product)
      key_kpis() %>%
        # select(Partially_Paid_Amount,Fully_Paid_Amount) %>%
        # as.list() %>%
        map2(.,names(.),.f=function(x,y){
          kpis = c('Partially_Paid_Amount','Fully_Paid_Amount')
          if(y %in% kpis){
            index <- which(y == kpis)
            col_6(class = paste("portfolio",ifelse(index==2,"watch","")),
                  portfolio_bd(value = paste("Ksh",format(x,big.mark=",")),
                               rate = "",
                               title = gsub("_"," ",y)))
          }

        })

    })
    # render fcr ===========
    output$fcr_output <- renderUI({
      tryCatch(
      expr = {

        req(input$product)
        fcr_stats <- tbl(db_con,'t_FCR') %>%
          filter(
            Channel == !!input$product,
            Debt_Collector == !!input$agents,
            year(as.Date(ReportDate)) == !!input$report_year,
            month(as.Date(ReportDate)) == !!which(input$months == month.abb),
            as.Date(ReportDate) == max(as.Date(ReportDate))
          ) %>%
          collect() %>%
          as.list()

        descriptionBlock(
          number = paste0("Ksh ",format(fcr_stats$CollectedAmount,big.mark = ",")),
          header = format(fcr_stats$Count,big.mark = ","),
          text = paste0(round(fcr_stats$RecoveryRate),"%"),
          rightBorder = F,
          marginBottom = FALSE
        )

      },
        error = function(e){

        },
        warning = function(w){

        },
        finally = {

        }
      )
    })
    # render slipages =============
    output$slipages_out <- renderUI({
      tryCatch(
      expr = {

        req(input$product)
        slippages <- tbl(db_con,'t_Slipages') %>%
          filter(
            Channel == !!input$product,
            Debt_Collector == !!input$agents,
            year(as.Date(ReportDate)) == !!input$report_year,
            month(as.Date(ReportDate)) == !!which(input$months == month.abb),
            as.Date(ReportDate) == max(as.Date(ReportDate))
          ) %>%
          # head(1) %>%
          collect() %>%
          as.list()

        descriptionBlock(
          number = paste0("Ksh ",format(slippages$Outstanding,big.mark = ",")),
          header = format(slippages$Count,big.mark = ","),
          text = paste0(round(slippages$SlipagePropotion*100,2),"%"),
          rightBorder = F,
          marginBottom = FALSE
        )

      },
        error = function(e){

        },
        warning = function(w){

        },
        finally = {

        }
      )
    })
    # render ptp rate =============
    output$ptp_out <- renderUI({
      tryCatch(
      expr = {
        req(input$product)
        ptp_date <- tbl(db_con,'t_PTPRateSummary') %>%
          filter(
            Channel == !!input$product,
            Debt_Collector == !!debt_collector,
            year(as.Date(ReportDate)) == !!input$report_year,
            month(as.Date(ReportDate)) == !!which(input$months == month.abb),
            as.Date(ReportDate) == max(as.Date(ReportDate))
          ) %>%
          # head(1) %>%
          collect() %>%
          as.list()


        descriptionBlock(
          number = paste0("Ksh ",format(ptp_date$CurrentBalance,big.mark = ",")),
          header = format(ptp_date$Count,big.mark = ","),
          text = paste0(round(ptp_date$PTPRate*100,2),"%"),
          rightBorder = F,
          marginBottom = FALSE
        )
      },
        error = function(e){

        },
        warning = function(w){

        },
        finally = {

        }
      )
    })
    # collection days  ==========
    output$collection_days <- renderText({
      tryCatch(
      expr = {
        req(input$product)
        collection_speed <- tbl(db_con,'t_CollectionTime')%>%
          filter(
            Debt_Collector == !!input$agents,
            year(as.Date(ReportDate)) == !!input$report_year,
            month(as.Date(ReportDate)) == !!which(input$months == month.abb),
            as.Date(ReportDate) == max(as.Date(ReportDate))
          ) %>%
          collect() %>%
          adorn_totals() %>%
          mutate(Channel = case_when(Channel == "-" ~ "All",
                                     TRUE ~ Channel),
                 CollectionTime = case_when(CollectionTime = Channel == 'All'~ floor(CollectionTime/2),
                                            TRUE ~ CollectionTime)) %>%
          filter(Channel == !!input$product)
        collection_speed %>% pull(CollectionTime)

      },
        error = function(e){

        },
        warning = function(w){

        },
        finally = {

        }
      )
    })
    # w$hide()
  }
  )
}

# copy to main.R in the box section
# app/view/mod_agentPerformanceDashboard

