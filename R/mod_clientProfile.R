
#' @importFrom shiny tabsetPanel tabPanel tableOutput renderTable renderUI uiOutput dateRangeInput selectInput updateSelectizeInput HTML fluidRow bootstrapPage  moduleServer selectizeInput  NS tags  h1  div hr  icon  tagList p a span
#' @importFrom shinyWidgets sliderTextInput actionBttn numericRangeInput radioGroupButtons
#' @importFrom shinydashboard box
#' @importFrom lubridate rollback ceiling_date
#' @importFrom kableExtra kable kable_styling scroll_box
#' @importFrom tibble tibble
#' @importFrom stats rnorm runif
#' @importFrom DT renderDT DTOutput
#' @importFrom dplyr `%>%`
#' @importFrom reactable reactable renderReactable reactableOutput

#' @import shinyThings

#' @noRd
clientProfile_UI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(class='profile-controls top-kpis'),
    fluidRow(
      col_3(class="search",
            box(title = HTML(paste(icon("search"),"Search")),status = "success",solidHeader = T,class="search-box",
                height = "466px",width = 12,
                class="filters",
                fluidRow(class="gen-filters",
                         col_4(selectizeInput(
                           ns("search_by"),"Search By",
                           choices = c("NationalID", "Mobile", "AccountID", "Client Name"),
                           multiple = F
                         )),
                         col_8(selectizeInput(ns("customer_general_search"),"Find Customer",
                                              choices=NULL,
                                              options = list(
                                                create = FALSE,multiple=T,
                                                placeholder = "Search",
                                                maxItems = '100',
                                                onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}"),
                                                onType = I("function (str) {if (str === \"\") {this.close();}}")
                                              ))
                         )),

                fluidRow(class='filters-inner',
                         div(class="col-sm-12 divider","More Filters"),
                         col_12(class = "more_filters_ui",
                                # uiOutput(ns('more_filters'))
                                div(class="more_filters_ui-inner",
                                    selectInput(ns('payment_status'),"Loan Payment Status",
                                                choices = c('No_Payment','Partially_Paid','Fully_Paid'),multiple = F),
                                    selectInput(ns("channel"),"Channel",choices = NULL,multiple = T,selectize = T),
                                    # selectInput(ns("contact_status"),"Contact Status",choices = list("Un Contacted" = c("PTP","Non comital","...etc"),"Contacted"),multiple = T,selectize = T),
                                    sliderInput(inputId = ns("loanseries"),label = "Loan Series:",
                                                min = 1,max = 1,step = 1,value = c(1,1)),
                                    sliderInput(inputId = ns("arrears_days"),label = "Overdue Days:",
                                                min = 1,max = 1,step = 1,value = c(1,1)
                                                ),
                                    numericRangeInput(ns("arrears_amount"),"Loan Balance",
                                                      value = c(1,1),
                                                      separator = "to",step = 1),
                                    dateRangeInput(ns("allocation_date"),label = "Outsource Date",
                                                   autoclose = T)
                                )

                           )
                         ),
                fluidRow(class='filters-inner',
                         col_12(
                          uiOutput(ns('staged'))
                          )
                         )
                )
            ),
      col_9(class='profile-details',
            uiOutput(ns('client_kyc')),
          # box(title = "Debtor KYC",status = "success",solidHeader = T,class="kyc-box",height = "99px",width = 10,
          #     col_1(class="avatar",
          #       tags$img(class="client-avatar",src="static/images/logo.png")
          #     ),
          #
          # ),

          col_12(class="navigate_details",
                 radioGroupButtons(
                   inputId = ns("detail"),
                   label = "",
                   choices = c("Loan Details",
                               "Repayment Behaviour",
                               # "Account Statement",
                               "Previous Collection Activity"),
                   justified = FALSE,size = "xs",status = "success",
                 )
                 ),
          col_12(class='loan-details text-focus-in',
                 uiOutput(ns("loan_details"))
                 )
          )
      )
    )
}

#' @import shinyThings
#' @import stringr
#' @import readr
#' @import highcharter
#' @import shinyjs
#' @importFrom purrr pluck
#' @noRd
clientProfile_Server <- function(id,authentication) {
  moduleServer(id,function(input, output, session) {
      ns <- session$ns
      auth_res <- authentication()
      # data used ======
      user = reactive({"Eva"})
      loanbk = tbl(db_con,'t_LoanBook')
      repayments = tbl(db_con,'t_Repayment')
      feedback_df = tbl(db_con,'t_CustomerFeedBack')
      datastaged <- tbl(db_con, "t_Outstanding") %>%
        filter(Debt_Collector == "Eva") %>%
        filter(Repayment_Status == !!input$payment_status)
      datastaged_sum <- datastaged %>%
        summarise(
          MaxLoanSeries      = max(as.numeric(LoanSeries), na.rm = T),
          MinLoanSeries      = min(as.numeric(LoanSeries), na.rm = T),
          MinArrearsDays     = min(as.numeric(ArrearsDays), na.rm = T),
          MaxArrearsDays     = max(as.numeric(ArrearsDays), na.rm = T),
          MaxLoanBalance     = max(as.numeric(CurrentBalance), na.rm = T),
          MinLoanBalance     = min(as.numeric(CurrentBalance), na.rm = T),
          OutsourceDateStart = min(as.Date(Outsource_Date), na.rm = T),
          OutsourceDateEnd   = max(as.Date(Outsource_Date), na.rm = T)
        )
      observe({
        library(tictoc);tic()
        # customer general search
        updateSelectizeInput(session, ("customer_general_search"),
                             choices =choices(input$search_by,auth_res),
                             server = TRUE)
        # channel
        updateSelectInput(session, "channel",
                          choices =datastaged %>% pull(Channel) %>% unique()
                          )
        # loan series update
        updateSliderInput(session,'loanseries',
                          min = pull(datastaged_sum,MinLoanSeries),
                          max = pull(datastaged_sum,MaxLoanSeries),
                          value = c(pull(datastaged_sum,MinLoanSeries),pull(datastaged_sum,MaxLoanSeries))
                          )
        # arrears_days update
        updateSliderInput(session,'arrears_days',
                          min = pull(datastaged_sum,MinArrearsDays),
                          max = pull(datastaged_sum,MaxArrearsDays),
                          value = c(pull(datastaged_sum,MinArrearsDays),pull(datastaged_sum,MaxArrearsDays))
                          )
        #arrears_amount update
        updateNumericRangeInput(session = session,inputId = "arrears_amount",
          value = c(pull(datastaged_sum,MinLoanBalance),pull(datastaged_sum,MaxLoanBalance))
        )
        # allocation_date update
        updateDateRangeInput(session,inputId = 'allocation_date',
                             start = pull(datastaged_sum,OutsourceDateStart),
                             end = pull(datastaged_sum,OutsourceDateEnd)
                             )
      toc()
      })
      conditions <- reactiveVal(NULL)
      staged <- reactiveVal(NULL)
      pages <- reactiveVal(NULL)

      observe({
        conditions <- list(
          'LoanSeries'       = list(min = input$loanseries[1], max = input$loanseries[2]),
          'ArrearsDays'      = list(min = input$arrears_days[1], max = input$arrears_days[2]),
          'CurrentBalance'   = list(min = input$arrears_amount[1],max = input$arrears_amount[2]),
          'Outsource_Date'   = list(min = input$allocation_date[1],max = input$allocation_date[2]),
          'Channel'          = input$channel,
          'Repayment_Status' = input$payment_status
        )
        conditions(conditions)
        staged <- staged_accounts(datastaged,
                                  conditions,
                                  auth_res,
                                  input$search_by,
                                  input$customer_general_search) %>%
          unique()
        staged(staged)
        page_indices <- pager("pager", length(staged[[1]]), 1)
        pages(isolate(page_indices()))

        # show staged summary===========
        output$staged <- renderUI({
          total <- length(staged[[1]])
          customers <-page_indices()
          p(class="stages_status",paste("Selected:",total),span(paste0("Contacted :",customers,"(",paste0(round(customers/total*100,1),"%"),")")))
        })
        # # render client_kyc ==============
        output$client_kyc <- renderUI({
          staged <- isolate(staged()[[1]])
          conditions <- isolate(conditions())
          customerid <-  staged[page_indices()]

          debtor_kyc_UI(ns,datastaged,
                        accid = customerid,
                        loanbk=loanbk,
                        feedback=feedback_df)
        })
        # # render loan repayments chart =====
        output$loanrepayment <- renderEcharts4r({
          staged <- isolate(staged())
          customerid <-  staged[[1]][page_indices()]
          loan_series <-  staged[[2]][page_indices()]
          loanseries <- datastaged %>%
            filter(AccountID == customerid) %>%
            select(AccountID,LoanSeries)

          repayments <- repayments %>%
            filter(AccountID == !!customerid,
                   LoanSeries == !!loan_series) %>%
            # left_join(loanseries,by=c('AccountID','LoanSeries')) %>%
            mutate(RepaymentDate=as.character(RepaymentDate)) %>%
            select(RepaymentDate,LoanSeries,Amount_Paid) %>%
            arrange(RepaymentDate) %>% collect()

          plot_repayment_trend(repayments = repayments)
        })
        #
        # # table loanlisting ===========
        output$loan_listing <- function(){
          staged      <- isolate(staged())
          customerid  <-  staged[[1]][page_indices()]
          loanbk      <- loanbk %>%
            filter(AccountID == customerid)
          loanbk1     <- loanbk %>%
            mutate(ArrearsDays = paste(ArrearsDays,'days')) %>%
            select(LoanSeries,`Loan Amount`=LoanAmount,
                   `Disbursement Date`=DisbursedOn,
                   `Account CloseDate`=AccountCloseDate,
                   `Paid After`=ArrearsDays) %>%
            mutate(
              `Paid After` = ifelse(is.na(`Account CloseDate`),'Outstanding',`Paid After`)
              ) %>%
            arrange(LoanSeries) %>% collect()

          paid_after <- which(parse_number(loanbk1$`Paid After`) != 0)

          loanbk1 %>%
            kable(digits = 2,align = "c",format.args = list(big.mark=",")) %>%
            kable_styling(font_size = 12,full_width = F,position = "center") %>%
            row_spec(row = paid_after,color = 'red') %>%
            scroll_box(height = "210px")


        }
        # # contact progress schedule ===========
        output$call_feed <- renderReactable({
          staged <- isolate(staged())
          customerid  <-  staged[[1]][page_indices()]
          loan_series <-  staged[[2]][page_indices()]
          df <- feedback_data(datastaged,customerid,loan_series)
          df %>%
            collect() %>%
            reactable(
              striped = TRUE,compact = TRUE,sortable = T,resizable = T,
              class = "feedbacktables text-focus-in",
              # onClick = "expand",
              defaultPageSize = 6
            )
        })
        # # SMS sent listing ==========
        output$sms_sent <- renderDT(
          sms_sent_data(accid = isolate(staged())[[1]][page_indices()],
                        loan_series = isolate(staged())[[2]][page_indices()])
        )
        # allocation listing
        output$allocation_history <- renderDT(
          allocation_timeline(accountid = isolate(staged())[[1]][page_indices()],
                              loan_series = isolate(staged())[[2]][page_indices()]),
          options = list(scrollX = TRUE,lengthMenu = seq(5,10,1)))
        # # render main loan details ===================
        output$loan_details <- renderUI({
          staged <- isolate(staged())
          conditions  <- isolate(conditions())
          customerid  <-  staged[[1]][page_indices()]
          loan_series <-  staged[[2]][page_indices()]
          # print(auth_res)
          switch (
            input$detail,
            "Loan Details" = {
              loan_details_UI(datastaged,
                              accid = customerid,
                              loanbk = loanbk,
                              feedback = feedback_df)
            },
            "Repayment Behaviour" = {
              rep_behavior_UI(ns,
                              datastaged,
                              accid = customerid,
                              loanbk = loanbk)
            },
            "Previous Collection Activity" = {
              prev_collection_activity_UI(ns,auth_res)
            }
          )

        })

      })

      # save feedback Call ui =================
      observeEvent(input$Call,{
        req(input$Call)
        username <- strsplit(input$Call,"_")[[1]]
        show_feedbackinput(ns,username)
      })
      # submit feeedback ===============
      observeEvent(input$submitfeedback,{
        feedback_df <- tibble(
          AccountID = ifelse(is.null(input$accountid), '', input$accountid),
          LoanSeries = ifelse(is.null(input$loan_series), '', input$loan_series),
          FeedBackClass = ifelse(is.null(input$feedbackclass), '', input$feedbackclass),
          PTPDate = ifelse(is.null(format(input$ptpdate, '%Y-%m-%d')), '', format(input$ptpdate, '%Y-%m-%d')),
          Occupation = ifelse(is.null(input$occupation), '', input$occupation),
          ExactVerbatim = ifelse(is.null(input$exactverbatim), '', input$exactverbatim),
          FeedBackDate = format(Sys.Date(), '%Y-%m-%d'),
          DebtCollector = isolate(user()),
        ) %>%
          mutate(
            Channel = ifelse(substr(input$accountid,1,4)=="0019","MfanisiSafaricom",
                             ifelse(substr(input$accountid,1,4) == "0018","MfanisiAirtel",""))
          )
        custom_db_actions(action = {
          dbWriteTable(db_con,"t_CustomerFeedBack",feedback_df,append=T)
          click(id = "pager-page_extra_next")
        },toast = T,successmessage = "Feedback Saved Successfulycli")

      })
      # recal case ==============
        observeEvent(input$recal,{
          delete_confirm(text = span(icon('thumbs-down'), "No"),
                         delete_id = "recal_client",
                         msg="Recal this client?",
                         ns = ns)
        })
      # excecute recal
      observeEvent(input$recal_client,{
        staged <- isolate(staged())
        customerid <-  isolate(staged[[1]][pages()])
        loan_series <-  isolate(staged[[2]][pages()])
        # set debtcollector in allocation table to null =============
        custom_db_actions(action = {
          customer_identity <- datastaged %>%
            filter(
              AccountID == customerid,
              LoanSeries == loan_series
            ) %>%
            pull(allocation_id)
          delete_clause(db_con,'t_Outstanding',where = 'allocation_id',
                        is = customer_identity,all = F,exec =T)
          update_clause(db_con,'t_Allocation',what = 'Debt_Collector',by = "",
                        where = 'allocation_id',is = customer_identity,
                        exec = T)
        },successmessage = "User Recalled successfuly!",toast=T)
      })
      # realocate case =======
      observeEvent(input$realocate,{
        showModal(ui = modalDialog(title = "Realocate",size = 's',
                                   div(class='feedback',
                                       p(class="","Current Collection Agent"),
                                       p(class="","Keysian"),
                                       pickerInput(inputId = ns('new_agent'),label = "to",
                                         choices = list(
                                           "ExternalCallAgents" = tbl(db_con,'t_Agents') %>% pull('name') ,
                                           "InternalCallAgents" = tbl(db_con,'t_Users') %>% pull('email')
                                           )
                                         ),
                                       actionBttn(ns('saverealocation'),"Save",icon = icon("save"),style = 'material-flat',color = 'success',size = 'xs',block = T)
                                       )
                                   )
                  )
      })



    }
  )
}

# copy to main.R in the box section
# app/view/mod_clientProfile

