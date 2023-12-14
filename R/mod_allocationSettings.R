#' @importFrom shiny fileInput plotOutput renderUI observe uiOutput bootstrapPage  moduleServer  NS  h1  div  icon  tagList p tabsetPanel tabPanel numericInput fluidRow hr span
#' @importFrom shinyWidgets prettyToggle pickerInput prettyRadioButtons actionBttn numericRangeInput sliderTextInput
#' @importFrom shinydashboard box
#' @importFrom purrr map
#' @importFrom dplyr `%>%`

#' @noRd
allocationSettings_UI <- function(id) {
  ns <- NS(id)
  div(class="setings-content",
    fluidRow(class = "title_row",
             p(class = "title_text text-focus-in mr-5","Debt Collection Outsource")
    ),
    col_12(class = "allocations-ui",
           tabsetPanel(id = "allocation-tabs",type = "pills",
             tabPanel(title = "Allocation Listing",
                      div(id = "filter_area",
                        header_ui(ns)
                      ),
                      col_10(class = "allocation_listing",
                             DTOutput(ns('allocation_file'))),
                      col_2(
                        div(class = "allocation_download")
                      ),
                      icon = icon("table")
                      ),
             tabPanel(title = "Performance Update",
                      fluidRow(class="debt-allocation filter-items2",
                               col_3(class = 'allocation_utils',
                                 tagAppendAttributes(class="method",pickerInput(
                                   inputId = ns('method'),
                                   label = "Outstanding File Input",
                                   choices = c("Upload","Core Banking Database"),
                                   choicesOpt = list(
                                     icon = c(icon("upload"),icon("database")))
                                 )),
                                 actionBttn(ns('excecute_UPDATES'),"Submit",icon = icon('arrow-right'),style = 'material-flat',color = 'success',size = 'xs')
                                 ),
                               uiOutput(ns('upload_ui')),
                               col_2(
                                 class = 'allocation_utils',dateInput(ns('report_date'),"Report Date",max = Sys.Date()),
                                     ),
                               ),
                      # fluidRow(class="debt-allocation",
                      #          col_6(class="available_cases",
                      #                p(class="section-head text-focus-in","Internal Call Agents"),
                      #                p(class="available-text text-focus-in","M-Fanisi Safaricom Cases:",span(class="cases-value pull-right","21")),
                      #                p(class="available-text text-focus-in","M-Fanisi Airtel Cases:",span(class="cases-value pull-right","21"))
                      #          ),
                      #          col_6(class="available_cases",
                      #                p(class="section-head text-focus-in","External Call Agents"),
                      #                p(class="available-text text-focus-in","M-Fanisi Safaricom Cases:",span(class="cases-value pull-right","212")),
                      #                p(class="available-text text-focus-in","M-Fanisi Airtel Cases:",span(class="cases-value pull-right","212")))
                      # ),
                      fluidRow(class = "debt-allocation",
                               tagList(
                                 col_3(
                                   dropdown(
                                     tagList(
                                         divider("Internal Debt Collectors"),
                                         div(class="settings",
                                             numericRangeInput(ns("arrears_days_internal"),"Arrears Days",value = c(1,90),min = 1,separator = " to ",step = 1),
                                             prettyRadioButtons(
                                               inputId = ns("existing_outsource"),
                                               label = "Existing outsource",
                                               choices = c("Keep", "Reshufle"),
                                               icon = icon("check"), bigger = F,
                                               status = "success",
                                               animation = "tada",inline = T
                                             )
                                         ),
                                         divider("External Debt Collectors"),
                                         div(class="settings",
                                             numericInput(ns("arrears_days_external"),"Minimum Arrears Days",min = 1,value = 91,step = 1)
                                         ),
                                         hr(),
                                         actionBttn(ns("save"),"Save",icon = icon('save'),style = "material-flat",size = "xs",color = "success")
                                     ),size = "xs",label = "Update Outsource settings"
                                     ,style = "material-flat", icon = icon("gears"),
                                     status = "success", width = "300px",up = F,
                                     animate = animateOptions(
                                       enter = shinyWidgets::animations$specials$rollIn,
                                       exit = shinyWidgets::animations$specials$rollOut
                                     )
                                   ),
                                 p(class="section-head","Collection Teams"),
                                 selectInput(
                                   inputId =ns("call_center_agents"),
                                   label = "Internal Debt Collectors",choices = NULL,selected = NULL,
                                   multiple = TRUE,
                                   # options = list(`actions-box` = TRUE,`data-divider`=T)
                                 ),
                                 pickerInput(
                                   inputId =ns("external_agents"),
                                   label = "External Debt Collectors",
                                   choices = NULL,
                                   options = list(`actions-box` = TRUE,`data-divider`=T), multiple = TRUE
                                 ),
                                 col_6(class="nav-button"),
                                 col_6(class="nav-button",
                                       # actionBttn(ns("outsource"),"Outsource",style = "material-flat",size = "xs",color = "success")
                                 )
                               ),
                               col_9(
                                 class = "segmentation",
                                 col_12(class = "segmentation_settings",
                                        tabsetPanel(
                                          type = 'pills',
                                          tabPanel(
                                            title = "Internal Agents",
                                            fluidRow(class = "allocations",
                                              allocation_distribution_widget(ns),
                                              tagList(
                                                col_12(class='allocation_method',
                                                       col_6("Debt Collector"),
                                                       col_3(class = "available_cases",span(class="value","Cases")),
                                                       col_3(class = "available_cases",span(class = "value","Outsource Amount"))

                                                ),
                                                uiOutput(ns("segmentation_ui"))
                                              )

                                            )
                                          ),
                                          tabPanel(
                                            title = "External Agents",
                                            allocation_distribution_widget(ns, F),
                                            div(
                                              class = "top-row",
                                              # allocation_items(ns, "Agency", "Propotion", "Amount", "Count", class = "top_col")
                                            ),
                                            uiOutput(ns("segmentation_u"))
                                          )
                                        )),

                                 col_12(
                                   class = "action-points",
                                   hr(class = "divider"),
                                   col_4(
                                     "Submit as final",
                                     prettyToggle(
                                       inputId = ns("submit-status"),
                                       label_on = "Yes!",
                                       icon_on = icon("check"),
                                       status_on = "success",
                                       status_off = "danger",
                                       label_off = "No..",
                                       animation = "rotate",
                                       inline = T,
                                       icon_off = icon("xmark")
                                     )
                                   ),
                                   col_8(col_6(
                                     actionBttn(
                                       ns("outsource"),
                                       "Outsource",
                                       icon = icon('save'),
                                       style = "material-flat",
                                       size = "xs",
                                       color = "success"
                                     )
                                   ),
                                   col_6(
                                     actionBttn(
                                       ns("share_outsource"),
                                       "Send",
                                       icon = icon('share'),
                                       style = "material-flat",
                                       size = "xs",
                                       color = "success"
                                     )
                                   ))
                                 )
                               ))

                               ),
                      icon = icon("gear")),
             tabPanel(title = "Reshuflle",icon = icon("random"),
                      col_3(class="recal-column",
                        divider("Recal Request"),
                        prettyRadioButtons(
                          inputId = ns("reshuffle_method"),
                          label = "Reshuffle Method",
                          choices = c("Requested", "Prepare"),
                          icon = icon("check"), bigger = F,
                          status = "success",
                          animation = "tada",inline = T
                        ),
                        uiOutput(ns("reshuffle_method_ui")),
                        pickerInput(
                          inputId =ns("external_agents"),
                          label = "New Agencies",
                          choices = c("Agency A","Agency B","Agency C"),
                          options = list(`actions-box` = TRUE,`data-divider`=T), multiple = TRUE
                        ),
                        actionBttn(ns("upload"),label = "Submit Upload",icon = icon("upload"),style = "material-flat",size = "xs",block = T,color = "success"),

                      )
                      )
           )
           )
  )
}

#' @noRd
#' @import openxlsx
#' @import feather
allocationSettings_Server <- function(id,authentication) {
  moduleServer(
    id,
    function(input, output, session) {

      ns = session$ns
      auth_res <- authentication()
      allocation_file <- tbl(db_con, 't_Allocation')

      key_kpis <- reactive({
        req(input$product)
        get_key_kpis(
          input$agents,
          year_val = input$report_year,
          month_val = which(input$months == month.abb),
          product = input$product,
          team_val = gsub("\\s|s$", "", input$category)
        )
      })

      agents <- reactive({
        req(input$category)
        find_agent_listing(input$category, auth_res)
      })
      teams <- reactive({
        find_teams(auth_res)
      })
      # dynamicaly update  call center agents listing ==========
      reportDate = input$report_date

      call_center_agents = tbl(db_con, 't_EndOfMonthCollectionTrend') %>%
        filter(RepaymentDate == reportDate,
               Team == 'InternalDebtCollector') %>%
        distinct(Debt_Collector)

      call_center_agents_all <<- tbl(db_con, 't_Users') %>%
        select(user_id = id,email, first_name, last_name, usergroup) %>%
        mutate(FullName = paste(first_name, last_name)) %>%
        inner_join(
          tbl(db_con, 't_UserGroups') %>%
            filter(name == 'DebtCollectionAgent'),
          by = c('usergroup' = 'id')
        )


      external_agents = tbl(db_con, 't_EndOfMonthCollectionTrend') %>%
        filter(RepaymentDate == reportDate,
               Team == 'ExternalDebtCollector') %>%
        distinct(Debt_Collector) %>%
        pull(Debt_Collector)

      updateSelectInput(
        session,
        inputId = 'call_center_agents',
        choices = setNames(
          nm = pull(call_center_agents_all, FullName),
          object = pull(call_center_agents_all, email)
        ),
        selected = pull(call_center_agents_all, email)
      )

      updatePickerInput(session,
                        inputId = 'external_agents',
                        choices = external_agents,
                        selected = external_agents)


      # dynamicaly update search inputs ================
      # category
      updatePickerInput(
        session,
        inputId = 'category',
        choices = teams(),
        selected = teams()[1]
      )

      # observe({
      #
      #   # Agents
      #   updatePickerInput(session,inputId = 'agents',choices = agents())
      #   # channel
      #   updatePickerInput(session,inputId = 'product',
      #                     choices = c("All",tbl(db_con,'t_Outstanding') %>% distinct(Channel) %>% pull(Channel) ),
      #                     selected = "All")
      #   # year
      #   years = allocation_file %>% distinct(AllocationDate) %>% mutate(Year = substr(as.character(AllocationDate),1,4)) %>% distinct(Year) %>% pull(Year)
      #   updateNumericInput(session,"report_year",value = max(years),min = min(years),max = max(years),step = 1)
      #
      # })

      output$reshuffle_method_ui <- renderUI({
        ns = session$ns
        ui <- switch (input$reshuffle_method,
                      "Requested" = {
                        div(class = "reshuffle_method_ui-inner",
                            fileInput(ns("recals"), "", multiple = T, accept = ".xlsx"))
                      },
                      "Prepare" = {
                        div(
                          class = "reshuffle_method_ui-inner",
                          pickerInput(
                            inputId = ns("channel"),
                            label = "Channel",
                            choices = c("Mfanisi Airtel", "Mfanisi Safaricom"),
                            options = list(`actions-box` = TRUE, `data-divider` = T),
                            multiple = TRUE
                          ),
                          sliderTextInput(
                            inputId = ns("arrearsDays"),
                            label = "Arrears Days",
                            choices = 1:100,
                            selected = c(10, 56)
                          ),
                          sliderTextInput(
                            inputId = ns("outsourcedays"),
                            label = "Days Since Outsource",
                            choices = 1:100,
                            selected = c(10, 56)
                          ),
                          numericInput(
                            ns("propotionpaid"),
                            label = "Propotion paid" ,
                            min = 0,
                            max = 1,
                            value = 0,
                            step = 1
                          ),

                        )
                      })
      })

      allocationItems <- function(ns,agent_name,agent_id,cases,amounts) {
        col_12(class='allocation_method',
               col_6(agent_name),
               col_3(class = "available_cases",
                     span(class="value",
                          numericInput(ns(glue("{agent_id}_count")),label = "",min = 1,value = cases,step = 1,width = "100%") %>%
                            tagAppendAttributes(class = "custom_numeric_input")
                     )),
               col_3(class = "available_cases",span(class = "value",amounts))

        )
      }

      output$segmentation_ui <- renderUI({

        ns = session$ns
        tagList(
          input$call_center_agents %>%
            map(.f = function(x){
              agent_info = call_center_agents_all %>%
                dplyr::filter(email == x) %>% collect()
              allocationItems(
                ns,
                agent_name = agent_info %>% pull(FullName),
                agent_id = agent_info %>% pull(user_id),
                cases = 20,
                amounts = 200
              )
            }) #%>%
            # list_to_li(class = "agents_list")
        )
      })
      # manual allocations
      observe({
        agents <- call_center_agents_all %>%
          dplyr::filter(email %in% !!input$call_center_agents)
        inputs <- lapply(agents %>% pull(user_id), function(x){
          input %>% pluck(x)
        })
        print(agents)
      })

      output$upload_ui <- renderUI({
        if (input$method == 'Upload') {
          col_2(
            fileInput(
              ns('upload_olb'),
              "Upload File",
              multiple = F,
              accept = c('.xlsx', '.csv', '.rdata', '.rds', '.feather')
            ) %>%
              tagAppendAttributes(class = "upload_button bounce-in-right")
          )
        }
      })
      # run daily update ==========
      observeEvent(input$excecute_UPDATES, {
        # read loanbook file ====
        # read_loanbook <- function(input_method=input$method,filepath =  input$upload_olb$datapath) {
        #
        #   switch (input_method,
        #           'Upload' = {
        #             # get file type
        #             file_type = unlist(strsplit(unlist(basename(filepath) %>% as.list()), "[.]"))[2]
        #             loanbook_file <-switch(file_type,
        #               'Rdata' = load(file = filepath),
        #               'xlsx' = read.xlsx(xlsxFile = filepath),
        #               'csv' = read.csv(file = filepath),
        #               'rds' = read_rds(file = filepath),
        #               'feather' = read_feather(path = filepath)
        #               )
        #             },
        #           "Core Banking Database" = {
        #             # read active closed loanbook from core banking
        #           })
        # }
        #
        # loanbook <- process_loan_data(read_loanbook(input$method,input$upload_olb$datapath),input$report_date,con=NULL) %>%
        #   suppressWarnings() %>% suppressMessages()
        # gt_current_status =====
        # collections update =================
        dfs <<-
          get_current_status(report_date = reportDate, writedb = T)
        check(db_con) %>% invisible()
        todays_collection = get_today_payments(db_con, report_date = get_today_payments, dbwrite = T)
        if (nrow(todays_collection) == 0) {
          stop("Error! at todays_collection",)
        }
        get_outstanding_file(db_con, report_date = reportDate, dbwrite = T)
        daily_trend(db_con, reportDate, T)
        # endmonth
        updateEndOfMonthCollection(db_con , reportDate, T) %>% invisible()
        # fcr statistic -----
        fcr_update(db_con, debt_collector, reportDate) %>% invisible()
        # ptp statistics ----------
        ptp_summary_update(report_date = reportDate)
        # slippages statistics ----------
        slippages(report_date = reportDate)
        # average collection days rate ------
        collectionDate(reportDate)
        # feedback summary ------------
        feedback_summary(reportDate)
        #  allocation
        if (!is_endMonth(reportDate)) {
          tryCatch(
            expr = {
              aloc <-
                AllocationEngine$new(
                  db_con = db_con,
                  id_col = 'National_ID',
                  report_date = rollback(Sys.Date())
                )
              dt_aloc <- aloc$get_allocation_file()
              # daily allocation for internal teams
              agents = tbl(db_con, 't_EndOfMonthCollectionTrend') %>%
                filter(RepaymentDate == reportDate,
                       Team == 'InternalDebtCollector') %>%
                distinct(Debt_Collector)
              aloc$internal_team_outsource_new(
                agents,
                dt = dt_aloc,
                outsource_method = "equal",
                test = T
              ) %>%
                mutate(AllocationDate = reportDate + 1,) %>%
                select(
                  National_ID,
                  Client_Name = AccountName,
                  MFanisi_Account = RepaymentAccountID,
                  AccountID,
                  Mobile_No = MobileNo,
                  Disbursement_Date = DisbursedOn,
                  Last_Installement_Date = InstallmentDueDate,
                  Outsourced_Amount = ClearBalance,
                  Days_Overdue = ArrearsDays,
                  Debt_Collector,
                  LoanSeries,
                  AllocationDate,
                  Team
                )

            },
            error = function(e) {
              message(paste("Daily allocation failed,", e))
            }
          )
        }
      })
      observeEvent(input$data, {
        # end month allocation file
        aloc <-
          AllocationEngine$new(
            db_con = db_con,
            id_col = 'National_ID',
            report_date = rollback(Sys.Date())
          )
        dt_aloc <- aloc$get_allocation_file(max_internal = 90)
        print("Done!")
      })


      # render allocation listing =================
      output$allocation_file <- renderDT(# req(input$report_year,input$months,input$agents,input$category,input$product),
        tryCatch(
          expr = {
            get_allocation_listing(
              year_val = input$report_year,
              month_val = which(input$months == month.abb),
              agent = input$agents,
              team = gsub("\\s|s$", "", input$category),
              product = input$product
            )
          },
          error = function(e) {
          },
          warning = function(w) {
          },
          finally = {
          }
        ),
        options = list(scrollX = TRUE))

    }
  )
}

# copy to main.R in the box section
# app/view/mod_allocationSettings

