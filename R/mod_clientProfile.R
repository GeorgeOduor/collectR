
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



#' @noRd
clientProfile_UI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(class='profile-controls top-kpis',
             ),
    fluidRow(
      col_3(class="search",
            box(title = HTML(paste(icon("search"),"Search")),status = "success",solidHeader = T,class="search-box",
                height = "466px",width = 12,
                selectizeInput(ns("customer_general_search"),"Find Customer",
                               choices=NULL,
                               options = list(
                                 create = FALSE,multiple=T,
                                 placeholder = "Search",
                                 maxItems = '100',
                                 onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}"),
                                 onType = I("function (str) {if (str === \"\") {this.close();}}")
                               )),
                div(class="divider","More Filters"),
                div(class = "more_filters_ui",
                  div(class="more_filters_ui-inner",
                        selectInput(ns("channel"),"Channel",choices = c("Mfanisi Safaricom","Mfanisi Airtel"),multiple = T,selectize = T),
                        selectInput(ns("contact-status"),"Contact Status",choices = list(
                          "Un Contacted" = c("PTP","Non comital","...etc"),
                          "Contacted"
                        ),multiple = T,selectize = T),
                        selectInput(ns("payment_status"),"Payment Status",choices = c("Paid","Un paid"),multiple = T,selectize = T),
                        sliderTextInput(inputId = ns("loanseries"),label = "Loan Series:", choices = 1:100,selected = c(10,56)),
                        sliderTextInput(inputId = ns("arrears_days"),label = "Arrears Days:", choices = 1:100,selected = c(10,56)),
                        numericRangeInput(ns("arrears_amount"),"Arrears Amount",value = c(200,10000),separator = "to",step = 1,min = 0,max = 80000),
                        dateRangeInput(ns("allocation_date"),
                                       label = "Allocation Date",
                                       start  = as.Date(rollback(Sys.Date())),
                                       end  = as.Date(ceiling_date(Sys.Date(),"month")-1),
                                       autoclose = T)
                )),
                col_6(class="nav-button",
                      actionBttn(ns("previous_cust"),"Previous",icon("arrow-left"),style = "material-flat",status = "success",size = "xs")
                      ),
                col_6(class="nav-button",
                      actionBttn(ns("previous_cust"),"Next",icon("arrow-right"),style = "material-flat",status = "success",size = "xs")
                      )
                )
            ),
      col_9(class='profile-details',
          box(title = "Debtor KYC",status = "success",solidHeader = T,class="kyc-box",height = "99px",width = 10,
              col_1(class="avatar",
                tags$img(class="client-avatar",src="static/images/logo.png")
              ),
              col_11(
                tagList(
                  col_6(
                    col_4(class = "left-client_particulars", "Client Name:"),
                    col_8(class = "right-client_particulars-detail pull-right", "George Oduor Wamaya:"),
                    col_4(class = "left-client_particulars", "Gender:"),
                    col_8(class = "right-client_particulars-detail pull-right", "Male"),
                    col_4(class = "left-client_particulars", "Age:"),
                    col_8(class = "right-client_particulars-detail pull-right", "21")
                  ),
                  col_6(
                    class = "customer_detail",
                    col_7(class = "left-client_particulars", "Mobile Number:"),
                    col_5(class = "right-client_particulars-detail pull-right", a(href = "tel:254711894704", "254711894704")),
                    col_7(class = "left-client_particulars", "Account Number:"),
                    col_5(class = "right-client_particulars-detail pull-right", "001802000023"),
                    col_7(class = "left-client_particulars", "Loan Series:"),
                    col_5(class = "right-client_particulars-detail pull-right", "1")
                  )
              )
              )
          ),
          box(title = "Contact",status = "success",solidHeader = T,class="call-box",height = "99px",width = 2,
              actionBttn(ns('Call'),label = "Call",icon = icon("phone"),style = "material-flat",status="success",size = "xs"),
              actionBttn(ns('sms_send'),label = "Send message",icon = icon("sms"),style = "material-flat",status="success",size = "xs")
          ),
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

#' @noRd
clientProfile_Server <- function(id) {
  moduleServer(id,function(input, output, session) {
      ns <- session$ns

      updateSelectizeInput(session, ("customer_general_search"),
                           choices = c(rep(paste0(LETTERS,sample(LETTERS,26)),100)), server = TRUE)
      # table loanlisting
      output$loan_listing <- function(){
        tibble(
          "Loan Series" = 1:34,
          'Loan Amount' = round(rnorm(34, 5000, 20),2),
          "Disbursement Date" = as.character(Sys.Date() + 1:34),
          "Account CloseDate" = as.character(Sys.Date() + 1:34),
          "Paid After" = round(runif(34, 0, 90))
        ) %>%
          kable(digits = 2,align = "c",format.args = list(big.mark=",")) %>%
          kable_styling(font_size = 12,full_width = F,position = "center") %>%
          scroll_box(height = "178px")

      }
      # contact progress schedule
      output$call_feed <- renderReactable({
        df <- tibble(
          ContactedBy = "George Oduor",
          ContactedOn = "20/02/2023",
          Feedback = "Non Commital",
          Exactverbatim = "I will py by end month when i get money but now am not well financialy",
          NextContactDate = "25/03/2023"
        )
        rbind(df,df,df,df,df,df,df,df,df,df,df,df) %>%
          reactable(
            striped = TRUE,
            compact = TRUE,
            sortable = T,
            resizable = T,
            class = "feedbacktables text-focus-in",
            # onClick = "expand",
            defaultPageSize = 6
          )
      })
      output$loan_details <- renderUI({

        # ns <- session$ns
          print("Here")

        switch (input$detail,
                "Loan Details" = {
                  tagList(
                    custom_panel(title="Loan Amount",value="45,900",width = 4,caretstats = "",id = "client_profile"),
                    custom_panel(title="Disbursement Date",value="12th Jan 2020",width = 4,caretstats = "",id = "client_profile"),
                    custom_panel(title="Loan Status",value="Open",width = 4,caretstats = "",id = "client_profile"),
                    custom_panel(title="Arrears Amount",value="45,900",width = 4,caretstats = "",id = "client_profile"),
                    custom_panel(title="Installment Due Date",value="12th Feb 2020",width = 4,caretstats = "",id = "client_profile"),
                    custom_panel(title="Recent Contact Date",value="15th Dec 2022",width = 4,caretstats = "Contact type:SMS"),
                    custom_panel(title="Arrears Days",value="4 days",width = 4,caretstats = "",id = "client_profile"),
                    custom_panel(title="Maximum Arrears Days",value="400 days",width = 4,caretstats = "",id = "client_profile"),
                    custom_panel(title="Recent Contact Response",value="Undelivered",width = 4,caretstats = "",id = "client_profile"),
                  )
                },
                "Repayment Behaviour" = {
                  tagList(
                    box(title = "Previous Loans History",width = 6,solidHeader = T,status = "success",
                        col_12(
                          col_6(p(class="loandetails",span(class="loandetails_title left text-focus-in","Number of loans :"),span(class="loandetails_value text-focus-in pull-right"," 10"))),
                          col_6(p(class="loandetails",span(class="loandetails_title right text-focus-in","Early Payments :"),span(class="loandetails_value text-focus-in pull-right"," 10"))),
                          col_6(p(class="loandetails",span(class="loandetails_title left text-focus-in","Repayment Days :"),span(class="loandetails_value text-focus-in pull-right","32"))),
                          col_6(p(class="loandetails",span(class="loandetails_title right text-focus-in","Average installments :"),span(class="loandetails_value text-focus-in pull-right"," 2")))
                          ),
                        col_12(div(class="divider","Loan Listing")),
                        col_12(class="loanlisting",
                               tableOutput(ns("loan_listing"))
                               )
                        ),
                    box(title = "Repayment Trend",width = 6,solidHeader = T,status = "success",
                        )
                  )
                },
                "Previous Collection Activity"={
                  div(class="collection_activity",
                      tabsetPanel(type = "pills",
                                  tabPanel(title = "Contact feedback/response",
                                           reactableOutput(ns("call_feed"))),
                                  tabPanel(title = "SMS Sent",
                                           col_2(class="controls_stats",
                                                 div(id="controls",
                                                     selectInput(ns("sms_groups"),label="Senders",
                                                                 choices = c("Call Agent",
                                                                             "Mfanisi System"),
                                                                 width = "100%")),
                                                 div(class="stats",
                                                 )
                                           ),
                                           col_10(class="listing",
                                                  shinipsum::random_DT(6,4,type = "random"))
                                  ),
                                  tabPanel(title = "Collection Team Progress",
                                           box(title="Allocation timeline",width=9,solidHeader =T,status="success"),
                                           box(title="Action points",width=3,solidHeader =T,status="success",
                                               actionBttn(ns("recal"),"Recal",icon = icon("undo"),style = "material-flat",size = "xs",block = T,color = "danger"),
                                               hr(),
                                               actionBttn(ns("realocate"),"Realocate",icon = icon("redo"),style = "material-flat",size = "xs",block = T,color = "success"),
                                           )
                                  )
                      )

                  )
                }
                )

      })


    }
  )
}

# copy to main.R in the box section
# app/view/mod_clientProfile

