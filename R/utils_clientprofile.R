
#' Apply Conditions to Filter Data
#'
#' This function applies conditions to filter a given dataset based on specific criteria.
#'
#' @param data A data frame containing the dataset to be filtered.
#' @param conditions A list specifying the filtering conditions.
#' @return A vector of account IDs that meet the specified conditions.
#' @noRd
more_filters <- function(dat,conditions,auth_res) {
    # check user permisions
    data <- dat
    if (!is.null(conditions$Repayment_Status)) {
        data <- data %>% filter(Repayment_Status %in% !!conditions$Repayment_Status,)
    }

    if (!is.null(conditions$Channel)) {
        data <- data %>% filter(Channel %in% !!conditions$Channel)
    }
    # mandatory columns
    data <- data %>%
        filter(
            between(Outsource_Date,!!conditions$Outsource_Date$min,!!conditions$Outsource_Date$max),
            between(LoanSeries, !!conditions$LoanSeries$min, !!conditions$LoanSeries$max),
            between(ArrearsDays, !!conditions$ArrearsDays$min, !!conditions$ArrearsDays$max),
            between(CurrentBalance, !!conditions$CurrentBalance$min, !!conditions$CurrentBalance$max)
        )
    data <- data %>%
        select(AccountID,LoanSeries) %>%
        collect() %>%
        as.list()

    return(data)
}

#' Get Distinct Choices for Search Term
#'
#' This function retrieves distinct values for a specific search term from the loan book table, based on user permissions.
#'
#' @param search_term A character specifying the search term ('NationalID', 'Mobile', 'AccountID', 'Client Name').
#' @param auth_res An object containing user authentication information.
#' @return A vector of distinct choices for the given search term.
#' @noRd
choices <- function(search_term,auth_res) {

    loanbk <- tbl(db_con,'t_Outstanding') %>%
        # filter(Debt_Collector == !!auth_res$email) %>%
        rename(
            MobileNo = Mobile_No,
            AccountName = Client_Name
        )

    if (!is.null(search_term)) {
        choice <- switch (
            search_term,
            'NationalID' = {
                loanbk %>% distinct(National_ID) %>% pull(National_ID)
            },
            'Mobile' = {
                loanbk %>% distinct(MobileNo) %>% pull(MobileNo)
            },
            'AccountID' = {
                loanbk %>% distinct(AccountID) %>% pull(AccountID)
            },
            'Client Name' = {
                loanbk %>% distinct(AccountName) %>% pull(AccountName)
            }
        )
        return(choice)
    }
}

#' Perform General Search
#'
#' This function performs a general search based on the given search term and values, considering user permissions.
#'
#' @param search_term A character specifying the search term ('NationalID', 'Mobile', 'AccountID', 'Client Name').
#' @param values A vector of values to search for within the specified search term.
#' @param auth_res An object containing user authentication information.
#' @return A vector of matching AccountIDs based on the search term and values.
#' @noRd
general_search <- function(dat,search_term,values,auth_res) {
    # check user permissions
    data <- switch (search_term,
                    'NationalID' = {
                        dat %>% filter(National_ID %in% c(values))
                    },
                    'Mobile' = {
                        dat %>% filter(Mobile_No %in% c(values))
                    },
                    'AccountID' = {
                        data %>% filter(AccountID  %in% c(values))
                    },
                    'Client Name' = {
                        dat %>% filter(Client_Name  %in% c(values))
                    }
                    ) %>%
        select(AccountID,LoanSeries) %>%
        distinct(AccountID,.keep_all = T) %>%
        collect() %>%
        as.list()

    return(data)


}

#' Retrieve Staged Accounts
#'
#' This function retrieves staged accounts based on conditions, user permissions, and search inputs.
#'
#' @param conditions A list of conditions to filter the accounts (used in agent_search).
#' @param auth_res An object containing user authentication information.
#' @param search_term A character specifying the search term ('NationalID', 'Mobile', 'AccountID', 'Client Name').
#' @param customer_general_search A vector of values to search for within the specified search term (used in general_search). Default is NULL.
#' @return A vector of staged accounts based on the conditions and search inputs.
#' @noRd
staged_accounts <- function(dat,conditions,auth_res,search_term,customer_general_search = NULL) {
    if (length(customer_general_search) > 0) {
        staged <- general_search(dat,search_term, customer_general_search,auth_res)
    } else{
        staged <- more_filters(dat,conditions, auth_res)
    }
    return(staged)
}

#' Agent Filters UI
#'
#' This function creates a user interface for agent filters.
#'
#' @param ns The namespace for the Shiny inputs.
#' @param agent The agent object.
#' @return A div element containing the agent filters user interface.
#' @export
agent_filters_UI <- function(ns,auth_res,conditions) {
    team = auth_res$group %in% c("DebtCollectionAgent","SuperAdmin")

    data <- tbl(db_con,'t_Outstanding') #%>%
    # filter(Debt_Collector == !!auth_res$email)
    outsource_dte <- data %>%
        summarise(
            MinOutsourceDate = min(as.Date(Outsource_Date),na.rm = T),
            MaxOutsourceDate = max(as.Date(Outsource_Date),na.rm = T)
        ) %>%
        collect() %>%
        as.list()

    data <- data %>%
        filter(
            # Repayment_Status %in% !!data %>% pull(Repayment_Status) %>% unique(),
            between(Outsource_Date, !!outsource_dte$MinOutsourceDate, !!outsource_dte$MaxOutsourceDate)
        )

    ls_arrears <- data %>%
        summarise(
            MaxLoanSeries = max(as.numeric(LoanSeries),na.rm = T),
            MinLoanSeries = min(as.numeric(LoanSeries),na.rm = T),
            MinArrearsDays = min(as.numeric(ArrearsDays),na.rm = T),
            MaxArrearsDays = max(as.numeric(ArrearsDays),na.rm = T),
            MaxLoanBalance = max(as.numeric(CurrentBalance),na.rm = T),
            MinLoanBalance = min(as.numeric(CurrentBalance),na.rm = T)
        ) %>% collect() %>% as.list()

    div(class="more_filters_ui-inner",
        radioGroupButtons(
            inputId = ns('payment_status'),
            label = "Loan Payment Status",
            choices = c("Outstanding","Paid"),
            size = "xs",status = "success",
            individual = T,
            justified = TRUE
        ),
        selectInput(ns("channel"),"Channel",choices = c("MfanisiSafaricom","MfanisiAirtel"),multiple = T,selectize = T),
        if (team) {
            tagList(
                selectInput(ns("contact_status"),"Contact Status",choices = list(
                    "Un Contacted" = c("PTP","Non comital","...etc"),
                    "Contacted"
                ),multiple = T,selectize = T)
                # selectInput(ns("payment_status"),"Payment Status",choices = c("Paid","Un paid"),multiple = T,selectize = T),
            )
        },
        sliderTextInput(inputId = ns("loanseries"),label = "Loan Series:",
                        choices = sort(unique(data$LoanSeries)),
                        selected = c(ls_arrears$MinLoanSeries,ls_arrears$MaxLoanSeries)),
        sliderTextInput(inputId = ns("arrears_days"),label = "Arrears Days:",
                        choices = sort(unique(data$ArrearsDays)),
                        selected = c(ls_arrears$MinArrearsDays,ls_arrears$MaxArrearsDays)),
        numericRangeInput(ns("arrears_amount"),"Loan Balance",value = c(ls_arrears$MinLoanBalance,ls_arrears$MaxLoanBalance),
                          separator = "to",step = 1,min = ls_arrears$MinLoanBalance,max = ls_arrears$MaxLoanBalance),
        if (team ) {
            dateRangeInput(ns("allocation_date"),label = "Outsource Date",
                           start  = outsource_dte$MinOutsourceDate,
                           end  = outsource_dte$MaxOutsourceDate,
                           autoclose = T)
        }
    )
}

#' Get KYC information for a debtor
#'
#' This function retrieves KYC (Know Your Customer) information for a debtor based on the provided \code{AccountID} and optional \code{LoanSeries}.
#'
#' @param db_con The database connection object.
#' @param accid The AccountID of the debtor.
#'
#' @return A tibble containing the KYC information for the debtor, including columns: \code{Client_Name}, \code{AccountID}, \code{LoanSeries}, and \code{Mobile_No}.
#' @noRd
#' loanbk <- tbl(db_con,"t_LoanBook") %>%
#' filter(AccountID == accid) %>%
#' select(AccountID,LoanSeries,MaxArrears)
#' customer_feedback <- tbl(db_con,'t_CustomerFeedBack')
debtor_info <- function(outstandingfile,accid,loanbk,feedback,type) {

    data <- outstandingfile %>%
        filter(AccountID == accid) %>%
        left_join(
            loanbk %>% select(AccountID,LoanSeries,MaxArrears) %>%
                filter(AccountID == accid),
            by = c("AccountID","LoanSeries")
            ) %>%
        left_join(
            feedback %>%
                filter(AccountID == accid,id == max(id)) %>% select(-Channel) %>%
                arrange(FeedBackDate),
            by = c("AccountID","LoanSeries")
        )

    df <- switch (type,
                  kyc = {
                      data %>%
                          select(Client_Name, AccountID, LoanSeries, Mobile_No) %>%
                          collect() %>%
                          as.list()
                  },
                  loan_details = {
                      data %>%
                          select(Outsourced_Amount,
                                 DisbursedOn,
                                 CurrentBalance,
                                 ArrearsDays,
                                 InstallmentDueDate,
                                 MaxArrears,
                                 FeedBackDate,
                                 ) %>%
                          collect() %>%
                          as.list()
                  }
    )
    return(df)
}
#' @importFrom shinydashboardPlus loadingState
debtor_kyc_UI <- function(ns,outstandingfile,accid,loanbk,feedback){
    tryCatch(
        expr = {
            kyc_data <- debtor_info(outstandingfile, accid, loanbk, feedback, type = 'kyc')
            tagList(box(title = "Debtor KYC",status = "success",solidHeader = T,class="kyc-box",height = "99px",width = 10,
                        col_1(class="avatar",
                              tags$img(class="client-avatar",src="static/images/logo.png")
                        ),
                        col_11(tagList(
                            col_6(
                                col_4(class = "left-client_particulars", "Client Name:"),
                                col_8(class = "right-client_particulars-detail pull-right", str_to_title(kyc_data$Client_Name)),
                                col_4(class = "left-client_particulars", "Gender:"),
                                col_8(class = "right-client_particulars-detail pull-right", NA),
                                col_4(class = "left-client_particulars", "Age:"),
                                col_8(class = "right-client_particulars-detail pull-right", NA)
                            ),
                            col_6(
                                class = "customer_detail",
                                col_7(class = "left-client_particulars", "Mobile Number:"),
                                col_5(class = "right-client_particulars-detail pull-right", kyc_data$Mobile_No),
                                col_7(class = "left-client_particulars", "Account Number:"),
                                col_5(class = "right-client_particulars-detail pull-right", kyc_data$AccountID),
                                col_7(class = "left-client_particulars", "Loan Series:"),
                                col_5(class = "right-client_particulars-detail pull-right", kyc_data$LoanSeries)
                            )
                        ))
            ),
            box(title = "Contact",status = "success",solidHeader = T,class="call-box",height = "99px",width = 2,
                HTML(shinyInput(
                    FUN = actionBttn,
                    class = 'action-button bttn bttn-material-flat bttn-xs bttn-default bttn-no-outline',
                    n = 1,
                    id = paste0(kyc_data$AccountID,"_",kyc_data$LoanSeries,"_",kyc_data$Mobile_No,"_",kyc_data$Client_Name,"_"),
                    icon = icon('phone'),
                    label = "Call",
                    status = 'success',
                    style = 'material-flat',
                    size = 'xs',
                    onclick = 'Shiny.setInputValue(\"clientprofile-Call\", this.id, {priority: \"event\"})'
                )),
                actionBttn(ns('sms_send'),label = "Send message",icon = icon("sms"),style = "material-flat",status="success",size = "xs"),
                pagerUI(ns("pager"), centered = FALSE,class = "pagercontainer",label_prev = "<<",">>")
            ))

        },
        error = function(e){
            print(e)
            loadingState()
        }
    )
}
# debtor_kyc_UI(outstandingfile,accid,loanbk=loanbk,feedback=feedback_df)
#' @import toOrdinal
loan_details_UI <- function(outstandingfile,accid,loanbk,feedback) {
    loan_details <- debtor_info(outstandingfile,accid,loanbk,feedback,type = 'loan_details')
    tagList(
        custom_panel(title="Loan Amount",value=  span(class='loan_details',format(loan_details$Outsourced_Amount,big.mark=",")),width = 4,caretstats = "",id = "client_profile"),
        custom_panel(title="Disbursement Date",value=  span(class='loan_details',toOrdinalDate2(loan_details$DisbursedOn)),width = 4,caretstats = "",id = "client_profile"),
        custom_panel(title="Loan Status",value= span(class='loan_details',"Open"),width = 4,caretstats = "",id = "client_profile"),
        custom_panel(title="Arrears Amount",value =  span(class='loan_details',format(loan_details$CurrentBalance,big.mark=",")),width = 4,caretstats = "",id = "client_profile"),
        custom_panel(title="Installment Due Date",value =  span(class='loan_details',toOrdinalDate2(loan_details$InstallmentDueDate)),width = 4,caretstats = "",id = "client_profile"),
        custom_panel(title="Recent Contact Date",value= span(class='loan_details',"15th Dec 2022"),width = 4,caretstats = "Contact type:SMS"),
        custom_panel(title="Arrears Days",value= span(class='loan_details',paste(loan_details$ArrearsDays," days")),width = 4,caretstats = "",id = "client_profile"),
        custom_panel(title="Maximum Arrears Days",value= span(class='loan_details',paste(loan_details$MaxArrears," days")),width = 4,caretstats = "",id = "client_profile"),
        custom_panel(title="Recent Contact Response",value= span(class='loan_details',span(class="lastfeed",loan_details$FeedBackClass)),width = 4,caretstats = "",id = "client_profile"),
    )
}
#' @import highcharter
rep_behavior_UI <- function(ns,outstandingfile,accid,loanbk) {
    outstandingfile <- outstandingfile %>% filter(AccountID == accid)
    loanbk      <- loanbk %>% filter(AccountID == accid)
    tagList(
        box(title = "Previous Loans History",width = 6,solidHeader = T,status = "success",
            col_12(
                col_6(p(class="loandetails",span(class="loandetails_title left text-focus-in","Number of loans :"),span(class="loandetails_value text-focus-in pull-right",pull(outstandingfile,LoanSeries)))),
                col_6(p(class="loandetails",span(class="loandetails_title right text-focus-in","Early Payments :"),span(class="loandetails_value text-focus-in pull-right",unique(pull(loanbk,PaidEarly)))))
            ),
            col_12(div(class="divider","Loan Listing")),
            col_12(class="loanlisting",
                   tableOutput(ns("loan_listing"))
            )
        ),
        box(title = "Repayment Trend",width = 6,solidHeader = T,status = "success",
            echarts4rOutput(ns('loanrepayment'))
        )
    )


}

prev_collection_activity_UI <- function(ns,auth_res) {

    div(class = "collection_activity",
        tabsetPanel(
            type = "pills",
            tabPanel(title = "Contact feedback/response",
                     reactableOutput(ns("call_feed"))),
            tabPanel(
                title = "SMS Sent",
                col_2(class = "controls_stats",
                      div(
                          id = "controls",
                          selectInput(
                              ns("sms_groups"),
                              label = "Senders",
                              choices = c("Call Agent",
                                          "Mfanisi System"),
                              width = "100%"
                          )
                      ),
                      div(class = "stats")),
                col_10(class = "listing",
                       DTOutput(ns("sms_sent")))
            ),
            tabPanel(
                title = "Collection Team Progress",
                box(
                    title = "Allocation timeline",
                    width = 9,
                    solidHeader = T,
                    status = "success",
                    DTOutput(ns("allocation_history"))
                ),
                box(
                    title = "Action points",
                    width = 3,
                    solidHeader = T,
                    status = "success",
                    if (auth_res$group %in% c('SuperAdmin', 'Supervisor')) {
                        tagList(
                            actionBttn(
                                ns("recal"),
                                "Recal",
                                icon = icon("undo"),
                                style = "material-flat",
                                size = "xs",
                                block = T,
                                color = "danger"
                            ),
                            hr(),
                            actionBttn(
                                ns("realocate"),
                                "Realocate",
                                icon = icon("redo"),
                                style = "material-flat",
                                size = "xs",
                                block = T,
                                color = "success"
                            )
                        )
                    }
                )
            )
        ))
}

#' Plot Repayment Trend
#'
#' This function plots the repayment trend based on the provided dataset of repayments.
#'
#' @param repayments A data frame containing the repayment data.
#' @return An ECharts plot object representing the repayment trend.
#' @import dplyr
#' @importFrom echarts4r e_charts e_line e_bar e_title e_tooltip e_theme e_labels
#' @noRd

plot_repayment_trend <- function(repayments, ...) {
    # get data
    if (nrow(repayments) == 0) {
        stop("Dataset is not available")
    }

    chart_type <- ifelse(nrow(repayments) > 3, "line", "bar")

    chart <- repayments %>% e_charts(RepaymentDate)
    chart <- switch(
        chart_type,
        line = chart %>% e_line(Amount_Paid, smooth = TRUE),
        bar = chart %>% e_bar(Amount_Paid, smooth = TRUE)
    ) %>%
        e_title("Repayment Pattern") %>%
        e_tooltip(trigger = "item") %>%
        e_theme("macarons") %>%
        e_labels() %>%
        e_show_loading()
    return(chart)
}



feedback_data <- function(datastaged, accid,loan_series) {
    customer_feedback <- tbl(db_con, 't_CustomerFeedBack') %>%
        filter(
            AccountID == accid,
            LoanSeries == loan_series
            )
    # outstanding <- datastaged %>% filter(AccountID == accid) %>%
    #     select(AccountID, LoanSeries)

    feedback <- customer_feedback %>%
        # left_join(outstanding %>% ,
        #           by = c("AccountID", "LoanSeries")) %>%
        select(
            ContactedBy = DebtCollector,
            ContactedOn = FeedBackDate,
            FeedBack  = FeedBackClass,
            ExactVerbatim
        )

    return(feedback)

}

sms_sent_data <- function(accid,loan_series) {
    sms <- tbl(db_con,"t_RecoverySMS") %>%
        filter(AccountID == accid,
               LoanSeries == loan_series)   %>%
        # left_join(tbl(db_con,'t_Outstanding') %>%
        #               select(AccountID, LoanSeries) %>%
        #               filter(AccountID == accid) ,
        #           by = c("AccountID", "LoanSeries")) %>%
        select(-c(-id,AccountID,LoanSeries,Destination)) %>%
        collect()

    return(sms)
}


sent_messages <- function(outstanding_fil,account_id,loan_series) {
    allocation_timeline <- tbl(db_con, 't_Allocation') %>%
        filter(AccountID == account_id, LoanSeries == loan_series) %>%
        mutate(
            'Amount Collected' = Outsourced_Amount - lead(Outsourced_Amount),
            'Amount Collected' = ifelse(is.na(`Amount Collected`),0,`Amount Collected`)
        ) %>%
        select(
            "Disbursement Date" = Disbursement_Date,
            "Due Date" = Last_Installement_Date,
            "Outsourced Amount" = Outsourced_Amount,
            "Amount Collected",
            'Debt Collector' = "Debt_Collector",
            "Outsource Date" = AllocationDate) %>%
        collect()

    return(allocation_timeline)
}

allocation_timeline <- function(accountid,loan_series) {
    data <- tbl(db_con, 't_Allocation') %>%
    filter(AccountID == accountid,
           LoanSeries == as.character(loan_series)) %>%
        group_by(Team,AllocationDate) %>%
        mutate(
            id = row_number(),
            "Allocation Stage" = ifelse(id == 1,"New Allocation","Reallocation"),
            'Amount Collected' = Outsourced_Amount - lead(Outsourced_Amount),
            'Amount Collected' = ifelse(is.na(`Amount Collected`),0,`Amount Collected`)
        ) %>%
        select(
            "Disbursement Date" = Disbursement_Date,
            "Due Date" = Last_Installement_Date,
            "Outsourced Amount" = Outsourced_Amount,
            "Amount Collected",
            'Debt Collector' = "Debt_Collector",
            "Outsource Date" = AllocationDate,
            'Allocation Stage') %>%
        collect()

    return(data)
}

#' Show Feedback Input Modal
#'
#' This function displays a modal dialog for collecting customer feedback.
#'
#' @param username The username containing account information.
show_feedbackinput <- function(ns,username) {
    tryCatch(
        expr = {
            if (length(username) == 5) {
                showModal(ui = modalDialog(
                    title = p(class='title-text',"Calling",username[[4]],"..."),
                    fluidRow(class="feedbackbox",
                             p(classs='title-text',paste(username[[3]])),
                             textInput(ns('loan_series'),'',value = username[[2]]) %>% tagAppendAttributes(class = "hidden_input"),
                             textInput(ns('accountid'),'',value = username[[1]]) %>% tagAppendAttributes(class = "hidden_input"),
                             selectInput(ns('feedbackclass'),"Customer Feedback",
                                         choices = tbl(db_con,'t_FeedbackClass') %>%
                                             pull('FeedBackClass')),
                             dateInput(ns('ptpdate'),"PTP Date",min = Sys.Date()),
                             textInput(ns('occupation'),'Employment/Business'),
                             textAreaInput(ns('exactverbatim'),'Exact Verbatim'),
                             actionBttn(ns('submitfeedback'),'Submit',icon = icon('send'),style = 'material-flat',size = 'xs',block = T,color = 'success')
                    ),
                    footer = modal_footer(T),size = 's',easyClose = T))
            }
        },
        error = function(e){
            # Show an error toast message
            print(e)
        }
    )
}
