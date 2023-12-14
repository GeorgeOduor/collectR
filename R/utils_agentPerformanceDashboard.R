recovery_listing <- function(debt_collector,product,year_val,month_val,team_val) {

    collection_trend <- tbl(db_con, 't_CollectionTrend') %>%
        filter(
               year(RepaymentDate)  == year_val,
               month(RepaymentDate) == month_val,
               if (product == 'All') {
                   Channel %in% c('MfanisiAirtel', 'MfanisiSafaricom')
               } else {
                   Channel == product
               },
               if (team_val == 'All') {
                   Team %in% c('ExternalDebtCollector', 'InternalDebtCollector')
               } else {
                   Team ==  !!gsub("\\s|s$", "", team_val)
               }
               )

    if (debt_collector != "All") {
        collection_trend <- collection_trend %>%
            filter(Debt_Collector == debt_collector)
    }

    collection_trend <- collection_trend %>%
        group_by(RepaymentDate = as.character(RepaymentDate)) %>%
        summarise(`Amount Recovered` = sum(Amount))

    return(collection_trend)


}


get_key_kpis <- function(debt_collector,year_val,month_val,product,team_val = NULL) {

    key_kpis <- tbl(db_con, 't_EndOfMonthCollectionTrend') %>%
        filter(year(RepaymentDate)==year_val,
               month(RepaymentDate)==month_val,
               RepaymentDate==max(RepaymentDate))

    if (team_val != "All") {
        key_kpis <- key_kpis %>% filter(Team == team_val)
    }


        if (debt_collector != "All") {
            key_kpis <- key_kpis %>% filter(Debt_Collector == debt_collector)
            key_kpis <- key_kpis %>%# collnames()
                select(Channel,Team,RepaymentDate,Outsource_Amount,Fully_Paid_Amount,Partially_Paid_Amount,
                       Outsource_Count,Recovered_Amount,Recovered_Count) %>%
                collect() %>%
                adorn_totals('row') %>%
                mutate(Channel = case_when(Channel == 'Total' ~ 'All',TRUE ~ Channel),
                       Recovery_Rate = round(Recovered_Amount/Outsource_Amount*100)) %>%
                janitor::untabyl() %>%
                filter(Channel == product) %>%
                as.list()
        }else{
            key_kpis <- key_kpis %>%# collnames()
                select(Channel,Team,RepaymentDate,Outsource_Amount,Fully_Paid_Amount,Partially_Paid_Amount,
                       Outsource_Count,Recovered_Amount,Recovered_Count) %>%
                collect() %>%
                group_by(
                    Channel
                ) %>%
                summarise_if(is.numeric,sum,na.rm=T
                ) %>%
                adorn_totals('row') %>%
                mutate(Channel = case_when(Channel == 'Total' ~ 'All',TRUE ~ Channel),
                       Recovery_Rate = round(Recovered_Amount/Outsource_Amount*100)) %>%
                janitor::untabyl() %>%
                filter(Channel == product) %>%
                as.list()
        }


    return(key_kpis)
}
#' Get Current Report
#'
#' This function retrieves the current report based on the provided channel filter.
#'
#' @param db_con The database connection.
#' @param channel The channel to filter the report. Use "All" to retrieve the overall report.
#'
#' @return A data frame containing the current report.
#' @importFrom janitor adorn_totals
#' @importFrom lubridate year
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
get_current_report <- function(db_con, channel,agent,year,month_or_quater,type) {
    # filter datasets

    end_month_collection <- tbl(db_con, 't_EndOfMonthCollectionTrend') %>%
        filter(Debt_Collector == !!agent,
               year(RepaymentDate)  == !!year,
               month(RepaymentDate) == !!month_or_quater,
        )
    daily_collection <- tbl(db_con, 't_CollectionTrend') %>%
        filter(Debt_Collector == !!agent,
               year(RepaymentDate)  == !!year,
               month(RepaymentDate) == !!month_or_quater,
               )



    current_report <- end_month_collection %>%
        left_join(
            daily_collection %>%
                select(RepaymentDate, Team, Debt_Collector, Channel, Amount, RepaymentTrend) ,
            by = c("Channel", "RepaymentDate", "Team", "Debt_Collector")
        ) %>%
        select(
            RepaymentDate, Channel, Debt_Collector, Outsource_Amount, Outsource_Count,
            Recovered_Amount, Recovered_Count, Amount, RepaymentTrend,
            Fully_Paid_Amount, Partially_Paid_Amount
        ) %>%
        mutate(RecoveryRate = round(Recovered_Amount / Outsource_Amount * 100, 2))

    current_report <- switch (type,
                              'top_kpis' = {
                                  current_report %>%
                                      filter(RepaymentDate == max(RepaymentDate, na.rm = TRUE)) %>%
                                      collect() %>%
                                      adorn_totals(na.rm = TRUE) %>%
                                      mutate(
                                          RecoveryRate = round(Recovered_Amount / Outsource_Amount * 100, 2),
                                          RepaymentTrend = RepaymentTrend * 0.5,
                                          Channel = case_when(Channel == '-' ~ "All",
                                                              TRUE ~ Channel)
                                      ) %>% split(.$Channel)
                              },
                              'listing' = {
                                  list(
                                      current_report = current_report,
                                      current_report_all = current_report %>%
                                          group_by(RepaymentDate) %>%
                                          summarise_if(is.numeric, sum) %>%
                                          mutate(Channel = "All")
                                  )
                              }
    )


    return(current_report)
}

#' Top KPIs UI
#'
#' This function generates the UI for displaying top key performance indicators (KPIs).
#'
#' @param key_kpis A list of key performance indicators.
#'
#' @return An HTML UI for displaying the top KPIs.
#'
#' @import shiny
#' @import shinydashboard
#' @import shinydashboardPlus
#' @import shinyWidgets
#' @noRd
top_kpis_UI <- function(key_kpis) {
    tryCatch(
        expr = {
            key_kpis %>%
                map2(.,names(.),.f = function(x,y){
                    if(y %in% c("Outsource_Amount","Recovered_Amount","Recovered_Count","Recovery_Rate")){
                        col_3(class = "agent_kpis",
                              custom_value_box(title = gsub("_"," ",y),
                                               value = ifelse(y=='Recovery_Rate',paste0(x," %"),
                                                              ifelse(y == 'Recovered_Count',format(x,big.mark=","),
                                                                     paste('Ksh',format(x,big.mark=",")))),
                                               trend = ifelse(y == "Outsource_Amount",key_kpis$Outsource_Count,""),
                                               trend_text=ifelse(y == "Outsource_Amount","Cases",""),
                                               trend_icon = "",
                                               main_icon ="coins"

                              )
                        )

                    }
                })
        },
        error = function(e){
            print(e)
            loadingState()
        }
    )
}

#' Retrieve Agent Summary
#'
#' This function retrieves the agent summary for a specific channel, debt collector, year, and month.
#'
#' @param channel_selected The selected channel for filtering the data. Can be 'All' or a specific channel.
#' @param debt_collector The debt collector for filtering the data.
#' @param year_val The year for filtering the data.
#' @param month_val The month for filtering the data.
#' @noRd
retrieve_agent_summary <- function(channel_selected, debt_collector, year_val, month_val,team_val) {

    # read required data here
    agent_repayments <- tbl(db_con, "t_Repayment")
    agent_allocation <- tbl(db_con, "t_Allocation")

    agent_repayments <- agent_repayments %>%
        filter(
            year(RepaymentDate) == year_val,
            month(RepaymentDate) == month_val,
            if (channel_selected == 'All') {
                Channel %in% c('MfanisiAirtel', 'MfanisiSafaricom')
            } else {
                Channel == channel_selected
            },
            if (team_val == 'All') {
                Team %in% c('ExternalDebtCollector', 'InternalDebtCollector')
            } else {
                Team ==  !!gsub("\\s|s$", "", team_val)
            }
            ) %>%
        mutate(
            LoanSeries = case_when(
                as.numeric(LoanSeries) == 1 ~ paste("1 (New Clients)"),
                between(as.numeric(LoanSeries), 2, 4) ~ "2 - 4",
                TRUE ~ '5 and above'
            )
        )

    if (debt_collector != "All") {
        agent_repayments <- agent_repayments %>% filter(Debt_Collector == debt_collector)
        agent_allocation <- agent_allocation %>% filter(Debt_Collector == debt_collector)
    }

    agent_repayments <- agent_repayments %>%
        group_by(Channel, LoanSeries) %>%
        summarise(
            Amount_Paid = sum(round(Amount_Paid, 2), na.rm = TRUE)
        ) %>% ungroup()

    # prepare allocation====
    agent_allocation <- agent_allocation %>%
        mutate(
            Channel = case_when(
                substr(AccountID, 1, 4) == "0019" ~ "MfanisiSafaricom",
                TRUE ~ "MfanisiAirtel"
            )
        ) %>%
        filter(
            year(AllocationDate) == year_val,
            month(AllocationDate) == month_val,
            if (channel_selected == 'All') {
                Channel %in% c('MfanisiAirtel', 'MfanisiSafaricom')
            } else {
                Channel == channel_selected
            },
            if (team_val == 'All') {
                Team %in% c('ExternalDebtCollector', 'InternalDebtCollector')
            } else {
                Team ==  !!gsub("\\s|s$", "", team_val)
            }
        )



    agent_allocation <- agent_allocation %>%
        mutate(
            LoanSeries = as.numeric(LoanSeries),
            LoanSeries = case_when(
                LoanSeries == 1 ~ paste("1 (New Clients)"),
                between(LoanSeries, 2, 4) ~ "2 - 4",
                TRUE ~ '5 and above'
            )
        ) %>%
        group_by(Channel, LoanSeries) %>%
        summarise(
            OutsourcedAmount = sum(Outsourced_Amount),OutsourcedCount = n()
        ) %>% ungroup()

    result <- agent_allocation %>%
        left_join(agent_repayments, by = c("Channel", "LoanSeries")) %>%
        select(-Channel) %>%
        collect()

     if (channel_selected == "All") {
        result <- result %>%
            group_by(LoanSeries) %>%
            summarize_all(sum,na.rm=T)
    #
     }

    result <- result %>%
        mutate(RecoveryRate = paste(round(Amount_Paid / OutsourcedAmount * 100, 2), "%"))%>%
        mutate_if(is.numeric, format, big.mark = ",") %>%
        select(
            'Loan Series' = LoanSeries,
            'Outsourced Count' = OutsourcedCount,
            'Outsourced Amount' = OutsourcedAmount,
            'Amount Paid' = Amount_Paid,
            'Recovery Rate' = RecoveryRate
        )

    return(result)

    }
#' Table Bar
#'
#' This function generates a table bar element with a label and a colored bar chart.
#'
#' @param label The label text to be displayed.
#' @param width The width of the bar chart. Default is "100%".
#' @param height The height of the bar chart. Default is "0.875rem".
#' @param fill The fill color of the bar chart. Default is "#00bfc4".
#' @param background The background color of the bar chart. Default is NULL.
#' @noRd
table_bar <- function(label, width = "100%", height = "0.875rem", fill = "#00bfc4", background = NULL) {
    bar <- div(style = list(background = fill, width = width, height = height))
    chart <- div(style = list(flexGrow = 1, marginLeft = "0.375rem", background = background), bar)
    div(style = list(display = "flex", alignItems = "center"), label, chart)
}

#' Performance Table
#'
#' This function retrieves performance data for a specific channel, year, and month, and generates a summary table.
#'
#' @param channel_selected The selected channel for performance comparison.
#'                        Set to "All" to include all channels. Default is "All".
#' @param year_value The year value for filtering the performance data.
#' @param month_value The month value for filtering the performance data.
#'
#' @return A data frame representing the performance summary table, including debt collectors, recovered amount, and recovery rate.
#' @noRd
performance_table <- function(channel_selected,year_value,month_value,team_value) {
    performance_comparison <- tbl(db_con, 't_EndOfMonthCollectionTrend') %>%
        filter(
               year(RepaymentDate) == year_value,
               month(RepaymentDate) == month_value,
               if (channel_selected == 'All') {
                   Channel %in% c('MfanisiAirtel', 'MfanisiSafaricom')
               } else {
                   Channel == channel_selected
               },
               if (team_value == 'All') {
                   Team %in% c('InternalDebtCollector','ExternalDebtCollector')
               } else {
                   Team == team_value
               },
               RepaymentDate == max(RepaymentDate)
        ) %>%
        select(-id,-Channel,-Team,-RepaymentDate,-UpdatedBy)  %>%
        group_by(Debt_Collector)  %>%
        summarise_all(sum) %>% #colnames()
        mutate(
            Recovery_Rate = round(Recovered_Amount/Outsource_Amount,4)
        ) %>%
        select(
            Debt_Collector,
            Recovered_Amount,
            Recovery_Rate
        ) %>%
        collect()
}

#' Show Performance Ranks
#'
#' This function displays the performance ranks in a reactive table format based on the provided performance comparison data.
#'
#' @param performance_comparison A data frame containing the performance comparison data, including debt collectors, recovered amount, and recovery rate.
#'
#' @return A reactive table displaying the performance ranks.
#' @noRd
#' @importFrom reactable reactable colDef colFormat

show_performance_ranks  <- function(performance_comparison) {
    performance_comparison %>%
        mutate(Rank = rank(-Recovery_Rate,ties.method = 'first')) %>%
        reactable(
            defaultSorted = "Recovery_Rate",
            columns = list(
                Rank = colDef(
                    width = 50
                ),
                Debt_Collector = colDef(
                    name = 'Debt Collector',
                    width = 100,
                ),
                Recovered_Amount = colDef(
                    name = 'Recovered Amount',
                    width = 120,
                    format = colFormat(separators = TRUE),
                    cell = function(value){
                        value <- paste0(format(value , nsmall = 2,big.mark=","))
                    }
                ),
                Recovery_Rate = colDef(
                    name = "Recovery Rate",
                    defaultSortOrder = "desc",
                    cell = function(value) {
                        value <- paste0(format(value * 100, nsmall = 2), "%")
                        # Fix width here to align single and double-digit percentages
                        value <- format(value, width = 5, justify = "right")
                        table_bar(value, width = value, fill = "#585985", background = "#e1e1e1")
                    },
                    align = 'center'
                )
            ), resizable = TRUE, bordered = TRUE
        )
}

#' Fetch Feedback Data
#'
#' Retrieves feedback data based on the specified parameters.
#'
#' @param channel_selected The selected channel for feedback data. Use "All" for all channels.
#' @param debt_collector The name of the debt collector.
#' @param year_val The year for filtering feedback data.
#' @param month_val The month for filtering feedback data.
#'
#' @return A data frame with consolidated feedback information including count, outsourced amount, amount recovered, and recovery rate.
feedbac_data <- function(channel_selected,debt_collector,year_val,month_val,team_val) {
    feedbackdata <- tbl(db_con,'t_CustomerFeedBack') %>% #collect() %>% view()
        filter(
            year(as.Date(FeedBackDate)) == year_val,
            month(as.Date(FeedBackDate)) == month_val
        ) %>%
        group_by(
            AccountID
        ) %>%
        mutate(rowid = row_number()) %>%
        filter(
            rowid == max(rowid)
        ) %>% ungroup()

    outstanding <- tbl(db_con,'t_Outstanding') %>%
        filter(
               if (channel_selected == 'All') {
                   Channel %in% c('MfanisiAirtel', 'MfanisiSafaricom')
               } else {
                   Channel == channel_selected
               },
               if (team_val == 'All') {
                   Team %in% c('InternalDebtCollector', 'ExternalDebtCollector')
               } else {
                   Team == team_val
               }) %>%
        group_by(AccountID,LoanSeries) %>%
        select(AccountID,LoanSeries,Outsourced_Amount,Amount_Paid,Debt_Collector)
        # adornto

    if (debt_collector != "All") {
        feedbackdata <- feedbackdata %>% filter(DebtCollector == debt_collector)
        outstanding <- outstanding %>% filter(Debt_Collector == debt_collector)
    }

    feedbackdata_consolidated <- outstanding %>% inner_join(feedbackdata,by = c("AccountID", "LoanSeries"))

    out <- feedbackdata_consolidated %>%
        group_by(FeebBack = FeedBackClass) %>%
        summarise(
            Count = n(),
            Outsourced_Amount = sum(Outsourced_Amount),
            Amount_Recovered = sum(Amount_Paid),
        ) %>%
        mutate(
            RecoveryRate = round(Amount_Recovered/Outsourced_Amount)
        )

    return(out)
}

header_ui <- function(ns) {
    div(class='filter-items',
        col_2(class="filter_details text-focus-in",
              pickerInput(ns('category'),label = "Category",
                          choices = NULL,width = "fit",
                          inline = T,options = list(size=5),
                          multiple = F)
        ),
        col_4(class="filter_details text-focus-in",
              pickerInput(ns('agents'),label = "Agents",
                          choices = NULL,width = "fit",
                          selected = NULL,
                          inline = T,options = list(size=10),
                          multiple = F)),
        col_3(class="filter_details text-focus-in",
              pickerInput(ns('product'),label = "Channel:",
                          choices = NULL,width = "fit",
                          selected = NULL,
                          inline = T,options = list(size=5),
                          multiple = F)),
        col_1(class="filter_details text-focus-in",
              numericInput(inputId = ns('report_year'),"Year",
                           value = year(Sys.Date()),min = year(Sys.Date()),
                           max = year(Sys.Date()),step = 1)
        ),
        col_2(class="filter_details last text-focus-in",
              pickerInput(ns('months'),label = "Month:",
                          choices = list(
                              Months=month.abb
                          ),width = "fit",
                          selected = as.character(month(Sys.Date(),T,F)),
                          inline = T,options = list(size=5),
                          multiple = F)
        )
    )


}

find_agent_listing <- function(category,auth_res) {
    if (category == "All") {
        agentslist <- tbl(db_con, 't_EndOfMonthCollectionTrend')
    }else{
        agentslist <- tbl(db_con, 't_EndOfMonthCollectionTrend') %>%
            filter(Team == !!sub("s$", "", gsub(" ","",category)))
    }
    agentslist <- agentslist %>% group_by(Team,Debt_Collector) %>% tally()
    if (!auth_res$group %in% c('Supervisor','SuperAdmin')) {
        agents = debt_collector
    }else{
        agents = list(
            "All",
            InternalDebtCollectors = agentslist %>% filter(Team == "InternalDebtCollector") %>% pull(Debt_Collector),
            ExternalDebtCollectors  = agentslist %>% filter(Team == "ExternalDebtCollector") %>% pull(Debt_Collector)
        )
    }
    return(agents)
}

find_teams <- function(auth_res){

    if (auth_res$group %in% c('Supervisor','SuperAdmin')) {
        CATEGORIES = c("All","Internal Debt Collectors","External Debt Collectors")
    }else{
        CATEGORIES = "Internal Debt Collectors"
    }
    return(CATEGORIES)
}

get_years <- function(agents) {
    tbl(db_con,'t_Allocation') %>%
        filter(
            if ( agents != 'All') {
                Debt_Collector == !!agents
            }
        ) %>%
        distinct(AllocationDate) %>% mutate(Year = substr(as.character(AllocationDate),1,4)) %>%
        distinct(Year) %>%
        pull(Year)

}

get_allocation_listing <- function(year_val, month_val, agent = "All", team = "All", product = "All") {
    if (!is.null(year_val)) {
        allocation_file <- tbl(db_con, 't_Allocation') %>%
            filter(year(AllocationDate) == !!year_val, month(AllocationDate) == !!month_val)

        if (product != "All") {
            allocation_file <- allocation_file %>% filter(Channel == !!product)
        }
        if (agent != "All") {
            allocation_file <- allocation_file %>% filter(Debt_Collector == !!agent)
        }
        if (team != "All") {
            allocation_file <- allocation_file %>% filter(Team == !!team)
        }

        allocation_data <- allocation_file %>% collect()

        return(allocation_data)
    }
}


