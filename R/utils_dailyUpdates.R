# rm(list=ls())
# library(janitor);library(dplyr);library(tidyr);library(purrr);library(openxlsx);library(lubridate);library(glue)
# devtools::load_all() %>% suppressMessages() %>% suppressWarnings()
# # report_date = Sys.Date()-107
# # report_date
# # load(glue("E:/Reports/Dashboard/May/{format(report_date,'%d.%m.%Y')}/All_Dash_Data_{format(report_date,'%d.%m.%Y')}.Rdata"),ex <- new.env())
# # largetrx = ex$largeTransactions %>% filter(year(TrxDate)==2023,month(TrxDate)==5) %>% select(AccountID,TrxDate,Credit,Debit)
# disputed_accounts <- read.xlsx("E:/external_debt_collectors/Disputes/consolidated_disputes.xlsx") %>%
#     rename(Channel = Chanel) %>%
#     mutate(Channel=paste0("Mfanisi",Channel),CreatedOn = "2020-01-01",UpdatedBy="George")
# dbWriteTable(db_con,'t_DisputedAccounts',disputed_accounts,overwrite=T)
# loanbook ----------------------------------------------------------------

#' Process Loan Data
#'
#' This function processes loan data by performing various transformations and calculations.
#'
#' @param loan_list A data frame containing loan data.
#' @param report_date The date of the report.
#' @param con An optional database connection object.
#'
#' @return A processed loan data tibble.
#'
#' @import dplyr
#' @import tidyr
#' @import lubridate
#' @import DBI
#'
#' @noRd
process_loan_data <- function(loan_list,report_date,con=NULL) {
    stopifnot(is.data.frame(loan_list))
    Loanbk <- loan_list %>%
        mutate(Channel = case_when(grepl("MFC", ProductID) ~ "MfanisiAirtel",
                                   ProductID == "MLS904" ~ "MfanisiSafaricom",
                                   TRUE ~ "BranchLoans"),
               IDNo2 = IDNo) %>%
        split(.$Channel) %>%
        map_df(.f = function(x) {
            if(grepl("Mfanisi", unique(x$Channel))) {
                x %>%
                    separate(IDNo, into = c('Mfanisi', 'Ref', 'IDNo'), sep = "_")
            } else {
                x
            }
        }) %>%
        mutate(
            IDNo = trimws(case_when(
                is.na(Ref) & is.na(IDNo) ~ Mfanisi,
                is.na(IDNo) ~ Ref,
                TRUE ~ IDNo
            )),
            ArrearsDays = case_when(
                is.na(AccountCloseDate) ~ as.numeric(report_date - as.Date(InstallmentDueDate)),
                !is.na(AccountCloseDate) &  ArrearsDays < 90 ~ as.numeric(report_date - as.Date(InstallmentDueDate)),
                TRUE ~ as.numeric(as.Date(AccountCloseDate) - as.Date(InstallmentDueDate))
            ),
            # ArrearsDays = case_when(!is.na(ArrearsDa))
            ArrearsDays = case_when(ArrearsDays < 0 ~ 0,TRUE ~ ArrearsDays)
        ) %>%
        select(
            ClientID, AccountID, RepaymentAccountID, AccountName, MobileNo, National_ID = IDNo, LoanSeries,
            DisbursedOn, InstallmentDueDate, LoanAmount, AccountCloseDate, ClearBalance, ArrearsDays,
            ArrearsAmount, Channel,LastRepaymentDate,LastRepaymentAmount
        ) %>%
        mutate_at(vars(LoanSeries, LoanAmount, ClearBalance, ArrearsDays, ArrearsAmount), as.numeric) %>%
        mutate_at(vars(InstallmentDueDate, DisbursedOn, AccountCloseDate), as.Date) %>%
        # filter(ProductID %in% product_ids) %>%
        as_tibble() %>%
        group_by(AccountID) %>%
        mutate(MaxArrears = max(ArrearsDays,na.rm = T),
               PaidEarly = sum(ArrearsDays <= 0))

    if (!is.null(con)) {
        dbWriteTable(con,"t_LoanBook",value = Loanbk,overwrite=T)
    }else{
        return(Loanbk)
    }
}
# process_loan_data(ex$Loan_List2,con = db_con)
# allocation file ---------------------------------------------------------
alocation_files <- function(Allocation1,writedb,overwrite,...) {
    kwargs <- list(...)
    Allocation1 <- Allocation1 %>%
        mutate(year_ = year(Allocation_Date),
               month_ = month(Allocation_Date),
               National_ID = trimws(National_ID)) %>%
        filter(year_ == !!year(report_date),
               month_ == !!month(report_date),
               as.Date(Allocation_Date <= !!report_date)) %>%
        collect() %>%
        select(
            National_ID,Client_Name,MFanisi_Account,AccountID,Mobile_No,Disbursement_Date,Last_Installement_Date,
            Outsourced_Amount=Loan_Balance,Days_Overdue,Debt_Collector=Call_Agent,LoanSeries,AllocationDate=Allocation_Date
        )%>%
        mutate(
            Team = "InternalDebtCollector"
        )
    if (nrow(collect(Allocation1)) == 0) {
        stop("Internal allocation are not there skipping")
    }
    Allocation2 <- read.xlsx("E:/external_debt_collectors/allocation/2023/May/01.05.2023/Allocation_Apr_CarryForward_cleaned.xlsx") %>%
        bind_rows(
            read.xlsx("E:/external_debt_collectors/allocation/2023/May/02.05.2023/Mfanisi1/Allocation__May.xlsx")
        ) %>%
        bind_rows(
            read.xlsx("E:/external_debt_collectors/allocation/2023/May/02.05.2023/Mfanisi2/Allocation_Saf_May.xlsx")
        )%>%
        select(
            National_ID,Client_Name,MFanisi_Account,AccountID,Mobile_No,Disbursement_Date,Last_Installement_Date,
            Outsourced_Amount,Days_Overdue,Debt_Collector,LoanSeries,AllocationDate
        ) %>%
        mutate(
            National_ID = trimws(National_ID),
            Team = "ExternalDebtCollector"
        )
    Allocation <- rbind(Allocation1, Allocation2) %>%
        mutate(across(c(LoanSeries, Outsourced_Amount, Days_Overdue), as.numeric),
               across(c(AllocationDate, Disbursement_Date, Last_Installement_Date), as.Date)
        )

    if (writedb) {
        if (overwrite) {
            dbExecute(db_con,'DELETE FROM "t_Allocation"')
        }
        dbWriteTable(db_con,"t_Allocation",Allocation,append=T) %>%
            suppressWarnings()
    }
    return(Allocation)
}
# Outstanding File --------------------------------------------------------
#' Get Current Status
#'
#' This function retrieves the current status of loan allocations based on the given data.
#'
#' @param Allocation A data frame containing loan allocation data.
#' @param LoanBook A data frame containing loan book data.
#' @param report_date The date of the report.
#' @param writedb An optional flag indicating whether to write the results to a database table.
#'
#' @return A data frame representing the current status of loan allocations.
#' @import dplyr
#' @import DBI
#' @noRd
get_current_status <- function(report_date,writedb=NULL) {
    LoanBook = tbl(db_con,'t_LoanBook')
    Allocation = tbl(db_con,'t_Allocation') %>%
        filter(
            year(AllocationDate) == !!year(report_date),
            month(AllocationDate) == !!month(report_date),
        )
    current_status <- Allocation %>%
        select(
            Mobile_No, National_ID, AccountID, RepaymentAccountID = MFanisi_Account,
            LoanSeries, Client_Name, Outsourced_Amount, Debt_Collector, Team, AllocationDate,
            allocation_id
        ) %>%
        mutate(LoanSeries = as.numeric(LoanSeries)) %>%
        left_join(
            LoanBook %>%
                select(-LoanAmount) %>%
                filter(grepl("Mfanisi", Channel)),
            by = c("Mobile_No" = "MobileNo", "AccountID", "RepaymentAccountID",
                   "LoanSeries", 'National_ID')
        ) %>%
        rename(
            CurrentBalance = ClearBalance
        ) %>%
        mutate(
            CurrentBalance = ifelse(is.na(CurrentBalance)|as.numeric(CurrentBalance) < 0, 0, as.numeric(CurrentBalance)),
            Amount_Paid = Outsourced_Amount - CurrentBalance,
            Amount_Paid = case_when(Amount_Paid < 0 ~ 0, TRUE ~ Amount_Paid)
        ) %>%
        mutate(
            ReportDate = report_date
        ) #%>%
    # filter(!is.na(ClientID))
    if (writedb) {
        dbWriteTable(db_con,"t_Outstanding_prov",collect(current_status),overwrite=T)
    }
    return(current_status)
}
# check consistency -------------------------------------------------------
check <- function(db_con) {
    # Calculate summary statistics for both data frames
    current_summary <- tbl(db_con,"t_Outstanding_prov") %>%
        summarise(
            Count = n(),
            Outsourced_Amount = sum(Outsourced_Amount,na.rm = T),
            data = "Current_status"
        ) %>% collect()

    prev_summary <- tbl(db_con,"t_Allocation") %>%
        summarise(
            Count = n(),
            Outsourced_Amount = sum(Outsourced_Amount),
            data = "Prev_status"
        ) %>% collect()

    summary_tbl <- current_summary %>%
        bind_rows(prev_summary) %>%
        t() %>%
        as_tibble(.name_repair = 'minimal',rownames = "data") %>%
        clean_names() %>%
        arrange(desc(x_2)) %>%
        row_to_names(1) %>%
        mutate(
            Check = Current_status == Prev_status
        )

    # Check if data is consistent
    if (all(summary_tbl$Check)) {
        message("All is well up to this point.")
    } else {
        wrong_idx <- which(!summary_tbl$Check)
        wrong_data <- names(summary_tbl)[wrong_idx + 2]

        if (length(wrong_data) == 2) {
            stop("Error: Both Count and Outsourced Amount are inconsistent.")
        } else if (wrong_data == "Outsourced_Amount_diff") {
            stop("Error: Outsourced Amount is inconsistent.")
        } else {
            stop("Error: Count is inconsistent.")
        }
    }

    # Return summary table
    return(summary_tbl)
}
# create_repayments_file --------------------------------------------------
get_today_payments <- function(db_con, report_date,dbwrite=F) {

    repayment <- tbl(db_con,"t_Outstanding_prov") %>%
        select(
            ClientID,Team,Debt_Collector,Channel,AccountID,
            LoanSeries,Amount_Paid,RepaymentDate = ReportDate
        ) %>%
        filter(Amount_Paid> 0)

    existing_repayment <- tbl(db_con, 't_Repayment')
    last_repayment_update <- existing_repayment %>%
        filter(RepaymentDate == max(RepaymentDate, na.rm = TRUE)) %>%
        distinct(RepaymentDate) %>% pull(RepaymentDate)
    if (length(last_repayment_update)>0) {
        if (last_repayment_update == report_date) {
            message("Deleting previous repayments data for report date:", report_date)
            dbSendQuery(db_con,statement = glue("DELETE FROM \"t_Repayment\" WHERE \"RepaymentDate\" = '{report_date}' "))
        }

    }

    todays_payment <- tbl(db_con, 't_Repayment') %>%
        select(-Channel)%>%
        select(-Team,-Debt_Collector) %>%
        filter(year(RepaymentDate)==!!year(report_date),
               month(RepaymentDate)==!!month(report_date)) %>%
        group_by(ClientID,AccountID,LoanSeries) %>%
        window_order(RepaymentDate) %>%
        mutate(Amount_Paid = cumsum(Amount_Paid)) %>%
        filter(RepaymentDate == max(RepaymentDate, na.rm = TRUE),
               # AccountID == '0019040004768'
        ) %>% collect() %>%
        full_join(collect(repayment), by = c("ClientID", "AccountID", "LoanSeries")) %>%
        mutate(
            Amount_Paid.x = ifelse(is.na(Amount_Paid.x), 0, Amount_Paid.x),
            Amount_Paid.y = ifelse(is.na(Amount_Paid.y), 0, Amount_Paid.y),
        ) %>%
        # mutate_at(vars('Amount_Paid.x', 'Amount_Paid.y'), .funs = function(x) ifelse(is.na(x), 0, x)) %>%
        mutate(
            New_AmountPaid = case_when(
                Amount_Paid.x > 0 & Amount_Paid.y == 0 ~ 0,
                TRUE ~ Amount_Paid.y - Amount_Paid.x
            )
        ) %>%
        select(-Amount_Paid.y) %>%
        collect() %>%
        reshape2::melt(
            id.vars = c('ClientID', 'AccountID', 'LoanSeries', 'RepaymentDate.x', 'RepaymentDate.y','Team','Debt_Collector','Channel')
        ) %>%
        filter(value > 0) %>%
        as_tibble() %>%
        mutate(
            RepaymentDate = as.Date(ifelse(
                variable == 'New_AmountPaid' , as.character(RepaymentDate.y),
                as.character(RepaymentDate.x)
            ))
        ) %>%
        select(-RepaymentDate.x, -RepaymentDate.y, -variable) %>%
        filter(RepaymentDate == report_date) %>%
        rename(Amount_Paid = 'value') %>%
        arrange(desc(Amount_Paid)) %>%
        as_tibble() %>%
        mutate(Amount_Paid = as.numeric(Amount_Paid))

    if (dbwrite) {
        dbWriteTable(db_con,'t_Repayment',todays_payment,append=T)
        message("Successfuly save repayments for:", report_date)
    }
    return(todays_payment)
}
# update outstanding -----------------------------------------------------
get_outstanding_file <- function(db_con, report_date,dbwrite = F) {

    # Check if required input arguments are provided
    if (!missing(db_con) && !missing(report_date)) {

        # Get repayment data for the report month
        repayment_data <- tbl(db_con, "Repayment") %>%
            filter(year(RepaymentDate) == !!year(report_date),
                   month(RepaymentDate) == !!month(report_date))

        # Get the outstanding file data
        outstanding_file <- repayment_data %>%
            group_by(AccountID, LoanSeries) %>%
            summarise(
                Count = n(),
                Amount = sum(Amount_Paid)
            ) %>%
            ungroup() %>%
            arrange(
                desc(Count), desc(Amount)
            ) %>%
            right_join(
                tbl(db_con,"t_Outstanding_prov"),
                by = c("AccountID", "LoanSeries")
            ) %>%
            mutate(
                Amount_Paid = case_when(
                    Amount > Amount_Paid & Team == "InternalDebtCollector" ~ Amount,
                    TRUE ~ Amount_Paid
                ),
                Repayment_Status = case_when(
                    Amount_Paid >= Outsourced_Amount ~ "Fully_Paid",
                    Amount_Paid < Outsourced_Amount & Amount_Paid > 0 ~ "Partially_Paid",
                    TRUE ~ "No_Payment"
                )
            ) %>%
            select(
                ClientID,Client_Name, National_ID, AccountID, RepaymentAccountID, Mobile_No, LoanSeries,DisbursedOn,
                InstallmentDueDate, Outsource_Date = AllocationDate,AccountCloseDate,Outsourced_Amount,
                CurrentBalance, Amount_Paid,ArrearsDays,Repayment_Status,Debt_Collector, Team,Channel,
                allocation_id
            ) %>%
            # rowid_to_column(var = "id") %>%
            mutate(
                date_created = report_date,
                CreatedBy = "George"
            )

        # Add log message
        message("Outstanding file data retrieved successfully.")

        if (dbwrite == T) {
            dbWriteTable(db_con,"t_Outstanding",collect(outstanding_file),overwrite = T)
        }

        return(outstanding_file)

    } else {
        # Return error message if required input arguments are missing
        return("Error: Missing required input arguments.")
    }


}
# update daily trend collections -----------------------------------------------
daily_trend <- function(db_con, report_date, dbwrite = FALSE) {

    collection_trend <- tbl(db_con, "t_CollectionTrend")

    last_2_days <- collection_trend %>%
        distinct(RepaymentDate) %>%
        arrange(RepaymentDate) %>%
        pull(RepaymentDate) %>%
        tail(2) %>%
        append(report_date)

    last_update <- collection_trend %>%
        filter(RepaymentDate == max(RepaymentDate, na.rm = TRUE)) %>%
        pull(RepaymentDate) %>%
        last()

    if (!is.na(last_update) & last_update == report_date) {
        message("Deleting previous data for report date:", report_date)
        dbSendQuery(db_con, statement = glue("DELETE FROM \"t_CollectionTrend\" WHERE \"RepaymentDate\" >= '{report_date}' "))
    }

    collections <- tbl(db_con, "t_Repayment") %>%
        filter(RepaymentDate %in% last_2_days) %>%
        mutate(
            id = row_number(),
            Channel = case_when(substr(AccountID, 1, 4) == "0019" ~ "MfanisiSafaricom",
                                TRUE ~ "MfanisiAirtel")
        ) %>%
        group_by(RepaymentDate, Team, Debt_Collector, Channel) %>%
        arrange(RepaymentDate) %>%
        collect() %>%
        summarise(
            Amount = sum(Amount_Paid, na.rm = TRUE)
        ) %>%
        ungroup() %>%
        group_by(Channel, Team, Debt_Collector) %>%
        mutate(
            RepaymentTrend = round((Amount - lag(Amount)) / lag(Amount) * 100, 2),
            UpdatedBy = "George",
            Category = "DailyTrend"
        ) %>%
        ungroup() %>%
        collect() %>%
        suppressMessages()

    collector_team_trend <- collections %>%
        ungroup() %>%
        group_by(RepaymentDate, Team, Channel) %>%
        summarise(
            Amount = sum(Amount, na.rm = TRUE)
        ) %>%
        ungroup() %>%
        arrange(Channel, Team, RepaymentDate) %>%
        group_by(Team, Channel) %>%
        mutate(
            RepaymentTrend = round((Amount - lag(Amount)) / lag(Amount) * 100, 2),
            Debt_Collector = "All",
            UpdatedBy = "George",
            Category = "DailyTrend"
        ) %>%
        select(
            RepaymentDate, Channel, Team, Debt_Collector, Amount, RepaymentTrend, UpdatedBy, Category
        ) %>%
        filter(RepaymentDate == !!report_date) %>% suppressMessages()

    collections_trend <- rbind(
        collections %>% filter(RepaymentDate == !!report_date) %>% collect(),
        collector_team_trend %>% collect()
    ) %>%
        mutate(across(where(is.numeric), ~ round(., 2)))

    if (dbwrite) {
        message("Writing data to 't_CollectionTrend' table.")
        dbWriteTable(db_con, "t_CollectionTrend", collections_trend, append = TRUE)
    }


}
# update end of month collection ------
updateEndOfMonthCollection <- function(db_con, report_date,dbwrite=F) {
    message("Updating End of Month Collection...")

    existing_summary <- tbl(db_con, 't_EndOfMonthCollectionTrend')

    last_update <- existing_summary %>%
        filter(RepaymentDate == max(RepaymentDate, na.rm = TRUE)) %>%
        distinct(RepaymentDate) %>%
        pull(RepaymentDate)

    if (length(last_update) > 0) {
        if (last_update == report_date) {
            message("Deleting previous data for report date:", report_date)
            dbSendQuery(db_con, statement = glue("DELETE FROM \"t_EndOfMonthCollectionTrend\" WHERE \"RepaymentDate\" = '{report_date}' "))
        }
    }

    current_day <- tbl(db_con, "t_Outstanding") %>%
        mutate(PaymentsMade = as.integer(Amount_Paid > 0)) %>%
        group_by(Channel, Team, Debt_Collector, Repayment_Status)  %>%
        summarise(
            Amount = sum(Amount_Paid),
            Count = n(),
            RepaidCount = sum(PaymentsMade, na.rm = TRUE),
            Outsourced_Amount = sum(Outsourced_Amount)
        ) %>%
        mutate(RepaymentDate = report_date) %>%
        pivot_longer(cols = c(Amount, Outsourced_Amount, Count, RepaidCount)) %>%
        filter(!(name == "Amount" && value == 0)) %>%
        pivot_wider(
            id_cols = c(Channel, Team, Debt_Collector, RepaymentDate),
            names_from = c(Repayment_Status, name),
            values_from = value,
            values_fill = 0
        ) %>% collect() %>% ungroup() %>%
        suppressMessages()

    endofmonth_all <- summary_template %>%
        ungroup() %>%
        bind_rows(current_day) %>%
        mutate(
            Total_Outsource = Partially_Paid_Outsourced_Amount + Fully_Paid_Outsourced_Amount + No_Payment_Outsourced_Amount,
            Total_Outsource_Count = Partially_Paid_Count + Fully_Paid_Count + No_Payment_Count,
            Total_Repaid = Partially_Paid_Amount + Fully_Paid_Amount,
            Total_Repaid_Count = Partially_Paid_RepaidCount + Fully_Paid_RepaidCount
        ) %>%
        select(
            RepaymentDate, Channel, Team, Debt_Collector, Outsource_Amount = Total_Outsource,
            Outsource_Count = Total_Outsource_Count, Recovered_Count = Total_Repaid_Count,
            Partially_Paid_Amount, Fully_Paid_Amount, Recovered_Amount = Total_Repaid
        ) %>%
        mutate(across(where(is.numeric), ~ round(., 2)), UpdatedBy = "George") %>%
        suppressMessages()

    message("End of Month Collection updated successfully.")
    if (dbwrite) {
        dbWriteTable(db_con,"t_EndOfMonthCollectionTrend",endofmonth_all,append=T)
    }
    return(endofmonth_all)
}
# first contact resolution =====
fcr_update <- function(db_con,debt_collector,reportDate) {
    year_val = year(reportDate)
    month_val = month(reportDate)
    # collect feedback data for the current month====
    feedbackdata <- tbl(db_con,'t_CustomerFeedBack') %>%
        filter(
            # DebtCollector == debt_collector,
            # as.Date(FeedBackDate) >= !!startDate,
            year(as.Date(FeedBackDate)) == year_val,
            month(as.Date(FeedBackDate)) == month_val,
        ) %>% #collect() %>%
        group_by(
            DebtCollector,AccountID
        ) %>%
        mutate(
            ContactTrials = n()
        ) %>%
        filter(
            ContactTrials == 1
        )
    # collect repaymenrData For the currentMonth====
    repaymentdata <- tbl(db_con,'t_Repayment') %>%
        filter(
            # Debt_Collector == debt_collector,
            year(as.Date(RepaymentDate)) == year_val,
            month(as.Date(RepaymentDate)) == month_val
        ) %>%
        group_by(
            Debt_Collector,AccountID
        ) %>%
        filter(id == min(id)) %>%
        mutate(
            Installments = row_number(),
            Amount_Paid = round(Amount_Paid,2)
        ) %>%
        select(-id,-ClientID,-Team) %>%
        ungroup()
    # get outstanding data ====
    outstandingdata <- tbl(db_con,"t_Outstanding") %>%
        filter(
            # Debt_Collector == debt_collector,
            # year(as.Date(RepaymentDate)) == year_val,
            # month(as.Date(RepaymentDate)) == month_val,
        )%>%
        select(
            AccountID,LoanSeries,Paid = Amount_Paid,Repayment_Status,Channel,Debt_Collector,Outsourced_Amount
        ) %>%
        mutate(Paid = round(Paid,2))
    # compile repayments =====
    repayments <- repaymentdata %>%
        inner_join(outstandingdata,
                   by = c("AccountID", "LoanSeries")) %>%
        mutate(
            FirstTime = ifelse(Paid == Amount_Paid,T,F)
        )

    # summarise ====
    out = feedbackdata %>%
        rename(Debt_Collector=DebtCollector) %>%
        select(-Occupation,-ExactVerbatim,-FeedBackClass,-PTPDate) %>%
        inner_join(
            repayments,
            by = c("AccountID", "LoanSeries")
        ) %>%
        select(
            AccountID,LoanSeries,Channel,Amount_Paid,Paid,Repayment_Status,ContactTrials,
            Outsourced_Amount,FeedBackDate
        ) %>%
        mutate(
            Channel = case_when(
                substr(AccountID,1,4) == '0019' ~ "MfanisiSafaricom",
                TRUE  ~  "MfanisiAirtel"
            )
        ) %>% #collect() %>% View()
        group_by(
            Debt_Collector,Channel,Repayment_Status,
            Installment = ifelse(!is.na(ContactTrials),"First","Subsequent")
        ) %>%
        summarise(
            Count = n(),
            CollectedAmount = sum(Paid,na.rm=T),
            Outsourced_Amount = sum(Outsourced_Amount,na.rm=T)
        ) %>%
        ungroup() %>%
        group_by(
            Debt_Collector,Channel
        ) %>%
        filter(
            Installment == "First",
            Repayment_Status == "Fully_Paid"
        ) %>%
        left_join(
            outstandingdata %>%
                group_by(Debt_Collector,Channel) %>%
                summarise(OutsourcedAmount = sum(Outsourced_Amount,na.rm = T)),
            by = c("Channel",'Debt_Collector')
        ) %>%
        mutate(
            RecoveryRate = round(CollectedAmount/OutsourcedAmount*100,2)
        ) %>%
        collect() %>%
        split(.$Debt_Collector) %>%
        map_df(~adorn_totals(.,name = "All") %>%
                   mutate(
                       ReportDate = reportDate,
                       Channel = case_when(Channel == '-' ~ Debt_Collector,T~Channel),
                       Debt_Collector = case_when(Debt_Collector == 'All' ~ lag(Debt_Collector),T~Debt_Collector)
                       # Debt_Collector = debt_collector
                   ) %>%
                   select(-Repayment_Status,-Outsourced_Amount,-Installment,-OutsourcedAmount) %>%
                   suppressMessages()) %>% suppressMessages()

    query <- paste0("DELETE FROM \"t_FCR\" WHERE CAST(\"ReportDate\" AS DATE) = '", reportDate, "'")
    dbExecute(db_con, statement = query)
    dbWriteTable(db_con,"t_FCR",as_tibble(out),append=T)

}
# ptp rate update =========
ptp_summary_update <- function(report_date) {
    # collect feedback ==============
    feedback <- tbl(db_con,"t_CustomerFeedBack")%>%
        filter(
            FeedBackClass == 'Promised to Pay',
            year(as.Date(FeedBackDate)) == year_val,
            month(as.Date(FeedBackDate)) == month_val,
            as.Date(FeedBackDate) <= as.Date(report_date)
        ) %>%
        distinct(
            AccountID,.keep_all = T
        ) %>% select(-Channel,-DebtCollector)
    # collect outstanding ==============

    outstanding <- tbl(db_con,"t_Outstanding") %>%
        select(
            AccountID,LoanSeries,CurrentBalance,Channel,Debt_Collector,Team
        )
    # collect ptp_summary ==============

    ptp_summary <- feedback %>%
        right_join(outstanding, by = c("AccountID", "LoanSeries")) %>%
        group_by(
            FeedBackClass,Channel,Team,Debt_Collector
        ) %>%
        summarise(
            Count = n(),
            CurrentBalance = sum(CurrentBalance)
        ) %>%
        collect() %>% ungroup() %>%
        split(.$Debt_Collector) %>%
        map_df(~mutate(.,PTPRate = Count/sum(Count)) %>%
                   filter(!is.na(FeedBackClass)) %>%
                   adorn_totals("row")
        ) %>%
        # filter(CurrentBalance >0) %>%
        mutate(
            Channel = case_when(Channel == "-" ~ "All",T ~ Channel),
            Debt_Collector = case_when(Debt_Collector == "-" ~ lag(Debt_Collector),T ~ Debt_Collector),
            Team = case_when(Team == "-" ~ lag(Team),T ~ Team),
            ReportDate = report_date,
            PTPRate = round(PTPRate,2)
        ) %>%
        select(-FeedBackClass) %>%
        suppressMessages()

    delete_clause(db_con,'t_PTPRateSummary',all = F,where = 'ReportDate',
                  is=as.character(report_date),exec = T)

    dbWriteTable(db_con,'t_PTPRateSummary',as_tibble(ptp_summary),append=T)
    message("Repayment Summary writtensuccessfully.")
}
# update slippages ====
slippages <- function(report_date) {
    outstanding <- tbl(db_con,"t_Outstanding")

    slippages <- outstanding %>%
        mutate(
            Slipage = ArrearsDays > 35
        ) %>%
        filter(
            Team == 'InternalDebtCollector'
        ) %>%
        group_by(
            Debt_Collector,Slipage,Channel
        ) %>%
        summarise(
            Count = n(),
            Outstanding = sum(CurrentBalance)
        ) %>%
        mutate(
            SlipagePropotion = Count/sum(Count)
        ) %>%
        arrange(
            Debt_Collector
        ) %>%
        # filter(Slipage) %>%
        collect() %>%
        split(.$Debt_Collector) %>%
        map_df(~ split(.,.$Channel) %>%
                   map_df(~mutate(ungroup(.),
                                  Channel = case_when(Channel == "-" ~ "All",T ~ Channel),
                                  Debt_Collector = case_when(Debt_Collector == "Total" ~ lag(Debt_Collector),T ~ Debt_Collector),
                                  SlipagePropotion = Count/sum(Count)
                   )
                   ) %>%
                   split(.$Slipage) %>%
                   map_df(~adorn_totals(.,'row')) %>%
                   untabyl() %>%
                   bind_rows(
                       filter(.,Debt_Collector == "Total") %>%
                           mutate(SlipagePropotion = Count/sum(Count),
                                  Slipage = 'TRUE',
                                  Channel = "All"
                           ) %>%
                           tail(1) %>%
                           suppressWarnings()
                   )
        ) %>% filter(Slipage == 'TRUE') %>%
        mutate(ReportDate = report_date,
               Debt_Collector = case_when(Debt_Collector == "Total" ~ lag(Debt_Collector),T ~ Debt_Collector),
        ) %>%
        select(-Slipage) %>%
        suppressWarnings()
    delete_clause(db_con,'t_Slipages',all = F,is=as.character(report_date),where = 'ReportDate',exec = T)
    dbWriteTable(db_con,name = "t_Slipages",value = as_tibble(slippages),append=T)
    message("Slippages Written Successfully!")
}
# update recovery days =========
collectionDate <- function(report_date) {
    outstanding_file <- tbl(db_con,"t_Outstanding")
    feedback_table <- tbl(db_con,'t_CustomerFeedBack') %>%
        filter(
            year(as.Date(FeedBackDate)) == !!year(report_date),
            month(as.Date(FeedBackDate)) == !!month(report_date),
        ) %>%
        select(AccountID,LoanSeries,FeedBackClass)

    collected_directly <- outstanding_file %>%
        inner_join(feedback_table) %>%
        suppressMessages()

    collectiontyme <- collected_directly %>%
        filter(!is.na(AccountCloseDate)) %>%
        mutate(CollectionTime = AccountCloseDate - Outsource_Date)   %>%
        group_by(Team,Channel,Debt_Collector) %>%
        collect() %>%filter(CollectionTime > 0) %>%
        summarise(CollectionTime = round(mean(CollectionTime))) %>%
        mutate(ReportDate = report_date)

    delete_clause(db_con,'t_CollectionTime',all = F,is=as.character(report_date),where = "ReportDate",exec = T)

    dbWriteTable(db_con,'t_CollectionTime',as_tibble(collectiontyme),append=T)
    message("Average Collection Time written successfully")
}
# update feedback summary
feedback_summary <- function(reportdate) {
    report_date <- ceiling_date(report_date,unit = 'month')-1

    feedbackdata <- tbl(db_con,'t_CustomerFeedBack') %>% #collect() %>% view()
        filter(
            year(as.Date(FeedBackDate)) == !!year(reportdate),
            month(as.Date(FeedBackDate)) == !!month(reportdate)
        ) %>%
        group_by(
            Deb_Collector = DebtCollector,AccountID
        ) %>%
        mutate(rowid = row_number()) %>%
        filter(
            rowid == max(rowid)
        ) %>% ungroup() %>%
        select(
            Deb_Collector,AccountID,LoanSeries,FeedBack = FeedBackClass
        )

    outstanding <- tbl(db_con,'t_Outstanding') %>%
        group_by(Team,Debt_Collector,AccountID,LoanSeries) %>%
        select(Team,Debt_Collector,Channel,AccountID,LoanSeries,Outsourced_Amount,Amount_Paid) %>%
        mutate(Amount_Paid = round(Amount_Paid,2)) %>%
        ungroup()
    # adornto

    feedbackdata_consolidated <- outstanding %>%
        left_join(feedbackdata,by = c("AccountID", "LoanSeries"))

    recovery_status <- feedbackdata_consolidated %>%
        group_by(Team,Debt_Collector,Channel,FeedBack) %>%
        mutate(
            Contacted = case_when(!is.na(FeedBack)~1,TRUE~ 0)
        ) %>%
        summarise(
            Outsourced_Cases = n(),
            Contacted_Cases = sum(Contacted,na.rm = T),
            Outsourced_Amount = sum(Outsourced_Amount),
            Amount_Recovered = sum(Amount_Paid),
        ) %>%
        collect()

    feedback_summary_df <- recovery_status %>%
        split(.$Team) %>%
        map_df(~split(.,.$Debt_Collector) %>%
                   map_df(~split(.,.$Channel) %>%
                              map_df(~adorn_totals(.) %>%
                                         mutate(
                                             Debt_Collector = ifelse(Team == 'Total', lag(Debt_Collector), Debt_Collector),
                                             Channel = ifelse(Team == 'Total', lag(Channel), Channel),
                                             FeedBack = ifelse(Team == 'Total', "All", FeedBack),
                                             Team = ifelse(Team == 'Total', lag(Team), Team)
                                         )
                              ) %>%
                              untabyl() %>%
                              bind_rows(filter(.,FeedBack == "All") %>% adorn_totals('row') %>%
                                            mutate(
                                                Debt_Collector = ifelse(Team == 'Total', lag(Debt_Collector), Debt_Collector),
                                                Channel = ifelse(Team == 'Total', "All", Channel),
                                                FeedBack = ifelse(Team == 'Total', "All", FeedBack),
                                                Team = ifelse(Team == 'Total', lag(Team), Team)) %>%
                                            filter(Channel == "All"))
                   )%>%bind_rows(filter(.,Channel == 'All') %>%  adorn_totals() %>% mutate(Team = lag(Team),Debt_Collector = ifelse(Debt_Collector == "-",'All',Debt_Collector),Channel = "All",FeedBack = "All") %>% filter(Debt_Collector == "All") )
        ) %>%
        bind_rows(
            filter(.,
                   Debt_Collector == "All") %>%
                adorn_totals() %>% mutate_at(
                    vars(Team, Debt_Collector, Channel, FeedBack),
                    .funs = function(x)
                        "All") %>%
                tail(1)) %>%
        mutate(
            ReportDate = report_date
        ) %>% as_tibble()
    # save to db logic
    delete_clause(db_con,'t_ClientFeedBackSummary',all = F,is=as.character(report_date),where = "ReportDate",exec = T)
    dbWriteTable(db_con,'t_ClientFeedBackSummary',value = feedback_summary_df,append=T)
    return(feedback_summary)
}
# automation





