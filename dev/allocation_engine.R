rm(list=ls())
library(janitor)
library(dplyr)
library(tidyr)
library(purrr)
library(openxlsx)
library(lubridate)
library(glue)
devtools::load_all()
report_date = Sys.Date()-1
load(glue("E:/Reports/Dashboard/May/{format(report_date,'%d.%m.%Y')}/All_Dash_Data_{format(report_date,'%d.%m.%Y')}.Rdata"),ex <- new.env())
largetrx = ex$largeTransactions %>% filter(year(TrxDate)==2023,month(TrxDate)==5) %>% select(AccountID,TrxDate,Credit,Debit)
disputed_accounts <- read.xlsx("E:/external_debt_collectors/Disputes/consolidated_disputes.xlsx")
# loanbook ----------------------------------------------------------------
process_loan_data <- function(loan_list,con=NULL) {

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
                is.na(AccountCloseDate) ~ as.numeric(Sys.Date() - as.Date(InstallmentDueDate)),
                TRUE ~ as.numeric(ArrearsDays)
            ),
            ArrearsDays = case_when(ArrearsDays < 0 ~ 0,
                                    TRUE ~ ArrearsDays)
        ) %>%
        select(
            ClientID, AccountID, RepaymentAccountID, AccountName, MobileNo, National_ID = IDNo, LoanSeries,
            DisbursedOn, InstallmentDueDate, LoanAmount, AccountCloseDate, ClearBalance, ArrearsDays,
            ArrearsAmount, Channel,LastRepaymentDate,LastRepaymentAmount
        ) %>%
        mutate_at(vars(LoanSeries, LoanAmount, ClearBalance, ArrearsDays, ArrearsAmount), as.numeric) %>%
        mutate_at(vars(InstallmentDueDate, DisbursedOn, AccountCloseDate), as.Date) %>%
        # filter(ProductID %in% product_ids) %>%
        as_tibble()

    if (!is.null(con)) {
        dbWriteTable(con,"t_LoanBook",value = Loanbk,overwrite=T)

    }

}
process_loan_data(ex$Loan_List2,con = db_con)
# allocation file ---------------------------------------------------------
refresh <- F
if (refresh) {
    sess = ssh::ssh_connect("root@172.16.200.23",passwd = "Taka.Tena89#")
    ssh::scp_download(sess,files = "/srv/shiny-server/debtcollection/creds.sqlite",to = "E:\\New folder\\projects/debtCollection/")
}

con_sqlite = DBI::dbConnect(RSQLite::SQLite(),"E:/New folder/projects/debtCollection/creds.sqlite")

alocation_files <- function(con=NULL) {
    Allocation1 <- tbl(con_sqlite,"debtcollection_Allocation") %>%
        mutate(year_ = year(Allocation_Date),
               month_ = month(Allocation_Date),
               National_ID = trimws(National_ID)) %>%
        filter(year_ == 2023,month_ == 5,as.Date(Allocation_Date <= !!report_date)) %>%
        collect() %>%
        select(
            National_ID,Client_Name,MFanisi_Account,AccountID,Mobile_No,Disbursement_Date,Last_Installement_Date,
            Outsourced_Amount=Loan_Balance,Days_Overdue,Debt_Collector=Call_Agent,LoanSeries,AllocationDate=Allocation_Date
        )%>%
        mutate(
            Team = "InternalDebtCollector"
        )

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
    Allocation <- list(Allocation1, Allocation2) %>%
        map_df(~ .x %>%
                   mutate(across(c(LoanSeries, Outsourced_Amount, Days_Overdue), as.numeric),
                          across(c(AllocationDate, Disbursement_Date, Last_Installement_Date), as.Date))
        )
    if (!is.null(con)) {
        dbWriteTable(con,"t_Allocation",Allocation,overwrite=T)
    }
    return(Allocation)
}
# Allocation <-
alocation_files(con=db_con)
# Outstanding File --------------------------------------------------------

get_current_status <- function(Allocation, LoanBook,report_date,writedb=NULL) {
    current_status <- Allocation %>%
        select(
            Mobile_No, National_ID, AccountID, RepaymentAccountID = MFanisi_Account,
            LoanSeries, Client_Name, Outsourced_Amount, Debt_Collector, Team, AllocationDate
        ) %>%
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
            CurrentBalance = ifelse(is.na(CurrentBalance), 0, CurrentBalance),
            Amount_Paid = Outsourced_Amount - CurrentBalance,
            Amount_Paid = case_when(Amount_Paid < 0 ~ 0, TRUE ~ Amount_Paid)
        ) %>%
        mutate(
            ReportDate = (report_date)
        )
    if (writedb) {
        dbWriteTable(db_con,"t_Outstanding_prov",collect(current_status),overwrite=T)
    }
    return(current_status)
}
get_current_status(Allocation=tbl(db_con,"t_Allocation"),
                   LoanBook = tbl(db_con,"t_LoanBook"),
                   report_date,writedb = T) %>% invisible()

check <- function(current_status, prev_status) {

    # Calculate summary statistics for both data frames
    current_summary <- current_status %>%
        summarise(
            Count = n(),
            Outsourced_Amount = sum(Outsourced_Amount),
            data = "Current_status"
        ) %>% collect()

    prev_summary <- prev_status %>%
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
check(tbl(db_con,"t_Outstanding_prov"),prev_status=tbl(db_con,"t_Allocation")) %>% invisible()

# create_repayments_file --------------------------------------------------
repayment <- tbl(db_con,"t_Outstanding_prov") %>%
    select(
        ClientID,AccountID,LoanSeries,Amount_Paid
    ) %>%
    filter(Amount_Paid> 0) %>%
    mutate(RepaymentDate = report_date)

get_today_payments <- function(db_con, repayment, report_date,dbwrite=NULL) {
    existing_repayment <- tbl(db_con, 't_Repayment')
    last_repayment_update <- existing_repayment %>%
        filter(RepaymentDate == max(RepaymentDate, na.rm = TRUE)) %>%
        distinct(RepaymentDate) %>% pull(RepaymentDate)
    if (length(last_repayment_update)>0) {
        if (last_repayment_update == report_date ) {
            dbSendQuery(db_con,statement = glue("DELETE FROM \"t_Repayment\" WHERE \"RepaymentDate\" = '{report_date}' "))
        }
    }

    todays_payment <- tbl(db_con, 't_Repayment') %>%
        filter(year(RepaymentDate)==!!year(report_date),
               month(RepaymentDate)==!!month(report_date)) %>%
        group_by(ClientID,AccountID,LoanSeries) %>%
        window_order(RepaymentDate) %>%
        mutate(Amount_Paid = cumsum(Amount_Paid)) %>%
        filter(RepaymentDate == max(RepaymentDate, na.rm = TRUE),
               # AccountID == '0019040004768'
               ) %>%
        # collect() %>%
        full_join(repayment, by = c("ClientID", "AccountID", "LoanSeries")) %>%
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
        select(-Amount_Paid.y) %>% collect() %>%
        reshape2::melt(
            id.vars = c('ClientID', 'AccountID', 'LoanSeries', 'RepaymentDate.x', 'RepaymentDate.y')
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
        arrange(Amount_Paid) %>%
        as_tibble()

    if (dbwrite) {
        dbWriteTable(db_con,'t_Repayment',todays_payment,append=T)
    }
    return(todays_payment)
}
get_today_payments(db_con,repayment ,report_date,dbwrite = T) #%>%
    # filter(AccountID == '0019040004768')

get_outstanding_file <- function(db_con, report_date) {

    # Check if required input arguments are provided
    if (!missing(db_con) && !missing(report_date)) {

        tryCatch({
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
                    PaymentStatus = case_when(
                        Amount_Paid >= Outsourced_Amount ~ "Fully_Paid",
                        Amount_Paid < Outsourced_Amount & Amount_Paid > 0 ~ "Partially Paid",
                        TRUE ~ "No Payment"
                    )
                ) %>%
                select(
                    ClientID, National_ID, AccountID, RepaymentAccountID, Mobile_No, LoanSeries,
                    DisbursedOn, InstallmentDueDate, Outsource_Date = AllocationDate,
                    Outsourced_Amount,CurrentBalance, Amount_Paid, Debt_Collector, Team
                ) %>%
                # rowid_to_column(var = "id") %>%
                mutate(
                    date_created = report_date,
                    CreatedBy = "George"
                )

            # Add log message
            message("Outstanding file data retrieved successfully.")

            return(outstanding_file)

        }, error = function(e) {
            # Add log message
            message(paste("Error in get_outstanding_file():", conditionMessage(e)))
            # Return error message
            return("Error: Unable to retrieve outstanding file data.")
        })

    } else {
        # Return error message if required input arguments are missing
        return("Error: Missing required input arguments.")
    }
}

outstanding_fil <- get_outstanding_file(db_con,report_date = report_date)

outstanding_fil %>%
    group_by(Team) %>%
    summarise(Amount_Paid=sum(Amount_Paid))

tbl(db_con,"t_Outstanding_prov") %>%
    group_by(Team) %>%
    summarise(
        Count = n(),
        Outsourced_Amount = sum(Outsourced_Amount,na.rm = T),
        Amount_Paid = sum(Amount_Paid,na.rm = T),
        CurrentBalance = sum(CurrentBalance,na.rm = T)
    ) %>% collect() %>%
    mutate_all(format,big.mark=",")

dbWriteTable(db_con,"t_Outstanding",collect(outstanding_fil),overwrite=T)

outstanding_file <- tbl(db_con,"t_Outstanding") %>%
    select(
        AccountID,LoanSeries,Outsourced_Amount,CurrentBalance,Debt_Collector,Team
    ) %>%
    left_join(
        tbl(db_con,"t_Repayment") %>%
            group_by(AccountID,LoanSeries) %>%
            summarise(
                Amount_Paid = sum(Amount_Paid,na.rm=T)
            ) %>% ungroup(),
        by = c("AccountID", "LoanSeries")
    ) %>% invisible()


# collections_vs_repayments -----------------------------------------------

collections <- tbl(db_con,"t_Repayment") %>%
    filter(year(RepaymentDate) == !!year(report_date),
           month(RepaymentDate) == !!month(report_date)
    ) %>%
    mutate(id = row_number(),
           Channel = case_when(
               substr(AccountID,1,4) == "0019" ~ "MfanisiSafaricom",
               TRUE ~ "MfanisiAirtel"
           )) %>%
    inner_join(tbl(db_con,"t_Outstanding") %>%
                   select(AccountID,LoanSeries,Debt_Collector,Team),
               by = c("AccountID", "LoanSeries")) %>%
    group_by(RepaymentDate,Team,Debt_Collector,Channel) %>%
    arrange(RepaymentDate) %>%
    # collect() %>%
    summarise(
        Amount = sum(Amount_Paid,na.rm = T)
    ) %>% ungroup() %>%
    group_by(Team,Debt_Collector,Channel) %>%
    mutate(
        RepaymentTrend = round((Amount - lag(Amount))/lag(Amount)*100,2),
        UpdatedBy = "George",
        Category = "DailyTrend"
    ) %>% ungroup()

# daily collection summaries --------------------------------------------------------
collection_trend <- tbl(db_con,"t_CollectionTrend")
last_update <- collection_trend %>%
    filter(RepaymentDate==max(RepaymentDate)) %>%
    pull(RepaymentDate) %>% last()

if (last_update == "") {
    last_update
}
daily_collection_all <- collections %>%
    arrange(RepaymentDate) %>%
    group_by(Channel,RepaymentDate) %>%
    summarise(
        Amount = sum(Amount,na.rm = T)
    ) %>%
    mutate(
        RepaymentTrend = round((Amount - lag(Amount))/lag(Amount)*100,2),
        Team = "All",
        Debt_Collector = "All",
        UpdatedBy = "George",
        Category = "DailyTrend"
    ) %>%
    select(RepaymentDate,everything())


# collectorc daily_collections -------------------------------------------

collector_team_trend <- collections %>%
    ungroup() %>%
    group_by(RepaymentDate,Team,Channel) %>%
    summarise(
        Amount = sum(Amount,na.rm = T)
    )  %>%
    ungroup() %>%
    arrange(Channel,Team,RepaymentDate) %>%
    group_by(Channel,Team) %>%
    mutate(
        RepaymentTrend = round((Amount - lag(Amount))/lag(Amount)*100,2),
        Debt_Collector = "All",
        UpdatedBy = "George",
        Category = "DailyTrend"
    ) %>% #colnames()
    select(
        RepaymentDate,Channel,Team,Debt_Collector,Amount,RepaymentTrend,UpdatedBy,Category
    )

# end of month ------

if ((ceiling_date(report_date,unit = "month")-1)==report_date) {
    endofmonth_all <- collections %>%
        ungroup() %>%
        group_by(Channel,Team,Debt_Collector) %>%
        summarise(
            RepaymentDate = report_date,
            Amount = sum(Amount,na.rm = T)
        ) %>%
        mutate(
            UpdatedBy = "George",
            Category = "CurrentMonthCollection",
            RepaymentTrend = round((Amount - lag(Amount))/lag(Amount)*100,2),
        ) %>%
        collect()
    dbWriteTable(db_con,"t_EndOfMonthCollectionTrend",endofmonth_all,append=T)

}




colections_trend <- rbind(
    collections %>% collect(),
    daily_collection_all %>% collect(),
    collector_team_trend %>% collect()
)

dbWriteTable(db_con,"t_CollectionTrend",colections_trend,append=T)



