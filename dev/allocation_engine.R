rm(list=ls())
library(janitor)
library(dplyr)
library(tidyr)
library(purrr)
library(openxlsx)
library(lubridate)
report_date = Sys.Date()-1
load(glue("E:/Reports/Dashboard/May/{format(report_date,'%d.%m.%Y')}/All_Dash_Data_{format(report_date,'%d.%m.%Y')}.Rdata"),ex <- new.env())
disputed_accounts <- read.xlsx("E:/external_debt_collectors/Disputes/consolidated_disputes.xlsx")
# loanbook ----------------------------------------------------------------


LoanBook <- ex$Loan_List2 %>%
    mutate(Channel = case_when(grepl("MFC", ProductID) ~ "MfanisiAirtel",
                               ProductID == "MLS904" ~ "MfanisiSafaricom",
                               TRUE ~ "BranchLoans"),
           IDNo2 = IDNo) %>%
    split(.$Channel) %>%
    map2_df(.y = names(.),.x = .,.f = function(x=.x,y=.y){
        if(grepl("Mfanisi",y)){
            x  %>%
                # filter(RepaymentAccountID == '0018010031026') %>%
                separate(IDNo,into = c('Mfanisi','Ref','IDNo'),sep="_")
        }else{
            x #%>% head(0)
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
        ClientID,AccountID,RepaymentAccountID,AccountName,MobileNo,National_ID = IDNo,LoanSeries,
        DisbursedOn,InstallmentDueDate,LoanAmount,AccountCloseDate,ClearBalance,ArrearsDays,
        ArrearsAmount,
        Channel
    ) %>%
    mutate_at(vars(LoanSeries,LoanAmount,ClearBalance,ArrearsDays,ArrearsAmount),as.numeric) %>%
    mutate_at(vars(InstallmentDueDate,DisbursedOn,AccountCloseDate),as.Date) %>%
    as_tibble()



# allocation file ---------------------------------------------------------
refresh <- F
if (refresh) {
    sess = ssh::ssh_connect("root@172.16.200.23",passwd = "Taka.Tena89#")
    ssh::scp_download(sess,files = "/srv/shiny-server/debtcollection/creds.sqlite",to = "E:\\New folder\\projects/debtCollection/")
}
con_sqlite = DBI::dbConnect(RSQLite::SQLite(),"E:/New folder/projects/debtCollection/creds.sqlite")

Allocation1 <- tbl(con_sqlite,"debtcollection_Allocation") %>%
    mutate(year_ = year(Allocation_Date),
           month_ = month(Allocation_Date),
           National_ID = trimws(National_ID)) %>%
    filter(year_ == 2023,month_ == 5) %>%
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

# Outstanding File --------------------------------------------------------

current_status <- Allocation %>%
    # filter(National_ID =='21468450') %>%
    select(
        Mobile_No,National_ID,AccountID,RepaymentAccountID = MFanisi_Account,
        LoanSeries,Client_Name,Outsourced_Amount,Debt_Collector,Team
    )  %>%
    left_join(
        LoanBook %>%
            # filter(National_ID =='21468450') %>%
            select(-LoanAmount) ,
            filter(grepl("Mfanisi",Channel)),
        by = c("Mobile_No"="MobileNo", "AccountID", "RepaymentAccountID", "LoanSeries",'National_ID')
    ) %>%
    rename(
        CurrentBalance = ClearBalance
        ) %>%
    mutate(
        CurrentBalance = ifelse(is.na(CurrentBalance),0,CurrentBalance),
        Amount_Paid = Outsourced_Amount - CurrentBalance,
        Amount_Paid = case_when(Amount_Paid < 0~0,TRUE~Amount_Paid)
    ) %>%
    mutate(
        ReportDate = Sys.Date()-1
    )

check <- current_status %>%
    summarise(
        Count = n(),
        Outsourced_Amount = sum(Outsourced_Amount),
    ) %>%
    mutate(
        data = "Current_status"
    ) %>%
    bind_rows(Allocation %>%
                  summarise(
                      Count = n(),
                      Outsourced_Amount = sum(Outsourced_Amount)
                  ) %>%
                  mutate(
                      data = "prev_status"
                  )  ) %>%
    t() %>%
    as_tibble(.name_repair = 'minimal',rownames = "data") %>%
    clean_names() %>%
    arrange(desc(x_2)) %>%
    row_to_names(1) %>%
    mutate(
        Check = Current_status == prev_status
    )

if (all(check$Check)) {
    print("All is well up to this point")
} else {
    wrong_idx <- which(!check$Check)
    wrong_data <- check$data[wrong_idx]

    if (length(wrong_data) == 2) {
        print("Both Count and Outsourced Amount are wrong")
    } else if (wrong_data == "Outsourced_Amount") {
        print("Outsourced Amount is wrong")
    } else {
        print("Count is wrong")
    }
}


current_status %>%
    group_by(
        Team#,Debt_Collector
    ) %>%
    summarise(
        Count = n(),
        Outsourced_Amount = sum(Outsourced_Amount,na.rm = T),
        Amount_Paid = sum(Amount_Paid,na.rm = T),
        CurrentBalance = sum(CurrentBalance,na.rm = T)
    ) %>%
    mutate_all(format,big.mark=",")


# create_repayments_file --------------------------------------------------

repayment <- current_status %>%
    select(
        ClientID,AccountID,LoanSeries,Amount_Paid
    ) %>%
    filter(Amount_Paid> 0) %>%
    mutate(RepaymentDate = report_date)

todays_payment <- tbl(db_con,'Repayment') %>%
    filter(
        RepaymentDate == max(RepaymentDate,na.rm = T)
    ) %>%
    collect() %>%
    full_join(
        repayment,
        by = c("ClientID", "AccountID", "LoanSeries")
    ) %>%
    mutate_at(vars(Amount_Paid.x,Amount_Paid.y),.funs = function(x){
        ifelse(is.na(x),0,x)
    }) %>%
    mutate(
        New_AmountPaid = case_when(
            Amount_Paid.x > 0 & Amount_Paid.y == 0 ~ 0,
            TRUE ~ Amount_Paid.y - Amount_Paid.x
        )
    ) %>%
    select(-Amount_Paid.y) %>%
    reshape2::melt(
        id.vars=c('ClientID','AccountID','LoanSeries','RepaymentDate.x','RepaymentDate.y')
    ) %>%
    filter(
        value > 0
    ) %>%
    mutate(
        RepaymentDate = case_when(
            variable == 'New_AmountPaid'~RepaymentDate.y,
            TRUE ~ RepaymentDate.x
        )
    ) %>%
    select(-RepaymentDate.x,-RepaymentDate.y,-variable) %>%
    filter(RepaymentDate == report_date) %>%
    rename("Amount_Paid"='value') %>%
    arrange(Amount_Paid)


outstanding_file <- rbind(
    tbl(db_con,"Repayment")%>%
        filter(year(RepaymentDate)== !!year(report_date),
               month(RepaymentDate) == !!month(report_date)) %>%
        collect(),
    todays_payment
    ) %>%
    group_by(AccountID,LoanSeries) %>%
    summarise(
        Count = n(),
        Amount = sum(Amount_Paid)) %>%
    ungroup() %>%
    arrange(
        desc(Count),desc(Amount)
        )%>%
    right_join(
        current_status,
        by = c("AccountID", "LoanSeries")
        ) %>%
    mutate(
        Amount_Paid = case_when(Amount > Amount_Paid & Team == "InternalDebtCollector" ~ Amount,
                                TRUE ~ Amount_Paid),
        PaymentStatus = case_when(Amount_Paid >= Outsourced_Amount~"Fully_Paid",
                                  Amount_Paid < Outsourced_Amount & Amount_Paid > 0 ~ "Partially Paid",
                                  TRUE ~ "No Payment")
        )

dbWriteTable(db_con,'Repayment',todays_payment,append=T)
