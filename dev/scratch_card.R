rm(list=ls())
library(janitor)
library(dplyr)
library(tidyr)
library(purrr)
library(openxlsx)
library(lubridate)
load("E:/Reports/Dashboard/May/07.05.2023/All_Dash_Data_07.05.2023.Rdata",ex <- new.env())
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
        AccountID,RepaymentAccountID,AccountName,MobileNo,National_ID = IDNo,LoanSeries,
        DisbursedOn,InstallmentDueDate,LoanAmount,AccountCloseDate,ClearBalance,ArrearsDays,
        ArrearsAmount,
        Channel
    ) %>%
    mutate_at(vars(LoanSeries,LoanAmount,ClearBalance,ArrearsDays,ArrearsAmount),as.numeric) %>%
    mutate_at(vars(InstallmentDueDate,DisbursedOn,AccountCloseDate),as.Date) %>%
    as_tibble()



# allocation file ---------------------------------------------------------

con_sqlite = DBI::dbConnect(RSQLite::SQLite(),"E:/New folder/projects/debtCollection/creds.sqlite")

Allocation1 <- tbl(con_sqlite,"debtcollection_Allocation") %>%
    mutate(year_ = year(Allocation_Date),
           month_ = month(Allocation_Date),
           National_ID = trimws(National_ID)) %>%
    filter(year_ == 2023,month_ == 4) %>%
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


Allocation <- list(Allocation1,Allocation2) %>%
    map_df(.f = function(x){
        x %>% #head() %>%
            mutate_at(
                vars(LoanSeries,Outsourced_Amount,Days_Overdue),
                as.numeric
            ) %>%
            mutate_at(
                vars(AllocationDate,Disbursement_Date,Last_Installement_Date),
                as.Date
            )
    })

current_status <- LoanBook %>%
    # filter(as.numeric(ClearBalance) > 0,
    #        ArrearsDays > 1) %>%
    select(-LoanAmount) %>%
    full_join(
        Allocation %>%
            select(
                Mobile_No,National_ID,AccountID,RepaymentAccountID = MFanisi_Account,LoanSeries,Client_Name,Outsourced_Amount,Debt_Collector,Team
            ),
        by = c("MobileNo"="Mobile_No", "AccountID", "RepaymentAccountID", "LoanSeries")
    ) %>%
    rename(
        ClurrentBalance = ClearBalance
    ) %>%
    mutate(
        Amount_Paid = Outsourced_Amount - ClurrentBalance,
        Amount_Paid = case_when(Amount_Paid < 0~0,TRUE~Amount_Paid),
        PaymentStatus = case_when(Amount_Paid >= Outsourced_Amount~"Fully_Paid",
                                  Amount_Paid < Outsourced_Amount & Amount_Paid > 0 ~ "Partially Paid",
                                  TRUE ~ "No Payment")
    ) %>%
    mutate(
        ReportDate = Sys.Date()-1
    )

branch  <- current_status %>% filter(Channel == 'BranchLoans')
mfanisi <- current_status %>% filter(Channel != 'BranchLoans')   %>%
    filter(as.numeric(ClurrentBalance) > 0,
           ClurrentBalance  > 1,
           ArrearsDays > 0)

new_for_allocation <- mfanisi %>%
    filter(is.na(National_ID.y)) %>%
    left_join(
        disputed_accounts
    ) %>% filter(is.na(Chanel))


Realocate1 <- Allocation2 %>%
    group_by(National_ID) %>%
    tally() %>%
    right_join(
        LoanBook %>%
            mutate(LoanSeries = as.character(LoanSeries)) %>%
            filter(ArrearsDays > 90,
                   ClearBalance> 0,substr(AccountID,1,4) %in% c("0018",'0019')) %>%
            group_by(National_ID) %>% tally(name = "Counts")
    ) %>%
    filter(
        Counts > `n`
    ) %>% #View('Check1') %>%
    left_join(
        LoanBook %>%
            filter(ArrearsDays > 90,
                       ClearBalance> 0,substr(AccountID,1,4) %in% c("0018",'0019')),
        by = c("National_ID")
    ) %>%
    mutate(Disbursement_Date = as.character(DisbursedOn),
           Last_Installement_Date = as.character(InstallmentDueDate)) %>%
    select(
        Counts,`n`,
         National_ID,
         Mobile_No = MobileNo,
         MFanisi_Account = RepaymentAccountID,
         AccountID,
         Client_Name = AccountName,
         Disbursement_Date,
         LoanSeries,
         Days_Overdue = ArrearsDays,
         Last_Installement_Date,
         Outsourced_Amount = ClearBalance
         ) %>% #View("Check2") %>%
    rowid_to_column() %>%
    # filter(National_ID == '31797735') %>%
    left_join(Allocation2 %>%
                  # filter(National_ID == '31797735') %>%
                  mutate(LoanSeries=as.numeric(LoanSeries)) %>%
                  select(Debt_Collector,National_ID,AccountID)) %>%
    select(rowid,Counts,`n`,
           AccountID,National_ID,Mobile_No,MFanisi_Account,Client_Name,
           Disbursement_Date,Last_Installement_Date,
           Days_Overdue,Debt_Collector,Outsourced_Amount,LoanSeries) %>%
    mutate(NotOutsourced = is.na(Debt_Collector)) %>%
    group_by(National_ID) %>%
    arrange(Debt_Collector) %>%
    left_join(
        Allocation2 %>% distinct(National_ID,.keep_all=T) %>%
            select(National_ID,AgentName = Debt_Collector)
    ) %>%
    mutate(
        Reallocate = is.na(Debt_Collector),
        Debt_Collector = ifelse(Reallocate,AgentName,Debt_Collector)
    ) %>%
    filter(Reallocate) %>%
    select(
       contains(names(Allocation2))
    ) %>%
    mutate(AllocationDate = '2023-05-8', Team = 'ExternalDebtCollectors')



new_for_allocation %>%
    select(AccountID,RepaymentAccountID,ArrearsDays) %>%
    filter(ArrearsDays > 91) %>%
    left_join(
        LoanBook %>%
            select(RepaymentAccountID,AccountID,LoanSeries,National_ID,ClearBalance) %>%
            filter(ClearBalance > 0) %>%
            mutate_all(as.character),
        by='RepaymentAccountID') %>%
    left_join(
        Allocation2 %>% select(AccountID.y = AccountID,LoanSeries,Debt_Collector)
    ) %>% View()






# to_recal <- current_status %>%
#     filter(ClurrentBalance > 0) %>%
#     group_by(National_ID=IDNo) %>% tally() %>% #nrow()
#     right_join(
#         Allocation %>% count(National_ID,name="Check")
#     ) %>% #nrow()
#     filter(is.na(`n`)) %>%
#     distinct(National_ID,.keep_all = T)

outstandning <- read.xlsx("E:/external_debt_collectors/outstanding/2023/May/04.05.2023/Outstanding_04.05.2023_.xlsx")

Allocation2 %>%
    select(IDNo=National_ID,AccountID,Mobile_No,MFanisi_Account,Client_Name) %>%
    mutate(Data = "Allocation") %>%
    right_join(
        LoanBook %>%
            filter(ClearBalance > 0) %>%
            # distinct(AccountID,.keep_all=T) %>%
            select(AccountID,RepaymentAccountID,National_ID,MobileNo,AccountName),
        by=c('AccountID')
    ) %>%
    select(
        AccountID,MFanisi_Account,RepaymentAccountID,IDNo,National_ID,Mobile_No,MobileNo,
        Client_Name,AccountName
    ) %>%
    filter(
        trimws(IDNo) != trimws(National_ID)
        ) %>%
    # View("Check") %>%
    select(
        AccountID,IDNo,CorrectID = National_ID,
    ) %>%
    right_join(
        outstandning
    ) %>%
    filter(!is.na(IDNo)) -> to_recal


# source("E:/Automation/resources/email_push.R")
# contacts <- read.xlsx("E:/external_debt_collectors/contact/contactlist.xlsx")
# to_recal %>%
#     select(
#         AccountID,National_ID,Client_Name,Mobile_No,Outsourced=Outsourced_Amount,
#         Outstanding=Outstanding_Amount,Debt_Collector
#     ) %>%
#     split(.$Debt_Collector) %>%
#     map2(.y = names(.),.x = .,.f = function(x=.x,y=.y){
#         if(y != "Deleted_Cases"){
#             contacts_to = contacts %>% filter(Name == y)
#             maisha_email_push2(cc = unlist(strsplit(contacts_to$cc,",")),
#                                Recipient = "",
#                                to = contacts_to$email,
#                                Subject = glue("Account{ifelse(nrow(x) > 1,'s','')} Recall"),
#                                message = glue("
#
# Kindly note that I would like to recall the account{ifelse(nrow(x) > 1,'s','')} listed below for investigation purposes.
#
# {flextable::flextable(x) %>% htmltools_value()}
#
# You are therefore requered to close {ifelse(nrow(x) > 1,'them','it')} from your system.
#                           "),
#                                Sender = "George",test = F,
#                                from = 'george.oduor@maishabank.com',
#                                credfile = normalizePath( 'E:/Automation/resources/george'))
#         }
#     })


# realocate <- to_recal %>%
#     distinct(AccountID,.keep_all = T) %>%
#     select(AccountID,CorrectID) %>%
#     inner_join(LoanBook) %>%
#     mutate(Disbursement_Date = as.character(DisbursedOn)) %>%
#     mutate(Last_Installement_Date = as.character(InstallmentDueDate)) %>%
#     select(
#         National_ID,
#         Mobile_No = MobileNo,
#         MFanisi_Account = RepaymentAccountID,
#         AccountID,
#         Client_Name = AccountName,
#         Disbursement_Date,
#         LoanSeries,
#         Days_Overdue = ArrearsDays,
#         Last_Installement_Date,
#         Outsourced_Amount = ClearBalance
#     ) %>% filter(Outsourced_Amount > 1) %>%
#     mutate(
#         Debt_Collector = 'Itothya Agencies',
#         AllocationDate = '2022-01-01',
#         LoanSeries = as.character(LoanSeries)
#     ) %>%
#     select(contains(names(Allocation2)))

# write.xlsx(realocate,"E:/external_debt_collectors/allocation/2023/May/Itothya Agencies_May_Allocation2.xlsx")
# read.xlsx("E:/external_debt_collectors/allocation/2023/May/01.05.2023/Allocation_Apr_CarryForward.xlsx") %>%
#     filter(!AccountID %in% unique(to_recal$AccountID)) %>%
#     # select(-Team) %>%
#     # nrow()
#     bind_rows(realocate) %>%
#     write.xlsx("E:/external_debt_collectors/allocation/2023/May/01.05.2023/Allocation_Apr_CarryForward_cleaned.xlsx")

# read.xlsx("E:/external_debt_collectors/allocation/2023/May/01.05.2023/Allocation_Apr_CarryForward_cleaned.xlsx") %>%
#     group_by(Debt_Collector) %>% tally() %>%
#     left_join(Allocation2 %>%
#                   filter(AllocationDate != '2023-05-03') %>%
#                   group_by(Debt_Collector) %>%
#                   tally(name = 'prev')) %>%
#     mutate(change = prev - `n`) %>%
#     adorn_totals('row')

# write.xlsx("E:/Reports/improptu/May/test.xlsx")
