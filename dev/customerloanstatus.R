library(mbanalytics)
client_loan_performance <- function(Loan_List2,Mobile) {
        client_loan_performance <- Loan_List2 %>%
            filter(MobileNo == Mobile) %>%
            select(
                AccountID,
                LoanSeries,
                AccountName,
                IDNo,
                MobileNo,
                LoanAmount,
                DisbursedOn,
                InstallmentDueDate,
                AccountCloseDate,
                ArrearsDays
            ) %>%
            mutate_at(
                vars(DisbursedOn ,AccountCloseDate,InstallmentDueDate),as.Date
            )  %>%
            mutate(
                ArrearsDays = ifelse(is.na(AccountCloseDate),ArrearsDays,as.Date(AccountCloseDate) - as.Date(InstallmentDueDate)),
                ArrearsDays = ifelse(ArrearsDays < 0,0,ArrearsDays),
                IDNo = gsub("ONFON_", "", IDNo),
                MaxArrearsDays = paste(max(ArrearsDays),"days"),
                LatestArrearsDays = paste(ifelse(DisbursedOn == max(DisbursedOn),ArrearsDays,0),"days"),
                Tenure = max(DisbursedOn)-min(DisbursedOn),
                DefaultRate = paste(round(sum(ArrearsDays > 90)/n()* 100,2),"%","(",sum(ArrearsDays > 90),"bad",",",n()-sum(ArrearsDays > 90),"good",")"),
                MaxLoanTaken = paste("Ksh",format(max(as.numeric(LoanAmount)),big.mark=","))
            )

        client_loan_performance %>%
            tail(1) %>%
            select(
                ClientName = AccountName,
                Tenure = Tenure,
                NumberOfLoansTaken = LoanSeries,
                DefaultRate,
                MaxLoanTaken,
                LatestArrearsDays,
                MaximumArrearsDays = MaxArrearsDays
            ) %>%
            t() %>%
            as_tibble(rownames = 'KPI') %>%
            # rename('Value'='8') %>%
            clean_names() %>%
            kable() %>%
            kable_styling(font_size = 15,full_width = F)



    }
client_loan_performance(Loan_List2=feather::read_feather('E:/Reports/Dashboard/Jun/06.06.2023/ActiveClosedLoanBook06.06.2023.feather'),
                        '254735628005')
