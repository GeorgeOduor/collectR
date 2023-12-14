outstanding_fil <- tbl(db_con,"t_Outstanding")
conditions <- list(
  'LoanSeries' = list(
    min = 1, max = 3
  ),
  'ArrearsDays' = list(
    min = 1, max = 30
  ),
  'CurrentBalance' = list(
    min = 1000, max = 2000
  ),
  'Outsource_Date' = list(
    min = Sys.Date() - 20, max = Sys.Date()
  ),
  'Channel' = c('MfanisiSafaricom', 'MfanisiAirtel'),
  'Repayment_Status' = c('Partially_Paid', 'No_Payment', 'Fully_Paid')
)
apply_conditions <- function(data, conditions) {
  data <- data %>%
    select(AccountID, ArrearsDays, LoanSeries, Channel, Repayment_Status, CurrentBalance, Outsource_Date) %>%
    filter(
      between(LoanSeries, !!conditions$LoanSeries$min, !!conditions$LoanSeries$max),
      between(ArrearsDays, !!conditions$ArrearsDays$min, !!conditions$ArrearsDays$max),
      between(CurrentBalance, !!conditions$CurrentBalance$min, !!conditions$CurrentBalance$max),
      between(Outsource_Date, !!conditions$Outsource_Date$min, !!conditions$Outsource_Date$max)
    )

  if (!is.null(conditions$Channel)) {
    data <- data %>% filter(Channel %in% !!conditions$Channel)
  }

  if (!is.null(conditions$Repayment_Status)) {
    data <- data %>% filter(Repayment_Status %in% !!conditions$Repayment_Status)
  }

  data <- data %>% collect() %>% pull(AccountID)

  data
}

filtered_account_ids <- apply_conditions(outstanding_fil, conditions)





