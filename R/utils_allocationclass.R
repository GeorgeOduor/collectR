#' Get the files required for allocation.
#'
#' @return A list containing the "outstandingfile" and "loanbook" data frames.
get_scoring_files = function(report_date) {
  outstandingfile <- tbl(self$db_con, "t_Outstanding") %>% collect()
  loanbook <- tbl(db_con, "t_LoanBook") %>%
    dplyr::filter(ArrearsDays >= 1,
                  Channel != "BranchLoans",
                  as.numeric(ClearBalance) >= 1) %>%
    ungroup() %>%
    collect() %>%
    mutate(
      ArrearsDays = case_when(
        !is.na(AccountCloseDate) &
          as.numeric(report_date - as.Date(InstallmentDueDate)) > 90 &
          ArrearsDays < 90 ~ as.numeric(report_date - as.Date(InstallmentDueDate)),
        TRUE ~ ArrearsDays
      ),
      Team_New = case_when(
        as.numeric(ArrearsDays) >= 1 &
          as.numeric(ArrearsDays) <= 90 ~ "InternalDebtCollector",
        TRUE ~ "ExternalDebtCollector"
      )
    )

  return(list(outstandingfile = outstandingfile,
              loanbook = loanbook))
}

#' Get the allocation file for debt collectors.
#'
#' @return A list containing the "carried_foward" and "new_cases" data frames.
get_allocation_file = function(data,report_date) {
  stopifnot(is.list(data))
  loanbook <- data %>% pluck('loanbook')
  outstanding_fil <- data %>% pluck('outstandingfile')
  disputed_accounts <- tbl(db_con, "t_DisputedAccounts") %>% collect()
  allocation_file <- loanbook %>%
    dplyr::left_join(disputed_accounts, by = c("AccountID", "LoanSeries", "Channel")) %>%
    dplyr::filter(is.na(CreatedOn)) %>%
    dplyr::left_join(
      outstanding_fil %>%
        dplyr::select(ClientID, AccountID, LoanSeries, DisbursedOn, InstallmentDueDate, Debt_Collector, Team, ArrearsDays2 = ArrearsDays),
      by = c("ClientID", "AccountID", "LoanSeries", "DisbursedOn", "InstallmentDueDate")
    ) %>%
    collect() %>%
    mutate(
      Check = case_when(
        Team == Team_New ~ "CarriedForward",
        TRUE ~ "New Cases"
      ),
      IsEndMonth = (ceiling_date(report_date, "month") - 1) == report_date,
      Team = case_when(
        ArrearsDays < 90 ~ "InternalDebtCollector",
        TRUE ~ "ExternalDebtCollector"
      )
    )

  # carried forward accounts
  cf <- allocation_file %>% dplyr::filter(Check == 'CarriedForward' & !is.na(Debt_Collector))
  new_cases <- allocation_file %>% dplyr::filter(Check != 'CarriedForward' | is.na(Debt_Collector),
                                                 ArrearsDays > 0)

  return(list(
    carried_foward = cf,
    new_cases = new_cases
  ))
}

#' Perform account allocation to internal team based on outsource method.
#'
#' @param agents A character vector of internal debt collectors' names.
#' @param dt The allocation data frame.
#' @param outsource_method The outsource method, either "equal" or "ratio".
#' @param ratio The allocation ratio for each agent (optional, required for "ratio" outsource method).
#' @param test Logical indicating whether to run a test mode (default is TRUE).
#' @return The allocation data frame.
internal_team_outsource_new = function(id_col,agents, dt, outsource_method = "equal", ratio = NULL, test = TRUE) {
  stopifnot(is.list(dt))
  dt <- dt %>% pluck('new_cases') %>% dplyr::filter(Team == 'InternalDebtCollector')
  new_agents_int <- self$accounts_distributor(id_col = id_col, dt = dt, agents = agents,
                                              outsource_method = outsource_method,
                                              ratio = ratio)
  Allocation <- dt %>%
    dplyr::select(all_of(id_col), everything()) %>% dplyr::select(-Debt_Collector) %>%
    dplyr::left_join(new_agents_int, by = id_col)
  if (test) {
    Allocation <- Allocation %>% janitor::tabyl(Debt_Collector)
  }
  return(Allocation)
}

#' Perform account allocation to internal team for carried forward accounts based on outsource method.
#'
#' @param dt The allocation data frame.
#' @param outsource_method The outsource method, either "equal" or "ratio".
#' @param test Logical indicating whether to run a test mode (default is TRUE).
#' @return The reallocated data frame.
internal_team_outsource_cf = function(id_col,dt, outsource_method = "equal", test = TRUE) {
  stopifnot(is.list(dt))
  dt <- dt %>% pluck('carried_foward') %>% dplyr::filter(Team == 'InternalDebtCollector')
  agents <- unique(dt$Debt_Collector)
  new_agents_int <- self$accounts_distributor(id_col, dt, agents, outsource_method)
  realocated <- dt %>%
    rename(Previous_Collector = Debt_Collector) %>%
    dplyr::left_join(new_agents_int, by = id_col)
  if (test) {
    realocated = realocated %>%
      janitor::tabyl(Previous_Collector, Debt_Collector)
  }
  return(realocated)
}
#' Perform account allocation to external team based on outsource method.
#'
#' @param dt The allocation data frame.
#' @param outsource_method The outsource method, either "equal" or "ratio".
#' @param top_n The number of top-performing agents to consider.
#' @param manual_agents A list of manual agents to consider for each channel (optional).
#' @param manual_ratio A list of manual allocation ratios for each channel (optional).
#' @param test Logical indicating whether to run a test mode (default is TRUE).
#' @return The allocation data frame.
external_team_outsource = function(id_col,dt, outsource_method, top_n = 3, manual_agents = NULL,
                                   manual_ratio = NULL, test = TRUE) {
  stopifnot(is.list(dt))
  agents <- get_latest_month_perf(top_n)
  dt <- dt %>% pluck('new_cases') %>% dplyr::filter(Team == 'ExternalDebtCollector', !is.na(Debt_Collector))
  # Allocation file
  DFAllocation <- agents %>%
    purrr::map2_df(., names(.), .f = function(x, y) {
      ratio <- x$AllocationRatios
      agents <- x$Debt_Collector

      if (!is.null(manual_agents)) {
        if (is.null(names(manual_agents))) {
          stop('List must be named')
        }
        agents <- manual_agents %>% pluck(y)
      }

      if (!is.null(manual_ratio)) {
        ratio <- manual_ratio %>% pluck(y)
      }

      key_df <- dt %>% dplyr::filter(Channel == y)
      AllocationFile <- self$accounts_distributor(id_col, key_df, agents, outsource_method, ratio = ratio)

      # Assign cases per channel per agent
      dt <- dt %>%
        dplyr::select(-Debt_Collector) %>%
        dplyr::select(!!id_col, everything()) %>%
        dplyr::left_join(AllocationFile, by = id_col)

      if (test) {
        dt <- dt %>% janitor::tabyl(Debt_Collector)
      }

      dt
    })

  return(DFAllocation)
}
#' Get the latest month's performance for external debt collectors.
#'
#' @param top_n The number of top-performing agents to consider.
#' @return A list of data frames containing allocation ratios for each channel.
get_latest_month_perf = function(top_n) {
  # Get previous month allocation
  agents <- tbl(db_con, 't_EndOfMonthCollectionTrend') %>%
    dplyr::filter(Team == 'ExternalDebtCollector', RepaymentDate == max(RepaymentDate, na.rm = TRUE)) %>%
    mutate(RecoveryRate = Recovered_Amount / Outsource_Amount) %>%
    dplyr::select(Channel, Debt_Collector, RecoveryRate) %>% collect() %>%
    split(.$Channel) %>%
    purrr::map(.f = function(x) {
      top_performing <- head(x, top_n)
      ratios <- top_performing$RecoveryRate
      total_ratio <- sum(ratios)

      if (total_ratio != 1) {
        ratios <- round(ratios / total_ratio, 2)
      }

      top_performing %>%
        mutate(AllocationRatios = ratios) %>%
        ungroup() %>%
        dplyr::select(Debt_Collector, AllocationRatios)
    })

  return(agents)
}




