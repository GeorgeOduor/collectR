rm(list = ls())
require(httr)
library(glue)
library(janitor)
library(dplyr)
library(purrr)
library(tibble)
library(tidyr)
library(XLConnect)

headers = c(
    `Accept` = "application/json, text/plain, */*",
    `Accept-Language` = "en-US,en;q=0.9",
    `Authorization` = "Basic QzFZRlN5ZUp2MDRDenZwMkp1TGdMY0ZqZjRTMGRueXA6ejhrTHNMd2NFV1NsWnhmbQ=="
)

res <- httr::POST(url = "https://v2.analytics.metropol.co.ke:51510/map/v1/authenticate?username=eric.mshila@maishabank.com&password=Riswa2024!&grant_type=password",
                  httr::add_headers(.headers=headers))
res$status_code
access_token <- res %>% content() %>% pluck('access_token')
# check if the client had been fetched


# identity data=====
headers = c(
    `Accept` = "application/json",
    `Accept-Language` = "en-US,en;q=0.9",
    `Authorization` = glue("Bearer {access_token}"),
    `Connection` = "keep-alive",
    `Content-Type` = "application/json"
)
idnumber = '22938213'
idnumber = '29813942'
data = sprintf('{"identity_numbers":["%s"]}',idnumber)

identity_data <- httr::POST(url = "https://v2.analytics.metropol.co.ke:51510/map/v1/report/identitysearch",
               httr::add_headers(.headers = headers),
               body = data)
identity_data$status_code
# report data ====
data = sprintf('{"report_identities":[{"identity_number":"%s","identity_type_id":2}],"report_request_type":1,"report_reason":10,"report_id":6}',idnumber)
STATUS = TRUE
while (STATUS) {
    res <- httr::POST(url = "https://v2.analytics.metropol.co.ke:51510/map/v1/report", httr::add_headers(.headers=headers), body = data)
    fetchedreport <- jsonlite::fromJSON(res$content %>% rawToChar())$data$reports$report
    if(class(fetchedreport) == "logical"){
        print("here")
        res <- httr::POST(url = "https://v2.analytics.metropol.co.ke:51510/map/v1/report", httr::add_headers(.headers=headers), body = data)
        fetchedreport <- jsonlite::fromJSON(res$content %>% rawToChar())$data$reports$report
    }
    STATUS = FALSE

}

associations <- fetchedreport$associations %>% bind_rows()

includedinfo <- map2_df(
        .x = names(fetchedreport$contact_info),
        .y = fetchedreport$contact_info ,
        .f = function(x = .x, y = .y) {
            df = bind_rows(tibble(data = x),
                      mutate(bind_rows(y), data = x))
        }
    ) %>%
    select(data, everything())

employments <- fetchedreport$employment_info %>% bind_rows()

associations <- fetchedreport$associations %>% bind_rows()

verified_identity <- fetchedreport$identity_info$verified_info %>%
    bind_cols(fetchedreport$identity_info$marital_statuses %>% bind_rows()  %>%
                  head(1)) %>%
    select(identity_number,first_name,other_name,surname,last_name,date_of_birth,gender,citizenship,
           MaritalStatus = status_name)

credit_performance <- fetchedreport$credit_performance %>%
    as.list() %>%
    enframe() %>% split(.$name) %>%
    map_df(~unlist(.) %>% enframe() %>%
               reshape2::dcast(.~name,value.var = 'value')) %>%
    as_tibble() %>% select(-.) %>% clean_names() %>%
    rename_all(.funs = function(x)gsub("value_","",x)) %>%
    select(-value)


latest_score <- fetchedreport$rating_info$latest_score

latest_ppi <- fetchedreport$rating_info$latest_ppi

credit_accounts <- fetchedreport$credit_accounts %>%
    map_df(.f = function(x){
        # get risk classification columns
        # payment_performance <- x %>% pluck('payment_performance')
        risk_classification <- x %>% pluck('risk_classification')
        # remove these from x
        x[c('payment_performance','risk_classification')] <- NULL
        x %>% bind_cols(risk_classification)
    })

paymentperformance <- fetchedreport$credit_accounts %>%
    map_df(~ select(.,account_number,payment_performance) %>%
               unnest(cols = payment_performance)
           )



tempfile = glue("E:/Reports/improptu/Mar/tempfile.xlsx")

wb <- loadWorkbook(filename  = tempfile)
# addWorksheet(wb,"CreditSummary")
# addWorksheet(wb,"CreditAccounts")
# addWorksheet(wb,"PaymentPerformance")
pathtofile = glue("E:/Reports/improptu/Mar/CRB_Report_{idnumber}_{format(Sys.Date(),'%d.%m.%Y')}.xlsx")
# save data
writeWorksheet(wb,verified_identity,1,startRow = 2,startCol = 1)
writeWorksheet(wb,latest_score,1,startRow = 6,startCol = 1)
writeWorksheet(wb,latest_ppi,1,startRow = 10,startCol = 1)
writeWorksheet(wb,employments,1,startRow = 14,startCol = 1)
writeWorksheet(wb,includedinfo,1,startRow = 18,startCol = 1)
writeWorksheet(wb,associations,1,startRow = 2,startCol = 11)
writeWorksheet(wb,credit_performance ,2)
writeWorksheet(wb,credit_accounts ,3)
writeWorksheet(wb,paymentperformance ,4)
saveWorkbook(wb,file = pathtofile)
shell.exec(pathtofile)
