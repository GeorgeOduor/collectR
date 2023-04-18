## code to prepare `my_dataset` dataset goes here
credentials <- data.frame(
    user = c("George", "Oduor"), # mandatory
    password = c("", ""), # mandatory
    start = c("2019-04-15"), # optinal (all others)
    expire = c(NA, "2019-12-31"),
    admin = c(T, TRUE),
    comment = "Simple and secure authentification mechanism
  for single ‘Shiny’ applications.",
    stringsAsFactors = FALSE
)
usethis::use_data(credentials, overwrite = TRUE)
