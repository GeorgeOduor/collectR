#
#
#
golem::fill_desc(
  pkg_name = "collectoR",
  pkg_title = "Debt Collection app with R",
  pkg_description = "This is a shinypackage used for performing debt collection management for a small team",
  author_first_name = "George",
  author_last_name = "Oduor",
  author_email = "george.wamaya@gmail.com",
  repo_url = NULL
)
golem::set_golem_options(golem_name = "collectoR",talkative = T,golem_version = 2.0)
usethis::use_mit_license(copyright_holder = "George Oduor")
usethis::use_readme_rmd(open = FALSE)
usethis::use_code_of_conduct(contact = "george.wamaya@gmail.com")
usethis::use_lifecycle_badge("Experimental")
usethis::use_news_md(open = FALSE)
usethis::use_git()
golem::use_recommended_tests()
golem::use_favicon(path = "E:/New folder/projects/debtCollection/inst/app/www/maishalogo.png")
golem::use_utils_ui(with_test = F)
golem::use_utils_server(with_test = F)
rstudioapi::navigateToFile("dev/02_dev.R")
