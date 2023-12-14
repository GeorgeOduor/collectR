options(golem.app.prod = FALSE)
options(shiny.port = 2023)
options(shiny.maxRequestSize = 500 * 1024^2)
golem::detach_all_attached()
golem::document_and_reload()
run_app()
devtools::load_all()


