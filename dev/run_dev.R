options(golem.app.prod = FALSE)
options(shiny.port = httpuv::randomPort())
golem::detach_all_attached()
golem::document_and_reload()
run_app()