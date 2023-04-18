
#' @noRd
#' @importFrom  shiny bootstrapPage moduleServer NS h1 div icon tagList
landingPage_UI <- function(id) {
  ns <- NS(id)
  tagList(
    "Landing Page!"

  )
}

#' @noRd
landingPage_Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
    }
  )
}

# copy to main.R in the box section
# app/view/mod_landingPage

