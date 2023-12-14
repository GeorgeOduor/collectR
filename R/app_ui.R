#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom shinydashboard dashboardBody tabItem tabItems
#' @importFrom shinydashboardPlus dashboardPage dashboardHeader dashboardSidebar dashboardFooter
#' @importFrom shinyEffects setShadow
#' @importFrom shinymanager secure_app
#' @import waiter
#' @import shinytoastr
#' @importFrom  shinyanimate withAnim
#' @importFrom shinyjs useShinyjs
#' @import shiny.pwa
#' @noRd
ui <- function(request) {
  tagList(
    golem_add_external_resources(),
    tagList(
      useToastr(),
      withAnim(),
      useWaiter(),
      useShinyjs(),
      # setShadow(class = "info-box"),
      # setShadow(class = "box"),
      # setShadow(class = "stat-card"),
      # setShadow(class = "colection_vs_targets"),
      # setShadow(class = "cohort_analysis"),
      # setShadow(class = "full-recoverd"),
      # setShadow(class = "partial-recovered"),
      # setShadow(class = "allocation_count"),
      lapply(strsplit("sub-topic
                      full-recoverd partial-recovered allocation_count info-box
                      colection_vs_targets cohort_analysis"," ")[[1]],
             animate_entrace),

      # animate_entrace("stat-card"),
      fluidRow(class = "orgtitle",
               # pwa("http://127.0.0.1:2023/", output = "inst/app/www/"),
               # col_6(class = "text-section",
               #       # img(class="logo-img bounce-in-top",src="static/images/logo.png"),
               #       # h3(class="logo-text focus-in-contract-bck ","aisha Microfinance Bank")
               # ),
               # col_4(class = "date-section",
               #       tags$p(
               #         class = "date",
               #         datepanel())
               # ),
               # col_2(class = "user_panel","Users")
      ),
      dashboardPage(skin = "purple",
                    options = list(sidebarExpandOnHover = TRUE),
                    preloader = list(html = tagList(img(class="intro_image",src="www/collectoR.png"))
                                     ),
                    scrollToTop = T,
                    header = dashboardHeader(title = "LoanRecovery 2",disable = T,fixed = F),
                    sidebar = dashboardSidebar(collapsed = T,sidebar_UI("sidebar")),
                    body = dashboardBody(body_ui()),
                    footer = dashboardFooter(left = "copyright 2023:Maisha Microfinance Bank"),
                    title = "Debt Collection"
      )

    )
  )
}
app_ui <- shinymanager::secure_app(ui)
#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )
  tags$head(
    favicon(ext = 'png'),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "collectoR"
    )
  )
}

golem_add_external_resources <- function(){

  add_resource_path('www', app_sys('app/www'))

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'colection'),
      shinyjs::useShinyjs()
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
