
#' @importFrom  shiny bootstrapPage  moduleServer  NS  h1  div  icon  tagList uiOutput renderUI observe reactive
#' @importFrom  shinydashboard sidebarMenu  menuItem  menuSubItem  dashboardBody  tabItem  tabItems renderMenu sidebarMenuOutput

#' @noRd
sidebar_UI <- function(id) {
    ns <- NS(id)
    tagList(
        sidebarMenuOutput(ns("sidebar_out"))
    )
}

#' @noRd
sidebar_Server <- function(id) {
    moduleServer(id,
                 function(input, output, session) {
                     output$sidebar_out <- renderMenu({
                         ns = session$ns
                         sidebarMenu(id = ns("dashboard_menu"),
                             #         menuItem(
                             #             text = "Home",
                             #             icon = icon("home"),
                             #             tabName = "landingpage"
                             #         ),
                             menuItem(
                                 text = "Collection Dashboard",
                                 icon = icon("chart-line"),
                                 tabName = "agent_performance"
                             ),
                             # menuItem(
                             #     text = "Collection Dashboard",
                             #     icon = icon("shuffle"),
                             #     tabName = "allocation_tab"
                             # ),
                             menuItem(
                                 text = "Client Information",
                                 icon = icon("user"),
                                 tabName = "clientprofile"
                             ),
                             menuItem(
                                 text = "Admin",
                                 icon = icon("gears"),
                                 menuSubItem("Agents Management", tabName = "agentmanagement"),
                                 menuSubItem("Allocation", tabName = "allocation_tab",icon =  icon("shuffle")),
                                 # menuSubItem("Performance Update", tabName = "performance"),
                                 menuSubItem(
                                     text = "Settings",
                                     icon = icon("gears"),
                                     tabName = "settings"
                                 )
                             )



                         )
                     })

                     selected_menu <- reactive({
                         input$dashboard_menu
                     })

                 })
}
