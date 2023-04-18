
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
                             # menuItem(
                             #     text = "Collection Analytics",
                             #     icon = icon("dashboard"),
                             #     menuSubItem("Collections Summary", tabName = "overalcolletions"),
                             #     menuSubItem("Internal Agents Report", tabName = "callagents"),
                             #     menuSubItem("External Agents Report", tabName = "extdebtcol")
                             # ),
                             # menuItem(
                             #     text = "My Performance",
                             #     icon = icon("chart-line"),
                             #     tabName = "agent_performance"
                             # ),
                             # menuItem(
                             #     text = "Client Information",
                             #     icon = icon("user"),
                             #     tabName = "clientprofile"
                             # ),
                             menuItem(
                                 text = "Admin",
                                 icon = icon("dashboard"),
                                 menuSubItem("Agents Management", tabName = "agentmanagement")
                                 # menuSubItem("Allocation", tabName = "allocation_tab",),
                                 # menuSubItem("Performance Update", tabName = "performance")
                             ),
                             menuItem(
                                 text = "Settings",
                                 icon = icon("gears"),
                                 tabName = "settings"
                             )


                         )
                     })

                     selected_menu <- reactive({
                         input$dashboard_menu
                     })

                 })
}
