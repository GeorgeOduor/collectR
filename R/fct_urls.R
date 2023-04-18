#' urls
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#' @importFrom shinydashboard tabItem tabItems
body_ui <- function(tabset=NULL) {
    tabItems(
        tabItem("landingpage",landingPage_UI("landingpage-ux")),
        tabItem("overalcolletions",collection_dashboard_UI("collections")),
        tabItem("callagents", internalAgentsDash_UI("agent_dash")),
        tabItem("extdebtcol", dca_dashboard_UI("dca_dash")),
        tabItem("agent_performance", agentPerformanceDashboard_UI("agent_performance")),
        tabItem("clientprofile", clientProfile_UI("clientprofile")),
        # administrative
        tabItem("agentmanagement", agentManagement_UI("agent_management")),
        tabItem("allocation_tab", allocationSettings_UI("allocation-ux")),
        tabItem("performance", performanceUpdate_UI("performance_update-ux")),
        tabItem("settings", mod_settings_ui("settings_1"))

    )
}


