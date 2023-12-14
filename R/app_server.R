#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import promises
#' @import future
#' @importFrom  shinymanager secure_server check_credentials
#' @noRd
app_server <- function(input, output, session) {
    # plan(multisession)
    # worker <- initialize_worker()
    # db connection
    res_auth <- secure_server(keep_token = T,check_credentials = check_creds())
    auth_res <- reactive({reactiveValuesToList(res_auth)})
    sidebar_ns <- sidebar_Server("sidebar")
# set max upload size


    observe({
        sidebar_ns <- sidebar_ns()

        tryCatch(
            expr = {
                switch (sidebar_ns,
                        # "landingpage"       = landingPage_Server("landingpage-ux"),
                        # "overalcolletions"  = collection_dashboard_Server("collections",auth_res),
                        # "callagents"        = internalAgentsDash_Server("agent_dash"),
                        # "extdebtcol"        = dca_dashboard_Server("dca_dash"),
                        "agent_performance" = agentPerformanceDashboard_Server("agent_performance",auth_res),
                        "clientprofile"     = clientProfile_Server("clientprofile",auth_res),
                        "agentmanagement"   = agentManagement_Server("agent_management"),
                        "allocation_tab"    = allocationSettings_Server("allocation-ux",auth_res),
                        # "performance"       = performanceUpdate_Server("perfornce_update-ux"),
                        "settings"          = mod_settings_server("settings_1",auth_res)
                )
            },
            error = function(e){print(e)},warning = function(w){},finally = {}
        )
    },label = "selectedmenu",priority = 10)




}
