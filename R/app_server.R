#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom  shinymanager secure_server check_credentials
#' @noRd
app_server <- function(input, output, session) {

    # db connection
    res_auth <- secure_server(
        keep_token = T,
        check_credentials = check_creds()
        )
    # authentication <- dash_auth()
    auth_res <- reactive({
        reactiveValuesToList(res_auth)
        })

    sidebar_ns <- sidebar_Server("sidebar")

    observe({
        # check_user_rights <- function(user,perms,exp) {
        #     if (perm) {
        #
        #     }
        # }
        # check_user_rights <<- check_user_rights(auth_res(),1)
        sidebar_ns <- sidebar_ns()

        tryCatch(
            expr = {
                switch (sidebar_ns,
                        # "landingpage"       = landingPage_Server("landingpage-ux"),
                        # "overalcolletions"  = collection_dashboard_Server("collections"),
                        # "callagents"        = internalAgentsDash_Server("agent_dash"),
                        # "extdebtcol"        = dca_dashboard_Server("dca_dash"),
                        # "agent_performance" = agentPerformanceDashboard_Server("agent_performance"),
                        "clientprofile"     = clientProfile_Server("clientprofile"),
                        # "agentmanagement"   = agentManagement_Server("agent_management"),
                        # "allocation_tab"    = allocationSettings_Server("allocation-ux"),
                        "performance"       = performanceUpdate_Server("perfornce_update-ux"),
                        "settings"          = mod_settings_server("settings_1",auth_res)
                )
            },
            error = function(e){print(e)},warning = function(w){},finally = {}
        )
    },label = "selectedmenu",priority = 10)




}
