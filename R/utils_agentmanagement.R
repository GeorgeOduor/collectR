#' agentmanagement
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

new_agent_details_ui <- function(ns,agents) {
    div(
        id = "agent_details",
        tagList(
            tabsetPanel(
                type = 'pills',
                id = ns("agent_info_tabs"),
                tabPanel(
                    title = "Internal Debt Collectors",
                    icon = icon("user"),
                    fluidRow(class="toprow",
                        col_2(class = "image",
                              p(class =
                                    "icon-image", icon("user"))),
                        col_10(
                            class = "agent_search",
                            selectizeInput(
                                ns("internal_agent_search"),
                                "Agent Username",
                                width = '70%',
                                choices = agents %>% pull(email),
                                options = list(
                                    create = FALSE,
                                    multiple = F,
                                    placeholder = "Search",
                                    maxItems = '100',
                                    onDropdownOpen = I(
                                        "function($dropdown) { if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}"
                                    ),
                                    onType = I("function (str) {if (str === \"\") {this.close();}}")
                                )
                            )
                        )
                    ),
                    fluidRow(class = "agent_info",
                             uiOutput(ns('agent_info')))
                ),
                tabPanel(
                    title = "External Debt Collectors",
                    icon = icon("briefcase"),
                    col_2(class = "image",
                          p(class ="icon-image", icon("briefcase"))),
                    col_10()
                )
            )
        )
    )
}


existing_agent_details_ui <- function(ns) {
    div(id="agent_details",
        box(title = "Agent Details",
            class="agentdetails",solidHeader = T,
            status = "success",width = 12,
            col_2(class="image",
                  p(class="icon-image",icon("user"))),
            col_10(
                col_6(class="details",p("Name",span(class="pull-right","George Oduor"))),
                col_6(class="details",p("Join Date",span(class="pull-right","20th Jan 2023"))),
                col_6(class="details",p("Gender",span(class="pull-right","Male"))),
                col_6(class="details",p("Tenure",span(class="pull-right","3 years"))),
                col_6(class="details",p("Contact",span(class="pull-right","+254711894704"))),
                col_6(class="details",p("Team",span(class="pull-right","Internal Debt Collector"))),
            ),

            col_12(class="actions",
                   hr(),
                   actionBttn(ns("save_data"),"Terminate Contract",icon = icon('close'),style = "material-flat",size = "xs",color = "danger"),
                   # divider("Agent Target"),
                   actionBttn(ns("set_target"),"Set Target",style = "material-flat",size = "xs",color = "success"),
                   actionBttn(ns("edit_details"),"Edit Details",style = "material-flat",size = "xs",color = "success")
            )
        ))

}
