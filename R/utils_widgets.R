#' widgets
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#' @noRd
#' @importFrom shiny div tagList tagAppendAttributes tags

divider <- function(text) {
    div(class="divider",text)
}


allocation_items <- function(agent_name,count=NULL,amount=NULL,propotion=NULL,...) {
    widget <- tags$div(
        class = "debtcollector_item text-focus-in",
        tags$div(
            class = "col-sm-5 collector_name",
            tags$p(
                agent_name
            )
        ),
        tags$div(
            class = "col-sm-3 propotion",
            tags$div(
                class = "form-group shiny-input-container",
                style = "width: 100%;",
                tags$label(
                    class = "control-label",
                    id = "propotion-label",
                    `for` = "propotion"
                ),
                propotion
            )
        ),
        tags$div(
            class = "col-sm-2 stats",
            tags$p(
                count
            )
        ),
        tags$div(
            class = "col-sm-2 stats",
            tags$p(
                amount
            )
        )
    )
    return(widget)
}

