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

#' Allocation Items Widget
#'
#' This function generates a widget for displaying allocation items, typically used
#' for representing information about agents, their counts, amounts, and proportions.
#'
#' @param agent_name Character string. The name of the agent.
#' @param count Numeric. The count associated with the agent.
#' @param amount Numeric. The amount associated with the agent.
#' @param propotion A Shiny input element representing the proportion. It could be a shiny input like `sliderInput` or `numericInput`.
#'
#' @return A widget displaying allocation items.
#' @noRd

allocation_items <- function(ns,agent_name,count=NULL,amount=NULL,propotion=NULL,...) {
    widget <- tags$div(
        class = "col-sm-12 debtcollector_item text-focus-in",
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
                tags$input(
                    id = ns(id),
                    type = "number",
                    class = "form-control",
                    value = "3"
                )
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

#' Allocation Distribution Widget
#'
#' This function generates an allocation distribution widget that allows users
#' to toggle the allocation method for equally distributing cases.
#'
#' @param ns A namespace object created using the `shiny::NS` function.
#' @param value Logical. Specifies the initial state of the widget (TRUE for "Yes" and FALSE for "No").
#'
#' @return A shiny widget with a toggle button for equally distributing cases.
#'
#' @noRd
allocation_distribution_widget <- function(ns,value=T) {
    col_12(class='allocation_method',
        col_4("Equally distribute Cases?",
            prettyToggle(
                inputId = ns("allocation_method"),
                label_on = "Yes!",
                icon_on = icon("check"),
                status_on = "success",
                status_off = "danger",value = value,
                label_off = "No..", animation = "rotate", inline = T,
                icon_off = icon("xmark")
            )
        ),
        col_4(class = "available_cases",span(class="value","Available Cases: 2000")),
        col_4(class = "available_cases,",span(class = "value","Amount:Ksh 149,234"))

        )
}


