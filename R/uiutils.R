#' @importFrom shiny fluidRow column tagList tags HTML icon div
#' @importFrom glue glue
#' @importFrom shinyWidgets dropdown pickerInput animateOptions

#' @noRd
filterui <- function(ns) {
    dropdown(
        tags$p("List of Input"),
        div(class="title-section-float",
            pickerInput(inputId = ns("call_agent"),label = "Call Agent",choices = c("George","Oduor","Eric","Mshila","Stephen","Robia"),options = list(title = "Call Agent")),
            pickerInput(inputId = ns("product"),label = "Product",choices = c('All','M-Fanisi Safaricom','M-Fanisi Airtel','Branch'),options = list(title = "Channel")),
            fluidRow(class = "month-year-quater",
                     col_6(class="left",pickerInput(inputId = ns("year"),label = "Year",choices = c(2019:as.numeric(substr(Sys.Date(),1,4))),options = list(title = "Year"))),
                     col_6(class="right",(pickerInput(inputId = ns("month"),label = "Month/Quarter",choices = list(Months=month.name,Quarters=paste0("Q",1:4)),options = list(title = "Month/Quarter"),width = "100%")))
            ),
            pickerInput(inputId = ns("compareby"),label = "Compare to",choices = c("","Last Month","Last Year"))
        ),

        style = "material-flat", icon = icon("filter"),size = "xs",
        status = "success", width = "100%",label = "Filter",
        animate = animateOptions(
            enter = shinyWidgets::animations$specials$rollIn,
                exit = shinyWidgets::animations$specials$rollOut
        ))
}
#' @noRd
col_1 <- function(...) {
    column(1,...)
}
#' @noRd
col_2 <- function(...) {
    column(2,...)
}
#' @noRd
col_3 <- function(...) {
    column(3,...)
}
#' @noRd
col_4 <- function(...) {
    column(4,...)
}
#' @noRd
col_5 <- function(...) {
    column(5,...)
}
#' @noRd
col_6 <- function(...) {
    column(6,...)
}
#' @noRd
col_7 <- function(...) {
    column(7,...)
}
#' @noRd
col_8 <- function(...) {
    column(8,...)
}
#' @noRd
col_9 <- function(...) {
    column(9,...)
}
#' @noRd
col_10 <- function(...) {
    column(10,...)
}
#' @noRd
col_11 <- function(...) {
    column(11,...)
}
#' @noRd
col_12 <- function(...) {
    column(12,...)
}
#' @noRd
datepanel <- function(date = Sys.Date()) {
    date = as.character(date)
    year = substr(date,1,4)
    month = month.abb[as.numeric(substr(date,6,7))]
    day = substr(date,9,10)

    tagList(tags$span(class = "date-tile",day),
            tags$span(class = "date-tile",month),
            tags$span(class = "date-tile",year))
}

#' @noRd
#'
animate_entrace <- function(class) {
    tags$style(
        HTML(
            "\n",
            sprintf(".%s {\n",class),
            "  background-color: #8080804a;\n",
            "  border-radius: 10px !important;\n",
            "  //    box-shadow: 7px 7px 6px 0 rgb(0 0 0 / 20%) ;\n",
            "}\n",
            "\n",
            sprintf(".%s-icon {\n",class),
            "  background: transparent !important;\n",
            "  color: #3f51b5 !important;\n",
            "  font-weight: 900 !important;\n",
            "}\n",
            sprintf(".%s {\n",class),
            sprintf("  -webkit-animation: %s 0.5s cubic-bezier(0.25, 0.46, 0.45, 0.94) both;\n",class),
            sprintf("  animation: %s 0.5s cubic-bezier(0.25, 0.46, 0.45, 0.94) both;\n",class),
            "}\n",
            "\n",
            sprintf("@-webkit-keyframes %s {\n",class),
            "  0% {\n",
            "    -webkit-transform: scale(0);\n",
            "    transform: scale(0);\n",
            "    opacity: 1;\n",
            "  }\n",
            "  100% {\n",
            "    -webkit-transform: scale(1);\n",
            "    transform: scale(1);\n",
            "    opacity: 1;\n",
            "  }\n",
            "}\n",
            sprintf("@keyframes %s {\n",class),
            "  0% {\n",
            "    -webkit-transform: scale(0);\n",
            "    transform: scale(0);\n",
            "    opacity: 1;\n",
            "  }\n",
            "  100% {\n",
            "    -webkit-transform: scale(1);\n",
            "    transform: scale(1);\n",
            "    opacity: 1;\n",
            "  }\n",
            "}\n"
        )
    )
}

#' @noRd
change_icon <- function(value = NULL,icon=NULL) {
    tags$span(
        class = "description-percentage text-red",
        value,
        icon
    )
}

#' @noRd
tabset_panel <- function(...) {
    kwargs <- list()
    # create the tabs
    tags$div(
        class = "warpper",
        tagList(...),

    )
}
#' @noRd
tab_panel <- function(title,id,content_title,content) {
    tagList(
        # input specs
        tags$input(
            class = "radio",
            id = glue("{id}"),
            name = "group",
            type = "radio"
        ),# tab names
        tags$div(
            class = "tabs",
            tags$label(
                class = "tab",
                id = glue("{id}-tab"),
                `for` = glue("{id}"),
                title
            )
        ),
        # contents
        tags$div(
            class = "panels",
            tags$div(
                class = "panel",
                id = glue("{id}-panel"),
                tags$div(
                    class = "panel-title",
                    content_title
                ),
                content
            )
        ))
}
#' @noRd
agent_dash <- function(tab1name,tab1content,tab2name,tab2content){
    tags$div(
        class = "warpper",
        tags$input(class = "radio",id = "one",name = "group",type = "radio",checked = "checked"),
        tags$input(class = "radio",id = "two",name = "group",type = "radio"),
        tags$div(class = "tabs",tags$label(class = "tab",id = "one-tab",`for` = "one",tab1name),
            tags$label(
                class = "tab",
                id = "two-tab",
                `for` = "two",
                tab2name
            )
            ),
        tags$div(
            class = "panels",
            tags$div(
                class = "panel",
                id = "one-panel",
                # tags$div(
                #     class = "panel-title",
                #     tab1name
                # ),
                tags$p(tab1content)
            ),
            tags$div(
                class = "panel",
                id = "two-panel",
                # tags$div(
                #     class = "panel-title",
                #     tab2name
                # ),
                tab2content
            )
            # tags$div(
            #     class = "panel",
            #     id = "three-panel",
            #     tags$div(
            #         class = "panel-title",
            #         "Title3"
            #     ),
            #     tags$p("Content3")
            # )
        )
    )
}
#' @noRd
custom_value_box <- function(title,value,trend,trend_icon,trend_text,main_icon) {
    tags$div(
        class = "stat-card",
        tags$div(
            class = "stat-card__content",
            tags$p(
                class = "title-text",
                title
            ),
            tags$h2(class = "value",
                value
            ),
            tags$div(
                tags$span(
                    class = "text-danger font-weight-bold mr-1",
                    icon(trend_icon),
                    trend
                ),
                tags$span(
                    class = "text-muted",
                    trend_text
                )
            )
        ),
        tags$div(
            class = "stat-card__icon stat-card__icon--primary",
            tags$div(
                class = "stat-card__icon-circle",
                icon(main_icon)
            )
        )
    )
}

#' @noRd
custom_ui <- function(title,...) {
    tags$div(
        class = "stat-card",
        tags$div(
            class = "stat-card__content",
            tags$p(
                class = "title-text",
                title
            ),
            tags$h2(
                class = "value center",
                "Full View"
            ),...
        )
    )
}
#' @noRd
custom_panel <- function(width,title,value,caretstats,id=NULL) {
    tags$div(
        class = glue("col-sm-{width}"),
        tags$div(
            id = id,
            class = "info-box",
            tags$div(
                class = "info-box-content",
                tags$div(
                    class = "infobox-title",
                    title
                ),
                tags$div(
                    class = "infobox-value",
                    value
                ),
                tags$div(
                    class = "caretstats",
                    caretstats
                )
            )
        )
    )
}

#' @noRd
#'
portfolio_bd <- function(title,value,rate) {
    tags$div(
        class = "dashboard-section",
        tags$h3(
            class = "portfolio-title",
            title
        ),
        tags$h1(
            class = "portfolio-value",
            value
        ),
        tags$p(
            class = "rate",
            rate
        )
    )
}

#' @noRd
#'
portfolio_bd2 <- function(title,value,rate) {
    tags$div(
        class = "dashboard-section",
        tags$h3(
            class = "portfolio-title2",
            title
        ),
        tags$h1(
            class = "portfolio-value2",
            value
        ),
        tags$p(
            class = "rate",
            rate
        )
    )
}

feedback <- function() {
    # create vectors for each column of the data
    feedback <- c("Affected by Corona Pandemic", "Already Paid", "Hang-Up", "Loan Disputed",
                  "Non- Commital", "Partial Payments", "Picked by 3rd Party", "Promised to Pay",
                  "Unreachable -Phone Off", "Unreachable -Phone Unanswered")
    outstanding_amount <- round(c(3017.49, 159685.39, 245452.48, 49904.67, 706867.81, 445907.48,
                                  35961.33, 1414699.21, 2866598.84, 41304930.84)/1000)
    percent <- c(0, 0.3, 0.5, 0.1, 1.5, 0.9, 0.1, 3, 6.1, 87.4)

    # combine the vectors into a data frame
    feedback_df <-  dplyr::arrange(tibble(FeedBack = feedback,
                                            `Outstanding Amount('000)` = outstanding_amount,
                                              Percent = percent),
                                   dplyr::desc(Percent))
    saveRDS(feedback_df,"app/static/feedback.rds")
    return(feedback_df)
}

#' @noRd
#' @importFrom shiny HTML
modal_footer <- function(cancelbutton=F,...) {
    kwargs <- list(...)
    tagList(
        HTML(paste(
            "&copy;",
            get_golem_config("custodian", "misc"),
            get_golem_config("year", "misc")
        ),
        )
    )
}
#' @noRd
#' @importFrom shiny HTML
modal_exit <- function(text="Dismis") {
    tags$button(
        type = "button",
        class = "btn btn-success bttn bttn-material-flat bttn-xs bttn-no-outline",
        `data-dismiss` = "modal",
        `data-bs-dismiss` = "modal",
        text
    )
}
