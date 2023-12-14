#' @importFrom shiny fluidRow column tagList tags HTML icon div
#' @importFrom glue glue
#' @importFrom shinyWidgets dropdown pickerInput animateOptions

#' @noRd
filterui <- function(ns) {
    dropdown(
        tags$p("List of Input"),
        div(class="title-section-float",
            uiOutput(ns('agents')),
            pickerInput(inputId = ns("product"),label = "Product",
                        choices = c('All'),options = list(title = "Channel")),
            fluidRow(class = "month-year-quater",
                     col_6(class="left",pickerInput(inputId = ns("year"),label = "Year",
                                                    choices = NULL,options = list(title = "Year"))),
                     col_6(class="right",(pickerInput(inputId = ns("month"),
                                                      label = "Month/Quarter",
                                                      choices = list(Months=month.name,
                                                                     Quarters=paste0("Q",1:4)),
                                                      options = list(title = "Month/Quarter"),
                                                      width = "100%"))
                           )
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
    tryCatch(
    expr = {
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
                        trend_icon,
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
#' @importFrom shiny tags
custom_stats <- function(value,percent,icon,last_item=F) {

    tags$div(
        class = paste("info-box-content custom",ifelse(last_item,"last-item","")),
        tags$div(
            class = "col-sm-2 gender_icon",
            icon
        ),
        tags$div(
            class = "col-sm-10",
            tags$div(
                class = "infobox-value",
                tags$span(
                    class = "gender_details",value
                )
            ),
            tags$div(
                class = "caretstats",
                tags$span(
                    class = "description-percentage text-red",
                    percent
                )
            )
        )
    )
}

#' @noRd
#' @importFrom shiny tags
custom_info_box <- function(title,...) {
    tags$div(
        class = "info-box infobox_large_vertical",
        tags$span(
            class = "subtitle_gender infobox-title",
            title
        ),
        ...
    )
}
#' @noRd
#' @importFrom shiny tags
collection_time_UI <- function(value,title = "Average Recovery Time(Days)") {
    tags$div(
        class = "info-box infobox_large_vertical",
        tags$div(
            class = "info-box-content custom ",
            tags$div(
                class = "col-sm-12",
                tags$div(
                    class = "infobox-value",
                    tags$span(
                        class = "gender_details_special",
                        value
                    )
                )
            )
        ),
        tags$div(
            class = "info-box-content custom last-item",
            tags$div(
                class = "col-sm-12",
                tags$div(
                    class = "infobox-value",
                    tags$span(
                        class = "gender_details_desc",
                        title
                    )
                )
            )
        )
    )
}


#' @importFrom plotly ggplotly
#' @import ggplot2
gg.gauge <- function(pos,breaks=c(0,30,70,100)) {
    get.poly <- function(a,b,r1=0.5,r2=1.0) {
        th.start <- pi*(1-a/100)
        th.end   <- pi*(1-b/100)
        th       <- seq(th.start,th.end,length=100)
        x        <- c(r1*cos(th),rev(r2*cos(th)))
        y        <- c(r1*sin(th),rev(r2*sin(th)))
        return(data.frame(x,y))
    }
    p = ggplot()+
        geom_polygon(data=get.poly(breaks[1],breaks[2]),aes(x,y),fill="red")+
        geom_polygon(data=get.poly(breaks[2],breaks[3]),aes(x,y),fill="gold")+
        geom_polygon(data=get.poly(breaks[3],breaks[4]),aes(x,y),fill="forestgreen")+
        geom_polygon(data=get.poly(pos-1,pos+1,0.2),aes(x,y))+
        geom_text(data=as.data.frame(breaks), size=5, fontface="bold", vjust=0,
                  aes(x=1.1*cos(pi*(1-breaks/100)),y=1.1*sin(pi*(1-breaks/100)),label=paste0(breaks,"%")))+
        annotate("text",x=0,y=0,label=pos,vjust=0,size=8,fontface="bold")+
        coord_fixed()+
        # theme_bw()+
        theme(axis.text=element_blank(),
              axis.title=element_blank(),
              axis.ticks=element_blank(),
              panel.grid=element_blank(),
              panel.border=element_blank())
    p
}
# gg.gauge(4)

#' Calculate Point-to-Point Rate
#'
#' This function generates a solid gauge highchart representing a point-to-point rate.
#'
#' @param value The point-to-point rate value to be displayed in the gauge.
#'
#' @return A highchart object representing the point-to-point rate gauge.
#'
#' @import highcharter
#' @noRd
ptp_rate <- function(value) {
    col_stops <- data.frame(
        q = c(0.15, 0.4, .8),
        c = c('#DF5353', '#DDDF0D','#55BF3B'),
        stringsAsFactors = FALSE
    )

    hcplot <- highchart() %>%
        hc_chart(type = "solidgauge") %>%
        hc_pane(
            startAngle = -90,
            endAngle = 90,
            background = list(
                outerRadius = '100%',
                innerRadius = '60%',
                shape = "arc"
            )
        ) %>%
        hc_tooltip(enabled = FALSE) %>%
        hc_yAxis(
            stops = list_parse2(col_stops),
            lineWidth = 0,
            minorTickWidth = 0,
            tickAmount = 2,
            min = 0,
            max = 100,
            labels = list(y = 26, style = list(fontSize = "12px"))
        ) %>%
        hc_add_series(
            data = value,
            dataLabels = list(
                format = '{y}%',
                x = 2,
                y = -50,
                borderWidth = 0,
                useHTML = TRUE,
                style = list(
                    fontSize = "17px",
                    position= "absolute",
                    marginLeft= '3px !important',
                    marginTop= '28px !important',
                    left= '5px',
                    top= '5px'
                )
            )
        ) %>%
        hc_size(height = 300)
    return(hcplot)
}
# ptp_rate(23)
