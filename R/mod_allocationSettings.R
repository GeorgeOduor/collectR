#' @importFrom shiny fileInput plotOutput renderUI observe uiOutput bootstrapPage  moduleServer  NS  h1  div  icon  tagList p tabsetPanel tabPanel numericInput fluidRow hr span
#' @importFrom shinyWidgets prettyToggle pickerInput prettyRadioButtons actionBttn numericRangeInput sliderTextInput
#' @importFrom shinydashboard box
#' @importFrom purrr map
#' @importFrom dplyr `%>%`

#' @noRd
allocationSettings_UI <- function(id) {
  ns <- NS(id)
  div(class="setings-content",
    fluidRow(class = "title_row",
             p(class = "title_text text-focus-in mr-5","Debt Collection Outsource")
    ),
    box(title = "Outsource Settings",solidHeader = T,status = 'success',width = 3,height = "400px",
        divider("Internal Debt Collectors"),
        div(class="settings",
        numericRangeInput(ns("arrears_days_internal"),"Arrears Days",value = c(1,90),min = 1,separator = " to ",step = 1),
        prettyRadioButtons(
          inputId = ns("existing_outsource"),
          label = "Existing outsource",
          choices = c("Keep", "Reshufle"),
          icon = icon("check"), bigger = F,
          status = "success",
          animation = "tada",inline = T
        )
        ),
        divider("External Debt Collectors"),
        div(class="settings",
            numericInput(ns("arrears_days_external"),"Minimum Arrears Days",min = 1,value = 91,step = 1)
        ),
        hr(),
        actionBttn(ns("save"),"Save",icon = icon('save'),style = "material-flat",size = "xs",color = "success")
        ),
    col_9(class = "allocations-ui",
           tabsetPanel(id = "allocation-tabs",type = "pills",
             tabPanel(title = "Allocation Listing",shinipsum::random_DT(8,8),icon = icon("table")),
             tabPanel(title = "Run Month Allocation",
                      fluidRow(class="debt-allocation",
                               col_4(class= "checkbutton",
                                     actionBttn(ns("check"),"Check available outsource",size="xs",style = "material-flat",color = "success",icon = icon("question"))),
                               col_4(class="available_cases",
                                     p(class="section-head text-focus-in","Internal Call Agents"),
                                     p(class="available-text text-focus-in","M-Fanisi Safaricom Cases:",span(class="cases-value pull-right","21")),
                                     p(class="available-text text-focus-in","M-Fanisi Airtel Cases:",span(class="cases-value pull-right","21"))
                                     ),
                               col_4(class="available_cases",
                                     p(class="section-head text-focus-in","External Call Agents"),
                                     p(class="available-text text-focus-in","M-Fanisi Safaricom Cases:",span(class="cases-value pull-right","212")),
                                     p(class="available-text text-focus-in","M-Fanisi Airtel Cases:",span(class="cases-value pull-right","212")))
                               ),
                      fluidRow(class = "debt-allocation",
                               col_3(
                                 p(class="section-head","Collection Teams"),

                                 pickerInput(
                                   inputId =ns("call_center_agents"),
                                   label = "Internal Debt Collectors",
                                   choices = c("George","David","Ethan","Kevin","Bily","Ouma","Omuklela"),
                                   options = list(`actions-box` = TRUE,`data-divider`=T), multiple = TRUE
                                   ),
                                 pickerInput(
                                   inputId =ns("external_agents"),
                                   label = "External Debt Collectors",
                                   choices = c("George","David","Kevin"),
                                   options = list(`actions-box` = TRUE,`data-divider`=T), multiple = TRUE
                                 ),
                                 col_6(class="nav-button"),
                                 col_6(class="nav-button",
                                       # actionBttn(ns("outsource"),"Outsource",style = "material-flat",size = "xs",color = "success")
                                       )
                               ),
                               col_9(class="segmentation",
                                     col_12(class="segmentation_settings",
                                            tabsetPanel(type = 'pills',
                                              tabPanel(title = "Internal Agents",
                                                       div(class="top-row",allocation_items("Agent","Propotion","Amount","Count",class="top_col")),
                                                       uiOutput(ns("segmentation_ui"))),
                                              tabPanel(title = "External Agents",
                                                       div(class="top-row",allocation_items("Agency","Propotion","Amount","Count",class="top_col")),
                                                       uiOutput(ns("segmentation_u")))
                                            )
                                            ),

                                     col_12(class="action-points",
                                            hr(class="divider"),
                                           col_4("Submit as final",
                                                 prettyToggle(
                                                   inputId = ns("submit-status"),
                                                   label_on = "Yes!",
                                                   icon_on = icon("check"),
                                                   status_on = "success",
                                                   status_off = "danger",
                                                   label_off = "No..", animation = "rotate", inline = T,
                                                   icon_off = icon("xmark")
                                                 )),
                                           col_8(
                                                col_6(actionBttn(ns("outsource"),"Outsource",icon = icon('save'),style = "material-flat",size = "xs",color = "success")),
                                                col_6(actionBttn(ns("share_outsource"),"Send",icon = icon('share'),style = "material-flat",size = "xs",color = "success"))

                                           )
                                           )
                                     )

                               ),
                      icon = icon("gear")),
             tabPanel(title = "Reshuflle",icon = icon("random"),
                      col_3(class="recal-column",
                        divider("Recal Request"),
                        prettyRadioButtons(
                          inputId = ns("reshuffle_method"),
                          label = "Reshuffle Method",
                          choices = c("Requested", "Prepare"),
                          icon = icon("check"), bigger = F,
                          status = "success",
                          animation = "tada",inline = T
                        ),
                        uiOutput(ns("reshuffle_method_ui")),
                        pickerInput(
                          inputId =ns("external_agents"),
                          label = "New Agencies",
                          choices = c("Agency A","Agency B","Agency C"),
                          options = list(`actions-box` = TRUE,`data-divider`=T), multiple = TRUE
                        ),
                        actionBttn(ns("upload"),label = "Submit Upload",icon = icon("upload"),style = "material-flat",size = "xs",block = T,color = "success"),

                      )
                      )
           )
           )
  )
}

#' @noRd
allocationSettings_Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {

      output$reshuffle_method_ui <- renderUI({
        ns = session$ns
        ui <- switch (input$reshuffle_method,
          "Requested" = {
            div(class = "reshuffle_method_ui-inner",
            fileInput(ns("recals"),"",multiple = T,accept = ".xlsx"))
          },
          "Prepare" = {div(class = "reshuffle_method_ui-inner",
            pickerInput(
              inputId =ns("channel"),
              label = "Channel",
              choices = c("Mfanisi Airtel","Mfanisi Safaricom"),
              options = list(`actions-box` = TRUE,`data-divider`=T), multiple = TRUE
            ),
            sliderTextInput(inputId = ns("arrearsDays"),label = "Arrears Days", choices = 1:100,selected = c(10,56)),
            sliderTextInput(inputId = ns("outsourcedays"),label = "Days Since Outsource", choices = 1:100,selected = c(10,56)),
            numericInput(ns("propotionpaid"),label = "Propotion paid" , min=0,max=1,value = 0,step = 1),

          )}
        )
      })

      output$segmentation_ui <- renderUI({
        ns = session$ns
        tagList(
          input$call_center_agents %>%
            map(~allocation_items(agent_name = .,count = 20,amount = 200,
                                  numericInput(ns("propotion"),min=0,max=Inf,value = 0,label = "",step = 1))) %>%
            list_to_li(class="agents_list")
        )
      })


    }
  )
}

# copy to main.R in the box section
# app/view/mod_allocationSettings

