char <- function(x) {
    y <- paste0('"', x, '"')
    return(y)
}

self_profile_UI <- function(id) {
    ns <- NS(id)
    fluidPage(
    div(id = ns('self_profile_page'), box(title = textOutput(outputId = 'profile_name'), status = 'success', solidHeader = TRUE, width = 12,
        valueBoxOutput(outputId = ns('name'), width = 9),
        valueBoxOutput(outputId = ns('manager'), width = 4),
        valueBoxOutput(outputId = ns('connections'), width = 2),
        # column(width = 2, uiOutput(outputId = ns('reportees'))),
        hidden(valueBoxOutput(outputId = ns('reportee'), width = 2)),
        valueBoxOutput(outputId = ns('endorsement'), width = 3),
        
        box(title = 'Skills', solidHeader = TRUE, status = 'warning', width = 12, collapsible = TRUE, collapsed = FALSE,
            column(width = 10, plotOutput(outputId = ns('skill_cloud'))),
            column(width = 2, tags$head(
                tags$style(HTML("
                                #add_skill {
                                display:block;
                                height: 60px;
                                width: 60px;
                                border-radius: 100%;
                                border: 2px solid red;
                                }
                                "))
            ),
            actionButton(inputId = ns('add_more_skill'), label = 'skill', icon = icon('plus')))
        ),
        
        actionButton(inputId = 'save_profile_changes', label = 'Save Profile Changes', icon = icon('save')),
        actionButton(inputId = 'discard_profile_changes', label = 'Discard Profile Changes', icon = icon('delete'))
    )),
    # shinyjs::hidden(div(id = ns('managers_profile'), manager_profile_UI(id = ns('manager')))),
    # shinyjs::hidden(div(id = ns('connection_profile'), associate_names_UI(id = ns('Conn'), box_title = 'Connections'))),
    # shinyjs::hidden(div(id = ns('direct_report_profile'), associate_names_UI(id = ns('DR'), box_title = 'Direct Reports'))),
    shinyjs::hidden(div(id = ns('others_profile'), other_profile_UI(id = ns('other'))))
    )
}

self_profile_ <- function(input, output, session, self, is_manager) {
    ns <- session$ns
    profile <- reactive({
        # print(self)
        b <- paste0('profile[', char(self), ']')
        # print(b)
        a <- python.get(b)
        # print(a)
        return(a)
    })
    
    observe({
        if (is_manager) {
            shinyjs::show(id = 'reportee')
        }
    })

    output$name <- renderValueBox(valueBox(value = profile()$Name, subtitle = '', color = 'green', width = 12))
    output$manager <- renderValueBox(valueBox(value = actionLink(inputId = ns('manager_link'), label = profile()$Manager), 
                                              subtitle = 'Manager ::', color = 'orange', width = 12))
    output$connections <- renderValueBox(valueBox(value = actionLink(inputId = ns('conn_link'), label = profile()$Connections), 
                                                  subtitle = 'Connections', color = 'yellow', width = 12))
    # output$reportees <- renderUI({
    #     if (is_manager) {
    #         valueBox(value = actionLink(inputId = ns('dr_link'), label = length(profile()$DirectReports) - 1), 
    #                  subtitle = 'Direct Reports', color = 'yellow', width = 12)
    #     } else {
    #         NULL
    #     }
    # })
    output$reportee <- renderValueBox({
        # if (is_manager) {
            valueBox(value = actionLink(inputId = ns('dr_link'), label = length(profile()$DirectReports) - 1),
                                                  subtitle = 'Direct Reports', color = 'yellow', width = 12)
        # }
        })
    output$endorsement <- renderValueBox(valueBox(value = actionLink(inputId = ns('endorse_link'), 
                                                                     label = paste(profile()$Endorsements[1], '/', profile()$Endorsements[2])), 
                                                  subtitle = 'Endorsements : Received / Endorsed', color = 'yellow', width = 12))
    # current_project_populated <- !is.null(profile()$Current_Project)
    # past_project_populated <- length(profile()$Past_Projects)
    output$skill_cloud <- renderPlot({
        skills <- profile()$Skills
        wordcloud(skills, min.freq = 1)
    })
    
    click <- reactiveValues(id = NULL, type = NULL)
    observeEvent(input$manager_link, {
        click$id <- python.get(paste0('[k for k in person.keys() if person[k]["Name"] == ', char(profile()$Manager), ']'))
        click$type <- 'Manager'
        show(id = 'others_profile')
        hide(id = 'self_profile_page')
        callModule(other_profile, id = 'other', id = click$id)
    })
    observeEvent(input$conn_link, {
        click$id <- self
        click$type <- 'Connection'
    })
    observeEvent(input$endorse_link, {
        click$id <- self
        click$type <- 'Endorsement'
    })
    observeEvent(input$dr_link, {
        click$id <- self
        click$type <- 'Directs'
    })
    
    # click <- reactive({
    #     id <- NULL
    #     type <- NULL
    #     if (input$manager_link) {
    #         id <- python.get(paste0('[k for k in person.keys() if person[k]["Name"] == ', char(profile()$Manager), ']'))
    #         type <- 'Manager'
    #     }
    #     if (input$conn_link) {
    #         id <- self
    #         type <- 'Connection'
    #     }
    #     if (input$endorse_link) {
    #         id <- self
    #         type <- 'Endorsement'
    #     }
    #     if (input$dr_link) {
    #         id <- self
    #         type <- 'Directs'
    #     }
    #     return(list('id' = id, 'type' = type))
    # })
    
    click_info <- reactive({
        id <- click$id
        type <- click$type
        return(list('id' = id, 'type' = type))
    })
    
    # return(click_info)
}
