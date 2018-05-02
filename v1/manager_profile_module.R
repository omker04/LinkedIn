manager_profile_UI <- function(id) {
    ns <- NS(id)
    fluidPage(
    div(id = 'same_manager_page', box(title = textOutput(outputId = 'm_profile_name'), status = 'success', solidHeader = TRUE, width = 12,
        valueBoxOutput(outputId = ns('m_name'), width = 9),
        valueBoxOutput(outputId = ns('m_manager'), width = 4),
        valueBoxOutput(outputId = ns('m_connections'), width = 2),
        valueBoxOutput(outputId = ns('m_reportees'), width = 2),
        valueBoxOutput(outputId = ns('m_endorsement'), width = 3),
        
        
        box(title = 'Skills', solidHeader = TRUE, status = 'warning', width = 12, collapsible = TRUE, collapsed = FALSE,
            column(width = 10, plotOutput(outputId = ns('m_skill_cloud')))
        )
    )),
    shinyjs::hidden(div(id = ns('m_managers_profile'), manager_profile_UI(id = ns('manager'))))
    )
}

manager_profile_ <- function(input, output, session, manager) {
    
    ns <- session$ns
    m_profile <- reactive({
        a <- python.get(paste0('profile[', char(manager), ']'))
        return(a)
    })
    
    output$m_name <- renderValueBox(valueBox(value = m_profile()$Name, subtitle = '', color = 'green', width = 12))
    output$m_manager <- renderValueBox(valueBox(value = actionLink(inputId = ns('m_manager_link'), label = m_profile()$Manager), 
                                              subtitle = 'Manager ::', color = 'orange', width = 12))
    output$m_connections <- renderValueBox(valueBox(value = actionLink(inputId = ns('m_conn_link'), label = m_profile()$Connections), 
                                                  subtitle = 'Connections', color = 'yellow', width = 12))
    output$m_reportees <- renderValueBox(valueBox(value = actionLink(inputId = ns('m_dr_link'), label = length(m_profile()$DirectReports) - 1), 
                                                  subtitle = 'Direct Reports', color = 'yellow', width = 12))
    output$m_endorsement <- renderValueBox(valueBox(value = actionLink(inputId = ns('m_endorse_link'), 
                                                                     label = paste(m_profile()$Endorsements[1], '/', m_profile()$Endorsements[2])), 
                                                  subtitle = 'Endorsements : Received / Endorsed', color = 'yellow', width = 12))
    
    output$m_skill_cloud <- renderPlot({
        skills <- m_profile()$Skills
        wordcloud(skills, min.freq = 1)
    })
    
    click <- reactiveValues(id = NULL, type = NULL)
    observeEvent(input$m_manager_link, {
        click$id <- python.get(paste0('[k for k in person.keys() if person[k]["Name"] == ', char(m_profile()$Manager), ']'))
        click$type <- 'Manager'
        show(id = 'm_managers_profile')
        hide(id = 'same_manager_page')
        callModule(manager_profile_, id = 'manager', manager = click$id)
    })
    observeEvent(input$m_conn_link, {
        click$id <- manager
        click$type <- 'Connection'
    })
    observeEvent(input$m_endorse_link, {
        click$id <- manager
        click$type <- 'Endorsement'
    })
    observeEvent(input$m_dr_link, {
        click$id <- manager
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
    
    # observe({
    #     print(click_info())
    # })
    
    # return(click_info)
}