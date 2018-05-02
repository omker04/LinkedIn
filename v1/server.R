shinyServer(function(input, output, session) {
    ##### Login Signup ####
    
    observe({
        if (input$user_name != '' & input$user_ID %like% 'GTS' & nchar(input$user_ID) == 7 & input$agree) {
            enable('login')
            enable('signup')
        } else {
            disable('login')
            disable('signup')
        }
    })
    
    observeEvent(input$login, {
        closeAlert(session, 'warning_alert')
        if (!python.get(paste0('"', input$user_ID, '" in person.keys()'))) {
            createAlert(session, anchorId = 'danger', alertId = 'warning_alert', 
                        title = 'User Not Registered.', 
                        content = 'You have not yet signed up. Please sign up now.', 
                        style = 'warning', dismiss = TRUE)
        } else {
            if (python.call('add_new_user', input$user_ID, input$user_name) %like% 'match') {
                createAlert(session, anchorId = 'warning', alertId = 'warning_alert', 
                            title = 'Incorrect Entry', 
                            content = 'Input GTS_ID and Name doesnt match', 
                            style = 'danger', dismiss = TRUE)
            } else {
                shinyjs::hide(id = 'login_page', anim = TRUE)
                shinyjs::show(id = 'search_bar', anim = TRUE)
                shinyjs::show(id = 'self_profile', anim = TRUE)
                # shinyjs::show(id = 'managers_profile', anim = TRUE)
                # shinyjs::show(id = 'connection_profile', anim = TRUE)
                # shinyjs::show(id = 'direct_report_profile', anim = TRUE)
                # hide(id = 'self_profile', anim = TRUE)
                #shinyjs::show(id = 'managers_profile', anim = TRUE)
            } 
        }
    })
    
    # observeEvent(input$signup, {
    #     closeAlert(session, 'warning_alert')
    #     if (python.call('add_new_user', input$user_ID, input$user_name) %like% 'already') {
    #         createAlert(session, anchorId = 'warning', alertId = 'warning_alert', 
    #                     title = 'User Already Registered.', 
    #                     content = 'You have already signed up. Try logging in.', 
    #                     style = 'warning', dismiss = TRUE)
    #     } else {
    #         shinyjs::hide(id = 'login_page', anim = TRUE)
    #         shinyjs::show(id = 'search_bar', anim = TRUE)
    #         shinyjs::show(id = 'self_profile', anim = TRUE)
    #     }
    # })
    # 
    
    current_click <- reactiveValues(type = NULL, id = NULL, self = 'Yes', anchor = NULL, manager = NULL)
    
    anchor <- reactive({
        print(self())
        if (!is.null(self()$id)) {
            current_click$anchor <- self()$id
        } else {
            current_click$anchor <- input$user_ID
        }
        manager = python.get(paste0('person[', char(current_click$anchor), ']["Manager"]'))
        return(list('anchor' = current_click$anchor, 'manager' = manager))
    })
    # observe({
    # self <<- callModule(self_profile_, id = 'self_page', self = input$user_ID, is_manager = python.get(paste0(char(input$user_ID), ' in manager.keys()')))
    # anchor_manager <<- callModule(manager_profile_, id = 'manager', manager = anchor()$manager)
    # anchor_connections <<- callModule(associate_names_, id = 'Conn', Uid = anchor()$anchor, type = 'connections')
    # anchor_directs <<- callModule(associate_names_, id = 'DR', Uid = anchor()$anchor, type = 'DR')
    # })
    # 
    # observe({
    #     if (is.null(self()$id)) {
    #         current_click$anchor <- input$user_ID
    #     } else {
    #         current_click$anchor <- self()$id
    #     }
    #     current_click$self <- ''
    #     print(self())
    #     print(paste('anchor', anchor()))
    # })
    # 
    # observe({
    #     print(anchor())
    # })
    # 
    callModule(self_profile_, id = 'self_page', self = input$user_ID, is_manager = python.get(paste0(char(input$user_ID), ' in manager.keys()')))
    # 
    # 
    # observe({
    #     if (!is.null(s()$id)) {
    #         current_click$id <- s()$id
    #         current_click$type <- s()$type
    #         current_click$self <- 'No'
    #     }
    # })
    # 
    # new <- eventReactive(current_click, {
    #     if (current_click$type == 'Manager') {
    #         show(id = 'managers_profile', anim = TRUE)
    #         hide(id = 'connection_profile', anim = TRUE)
    #         hide(id = 'direct_report_profile', anim = TRUE)
    #         hide(id = 'self_profile', anim = TRUE)
    #         s1 <- callModule(manager_profile_, id = 'manager', manager = s()$id)
    #     }
    #     if (current_click$type == 'Connection') {
    #         hide(id = 'managers_profile', anim = TRUE)
    #         show(id = 'connection_profile', anim = TRUE)
    #         hide(id = 'direct_report_profile', anim = TRUE)
    #         hide(id = 'self_profile', anim = TRUE)
    #         s1 <- callModule(associate_names_, id = 'Conn', Uid = s()$id, type = 'connections')
    #     }
    #     if (current_click$type == 'Directs') {
    #         hide(id = 'managers_profile', anim = TRUE)
    #         hide(id = 'connection_profile', anim = TRUE)
    #         show(id = 'direct_report_profile', anim = TRUE)
    #         hide(id = 'self_profile', anim = TRUE)
    #         s1 <- callModule(associate_names_, id = 'DR', Uid = s()$id, type = 'DR')
    #     }
    #     return(s1)
    # })
    # 
    # 
    # change_current_value <- eventReactive(new(), {
    #     s <- new()
    #     print('esss')
    #     print(s())
    #     current_click$id <- s()$id
    #     current_click$type <- s()$type
    #     return(new())
    # })
    # 
    # observe({
    #     print(paste('observe', current_click$type))
    #     print(paste('observe', current_click$id))
    #     if (!is.null(current_click$type) && !is.null(current_click$id) && current_click$self == 'No') {
    #         s1 <- new()
    #         change_current_value()
    #     } else {
    #         print('else')
    #     }
    # })
    # observe({
    #     print('observe')
    #     print(current_click$id)
    #     print(current_click$type)
    #     print(type())
    #     print(id())
    # })
    
})