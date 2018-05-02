shinyServer(function(input, output, session) {
    ##### Login Signup ####
    observe({
        if (input$user_pwd != '' & input$user_ID %like% 'GTS' & nchar(input$user_ID) == 7 & input$agree & input$user_name == '') {
            shinyjs::enable(id = 'login')
            # shinyjs::enable(id = 'signup')
        } else {
            shinyjs::disable(id = 'login')
            # shinyjs::disable(id = 'signup')
        }
    })
    observe({
        if (input$user_pwd != '' & input$user_ID %like% 'GTS' & nchar(input$user_ID) == 7 & input$agree & input$user_name != '') {
            # shinyjs::enable(id = 'login')
            shinyjs::enable(id = 'signup')
        } else {
            # shinyjs::disable(id = 'login')
            shinyjs::disable(id = 'signup')
        }
    })
    
    current_id <- reactiveVal(value = NULL)
    current_type <- reactiveVal(value = NULL)
    update_state <- reactiveVal(value = NULL)
    
    observeEvent(input$login, {
        shinyBS::closeAlert(session, alertId = 'warning_alert')
        if (!python.get(paste0(char(input$user_ID), ' in person.keys()'))) {
            shinyBS::createAlert(session, anchorId = 'warning', alertId = 'warning_alert', 
                                 title = 'User Not Registered.', 
                                 content = 'You have not yet signed up. Please Sign Up now.', 
                                 style = 'warning', dismiss = TRUE)
            shinyjs::show(id = 'user_name', anim = TRUE)
            #print('User Not Registered.')
        } else {
            if (python.get(paste0('person[', char(input$user_ID), ']["password"]')) != input$user_pwd) {
                shinyBS::createAlert(session, anchorId = 'warning', alertId = 'warning_alert', 
                                     title = 'Incorrect Entry', 
                                     content = 'Input GTS_ID and Passowrd doesn\'t match', 
                                     style = 'danger', dismiss = TRUE)
                #print('Incorrect Entry')
            } else {
                current_id(input$user_ID)
                update_state(sample(1:1000, 1))
                shinyjs::hide(id = 'login_page', anim = TRUE)
                shinyjs::show(id = 'search_bar', anim = TRUE)
                shinyjs::show(id = 'profile_page', anim = TRUE)
            } 
        }
    })
    
    
    observeEvent(input$signup, {
        shinyBS::closeAlert(session, 'warning_alert')
        if (input$user_ID %in% python.get('person.keys()')) {
            createAlert(session, anchorId = 'warning', alertId = 'warning_alert',
                        title = 'User Already Registered.',
                        content = 'You have already signed up. Try logging in.',
                        style = 'warning', dismiss = TRUE)
            #print('User Already Registered.')
        } else {
            current_id(input$user_ID)
            current_type('self')
            update_state(sample(1:1000, 1))
            python.call('add_new_user', input$user_ID, input$user_name)
            python.exec('read_data()')
            python.exec(paste0('person[', char(input$user_ID), ']["password"] = ', char(input$user_pwd)))
            python.exec('write_data()')
            shinyjs::hide(id = 'login_page', anim = TRUE)
            shinyjs::show(id = 'search_bar', anim = TRUE)
            shinyjs::show(id = 'profile_page', anim = TRUE)
        }
    })
    
    ##### Profile #####
    
    associate_profile <- eventReactive(current_id(), {
        #print(system.time(python.exec('read_data()')))
        #print('recalculating profile')
        p <- python.get(paste0('profile[', char(current_id()), ']'))
        return(p)
    })
    
    observe({
        if (!is.null(current_id()) && current_id() == 'GTS9999') {
            shinyjs::hide('profile_page', anim = TRUE)
        }
    })
    
    observe({
        if (!is.null(current_id()) && !is.null(associate_profile()$DirectReports)) {
            show(id = 'reportee', anim = TRUE)
        } else {
            hide(id = 'reportee', anim = TRUE)
        }
    })
    
    output$name <- renderValueBox({
        sub <- associate_profile()$Designation
        if (!is.null(associate_profile()$Domain)) {
            if (!is.null(associate_profile()$Designation)) {
                sub <- paste0(sub, ' in ', associate_profile()$Domain)
            } else {
                sub <- associate_profile()$Domain
            }
        }
        valueBox(value = associate_profile()$Name, subtitle = sub, color = 'green', width = 12)
    })
    
    output$manager <- renderValueBox({
        # if (current_id() == 'GTS0000') {
        #     valueBox(value = 'Hari Vasudev', 
        #              subtitle = 'Manager ::', color = 'maroon', width = 12)
        # } else {
        all_manager <- python.get('[person[k]["Name"] for k in manager.keys()]')
        if (is.null(associate_profile()$Manager) || !associate_profile()$Manager %in% all_manager) {
            valueBox(value = associate_profile()$Manager, 
                     subtitle = 'Manager ::', color = 'maroon', width = 12)
        } else {
            valueBox(value = actionLink(inputId = 'manager_link', label = associate_profile()$Manager), 
                     subtitle = 'Manager ::', color = 'maroon', width = 12)
        }
        # }
    })
    
    output$connections <- renderValueBox(valueBox(value = actionLink(inputId = 'conn_link', label = associate_profile()$Connections), 
                                                  subtitle = 'Connections', color = 'yellow', width = 12))
    output$reportee <- renderValueBox({
        valueBox(value = actionLink(inputId = 'dr_link', label = length(associate_profile()$DirectReports)),
                 subtitle = 'Direct Reports', color = 'yellow', width = 12)
    })
    output$endorsement <- renderValueBox(valueBox(value = actionLink(inputId = 'endorse_link', 
                                                                     label = paste(associate_profile()$Endorsements[1], '/', associate_profile()$Endorsements[2])), 
                                                  subtitle = 'Endorsements : Received / Endorsed', color = 'yellow', width = 12))
    
    observe({
        if (!is.null(current_id()) && current_id() != input$user_ID) {
            shinyjs::hide('edit_profile')
            shinyjs::hide('add_skill')
        } else {
            shinyjs::show('edit_profile')
            shinyjs::show('add_skill')
        }
    })
    observeEvent(input$manager_link, {
        new_id <- python.get(paste0('[k for k in person.keys() if person[k]["Name"] == ', char(associate_profile()$Manager), ']'))
        current_type('other')
        current_id(new_id)
        update_state(sample(1:1000, 1))
    })
    observeEvent(input$conn_link, {
        current_type('connections')
    })
    observeEvent(input$dr_link, {
        current_type('directs')
    })
    observeEvent(input$endorse_link, {
        current_type('endorsements')
    })
    ##### Profile Skills ####
    obsList_skills <- list()
    
    skills <- eventReactive(current_id(), {
        skill_person <- python.get(paste0('skill_person[', char(current_id()), ']')) %>% as.data.frame()
        python.assign('s', skill_person$skills)
        skill_name <- python.get('[skill["Skill"][k-1] for k in s]')
        skill_rate <- sapply(skill_person$skills, function(x) python.call('get_aggregate_skill_rank', current_id(), x))
        skill_person %<>% select(., skills) %>% mutate(., skill_name, 'rate' = skill_rate) %>% arrange(., desc(rate))
        
        relative_rate <- lapply(as.list(skill_person$skills), function(x) {
            python.assign('p', python.call('get_persons_with_skill', x))
            if (length(python.call('get_persons_with_skill', x)) == 1) {
                p1 <- python.get(paste0('skill_person[p]["rate"][skill_person[p]["skills"].index(', x, ')]'))
                p <- data.frame('user' = current_id(), 'skills' = x, 'rank' = p1, 'icon' = 'bolt', star = '* *') %>% 
                    select(skills, star, icon)
            } else {
                p <- python.get(paste0('[get_aggregate_skill_rank(k, ', x, ') for k in p]'))
                p <- data.frame('user' = python.get('p'), 'rank' = p)
                #p %<>% unlist() %>% matrix(.,  ncol = 2, byrow = TRUE) %>% as.data.frame() %>% set_colnames(c('user', 'rank'))
                
                p$rank <- as.numeric(p$rank)
                b <- quantile(p$rank, c(0, 0.2, 0.4, 0.6, 0.8, 1))
                b[6] <- b[6] + 1
                b[1] <- b[1] - 1
                p %<>% mutate(rating = cut(p$rank, breaks = unique(b), 
                                           include.lowest = TRUE) %>% as.numeric()
                ) %>% 
                    mutate(star = apply(., 1, function(x) paste(rep('*', x[3]), collapse = ' ')),
                           max_r = max(rating),
                           icon = if_else(rating == max_r & rating != 1, 'star', 'shit'),
                           skills = x) %>% 
                    filter(user == current_id()) %>% 
                    select(skills, star, icon)
            }
            return(p)
        })
        
        if (skill_person %>% nrow() != 0)
            skill_person %<>% inner_join(., do.call('rbind', relative_rate)) %>% as.data.frame()
        
        # b <- quantile(skill_person$rate, c(0, 0.2, 0.4, 0.6, 0.8, 1))
        # b[6] <- b[6] + 1
        # b[1] <- b[1] - 1
        # skill_person %<>% mutate(rank = cut(skill_person$rate, breaks = unique(b), 
        #                                     include.lowest = TRUE) %>% as.numeric()
        # ) %>% 
        #     mutate(star = apply(., 1, function(x) paste(rep('*', x[4]), collapse = ' ')))
        
        return(skill_person)
    })
    
    #observe(delay(100, {
    observeEvent(skills(), {
        #print(skills())
        output$skill_ui <- renderUI({
            if (nrow(skills()) == 0 & current_id() == input$user_ID) {
                valueBox(value = 'While we appreciate your Modesty', 
                         subtitle = 'You must add a few skills to make your profile stronger', color = 'teal', width = 12)
            } else {
                if (nrow(skills()) == 0 & current_id() != input$user_ID) {
                    NULL
                } else {
                    nbr <- nrow(skills())
                    dr_id <- skills()$skills
                    dr <- data.frame('DR' = skills()$skill_name, 'star' = skills()$star, 'icon' = skills()$icon) %>% 
                        mutate_all(funs(as.character))
                    fluidRow(
                        output_list <- lapply(1:nbr, function(i) {
                            name = paste0("name_", i)
                            if (current_id() %in% python.get(paste0('connection[', char(input$user_ID), ']')) ) {
                                if (dr$DR[i] %in% python.get(paste0('profile[', char(input$user_ID), ']["Skills"]')) ) {
                                    if (!paste0(current_id(), '_', dr_id[i], '_', input$user_ID) %in% python.get('endorsement_rating.keys()')) {
                                        link_name <- paste0(current_id(), '_', dr_id[i])
                                        
                                        if (is.null(obsList_skills[[link_name]])) {
                                            obsList_skills[[link_name]] <<- observeEvent(input[[link_name]], {
                                                update_state(sample(1:1000, 1))
                                                a <- associate_profile()
                                                end_result <- python.call('add_endorsement', current_id(), dr_id[i], input$user_ID)
                                                delay(200, expr = removeTooltip(session, id = link_name))
                                            })
                                        }
                                        return(
                                            column(width = 3, 
                                                   tipify(valueBox(value = dr$star[i], subtitle = actionLink(inputId = link_name, label = strong(dr$DR[i])), 
                                                                   color = 'teal', width = 12, icon = icon(dr$icon[i])), 
                                                          title = 'Click the Skill to Endorse', trigger = 'hover'),
                                                   bsTooltip(id = link_name, title = 'Endorsement Successful', trigger = 'click')
                                            )
                                        )
                                    } else {
                                        return(
                                            column(width = 3, 
                                                   tipify(valueBox(value = dr$star[i], subtitle = strong(dr$DR[i]), 
                                                                   color = 'teal', width = 12, icon = icon(dr$icon[i])),
                                                          title = 'Already Endorsed the User in selected Skill', trigger = 'hover', placement = 'right')
                                            )
                                        )
                                    }
                                    
                                } else {
                                    return(
                                        column(width = 3, 
                                               tipify(valueBox(value = dr$star[i], subtitle = strong(dr$DR[i]), 
                                                               color = 'teal', width = 12, icon = icon(dr$icon[i])),
                                                      title = 'You have not listed the selected Skill', trigger = 'hover', placement = 'right')
                                        )
                                    )
                                }
                            } else {
                                if (current_id() == input$user_ID) {
                                    return(
                                        column(width = 3, 
                                               valueBox(value = dr$star[i], subtitle = strong(dr$DR[i]), 
                                                        color = 'teal', width = 12, icon = icon(dr$icon[i]))
                                        )
                                    )
                                } else {
                                    return(
                                        column(width = 3, 
                                               tipify(valueBox(value = dr$star[i], subtitle = strong(dr$DR[i]), 
                                                               color = 'teal', width = 12, icon = icon(dr$icon[i])),
                                                      title = 'You have to connected to User, to be able to endorse', trigger = 'hover', placement = 'right')
                                        )
                                    )
                                }
                            }
                            
                        })
                    )
                    do.call(tagList, output_list)
                }
            }
        })
        
    })
    
    #}))
    
    ##### Associates ####
    associates <- eventReactive(current_type(), {
        Uid <- current_id()
        if (current_type() == 'connections') {
            associate <- python.get(paste0('connection[', char(Uid), ']'))
            python.assign('associate', associate)
            if (length(associate) == 1) {
                associate_name <- python.get('person[associate]["Name"]')
            } else {
                associate_name <- python.get('[person[k]["Name"] for k in associate]')
            }
            
            shinyjs::show(id = 'connection_box', anim = TRUE)
            shinyjs::hide(id = 'directs_box', anim = TRUE)
            shinyjs::hide(id = 'profile_page', anim = TRUE)
            shinyjs::hide(id = 'search_result_skill', anim = TRUE)
            shinyjs::hide(id = 'endorsement_box', anim = TRUE)
        } else {
            if (current_type() == 'directs' & Uid %in% python.get('manager.keys()')) {
                associate <- python.get(paste0('manager[', char(Uid), ']'))
                associate <- associate[associate != Uid]
                python.assign('associate', associate)
                if (length(associate) == 1) {
                    associate_name <- python.get('person[associate]["Name"]')
                } else {
                    associate_name <- python.get('[person[k]["Name"] for k in associate]')
                }
                
                shinyjs::hide(id = 'connection_box', anim = TRUE)
                shinyjs::show(id = 'directs_box', anim = TRUE)
                shinyjs::hide(id = 'profile_page', anim = TRUE)
                shinyjs::hide(id = 'search_result_skill', anim = TRUE)
                shinyjs::hide(id = 'endorsement_box', anim = TRUE)
            } else {
                if (current_type() == 'endorsements') {
                    shinyjs::show(id = 'endorsement_box', anim = TRUE)
                    shinyjs::hide(id = 'connection_box', anim = TRUE)
                    shinyjs::hide(id = 'directs_box', anim = TRUE)
                    shinyjs::hide(id = 'profile_page', anim = TRUE)
                    shinyjs::hide(id = 'search_result_skill', anim = TRUE)
                    all_endorsements <- python.get('endorsement_rating.keys()')
                    if (length(all_endorsements) > 0) {
                        all_endorsements %<>% 
                            strsplit(., split = '_') %>% 
                            do.call('rbind', .) %>% 
                            as.data.frame() %>% 
                            set_colnames(., c('to', 'skill', 'from'))
                        recd <- all_endorsements %>% filter(to == current_id())
                        given <- all_endorsements %>% filter(from == current_id())
                        if (nrow(recd) + nrow(given) > 0) {
                            skill_df <- python.get('skill') %>% do.call('cbind', .) %>% as.data.frame() %>% mutate_all(funs(as.character)) %>% 
                                set_colnames(c('skill', 'skill_name'))
                            person_df <- python.get('[(k, person[k]["Name"]) for k in person.keys()]') %>% 
                                do.call('rbind', .) %>% 
                                as.data.frame() %>% set_colnames(c('id', 'name'))
                            associate <- recd %>% left_join(skill_df) %>% left_join(., person_df, by = c('from'='id'))
                            associate_name <- given %>% left_join(skill_df) %>% left_join(., person_df, by = c('to'='id'))
                        } else {
                            associate <- NULL
                            associate_name <- NULL
                        }
                    } else {
                        associate <- NULL
                        associate_name <- NULL
                    }
                } else {
                    if (current_id() != input$user_ID) {
                        current_type('other')
                    } else {
                        current_type('self')
                    }
                    associate <- NULL
                    associate_name <- NULL
                    
                    shinyjs::hide(id = 'connection_box', anim = TRUE)
                    shinyjs::hide(id = 'directs_box', anim = TRUE)
                    shinyjs::show(id = 'profile_page', anim = TRUE)
                    shinyjs::hide(id = 'search_result_skill', anim = TRUE)
                    shinyjs::hide(id = 'endorsement_box', anim = TRUE)
                }
            }
        }
        return(list(associate, associate_name))
    })
    
    observe({
        associates()
    })
    
    
    ##### Connections ####
    obsList_conn <- list()
    output$conns <- renderUI({
        #print(current_type())
        if (current_type() == 'connections') {
            nbr <- length(associates()[[2]])
            dr_id <- associates()[[1]]
            dr <- data.frame('DR' = associates()[[2]]) %>% mutate_all(funs(as.character))
            
            fluidRow(
                output_list <- lapply(1:nbr, function(i) {
                    name = paste0("name_", i)
                    link_name <- paste0(current_type(), current_id(), i)
                    
                    if (is.null(obsList_conn[[link_name]])) {
                        obsList_conn[[link_name]] <<- observeEvent(input[[link_name]], {
                            if (dr_id[i] != input$user_ID) {
                                current_type('other')
                            } else {
                                current_type('self')
                            }
                            current_id(dr_id[i])
                        })
                    }
                    column(width = 3, 
                           valueBox(value = actionLink(inputId = link_name, label = substr(dr$DR[i], 1, 1)), 
                                    subtitle = dr$DR[i], color = 'yellow', width = 12))
                })
            )
            do.call(tagList, output_list)
        } else {
            NULL
        }
    })
    
    ##### Directs #####
    obsList_dr <- list()
    output$DRs <- renderUI({
        if (current_type() == 'directs') {
            nbr <- length(associates()[[2]])
            dr_id <- associates()[[1]]
            dr <- data.frame('DR' = associates()[[2]]) %>% mutate_all(funs(as.character))
            
            fluidRow(
                output_list <- lapply(1:nbr, function(i) {
                    name = paste0("name_", i)
                    link_name <- paste0(current_type(), current_id(), i)
                    
                    if (is.null(obsList_dr[[link_name]])) {
                        obsList_dr[[link_name]] <<- observeEvent(input[[link_name]], {
                            current_id(dr_id[i])
                            if (dr_id[i] != input$user_ID) {
                                current_type('other')
                            } else {
                                current_type('self')
                            }
                        })
                    }
                    column(width = 3,
                           valueBox(value = actionLink(inputId = link_name, label = substr(dr$DR[i], 1, 1)),
                                    subtitle = dr$DR[i], color = 'yellow', width = 12))
                })
            )
            do.call(tagList, output_list)
        } else {
            NULL
        }
    })
    
    ##### Endorsements ####
    obsList_end_gvn <- list()
    output$end_given <- renderUI({
        if (current_type() == 'endorsements') {
            if (is.null(associates()[[2]])) {
                return(valueBox(value = 'No Endorsements Made as Yet.', subtitle = '', width = 12))
            } else {
                n <- nrow(associates()[[2]])
                df <- associates()[[2]]
                if (n == 0) {
                    return(valueBox(value = 'No Endorsements Made as Yet.', subtitle = '', width = 12))
                } else {
                    fluidRow(
                        output_list <- lapply(1:n, function(i) {
                            name = paste0('name_', i)
                            column(width = 4,
                                   valueBox(value = df$name[i], subtitle = df$skill_name[i], color = 'purple', width = 12)
                            )
                        })
                    )
                    return(do.call(tagList, output_list))
                }
            }
        } else {
            NULL
        }
    })
    
    obsList_end_rcd <- list()
    output$end_recd <- renderUI({
        if (current_type() == 'endorsements') {
            if (is.null(associates()[[1]])) {
                return(valueBox(value = 'No Endorsements Received as Yet.', subtitle = '', width = 12))
            } else {
                n <- nrow(associates()[[1]])
                df <- associates()[[1]]
                if (n == 0) {
                    return(valueBox(value = 'No Endorsements Received as Yet.', subtitle = '', width = 12))
                } else {
                    fluidRow(
                        output_list <- lapply(1:n, function(i) {
                            name = paste0('name_', i)
                            column(width = 4,
                                   valueBox(value = df$name[i], subtitle = df$skill_name[i], color = 'orange', width = 12)
                            )
                        })
                    )
                    return(do.call(tagList, output_list))
                }
            }
        } else {
            NULL
        }
    })
    
    observeEvent(input$back_to_host_p, {
        current_id(input$user_ID)
        current_type('self')
    })
    ##### Connect #####
    observe({
        if (!is.null(current_id()) && current_id() != input$user_ID && current_type() == 'other') {
            update_state(sample(1:1000, 1))
            connections <- python.get(paste0('connection[', char(input$user_ID), ']'))
            connection_received <- python.get(paste0('profile[', char(input$user_ID), ']["ConnectionReceived"]'))
            connection_sent <- python.get(paste0('profile[', char(input$user_ID), ']["ConnectionInitiated"]'))
            if (current_id() %in% connections) {
                shinyjs::hide(id = 'connect_with', anim = TRUE)
                shinyjs::hide(id = 'connection_received', anim = TRUE)
                shinyjs::hide(id = 'connection_sent', anim = TRUE)
                shinyjs::show(id = 'level', anim = TRUE)
            } else {
                if (current_id() %in% connection_received) {
                    shinyjs::hide(id = 'connect_with', anim = TRUE)
                    shinyjs::show(id = 'connection_received', anim = TRUE)
                    shinyjs::hide(id = 'connection_sent', anim = TRUE)
                    shinyjs::show(id = 'level', anim = TRUE)
                } else {
                    if (current_id() %in% connection_sent) {
                        shinyjs::hide(id = 'connect_with', anim = TRUE)
                        shinyjs::hide(id = 'connection_received', anim = TRUE)
                        shinyjs::show(id = 'connection_sent', anim = TRUE)
                        shinyjs::show(id = 'level', anim = TRUE)
                    } else {
                        shinyjs::show(id = 'connect_with', anim = TRUE)
                        shinyjs::hide(id = 'connection_received', anim = TRUE)
                        shinyjs::hide(id = 'connection_sent', anim = TRUE)
                        shinyjs::show(id = 'level', anim = TRUE)
                    }
                }
            }
        }
        if (!is.null(current_id()) && current_id() == input$user_ID) {
            shinyjs::hide(id = 'connect_with', anim = TRUE)
            shinyjs::hide(id = 'connection_received', anim = TRUE)
            shinyjs::hide(id = 'connection_sent', anim = TRUE)
            shinyjs::hide(id = 'level', anim = TRUE)
        }
    })
    
    output$level <- renderValueBox({
        l <- python.call('get_connection_level', current_id(), input$user_ID)
        valueBox(value = l, subtitle = 'Order', color = 'red', width = 12)
    })
    
    ##### Notification #####
    obsList_noti_accept <- list()
    obsList_noti_profile <- list()
    
    observeEvent(input$connect_now, {
        update_state(sample(1:1000, 1))
        python.call('initiate_connection', input$user_ID, current_id())
        update_state(sample(1:1000, 1))
        shinyjs::hide(id = 'connect_with', anim = TRUE)
        shinyjs::show(id = 'connection_sent', anim = TRUE)
    })
    observeEvent(input$add_connect, {
        update_state(sample(1:1000, 1))
        python.call('add_connection', current_id(), input$user_ID)
        update_state(sample(1:1000, 1))
        shinyjs::hide(id = 'connection_received', anim = TRUE)
    })
    
    observeEvent(associate_profile(), {
        received <- python.get(paste0('profile[', char(input$user_ID), ']["ConnectionReceived"]'))
        #print(paste('received', received))
        #print(paste('login', input$login))
        output$notification <- renderMenu({
            if (input$login > 0 && length(received) > 0) {
                noti <- lapply(received, function(x) {
                    #print(paste('received', received))
                    button_link <- paste0('button_', x)
                    if (is.null(obsList_noti_profile[[paste0(button_link, '_profile')]])) {
                        obsList_noti_profile[[paste0(button_link, '_profile')]] <<- observeEvent(input[[paste0(button_link, '_profile')]], {
                            current_id(x)
                            if (x != input$user_ID) {
                                current_type('other')
                            } else {
                                current_type('self')
                            }
                        })
                    }
                    if (is.null(obsList_noti_accept[[paste0(button_link, '_accept')]])) {
                        obsList_noti_accept[[paste0(button_link, '_accept')]] <<- observeEvent(input[[paste0(button_link, '_accept')]], {
                            python.call('add_connection', x, input$user_ID)
                        })
                    }
                    name <- python.get(paste0('person[', char(x), ']["Name"]'))
                    messageItemCustom <- function(from, message, time, time_icon, space = '       ') {
                        tags$li(a(h4(from), h5(message, '  --- or ---  ', shiny::icon(time_icon), time)))
                    }
                    
                    messageItemCustom(from = name,
                                      message = actionLink(inputId = paste0(button_link, '_accept'), label = 'ACCEPT', icon = icon('check')), 
                                      time = actionLink(inputId = paste0(button_link, '_profile'), label = 'View Profile'),
                                      time_icon = 'user'
                    )
                })
            } else {
                #print(paste('not received', received))
                noti <- NULL
            }
            dropdownMenuCustom(type = 'notifications', .list = noti, badgeStatus = 'danger', header = TRUE, text = 'new Connection Request')
        })
    })
    
    observeEvent(input$back_to_host, {
        if (current_id() != input$user_ID) {
            current_type('other')
        } else {
            current_type('self')
        }
    })
    observeEvent(input$back_to_manager, {
        if (current_id() != input$user_ID) {
            current_type('other')
        } else {
            current_type('self')
        }
    })
    
    ##### Search Result ####
    
    observe({
        if (input$search_box_skill != '' & input$search_box_associate == '') {
            shinyjs::enable('search')
        } else {
            if (input$search_box_skill == '' & input$search_box_associate != '') {
                shinyjs::enable('search')
            } else {
                shinyjs::disable('search')
            }
        }
    })
    
    observeEvent(current_id(), {
        python.exec('read_data()')
        updateSelectizeInput(session, inputId = 'search_box_associate', choices = c('', python.get('[person[k]["Name"] for k in person.keys()]')))
        updateSelectizeInput(session, inputId = 'search_box_skill', choices = c('', python.get('skill["Skill"]')))
    })
    
    associate_skill <- eventReactive(input$search, {
        associate_skill <- python.call('get_persons_with_skill_ordered', which(python.get('skill')$Skill == input$search_box_skill))
        python.assign('skilled_id', associate_skill)
        if (length(associate_skill) == 1){
            associate_skill_2 <- python.get(paste0('person[', char(associate_skill), ']["Name"]'))
        } else {
            associate_skill_2 <- python.get('[person[k]["Name"] for k in skilled_id]')
        }
        return(list(associate_skill, associate_skill_2))
    })
    
    
    output$search_result <- renderUI({
        obsList_search <- list()
        nbr <- length(associate_skill()[[1]])
        dr_id <- associate_skill()[[1]]
        dr <- data.frame('DR' = associate_skill()[[2]]) %>% mutate_all(funs(as.character))
        
        fluidRow(
            output_list <- lapply(1:nbr, function(i) {
                name = paste0("name_", i)
                link_name <- paste0('search', current_id(), i)
                
                if (is.null(obsList_search[[link_name]])) {
                    obsList_search[[link_name]] <<- observeEvent(input[[link_name]], {
                        #print(paste('search', dr_id[i]))
                        current_id(dr_id[i])
                        current_type(sample(LETTERS, 1))
                    })
                }
                column(width = 3,
                       valueBox(value = actionLink(inputId = link_name, label = substr(dr$DR[i], 1, 1)),
                                subtitle = dr$DR[i], color = 'yellow', width = 12))
            })
        )
        do.call(tagList, output_list)
    })
    
    observeEvent(input$search, {
        shinyjs::hide(id = 'connection_box', anim = TRUE)
        shinyjs::hide(id = 'directs_box', anim = TRUE)
        shinyjs::hide(id = 'endorsement_box', anim = TRUE)
        shinyjs::show(id = 'profile_page', anim = TRUE)
        shinyjs::hide(id = 'search_result_skill', anim = TRUE)
        if (input$search_box_skill == '' & input$search_box_associate != '') {
            shinyjs::hide(id = 'search_result_skill', anim = TRUE)
            python.exec('read_data()')
            associate <- python.get(paste0('[k for k in person.keys() if person[k]["Name"] == ', char(input$search_box_associate), ']'))
            current_id(associate)
            current_type('A')
        }
        if (input$search_box_skill != '' & input$search_box_associate == '') {
            shinyjs::hide(id = 'profile_page', anim = TRUE)
            shinyjs::show(id = 'search_result_skill', anim = TRUE)
        }
    })
    observeEvent(input$home, {
        current_id(input$user_ID)
        current_type('home')
    })
    
    
    ##### Modal Skill Edit ####
    observeEvent(input$edit_link, {
        python.exec('read_data()')
        updateSelectizeInput(session, inputId = 'designation', selected = associate_profile()$Designation)
        updateTextInput(session, inputId = 'curr_team', value = associate_profile()$Domain)
        updateSelectInput(session, inputId = 'change_manager', selected = associate_profile()$Manager)
    })
    
    observeEvent(input$add_more_skill, {
        update_state(sample(1:1000, 1))
        a <- associate_profile()
        skills_i_have <- python.call('get_all_skills', input$user_ID)
        all_skills <- python.get('skill["Skill"]')
        choices <- anti_join(data.frame('skill' = as.character(all_skills)), data.frame('skill' = as.character(skills_i_have)))
        output$new_skill <- renderUI({
            fluidPage(
                selectInput(inputId = 'existing_skill_to_add', label = 'Choose from already existing skills', multiple = TRUE, choices = choices$skill),
                textAreaInput(inputId = 'new_skill_to_add', label = 'Didn\'t find your skill in the above list?', 
                              placeholder = 'Enter your UNIQUE skills, sepearted by a comma (,).')
            )
        })
    }, ignoreInit = TRUE)
    
    observeEvent(input$save, {
        toggleModal(session, modalId = 'edit_modal', toggle = 'close')
        if (! input$designation %in% c('', ' ')) {
            python.assign(paste0('profile[', char(input$user_ID), ']["Designation"]'), input$designation)
            python.exec('write_data()')
        }
        if (! input$curr_team %in% c('', ' ')) {
            python.assign(paste0('profile[', char(input$user_ID), ']["Domain"]'), input$curr_team)
            python.exec('write_data()')
        }
        if (! input$change_manager %in% c('', ' ', 'None')) {
            python.assign('m', input$change_manager)
            manager_id <- python.get('[k for k in manager.keys() if person[k]["Name"] == m]')
            python.call('change_manager', input$user_ID, manager_id)
        }
        #print(input$change_manager)
        if (input$change_manager == 'None' & !input$brand_new_manager %in% c('', ' ')) {
            python.exec(paste0('profile[', char(input$user_ID), ']["Manager"] = ', char(input$brand_new_manager)))
            python.exec('write_data()')
        }
        output$new_skill <- renderUI({NULL})
        if (input$add_more_skill) {
            sapply(input$existing_skill_to_add, function(x) python.call('add_skill_to_person', input$user_ID, x))
            sapply(strsplit(input$new_skill_to_add, split = ',') %>% unlist(), function(x) python.call('add_skill_to_person', input$user_ID, trimws(x)))
        }
        update_state(sample(1:1000, 1))
        a <- associate_profile()
    })
    ##### Path ####
    get_path <- eventReactive(current_id(), {
        if (current_id() == input$user_ID | current_id() %in% python.get(paste0('connection[', char(input$user_ID), ']'))) {
            return(NULL)
        } else {
            paths <- python.call('get_path', current_id(), input$user_ID)
            if (all.equal(paths$best_path, paths$all_short_paths)) {
                return(list('equal', paths$best_path))
            } else {
                return(paths)
            }
        }
    })
    
    output$path <- renderUI({
        P <- get_path()
        if (is.null(P)) {
            return(NULL)
        } else {
            path <- get_path()[[2]][[1]]
            python.assign('p', path)
            python.exec('read_data()')
            names <- paste(c('You', rev(python.get('[person[k]["Name"] for k in p]'))), collapse = ' -> ')
            #print(names)
            return(
                valueBox(value = names, subtitle = 'Ideal Path to Initiate a Connection.', color = 'orange', width = 12)
            )
        }
    })
    
    ##### Endorsement Notification ####
    obsList_noti_end_recd <- list()
    observeEvent(associate_profile(), {
        received <- python.get(paste0('profile[', char(input$user_ID), ']["Endorsements"]["New"]'))
        #print(received)
        messageItemCustom2 <- function(from, message) {
            tags$li(a(h4(from), h5(message)))
        }
        output$endorse_info <- renderMenu({
            if (input$login > 0 && length(received) > 0) {
                received_from <- received %>% strsplit(., split = '_') %>% lapply(., function(x) tail(x, 1)) %>% unlist()
                skill <- received %>% strsplit(., split = '_') %>% lapply(., function(x) x[2]) %>% unlist() %>% as.numeric()
                
                noti <- lapply(1:(length(received_from) + 1), function(x) {
                    if (x <= length(received_from)) {
                        #print(x)
                        button_link <- paste0('button1_', received[x])
                        
                        if (is.null(obsList_noti_end_recd[[paste0(button_link, '_profile')]])) {
                            obsList_noti_end_recd[[paste0(button_link, '_profile')]] <<- observeEvent(input[[paste0(button_link, '_profile')]], {
                                current_id(received_from[x])
                                if (received_from[x] != input$user_ID) {
                                    current_type('other')
                                    python.exec(paste0('profile[', char(input$user_ID), ']["Endorsements"]["New"].remove(', char(received[x]), ')'))
                                    python.exec('write_data()')
                                } else {
                                    current_type('self')
                                }
                            })
                        }
                        
                        name <- python.get(paste0('person[', char(received_from[x]), ']["Name"]'))
                        skill_name <- python.get(paste0('skill["Skill"][', skill[x] - 1, ']'))
                        messageItemCustom2(from = actionLink(inputId = paste0(button_link, '_profile'), label = name),
                                           message = skill_name
                        )
                    } else {
                        if (is.null(obsList_noti_end_recd[['read_all']])) {
                            obsList_noti_end_recd[['read_all']] <<-observeEvent(input[['read_all']], {
                                current_id(input$user_ID)
                                python.exec('read_data()')
                                python.exec(paste0('profile[', char(input$user_ID), ']["Endorsements"]["New"] = []'))
                                python.exec('write_data()')
                            })
                        }
                        messageItemCustom2(from = actionLink(inputId = 'read_all', label = 'Read all Notifications'), 
                                           message = '')
                    }
                    
                })
                #print(noti)
            } else {
                noti <- NULL
            }
            dropdownMenuCustom(type = 'notifications', .list = noti, badgeStatus = 'danger', header = TRUE, 
                               text = 'new Endorsement Received', n = 1, icon = icon('thumbs-up', 'fa-2x'))
        })
    })
    ##### logout ####
    obsList_noti_logout <- list()
    output$logout <- renderMenu({
        if (input$login | input$signup) {
            noti <- lapply(list(1), function (x) {
                messageItemCustom2 <- function(from, message) {
                    tags$li(a(h4(from), h5(message)))
                }
                if (is.null(obsList_noti_logout[['change_pwd']])) {
                    obsList_noti_logout[['change_pwd']] <- observeEvent(input[['change_pwd']], {
                        toggleModal(session, modalId = 'pwd', toggle = 'open')
                    })
                }
                
                
                messageItemCustom2(from = actionLink(inputId = 'change_pwd', label = 'Change Password'), message = '')
                
            })
            dropdownMenuCustom(type = 'notifications',  .list = noti, badgeStatus = 'danger', header = FALSE, 
                               text = 'new Endorsement Received', n = 1, icon = icon('lock', 'fa-2x'))
        } else {
            NULL
        }
    })
    observeEvent(input$conf_change, {
        if (input$new_pwd == input$conf_pwd) {
            closeAlert(session, alertId = 'hmm')
            python.exec('read_data()')
            python.exec(paste0('person[', char(input$user_ID), ']["password"] = ', char(input$new_pwd)))
            python.exec('write_data()')
            createAlert(session, anchorId = 'pwd_warning', alertId = 'hmmm', 
                        title = 'Password Changed Successfully', 
                        content = 'Use this New Password, next time you Log-In.', style = 'success')
        } else {
            closeAlert(session, alertId = 'hmm')
            createAlert(session, anchorId = 'pwd_warning', alertId = 'hmmm', 
                        title = 'Password Change Un-Successful', 
                        content = 'New Password and Confirm Password do not match', style = 'danger')
        }
    })
    
})

