header <- dashboardHeader(title = 'Walmart Skill Metrics', 
                          dropdownMenuOutput('notification'),
                          dropdownMenuOutput('endorse_info'),
                          dropdownMenuOutput('logout'))
sidebar <- dashboardSidebar(disable = TRUE)
body <- dashboardBody(fluidPage(
    shinyjs::useShinyjs(),
    ##### Login Page ####
    div(id = 'login_page', 
        column(width = 6, offset = 3, 
               box(title = 'Log-In or Sign-Up :', status = 'primary', solidHeader = TRUE, width = 12,
                   shinyjs::hidden(textInput(inputId = 'user_name', label = 'Please Enter your Full Name : *', value = NULL,
                                             placeholder = 'Enter your Full Name', width = '100%')),
                   textInput(inputId = 'user_ID', label = 'Please Enter your 7 diigt GTS-ID without any space : *', value = NULL,
                             placeholder = 'GTS-ID without any space (eg : GTS0000)', width = '100%'),
                   passwordInput(inputId = 'user_pwd', label = 'Please Enter your Password : *', value = NULL, width = '100%'),
                   hr(),
                       h4('Disclaimer'), 
                       p('This is just an avenue to understand and explore the Skill Metrics of Walmart Associates, and is not intended to be considered as a metric for evaluation.'),
                   hr(),
                   checkboxInput(inputId = 'agree', label = 'I have READ and UNDERSTOOD the Disclaimers.'), 
                   disabled(actionButton(inputId = 'login', label = 'Log-In', width = '49.5%')),
                   disabled(actionButton(inputId = 'signup', label = 'Sign-Up', width = '49.5%')),
                   shinyBS::bsAlert(anchorId = 'warning')
               )
        )
    ),
    bsModal(id = 'pwd', title = 'Change Password', trigger = 'change_pwd', size = 'small', 
            passwordInput(inputId = 'new_pwd', label = 'Enter Your New Password :'),
            passwordInput(inputId = 'conf_pwd', label = 'Re-Enter Your New Password :'),
            actionButton(inputId = 'conf_change', label = 'Confirm Password Change', icon = icon('check')),
            bsAlert(anchorId = 'pwd_warning')
    ),
    ##### Search Box ####
    shinyjs::hidden(div(id = 'search_bar', 
                        box(title = 'Search Options', status = 'primary', solidHeader = TRUE, width = 12, collapsible = TRUE, collapsed = FALSE, background = 'blue',
                            valueBox(subtitle =
                                         fluidRow(
                                             column(width = 4, selectizeInput(inputId = 'search_box_associate', width = '100%',
                                                                              choices = c('', python.get('[person[k]["Name"] for k in person.keys()]')),
                                                                              label = 'Search By Associate Name', selected = '')
                                             ),
                                             column(width = 2, h3('OR', align = 'center')),
                                             column(width = 4, selectizeInput(inputId = 'search_box_skill', width = '100%',
                                                                              choices = c('', python.get('skill["Skill"]')),
                                                                              label = 'Search By Skill', selected = '')
                                             ),
                                             column(width = 2, br(),
                                                    shinyjs::disabled(actionButton(inputId = 'search', label = 'Search', icon = icon('search')))
                                             )
                                         ),
                                     value = '', icon = icon('search'), color = 'blue', width = 10
                            ),
                            valueBox(value = actionLink(inputId = 'home', label = 'Home'),
                                     subtitle = '   .', icon = icon('home'), color = 'blue', width = 2)
                        )
    )),
    ##### Profile #####
    shinyjs::hidden(div(id = 'profile_page', 
                        box(title = 'Profile', status = 'success', solidHeader = TRUE, width = 12,
                            valueBoxOutput(outputId = 'name', width = 9),
                            column(width = 1, 
                                   hidden(div(id = 'edit_profile', 
                                              valueBox(value = actionLink(inputId = 'edit_link', label = 'Edit Profile', icon = icon('pencil')), 
                                                       subtitle = ' ', color = 'lime', width = 16)
                                   )),
                                   hidden(valueBoxOutput(outputId = 'level', width = 20))
                            ),
                            ##### Modal #####
                            bsModal(id = 'edit_modal', title = 'Edit your Profile', trigger = 'edit_profile', size = 'large', 
                                    fluidPage(
                                        column(width = 6, selectizeInput(inputId = 'designation', label = 'Please Choose Your Designation :', 
                                                                         choices = c('', 'Statistical Analyst', 'Senior Statistical Analyst', 
                                                                                     'Associate Data Scientist', 'Data Scientist', 'Senior Data Scientist',
                                                                                     'Manager', 'Senior Manager', 'Director',
                                                                                     'Project Analyst', 'Product Manager 1', 'Product Manager 2', 'Product Manager 3'), 
                                                                         selected = NULL, width = '98%')),
                                        column(width = 6, textInput(inputId = 'curr_team', label = 'Current Domain :', width = '98%')),
                                        column(width = 6, selectInput(inputId = 'change_manager', label = 'Change Manager :', width = '98%', 
                                                                      choices = c('None', python.get('[person[k]["Name"] for k in manager.keys()]')))),
                                        column(width = 6, textInput(inputId = 'brand_new_manager', label = 'In case your manager in not in the dropdown :', width = '98%', 
                                                                    placeholder = 'Enter his/her name here. probably your manager has not yet Signed Up.')),
                                        # column(width = 6, valueBox(value = actionLink(inputId = 'add_proj', label = 'Add Project(s)'), 
                                        #                            subtitle = 'that you have been part of.', icon = icon('plus'), color = 'blue', width = 12),
                                        #        uiOutput(outputId = 'new_proj')
                                        # ),
                                        # column(width = 6, valueBox(value = 
                                        actionButton(inputId = 'add_more_skill', label = 'Add Skills', icon = icon('plus')),
                                        # subtitle = 'to make your profile stronger', icon = icon('plus'), color = 'blue', width = 12),
                                        uiOutput(outputId = 'new_skill')
                                        # )
                                    ),
                                    br(),
                                    br(),
                                    wellPanel(actionButton(inputId = 'save', label = 'SAVE', icon = icon('save'))),
                                    tags$head(tags$style("#edit_modal .modal-footer{ display:none}"))
                            ),
                            ##### Other #####
                            column(width = 2,
                                   hidden(div(id = 'connect_with',
                                              valueBox(value = actionLink(inputId = 'connect_now', label = 'Connect'), 
                                                       subtitle = '', color = 'aqua', width = 12)
                                   )),
                                   hidden(div(id = 'connection_received',
                                              valueBox(value = actionLink(inputId = 'add_connect', label = 'ACCEPT'), 
                                                       subtitle = 'Connection Request', color = 'blue', width = 12)
                                   )),
                                   hidden(div(id = 'connection_sent',
                                              valueBox(value = '', 
                                                       subtitle = strong('Connection Request Sent'), color = 'blue', width = 12)
                                   ))
                            ),
                            valueBoxOutput(outputId = 'manager', width = 5),
                            valueBoxOutput(outputId = 'connections', width = 2),
                            valueBoxOutput(outputId = 'endorsement', width = 3),
                            hidden(valueBoxOutput(outputId = 'reportee', width = 2)),
                            column(width = 12, uiOutput(outputId = 'path')),
                            column(width = 12, withSpinner(uiOutput(outputId = 'skill_ui'), type = 6))
                        )
    )),
    ##### Connection #####
    shinyjs::hidden(div(id = 'connection_box',
                        box(title = 'Connections', solidHeader = TRUE, status = 'success', width = 12, collapsible = TRUE, collapsed = FALSE,
                            uiOutput(outputId = 'conns'),
                            hr(),
                            br(),
                            actionButton(inputId = 'back_to_host', label = h4(strong('Back to Profile Page')), width = '100%')
                        )
    )),
    ##### Directs #####
    shinyjs::hidden(div(id = 'directs_box',
                        box(title = 'Direct Reports', solidHeader = TRUE, status = 'success', width = 12, collapsible = TRUE, collapsed = FALSE,
                            uiOutput(outputId = 'DRs'),
                            hr(),
                            br(),
                            actionButton(inputId = 'back_to_manager', label = h4(strong('Back to Profile Page')), width = '100%')
                        )
    )),
    ##### Endorsement ####
    shinyjs::hidden(div(id = 'endorsement_box',
                        box(title = 'Endorsements Received', solidHeader = TRUE, status = 'success', width = 12, collapsible = TRUE, collapsed = FALSE,
                            uiOutput(outputId = 'end_recd')
                        ),
                        box(title = 'Endorsements Made', solidHeader = TRUE, status = 'success', width = 12, collapsible = TRUE, collapsed = FALSE,
                            uiOutput(outputId = 'end_given')
                        ),
                        hr(),
                        br(),
                        actionButton(inputId = 'back_to_host_p', label = h4(strong('Back to Profile Page')), width = '100%')
    )),
    ##### Search Results ####
    shinyjs::hidden(div(id = 'search_result_skill',
                        box(title = 'Search Result by Skill', solidHeader = TRUE, status = 'success', width = 12, 
                            uiOutput(outputId = 'search_result')
                        )
    ))
    #####
))

ui <- dashboardPage(header = header, sidebar = sidebar, body = body, skin = 'blue')