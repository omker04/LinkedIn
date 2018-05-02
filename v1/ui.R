library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyBS)
library(shinydashboard)
library(htmltools)
library(rPython)
library(data.table)
library(dplyr)
library(wordcloud)

python.load('functions.py', get.exception = TRUE)


source('self_profile_module.R')
#source('manager_profile_module.R')
#source('associate_names_module.R')
source('other_profile_module.R')

header <- dashboardHeader(title = 'LinkedIn for Walmart Analytics', dropdownMenuOutput('notification'))
sidebar <- dashboardSidebar(disable = TRUE)
body <- dashboardBody(fluidPage(
    shinyjs::useShinyjs(),
    ##### Login Page ####
    div(id = 'login_page', column(width = 6, offset = 3, 
           box(title = 'Log-In or Sign-Up', status = 'primary', solidHeader = TRUE, width = 12, 
               textInput(inputId = 'user_name', label = 'Please Enter your Full Name : *', value = NULL, 
                         placeholder = 'Full Name as per Walmart Records', width = '100%'),
               textInput(inputId = 'user_ID', label = 'Please Enter your 7 diigt GTS-ID without any space : *', value = NULL,
                         placeholder = 'GTS-ID without any space (eg : GTS0000)', width = '100%'),
               checkboxInput(inputId = 'agree', label = 'I have READ and UNDERSTOOD the Disclaimers.'), 
               disabled(actionButton(inputId = 'login', label = 'Log-In', width = '49.5%')),
               disabled(actionButton(inputId = 'signup', label = 'Sign-Up', width = '49.5%')),
               bsAlert(anchorId = 'warning')
           )
    )
    ),
    shinyjs::hidden(div(id = 'search_bar', 
    ##### Search Box ####
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
                                actionButton(inputId = 'search', label = 'Search', icon = icon('search'))
                         )
                     ),
                 value = '', icon = icon('search'), color = 'blue', width = 12
        )
    )
    )),

    # ##### Profile #####
    # box(title = textOutput(outputId = 'profile_name'), status = 'success', solidHeader = TRUE, width = 12,
    #     valueBoxOutput(outputId = 'name', width = 12),
    #     valueBoxOutput(outputId = 'manager', width = 6),
    #     valueBoxOutput(outputId = 'connections', width = 2),
    #     valueBoxOutput(outputId = 'endorsement', width = 3),
    #     box(title = 'Projects', solidHeader = TRUE, status = 'warning', width = 12, collapsible = TRUE, collapsed = FALSE,
    #         uiOutput('current_project_details'),
    #         tags$head(
    #             tags$style(HTML("
    #                             #add_past_project {
    #                             display:block;
    #                             height: 60px;
    #                             width: 60px;
    #                             border-radius: 100%;
    #                             border: 2px solid red;
    #                             }
    # 
    #                             "))
    #         ),
    #         column(width = 1, actionButton(inputId = 'add_past_project', label = '', icon = icon('plus'))),
    #         column(width = 8, h3(strong('Past Project Details -----------------------------------------'))),
    #         uiOutput('past_project_details')
    #     ),
    #     box(title = 'Skills', solidHeader = TRUE, status = 'warning', width = 12, collapsible = TRUE, collapsed = FALSE,
    #         column(width = 10, plotOutput(outputId = 'skill_cloud')),
    #         column(width = 2, tags$head(
    #             tags$style(HTML("
    #                             #add_skill {
    #                             display:block;
    #                             height: 60px;
    #                             width: 60px;
    #                             border-radius: 100%;
    #                             border: 2px solid red;
    #                             }
    #                             "))
    #             ),
    #             actionButton(inputId = 'add_skill', label = 'skill', icon = icon('plus')))
    #         )
    # )
    # #######
    shinyjs::hidden(div(id = 'self_profile', self_profile_UI(id = 'self_page')))
    # shinyjs::hidden(div(id = 'others_profile', other_profile_UI(id = 'others'))),
    # shinyjs::hidden(div(id = 'managers_profile', manager_profile_UI(id = 'manager'))),
    # shinyjs::hidden(div(id = 'connection_profile', associate_names_UI(id = 'Conn', box_title = 'Connections'))),
    # shinyjs::hidden(div(id = 'direct_report_profile', associate_names_UI(id = 'DR', box_title = 'Direct Reports')))
)
)

ui <- dashboardPage(header = header, sidebar = sidebar, body = body)