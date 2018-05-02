associate_names_UI <- function(id, box_title) {
    ns <- NS(id)
    fluidPage(
        div(id = 'box',
            box(title = box_title, solidHeader = TRUE, status = 'success', width = 12, collapsible = TRUE, collapsed = FALSE,
                uiOutput(outputId = ns('DRs'))
            )
        )
        #self_profile_UI(ns('inner_self'))
    )
}

associate_names_ <- function(input, output, session, type, Uid) {
    associates <- reactive({
        # print(type)
        # print(Uid)
        if (type == 'connections') {
            associate <- python.get(paste0('connection[', char(Uid), ']'))
        } else {
            if (type == 'DR' & Uid %in% python.get('manager.keys()')) {
                associate <- python.get(paste0('manager[', char(Uid), ']'))
                associate <- associate[associate != Uid]
            } else {
                shinyjs::hide(id = 'box')
            }
        }
        python.assign('associate', associate)
        associate_name <- python.get('[person[k]["Name"] for k in associate]')
        
        return(list(associate, associate_name))
    })
    
    obsList <- list()
    clicked <- reactiveValues(id = NULL)
    
    output$DRs <- renderUI({
        ns <- session$ns
        nbr <- length(associates()[[2]])
        dr_id <- associates()[[1]]
        dr <- data.frame('DR' = associates()[[2]]) %>% mutate_all(funs(as.character))
        
        fluidRow(
            output_list <- lapply(1:nbr, function(i) {
                name = paste0("name_", i)
                link_name <- paste0(type, Uid, i)
                
                if (is.null(obsList[[link_name]])) {
                    obsList[[link_name]] <<- observeEvent(input[[link_name]], {
                        clicked$id <- dr_id[i]
                    })
                }
                column(width = 3, 
                       valueBox(value = actionLink(inputId = ns(link_name), label = substr(dr$DR[i], 1, 1)), 
                                subtitle = dr$DR[i], color = 'yellow', width = 12))
            })
        )
        do.call(tagList, output_list)
    })
    
    new_id <- reactive({
        return(clicked$id)
    })
    
    # s <- callModule(self_profile_, id = 'inner_self', self = new_id())
    
    return(new_id)
}
