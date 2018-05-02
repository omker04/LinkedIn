other_profile_UI <- function(id) {
    ns <- NS(id)
    self_profile_UI(id = ns('new_id'))
}

other_profile_ <- function(input, output, session, id) {
    options(expressions = 100000)
    is_manager <- python.get(paste0(char(input$user_ID), ' in manager.keys()'))
    callModule(self_profile_('new_id', self = id, is_manager = is_manager))
}