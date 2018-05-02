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
library(magrittr)
library(rPython)
library(shinycssloaders)

char <- function(x) {
    y <- paste0('"', x, '"')
    return(y)
}

python.load('few_new_functions.py', get.exception = TRUE)

dropdownMenuCustom <- function (..., type = c("messages", "notifications", "tasks"), 
                                    badgeStatus = "danger", icon = NULL, .list = NULL, header, text, n = 0) {
    type <- match.arg(type)
    if (!is.null(badgeStatus)) shinydashboard:::validateStatus(badgeStatus)
    items <- c(list(...), .list)
    lapply(items, shinydashboard:::tagAssert, type = "li")
    dropdownClass <- paste0("dropdown ", type, "-menu")
    if (is.null(icon)) {
        icon <- switch(type, messages = shiny::icon("envelope"), 
                       notifications = shiny::icon("bell", 'fa-2x'), tasks = shiny::icon("tasks"))
    }
    numItems <- length(items)
    if (max(0, numItems - n) == 0 | is.null(badgeStatus)) {
        badge <- NULL
    } else {
        badge <- span(class = paste0("label label-", badgeStatus), 
                      max(0, numItems - n))
    }
    tags$li(
        class = dropdownClass, 
        a(
            href = "#", 
            class = "dropdown-toggle", 
            `data-toggle` = "dropdown", 
            icon, 
            badge
        ), 
        tags$ul(
            class = "dropdown-menu", 
            if (header) {
                tags$li(
                    class = "header", 
                    paste('You have', max(0, numItems - n), text)
                )
            },
            tags$li(
                tags$ul(class = "menu", items)
            )
        )
    )
}

