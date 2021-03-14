library(shiny)
library(jsonlite)
library(lubridate)
library(httr)
library(dplyr)
library(usa)
library(tidyverse)
library(stringr)

readRenviron(".Renviron")
YELP_API=Sys.getenv("YELP_API")

get_name <- function(loc, sortinfo) {
    if (!is.null(loc) && (loc %in% city.name | loc %in% zip.code)) {
        r <- GET(
            "https://api.yelp.com/v3/businesses/search",
            add_headers(Authorization = paste("Bearer", YELP_API)),
            query = list(
                location = loc,
                sort_by = sortinfo
            )
        )
        stop_for_status(r)
        json <- content(r, as = "text")
        info <- fromJSON(json, flatten = TRUE)$businesses %>% select(name)
    } else {
        info <- NULL
    }
    
    
    return(info)
}

get_name_rating <- function(loc, sortinfo) {
    if (!is.null(loc) && (loc %in% city.name | loc %in% zip.code)) {
        r <- GET(
            "https://api.yelp.com/v3/businesses/search",
            add_headers(Authorization = paste("Bearer", YELP_API)),
            query = list(
                location = loc,
                sort_by = sortinfo
            )
        )
        stop_for_status(r)
        json <- content(r, as = "text")
        info <- fromJSON(json, flatten = TRUE)$businesses %>% select(name, rating)
        colnames(info) <- c("Name", "Rating")
    } else {
        info <- NULL
    }
    
    
    return(info)
}

get_count <- function(loc, sortinfo) {
    if (!is.null(loc) && (loc %in% city.name | loc %in% zip.code)) {
        r <- GET(
            "https://api.yelp.com/v3/businesses/search",
            add_headers(Authorization = paste("Bearer", YELP_API)),
            query = list(
                location = loc,
                sort_by = sortinfo
            )
        )
        stop_for_status(r)
        json <- content(r, as = "text")
        info <- fromJSON(json, flatten = TRUE)$businesses %>% select(review_count)
        info <- info$review_count
    } else {
        info <- NULL
    }
    
    
    return(info)
}

get_id <- function(loc, sortinfo, resname) {
    r <- GET(
        "https://api.yelp.com/v3/businesses/search",
        add_headers(Authorization = paste("Bearer", YELP_API)),
        query = list(
            location = loc,
            sort_by = sortinfo
        )
    )
    stop_for_status(r)
    json <- content(r, as = "text")
    id <- fromJSON(json, flatten = TRUE)$businesses %>%
        select(id, name) %>%
        filter(name == resname) %>%
        select(id)
    id <- as.character(id)
    return(id)
}

get_hours <- function(id) {
    r <- GET(
        str_glue("https://api.yelp.com/v3/businesses/{id}", id = id),
        add_headers(Authorization = paste("Bearer", YELP_API)),
        query = list()
    )
    stop_for_status(r)
    json <- content(r, as = "text")
    hours <- fromJSON(json, flatten = TRUE)$hours$open
    hours <- data.frame(hours)
    hours$day <- recode(hours$day,
                        "6" = "Sunday",
                        "0" = "Monday",
                        "1" = "Tuesday",
                        "2" = "Wednesday",
                        "3" = "Thursday",
                        "4" = "Friday",
                        "5" = "Saturday"
    )
    hours$start <- sub("(\\d{2})$", ":\\1", hours$start)
    hours$end <- sub("(\\d{2})$", ":\\1", hours$end)
    colnames(hours) <- c("Open overnight", "Open", "Close", "Day")
    return(hours)
}

get_photo <- function(id) {
    r <- GET(
        str_glue("https://api.yelp.com/v3/businesses/{id}", id = id),
        add_headers(Authorization = paste("Bearer", YELP_API)),
        query = list()
    )
    stop_for_status(r)
    json <- content(r, as = "text")
    photo <- fromJSON(json, flatten = TRUE)$photos
    photo <- data.frame(photo)
    colnames(photo) <- c("Photo link")
    return(photo)
}

get_contact <- function(id) {
    r <- GET(
        str_glue("https://api.yelp.com/v3/businesses/{id}", id = id),
        add_headers(Authorization = paste("Bearer", YELP_API)),
        query = list()
    )
    stop_for_status(r)
    json <- content(r, as = "text")
    phone <- fromJSON(json, flatten = TRUE)$phone
    location <- toString(fromJSON(json, flatten = TRUE)$location$display_address)
    contact <- data.frame("Phone number" = phone, "Address" = location)
    return(contact)
}

get_review <- function(id) {
    r <- GET(
        str_glue("https://api.yelp.com/v3/businesses/{id}/reviews", id = id),
        add_headers(Authorization = paste("Bearer", YELP_API)),
        query = list()
    )
    stop_for_status(r)
    json <- content(r, as = "text")
    fromJSON(json, flatten = TRUE)$reviews %>% select(text, rating)
}

ui <- fluidPage(
    titlePanel("Find Nearby Restaurant"),
    sidebarLayout(
        sidebarPanel(
            textInput("location", "Enter your location (City Name or Zipcode)", "Davis"),
            selectInput("sort", "Sort Restaurant by", c("best_match", "rating", "review_count"),
                        selected = "best_match"
            ),
            selectInput("restaurant", "Choose a Restaurant Name for further information", c("-")),
            width = 3
        ),
        mainPanel(
            tabsetPanel(
                tabPanel(
                    "Nearby Restaurant Name",
                    column(tableOutput("restaurant_name"), width = 5),
                    column(plotOutput("reviewcount"), width = 7)
                ),
                tabPanel(
                    "Restaurant Info",
                    textOutput("infotitle"),
                    tableOutput("photos"),
                    textOutput("contacttitle"),
                    tableOutput("contact"),
                    textOutput("hourtitle"),
                    tableOutput("hours")
                ),
                tabPanel(
                    "Restaurant Review",
                    textOutput("reviewtitle"),
                    tableOutput("review")
                )
            ),
            width = 9
        )
    ),
    tags$head(tags$style(
        "#infotitle{
                                 font-size: 15px;
                                 color: grey;
                                 
                                 }",
        "#contacttitle{
                                 font-size: 15px;
                                 color: grey;
                                 
                                 }",
        "#hourtitle{
                                 font-size: 15px;
                                 color: grey;
                                 
                                 }",
        "#reviewtitle{
                                 font-size: 15px;
                                 color: grey;
                                 
                                 }"
    ))
)

server <- function(input, output, session) {
    observe({
        location <- str_to_title(input$location)
        sort <- input$sort
        if (!is.null(input$location)) {
            restaurant <- get_name(location, sort)
            updateSelectInput(session, "restaurant", choices = c("-", restaurant))
        }
    })
    output$restaurant_name <- renderTable({
        req(!is.null(input$location))
        req(str_to_title(input$location) %in% city.name | input$location %in% zip.code)
        get_name_rating(str_to_title(input$location), input$sort)
    })
    output$reviewcount <- renderPlot({
        req(!is.null(input$location))
        req(str_to_title(input$location) %in% city.name | input$location %in% zip.code)
        barplot(get_count(str_to_title(input$location), input$sort),
                xlab = "Restuarant listed on the left accordingly",
                ylab = "Review Count",
                main = "Review Count of Restuarants shown on the left"
        )
    })
    output$infotitle <- renderText({
        res <- input$restaurant
        if (res != "-") {
            paste0("Check to see photos of ", res)
        } else {
            "Choose a restaurant to see its information"
        }
    })
    output$photos <- renderTable({
        res <- input$restaurant
        if (res != "-") {
            get_photo(get_id(str_to_title(input$location), input$sort, res))
        }
    })
    output$contacttitle <- renderText({
        res <- input$restaurant
        if (res != "-") {
            paste0("Contact info for ", res)
        }
    })
    output$contact <- renderTable({
        res <- input$restaurant
        if (res != "-") {
            get_contact(get_id(str_to_title(input$location), input$sort, res))
        }
    })
    output$hourtitle <- renderText({
        res <- input$restaurant
        if (res != "-") {
            paste0("Open hours for ", res)
        }
    })
    output$hours <- renderTable({
        res <- input$restaurant
        if (res != "-") {
            get_hours(get_id(str_to_title(input$location), input$sort, res))
        }
    })
    output$reviewtitle <- renderText({
        res <- input$restaurant
        if (res != "-") {
            paste0("Reviews for ", res)
        } else {
            "Choose a restaurant to see its rating"
        }
    })
    output$review <- renderTable({
        res <- input$restaurant
        req(res != "-")
        get_review(get_id(str_to_title(input$location), input$sort, res))
    })
}

shinyApp(ui, server)
