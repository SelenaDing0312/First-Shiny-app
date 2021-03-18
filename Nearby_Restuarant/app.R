library(shiny)
library(jsonlite)
library(lubridate)
library(httr)
library(dplyr)
library(usa)
library(tidyverse)
library(stringr)
library(ggplot2)

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

get_price <- function(loc,sortinfo){
    `%!in%` = Negate(`%in%`)
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
        info <- (fromJSON(json, flatten = TRUE)$businesses %>% select(price))
        info$pricelevel<-nchar(info$price)
        level<-info%>%group_by(pricelevel)%>%count()
        for (i in 1:4){
            if (i %!in% level$pricelevel){
                pricelevel=c(level$pricelevel,i)
                n = c(level$n,0)
                level <- data.frame("pricelevel"=pricelevel,"n"=n)
                level<-level%>%arrange(pricelevel)
            }
        }
        level$pricelevel=c("Low Price","Median-Low Price","Median-High Price","High Price")
        
        info<-level
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
    colnames(photo) <- c("photos")
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
    fromJSON(json, flatten = TRUE)$reviews %>% select(time_created,text,rating)
}

get_rating <- function(id) {
    r <- GET(
        str_glue("https://api.yelp.com/v3/businesses/{id}", id = id),
        add_headers(Authorization = paste("Bearer", YELP_API)),
        query = list()
    )
    stop_for_status(r)
    json <- content(r, as = "text")
    fromJSON(json, flatten = TRUE)$rating
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
                    column(plotOutput("reviewcount"), plotOutput("price"),width = 7),
                    
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
                    tableOutput("review"),
                    textOutput("ratingtitle"),
                    plotOutput("rating")
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
                col=rgb(0.8,0.1,0.1,0.6),
                xlab = "Restaurant listed on the left accordingly",
                ylab = "Review Count",
                main = "Review Count of Restaurant shown on the left"
        )
    })
    output$price <- renderPlot({
        req(!is.null(input$location))
        req(str_to_title(input$location) %in% city.name | input$location %in% zip.code)
        pricelevel=get_price(str_to_title(input$location), input$sort)%>%select(pricelevel)
        ggplot(get_price(str_to_title(input$location), input$sort),aes(x="",y=n,fill=pricelevel))+
            geom_bar(width = 1, stat = "identity") +
            coord_polar("y", start=0) +
            ggtitle("Pir chart for Price Level of Restaurant shown on the left")+
            ylab("Number of Restaurant")+
            theme(plot.title = element_text(face="bold",hjust = 0.8))
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
    output$ratingtitle <- renderText({
        res <- input$restaurant
        if (res != "-") {
            paste0("Ratings for ", res)
        }
    })
    output$rating <- renderPlot({
        res <- input$restaurant
        req(res != "-")
        t=get_review(get_id(str_to_title(input$location), input$sort, res))
        plot.ts(t$rating,ylim=c(1,5),ylab="Ratings",col="blue")
        abline(h=get_rating(get_id(str_to_title(input$location), input$sort, res)),col="red",lty=2)
        legend("right",c("Recent Rating for the restaurant","Avergae Rating for the restaurant"),lty=c(1,2),col=c("blue","red"))
    })
    
}

shinyApp(ui, server)
