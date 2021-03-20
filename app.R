library(shiny)
library(shinydashboard)
library(dplyr)
library(plotly)
library(shinythemes)
library(ggplot2)
library(DT)
library(stringr)
library(tools)
library(maps) 

# Load and clean data

protests <- read.csv("protests_cleaned.csv")
protests <- as.data.frame(protests)
protests$Mnth_Yr <- as.factor(format(as.Date(protests$fixeddate), "%B %Y"))
protests$Mnth_Yr <- factor(protests$Mnth_Yr, c("January 2020", "February 2020", "March 2020", "April 2020", "May 2020", "June 2020", "July 2020", "August 2020", "September 2020", "October 2020", "November 2020", "December 2020", "January 2021", "February 2021"))

mainstates <- map_data("state")

protestsmainstates <- filter(protests, admin1 != "Alaska", admin1 != "Hawaii")

# Create dashboard header, sidebar, and main body

header <- dashboardHeader(title = "How Protests Spread Across 2020")

sidebar <- dashboardSidebar(
    sidebarMenu(
        id = "tabs",
        
        menuItem("Plots", icon=icon("bar-chart"), tabName = "plots"),
        menuItem("Tables", icon=icon("table"), tabName = "tables"),
        
        selectInput(input = "eventtype",
                    label = "Event Type:",
                    choices = unique(protests$event_type),
                    selected = "Protests",
                    multiple = T),
        
        selectInput(input = "state",
                    label = "Choose which states to show:",
                    choices = unique(protests$admin1),
                    selected = "Pennsylvania",
                    multiple = T),
        
        dateRangeInput(input = "daterange",
                       label = "Choose the time frame you'd like to view data for:",
                       start = min(protests$fixeddate),
                       end = max(protests$fixeddate),
                       min = min(protests$fixeddate),
                       max = max(protests$fixeddate))
    )
)


body <- dashboardBody(tabItems(
    
    tabItem("plots", 
            
            fluidRow(
                infoBoxOutput("datasrc")
            ),
            
            fluidRow(
                valueBoxOutput("totnumevents"),
                valueBoxOutput("numstates"),
                valueBoxOutput("numactors")
            ),
            
            fluidRow(
                tabBox(title = "Plots",
                       width = 12,
                       tabPanel("Distribution over Time", plotlyOutput("distPlot")),
                       tabPanel("Distribution by State", plotlyOutput("statePlot")),
                       tabPanel("Distribution by Main Actors", plotlyOutput("actorPlot")))
            ),
            
            fluidRow(
                tabBox(title = "Map",
                       width = 12,
                       tabPanel("Map of Selected Events", leafletOutput("mapPlot")))
            )
    ),
    
    tabItem("tables", 
            
            fluidPage(
                box(title = "Data on Selected Events", DT::dataTableOutput("table"), width = 12))
    )
)
)

ui <- dashboardPage(header, sidebar, body)

server <- function(input, output) {
    
    # Data cleaning steps 
    
    #create subset of dataset based on all user input
    protestssubset <- reactive({
        protestssubset <- protests %>%
            filter(event_type %in% input$eventtype, admin1 %in% input$state, as.Date(fixeddate) >= input$daterange[1], as.Date(fixeddate) <= input$daterange[2])
        protestssubset
    })
    
    protestsmainstatessubset <- reactive({
        protestsmainstates <- protestsmainstates %>%
            filter(event_type %in% input$eventtype, admin1 %in% input$state, as.Date(fixeddate) >= input$daterange[1], as.Date(fixeddate) <= input$daterange[2])
        protestsmainstates
    })
    
    #create summary table grouped by state and event type
    bystate <-reactive({
        bystate <- protestssubset() %>%
            group_by(admin1, event_type) %>%
            summarise(count=n(), percent_of_total = (n()/nrow(protestssubset()))*100, deaths = sum(fatalities))
        bystate
    })
    
    #grouped by month/year and event type
    bydate <- reactive({
        bydate <- protestssubset() %>%
            group_by(Mnth_Yr, event_type) %>%
            summarise(count=n(), percent_of_total = (n()/nrow(protestssubset()))*100, deaths = sum(fatalities))
        bydate
    })
    
    #grouped by event type and main actors 
    byactor <- reactive({
        byactor <- protestssubset() %>%
            group_by(event_type, actor1) %>%
            summarise(count=n(), percent_of_total = (n()/nrow(protestssubset()))*100, deaths = sum(fatalities))
        byactor
    })
    
    # Creating value boxes
    
    output$totnumevents <- renderValueBox({
        num <- nrow(protestssubset())
        valueBox(subtitle = "Total number of Events", value = num, icon = icon("sort-numeric-asc"), color = "green")
    })
    
    output$numstates <- renderValueBox({
        num <- length(input$state)
        valueBox(subtitle = "Total number of States", value = num, icon("sort-numeric-asc"), color = "blue")
    })
    
    output$numactors <- renderValueBox({
        num <- length(unique(byactor()$actor1))
        valueBox(subtitle = "Total number of Actors", value = num, icon("sort-numeric-asc"), color = "red")
    })
    
    #Creating Info box
    
    output$datasrc <- renderInfoBox({
        infoBox("Data Source", subtitle = "Data Source: ACLED US Crisis Monitor", color = "purple")
    })
    
    # Creating outputs 
    
    output$distPlot <- renderPlotly({
        ggplotly(ggplot(data = bydate(), aes(x=Mnth_Yr, y=count)) + 
                     geom_line(aes(color = event_type, group = event_type), size = 1.25) +
                     geom_point(aes(color = event_type), size = 2) +
                     scale_color_brewer(palette="Set2") +
                     theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)))
    })
    
    #bar chart showing number of events for each state, colored by events
    output$statePlot <- renderPlotly({
        ggplotly(ggplot(data = bystate(), aes(x=count, y=admin1)) + 
                     geom_bar(aes(fill = event_type), stat = 'identity') +
                     scale_fill_brewer(palette="Set2"))
    })
    
    #bar chart showing subtype by event type 
    output$actorPlot <- renderPlotly({
        ggplotly(ggplot(data=byactor(), aes(x=actor1, y=count)) +
                     geom_bar(aes(fill = event_type), stat='identity') + 
                     scale_fill_brewer(palette="Set2") +
                     theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)))
    })
    
    output$mapPlot <- renderLeaflet({
        leaflet() %>%
            addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = "Google", group = "Google") %>%
            addProviderTiles(provider = providers$Wikimedia, group = "Wiki") %>%
            setView(-79.931355, 40.449504, 9) %>%
            addLayersControl(baseGroups = c("Google", "Wiki"))
    })
    
    #output datatable based on subset of data
    output$table <- DT::renderDataTable(
        DT::datatable(data = protestssubset())
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
