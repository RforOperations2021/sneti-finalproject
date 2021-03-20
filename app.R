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
library(maptools)
library(rgeos)

# Load and clean data

protests <- read.csv("protests_cleaned.csv")
protests <- as.data.frame(protests)
protests$Mnth_Yr <- as.factor(format(as.Date(protests$fixeddate), "%B %Y"))
protests$Mnth_Yr <- factor(protests$Mnth_Yr, c("January 2020", "February 2020", "March 2020", "April 2020", "May 2020", "June 2020", "July 2020", "August 2020", "September 2020", "October 2020", "November 2020", "December 2020", "January 2021", "February 2021"))

#load in states shapefile and create centroids
states <- readShapePoly("tl_2020_us_state")
states@data <- cbind(states@data, rgeos::gCentroid(states, byid = TRUE)@coords)

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
                    selected = "Pennsylvania"),
        
        dateRangeInput(input = "daterange",
                       label = "Choose the time frame you'd like to view data for:",
                       start = min(protests$fixeddate),
                       end = max(protests$fixeddate),
                       min = min(protests$fixeddate),
                       max = max(protests$fixeddate)),
        
        downloadButton("download", "Download selected data")
    )
)


body <- dashboardBody(tabItems(
    
    tabItem("plots", 
            
            fluidRow(
                infoBoxOutput("datasrc")
            ),
            
            fluidRow(
                tabBox(title = "Map",
                       width = 12,
                       tabPanel("Map of Selected Events", leafletOutput("mapPlot")))
            ),
            
            fluidRow(
                tabBox(title = "Plots",
                       width = 12,
                       tabPanel("Distribution over Time", plotlyOutput("distPlot")),
                       tabPanel("Distribution by Main Actors", plotlyOutput("actorPlot")))
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
    
    #create subset of dataset based on all user input
    protestssubset <- reactive({
        protestssubset <- protests %>%
            filter(event_type %in% input$eventtype, admin1 == input$state, as.Date(fixeddate) >= input$daterange[1], as.Date(fixeddate) <= input$daterange[2])
        protestssubset
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
    
    #bar chart showing subtype by event type 
    output$actorPlot <- renderPlotly({
        ggplotly(ggplot(data=byactor(), aes(x=actor1, y=count)) +
                     geom_bar(aes(fill = event_type), stat='identity') + 
                     scale_fill_brewer(palette="Set2") +
                     theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)))
    })
    
    #base leaflet map, centered on pa's centroid 
    output$mapPlot <- renderLeaflet({
        leaflet() %>%
            addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = "Google", group = "Google") %>%
            addProviderTiles(provider = providers$Wikimedia, group = "Wiki") %>%
            setView(-77.194527, 41.203323, zoom = 6) %>%
            addLayersControl(baseGroups = c("Google", "Wiki"))
    })
    
    #subsetting state shapefile to plot 
    statesubset <- reactive({
        statesubset <- subset(states, states$NAME == input$state)
        statesubset
    })
    
    #observe function for changes in user input and add polygon
    observe({
        statessub <- statesubset()
        
        leafletProxy("mapPlot", data = statessub) %>%
            clearGroup(group = "statessub") %>%
            addPolygons(popup = ~paste0("<b>", NAME, "</b>"), group = "statessub", layerId = ~GEOID, fill = TRUE, color = "white") %>%
            setView(lng = statessub$x[1], lat = statessub$y[1], zoom = 6)
    })
    
    #observe function for changes in user input to add markers for protests 
    observe({
        protestssub <- protestssubset()
        
        pal <- colorFactor(palette="Set2", domain = unique(protests$event_type))
        
        leafletProxy("mapPlot", data = protestssub) %>%
            # In this case either lines 92 or 93 will work
            # clearMarkers() %>%
            clearGroup(group = "protestssub") %>%
            addCircleMarkers(popup = ~paste0("<b>", event_type, "</b></br> Actors: ", actor1, ", ", assoc_actor_1, "</br> Description: ", notes), 
                             group = "protestssub", layerId = ~data_id, color = ~pal(event_type), radius = 2)
    })
    
    #output datatable based on subset of data
    output$table <- DT::renderDataTable(
        DT::datatable(data = protestssubset())
    )
    
    #download handler 
    output$download <- downloadHandler(
        filename = function() {
            paste("protestsdata", ".csv", sep = "")
        },
        content = function(file) {
            write.csv(protestssubset(), file, row.names = FALSE)
        }
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)