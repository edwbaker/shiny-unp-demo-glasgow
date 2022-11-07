library(shiny)
library(feather)
library(sonicscrewdriver)

#Read BirdNet data
b <- read_feather("glasgow.feather")
b <- b[which(b$Confidence > 0.8),]

#Read temperature data
t <- read_feather("glasgowT.feather")

species <- c("All", unique(b$Common.Name))

ui <- fluidPage(
    titlePanel("UNP Bird Demo (Glasgow August-September 2022)"),

    sidebarLayout(
        sidebarPanel(
          tags$h4("Filter data"),
          tags$p("Dragging the date slider changes the day plotted, and the play button automatically advances through the days.
                 The Bird species drop-down allows data for a single species to be displayed."),
          sliderInput("date",
                      "Date:",
                      min = as.Date("2022-08-22","%Y-%m-%d"),
                      max = as.Date("2022-09-02","%Y-%m-%d"),
                      value=as.Date("2022-08-22","%Y-%m-%d"),
                      timeFormat="%Y-%m-%d",
                      step = 1,
                      animate = animationOptions(interval = 1000,loop=TRUE)
          ),
          selectInput("species",
                      "Bird species:",
                      species,
                      selected = "All",
                      multiple = FALSE,
                      selectize = TRUE,
                      width = NULL,
                      size = NULL
          )
        ),

        mainPanel(
          tags$h4("Automated bird identifications"),
          tags$p("Bird vocalisations have been identified using the BirdNet Analyser. The visualisation shows
                 the presence/absence of bird vocalisation on a 15-minute basis (blue bars). The base plot
                 shows periods of night, twilight, sunrise/sunset, and the sun's height above the horizon."),
          plotOutput("dielPlot"),
          tags$h4("Temperature"),
          tags$p("Audiomoth devices have an inbuilt temperature sensor that records the ambient temperature
                 at the start of each audio recording."),
          plotOutput("temperature")
        )
    )
)

server <- function(input, output) {
    output$dielPlot <- renderPlot({
      bb <- b[which(b$Start < as.POSIXct(input$date)+ 86400 & b$Start > as.POSIXct(input$date)),]
      if (input$species != "All") {
        bb <- bb[bb$Common.Name==input$species,]
      }
      dielPlot(input$date, 55.868581, -4.290506,legend=T)
      if (nrow(bb) > 0) {
        dielHistogram(bb$Start, by="15minute", col="blue", presence.only=T)
      }
    })
    output$temperature <- renderPlot({
      tt <- t[which(t$datetime < as.POSIXct(input$date)+ 86400 & t$datetime > as.POSIXct(input$date)),]
      plot(
        tt$datetime,
        tt$temperature,
        type="l",
        xlab="Time",
        ylab="Temperature (°C)"
      )
    })
}

# Run the application
shinyApp(ui = ui, server = server)
