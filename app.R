library(shiny)
library(feather)
library(sonicscrewdriver)

species <- c(
  "All",
  "Long-tailed Tit",
  "Eurasian Magpie",
  "Eurasian Wren",
  "Eurasian Jackdaw",
  "Dunnock",
  "Eurasian Blue Tit",
  "European Robin",
  "Common Chiffchaff",
  "Coal Tit",
  "Common Wood-Pigeon",
  "Eurasian Treecreeper",
  "Gray Wagtail",
  "Great Tit",
  "Goldcrest",
  "Eurasian Blackbird"
)

sites <- c(
  "Glasgow1",
  "Newcastle1",
  "Newcastle2"
)

ui <- fluidPage(
    titlePanel("UNP Bird Demo (Glasgow August-September 2022)"),

    sidebarLayout(
        sidebarPanel(
          tags$h4("Filter data"),
          tags$p("Dragging the date slider changes the day plotted, and the play button automatically advances through the days.
                 The Bird species drop-down allows data for a single species to be displayed."),
          selectInput("site",
                      "Site:",
                      sites),
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

server <- function(input, output, session) {
  #Read temperature data
  t <- reactive(read_feather(paste0(input$site,"T.feather")))

  observe({
    val <- input$site
    # Control the value, min, max, and step.
    # Step size is 2 when input value is even; 1 when value is odd.
    updateSliderInput(session, "date",
                      min = min(as.Date(t()$datetime)), max = max(as.Date(t()$datetime)))
  })




    output$dielPlot <- renderPlot({
      #Read BirdNet data
      b <- read_feather(paste0(input$site,".feather"))
      bb <- b[which(b$Confidence > 0.8),]
      bb <- bb[which(bb$Start < as.POSIXct(input$date)+ 86400 & bb$Start > as.POSIXct(input$date)),]
      if (input$species != "All") {
        bb <- bb[bb$Common.Name==input$species,]
      }
      dielPlot(input$date, 55.868581, -4.290506, legend=T)
      if (nrow(bb) > 0) {
        dielHistogram(bb$Start, by="15minute", col="blue", presence.only=T)
      }
    })
    output$temperature <- renderPlot({
      tt <- t()[which(t()$datetime < as.POSIXct(input$date)+ 86400 & t()$datetime > as.POSIXct(input$date)),]
      if (nrow(tt) > 0) {
        plot(
          tt$datetime,
          tt$temperature,
          type="l",
          xlab="Time",
          ylab="Temperature (°C)",
          xlim=c(as.POSIXct(input$date), as.POSIXct(input$date)+ 86400),
          xaxt="n"
        )
        axis(
          1,
          at = as.POSIXct(input$date) + seq(0, 86400, by = 60*60*3),
          labels = c("0000", "0300","0600", "0900", "1200", "1500", "1800", "2100", "2400")
        )
      }
    })
}

# Run the application
shinyApp(ui = ui, server = server)
