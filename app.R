library(shiny)
library(feather)
library(sonicscrewdriver)

t <- read_feather("glasgow.feather")
t <- t[which(t$Confidence > 0.8),]

species <- unique(t$Common.Name)

ui <- fluidPage(

    # Application title
    titlePanel("UNP Bird Demo (Glasgow August-September 2022)"),

    sidebarLayout(
        sidebarPanel(
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
                      "Species:",
                      species,
                      selected = "Eurasian Wren",
                      multiple = FALSE,
                      selectize = TRUE,
                      width = NULL,
                      size = NULL
          )
        ),

        mainPanel(
           plotOutput("dielPlot")
        )
    )
)

server <- function(input, output) {
    output$dielPlot <- renderPlot({
      tt <- t[which(t$Start < as.POSIXct(input$date)+ 86400 & t$Start > as.POSIXct(input$date)),]
      tt <- tt[tt$Common.Name==input$species,]
      dielPlot(input$date, 55.868581, -4.290506)
      if (nrow(tt) > 0) {
        dielHistogram(tt$Start, by="15minute", col="blue")
      }
    })
}

# Run the application
shinyApp(ui = ui, server = server)
