library(shinydashboard)
library(leaflet)

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()


ui <- dashboardPage(
  
  dashboardHeader(title = "Info boxes", disable = TRUE),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    fluidRow(
      infoBox("TOTAL PROFIT", 10 * 2,width = 6, icon = icon("credit-card")),
      infoBox("New Orders", 35 * 3,width = 6,color = "purple", icon = icon("plus")),
      infoBoxOutput("approvalBox",width = 6),
      infoBoxOutput("progressBox",width = 6)
    ),
   
    fluidRow(
      box(plotOutput("plot1", height = 250)),
      leafletOutput("mymap", width = 435, height = 250),
      p(),
      actionButton("recalc", "New points"),
      box(
        title = "Controls",
        sliderInput("slider", "Number of observations:", 1, 100, 50)
      )
    ),

    
    
  )
)

server <- function(input, output) {


  output$progressBox <- renderInfoBox({
    infoBox(
      "OPEN COMPLAINTS", "359", icon = icon("thumbs-down", lib = "glyphicon"),
      color = "red"
    )
  })
  output$approvalBox <- renderInfoBox({
    infoBox(
      "NEW USERS", "756", icon = icon("user", lib = "glyphicon"),
      color = "yellow"
    )
  })
  
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  
  points <- eventReactive(input$recalc, {
    cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
  }, ignoreNULL = FALSE)
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addMarkers(data = points())
  })
 
}

shinyApp(ui, server)

