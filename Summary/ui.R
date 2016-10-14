shinyUI(fluidPage(
  titlePanel("ScatterPlotVis"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Creat Scatter plot from the Tonnage Data"),
      
      selectInput("var", 
                  label = "Choose Y axis",
                  choices = c("TOT_CAR_EAST", "TOT_CAR_WEST",
                              "TOT_TRN_EAST", "TOT_TRN_WEST","total_train"),
                  selected = "TOT_CAR_EAST"),
      
      sliderInput("range", 
                  label = "Range of interest:",
                  min = 0, max = 36818, value = c(0, 36818))
      ),
    
    mainPanel(plotOutput("scatterPlot"))
  )
))