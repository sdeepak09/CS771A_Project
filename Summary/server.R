#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
tonnage=read.csv("data/tonnage.csv")
tonnage=subset(tonnage,tonnage$TOT_CAR_EAST!=0 & tonnage$TOT_CAR_WEST!=0 & tonnage$TOT_TRN_EAST!=0 & tonnage$TOT_TRN_WEST!=0)
tonnage=data.frame(tonnage,total_train=tonnage$TOT_TRN_EAST+tonnage$TOT_TRN_WEST)

shinyServer(
  function(input, output) {
    output$scatterPlot <- renderPlot({
      args <- switch(input$var,
                     "TOT_CAR_EAST" = tonnage$TOT_CAR_EAST,
                     "TOT_CAR_WEST" = tonnage$TOT_CAR_WEST,
                     "TOT_TRN_EAST" = tonnage$TOT_TRN_EAST,
                     "TOT_TRN_WEST" = tonnage$TOT_TRN_WEST,
                     "total_train"  = tonnage$total_train)
                     
      plot(tonnage$TOT_DFLT_MGT[input$range[1]:input$range[2]],args[input$range[1]:input$range[2]])
      
    }
  
)
  }
)