
library(shiny)
library(shinydashboard)
library(plotly)

shinyServer(function(input, output) {
  set.seed(20160817)
  
  data<-reactive({-log(1-(runif(input$nsim)))/input$lbda})
  
  xtest<-reactive({rexp(n=input$nsim,rate=input$lbda)})
  
  p<-plot_ly(x=data(),type="histogram",opacity=0.3)%>%add_trace(x=xtest(),type="histogram",opacity=0.3)
  output$plot1<-renderPlot({hist(data(),main="histogramme simulacion exp(lambda)",breaks=80)})
  output$hist<-renderPlotly({p})
  output$plot3<-renderPlot({hist(xtest(),main="histogramme fonction rexp ",breaks=80)})
   
  
  
})
