library(shiny)
library(shinydashboard)
ui<-dashboardPage(
  dashboardHeader(title="premierDash"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard",tabName  = "dashboard",icon = icon("dashboard")),
      menuItem("Widgets",tabName = "widgets",icon=icon("th"))
      )
    
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName="dashboard",
         fluidRow(
          box(title="controls",
          sliderInput(inputId = "slider",label="number of observations",value=50,min = 1,max = 100)),
      box(plotOutput(outputId = "plot1",height = 250))
      )
    ),
    tabItem(tabName="widgets",
             h2("wiwwi")
    )
    
  )
)

server<-function(input,output){
  data<-reactive({input$slider})
  output$plot1<-renderPlot({hist(rnorm(data()),main = "hist loi normale")})
  
}

shinyApp(ui=ui,server = server)
