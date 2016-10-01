
library(shiny)
library(shinydashboard)
library(plotly)

shinyUI(dashboardPage(
  dashboardHeader(title="Simulation distribution exponentielle"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Simul loi unif->Exp(lambda)",tabName  = "simul")
      
    )
    
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName="simul",
              fluidRow(
                box(title="lambda",
                    sliderInput(inputId = "lbda",label="lambda",value=1,min =0.001,max = 10)),
                
                box(title="numero de simulaciones",
                    sliderInput(inputId = "nsim",label="number of observations",value=50,min = 1,max = 10000)),
                box(plotOutput(outputId = "plot1",height =500,width=500)),
                box(plotOutput(outputId="plot3",height=500,width = 500)),
                box(plotlyOutput("hist"))
                
              )
              
      )
    )
  )
  
  
))
