
library(shiny)


shinyUI(fluidPage(
  titlePanel("Integration Monte Carlo"),
  
  
  sidebarLayout(
    sidebarPanel(
      radioButtons("tarea", label="Visualtion ",
                   choices = c(
                     "Integration",
                     "Integration multidimensionnelle"
                   ),
                   selected="Integration"
      )
      
    ),
    
    mainPanel(
      conditionalPanel(
        condition="input.tarea=='Integration'",
        h2("Integration de g sur [a,b]"),
        
        textInput(
          inputId="expresion1", 
          label="g(x)=",
          value="2*x"
        ),
        
        sliderInput("a", "a", min=-1000, max=1000, value=0),
        sliderInput("b", "b", min=-1000, max=1000, value=10),
        numericInput("nsim", "Numero de simulaciones", value=100), 
        sliderInput("alpha","alpha",min=0,max=1,value=0.05),
        
        textOutput("resultat1"),
        
        textOutput("resultat2")
        
    )
   )
  )
 )
)
