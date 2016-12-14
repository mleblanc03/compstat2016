library(Rcpp)
library(shiny)
library(plotly)

Rcpp::sourceCpp("Bayesian_fonctions_2.cpp")
library(ggplot2)
library(DT)
data <- read.csv("wine.csv")

shinyUI(fluidPage(
  
  titlePanel("Tareas"),
  
      
    navlistPanel(
      
      tabPanel("First",title="Tarea1",
                h2("Simulacion de la distribucion exponancial"),
                sliderInput(inputId = "lbda",label="Lambda de la ley exponancial",value=1,min =0.001,max = 10),
                sliderInput(inputId = "nsim_tarea1",label="Numero de observaciones",value=1000,min = 1,max = 10000),
                plotOutput(outputId = "plot1",height =500,width=500),
                plotOutput(outputId="plot3",height=500,width = 500),
                plotlyOutput("hist_tarea1")
      
               
               
                ),
      
      
      tabPanel(title="Tarea2",
               h2("Integracion Monte Carlo"),
               h3("Integration de la funcion g en [a,b]"),
               
               textInput(
                 inputId="expresion1", 
                 label="g(x)=",
                 value="2*x"
               ),
               
               sliderInput("a", "a", min=-1000, max=1000, value=0),
               sliderInput("b", "b (cuidado a>=b)", min=-1000, max=1000, value=10),
               numericInput("nsim", "Numero de simulaciones", value=1000), 
               sliderInput("alpha","alpha",min=0,max=1,value=0.05),
               
               textOutput("resultat_1"),
               
               textOutput("resultat_2"),
               
               textOutput("Title"),
               
               plotOutput(("graphica"))
               
               
               ),
               
               
      tabPanel(
        title="Tarea 4,5,6",
        
  tabsetPanel(
    
    tabPanel(title="Presentacion de los datos",
             fluidRow(
              DT::dataTableOutput("table")),
              selectInput("X", "variable dependiente X (Flavanoids mejor)",choices =names(data),selected = "Flavanoids"), 
              selectInput("Y", "variable independiente Y (TotalPhenols mejor)",choices = names(data),selected = "TotalPhenols"),
              plotOutput("dispersion")
             
              ), 
      
 
    tabPanel(title="Preparacion de los distribuciones inciales",
             titlePanel("Normal para (alpha,beta) y Gamma para sigma2"),
             numericInput("mean_a","mean_a",value=0),
             numericInput("sd_a","sd_a",value=100,min=0.0001),
             numericInput("mean_b","mean_b",value=0),
             numericInput("sd_b","sd_b",value=100,min=0.0001),
             numericInput("shape_sigma2","shape_sigma2",value=0.01,min=0.0001),
             numericInput("r_sigma2","r_sigma2",value=0.01,min=0.0001),
             plotOutput("alpha_graph"),
             plotOutput("beta_graph"),
             plotOutput("sigma2_graph")
    ),
   
    tabPanel(title="MCMC",
             titlePanel("Applicacion MCMC"),
             numericInput("num_simul","Numero de simulaciones",value=1000),
             numericInput("num_caden","Numero de cadenas",value=1,min=1),
             numericInput("num_burn","Numero de burning",value =100),
             numericInput("num_caden_selection","La cadena que quieres extractar (attencion no tiene que ser superior a Numero de cadena)",value=1),
             numericInput("alpha_inicial","alpha_incial",value=0),
             numericInput("beta_incial","beta_incial",value=0),
             numericInput("sigma_2","sigma_2",value=1,min=0.001),
             numericInput("tamano","tamano brinco",value=0.01,min=0.0001),
             numericInput("tiempo","tiempo t tal que matrice_jump=(2.38)^2*cov(sim[0->t])+tamano*ID3 desde de la simulacion 2*t",value=100),
             actionButton("Run", "Run_MCMC"),
             textOutput("text_tarea5_a"),
             textOutput("text_tarea5_b"),
             textOutput("text_tarea5_c"),
             plotOutput("hist_aplha"),
             plotOutput("hist_beta"),
             plotOutput("hist_sigma2"),
             selectInput("graph_density","Elige su comparacion de densidad (la cts de normalizacion no es conocido decidi de dividir por max(dens$y)) ",choices=c("alpha","beta","sigma_2")),
             plotOutput("comparar")
             
      )
  
  
  
)))))



