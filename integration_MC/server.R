
library(shiny)

shinyServer(function(input, output) {
  set.seed(20160819)
  
  fun1 <- reactive({
    texto <- paste("aux <-function(x) {return( ",input$expresion1,")}")
    eval(parse(text=texto))
    aux
  })
  
  x<-reactive({runif(n = input$nsim,min=input$a,max=input$b)})
  g_x<-reactive({sapply(x(),fun1())})
 
  #Application Monte-Carlo
  
  y<-reactive({sum(sapply(x(),fun1()))*(1/input$nsim)*(input$b-input$a)})
  #[sum((i=1)to nsim) g(Xi)]   con Xi~unif(a,b)  *(1/nsim)      *(b-a)
  
  
  ecart_type<-reactive({sqrt((1/(input$nsim-1))*sum( ((input$b-input$a)*g_x()-y())**2 ))}) 
  z<-reactive({qnorm(1-((input$alpha)/2),0,1)})
  dif<-reactive({z()*ecart_type()*(1/sqrt(input$nsim))})
  
  text1<-reactive({texto1<-paste("Approximation integrale de " ,input$expresion1," sur [ ",input$a," , ",input$b ," ] = ",y())})
  text2<-reactive({texto2<-paste("Intervalo de confianza de ",(1-(input$alpha))*100," % es [",y()-dif()," , ",y()+dif()," ]")})
  
  output$resultat1 <- renderText({text1()})
  output$resultat2<-renderText({text2()})
  
  
  
})
