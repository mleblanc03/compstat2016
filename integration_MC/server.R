library(ggplot2)
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
  
  #Graphicar:

  
  text1<-reactive({texto1<-paste("Approximation integrale de " ,input$expresion1," sur [ ",input$a," , ",input$b ," ] = ",y())})
  text2<-reactive({texto2<-paste("Intervalo de confianza de ",(1-(input$alpha))*100," % es [",y()-dif()," , ",y()+dif()," ]")})
  
  output$resultat1 <- renderText({text1()})
  output$resultat2<-renderText({text2()})
  
  output$Title<-renderText({"Resultados para N_simulaciones:"})
  
 
  
   output$graphica<-renderPlot( {
    
    simul<-c(10,100,1000,10000,100000)
    
    y_graph<-rep(0,5)
    
    ecart_type_graph<-rep(0,5)
    dif_graph<-rep(0,5)
    
    j=1                          
      for (i in simul) {
        
         x_graph<-runif(n =i,min=input$a,max=input$b)
         g_x_graph<-sapply(x_graph,fun1())
         y_graph[j]<-sum(sapply(x_graph,fun1())*(1/i)*(input$b-input$a)) 
         
         
         ecart_type_graph[j]<-sqrt((1/(i-1))*sum( ((input$b-input$a)*g_x_graph-y_graph[j])**2 ))
         dif_graph[j]<-z()*ecart_type_graph[j]*(1/sqrt(i))
         
         j=j+1 
         
         }
          
    resultats=data.frame(num=log(simul,base=10),integ=y_graph,limitsup=y_graph+dif_graph,limitinf=y_graph-dif_graph)
    
    ggplot(data=resultats,aes(x=num))+
      geom_line(aes(y=integ),color="red")+
      geom_line(aes(y=limitsup),color="blue")+
      geom_line(aes(y=limitinf),color="blue")+
      xlab("Numero de simulaciones")+ylab("Resulados de integration MC")
    
    })
  
})
