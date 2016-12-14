library(Rcpp)

Rcpp::sourceCpp("Bayesian_fonctions_2.cpp")
library(shiny)
library(ggplot2)
library(plotly)

data<-read.csv("wine.csv")

shinyServer(function(input, output) {
  
  ####Tarea1####
  
  set.seed(20160817)
  
  data_tarea1<-reactive({-log(1-(runif(input$nsim_tarea1)))/input$lbda})
  
  xtest_tarea1<-reactive({rexp(n=input$nsim_tarea1,rate=input$lbda)})
  
  p<-plot_ly(x=data_tarea1(),type="histogram",opacity=0.3,
             main="Comparacion entre nuestra simulacion y la funcion rexp")%>% add_trace(x=xtest_tarea1(),
                                                                                         type="histogram",
                                                                                         opacity=0.3)%>% layout(title = "Comparacion entre el histograma de nuestra simulacion(Azul) y Rexp(Naranja)", 
                                                                                                                scene = list(
                                                                                                                xaxis = list(title = "x"))) 
                                                                                                                 
                                               
                                                          
                                               
                                                  
                                                           
  output$plot1<-renderPlot({hist(data_tarea1(),main="Histograma de la simulacion de exp(lambda)",breaks=80,xlab = "x")})
  output$hist_tarea1<-renderPlotly({p})
  output$plot3<-renderPlot({hist(xtest_tarea1(),main="Histograma de la funcion rexp ",breaks=80,xlab="x")})
  
  
  ####Tarea2####
  
  set.seed(20160819)
  
  fun1 <- reactive({
    texto <- paste("aux <-function(x) {return( ",input$expresion1,")}")
    eval(parse(text=texto))
    aux
  })
  
  x_tarea2<-reactive({runif(n = input$nsim,min=input$a,max=input$b)})
  g_x_tarea2<-reactive({sapply(x_tarea2(),fun1())})
  
  #Application Monte-Carlo
  
  y_tarea2<-reactive({sum(sapply(x_tarea2(),fun1()))*(1/input$nsim)*(input$b-input$a)})
  #[sum((i=1)to nsim) g(Xi)]   con Xi~unif(a,b)  *(1/nsim)      *(b-a)
  
  
  ecart_type<-reactive({sqrt((1/(input$nsim-1))*sum( ((input$b-input$a)*g_x_tarea2()-y_tarea2())**2 ))}) 
  z_tarea2<-reactive({qnorm(1-((input$alpha)/2),0,1)})
  dif<-reactive({z_tarea2()*ecart_type()*(1/sqrt(input$nsim))})
  
  #Graphicar:
  
  
  text1<-reactive({texto1<-paste("Approximacion del integral de la funcion g:x->" ,input$expresion1," sobre [ ",input$a," , ",input$b ," ] = ",y_tarea2())})
  text2<-reactive({texto2<-paste("Intervalo de confianza de ",(1-(input$alpha))*100," % es [",y_tarea2()-dif()," , ",y_tarea2()+dif()," ]")})
  
  output$resultat_1 <- renderText({text1()})
  output$resultat_2<-renderText({text2()})
  
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
      dif_graph[j]<-z_tarea2()*ecart_type_graph[j]*(1/sqrt(i))
      
      j=j+1 
      
    }
    
    resultats=data.frame(num=log(simul,base=10),integ=y_graph,limitsup=y_graph+dif_graph,limitinf=y_graph-dif_graph)
    
    ggplot(data=resultats,aes(x=num))+
      geom_line(aes(y=integ),color="red")+
      geom_line(aes(y=limitsup),color="blue")+
      geom_line(aes(y=limitinf),color="blue")+
      xlab("Numero de simulaciones")+ylab("Resulados de integration MC")
    
  })
  
  
  ####Tarea4####
  
  x_essai=seq(-50,50,0.5)
  
  y_essai_a<-reactive({exp(sapply(x_essai,logapriori_a,input$mean_a,input$sd_a))})
  y_essai_b<-reactive({exp(sapply(x_essai,logapriori_beta,input$mean_b,input$sd_b))})
  y_essai_sigma2<-reactive({exp(sapply(x_essai,logapriori_sigma2,input$shape_sigma2,input$r_sigma2))})
  
  x<-reactive({data[[input$X]]})
  y<-reactive({data[[input$Y]]})
  
  output$table <- DT::renderDataTable(DT::datatable({
    data <- read.csv("wine.csv")
    data
    
  }))
  
  output$dispersion <- renderPlot({
    
    plot(x(),y(),xlab="X",ylab ="Y")
    
    })
  
  output$alpha_graph <- renderPlot({
    
    plot(x=x_essai,y=y_essai_a(),type="l",xlab="x",ylab ="Normal(mean_a,sd_a)")
    
  })
  
  output$beta_graph <- renderPlot({
    
    plot(x=x_essai,y=y_essai_b(),type="l",xlab="x",ylab ="Normal(mean_b,sd_b)")
    
  })
  
  output$sigma2_graph <- renderPlot({
    
    plot(x=x_essai,y=y_essai_sigma2(),type="l",xlab="x",ylab ="Gamma(shape_sigma2,r_sigma2)")
    
  })
  
  ##### Tarea 5 #####
  
  
  observeEvent(input$Run, {
  
  n_datos=nrow(data)
  matrice_jump<-reactive({input$tamano*matrix(c(1,0,0,0,1,0,0,0,1),nrow = 3,ncol=3,byrow = T)})
  theta_inicial<-reactive({return(c(input$alpha_inicial,input$beta_incial,input$sigma_2))})
  y<-reactive({data[[input$Y]]})
  X<-reactive({data[[input$X]]})
  
  
 
                                    
  simulation<-reactive({return (run_mcmc_3D(input$num_simul,n_datos,input$num_caden,theta_inicial(),y(),X(),matrice_jump(),
                                            input$mean_a,input$sd_a,input$mean_b,
                                            input$sd_b,input$shape_sigma2,input$r_sigma2,input$tiempo))})                               
                                 
  
  m<-reactive({
    
    m=matrix(rep(0,nrow(simulation())/3),nrow=(nrow(simulation()))/3,ncol =3)
    
    for (i in 1:(nrow(simulation())/3)){
      m[i,1]=simulation()[i*3-2,input$num_caden_selection];
      m[i,2]=simulation()[i*3-1,input$num_caden_selection];
      m[i,3]=simulation()[i*3,input$num_caden_selection];
    }
  return (m[-c(1:input$num_burn),])
  }) 
  text_tarea5_a<-reactive({text_alpha<-paste("E(alpha|(X,Y))=",mean(m()[,1]),"  y  IC_95=[",quantile(m()[,1],0.025)[[1]], ";" ,quantile(m()[,1],0.975)[[1]], "]")
                               return(text_alpha)})
  text_tarea5_b<-reactive({text_beta<-paste("E(beta|(X,Y))=",mean(m()[,2]),"  y  IC_95=[",quantile(m()[,2],0.025)[[1]], ";" ,quantile(m()[,2],0.975)[[1]], "]")
  return(text_beta)})
  text_tarea5_c<-reactive({text_sigma_2<-paste("E(sigma_2|(X,Y))=",mean(m()[,3]),"  y  IC_95=[",quantile(m()[,3],0.025)[[1]], ";" ,quantile(m()[,3],0.975)[[1]], "]")
  return(text_sigma_2)})
  
  output$text_tarea5_a<-renderText({text_tarea5_a()})
  output$text_tarea5_b<-renderText({text_tarea5_b()})
  output$text_tarea5_c<-renderText({text_tarea5_c()})
  
  output$hist_aplha<-renderPlot({hist(m()[,1],xlab="alpha",main="histogramme de alpha")})
  output$hist_beta<-renderPlot({hist(m()[,2],xlab="beta",main="histogramme de beta")})
  output$hist_sigma2<-renderPlot({hist(m()[,3],xlab="sigma_2",main="histogramme de sigma_2")})
  
  choice<-reactive({input$graph_density})
  #output$comparar<-renderPlot({if (choice()=="alpha"){par(mfrow=c(2,2))
                      #plot(density(m()[,1]),main="Densidad aproximada de alpha(red)  comparado a la distribucion inicial de alpha(blue)"
                                                                #,xlab="alpha",col="red" )
                                                              #plot(x_essai,y_essai_a(),col="blue")}
                                #if (choice()=="beta"){plot(density(m()[,2]),main="Densidad aproximada de beta(red) comparado a la distribucion inicial de beta(blue)"
                                                                    #,xlab="alpha",col="red" )
                                                                    #lines(x_essai,y_essai_b(),col="blue")}
    
                                #if (choice()=="sigma_2"){plot(density(m()[,3]),main="Densidad aproximada de sigma_2(red) comparado a la distribucion inicial de sigma_2(blue)" 
                                                                    #,xlab="alpha",col="red" )
                                                                    #lines(x_essai,y_essai_sigma2(),col="blue")}
    
  output$comparar<-renderPlot({
    
    if (choice()=="alpha"){
    dens<-density(m()[,1])
    post<-data.frame(
      tipo="posterior",
      x=dens$x, 
      densidad=dens$y/(max(dens$y))
    )
    
    prior<-data.frame(
      tipo="a priori",
      x=seq(-10,10,0.05),
      densidad=dnorm(seq(-10,10,0.05),input$mean_a,input$sd_a)
    )
    dat<-rbind(prior,post)
    g<-ggplot(dat,aes(x=x,y=densidad,color=tipo))+
      geom_line(size=1)}
    
    if (choice()=="beta"){
      dens<-density(m()[,2])
      post<-data.frame(
        tipo="posterior",
        x=dens$x, 
        densidad=dens$y/(max(dens$y))
      )
      
      prior<-data.frame(
        tipo="a priori",
        x=seq(-10,10,0.05),
        densidad=dnorm(seq(-10,10,0.05),input$mean_b,input$sd_b)
      )
      dat<-rbind(prior,post)
      g<-ggplot(dat,aes(x=x,y=densidad,color=tipo))+
        geom_line(size=1)}
    
    if (choice()=="sigma_2"){
      dens<-density(m()[,3])
      post<-data.frame(
        tipo="posterior",
        x=dens$x, 
        densidad=dens$y/(max(dens$y))
      )
      
      prior<-data.frame(
        tipo="a priori",
        x=seq(0,20,0.05),
        densidad=dgamma(seq(0,20,0.05),input$shape_sigma2,input$r_sigma2)
      )
      dat<-rbind(prior,post)
      g<-ggplot(dat,aes(x=x,y=densidad,color=tipo))+
        geom_line(size=1)}
      
      g
    

        })
  
    })
  


  
    
    
    
  
  })


