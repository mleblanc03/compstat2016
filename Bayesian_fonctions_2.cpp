
#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;


// ojo, para que esta funcion sirva las dos lineas superiores del codigo en rcpp debe ser asi

// [[Rcpp::export]]

NumericMatrix mvrnorm(int n, NumericVector mu, NumericMatrix sigma) {
  int ncols = sigma.ncol();
  arma::mat Y = arma::randn(n, ncols);
  return wrap(arma::repmat(as<arma::vec>(mu), 1, n).t() + Y * arma::chol(as<arma::mat>(sigma)));
}


// [[Rcpp::export]]
double logapriori_beta(double b,double mean_b,double sd_b)
{return R::dnorm(b,mean_b,sd_b,1);}

// [[Rcpp::export]]
double logapriori_a(double a ,double mean_a,double sd_a)
{return R::dnorm(a,mean_a,sd_a,1);}

// [[Rcpp::export]]
double logapriori_sigma2(double sigma2,double shape_sigma2,double r_sigma2)
{return R::dgamma(sigma2,shape_sigma2,r_sigma2,1);}

// [[Rcpp::export]]
double logapriori (NumericVector theta,
                   double mean_a,
                   double sd_a,
                   double mean_b,
                   double sd_b,
                   double shape_sigma2,
                   double r_sigma2)
{ double a=theta[0];
  double b=theta[1];
  double sigma2=theta[2];
  
  return logapriori_a(a,mean_a,sd_a)+logapriori_beta(b,mean_b,sd_b)+logapriori_sigma2(sigma2,shape_sigma2,r_sigma2);
}

// [[Rcpp::export]]

double loglikelihood(NumericVector theta ,int n_datos,NumericVector Y,NumericVector X )
{NumericVector error2(n_datos);
 //double pi=3.14159265358979323846;
 double pi=3.1415;
  for (int i=0; i < n_datos; i++)
  {
    error2[i]=pow(Y[i]-(theta[1]*X[i]+theta[0]),2);
  }
  
   return (-n_datos/2)*log(2*pi*pow(theta[2],1))-(1/(2*pow(theta[2],1)))*sum(error2);
  
  }

// [[Rcpp::export]]
double log_posterior(
    int n_datos,
    NumericVector theta,
    NumericVector Y,
    NumericVector X,
    double mean_a,
    double sd_a,
    double mean_b,
    double sd_b,
    double shape_sigma2,
    double r_sigma2) {
  
  return loglikelihood(theta,n_datos,Y,X) + logapriori(theta,mean_a,sd_a,
                       mean_b,sd_b,shape_sigma2,r_sigma2);
}

// [[Rcpp::export]]
NumericMatrix cov_cpp(NumericMatrix X) {
  return wrap(arma::cov(as<arma::mat>(X)));
}

// [[Rcpp::export]]
NumericMatrix separator_distributor(NumericMatrix sim,double len_sim,int numero_cadena){
  
  NumericMatrix m(len_sim,3);
  
  for (int i=0;i<len_sim;i++){
    
    m(i,0)=sim(i*3,numero_cadena-1);
    m(i,1)=sim(i*3+1,numero_cadena-1);
    m(i,2)=sim(i*3+2,numero_cadena-1);
  }
  
  return m;}

// [[Rcpp::export]]
NumericMatrix run_mcmc_3D(
    int n_sim,
    int n_datos,
    int n_cadenas,
    NumericVector theta_initial,
    NumericVector Y,
    NumericVector X,
    NumericMatrix matrice_jump,
    double mean_a,
    double sd_a,
    double mean_b,
    double sd_b,
    double shape_sigma2,
    double r_sigma2,
    int t)
    
 {
  NumericMatrix sim((n_sim + 1)*3,n_cadenas); // aqui voy a guardar las simulaciones
  double U;
  NumericMatrix eta(1,3);
  NumericVector eta_vector(3);
  NumericVector simul(3);
  bool accepted=FALSE;

 
  
  for (int j=0;j<n_cadenas;j++){
  
  sim(0,j) = theta_initial[0];
  sim(1,j) =theta_initial[1];
  sim(2,j)=theta_initial[2];
  
    for (int i=0; i < n_sim; i++) {
      
      simul[0]=sim(i*3,j);
      simul[1]=sim(1+i*3,j);
      simul[2]=sim(2+i*3,j);
      
    if (i==2*t and n_sim>=t ){
      NumericMatrix m(t,3);
      m=separator_distributor(sim,t,j);
      matrice_jump<-2.38*2.38*cov_cpp(m)+matrice_jump; //aqui i==2*t para evitar problema de surbounds para separator_distribut
    }                                                  // es un pequeno ajustamiento
    
    do{
    
      U = (runif(1))[0];
      eta =mvrnorm(1,simul, matrice_jump);
      
      eta_vector[0]=eta(0,0);
      eta_vector[1]=eta(0,1);
      eta_vector[2]=eta(0,2);
      
      accepted =(log(U)<= log_posterior(n_datos,eta_vector,Y,X,mean_a,sd_a,mean_b,sd_b,shape_sigma2,r_sigma2)-
                 log_posterior(n_datos,simul,Y,X,mean_a,sd_a,mean_b,sd_b,shape_sigma2,r_sigma2));
            
                                 
                                  
     
    }  while (accepted==FALSE);
   
    sim((i+1)*3,j) = eta_vector[0];
    sim(1+(i+1)*3,j)=eta_vector[1];
    sim(2+(i+1)*3,j)=eta_vector[2];
      }
    }
  return sim;
  }









