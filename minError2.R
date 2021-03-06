
#The function minAccumulatedError calculates the minimum accumulated error between the real cummulative distribution 
# for a finite population compared and the cummulative distribution assuming a parametric distribution
# the parametric distribution takes its parameters from the whole distribution. 
# The function receives a value y at which the accumulated error is calculated, the values for each individual in the
# population and the assumed parametric distribution, supports normal, lognormal and gamma.
minAccumulatedError2 <- function(y, pop, distribution="norm"){
  M        <- length(pop) # Population size
  pop      <- sort(pop)   # Order population
  Xm       <- pop[M]      # Largest value in the population
  X0       <- pop[1]      # Smallest value
  
  
  # Choose integral of the parametric cummulative distribution
  switch(distribution,
         "norm" = {
           mupop    <- mean(pop)
           sdpop    <- sd(pop)
           integral <- function(a,b){integrate(function(t){pnorm(t,mupop,sdpop)}, a, b)}
         },
         "lnorm"= {
           mupop    <- mean(pop)
           varpop   <- var(pop)
           
           Param.log<-function(mux,varx){
             sd2     <- log(varx/(mux^2)+1)
             meanlog <- log(mux)-1/2*sd2
             sdlog   <- sqrt(sd2)
             return(c(meanlog,sdlog))
           }
           params   <- Param.log(mupop, varpop)
           integral <- function(a,b){integrate(function(t){plnorm(t,params[1],params[2])}, a, b)}
         },
         "gamma"= {
           mupop    <- mean(pop)
           varpop   <- var(pop)
           
           Param.gamma<-function(mux,varx){
             alfa     <- mux^2/varx
             beta     <- alfa/mux
             return(c(alfa,beta))
           }
           params   <- Param.gamma(mupop, varpop)
           integral <- function(a,b){integrate(function(t){pgamma(t,params[1],params[2])}, a, b)}
         }
  )
  
  # Accumulated error for values smaller than X0
  if(y<X0){
    firstIntegral <- as.numeric(integral(-Inf,y)[1])
    return(firstIntegral)
  }
  
  tabpop   <- table(pop)
  ivec     <- sort(unique(pop))
  ivec     <- ivec[which(ivec<=y)]
  j        <- min(max(ivec),M)
  
  # Calculate the integral from -Inf,x0 
  firstIntegral <- as.numeric(integral(-Inf, X0)[1])
 
  #Parte1
  sums          <- 0
  sumtab        <- tabpop[1]
  
  if(j>1){
    for(i in 2:length(ivec)){
        
      lim <- min(ivec[i],y)
      intg1 <- as.numeric(integral(ivec[i-1],lim)[1])
      #intg2 <- as.numeric(integrate(function(t){ivec[i-1]/M + 0*t}, ivec[i-1], lim)[1])
      intg2 <- as.numeric(integrate(function(t){sumtab/M + 0*t}, ivec[i-1], lim)[1])
      sums <- sums + abs(intg1 - intg2)
      sumtab <- sumtab + tabpop[i]
      
    }
  }
  sums <- as.numeric(sums)
  
  
  
  firstTerm <- firstIntegral+ sums
  
  # parte2
  if(y>Xm){
    secondTerm <- y-Xm-as.numeric(integral(Xm,y)[1])
  }else{
    secondTerm <- 0
  }
  minAccumError <- firstTerm + secondTerm
  return(as.numeric(minAccumError))
  
}