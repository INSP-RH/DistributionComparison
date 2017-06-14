##########Error en la Prevalencia Población Uniforme############
rm(list = ls())
set.seed(633256)

M <- 1000 #Population size
n <- 100  #sample size

s <-1000 #Tamaño simulación

a     <- 18.5
b     <- 25

MVlogn <- function(muestra){
  mu <- sum(log(muestra))/length(muestra)
  var <- sum((log(muestra)- mu)^2)/length(muestra)
  sig <- sqrt(var)
  return(c(mu,sig))
}

Paramlogn<-function(mu,var){
  vl     <- log(var + mu^2) - 2*log(mu)
  ml <- 2*log(mu) - 1/2*log(var + mu^2) 
  sdl   <- sqrt(vl)
  return(c(ml,sdl))
}

##############################################

pop   <- sample(10:60, M, replace = TRUE) 
Freal <- ecdf(pop)
muv <- mean(pop)
varv <-  var(pop)*((M-1)/M)
sdv <- sqrt(varv)

paramv <- Paramlogn(muv,varv)
#paramv <- fitdistr(pop,"log-normal")



#Simulacion
Errores1 <- vector('numeric', 0)
Errores2 <- vector('numeric', 0)
Errores3 <- vector('numeric', 0)

for (i in 1:s) {
  sim<- sample(pop, size=n*.95, replace=FALSE)
  mu <- mean(sim)
  
   #Contaminación
  cont <- rnorm(n*.05, mu, 1000)
  
  sim <- c(sim,cont)
  
  mu <- mean(sim)
  sig <- sd(sim)
  
  Fn <- ecdf(sim)
  
  Erres1 <- (Freal(b)-Freal(a))- (Fn(b) - Fn(a))
  Erres2 <- (Freal(b)-Freal(a))- (pnorm(b,mu,sig) - pnorm(a,mu,sig))
  Erres3 <- (Fn(b)-Fn(a))- (pnorm(b,mu,sig) - pnorm(a,mu,sig))
  
  Errores1 <- c(Errores1, Erres1)
  Errores2 <- c(Errores2,Erres2)
  Errores3 <- c(Errores3, Erres3)
}

#Esperanza del Error
summary(Errores1)
summary(Errores2)
summary(Errores3)

########  Cuantiles  ######

Qes1 <- quantile(Errores1, probs = c(0, .05, .1, .90, 0.95))

#### ????? 

# Bootstrap for variance
library(boot)

# function for the estimate 
Est <- function(data,indices) {
  d <- data[indices]
  err <- var(d)
  return(err) 
} 

# bootstrapping with 1000 replications 
results <- boot(data=Errores1, statistic=Est, 
                R=1000)



####Ver los extremos

###Mayor que 50
b <- 50

ErrorVerdadero1 <- pnorm(b,muv,sdv) - Freal(b) 
ErrorVerdadero2 <- plnorm(b,paramv[1],paramv[2]) - Freal(b)


#Simulacion
muestras <- matrix(NA, ncol = n, nrow = 0)
Errores1 <- vector('numeric', 0)
Errores2 <- vector('numeric', 0)

estimadoresln <- matrix(NA, ncol=2, nrow=0)


for (i in 1:s) {
  sim<- sample(pop, size=n, replace=FALSE)
  #muestras  <- rbind(muestras, sim)
  mu  <- mean(sim)
  sig <- sd(sim)*sqrt((n-1)/n)
  Fn  <- ecdf(sim)
  
  Erres <- pnorm(b,mu,sig) - Fn(b)
  
  param <- fitdistr(sim,"log-normal")
  est   <- param[[1]] #c(param[[1]][1], param[[1]][2])
  
  Erres2 <- plnorm(b,est[1],est[2]) -Fn(b)
  
  Errores1 <- c(Errores1,Erres)
  Errores2 <- c(Errores2, Erres2)
  estimadoresln <- rbind(estimadoresln, est)
}

#Esperanza del Error
summary(Errores1, na.rm = FALSE)
summary(Errores2, na.rm = FALSE, dims = 1)
