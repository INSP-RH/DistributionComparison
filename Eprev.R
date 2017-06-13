set.seed(633256)

M <- 1000 #Population size
n <- 1000  #sample size

s <-1000 #Tamaño simulación

a     <- 30 
b     <- 60
#Poblacion y su empirica

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
#paramv <- fitdistr(sim,"log-normal")


ErrorVerdadero1 <- (Freal(b) - Freal(a)) - (pnorm(b,muv,sdv)- pnorm(a,muv,sdv))
ErrorVerdadero2 <- (Freal(b) - Freal(a)) - (plnorm(b,paramv[1],paramv[2])- plnorm(a,paramv[1],paramv[2]))
#ErrorVerdadero2 <- (Freal(b) - Freal(a)) - (plnorm(b,paramv[[1]][1],paramv[[1]][2])- plnorm(a,paramv[[1]][1],paramv[[1]][2]))

#Simulacion
muestras <- matrix(NA, ncol = n, nrow = 0)
Errores1 <- vector('numeric', 0)
Errores2 <- vector('numeric', 0)


for (i in 1:s) {
  sim<- sample(pop, size=n, replace=FALSE)
  muestras  <- rbind(muestras, sim)
  mu <- mean(sim)
  sig <- sd(sim)
  
  Erres <- (Freal(b)-Freal(a))- (pnorm(b,mu,sig) - pnorm(a,mu,sig))
  
  param<-fitdistr(sim,"log-normal")
  Erres2 <- (Freal(b)-Freal(a))- (plnorm(b,param[[1]][1],param[[1]][2]) - plnorm(a,param[[1]][1],param[[1]][2]))

  Errores1 <- rbind(Errores1,Erres)
  Errores2 <- rbind(Errores2, Erres2)
}

#Esperanza del Error
Espes<- colMeans(Errores1, na.rm = FALSE, dims = 1)
Espes2 <- colMeans(Errores2, na.rm = FALSE, dims = 1)

Vares1 <- var(Errores1)
Vares2 <- var(Errores2)


########  Cuantiles  ######

Qes1 <- quantile(Errores1, probs = c(0, .05, .1, .90, 0.95))
Qes2 <- quantile(Errores2, probs = c(0, .05, .1, .90, 0.95))

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



####Cambiar a los extremos
