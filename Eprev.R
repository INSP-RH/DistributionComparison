set.seed(633256)

M <- 1000 #Population size
n <- 300  #sample size

s <-1000 #Tamaño simulación

#Poblacion y su empirica
pop   <- sample(10:60, M, replace = TRUE) 
Freal <- ecdf(pop)

#Simulacion
muestras <- matrix(NA, ncol = n, nrow = 0)
Errores1 <- vector('numeric', 0)
Errores2 <- vector('numeric', 0)
Errorv <- vector('numeric', 0)
a     <- 30 
b     <- 60


for (i in 1:s) {
  sim<- sample(pop, size=n, replace=FALSE)
  muestras  <- rbind(muestras, sim)
  mu <- mean(sim)
  sig <- sd(sim)
  Fmu <- ecdf(sim)
  
  Error <-  (Freal(b)-Freal(a))- (Fmu(b) - Fmu(a))
  Erres <- (Freal(b)-Freal(a))- (pnorm(b,mu,sig) - pnorm(a,mu,sig))
  
  param<-fitdistr(sim,"log-normal")
  ECDF <- plnorm(datos,param[[1]][1],param[[1]][2])
  Erres2 <- (Freal(b)-Freal(a))- (plnorm(b,param[[1]][1],param[[1]][2]) - plnorm(a,param[[1]][1],param[[1]][2]))
  
  Errorv <- rbind(Errorv,Error)
  Errores1 <- rbind(Errores1,Erres)
  Errores2 <- rbind(Errores2, Erres2)
}

#Esperanza del Error
Esp<- colMeans(Errorv, na.rm = FALSE, dims = 1)
Espes<- colMeans(Errores1, na.rm = FALSE, dims = 1)
Espes2 <- colMeans(Errores2, na.rm = FALSE, dims = 1)

Var <- var(Errorv)
Vares <- var(Errores)

#### ????? 





