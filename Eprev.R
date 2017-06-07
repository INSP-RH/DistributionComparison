set.seed(633256)

M <- 1000 #Population size
n <- 100  #sample size

s <-1000 #Tamaño simulación

#Poblacion y su empirica
pop   <- sample(10:60, M, replace = TRUE) 
Freal <- ecdf(pop)

#Simulacion
muestras <- matrix(NA, ncol = n, nrow = 0)
Error <- vector('numeric', 0)
a     <- 30 
b     <- 60


for (i in 1:s) {
  sim<- sample(pop, size=n, replace=FALSE)
  muestras  <- rbind(muestras, sim)
  mu <- mean(sim)
  sig <- sd(sim)
  Errori <- abs((Freal(b)-Freal(a))- (pnorm(b,mu,sig) - pnorm(a,mu,sig)))
  Error <- rbind(Error,Errori)
}

#Esperanza del Error
Esp <- colMeans(Error, na.rm = FALSE, dims = 1)
Var <- var(Error)





