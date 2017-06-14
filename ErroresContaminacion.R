############# Errores Prevalencia #########
rm(list = ls())
set.seed(633256)

M <- 1000 #Tamaño población
n <- 100  #Tamaño muestra

s <-1000 #Tamaño simulación

MVlogn <- function(muestra){
  mu <- sum(log(muestra))/length(muestra)
  var <- sum((log(muestra)- mu)^2)/length(muestra)
  sig <- sqrt(var)
  return(c(mu,sig))
}




pop   <- sample(10:60, M, replace = TRUE) 
Freal <- ecdf(pop)

#paramv <- fitdistr(pop,"log-normal")

a     <- 18.5
b     <- 25


#Simulacion
Errores <- vector('numeric', 0)

ErroresN <- vector('numeric', 0)
ErroresLN <- vector('numeric', 0)
ErroresG <- vector('numeric', 0)
ErroresW<- vector('numeric', 0)


for (i in 1:s) {
  sim<- sample(pop, size=n*.95, replace=FALSE)
  mu <- mean(sim)
  
  #Contaminación
  cont <- rnorm(n*.05, mu, 1000)
  
  sim <- c(sim,abs(cont))
  
  mu <- mean(sim)
  sig <- sd(sim)
  
  Fn <- ecdf(sim)
  pln<- MVlogn(sim)
  pg<- fitdistr(sim,"gamma")
  pw<- fitdistr(sim,"weibull")
  
  Erres <- (Freal(b)-Freal(a))- (Fn(b) - Fn(a))
  ErresN <- (Freal(b)-Freal(a))- (pnorm(b,mu,sig) - pnorm(a,mu,sig))
  ErresLN <- (Freal(b)-Freal(a))- (plnorm(b,pln[1],pln[2]) - plnorm(a,pln[1],pln[2]))
  ErresG <- (Freal(b)-Freal(a))- (pgamma(b,pg[[1]][1],rate=pg[[1]][2]) - pgamma(a,pg[[1]][1],rate=pg[[1]][2]))
  ErresW <- (Freal(b)-Freal(a))- (pweibull(b,pw[[1]][1],scale=pw[[1]][2]) - pweibull(a,pw[[1]][1],scale=pw[[1]][2]))
  
  
  Errores <- c(Errores, Erres)
  ErroresN <- c(ErroresN,ErresN)
  ErroresLN <- c(ErroresLN, ErresLN)
  ErroresG <- c(ErroresG, ErresG)
  ErroresW <- c(ErroresW, ErresW)
}

#Esperanza del Error
sum1<- c(summary(Errores))
sum2 <- c(summary(ErroresN))
sum3 <- c(summary(ErroresLN))
sum4 <- c(summary(ErroresG))
sum5 <- c(summary(ErroresW))

Dist <- data.frame(sum1,sum2,sum3,sum4,sum5) 
options(knitr.table.format = "latex")
#Necesitas tener \usepackage{booktabs}


kable(Dist, caption = "Errores con muestras contaminadas", booktabs = T) %>%
  kable_styling(latex_options = c("striped")) %>%
  add_header_above(c(" " = 1, "Empírica" = 1, "LogNormal" = 1, "Normal" = 1, "Gamma" = 1, "Weibull" = 1))
