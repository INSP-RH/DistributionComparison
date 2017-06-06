
set.seed(633256)
library(ggplot2)
library(gridExtra) 
M <- 100 #Population size
n <- 30  


#Poblacion y su empirica
pop   <- sample(0:100, M, replace = TRUE) 
Freal <- ecdf(pop)

t     <- seq(0, 60, length.out = n)

cota1 <- NULL
cota2 <- NULL
for(i in 1:length(t)){
  cota1[i] <-  minAccumulatedError(t[i], pop, "norm")
}

for(i in 1:length(t)){
  cota2[i] <-  minAccumulatedError2(t[i], pop, "norm")
}

mupop <- mean(pop)
sdpop <- sd(pop)
Epuntual<- abs(Freal(t) - pnorm(t,mupop,sdpop))

#acum <- as.numeric(integrate(function(t){pnorm(t,mupop,sdpop)},-Inf, pop[1])[1])

Eacum <- NULL
Eacum[1] <- Epuntual[1] #+acum
for(i in 2:length(Epuntual)){
  Eacum[i] <- Eacum[i-1] + Epuntual[i]
}

ggplot()+
  geom_line(aes(t,cota1,color="Cota1"))+
  geom_line(aes(t,cota2,color="Cota2"))+
  geom_line(aes(t, Eacum, color = "Real"))+
  xlab("X") +
  ylab("Error Acumulado")
