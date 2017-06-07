


library(foreign)
library(ggplot2)
library(survey)
ENSANUT.2012   <- read.dta("C:/Users/Majo/Documents/INSP/ENSANUT/ENSANUT/2012/adultos_antropometria_2012.dta")
ENSANUT.2012.2 <- ENSANUT.2012[complete.cases(ENSANUT.2012$imc),]

#A partir de la media y la varianza se obtienen los parámetros 
# meanlog y sdlog para distribuciones lognormales

Param.log<-function(mux,varx){
  sd2     <- log(varx/(mux^2)+1)
  meanlog <- log(mux)-1/2*sd2
  sdlog   <- sqrt(sd2)
  return(c(meanlog,sdlog))
}

#Survey design
ENSANUT.2012.2$id = paste0("f",ENSANUT.2012.2$folio,"intp",ENSANUT.2012.2$intp)


design <- svydesign(id = ~id,strata = ~est_var, weights = ~pondef,PSU = ~code_upm, data = ENSANUT.2012.2)
options(survey.lonely.psu = "adjust")
cdf.imc<-svycdf(~imc, design)[[1]]


######
datos <- ENSANUT.2012.2$imc

mu <-mean(datos)
desv <- sd(datos)

new<-Param.log(mu,desv)

ECDF <- plnorm(datos,new[1],new[2])
ev <- cdf.imc(datos)


ggplot()+
  geom_step(aes(datos,ev,color="Empirica"))+
  geom_line(aes(datos,ECDF,color="Log Normal"))+
  xlab("IMC") +
  ylab("Prevalencia")


#Error 

Error <- abs(ECDF-ev)

ggplot()+
  geom_line(aes(datos,Error,color="Error"))+
  xlab("X") +
  ylab("Error")

