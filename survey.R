


library(foreign)
library(ggplot2)
library(survey)
library(knitr)
library(kableExtra)

ENSANUT.2012   <- read.dta("C:/Users/Majo/Documents/INSP/ENSANUT/ENSANUT/2012/adultos_antropometria_2012.dta") #descargar datos
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

#Cdf de los datos
cdf.imc<-svycdf(~imc, design)[[1]]

datos <- ENSANUT.2012.2$imc
n <- length(datos)

#sacar media y varianza muestral
mu <-mean(datos)
desv <- sd(datos)

#Asumiendo LogNormal
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

#Prevalencias Asumiendo LogNormal

    #Prevalencia Desnutrición

    Pr_deses <- plnorm(18.5, new[1], new[2])
    Pr_des <- cdf.imc(18.5)  

    Error_Des <- Pr_des - Pr_deses

  #Prevalencia Peso Normal

    Pr_pnes <- plnorm(25, new[1], new[2]) - plnorm(18.5, new[1], new[2])
    Pr_pn <- cdf.imc(25) - cdf.imc(18.5)  

    Error_Pn <- Pr_pn - Pr_pnes

  #Prevalencia Sobrepreso
    Pr_spes <- plnorm(30, new[1], new[2]) - plnorm(25, new[1], new[2])
    Pr_sp <- cdf.imc(30) - cdf.imc(25)
  
    Error_Sp <- Pr_sp - Pr_spes
    
    #Prevalencia Obesidad
    Pr_obes <- 1 - plnorm(30, new[1], new[2])
    Pr_ob <- 1 - cdf.imc(30) 
    
    Error_ob <- Pr_ob - Pr_obes
    
    
    #Prevalencia Obesidad Morbida
    Pr_obmes <- 1- plnorm(40, new[1], new[2])
    Pr_obm <- 1- cdf.imc(40) 
    
    Error_obm <- Pr_obm - Pr_obmes
    
################
    
    #Asumiendo Normal
    
    ECDF1 <- pnorm(datos,mu,desv)
    
    ggplot()+
      geom_step(aes(datos,ev,color="Empirica"))+
      geom_line(aes(datos,ECDF1,color="Normal"))+
      xlab("IMC") +
      ylab("Prevalencia")
    
  
    
    #Prevalencias Asumiendo Normal
    
    #Prevalencia Desnutrición
    
    PrN_deses <- pnorm(18.5, mu, desv)
    
    ErrorN_Des <- Pr_des - PrN_deses
    
    #Prevalencia Peso Normal
    
    PrN_pnes <- plnorm(25, new[1], new[2]) - plnorm(18.5, new[1], new[2])
    
    ErrorN_Pn <- Pr_pn - PrN_pnes
    
    #Prevalencia Sobrepreso
    PrN_spes <- plnorm(30, new[1], new[2]) - plnorm(25, new[1], new[2])
    
    ErrorN_Sp <- Pr_sp - PrN_spes
    
    #Prevalencia Obesidad
    PrN_obes <- 1 - plnorm(30, new[1], new[2])
    
    ErrorN_ob <- Pr_ob - PrN_obes
    
    
    #Prevalencia Obesidad Morbida
    PrN_obmes <- 1- plnorm(40, new[1], new[2])
    
    ErrorN_obm <- Pr_obm - PrN_obmes

    ################
    
    #Asumiendo Gamma Parámetros estimados por momentos
    scaleg <- ((n-1)*desv^2)/(n*mu)
    shapeg <-  (n*mu^2)/((n-1)*desv^2) 
      
      
    ECDF2 <- pgamma(datos,shapeg,scale=scaleg)
    
    ggplot()+
      geom_step(aes(datos,ev,color="Empirica"))+
      geom_line(aes(datos,ECDF2,color="Gamma"))+
      xlab("IMC") +
      ylab("Prevalencia")
    
    
    
#Prevalencias Asumiendo Gamma
    
    #Prevalencia Desnutrición
    
    PrG_deses <- pgamma(18.5, shapeg, scale=scaleg)
    
    ErrorG_Des <- Pr_des - PrG_deses
    
    #Prevalencia Peso Normal
    
    PrG_pnes <- pgamma(25, shapeg, scale=scaleg) - pgamma(18.5, shapeg, scale=scaleg)
    
    ErrorG_Pn <- Pr_pn - PrG_pnes
    
    #Prevalencia Sobrepreso
    PrG_spes <- pgamma(30, shapeg, scale=scaleg) - pgamma(25, shapeg, scale=scaleg)
    
    ErrorG_Sp <- Pr_sp - PrG_spes
    
    #Prevalencia Obesidad
    PrG_obes <- 1 - pgamma(30, shapeg, scale=scaleg)
    
    ErrorG_ob <- Pr_ob - PrG_obes
    
    
    #Prevalencia Obesidad Morbida
    PrG_obmes <- 1- pgamma(40, shapeg, scale=scaleg)
    
    ErrorG_obm <- Pr_obm - PrG_obmes
    
    ################
    
    #Asumiendo Weibull 
    
    
    ##############
    #Tabla
    
    Tit <- c("Desnutrición", "Peso normal", "Sobrepeso", "Obesidad", "Obesidad M")
    Prev <- c(Pr_des, Pr_pn, Pr_sp, Pr_ob,Pr_obm)
    PrevLN <- c(Pr_deses, Pr_pnes, Pr_spes, Pr_obes, Pr_obmes)
    ErrorLN <- c(Error_Des,Error_Pn,Error_Sp,Error_ob,Error_obm)
    PrevN <- c(PrN_deses, PrN_pnes, PrN_spes, PrN_obes, PrN_obmes)
    ErrorN <- c(ErrorN_Des,ErrorN_Pn,ErrorN_Sp,ErrorN_ob,ErrorN_obm)
    PrevG <- c(PrG_deses, PrG_pnes, PrG_spes, PrG_obes, PrG_obmes)
    ErrorG <- c(ErrorG_Des,ErrorG_Pn,ErrorG_Sp,ErrorG_ob,ErrorG_obm)
    
    Dist <- data.frame(Prev, PrevLN ,ErrorLN, PrevN,ErrorN,PrevG, ErrorG, row.names = Tit) 
    options(knitr.table.format = "latex")
    #Necesitas tener \usepackage{booktabs}
    
    
    kable(Dist, caption = "Preevalencia según distribución", booktabs = T) %>%
      kable_styling() %>%
      add_header_above(c(" " = 1, "LogNormal" = 3, "Normal" = 2, "Gamma" = 2))
    