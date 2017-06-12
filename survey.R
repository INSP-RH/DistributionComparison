


library(foreign)
library(ggplot2)
library(survey)
library(knitr)
library(kableExtra)
library(MASS) 

ENSANUT.2012   <- read.dta("C:/Users/Majo/Documents/INSP/ENSANUT/ENSANUT/2012/adultos_antropometria_2012.dta") #descargar datos
ENSANUT.2012.2 <- ENSANUT.2012[complete.cases(ENSANUT.2012$imc),] 

#A partir de la media y la varianza se obtienen los parámetros 
# meanlog y sdlog para distribuciones lognormales

#Param.log<-function(mux,varx){
#  sd2     <- log(varx/(mux^2)+1)
#  meanlog <- log(mux)-1/2*sd2
#  sdlog   <- sqrt(sd2)
#  return(c(meanlog,sdlog))
#}

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
new<-fitdistr(datos,"log-normal")
mu1 <- new[[1]][1]
desv1 <- new[[1]][2]
ECDF <- plnorm(datos,mu1,desv1)
ev <- cdf.imc(datos)


ggplot()+
  geom_step(aes(datos,ev,color="Empirica"))+
  geom_line(aes(datos,ECDF,color="Log Normal"))+
  xlab("IMC") +
  ylab("Prevalencia")

 ggsave("Log2.jpeg", path= "C:/Users/Majo/Documents/INSP")

#Error 

Error <- abs(ECDF-ev)

ggplot()+
  geom_line(aes(datos,Error,color="Error"))+
  xlab("X") +
  ylab("Error")

#Prevalencias Asumiendo LogNormal

    #Prevalencia Desnutrición

    Pr_deses <- plnorm(18.5, mu1, desv1)
    Pr_des <- cdf.imc(18.5)  

    Error_Des <- Pr_des - Pr_deses

  #Prevalencia Peso Normal

    Pr_pnes <- plnorm(25, mu1, desv1) - plnorm(18.5, mu1, desv1)
    Pr_pn <- cdf.imc(25) - cdf.imc(18.5)  

    Error_Pn <- Pr_pn - Pr_pnes

  #Prevalencia Sobrepreso
    Pr_spes <- plnorm(30, mu1, desv1) - plnorm(25, mu1, desv1)
    Pr_sp <- cdf.imc(30) - cdf.imc(25)
  
    Error_Sp <- Pr_sp - Pr_spes
    
    #Prevalencia Obesidad
    Pr_obes <- 1 - plnorm(30, mu1, desv1)
    Pr_ob <- 1 - cdf.imc(30) 
    
    Error_ob <- Pr_ob - Pr_obes
    
    
    #Prevalencia Obesidad Morbida
    Pr_obmes <- 1- plnorm(40, mu1, desv1)
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
    
    ggsave("Normal.jpeg", path= "C:/Users/Majo/Documents/INSP")
    
    
    #Prevalencias Asumiendo Normal
    
    #Prevalencia Desnutrición
    
    PrN_deses <- pnorm(18.5, mu, desv)
    
    ErrorN_Des <- Pr_des - PrN_deses
    
    #Prevalencia Peso Normal
    
    PrN_pnes <- pnorm(25, mu, desv) - pnorm(18.5, mu, desv)
    
    ErrorN_Pn <- Pr_pn - PrN_pnes
    
    #Prevalencia Sobrepreso
    PrN_spes <- pnorm(30, mu, desv) - pnorm(25, mu, desv)
    
    ErrorN_Sp <- Pr_sp - PrN_spes
    
    #Prevalencia Obesidad
    PrN_obes <- 1 - pnorm(30, mu, desv)
    
    ErrorN_ob <- Pr_ob - PrN_obes
    
    
    #Prevalencia Obesidad Morbida
    PrN_obmes <- 1- pnorm(40, mu, desv)
    
    ErrorN_obm <- Pr_obm - PrN_obmes

    ################
    
    #Asumiendo Gamma
    pgam <- fitdistr(datos, "gamma")
    
    shapeg <- pgam[[1]][1]
    rateg <- pgam[[1]][2]
      
    ECDF2 <- pgamma(datos,shapeg,rate=rateg)
    
    ggplot()+
      geom_step(aes(datos,ev,color="Empirica"))+
      geom_line(aes(datos,ECDF2,color="Gamma"))+
      xlab("IMC") +
      ylab("Prevalencia")
    
    ggsave("Gamma.jpeg", path= "C:/Users/Majo/Documents/INSP")
    
    
#Prevalencias Asumiendo Gamma
    
    #Prevalencia Desnutrición
    
    PrG_deses <- pgamma(18.5, shapeg, rate=rateg)
    
    ErrorG_Des <- Pr_des - PrG_deses
    
    #Prevalencia Peso Normal
    
    PrG_pnes <- pgamma(25, shapeg, rate=rateg) - pgamma(18.5, shapeg, rate=rateg)
    
    ErrorG_Pn <- Pr_pn - PrG_pnes
    
    #Prevalencia Sobrepreso
    PrG_spes <- pgamma(30, shapeg, rate=rateg) - pgamma(25, shapeg, rate=rateg)
    
    ErrorG_Sp <- Pr_sp - PrG_spes
    
    #Prevalencia Obesidad
    PrG_obes <- 1 - pgamma(30, shapeg, rate=rateg)
    
    ErrorG_ob <- Pr_ob - PrG_obes
    
    
    #Prevalencia Obesidad Morbida
    PrG_obmes <- 1- pgamma(40, shapeg, rate=rateg)
    
    ErrorG_obm <- Pr_obm - PrG_obmes
    
    ################
    
    #Asumiendo Weibull 
    
    pwe <- fitdistr(datos, "weibull")
    
    shapew <- pwe[[1]][1]
    scalew <- pwe[[1]][2]
    
    ECDF3 <- pweibull(datos,shapew, scale=scalew)
    
    ggplot()+
      geom_step(aes(datos,ev,color="Empirica"))+
      geom_line(aes(datos,ECDF3,color="Weibull"))+
      xlab("IMC") +
      ylab("Prevalencia")
    
    ggsave("Weibull.jpeg", path= "C:/Users/Majo/Documents/INSP")
    
    
    #Prevalencias Asumiendo Weibull
    
    #Prevalencia Desnutrición
    
    PrW_deses <- pweibull(18.5, shapew, scale=scalew)
    
    ErrorW_Des <- Pr_des - PrG_deses
    
    #Prevalencia Peso Normal
    
    PrW_pnes <- pweibull(25, shapew, scale=scalew) - pweibull(18.5, shapew, scale=scalew)
    
    ErrorW_Pn <- Pr_pn - PrW_pnes
    
    #Prevalencia Sobrepreso
    PrW_spes <- pweibull(30, shapew, scale=scalew) - pweibull(25, shapew, scale=scalew)
    
    ErrorW_Sp <- Pr_sp - PrW_spes
    
    #Prevalencia Obesidad
    PrW_obes <- 1 - pweibull(30, shapew, scale=scalew)
    
    ErrorW_ob <- Pr_ob - PrW_obes
    
    
    #Prevalencia Obesidad Morbida
    PrW_obmes <- 1- pweibull(40, shapew, scale=scalew)
    
    ErrorW_obm <- Pr_obm - PrW_obmes
    
    
    ##############
    #Tabla
    
    Tit <- c("Desnutrición", "Peso normal", "Sobrepeso", "Obesidad", "Obesidad M")
    TitC <- c("Prevalencia", "Prevalencia", "Error","Prevalencia", "Error","Prevalencia", "Error","Prevalencia", "Error")
    Prevalencia <- c(Pr_des, Pr_pn, Pr_sp, Pr_ob,Pr_obm)
    PrevLN <- c(Pr_deses, Pr_pnes, Pr_spes, Pr_obes, Pr_obmes)
    ErrorLN <- c(Error_Des,Error_Pn,Error_Sp,Error_ob,Error_obm)
    PrevN <- c(PrN_deses, PrN_pnes, PrN_spes, PrN_obes, PrN_obmes)
    ErrorN <- c(ErrorN_Des,ErrorN_Pn,ErrorN_Sp,ErrorN_ob,ErrorN_obm)
    PrevG <- c(PrG_deses, PrG_pnes, PrG_spes, PrG_obes, PrG_obmes)
    ErrorG <- c(ErrorG_Des,ErrorG_Pn,ErrorG_Sp,ErrorG_ob,ErrorG_obm)
    PrevW <- c(PrW_deses, PrW_pnes, PrW_spes, PrW_obes, PrW_obmes)
    ErrorW <- c(ErrorW_Des,ErrorW_Pn,ErrorW_Sp,ErrorW_ob,ErrorW_obm)
    
    Dist <- data.frame(Prevalencia, PrevLN ,ErrorLN, PrevN,ErrorN,PrevG, ErrorG, PrevW, ErrorW, row.names = Tit) 
    options(knitr.table.format = "latex")
    #Necesitas tener \usepackage{booktabs}
    
    
    kable(Dist, caption = "Preevalencia según distribución supuesta", booktabs = T, col.names = TitC) %>%
      kable_styling(latex_options = c("striped", "scale_down")) %>%
      add_header_above(c(" " = 1, "Empírica" = 1, "LogNormal" = 2, "Normal" = 2, "Gamma" = 2, "Weibull" = 2))
    
    
  
    