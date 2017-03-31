#Distribution comparison
#María José Benítez / Rodrigo Zepeda / Dalia Camacho García
#INSP 2017

#Description
#----------------------------
#Script that generates a random population and then estimates the error
#as well as the lower and upper bounds for the error when comparing
#the empirical distribution with a lognormal one.

#Setup
set.seed(633256)
library(ggplot2)
library(gridExtra) 
M <- 100 #Population size

#Simulate the population
pop   <- sample(0:50, M, replace = TRUE)
t     <- seq(-10, 60, length.out = 100)

#Build the cummulative density function for real and normal cases
Freal <- ecdf(pop)    
Fnorm <- function(x){pnorm(x, mean(pop), sd(pop))}

#Create data frame for plot
plotdata <- data.frame(t, Freal(t), Fnorm(t), 
                       Freal(t) - Fnorm(t))
colnames(plotdata) <- c("t","Real","Normal","Error_Normal")
plotdata$Cumerror <- abs(plotdata$Error)
for (i in 2:length(t)){
  plotdata$Cumerror[i] <- abs(plotdata$Error) +  
    plotdata$Cumerror[i-1]
}

#Plot the distributions
plot1 <- ggplot(plotdata, aes(x = t)) + 
            geom_line(aes(y = abs(Error)), color = "forestgreen") +
            ggtitle("Absolute point error of real vs normal") +
            theme_bw()
plot2 <- ggplot(plotdata, aes(x = t)) + 
            geom_line(aes(y = Cumerror), color = "orange") +
            ggtitle("Cummulative point error of real vs normal") +
            theme_bw()
plot3 <- ggplot(plotdata, aes(x = t)) + 
            geom_step(aes(y = Real), color = "red") +
            geom_line(aes(y = Normal), color = "deepskyblue4") +
            theme_bw() +
            ggtitle("Real vs Normal")

grid.arrange(plot1, plot2, plot3)


