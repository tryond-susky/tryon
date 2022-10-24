# Load the "anytime" and "ggplot2" packages to complete this week's assignment.

# Read the "Plankton_move_average" CSV in from GitHub. 
# These are data from the Great Lakes Environmental Research Laboratory plankton sampling.
setwd("C:/GitHub/tryon/code/Projects/Week7")
data <- read.csv("Plankton_move_average.csv")
install.packages("ggplot2")
library(ggplot2)
install.packages("anytime")
library(anytime)


#Used the following lines to format the date and remove NAs from the dataset: ##plot the populations of three species

data$Date <- as.Date(data$Date, origin = "0001-01-01") # Setting values to "day zero".
data <- na.omit(data)
summary(data)
#Plot these population data over time with the following code:
ggplot(data)  +
  xlab("Numeric Date") + ylab("Density Individuals")+
  geom_line(data=data, aes(Date, D.mendotae), color="black", alpha = 0.7, size=1)+
  geom_line(data=data, aes(Date, LimncalanusF+LimncalanusM), color="orange",  alpha = 0.7, size=1)+ # adding males and females together, hint: this is actually spelled Limnocalanus
  geom_line(data=data, aes(Date, Bythotrephes), color="sky blue",  alpha = 0.7, size=1)+
  geom_line(data=data, aes(Date, Bythotrephes), color="sky blue",  alpha = 0.7, size=1)+
  theme_bw() 

# Export this plot to have on hand for reference in the next section of the assignment (and upload with your script).
  
# (1) - Which species is most likely to be r-selected prey and which its primary predator? (2 pts)
  ##D.mendotae represented by the black line is most likely to be the r-selected prey with Linmncalanus represented by the orange line being the primary predator.

# What is one relationship the third species MIGHT have to the first two? (2 pts)
  ##The third species may have a commensalism relationship where the Bythotrephes benefits from the success of the other two speices but also continues do survive as the others fluctuate.
#Bit of a stretch...but no evidence you are wrong!
#Now copy/paste in the Lotka-Volterra function, plotting script, and load the "deSolve" package from the tutorial:
  install.packages("deSolve")
  library(deSolve)
  
  LotVmod <- function (Time, State, Pars) {
      with(as.list(c(State, Pars)), {
        dx = x*(alpha - beta*y)
        dy = -y*(gamma - delta*x)
        return(list(c(dx, dy)))
      })
  }  

  Pars <- c(alpha = 2, beta = 0.5, gamma = .2, delta = .6) #This is the line we will change
  State <- c(x = 10, y = 10)#Typo here I fixed
  Time <- seq(0, 100, by = 1)
  out <- as.data.frame(ode(func = LotVmod, y = State, parms = Pars, times = Time))
  
  matplot(out[,-1], type = "l", xlab = "time", ylab = "population")
  legend("topright", c("Linmncalanus", "D. mendotae"), lty = c(1,2), col = c(1,2), box.lwd = 0)
   
    
# (2) - What do alpha, beta, gamma, and delta represent in this function? (4 pts)
    ##Alpha is the rate of prey population growth. 
    ##Beta is the rate of predation. 
    ##Gamma is the rate of prey consumption, aka population stability. 
    ##Delta is the rate of prey consumption, aka predator die-off
  
# (3) - By only changing values for alpha, beta, gamma, and/or delta
# change the default parameters of the L-V model to best approximate the relationship between Limncalanus and D.mendotae, assuming both plots are on the same time scale.
  Pars <- c(alpha = 2.5, beta = 0.2, gamma = .2, delta = .6) #This is the line we will change
  State <- c(x = 10, y = 10)#Typo again...
  Time <- seq(0, 100, by = 1)
  out <- as.data.frame(ode(func = LotVmod, y = State, parms = Pars, times = Time))
  
  matplot(out[,-1], type = "l", xlab = "time", ylab = "population")
  legend("topright", c("Linmncalanus", "D. mendotae"), lty = c(1,2), col = c(1,2), box.lwd = 0)
# What are the changes you've made to alpha, beta, gamma, and delta from the default values; and what do they say in a relative sense about the plankton data? (4 pts)
    #I slightly increased alpha from 2 to 2.5 which increased the D. mendotae aka prey population growth. I also decreased beta from 0.5 to 0.2. This represents a 
    #decrease in rate of predation. I went with this since it helps maintain the predator populations and have them be most effected by the prey population increase.
    #This data shows that the (Linmncalanus) predator population relies on the increase of the (D. mendotae) preys population.
# Are there other paramenter changes that could have created the same end result? (2 pts)
    ##There are other similar parameter changes that could have resulted in a similar end result by adjusting time scales and wantnot.
  #And whatnot, eh? Be a smidge more specific?
# Export your final L-V plot with a legend that includes the appropriate genus and/or species name as if the model results were the real plankton data, 
  
# and upload with your script. (hint - remember which one is the predator and which is the prey)




