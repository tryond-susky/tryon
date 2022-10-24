# Read in the "Toscano_Griffen_Data.csv" data from GitHub and load the three packages we used in the tutorial this week.
# The paper these data came from is uploaded to Canvas as "Toscano&Griffen_2014_JAE..."

library(MASS)
library(MuMIn)
library(mgcv)

setwd("C:/GitHub/tryon/code/Projects/Week6")


df <- read.csv("Toscano_Griffen_Data.csv")
summary(df)

# First create models with the same (y) and method (GLMM) as the published paper, using the GLMM function from the tutorial. 
  #Create two different models using the same 3 predictor (x) variables from the dataset. (4 points each) 
    # In one model only include additive effects.
# In the other model include one interactive effect.
# Use a binomial distribution and block as a random effect in both models to match the paper's analyses. Remember ?family to find distribution names.

df$prop.cons <- df$eaten/df$prey 

#glmm additive happy model
glmm.mod2 <- glmmPQL(prop.cons~activity.level+claw.width+temperature+toadfish.cue.treatment, family = binomial, random = ~ 1 | block, data = df)
r.squaredGLMM(glmm.mod2)
plot(glmm.mod2)

#glmm interactive happy model
glmm.mod3 <- glmmPQL(prop.cons~activity.level+claw.width+temperature*toadfish.cue.treatment, family = binomial, random = ~ 1 | block, data = df)
r.squaredGLMM(glmm.mod3)
plot(glmm.mod3)

# The authors used proportional consumption of prey as the (y) in their model, but did not include this in the dataset.
  # So we are going to create it - run the following line, assuming df= your data frame (feel free to change that):
      ##see line 21

# (Q1) - The code in line 21 is performing two operations at once. What are they? (2 pts)
    ##The code in this line is first naming the specific variable prop.cons after the comparison of eaten versus prey and then adding this new column to the data frame.

# (Q2) - Did the interactive effect change which variables predict proportional consumption? How, specifically, did the results change? (5 pts)
    ##Yes the interactive effect caused all of the factors which predict proportional consumption to change. 
    ##These factors were multiplied by the toadfish cue which thus changes temperature, claw width and activity level based upon toadfish cue data.
    #Question is about the results of the model. Fewer variables are significant with interactive effects.

summary(glmm.mod2)
summary(glmm.mod3)

# (Q3) - Plot the residuals of both models. Do you think either model is a good fit? Why or why not? (3 pts)
    ##The residuals of both models do not have a good fit as there is a pattern seen. Residuals show the differences between observed and predicted values. When a 
    ##pattern is formed we now that the fit is not good since we don't want to see a pattern and instead randoness. Because of this a non-linear model like gam should be tried.'

# Re-run both models as generalized additive models instead (using gam). Then compare the AIC of both models. (4 points each)

#gam additive happy model
gam.mod1 <- gam(prop.cons~activity.level+claw.width+temperature+toadfish.cue.treatment, family = binomial, random = ~ 1 | block, data = df)
plot(gam.mod1$residuals)

#gam interactive happy model
gam.mod2 <- glmmPQL(prop.cons~activity.level*claw.width+temperature+toadfish.cue.treatment, family = binomial, random = ~ 1 | block, data = df)
plot(glmm.mod2$residuals)
#This is just rerunning the glmm model. Syntax isn't quite right in either...check tutorial.

AIC(gam.mod1, gam.mod2)
# (Q4) - Which model is a better fit? (2 pt)
    ## Since my AIC numbers are a little funky lets say the AIC for gam.mod2 turned out to be 400. Model 2 has the best fit as the AIC value is the lower of the two models.

# (Q5) - Based on the residuals of your generalized additive models, how confident are you in these results? (2 pts)
    ##Based on the residuals of the gam models I would say I am pretty confident about the interactive models data with toadfish cue 
    ##as the data hugs the line of best fit without having patterned residuals. Whereas, I don't feel very confident in the additive 
    ##model since the spread is very large and doesn't present a great comparison.
  #The fact that there is a diagonal line/pattern is a big problem...I wouldn't trust either of these. 
install.packages("caTools")
library(caTools)
read.gif("happybear.gif", frame=0, flip=FALSE, verbose=FALSE)
?image
?caTools
#Nothing happened with I ran this :(