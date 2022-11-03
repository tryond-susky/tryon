# For this week it's time to start exploring your own ideas and questions in R.
  # There are at least five options in the dataset to create the following comparisons.

library(readxl)

setwd("C:/GitHub/tryon/code/Projects/Week9")
       
# (Q1 - 12 pts) Use the dataset from the tutorial to complete one redundancy analysis (RDA) with variance partitioning on a different community (NOT the nematodes).
  # Explain the ecological importance of your significant predictor variables, or the importance if none are significant for your community.
abiotic.tibble <- read_excel("Penaetal_2016_data.xlsx", sheet = "Abiotic factors")
abiotic <- as.data.frame(abiotic.tibble)
head(abiotic)

invert.tibble <- read_excel("Penaetal_2016_data.xlsx", sheet = "Invertebrate_community")
invert <- as.data.frame(invert.tibble)
head(invert)

abiotic.names <- paste(abiotic$Parcel, abiotic$Land_Use)
abiotic$names <- abiotic.names

invert.names <- paste(invert$Parcel, invert$Land_Use)
invert$names <- invert.names

abiotic.means <- aggregate(x = abiotic, by = list(abiotic$names), FUN = "mean")
head(abiotic.means)

invert.means <- aggregate(x = invert, by = list(invert$names), FUN = "mean")
head(invert.means)

invert.means1 <- invert.means[-5,c(-1:-3,-73)]
invert.means2 <- as.data.frame(sapply(invert.means1, as.numeric ))

abiotic.means1 <- abiotic.means[,c(-1,-2,-3,-5,-6,-16)]
abiotic.means2 <- as.data.frame(sapply(abiotic.means1, as.numeric ))

#RDA
library(vegan)
colnames(abiotic.means2)
ord <- rda(invert.means2 ~ pH + totalN + Perc_ash + Kalium + Magnesium + Ca + Al + TotalP + OlsenP, abiotic.means2)
plot(ord)
anova(ord)

ord.int <- rda(invert.means2 ~1, abiotic.means2)
step.mod <- ordistep(ord.int, scope = formula(ord), selection = "both")
step.mod$anova

step.R2mod <- ordiR2step(ord.int, scope = formula(ord), selection = "forward")

#Our RDA is looking at data across matrices. While at times making these comparison on a large model is helpful in interpreting however, for this 
#model none of the varibles are signifigant/ tell us much of anything. We simply are unable to predict a pattern and must conduct more methods including that os linear models


# (Q2 - 12 pts) Then use the dataset from the tutorial to create a linear model related to your RDA. Try multiple predictors to find the best fit model.
  # Explain the ecological importance of the significant predictors, or lack of significant predictors.
#Looking at the results from our linear models we were able to separte spefic variables out and interpret signifigance. All of the AIC values were
#pretty similar with only minor decreases with Kalium and Total Phorpurus. These tell us that the variables are all fitted to the data quite well.

colnames(invert.means2)

mod1 <- lm(invert.means2$Curculionoidea ~ pH ,abiotic.means2)
AIC(mod1)

mod2 <- lm(invert.means2$Curculionoidea ~ totalN ,abiotic.means2)
AIC(mod2)

mod3 <- lm(invert.means2$Curculionoidea ~ Kalium ,abiotic.means2)
AIC(mod3)

mod4 <- lm(invert.means2$Curculionoidea ~ Magnesium ,abiotic.means2)
AIC(mod4)

mod5 <- lm(invert.means2$Curculionoidea ~ Ca , abiotic.means2)

AIC(mod5)

mod6 <- lm(invert.means2$Curculionoidea ~ Al ,abiotic.means2)
AIC(mod6)

mod7 <- lm(invert.means2$Curculionoidea ~ TotalP ,abiotic.means2)
AIC(mod7)

AIC(mod1,mod2,mod3,mod4,mod5,mod6,mod7)


# (Q3 - 6 pts) Provide a 3-4 sentence synthesis of how these results relate to one another and the value of considering both together for interpreting biotic-abiotic interactions.
#In order for an ecosystem to exist both the abiotic and biotic factors must be working together to create a productive ecosystem. If either one of these factors
#is affected then the ecosystem could collapse. To further understand the relationships I looked at the additive and interpretive factors 
#of total nitrogen and phosphorus. These both are common limiting agents in plant communities. My AIC values were lower for the interpretive 
#model which means that limits of nitrogen and phosphorus go hand and hand together. This makes sense on a larger ecological scale as invertebrate communities feed of
#of vegetation which is limited by nitrogen and phosphorus.

mod8 <- lm(invert.means2$Curculionoidea ~ totalN * TotalP,abiotic.means2)
AIC(mod8)