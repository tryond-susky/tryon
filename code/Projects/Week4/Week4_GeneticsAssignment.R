
# Look at the plot and model results for our Dryad data in the tutorial. Part 1: Without knowing which points represent which groups, 
  # give one explanation for why these data might be difficult to draw spatial inferences about genes.(3 points)
# The plot net1 is more of a flowchart and shows how data are correlated among eachother but can't necessarily used analyze spatial inferences.

  # Part 2: Despite the drawbacks, give the result or interpretation that you feel most confident in (3 points), and EXPLAIN WHY (4 points).
# From this plot we can however compare data relatively.The plot we can interpret that seq24-seq16 and seq5-seq16 are equally as far from one another and can be compared to points that are closer. 

# For your scripting assignment we will use the "ge_data" data frame found in the "stability" package.
  # Install the "stability" package, load it into your R environment, and use the data() function to load the "ge_data". (2 points)
install.packages("stability")
library("stability")
data(ge_data)


# Create two linear models for Yield Response: one related to the Environment and one to the Genotype. (2 points each)
  # 'Yield Response' in this dataset is a measure of phenotype expression.
  # Hint: Look at the help file for this dataset.
?ge_data
?lm

Morgan <- lm(ge_data$Yield ~ ge_data$Env)

Thomas <- lm(ge_data$Yield ~ ge_data$Gen)


# Test the significance of both models and look at the model summary. (3 points each)
anova(Morgan)
anova(Thomas)
summary(Morgan)
summary(Thomas)

  # Which model is a better fit to explain the yield response, and WHY? (6 points)
  # Hint: Does one model seem more likely to be over-fitted?
#Yield~Environment 0.4359
#Yield~Genotype 0.1183
#The r-squared value is much closer to 1, which signifies a perfect line of best fit,  but is not overfit to achieve this value. The yield~environment is the better fit to explain the yield response as it has the r-squared value closer to 1.


# Which environment would be your very WORST choice for generating a strong yield response? (2 points)
#The Pr for EnvSargodha is signigantly higher than the other Pr values and thus has the largest amount of variation which would result in a weak yield response.