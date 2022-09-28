#Suprise Virtual Trip
install.packages("leaflet")
library(leaflet)

pts <- data.frame(Latitude = 57.4270, Longitude = 152.3531)

file <- "https://upload.wikimedia.org/wikipedia/commons/thumb/c/c6/Narrow_Cape_Formation_by_Nick_Longrich.jpg/640px-Narrow_Cape_Formation_by_Nick_Longrich.jpg"

?leaflet
leaflet() %>%
  addTiles %>%
  addCircleMarkers(data = pts, lng =~Longitude, lat = ~Latitude,
                   popup = paste0("<img src = ", file, ">"))
leaflet() %>% addPopups(57.4270, 152.3531, "Here is <b>Fossil Beach,Kodiak Alaska</b>, ISU")
lng <- function(n = 10) rnorm(n, -93.65, .01)
lat <- function(n = 10) rnorm(n, 42.0285, .01)


#Virtual Vacation

library(leaflet)
m <- leaflet() %>% addTiles()
m  

# popup
m %>% addPopups(57.4270, 152.3531, "Here is the <b>Department of Statistics</b>, ISU")


# use automatic bounds derived from lng/lat data
m <- m %>% clearBounds()

# popup
m %>% addPopups(rand_lng(), rand_lat(), "Random popups")

# marker
m %>% addMarkers(rand_lng(), rand_lat())
m %>% addMarkers(
  rand_lng(), rand_lat(), popup = paste("A random letter", sample(LETTERS, 10))
)

Rlogo <- file.path(R.home("doc"), "html", "logo.jpg")
m %>% addMarkers(
  174.7690922, -36.8523071, icon = list(
    iconUrl = Rlogo, iconSize = c(100, 76)
  ), popup = "R was born here!"
)

m %>% addMarkers(rnorm(30, 175), rnorm(30, -37), icon = list(
  iconUrl = Rlogo, iconSize = c(25, 19)
))

# circle (units in metres)
m %>% addCircles(rand_lng(50), rand_lat(50), radius = runif(50, 50, 150))

# circle marker (units in pixels)
m %>% addCircleMarkers(rand_lng(50), rand_lat(50), color = "#ff0000")
m %>% addCircleMarkers(rand_lng(100), rand_lat(100), radius = runif(100, 5, 15))

# Look at the plot and model results for our Dryad data in the tutorial. Part 1: Without knowing which points represent which groups, 
  # give one explanation for why these data might be difficult to draw spatial inferences about genes.(3 points)
# The plot net1 is more of a flowchart and shows how data are correlated among eachother but can't necessarily used analyze spatial inferences.
  #This has nothing to do with the Dryad dataset in the tutorial. There is nothing spatial about the first dataset.

  # Part 2: Despite the drawbacks, give the result or interpretation that you feel most confident in (3 points), and EXPLAIN WHY (4 points).
# From this plot we can however compare data relatively.The plot we can interpret that seq24-seq16 and seq5-seq16 are equally as far from one another and can be compared to points that are closer. 
  #This is the wrong dataset...
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
  #It's doubly better because higher R squared AND fewer predictors, i.e. the genotype model is definitely over-fitted.

# Which environment would be your very WORST choice for generating a strong yield response? (2 points)
#The Pr for EnvSargodha is signigantly higher than the other Pr values and thus has the largest amount of variation which would result in a weak yield response.
  #not "significantly"...remember this is also a stats class!

