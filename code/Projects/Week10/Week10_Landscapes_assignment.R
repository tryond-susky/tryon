# Load the packages from this week's tutorial.
#In the tutorial we looked at the community as a whole and the swimmers which ultimately matched a prediction we had for their distribution.

setwd("C:/GitHub/tryon/code/Projects/Week10")
library(spdep)
library(adespatial)
library(vegan)

#Part 1: Look at two other subsets of the community and determine the relative influence of space and habitat on each following the methods in the tutorial. (10 points)
#The options include groupings by taxonomy, where Diptera (true flies) have the strongest flight ability, Trichoptera the 2nd strongest, 
    #Ephememeroptera are 3rd, and non insects are 4th...because they don't have wings.
#Groupings by habits include the swimmers (off limits for the assignment) as most mobile, sprawlers as 2nd (they move in search of food, but not quickly),
    #and the clingers come in last (they might move up and down on individual rocks).

HabitatbyPatch.csv <- read.csv("HabitatbyPatch.csv", header=T)
PatchLatLon.csv <- read.csv("PatchLatLon.csv")
PatchLatLon.mat <- as.matrix(PatchLatLon.csv[,-1])
HabitatbyPatch.mat <- as.matrix(HabitatbyPatch.csv)

Clingers.csv <- read.csv ("Clingers.csv", header=T)
Clingers.mat <-as.matrix(Clingers.csv)

Ephemeroptera.csv <- read.csv("Ephemeroptera.csv")
Ephemeroptera.mat <-as.matrix(Ephemeroptera.csv)


nb<-cell2nb(3,30,"queen")
nb1 <- droplinks(nb, 19, sym=TRUE)
nb2 <- droplinks(nb1, 22, sym=TRUE)
nb3 <- droplinks(nb2, 25, sym=TRUE)
nb4 <- droplinks(nb3, 30, sym=TRUE)


bin.mat <- aem.build.binary(nb4, PatchLatLon.mat, unit.angle = "degrees", rot.angle = 90, rm.same.y = TRUE, plot.connexions = TRUE)
plot(PatchLatLon.mat[,2]~PatchLatLon.mat[,3], xlim = rev(c(76.75,77)))

aem.ev <- aem(aem.build.binary=bin.mat)
aem.df <- aem.ev$vectors[c(-19,-22,-25,-30),]
aem.df

#We will use forward selection in this case because of the sheer number of variables.
Space.rda <- rda(Clingers.mat, as.data.frame(aem.df))
Space.r2a <- RsquareAdj(Space.rda)$adj.r.squared

aem.fwd <- forward.sel(Clingers.mat,aem.df, adjR2thresh=Space.r2a)

aem.fwd$order

#space controlling for habitat HIGHER R SQUARED
SpaceNoHab.rda <- rda(Clingers.mat, as.data.frame(aem.df[,aem.fwd$order]), HabitatbyPatch.mat)
SpaceNoHab.rda 
anova(SpaceNoHab.rda, perm.max = 10000)
RsquareAdj(SpaceNoHab.rda)

#habitat controlling for space:
HabNoSpace.rda <- rda(Clingers.mat, HabitatbyPatch.mat, as.data.frame(aem.df[,aem.fwd$order]))
HabNoSpace.rda 
anova(HabNoSpace.rda, perm.max = 10000)
RsquareAdj(HabNoSpace.rda)


#Part 2: What is your interpretation of the pattern for each group individually, and the two in comparison, based on their mobility? (5 points)

#The species we are looking at are relatively stationary which is observed with the higher adjusted r-square values and constrained  for space controlling 
#for habitat which shows how space and distance is more influential than differences in habitat. This also shows how space and habitat varaibiliy can act 
#independantly of eachother on a community.

#Part 3: For each of your chosen groups of bugs, perform variable selection for the habitat data rather than the AEM data. Which habitat variables are significant for each? (10 points)
  # Definitions for the habitat column names:
    #Inorg = total suspended inorganic solids in the water column
    #Organ = total suspended organic solids in the water column
    #Chla = Chlorophyll a concentration from benthic rocks collected in-stream
    #BOM = total benthic organic matter in the sample
    #Depth = water depth
    #Flow	= water velocity where samples were collected
    #Fines = Percent of the substrate as "fines" i.e. small particles too small to measure
    #AveAr = The average size of rocks where each sample was collected

Clingers.rda <- rda(HabitatbyPatch.mat, as.data.frame(aem.df))
Clingers.r2a <- RsquareAdj(Clingers.rda)$adj.r.squared

aem.fwd <- forward.sel(HabitatbyPatch.mat,aem.df, adjR2thresh=Space.r2a)

Ephemeroptera.rda <- rda(HabitatbyPatch.mat, as.data.frame(aem.df))
Ephemeroptera.r2a <- RsquareAdj(Ephemeroptera.rda)$adj.r.squared

aem.fwd <- forward.sel(HabitatbyPatch.mat,aem.df, adjR2thresh=Space.r2a)

#Ephemeroptera had more signifigant variables than Clingers.


#Part 4: How do you expect selecting both the spatial and the habitat variables would change the results of the RDAs from Part 1 above? (5 points)
  #(You do not need to redo the RDAs, unless you *want* to.)

#I think by selecting both the spatial and the habitat variables the RDAs would have higher conditional proportions as they would interact. In community terms this means that both habitat and space would be important.