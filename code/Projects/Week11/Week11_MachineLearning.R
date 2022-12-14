# Install package
install.packages("image.darknet", repos = "https://bnosac.github.io/drat")
# Load package
library(image.darknet)

# Set working directory 
setwd("C:/GitHub/R4Eco_2022/Week11")


# Detect - where are the objects?
#First select the model for identifying objects:
yolo_tiny_voc <- image_darknet_model(type = 'detect', 
                                     model = "tiny-yolo-voc.cfg", 
                                     weights = system.file(package="image.darknet", "models", "tiny-yolo-voc.weights"), 
                                     labels = system.file(package="image.darknet", "include", "darknet", "data", "voc.names"))

#Then test against specific images:
image_darknet_detect(file = "C:/GitHub/R4Eco_2022/Week11/BearSalmon2.jpg",
                     object = yolo_tiny_voc)

image_darknet_detect(file = "C:/GitHub/R4Eco_2022/Week11/BearSalmon.jpg",
                     object = yolo_tiny_voc)

image_darknet_detect(file = "C:/GitHub/R4Eco_2022/Week11/Bird.jpg",
                     object = yolo_tiny_voc)

image_darknet_detect(file = "C:/GitHub/R4Eco_2022/Week11/Urban.jpg",
                     object = yolo_tiny_voc)

#Lowering the threshold for confidence in detection:
image_darknet_detect(file = "C:/GitHub/R4Eco_2022/Week11/Urban.jpg",
                     object = yolo_tiny_voc,
                     threshold = .05)


# Classify - what is the most likely critter?
#Notice a better answer, but there is no confidence from the model.
tiny_model <- image_darknet_model(type = "classify",
                                  model = "tiny.cfg",
                                  weights = system.file(package = "image.darknet", "models", "tiny.weights"),
                                  labels = system.file(package = "image.darknet", "include", "darknet", "data", "imagenet.shortnames.list"))
image_darknet_classify(file = "C:/GitHub/R4Eco_2022/Week11/BearSalmon2.jpg",
                       object = tiny_model)
