# /////////////////// Project 3: STQD6114 //////////////////////

#groupmate : hilman (p121535) ; mardhiah (119717)

# ///////////////// part 1 : image analysis (hilman) ///////////////

library(imager)
#library("tidyverse")
library(dplyr)
library(ggplot2)
#install.packages("rlang")
#update.packages()
#remove.packages("ggplot2")
#install.packages('ggplot2', dependencies = TRUE)

# load image 
flower1 <- load.image("C:/Users/User/Desktop/STQD6114_ unstructured data/project_3/Flower1.jpeg")
flower2 <- load.image("C:/Users/User/Desktop/STQD6114_ unstructured data/project_3/Flower2.jpeg")
scenery1 <- load.image("C:/Users/User/Desktop/STQD6114_ unstructured data/project_3/Scenery1.jpeg")
scenery2 <- load.image("C:/Users/User/Desktop/STQD6114_ unstructured data/project_3/Scenery2.jpeg")
person1 <- load.image("C:/Users/User/Desktop/STQD6114_ unstructured data/project_3/Person1.jpg")
person2 <- load.image("C:/Users/User/Desktop/STQD6114_ unstructured data/project_3/Person2.jpeg")
building1 <- load.image("C:/Users/User/Desktop/STQD6114_ unstructured data/project_3/Building1.jpg")
building2 <- load.image("C:/Users/User/Desktop/STQD6114_ unstructured data/project_3/Building2.jpg")
animal1 <- load.image("C:/Users/User/Desktop/STQD6114_ unstructured data/project_3/Animal1.jpg")
animal2 <- load.image("C:/Users/User/Desktop/STQD6114_ unstructured data/project_3/Animal2.jpg")

# plotting image
plot(flower1)
plot(flower2)
plot(scenery1)
plot(scenery2)
plot(person1)
plot(person2)
plot(building1)
plot(building2)
plot(animal1)
plot(animal2)

# i) edge detection
?deriche
deriche(flower1,2,order=1,axis="x")%>% plot(main = "Edge detector along x-axis") #Edge detector along x-axis
deriche(flower1,2,order=1,axis="y")%>% plot(main = "Edge detector along y-axis") #Edge detector along y-axis


deriche(flower2,2,order=1,axis="x")%>% plot(main = "Edge detector along x-axis") #Edge detector along x-axis
deriche(flower2,2,order=1,axis="y")%>% plot(main = "Edge detector along y-axis") #Edge detector along y-axis

deriche(scenery1,2,order=1,axis="x")%>% plot(main = "Edge detector along x-axis") #Edge detector along x-axis
deriche(scenery1,2,order=1,axis="y")%>% plot(main = "Edge detector along y-axis") #Edge detector along y-axis

deriche(scenery2,2,order=1,axis="x")%>% plot(main = "Edge detector along x-axis") #Edge detector along x-axis
deriche(scenery2,2,order=1,axis="y")%>% plot(main = "Edge detector along y-axis") #Edge detector along y-axis

deriche(person1,2,order=1,axis="x")%>% plot(main = "Edge detector along x-axis") #Edge detector along x-axis
deriche(person1,2,order=1,axis="y")%>% plot(main = "Edge detector along y-axis") #Edge detector along y-axis

deriche(person2,2,order=1,axis="x")%>% plot(main = "Edge detector along x-axis") #Edge detector along x-axis
deriche(person2,2,order=1,axis="y")%>% plot(main = "Edge detector along y-axis") #Edge detector along y-axis

deriche(building1,2,order=1,axis="x")%>% plot(main = "Edge detector along x-axis") #Edge detector along x-axis
deriche(building1,2,order=1,axis="y")%>% plot(main = "Edge detector along y-axis") #Edge detector along y-axis

deriche(building2,2,order=1,axis="x")%>% plot(main = "Edge detector along x-axis") #Edge detector along x-axis
deriche(building2,2,order=1,axis="y")%>% plot(main = "Edge detector along y-axis") #Edge detector along y-axis

deriche(animal1,2,order=1,axis="x")%>% plot(main = "Edge detector along x-axis") #Edge detector along x-axis
deriche(animal1,2,order=1,axis="y")%>% plot(main = "Edge detector along y-axis") #Edge detector along y-axis

deriche(animal2,2,order=1,axis="x")%>% plot(main = "Edge detector along x-axis") #Edge detector along x-axis
deriche(animal2,2,order=1,axis="y")%>% plot(main = "Edge detector along y-axis") #Edge detector along y-axis

# ii) splitting & concatenating image
?imsplit
?imappend
# imsplit to split (data, "c")
# concat imappend

# !!! note, it wanna rerun and encounter issue, maybe need to run the imappend code first to concat the images back, then can plot the implit image again

# flower1
#split image into 3 individual colours channels
imsplit(flower1, "c")%>%plot() # "c" argument will produce a list of containing all individual colour channels
imsplit(flower1, "x",3)%>%plot()
# Concatenate the split images horizontally
imappend(imsplit(flower1, "c"), "x")%>%plot()
# Concatenate the split images vertically
imappend(imsplit(flower1, "c"), "y")%>%plot()
# Concatenate the split images back to original
imappend(imsplit(flower1, "c"), "c")%>%plot()

#flower2
#split image into 3 individual colours channels
imsplit(flower2, "c")%>%plot() # "c" argument will produce a list of containing all individual colour channels
imsplit(flower2, "x",3)%>%plot()
# Concatenate the split images horizontally
imappend(imsplit(flower2, "c"), "x")%>%plot()
# Concatenate the split images vertically
imappend(imsplit(flower2, "c"), "y")%>%plot()
# Concatenate the split images back to original
imappend(imsplit(flower2, "c"), "c")%>%plot()

#scenery1
#split image into 3 individual colours channels
imsplit(scenery1, "c")%>%plot() # "c" argument will produce a list of containing all individual colour channels
imsplit(scenery1, "x",3)%>%plot()
# Concatenate the split images horizontally
imappend(imsplit(scenery1, "c"), "x")%>%plot()
# Concatenate the split images vertically
imappend(imsplit(scenery1, "c"), "y")%>%plot()
# Concatenate the split images back to original
imappend(imsplit(scenery1, "c"), "c")%>%plot()

#scenery2
#split image into 3 individual colours channels
imsplit(scenery2, "c")%>%plot() # "c" argument will produce a list of containing all individual colour channels
imsplit(scenery2, "x",3)%>%plot()
# Concatenate the split images horizontally
imappend(imsplit(scenery2, "c"), "x")%>%plot()
# Concatenate the split images vertically
imappend(imsplit(scenery2, "c"), "y")%>%plot()
# Concatenate the split images back to original
imappend(imsplit(scenery2, "c"), "c")%>%plot()

#person1
#split image into 3 individual colours channels
imsplit(person1, "c")%>%plot() # "c" argument will produce a list of containing all individual colour channels
imsplit(person1, "x",3)%>%plot() 
# Concatenate the split images horizontally
imappend(imsplit(person1, "c"), "x")%>%plot()
# Concatenate the split images vertically
imappend(imsplit(person1, "c"), "y")%>%plot()
# Concatenate the split images back to original
imappend(imsplit(person1, "c"), "c")%>%plot()

#person2
#split image into 3 individual colours channels
imsplit(person2, "c")%>%plot() # "c" argument will produce a list of containing all individual colour channels
imsplit(person2, "x",3)%>%plot()
# Concatenate the split images horizontally
imappend(imsplit(person2, "c"), "x")%>%plot()
# Concatenate the split images vertically
imappend(imsplit(person2, "c"), "y")%>%plot()
# Concatenate the split images back to original
imappend(imsplit(person2, "c"), "c")%>%plot()

#building1
#split image into 3 individual colours channels
imsplit(building1, "c")%>%plot() # "c" argument will produce a list of containing all individual colour channels
imsplit(building1, "x",3)%>%plot()
# Concatenate the split images horizontally
imappend(imsplit(building1, "c"), "x")%>%plot()
# Concatenate the split images vertically
imappend(imsplit(building1, "c"), "y")%>%plot()
# Concatenate the split images back to original
imappend(imsplit(building1, "c"), "c")%>%plot()

#building2
#split image into 3 individual colours channels
imsplit(building2, "c")%>%plot() # "c" argument will produce a list of containing all individual colour channels
imsplit(building2, "x",3)%>%plot()
# Concatenate the split images horizontally
imappend(imsplit(building2, "c"), "x")%>%plot()
# Concatenate the split images vertically
imappend(imsplit(building2, "c"), "y")%>%plot()
# Concatenate the split images back to original
imappend(imsplit(building2, "c"), "c")%>%plot()
# no split, coz i think the image has no distinctive channels to split

#animal1
#split image into 3 individual colours channels
imsplit(animal1, "c")%>%plot() # "c" argument will produce a list of containing all individual colour channels
imsplit(animal1, "x",3)%>%plot()
# Concatenate the split images horizontally
imappend(imsplit(animal1, "c"), "x")%>%plot()
# Concatenate the split images vertically
imappend(imsplit(animal1, "c"), "y")%>%plot()
# Concatenate the split images back to original
imappend(imsplit(animal1, "c"), "c")%>%plot()

#animal2
#split image into 3 individual colours channels
imsplit(animal2, "c")%>%plot() # "c" argument will produce a list of containing all individual colour channels
imsplit(animal2, "x",3)%>%plot()
# Concatenate the split images horizontally
imappend(imsplit(animal2, "c"), "x")%>%plot()
# Concatenate the split images vertically
imappend(imsplit(animal2, "c"), "y")%>%plot()
# Concatenate the split images back to original
imappend(imsplit(animal2, "c"), "c")%>%plot()


# iii) image transformation (eg: resizing, rotation, scaling & cropping)

# if encounter issues in installing magick packages
#https://cran.r-project.org/web/packages/magick/index.html  
#( download manually from this website, choose r-release if use the latest stable R version)
#( download manually from this website, choose r-oldrel if use the older R version)
#install.packages("C:/Users/User/Documents/magick_2.7.4.zip", repos = NULL)

#install.packages("magick")
library("magick")
#?image_crop

Flower1 <- image_read("C:/Users/User/Desktop/STQD6114_ unstructured data/project_3/Flower1.jpeg")
plot(Flower1)
image_scale(Flower1, "300x200!")%>% plot()  # Replace "300x200" with your desired dimensions scales
image_crop(Flower1, "400x400+200+200")%>% plot() #note, Flower1 & flower1 are same image, just Flower1 is another variable for the purpose of using function image_Scale & image_crop
resize(flower1,-30,-10)%>%plot()
imrotate(flower1,30) %>% plot(main="Rotating")

Flower2 <- image_read("C:/Users/User/Desktop/STQD6114_ unstructured data/project_3/Flower2.jpeg")
plot(Flower2)
image_scale(Flower2, "300x200!")%>% plot()  # Replace "300x200" with your desired dimensions scales
image_crop(Flower2, "700x700+200+200")%>% plot()
resize(flower2,-10,-20)%>%plot()
imrotate(flower2,30) %>% plot(main="Rotating")

Scenery1 <- image_read("C:/Users/User/Desktop/STQD6114_ unstructured data/project_3/Scenery1.jpeg")
plot(Scenery1)
image_scale(Scenery1, "200x50!")%>% plot()  # Replace "300x200" with your desired dimensions scales
image_crop(Scenery1, "300x300+100+100")%>% plot()
resize(scenery1,-10,-30)%>%plot()
imrotate(scenery1,30) %>% plot(main="Rotating")

Scenery2 <- image_read("C:/Users/User/Desktop/STQD6114_ unstructured data/project_3/Scenery2.jpeg")
plot(Scenery2)
image_scale(Scenery2, "200x50!")%>% plot()  # Replace "300x200" with your desired dimensions scales
image_crop(Scenery2, "200x200+100+100")%>% plot()
resize(scenery2,-10,-30)%>%plot()
imrotate(scenery2,30) %>% plot(main="Rotating")

Person1 <- image_read("C:/Users/User/Desktop/STQD6114_ unstructured data/project_3/Person1.jpg")
plot(Person1)
image_scale(Person1, "50x200!")%>% plot()  # Replace "300x200" with your desired dimensions scales
image_crop(Person1, "200x200+110+150")%>% plot()
resize(person1,-10,-30)%>%plot()
imrotate(person1,30) %>% plot(main="Rotating")

Person2 <- image_read("C:/Users/User/Desktop/STQD6114_ unstructured data/project_3/Person2.jpeg")
plot(Person2)
image_scale(Person2, "50x200!")%>% plot()  # Replace "300x200" with your desired dimensions scales
image_crop(Person2, "150x150+80+30")%>% plot()
resize(person2,-10,-30)%>%plot()
imrotate(person2,30) %>% plot(main="Rotating")

Building1 <- image_read("C:/Users/User/Desktop/STQD6114_ unstructured data/project_3/Building1.jpg")
plot(Building1)
image_scale(Building1, "200x100!")%>% plot()  # Replace "300x200" with your desired dimensions scales
image_crop(Building1, "650x650+700+600")%>% plot()
resize(building1,-10,-30)%>%plot()
imrotate(building1,30) %>% plot(main="Rotating")

Building2 <- image_read("C:/Users/User/Desktop/STQD6114_ unstructured data/project_3/Building2.jpg")
plot(Building2)
image_scale(Building2, "200x100!")%>% plot()  # Replace "300x200" with your desired dimensions scales
image_crop(Building2, "500x500+400+200")%>% plot()
resize(building2,-10,-30)%>%plot()
imrotate(building2,30) %>% plot(main="Rotating")

Animal1 <- image_read("C:/Users/User/Desktop/STQD6114_ unstructured data/project_3/Animal1.jpg")
plot(Animal1)
image_scale(Animal1, "100x100!")%>% plot()  # Replace "300x200" with your desired dimensions scales
image_crop(Animal1, "500x500+300+100")%>% plot()
resize(animal1,-10,-30)%>%plot()
imrotate(animal1,30) %>% plot(main="Rotating")

Animal2 <- image_read("C:/Users/User/Desktop/STQD6114_ unstructured data/project_3/Animal2.jpg")
plot(Animal2)
image_scale(Animal2, "100x200!")%>% plot()  # Replace "300x200" with your desired dimensions scales
image_crop(Animal2, "300x300+200+50")%>% plot()
resize(animal2,-10,-30)%>%plot()
imrotate(animal2,30) %>% plot(main="Rotating")

# iv) filtering image

# REMINDER, NEED TO RUN (iii) CODE FIRST BECAUSE THE VARIABLE USED REQUIRED MAGICK LIBRARY WAY OF LOADING IMAGE
# IF ENCOUNTER ISSUE RUNNING THE CODE, TRY TO LOAD THE IMAGE AGAIN IN PART (iii)

#morphology filtering ( thinning to skeleton & dilate image)
?image_morphology()

Flower1%>% image_morphology("Thinning", "Skeleton") %>% image_scale("100%")  # thinning down to a skeleton
Flower1%>% image_morphology("Dilate", "Diamond") %>% image_scale("100%")  #dilate the image

Flower2%>% image_morphology("Thinning", "Skeleton") %>% image_scale("60%")  # thinning down to a skeleton
Flower2%>% image_morphology("Dilate", "Diamond") %>% image_scale("60%")  #dilate the image

Scenery1%>% image_morphology("Thinning", "Skeleton") %>% image_scale("200%")  # thinning down to a skeleton
Scenery1%>% image_morphology("Dilate", "Diamond") %>% image_scale("200%")  #dilate the image

Scenery2%>% image_morphology("Thinning", "Skeleton") %>% image_scale("200%")  # thinning down to a skeleton
Scenery2%>% image_morphology("Dilate", "Diamond") %>% image_scale("200%")  #dilate the image

Person1%>% image_morphology("Thinning", "Skeleton") %>% image_scale("200%")  # thinning down to a skeleton
Person1%>% image_morphology("Dilate", "Diamond") %>% image_scale("200%")  #dilate the image

Person2%>% image_morphology("Thinning", "Skeleton") %>% image_scale("200%")  # thinning down to a skeleton
Person2%>% image_morphology("Dilate", "Diamond") %>% image_scale("200%")  #dilate the image

Building1%>% image_morphology("Thinning", "Skeleton") %>% image_scale("200%")  # thinning down to a skeleton
Building1%>% image_morphology("Dilate", "Diamond") %>% image_scale("200%")  #dilate the image

Building2%>% image_morphology("Thinning", "Skeleton") %>% image_scale("200%")  # thinning down to a skeleton
Building2%>% image_morphology("Dilate", "Diamond") %>% image_scale("200%")  #dilate the image

Animal1%>% image_morphology("Thinning", "Skeleton") %>% image_scale("200%")  # thinning down to a skeleton
Animal1%>% image_morphology("Dilate", "Diamond") %>% image_scale("200%")  #dilate the image

Animal2%>% image_morphology("Thinning", "Skeleton") %>% image_scale("200%")  # thinning down to a skeleton
Animal2%>% image_morphology("Dilate", "Diamond") %>% image_scale("200%")  #dilate the image

# v) rectangular, circular & fuzzy selection
# add rectangular, circle and fuzzy shape onto the image

# //////////////////////////////////////////////////////////////////////////////////////////
#install.packages("BiocManager") 
#BiocManager::install("EBImage")
#library(EBImage)

# Crop the image
#?image_crop
#cropped_image<-image_crop(Flower1, geometry = sprintf("%dx%d+%d+%d", width=400, height=350, x=200, y=150))
#cropped_image
#flower_circle <- as.cimg(flower1)
#draw_rect(flower1, 1,1,50,50, "red")%>%plot

#draw_rect(flower1, 1, 1, 50, 50, "red") 

#?image_annotate
##
# Display the image
#plot(flower1)

# selecting rectangular region on image plot
#x <- 100
#y <- 100
#width <- 200
#height <- 150
#rect(x, y, x + width, y + height, border = "red", lwd = 2)


# draw circle on image
# Define the center and radius of the circle
#center_x <- 0.7
#center_y <- 0.7
#radius <- 0.2

# Generate the coordinates for the approximate circle
#num_sides <- 1000
#theta <- seq(0, 2 * pi, length.out = num_sides)
#x <- center_x + radius * cos(theta)
#y <- center_y + radius * sin(theta)

# Draw the approximate circle as a polygon
#polygon(x, y, border = "blue", lwd = 2)

# //////////////////////////////////////////////////////////////////////////////////////////

# flower1
# Plot the image flower1
plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1), xlab = "", ylab = "", asp = 1)
rasterImage(flower1, 0, 0, 1, 1)

# Draw a circle
symbols(0.5, 0.5, circles = 0.2, inches = FALSE, add = TRUE, bg = "transparent", fg = "yellow", lwd = 2)

# Draw a rectangle
rect(xleft = 0.2, ybottom = 0.2, xright = 0.5, ytop = 0.5, border = "red", lwd = 2)

# Draw the fuzzy shape
# Create a matrix of coordinates for the polygon fuzzy shape
coords <- matrix(c(x=c(0.1, 0.3, 0.5, 0.7, 0.9), y=c(0.8, 0.6, 0.4, 0.2, 0.8)), ncol = 2, byrow = TRUE)

# Display fuzzy shape
polygon(coords, border = "green")


# flower2
# Plot the image flower2
plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1), xlab = "", ylab = "", asp = 1)
rasterImage(flower2, 0, 0, 1, 1)

# Draw a circle
symbols(0.5, 0.5, circles = 0.2, inches = FALSE, add = TRUE, bg = "transparent", fg = "yellow", lwd = 2)

# Draw a rectangle
rect(xleft = 0.2, ybottom = 0.2, xright = 0.5, ytop = 0.5, border = "red", lwd = 2)

# Draw the fuzzy shape
# Create a matrix of coordinates for the polygon fuzzy shape
coords <- matrix(c(x=c(0.1, 0.3, 0.5, 0.7, 0.9), y=c(0.8, 0.6, 0.4, 0.2, 0.8)), ncol = 2, byrow = TRUE)

# Display fuzzy shape
polygon(coords, border = "green")


# scenery1
# Plot the image scenery1
plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1), xlab = "", ylab = "", asp = 1)
rasterImage(scenery1, 0, 0, 1, 1)

# Draw a circle
symbols(0.5, 0.5, circles = 0.2, inches = FALSE, add = TRUE, bg = "transparent", fg = "yellow", lwd = 2)

# Draw a rectangle
rect(xleft = 0.2, ybottom = 0.2, xright = 0.5, ytop = 0.5, border = "red", lwd = 2)

# Draw the fuzzy shape
# Create a matrix of coordinates for the polygon fuzzy shape
coords <- matrix(c(x=c(0.1, 0.3, 0.5, 0.7, 0.9), y=c(0.8, 0.6, 0.4, 0.2, 0.8)), ncol = 2, byrow = TRUE)

# Display fuzzy shape
polygon(coords, border = "green")


# scenery2
# Plot the image scenery2
plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1), xlab = "", ylab = "", asp = 1)
rasterImage(scenery2, 0, 0, 1, 1)

# Draw a circle
symbols(0.5, 0.5, circles = 0.2, inches = FALSE, add = TRUE, bg = "transparent", fg = "yellow", lwd = 2)

# Draw a rectangle
rect(xleft = 0.2, ybottom = 0.2, xright = 0.5, ytop = 0.5, border = "red", lwd = 2)

# Draw the fuzzy shape
# Create a matrix of coordinates for the polygon fuzzy shape
coords <- matrix(c(x=c(0.1, 0.3, 0.5, 0.7, 0.9), y=c(0.8, 0.6, 0.4, 0.2, 0.8)), ncol = 2, byrow = TRUE)

# Display fuzzy shape
polygon(coords, border = "green")


# person1
# Plot the image person1
plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1), xlab = "", ylab = "", asp = 1)
rasterImage(person1, 0, 0, 1, 1)

# Draw a circle
symbols(0.5, 0.5, circles = 0.2, inches = FALSE, add = TRUE, bg = "transparent", fg = "yellow", lwd = 2)

# Draw a rectangle
rect(xleft = 0.2, ybottom = 0.2, xright = 0.5, ytop = 0.5, border = "red", lwd = 2)

# Draw the fuzzy shape
# Create a matrix of coordinates for the polygon fuzzy shape
coords <- matrix(c(x=c(0.1, 0.3, 0.5, 0.7, 0.9), y=c(0.8, 0.6, 0.4, 0.2, 0.8)), ncol = 2, byrow = TRUE)

# Display fuzzy shape
polygon(coords, border = "green")


# person2
# Plot the image person2
plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1), xlab = "", ylab = "", asp = 1)
rasterImage(person2, 0, 0, 1, 1)

# Draw a circle
symbols(0.5, 0.5, circles = 0.2, inches = FALSE, add = TRUE, bg = "transparent", fg = "yellow", lwd = 2)

# Draw a rectangle
rect(xleft = 0.2, ybottom = 0.2, xright = 0.5, ytop = 0.5, border = "red", lwd = 2)

# Draw the fuzzy shape
# Create a matrix of coordinates for the polygon fuzzy shape
coords <- matrix(c(x=c(0.1, 0.3, 0.5, 0.7, 0.9), y=c(0.8, 0.6, 0.4, 0.2, 0.8)), ncol = 2, byrow = TRUE)

# Display fuzzy shape
polygon(coords, border = "green")


# building1
# Plot the image building1
plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1), xlab = "", ylab = "", asp = 1)
rasterImage(building1, 0, 0, 1, 1)

# Draw a circle
symbols(0.5, 0.5, circles = 0.2, inches = FALSE, add = TRUE, bg = "transparent", fg = "yellow", lwd = 2)

# Draw a rectangle
rect(xleft = 0.2, ybottom = 0.2, xright = 0.5, ytop = 0.5, border = "red", lwd = 2)

# Draw the fuzzy shape
# Create a matrix of coordinates for the polygon fuzzy shape
coords <- matrix(c(x=c(0.1, 0.3, 0.5, 0.7, 0.9), y=c(0.8, 0.6, 0.4, 0.2, 0.8)), ncol = 2, byrow = TRUE)

# Display fuzzy shape
polygon(coords, border = "green")


# building2
# Plot the image building2
plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1), xlab = "", ylab = "", asp = 1)
rasterImage(building2, 0, 0, 1, 1)

# Draw a circle
symbols(0.5, 0.5, circles = 0.2, inches = FALSE, add = TRUE, bg = "transparent", fg = "yellow", lwd = 2)

# Draw a rectangle
rect(xleft = 0.2, ybottom = 0.2, xright = 0.5, ytop = 0.5, border = "red", lwd = 2)

# Draw the fuzzy shape
# Create a matrix of coordinates for the polygon fuzzy shape
coords <- matrix(c(x=c(0.1, 0.3, 0.5, 0.7, 0.9), y=c(0.8, 0.6, 0.4, 0.2, 0.8)), ncol = 2, byrow = TRUE)

# Display fuzzy shape
polygon(coords, border = "green")


# animal1
# Plot the image animal1
plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1), xlab = "", ylab = "", asp = 1)
rasterImage(animal1, 0, 0, 1, 1)

# Draw a circle
symbols(0.5, 0.5, circles = 0.2, inches = FALSE, add = TRUE, bg = "transparent", fg = "yellow", lwd = 2)

# Draw a rectangle
rect(xleft = 0.2, ybottom = 0.2, xright = 0.5, ytop = 0.5, border = "red", lwd = 2)

# Draw the fuzzy shape
# Create a matrix of coordinates for the polygon fuzzy shape
coords <- matrix(c(x=c(0.1, 0.3, 0.5, 0.7, 0.9), y=c(0.8, 0.6, 0.4, 0.2, 0.8)), ncol = 2, byrow = TRUE)

# Display fuzzy shape
polygon(coords, border = "green")


# animal2
# Plot the image animal2
plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1), xlab = "", ylab = "", asp = 1)
rasterImage(animal2, 0, 0, 1, 1)

# Draw a circle
symbols(0.5, 0.5, circles = 0.2, inches = FALSE, add = TRUE, bg = "transparent", fg = "yellow", lwd = 2)

# Draw a rectangle
rect(xleft = 0.2, ybottom = 0.2, xright = 0.5, ytop = 0.5, border = "red", lwd = 2)

# Draw the fuzzy shape
# Create a matrix of coordinates for the polygon fuzzy shape
coords <- matrix(c(x=c(0.1, 0.3, 0.5, 0.7, 0.9), y=c(0.8, 0.6, 0.4, 0.2, 0.8)), ncol = 2, byrow = TRUE)

# Display fuzzy shape
polygon(coords, border = "green")


# vi) blurry & sharpen
layout(t(1:2))
isoblur(flower1,20)%>% plot()
imsharpen(flower1, 50)%>%plot()

isoblur(flower2,20)%>% plot()
imsharpen(flower2, 50)%>%plot()

isoblur(scenery1,20)%>% plot()
imsharpen(scenery1, 50)%>%plot()

isoblur(scenery2,20)%>% plot()
imsharpen(scenery2, 50)%>%plot()

isoblur(person1,20)%>% plot()
imsharpen(person1, 50)%>%plot()

isoblur(person2,20)%>% plot()
imsharpen(person2, 50)%>%plot()

isoblur(building1,20)%>% plot()
imsharpen(building1, 50)%>%plot()

isoblur(building2,20)%>% plot()
imsharpen(building2, 50)%>%plot()

isoblur(animal1,20)%>% plot()
imsharpen(animal1, 50)%>%plot()

isoblur(animal2,20)%>% plot()
imsharpen(animal2, 50)%>%plot()

# vii) segmentation 
# segmenting by thresholding and contour

#flower1
dev.off()
par(mfrow=c(1,3))

# Convert image to grayscale
grayscale_flower1 <- grayscale(flower1)

# Apply thresholding
threshold_value <- 0.5
binary_flower1 <- grayscale_flower1 > threshold_value

# Display the binary image
plot(binary_flower1)

# Convert the grayscale image to a numeric matrix
gray_matrix_flower1 <- as.matrix(grayscale_flower1)

# Perform contour detection
contours <- contour(gray_matrix_flower1)

# Plot the image with contours
plot(grayscale_flower1)
lines(contours, col = "red", lwd = 2)


#flower2
dev.off()
par(mfrow=c(1,3))
# Convert image to grayscale
grayscale_flower2 <- grayscale(flower2)

# Apply thresholding
threshold_value <- 0.5
binary_flower2 <- grayscale_flower2 > threshold_value

# Display the binary image
plot(binary_flower2)

# Convert the grayscale image to a numeric matrix
gray_matrix_flower2 <- as.matrix(grayscale_flower2)

# Perform contour detection
contours <- contour(gray_matrix_flower2)

# Plot the image with contours
plot(grayscale_flower2)
lines(contours, col = "red", lwd = 2)


#scenery1
dev.off()
par(mfrow=c(1,3))
# Convert image to grayscale
grayscale_scenery1 <- grayscale(scenery1)

# Apply thresholding
threshold_value <- 0.5
binary_scenery1 <- grayscale_scenery1 > threshold_value

# Display the binary image
plot(binary_scenery1)

# Convert the grayscale image to a numeric matrix
gray_matrix_scenery1 <- as.matrix(grayscale_scenery1)

# Perform contour detection
contours <- contour(gray_matrix_scenery1)

# Plot the image with contours
plot(grayscale_scenery1)
lines(contours, col = "red", lwd = 2)


#scenery2
dev.off()
par(mfrow=c(1,3))
# Convert image to grayscale
grayscale_scenery2 <- grayscale(scenery2)

# Apply thresholding
threshold_value <- 0.5
binary_scenery2 <- grayscale_scenery2 > threshold_value

# Display the binary image
plot(binary_scenery2)

# Convert the grayscale image to a numeric matrix
gray_matrix_scenery2 <- as.matrix(grayscale_scenery2)

# Perform contour detection
contours <- contour(gray_matrix_scenery2)

# Plot the image with contours
plot(grayscale_scenery2)
lines(contours, col = "red", lwd = 2)


#person1
dev.off()
par(mfrow=c(1,3))
# Convert image to grayscale
grayscale_person1 <- grayscale(person1)

# Apply thresholding
threshold_value <- 0.5
binary_person1 <- grayscale_person1 > threshold_value

# Display the binary image
plot(binary_person1)

# Convert the grayscale image to a numeric matrix
gray_matrix_person1 <- as.matrix(grayscale_person1)

# Perform contour detection
contours <- contour(gray_matrix_person1)

# Plot the image with contours
plot(grayscale_person1)
lines(contours, col = "red", lwd = 2)


#person2
dev.off()
par(mfrow=c(1,3))
# Convert image to grayscale
grayscale_person2 <- grayscale(person2)

# Apply thresholding
threshold_value <- 0.5
binary_person2 <- grayscale_person2 > threshold_value

# Display the binary image
plot(binary_person2)

# Convert the grayscale image to a numeric matrix
gray_matrix_person2 <- as.matrix(grayscale_person2)

# Perform contour detection
contours <- contour(gray_matrix_person2)

# Plot the image with contours
plot(grayscale_person2)
lines(contours, col = "red", lwd = 2)


#building1
dev.off()
par(mfrow=c(1,3))
# Convert image to grayscale
grayscale_building1 <- grayscale(building1)

# Apply thresholding
threshold_value <- 0.5
binary_building1 <- grayscale_building1 > threshold_value

# Display the binary image
plot(binary_building1)

# Convert the grayscale image to a numeric matrix
gray_matrix_building1 <- as.matrix(grayscale_building1)

# Perform contour detection
contours <- contour(gray_matrix_building1)

# Plot the image with contours
plot(grayscale_building1)
lines(contours, col = "red", lwd = 2)


#building2
dev.off()
par(mfrow=c(1,3))
# Convert image to grayscale
grayscale_building2 <- grayscale(building2)

# Apply thresholding
threshold_value <- 0.5
binary_building2 <- grayscale_building2 > threshold_value

# Display the binary image
plot(binary_building2)

# Convert the grayscale image to a numeric matrix
gray_matrix_building2 <- as.matrix(grayscale_building2)

# Perform contour detection
contours <- contour(gray_matrix_building2)

# Plot the image with contours
plot(grayscale_building2)
lines(contours, col = "red", lwd = 2)


#animal1
dev.off()
par(mfrow=c(1,3))
# Convert image to grayscale
grayscale_animal1 <- grayscale(animal1)

# Apply thresholding
threshold_value <- 0.5
binary_animal1 <- grayscale_animal1 > threshold_value

# Display the binary image
plot(binary_animal1)

# Convert the grayscale image to a numeric matrix
gray_matrix_animal1 <- as.matrix(grayscale_animal1)

# Perform contour detection
contours <- contour(gray_matrix_animal1)

# Plot the image with contours
plot(grayscale_animal1)
lines(contours, col = "red", lwd = 2)


#animal2
dev.off()
par(mfrow=c(1,3))
# Convert image to grayscale
grayscale_animal2 <- grayscale(animal2)

# Apply thresholding
threshold_value <- 0.5
binary_animal2 <- grayscale_animal2 > threshold_value

# Display the binary image
plot(binary_animal2)

# Convert the grayscale image to a numeric matrix
gray_matrix_animal2 <- as.matrix(grayscale_animal2)

# Perform contour detection
contours <- contour(gray_matrix_animal2)

# Plot the image with contours
plot(grayscale_animal2)
lines(contours, col = "red", lwd = 2)


# viii) histogram equalisation
dev.off()

#flower1
plot(flower1)
grayscale(flower1) %>% hist(main="Luminance values in boats picture")
R(flower1) %>% hist(main="Red channel values in boats picture") #  Red boat ; G(boats) is green, B(boats) is blue
G(flower1) %>% hist(main="Red channel values in boats picture")  # Green boat
B(flower1) %>% hist(main="Red channel values in boats picture")  # Blue boat

bdf <- as.data.frame(flower1)
head(bdf,3)
bdf <- mutate(bdf,channel=factor(cc,labels=c('R','G','B')))
ggplot(bdf,aes(value,col=channel))+geom_histogram(bins=30)+facet_wrap(~ channel)


f <- ecdf(flower1)
f(flower1) %>% hist(main="Transformed luminance values")
f(flower1) %>% str
f(flower1) %>% as.cimg(dim=dim(flower1)) %>% plot(
  main="With histogram equalisation")


#flower2
plot(flower2)
grayscale(flower2) %>% hist(main="Luminance values in boats picture")
R(flower2) %>% hist(main="Red channel values in boats picture") #  Red boat ; G(boats) is green, B(boats) is blue
G(flower2) %>% hist(main="Red channel values in boats picture")  # Green boat
B(flower2) %>% hist(main="Red channel values in boats picture")  # Blue boat

bdf2 <- as.data.frame(flower2)
head(bdf2,3)
bdf2 <- mutate(bdf2,channel=factor(cc,labels=c('R','G','B')))
ggplot(bdf2,aes(value,col=channel))+geom_histogram(bins=30)+facet_wrap(~ channel)


f <- ecdf(flower2)
f(flower2) %>% hist(main="Transformed luminance values")
f(flower2) %>% str
f(flower2) %>% as.cimg(dim=dim(flower2)) %>% plot(
  main="With histogram equalisation")


#scenery1
plot(scenery1)
grayscale(scenery1) %>% hist(main="Luminance values in boats picture")
R(scenery1) %>% hist(main="Red channel values in boats picture") #  Red boat ; G(boats) is green, B(boats) is blue
G(scenery1) %>% hist(main="Red channel values in boats picture")  # Green boat
B(scenery1) %>% hist(main="Red channel values in boats picture")  # Blue boat

bdf3 <- as.data.frame(scenery1)
head(bdf3,3)
bdf3 <- mutate(bdf3,channel=factor(cc,labels=c('R','G','B')))
ggplot(bdf3,aes(value,col=channel))+geom_histogram(bins=30)+facet_wrap(~ channel)


f <- ecdf(scenery1)
f(scenery1) %>% hist(main="Transformed luminance values")
f(scenery1) %>% str
f(scenery1) %>% as.cimg(dim=dim(scenery1)) %>% plot(
  main="With histogram equalisation")


#scenery2
plot(scenery2)
grayscale(scenery2) %>% hist(main="Luminance values in boats picture")
R(scenery2) %>% hist(main="Red channel values in boats picture") #  Red boat ; G(boats) is green, B(boats) is blue
G(scenery2) %>% hist(main="Red channel values in boats picture")  # Green boat
B(scenery2) %>% hist(main="Red channel values in boats picture")  # Blue boat

bdf4 <- as.data.frame(scenery2)
head(bdf4,3)
bdf4 <- mutate(bdf4,channel=factor(cc,labels=c('R','G','B')))
ggplot(bdf4,aes(value,col=channel))+geom_histogram(bins=30)+facet_wrap(~ channel)


f <- ecdf(scenery2)
f(scenery2) %>% hist(main="Transformed luminance values")
f(scenery2) %>% str
f(scenery2) %>% as.cimg(dim=dim(scenery2)) %>% plot(
  main="With histogram equalisation")



#person1
plot(person1)
grayscale(person1) %>% hist(main="Luminance values in boats picture")
R(person1) %>% hist(main="Red channel values in boats picture") #  Red boat ; G(boats) is green, B(boats) is blue
G(person1) %>% hist(main="Red channel values in boats picture")  # Green boat
B(person1) %>% hist(main="Red channel values in boats picture")  # Blue boat

bdf5 <- as.data.frame(person1)
head(bdf5,3)
bdf5 <- mutate(bdf5,channel=factor(cc,labels=c('R','G','B')))
ggplot(bdf5,aes(value,col=channel))+geom_histogram(bins=30)+facet_wrap(~ channel)


f <- ecdf(person1)
f(person1) %>% hist(main="Transformed luminance values")
f(person1) %>% str
f(person1) %>% as.cimg(dim=dim(person1)) %>% plot(
  main="With histogram equalisation")


#person2
plot(person2)
grayscale(person2) %>% hist(main="Luminance values in boats picture")
R(person2) %>% hist(main="Red channel values in boats picture") #  Red boat ; G(boats) is green, B(boats) is blue
G(person2) %>% hist(main="Red channel values in boats picture")  # Green boat
B(person2) %>% hist(main="Red channel values in boats picture")  # Blue boat

bdf6 <- as.data.frame(person2)
head(bdf6,3)
bdf6 <- mutate(bdf6,channel=factor(cc,labels=c('R','G','B')))
ggplot(bdf6,aes(value,col=channel))+geom_histogram(bins=30)+facet_wrap(~ channel)


f <- ecdf(person2)
f(person2) %>% hist(main="Transformed luminance values")
f(person2) %>% str
f(person2) %>% as.cimg(dim=dim(person2)) %>% plot(
  main="With histogram equalisation")


#building1
plot(building1)
grayscale(building1) %>% hist(main="Luminance values in boats picture")
R(building1) %>% hist(main="Red channel values in boats picture") #  Red boat ; G(boats) is green, B(boats) is blue
G(building1) %>% hist(main="Red channel values in boats picture")  # Green boat
B(building1) %>% hist(main="Red channel values in boats picture")  # Blue boat

bdf7 <- as.data.frame(building1)
head(bdf7,3)
bdf7 <- mutate(bdf7,channel=factor(cc,labels=c('R','G','B')))
ggplot(bdf7,aes(value,col=channel))+geom_histogram(bins=30)+facet_wrap(~ channel)


f <- ecdf(building1)
f(building1) %>% hist(main="Transformed luminance values")
f(building1) %>% str
f(building1) %>% as.cimg(dim=dim(building1)) %>% plot(
  main="With histogram equalisation")


#building2
plot(building2)
grayscale(building2) %>% hist(main="Luminance values in boats picture")
R(building2) %>% hist(main="Red channel values in boats picture") #  Red boat ; G(boats) is green, B(boats) is blue
#G(building2) %>% hist(main="Red channel values in boats picture")  # Green boat
#B(building2) %>% hist(main="Red channel values in boats picture")  # Blue boat

#bdf8 <- as.data.frame(building2)
#head(bdf8,3)
#bdf8 <- mutate(bdf8,channel=factor(cc,labels=c('R','G','B')))
#ggplot(bdf8,aes(value,col=channel))+geom_histogram(bins=30)+facet_wrap(~ channel)


f <- ecdf(building2)
f(building2) %>% hist(main="Transformed luminance values")
f(building2) %>% str
f(building2) %>% as.cimg(dim=dim(building2)) %>% plot(
  main="With histogram equalisation")


#animal1
plot(animal1)
grayscale(animal1) %>% hist(main="Luminance values in boats picture")
R(animal1) %>% hist(main="Red channel values in boats picture") #  Red boat ; G(boats) is green, B(boats) is blue
G(animal1) %>% hist(main="Red channel values in boats picture")  # Green boat
B(animal1) %>% hist(main="Red channel values in boats picture")  # Blue boat

bdf9 <- as.data.frame(animal1)
head(bdf9,3)
bdf9 <- mutate(bdf9,channel=factor(cc,labels=c('R','G','B')))
ggplot(bdf9,aes(value,col=channel))+geom_histogram(bins=30)+facet_wrap(~ channel)


f <- ecdf(animal1)
f(animal1) %>% hist(main="Transformed luminance values")
f(animal1) %>% str
f(animal1) %>% as.cimg(dim=dim(animal1)) %>% plot(
  main="With histogram equalisation")


#animal2
plot(animal2)
grayscale(animal2) %>% hist(main="Luminance values in boats picture")
R(animal2) %>% hist(main="Red channel values in boats picture") #  Red boat ; G(boats) is green, B(boats) is blue
G(animal2) %>% hist(main="Red channel values in boats picture")  # Green boat
B(animal2) %>% hist(main="Red channel values in boats picture")  # Blue boat

bdf10 <- as.data.frame(animal2)
head(bdf10,3)
bdf10 <- mutate(bdf10,channel=factor(cc,labels=c('R','G','B')))
ggplot(bdf10,aes(value,col=channel))+geom_histogram(bins=30)+facet_wrap(~ channel)


f <- ecdf(animal2)
f(animal2) %>% hist(main="Transformed luminance values")
f(animal2) %>% str
f(animal2) %>% as.cimg(dim=dim(animal2)) %>% plot(
  main="With histogram equalisation")


# //////////////////////////////////////////////////////////////////////////////
# /////////////////////////////////////// PART 2 ////////////////////////////////

# i) DENOISING
noise1 <- load.image("C:/Users/User/Desktop/STQD6114_ unstructured data/project_3/Noise Image/Noise1.png")
noise2 <- load.image("C:/Users/User/Desktop/STQD6114_ unstructured data/project_3/Noise Image/Noise2.png")
noise3 <- load.image("C:/Users/User/Desktop/STQD6114_ unstructured data/project_3/Noise Image/Noise3.png")
noise4 <- load.image("C:/Users/User/Desktop/STQD6114_ unstructured data/project_3/Noise Image/Noise4.png")
noise5 <- load.image("C:/Users/User/Desktop/STQD6114_ unstructured data/project_3/Noise Image/Noise4.png")

plot(noise1)
plot(noise2)
plot(noise3)
plot(noise4)
plot(noise5)

#?blur_anisotropic

# noise1
noise1.noisy <- (noise1 + .5*rnorm(prod(dim(noise1)))) 
layout(t(1:2)) 
# layout(1) #also can
plot(noise1.noisy,main="Original")
isoblur(noise1.noisy,2) %>% plot(main="Blurred")

plot(noise1.noisy,main="Original")
blur_anisotropic(noise1.noisy,ampl=1e2,sharp=.3) %>% plot(main="Blurred (anisotropic)")


# noise2
noise2.noisy <- (noise2 + .5*rnorm(prod(dim(noise2)))) 
layout(t(1:2)) 
# layout(1) #also can
plot(noise2.noisy,main="Original")
isoblur(noise2.noisy,2) %>% plot(main="Blurred")

plot(noise2.noisy,main="Original")
blur_anisotropic(noise2.noisy,ampl=1e2,sharp=.3) %>% plot(main="Blurred (anisotropic)")


# noise3
noise3.noisy <- (noise3 + .5*rnorm(prod(dim(noise3)))) 
layout(t(1:2)) 
# layout(1) #also can
plot(noise3.noisy,main="Original")
isoblur(noise3.noisy,2) %>% plot(main="Blurred")

plot(noise3.noisy,main="Original")
blur_anisotropic(noise3.noisy,ampl=1e2,sharp=.3) %>% plot(main="Blurred (anisotropic)")


# noise4
noise4.noisy <- (noise4 + .5*rnorm(prod(dim(noise4)))) 
layout(t(1:2)) 
# layout(1) #also can
plot(noise4.noisy,main="Original")
isoblur(noise4.noisy,2) %>% plot(main="Blurred")

plot(noise4.noisy,main="Original")
blur_anisotropic(noise4.noisy,ampl=1e2,sharp=.3) %>% plot(main="Blurred (anisotropic)")


# noise5
noise5.noisy <- (noise5 + .5*rnorm(prod(dim(noise5)))) 
layout(t(1:2)) 
# layout(1) #also can
plot(noise5.noisy,main="Original")
isoblur(noise5.noisy,2) %>% plot(main="Blurred")

plot(noise5.noisy,main="Original")
blur_anisotropic(noise5.noisy,ampl=1e2,sharp=.3) %>% plot(main="Blurred (anisotropic)")


# ii) MORPHOLOGICAL OPERATION

# using "magick" package, because imager cannot convert into greyscale
# load image using magick package method

Noise1 <- image_read("C:/Users/User/Desktop/STQD6114_ unstructured data/project_3/Noise Image/Noise1.png")
Noise2 <- image_read("C:/Users/User/Desktop/STQD6114_ unstructured data/project_3/Noise Image/Noise2.png")
Noise3 <- image_read("C:/Users/User/Desktop/STQD6114_ unstructured data/project_3/Noise Image/Noise3.png")
Noise4 <- image_read("C:/Users/User/Desktop/STQD6114_ unstructured data/project_3/Noise Image/Noise4.png")
Noise5 <- image_read("C:/Users/User/Desktop/STQD6114_ unstructured data/project_3/Noise Image/Noise5.png")

# Noise1
# Convert the image to grayscale
gray_Noise1 <- image_convert(Noise1, colorspace = "gray")

# Apply thresholding
#?image_threshold()
layout(t(1:3))
thresholded_image1_Noise1 <- image_threshold(gray_Noise1,type = c("black", "white"), "50%")
thresholded_image2_Noise1 <- image_threshold(gray_Noise1,type = c("black", "white"), "35%")
thresholded_image3_Noise1 <- image_threshold(gray_Noise1,type = c("black", "white"), "10%")

# Display the thresholded images
plot(thresholded_image1_Noise1)
plot(thresholded_image2_Noise1)
plot(thresholded_image3_Noise1)
layout(1)


# Noise2
# Convert the image to grayscale
gray_Noise2 <- image_convert(Noise2, colorspace = "gray")

# Apply thresholding
#?image_threshold()
layout(t(1:3))
thresholded_image1_Noise2 <- image_threshold(gray_Noise2,type = c("black", "white"), "50%")
thresholded_image2_Noise2 <- image_threshold(gray_Noise2,type = c("black", "white"), "35%")
thresholded_image3_Noise2 <- image_threshold(gray_Noise2,type = c("black", "white"), "10%")

# Display the thresholded images
plot(thresholded_image1_Noise2)
plot(thresholded_image2_Noise2)
plot(thresholded_image3_Noise2)
layout(1)


# Noise3
# Convert the image to grayscale
gray_Noise3 <- image_convert(Noise3, colorspace = "gray")

# Apply thresholding
#?image_threshold()
layout(t(1:3))
thresholded_image1_Noise3 <- image_threshold(gray_Noise3,type = c("black", "white"), "50%")
thresholded_image2_Noise3 <- image_threshold(gray_Noise3,type = c("black", "white"), "35%")
thresholded_image3_Noise3 <- image_threshold(gray_Noise3,type = c("black", "white"), "10%")

# Display the thresholded images
plot(thresholded_image1_Noise3)
plot(thresholded_image2_Noise3)
plot(thresholded_image3_Noise3)
layout(1)


# Noise4
# Convert the image to grayscale
gray_Noise4 <- image_convert(Noise4, colorspace = "gray")

# Apply thresholding
#?image_threshold()
layout(t(1:3))
thresholded_image1_Noise4 <- image_threshold(gray_Noise4,type = c("black", "white"), "50%")
thresholded_image2_Noise4 <- image_threshold(gray_Noise4,type = c("black", "white"), "35%")
thresholded_image3_Noise4 <- image_threshold(gray_Noise4,type = c("black", "white"), "10%")

# Display the thresholded images
plot(thresholded_image1_Noise4)
plot(thresholded_image2_Noise4)
plot(thresholded_image3_Noise4)
layout(1)


# Noise5
# Convert the image to grayscale
gray_Noise5 <- image_convert(Noise5, colorspace = "gray")

# Apply thresholding
#?image_threshold()
layout(t(1:3))
thresholded_image1_Noise5 <- image_threshold(gray_Noise5,type = c("black", "white"), "50%")
thresholded_image2_Noise5 <- image_threshold(gray_Noise5,type = c("black", "white"), "35%")
thresholded_image3_Noise5 <- image_threshold(gray_Noise5,type = c("black", "white"), "10%")

# Display the thresholded images
plot(thresholded_image1_Noise5)
plot(thresholded_image2_Noise5)
plot(thresholded_image3_Noise5)
layout(1)

# ////////////////////////////////// END PROJECT TASK 1 //////////////////////////////
# /////////////////////////////////////////////////////////////////////////////////////

