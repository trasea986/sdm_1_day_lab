#Packages to make this work:
library(rgbif)
library(sp)
library(raster)
library(maptools)
library(leaflet)
library(dismo) #if missing, install with "install.packages('package_name')"

#To do a distribution model, you need two things. First, you need location information, and second, you need environmental information for the area you want to model.

#to start with, we will bring in presence points from gbif.org, which is a repository of occurance data. feel free to check out their website!

#first make an object with the countries you are interested in using the official 2 letters. I reccomend picking one or two countries. 

countries <- c("US","CA")


#so let us get some present data. the first part is the function to find the data, and then we pass various arguments. if you are lost on all of the options, and there are a lot, type ?function, where 'function' in this case would be occ_search

#here we are creating an object named "Species_1" and use that throughout.

#now pull in data. "limit" is how many GPS points you want. to speed things up or if your internet is slow and the connection drops out, try lowering the number to 500.

Species_1 <- occ_search(scientificName= "Cervus canadensis", country=countries, limit=1000)

Species_2 <- occ_search(scientificName= "Canis lupus", country=countries, limit=1000)

#this is a gbif object, which has a lot of data to it. however, all we really want is the latitude and longitude so we will make a new dataframe out of the data object. Becauese of what we are going to do, we will be dealing with data frames and spatial data types.

Species_1_df1 <- as.data.frame(Species_1$US$data) #first country data
Species_1_df2 <- as.data.frame(Species_1$CA$data) #second country

#this removes everything except location information
Species_1_df1 <- Species_1_df1[,c("scientificName","decimalLatitude","decimalLongitude")]
Species_1_df2 <- Species_1_df2[,c("scientificName","decimalLatitude","decimalLongitude")]

#and finally, bringing the countries together
Species_1_df <- rbind(Species_1_df1, Species_1_df2)




####So now we have points, and just need environmental information

#this creates a list of rasters using the bioclim variables, which are biologicall relevant forms of temperature and precipiation. See WorldClim / BioClim websites for more information.

#better resolutions are great, but be cautious, these can be very large. Try 10 if you are short on space in the "res" argument.

worldclim <- getData("worldclim", var="bio", res=2.5)

#now before we get fancy with Maxent and distrbution models, just check out what the values are at your points for annual temperature

#first, here is mean annual temperature in tenths of a degree celsius
plot(worldclim$bio1)

#feel free to explore the other values, and check out: https://worldclim.org/bioclim if you have not already


#we can also turn our dataframe of points in to a spatial point object for the actual model, but one more line of code, which removes anything with missing data

Species_1_df <- na.omit(Species_1_df)
Species_1_sp <- Species_1_df
coordinates(Species_1_sp) <- ~decimalLongitude+decimalLatitude 

plot(Species_1_sp, add=TRUE)

#this may look a little funny, because your points may only be in a small part of the world
#so, let us do it again but flip the order.

#we are also going to get an existing map now



data("wrld_simpl")

plot(worldclim$bio1, legend=FALSE, xlim=c(-175,-60), ylim=c(25, 80), col=hcl.colors(palette='viridis', n=100))
plot(wrld_simpl, add = TRUE, xlim=c(-175,-60), ylim=c(25, 80)) #feel free to change the limits of your map to make it look nicer. I selected an area focused on the US

#add in the climate variable, but with new colors. n here is the number of color breaks. more=smoother colors but slower for the computer to make
#"add" keeps us from resetting the plot
#smallplot controls our legend

plot(worldclim$bio1, legend.only=TRUE, col=hcl.colors(palette='viridis', n=100), legend.width=1, legend.shrink=0.75, legend.args=list(text='Temperature (1/10 degree Celsius)', side=4, font=2, line=2.5, cex=0.8),
     smallplot=c(.2,.25, .4,.6)); par(mar = par("mar"))

plot(Species_1_sp, add=TRUE, pch=16) #get the points back



#we can now do species 2's point data

Species_2_df1 <- as.data.frame(Species_2$US$data) #first country data
Species_2_df2 <- as.data.frame(Species_2$CA$data) #second country
Species_2_df1 <- Species_2_df1[,c("scientificName","decimalLatitude","decimalLongitude")]
Species_2_df2 <- Species_2_df2[,c("scientificName","decimalLatitude","decimalLongitude")]
Species_2_df <- rbind(Species_2_df1, Species_2_df2)
Species_2_df <- na.omit(Species_2_df)
Species_2_sp <- Species_2_df
coordinates(Species_2_sp) <- ~decimalLongitude+decimalLatitude

#plot(Species_2_sp, add=TRUE, pch=15, col="grey")


#if you want to play with colors and so on, and want to clear the plot area use and start fresh without changing the "add" part of your code

#graphics.off() will reset your plotting stuff

#now to explore what the environmental data looks like at our GPS points
#make a new data frame
Species_1_ann_temp <- as.data.frame(Species_1_sp)
#extract values to a new columm "Temperature
Species_1_ann_temp$Temperature <- extract(worldclim$bio1, Species_1_sp)
head(Species_1_ann_temp) #head just shows the first part of the dataframe so we can make sure things worked. to see the whole thing, just type the name of the object.

#now we can plot the histogram of the environmental data after running the next line to make plots go side by side
old.par <- par(mfrow=c(1, 2), xpd=FALSE)

hist(Species_1_ann_temp$Temperature)
#if you tried another species above, compare it here simply by changing the object names

#do species 2
Species_2_ann_temp <- as.data.frame(Species_2_sp)
Species_2_ann_temp$Temperature <- extract(worldclim$bio1, Species_2_sp)

#now we can plot the histogram of the environmental data
hist(Species_2_ann_temp$Temperature)

#Do they have different environments they live in?
#Can you make a histogram of annual percipitation?


#so looking and trying to compare each one would be difficult, so let us build a mathematical model of the environmental suitability for each species. said another way, where are we most likely to find it?

#these models take a while to compute

Species_1_bioclim <- bioclim(worldclim, Species_1_sp)
Species_1_prediction <- predict(Species_1_bioclim, worldclim, progress='text')

#now to plot world
#clear plot settings
dev.off()
plot(wrld_simpl, xlim=c(-160,-60), ylim=c(25, 80), axes=TRUE)

#going to make very small values invisible and then plot with a fun color scheme
Species_1_prediction2 <- Species_1_prediction #this is just a backup object... wouldn't want to run predict again and that way you can play with the threshold of 0.01 by running the backup code again
Species_1_prediction2[Species_1_prediction2 < 0.01] <- NA
#one important argument here is "add" which means to put the plots together
plot(Species_1_prediction2, add=TRUE, col=hcl.colors(palette="Zissou 1",60))

Species_2_bioclim <- bioclim(worldclim, Species_2_sp)
Species_2_prediction <- predict(Species_2_bioclim, worldclim, progress='text')
Species_2_prediction2 <- Species_2_prediction
Species_2_prediction2[Species_2_prediction2 < 0.01] <- NA

#Now, instead of just going back and forth between the two, take a look at them side by side. See if you can ifugre out the extra arguments here.
old.par <- par(mfrow=c(1, 2), xpd=FALSE) #tells R you are making two plots
r.range <- c(0, 1)
plot(wrld_simpl, xlim=c(-160,-60), ylim=c(25, 80), axes=TRUE, main = "Species 1")
plot(Species_1_prediction2, add=TRUE, col=hcl.colors(palette="Zissou 1",60), axes = FALSE, legend = FALSE)
plot(Species_1_prediction2, legend.only=TRUE, col=hcl.colors(palette="Zissou 1",60), legend.width=1, legend.shrink=0.75,
     smallplot=c(0.2,0.25, 0.75,0.85)); par(mar = par("mar"))
plot(wrld_simpl, xlim=c(-160,-60), ylim=c(25, 80), axes=TRUE,  main = "Species 2")
plot(Species_2_prediction2, add=TRUE, col=hcl.colors(palette="Zissou 1",60), axes = FALSE, legend = FALSE)
plot(Species_2_prediction2, legend.only=TRUE, col=hcl.colors(palette="Zissou 1",60), legend.width=1, legend.shrink=0.75,
     smallplot=c(0.2,0.25, 0.75,0.85)); par(mar = par("mar"))