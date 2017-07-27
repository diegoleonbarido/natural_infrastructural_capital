############################################
# Plotting Complementary Infrastructure Data to Explore It
# Data: Includes agriculture, roads, towns, irrigation, and proposed irrigation projects.
###########################

#Installing libraries
library(maptools)
library(rgdal)
library(RColorBrewer)

#Setting the directory
setwd('/Users/Diego/Desktop/IBM/Raw_Data/Kenya_Ag_Data')

#Kenya Shape File
kenya_shapefile <- readShapePoly('county/County.shp')
kenya <- data.frame(kenya_shapefile)
plot(kenya_shapefile)

	#Plotting wards
	wards_shapefile <- readShapePoly('wards/OGRGeoJSON.shp')
	plot(wards_shapefile)
	wards <- data.frame(wards_shapefile)

#Agriculture
#agriculture <- readShapePoly('ke_agriculture/ke_agriculture')
#data.frame(agriculture)
#plot(agriculture, col = rainbow(10))
#ag_data <- data.frame(agriculture)

#Crop Diversity
crop_diversity <- readShapePoly('crop_diversity/ke_crops_diversity.shp')
diversity_data <- data.frame(crop_diversity)
plot(crop_diversity,col = brewer.pal(9,"YlGn"), add= TRUE)

#Crop Intensity
crop_intensity <- readShapePoly('ke_crops_intensity/ke_crops_intensity.shp')
diversity_data <- data.frame(crop_intensity)
plot(crop_intensity,col = brewer.pal(9,"YlOrBr"), add= TRUE)


#Crops Irrigated 
#Note irrigation projects are to be completed with http://www.nib.or.ke/map-of-projects
crop_irrigation <- readShapePoly('ke_crops_irrig/ke_crops_irrig.shp')
	#Crops that are actually irrigated
	irrigated <- subset(crop_irrigation, IRRIGATED>0)
	#data.frame(irrigated)
	plot(irrigated,col = brewer.pal(9,"YlGnBu"), add = TRUE)
	irrigation_data <- data.frame(irrigated)
	
	#Small Scale Irrigation
	small_scale_irrigation <- readShapePoints('ke_small_scale_irrigation/ke_small_scale_irrigation.shp')
	#data.frame(small_scale_irrigation)
	plot(small_scale_irrigation, col ='deepskyblue', pch = 19, cex=0.3,add= TRUE)
	small_irrigation_data <- data.frame(small_scale_irrigation)
	
	#Proposed Irrigation Projects
	proposed_irrigation <- readShapePoints('ke_proposed_irrigation/ke_proposed_irrigation.shp')
	#data.frame(proposed_irrigation)
	plot(proposed_irrigation, col ='deepskyblue', pch = 19, cex=0.5,add= TRUE)
	
	#Government Large Scale Irrigation Projects
	government_irrigation <- read.csv('irrigation_data.csv')
	points(government_irrigation$long,government_irrigation$lat,pch=19,col='deepskyblue',cex=0.4)
	
	#Plotting Rivers
	rivers_and_basins <- readShapeLines('Rivers/Rivers')
	#data.frame(irrigated)
	plot(rivers_and_basins,col = 'blue', add = TRUE)
	water <- data.frame(ke_water_basins)	
	
legend( x="bottomleft",legend=c("Rivers","Small, Large, and Government Ran Irrigation Projects"),col=c("blue","deepskyblue"), lwd =1, lty=c(1,NA), pch=c(NA,20),cex=0.6,merge=FALSE )
	
	
		
#Major Roads (NOTE: 'Lines')  -- Would be good to link this to a google map
roads <- readShapeLines('ke_major_roads/ke_major_roads.shp')
data.frame(roads)
plot(roads, col ='red', add= TRUE)
road_data <- data.frame(roads)

	#Looking at the other data that contains the roads
	roads_other <- readShapeLines('kenya_roads_other/Kenya_roads')
	data.frame(roads)
	plot(roads_other, col ='red', add= TRUE)
	road_data <- data.frame(roads)
	
#Major Towns
towns <- readShapePoints('ke_major_towns/ke_major_towns.shp')
data.frame(towns)
plot(towns, col ='purple', pch = 19, cex=0.5,add= TRUE)
road_data <- data.frame(roads)

legend( x="bottomleft",legend=c("Major Roads","Major Towns"),col=c("red","purple"), lwd =1, lty=c(1,NA), pch=c(NA,20),cex=0.6,merge=FALSE )


	


