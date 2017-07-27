########### Plotting solar and microgrid projects

libraries(maptools)



#Setting the directory
setwd('/Users/Diego/Desktop/IBM/Raw_Data/Kenya_Ag_Data')

#Kenya Shape File
kenya_shapefile <- readShapePoly('county/County.shp')
plot(kenya_shapefile)



### Plotting microgrids and solar projects

setwd('/Users/Diego/Desktop/IBM/Raw_Data')
power <- read.csv('everyone.csv')

government <- subset(power, power$Company == 'government' & power$Type =='microgrid')
points(government$Lng, government$Lat,pch=19,col='red',cex=0.8,add= TRUE)

solar_entrepreneur <- subset(power,power$Type =='solar')
points(solar_entrepreneur$Lng, solar_entrepreneur$Lat,pch=19,col='orange',cex=0.7,add= TRUE)

micro_entrepreneur <- subset(power, power$Company != 'government' & power$Type =='microgrid')
points(micro_entrepreneur$Lng, micro_entrepreneur$Lat,pch=19,col='purple',cex=0.7,add= TRUE)

green_fields <- subset(power, power$Type =='green_fields')
points(green_fields$Lng, green_fields$Lat,pch=19,col='green',cex=0.7,add= TRUE)

