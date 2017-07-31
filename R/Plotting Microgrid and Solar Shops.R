########### Plotting solar and microgrid projects

library(maptools)

#Setting the directory
setwd('/Users/diego/Desktop/Projects_Code/natural_infrastructural_capital/Raw_Data')

#Kenya Shape File
kenya_shapefile <- readShapePoly('kenya_ag_data/county/County.shp')
plot(kenya_shapefile)

### Plotting microgrids and solar projects

setwd('/Users/diego/Desktop/Projects_Code/natural_infrastructural_capital/Raw_Data')
power <- read.csv('everyone.csv')

government <- subset(power, power$Company == 'government' & power$Type =='microgrid')
points(government$Lng, government$Lat,pch=19,col='cornflowerblue',cex=0.8,add= TRUE)

solar_entrepreneur <- subset(power,power$Type =='solar')
points(solar_entrepreneur$Lng, solar_entrepreneur$Lat,pch=19,col='orange',cex=0.7,add= TRUE)

micro_entrepreneur <- subset(power, power$Company != 'government' & power$Type =='microgrid')
points(micro_entrepreneur$Lng, micro_entrepreneur$Lat,pch=19,col='purple',cex=0.7,add= TRUE)

green_fields <- subset(power, power$Type =='green_fields')
points(green_fields$Lng, green_fields$Lat,pch=19,col='green',cex=0.7,add= TRUE)


### Plotting the transmission line Kenya Transmission - AIKP

#Kenya Shape File
trans_shape <- readShapeLines('Kenya_Ag_Data/Kenya Transmission - AIKP/Kenya Electricity Transmission Network.shp')
plot(trans_shape,col='red',add=TRUE)

legend( x="bottomleft",legend=c("Transmission Grid","Government Owned Microgrids","Proposed Microgrids","Microgrid Entrepreneurs","PV Shop"),col=c("red","cornflowerblue","green","purple","orange"), lwd =1, lty=c(1,NA,NA,NA,NA), pch=c(NA,20,20,20,20),cex=0.6,merge=FALSE )



