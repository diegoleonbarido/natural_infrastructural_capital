########### Plotting solar and microgrid projects

library(maptools)



#Setting the directory
setwd('/Users/Diego/Desktop/IBM/Raw_Data/Kenya_Ag_Data')

#Kenya Shape File
kenya_shapefile <- readShapePoly('county/County.shp')
plot(kenya_shapefile)
par(new=TRUE)

### equity

setwd('/Users/Diego/Desktop/IBM/Raw_Data')
equity <- read.csv('equity.csv')

points(equity$Longitude, equity$Latitude,pch=20,col='dodgerblue',cex=0.8)

### MPESA 

mpesa <-read.csv('MPESA.csv')
mpesa_sub <- subset(mpesa,mpesa$LAT != 'NO_RESULTS' | mpesa$LNG != 'NO_RESULTS')

mpesa_sub$LAT <- as.numeric(as.character(mpesa_sub$LAT))
mpesa_sub$LNG <- as.numeric(as.character(mpesa_sub$LNG))

points(mpesa_sub$LNG, mpesa_sub$LAT, pch=20,col='brown1',cex=0.8)

legend( x="bottomleft",legend=c("M-PESA Agents","Equity Bank Agents"),col=c("brown1","dodgerblue"), pch=c(20,20),cex=0.6,merge=FALSE )








