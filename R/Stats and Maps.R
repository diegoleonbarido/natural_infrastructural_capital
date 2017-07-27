###########
#Analysis: Calculates Descriptive Statistics and Plots the Results on the Map

#Diego

#NOTE: Uses the cleaning data code.

#Setting up the libraries
library(maptools)
library(RColorBrewer)

setwd('/Users/Diego/Desktop/IBM/Raw_Data')

complementary_infrastructure <- read.csv('complementary_infrastructure_correct_names.csv')
population <- read.csv('population.csv')

#######				
#Uploading and creating a data frame for the complementary infrastructure in the region
setwd('/Users/Diego/Desktop/IBM/Raw_Data/Kenya_Ag_Data')
wards_shapefile <- readShapePoly('wards/OGRGeoJSON.shp')
wards <- data.frame(wards_shapefile)


##### Descriptive stats (start with an easy example and then plot more)

	#Creating Electrified/Unelectrified Facilities Vars by Ward
	complementary_infrastructure$Electrified <- ifelse(complementary_infrastructure$STATUS == 'ELECTRIFIED PUBLIC FACILITIES',1,0)
	complementary_infrastructure$Unelectrified <- ifelse(complementary_infrastructure$STATUS == 'UNELECTRIFIED PUBLIC FACILITIES',1,0)
	
	#Creating Schools, Cliniques and Trade Centers Vars by Ward
	complementary_infrastructure$Schools <- ifelse(complementary_infrastructure$TYPE == 'SEC SCHOOLS/POLY',1,0)
	complementary_infrastructure$Trades <- ifelse(complementary_infrastructure$TYPE == 'TRADING CENTRES',1,0)
	complementary_infrastructure$Health <- ifelse(complementary_infrastructure$TYPE == 'HEALTH CENTRES',1,0)
	
	#Creating Electrified Schools, Cliniques and Trade Centers by Ward
	complementary_infrastructure$E_Schools <- ifelse(complementary_infrastructure$Schools == 1 & complementary_infrastructure$Electrified == 1,1,0)
	complementary_infrastructure$E_Trades <- ifelse(complementary_infrastructure$Trades == 1 & complementary_infrastructure$Electrified == 1,1,0)
	complementary_infrastructure$E_Health <- ifelse(complementary_infrastructure$Health == 1 & complementary_infrastructure$Electrified == 1,1,0)	
	
	
	#Aggregating our variables
	elec_i <- aggregate(complementary_infrastructure$Electrified,by=list(complementary_infrastructure$WARD),FUN = sum, na.rm = TRUE)
	names(elec_i)[1]<-'WARD'
	names(elec_i)[2]<-'ELEC_NUM'
	
	SCHOOLS <- aggregate(complementary_infrastructure$Schools, by=list(complementary_infrastructure$WARD),FUN=sum, na.rm = TRUE)
	names(SCHOOLS)[2] <- 'SCHOOLS'
	
	TRADE <- aggregate(complementary_infrastructure$Trades, by=list(complementary_infrastructure$WARD),FUN=sum, na.rm = TRUE)
	names(TRADE)[2] <- 'TRADE'
	
	HEALTH <- aggregate(complementary_infrastructure$Health, by=list(complementary_infrastructure$WARD),FUN=sum, na.rm = TRUE)
	names(HEALTH)[2] <- 'HEALTH'
	
	E_SCHOOLS <- aggregate(complementary_infrastructure$E_Schools, by=list(complementary_infrastructure$WARD),FUN=sum, na.rm = TRUE)
	names(E_SCHOOLS)[2] <- 'E_SCHOOLS'
	
	E_TRADE <- aggregate(complementary_infrastructure$E_Trades, by=list(complementary_infrastructure$WARD),FUN=sum, na.rm = TRUE)
	names(E_TRADE)[2] <- 'E_TRADE'
	
	E_HEALTH <- aggregate(complementary_infrastructure$E_Health, by=list(complementary_infrastructure$WARD),FUN=sum, na.rm = TRUE)
	names(E_HEALTH)[2] <- 'E_HEALTH'	
	
	#Kepping only what we need
elec_inf <- cbind(elec_i, SCHOOLS[2], TRADE[2], HEALTH[2], E_SCHOOLS[2], E_TRADE[2], E_HEALTH[2])
	
	elec_inf$FAC_NUM <- elec_inf$SCHOOLS + elec_inf$TRADE + elec_inf$HEALTH

	
	#Number of Electrified Facilities per 1000 people
		#Getting the file ready
		elec_pop <- merge(elec_inf,population, by='WARD')
		vars = c('WARD','FAC_NUM','ELEC_NUM','SCHOOLS','TRADE','HEALTH','E_SCHOOLS','E_TRADE','E_HEALTH','Male','Female')
		elec_pop = elec_pop[vars]
		elec_pop$Male <- as.numeric(elec_pop$Male)
		elec_pop$Female <- as.numeric(elec_pop$Female) 		
		elec_pop$total_pop = elec_pop$Male + elec_pop$Female
		
		elec_pop$fac_1000 = elec_pop$FAC_NUM/(elec_pop$total_pop/1000)
		elec_pop$elec_fac_1000 = elec_pop$ELEC_NUM/(elec_pop$total_pop/1000)
		elec_pop$schools_1000 = elec_pop$SCHOOLS/(elec_pop$total_pop/1000)
		elec_pop$trade_1000 = elec_pop$TRADE/(elec_pop$total_pop/1000)
		elec_pop$health_1000 = elec_pop$HEALTH/(elec_pop$total_pop/1000)
		elec_pop$e_schools_1000 = elec_pop$E_SCHOOLS/(elec_pop$total_pop/1000)
		elec_pop$e_trade_1000 = elec_pop$E_TRADE/(elec_pop$total_pop/1000)
		elec_pop$e_health_1000 = elec_pop$E_HEALTH/(elec_pop$total_pop/1000)
		
	#Population Density
		vars1<- c('WARD','total_pop')
		one <- elec_pop[vars1]
		vars2 <- c('COUNTY_A_1','Shape_Area')
		two <- wards[vars2]
		names(two)[1] <- 'WARD'
		density <- merge(one,two,by='WARD')
		density$pop_density <- density$total_pop/density$Shape_Area
		elec_pop <- merge(elec_pop,density,by='WARD')


				
##########				
#Uploading and creating a data frame for the complementary infrastructure in the region
setwd('/Users/Diego/Desktop/IBM/Raw_Data/Kenya_Ag_Data')
wards_shapefile <- readShapePoly('wards/OGRGeoJSON.shp')
wards <- data.frame(wards_shapefile)


###Starting to plot our stats

	#Matching
	match_order <- match(wards$COUNTY_A_1,elec_pop$WARD)
	elec_pop$elec_fac_1000[match_order]

	wards$ELEC_NUM <- elec_pop$elec_fac_1000[match_order]
	
	#Histogram to create breaks for color buckets
	hist(wards$ELEC_NUM)
	
	#Creating variables for the buckets
	map_breaks <- c(0,5,10,20,30,40,50,60,70,80,90)
	buckets <- cut(wards$ELEC_NUM, breaks = map_breaks)
	numeric_buckets <- as.numeric(buckets)
	
	#Choosing colors for the map
	display.brewer.all()
	colors <- brewer.pal(9,"PuBu")
	colors[numeric_buckets]
	
	#Plotting our Nicaragua Map
	plot(wards_shapefile, col = colors[numeric_buckets])
	
###Legend







	
	