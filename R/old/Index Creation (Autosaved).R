#########################################
#Analysis: Stats on the complementary infrastructure data set, descriptive statitistics, map of metric through Kenya, and 

#NOTE: Uses the 'complementary_infrastructure_II' code, that's where 'comp_inf_for_stats.csv' gets created. 

#Libraries
library(maptools)
library(RColorBrewer)

setwd('/Users/Diego/Desktop/IBM/Raw_Data')
stats_analysis <- read.csv('comp_inf_for_stats.csv')


#Rescaling variables before putting everything togther

#Number of facilities per 1000 People
stats_analysis$r_fac_1000 <- (stats_analysis$fac_1000 - min(stats_analysis$fac_1000))/(max(stats_analysis$fac_1000)) - min(stats_analysis$fac_1000)

#Number of electrified facilities per 1000 People
stats_analysis$r_elec_fac <- (stats_analysis$elec_fac_1000 - min(stats_analysis$elec_fac_1000))/(max(stats_analysis$elec_fac_1000)) - min(stats_analysis$elec_fac_1000)

#Population density


#Distance to major towns



#Distance to grid


#MPESA Agents Per Ward



#Equity Branches


#Distance to major roads: Here a smaller distance is better so we'll take the opposite.
stats_analysis$r_dist_roads <- 1 - (stats_analysis$distance_major_roads - min(stats_analysis$distance_major_roads))/(max(stats_analysis$distance_major_roads)) - min(stats_analysis$distance_major_roads)

#Distance to secondary roads: Here a smaller distance is better so we'll take the opposite.
stats_analysis$r_2nd_roads <- 1 - (stats_analysis$distance_second_roads - min(stats_analysis$distance_second_roads))/(max(stats_analysis$distance_second_roads)) - min(stats_analysis$distance_second_roads)

#Crop Diversity



#Crop Intensity



#Distance to rivers
stats_analysis$r_rivers <- 1 - (stats_analysis$distance_rivers - min(stats_analysis$distance_rivers))/(max(stats_analysis$distance_rivers)) - min(stats_analysis$distance_rivers)



#Number of irrigation projects



####################
#Creating a file that puts the entire index together

vars <- c('WARD','r_fac_1000','r_elec_fac','r_dist_roads','r_2nd_roads','r_rivers')
index_vars <- stats_analysis[vars]

index_vars$index <- stats_analysis$r_fac_1000 + stats_analysis$r_elec_fac + stats_analysis$r_dist_roads + stats_analysis$r_2nd_roads + stats_analysis$r_rivers



###################
#Mapping the index

setwd('/Users/Diego/Desktop/IBM/Raw_Data/Kenya_Ag_Data')
wards_shapefile <- readShapePoly('wards/OGRGeoJSON.shp')
wards <- data.frame(wards_shapefile)

###Starting to plot our stats

	#Matching
	match_order <- match(wards$COUNTY_A_1,index_vars$WARD)
	index_vars$index[match_order]

	wards$index <- index_vars$index[match_order]
	
	#Histogram to create breaks for color buckets
	hist(wards$index)
	
	#Creating variables for the buckets
	map_breaks <- c(0,0.5,1,1.5,2,2.5,3,3.5)
	buckets <- cut(wards$index, breaks = map_breaks)
	numeric_buckets <- as.numeric(buckets)
	
	#Choosing colors for the map
	display.brewer.all()
	colors <- brewer.pal(9,"YlGnBu")
	colors[numeric_buckets]
	
	#Plotting our Nicaragua Map
	plot(wards_shapefile, col = colors[numeric_buckets])
	








