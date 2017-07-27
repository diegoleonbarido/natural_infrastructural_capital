#Analysis: Puts the rest of the complementary infrastructure data togehter.

#NOTE1: Uses the file 'Complementary Infrastructure I' at the end. Before we merge 'complementary infrastructure I' and 'complementary infrastructure II' together I append all my shapefile information on one large file, zero the variables and clean the data before plotting. 

#Author: Diegp


library(maptools)
library(RColorBrewer)

#Agriculture
setwd('/Users/Diego/Desktop/IBM/Raw_Data/Kenya_Ag_Data')

	#Crop Diversity: Average number of crops grown
	ag <- readShapePoly('crop_diversity/crop_div_average/Join_Output_2.shp')
	crop_diversity <- as.data.frame(ag)
	vars <- c('COUNTY_A_1','Avg_SP_AVG')
	crop_diversity <- crop_diversity[vars]
	names(crop_diversity)[1] <- 'WARD'
	names(crop_diversity)[2] <- 'crop_diversity'
		
	
	#Crop Intensity: Percent of land under cultivation
	intense <- readShapePoly('ke_crops_intensity/intensity_avg/Join_Output.shp')
	intensity <- as.data.frame(intense)
	vars <- c('COUNTY_A_1','Avg_TOTAL_')
	crop_intensity <- intensity[vars]
	names(crop_intensity)[1] <- 'WARD1'
	names(crop_intensity)[2] <- 'crop_intensity'
		
	
	#Cbinding things together (merging was giving me trouble before, so I just cbind)
	crop_diversity <- crop_diversity[order(crop_diversity$WARD),]
	crop_intensity <- crop_intensity[order(crop_intensity$WARD),]
	comp_inf_II <- cbind(crop_diversity,crop_intensity)
	
	
	#Agro_ecological zones ** Still need to go a good job with agro-ecological zones **
	#ag_zones <- readShapePoly('Kenya_aezones/Kenya_aezones.shp')
	#plot(ag_zones)

	
#Irrigation

	#Government irrigation projects 
	gov_irr <- readShapePoly('ke_gov_irrigation/kev_gov_count/govirrig_newjoin.shp')
	gov_irrigation <- as.data.frame(gov_irr)
	vars <- c('COUNTY_A_1','Sum_count')
	gov_irrigation <- gov_irrigation[vars]
	names(gov_irrigation)[1] <- 'WARD2'
	names(gov_irrigation)[2] <- 'gov_irrigation'
	
	gov_irrigation <- gov_irrigation[order(gov_irrigation$WARD2),]
	comp_inf_II <- cbind(comp_inf_II,gov_irrigation)


	#Proposed Irrigation
	prop_irr <- readShapePoly('ke_proposed_irrigation/prop_irrigation_count/largeirrig_newjoin.shp')
	proposed_irrigation <- as.data.frame(prop_irr)
	vars <- c('COUNTY_A_1','Sum_count')
	proposed_irrigation <- proposed_irrigation[vars]
	names(proposed_irrigation)[1] <- 'WARD3'
	names(proposed_irrigation)[2] <- 'prop_irrigation'
		
	proposed_irrigation <- proposed_irrigation[order(proposed_irrigation$WARD3),]
	comp_inf_II <- cbind(comp_inf_II, proposed_irrigation)
	
	#Small scale irrigation
	small_irr <- readShapePoly('ke_small_scale_irrigation/count_small_irrigation/smallscale_newjoin.shp')
	small_irrigation <- as.data.frame(small_irr)
	vars <- c('COUNTY_A_1','Sum_count')
	small_irrigation <- small_irrigation[vars]
	names(small_irrigation)[1] <- 'WARD4'
	names(small_irrigation)[2] <- 'small_irrigation'
		
	small_irrigation <- small_irrigation[order(small_irrigation$WARD4),]
	comp_inf_II <- cbind(comp_inf_II, small_irrigation)
	
	
#Roads and Towns

	#Major roads
	major <- readShapePoints('ke_major_roads/distance_major/distance_major.shp')
	major_roads <- as.data.frame(major)
	vars <- c('COUNTY_A_1','distance')
	major_roads <- major_roads[vars]
	names(major_roads)[1] <- 'WARD5'
	names(major_roads)[2] <- 'distance_major_roads'
	
	major_roads <- major_roads[order(major_roads$WARD5),]
	comp_inf_II <- cbind(comp_inf_II, major_roads)
	
	
	#Secondary roads
	secnd_road <- readShapePoints('kenya_roads_other/2nd_road_distance/2nd_road_dist.shp')
	second_road <- as.data.frame(secnd_road)
	vars <- c('COUNTY_A_1','distance')
	second_road <- second_road[vars]
	names(second_road)[1] <- 'WARD6'
	names(second_road)[2] <- 'distance_second_roads'
	
	second_road <- second_road[order(second_road$WARD6),]
	comp_inf_II <- cbind(comp_inf_II, second_road)
	
	#Major towns
	major <- readShapePoints('ke_major_towns/town_distance/town_distance.shp')
	major_town <- as.data.frame(major)
	vars <- c('COUNTY_A_1','Distance')
	major_town <- major_town[vars]
	names(major_town)[1] <- 'WARD7'
	names(major_town)[2] <- 'distance_major_town'
		
	major_town <- major_town[order(major_town$WARD7),]
	comp_inf_II <- cbind(comp_inf_II,major_town)
	
#River (now I'm merging instead of binding)

	rivers <- readShapePoints('Rivers/river_distance/river_distance.shp')
	river_distance <- as.data.frame(rivers)
	vars <- c('COUNTY_A_1','DistKM')
	river_distance <- river_distance[vars]
	names(river_distance)[1] <- 'WARD8'
	names(river_distance)[2] <- 'distance_rivers'
		
	river_distance <- river_distance[order(river_distance$WARD8),]
	comp_inf_II <- cbind(comp_inf_II, river_distance)
	

#Water Points


	w_points <- readShapePoly('Kenya_waterpoints1/sum_water_points/sum_water.shp')
	water_points <- as.data.frame(w_points)
	vars <- c('COUNTY_A_1','Sum_count')
	water_points <- water_points[vars]
	names(water_points)[1] <- 'WARD9'
	names(water_points)[2] <- 'water_points'
		
	water_points <- water_points[order(water_points$WARD9),]
	comp_inf_II <- cbind(comp_inf_II, water_points)

	
#Offgrid 

	off_grid <- readShapePoly('solar_and_microgrid/testoffgridjoin.shp')	
	offgrid <- as.data.frame(off_grid)
	vars <- c('COUNTY_A_1','Sum_Count')
	offgrid <- offgrid[vars]
	names(offgrid)[1] <- 'WARD10'
	names(offgrid)[2] <- 'num_offgrid'
	
	offgrid <- offgrid[order(offgrid$WARD10),]
	comp_inf_II <- cbind(comp_inf_II, offgrid)
	
	
#Transmission Grid Distance

	trans <- readShapePoints('Kenya Transmission - AIKP/distance_transmission/distance_transmission.shp')
	trans_grid <- as.data.frame(trans)
	vars <- c('COUNTY_A_1','dist_trans')
	trans_grid <- trans_grid[vars]
	names(trans_grid)[1] <- 'WARD11'
	names(trans_grid)[2] <- 'dist_trans'
	
	trans_grid <- trans_grid[order(trans_grid$WARD11),]
	comp_inf_II <- cbind(comp_inf_II, trans_grid)
	
	
#Equity Bank Locations 

	bank <- readShapePoly('Finance/count_finance_equity.shp')
	equity_bank <- as.data.frame(bank)
	vars <- c('COUNTY_A_1','Sum_count')
	equity_bank <- equity_bank[vars]
	names(equity_bank)[1] <- 'WARD12'
	names(equity_bank)[2] <- 'num_equity'
	
	equity_bank <- equity_bank[order(equity_bank$WARD12),]
	comp_inf_II <- cbind(comp_inf_II, equity_bank)




#MPESA Locations (W13)






#Gini Coefficient (W14)







#Ward Area (W14)

	area <- readShapePoly('Area/ward_area.shp')
	ward_area <- as.data.frame(area)
	vars <- c('COUNTY_A_1','ward_area')
	ward_area <- ward_area[vars]
	names(ward_area)[1] <- 'WARD14'
	names(ward_area)[2] <- 'ward_area'
	
	ward_area <- ward_area[order(ward_area$WARD14),]
	comp_inf_II <- cbind(comp_inf_II, ward_area)


	
####### Merge Begins	
	
	#Keeping only the vars we need ** Remember to update as variables are added **
	comp_vars <- c('WARD','crop_diversity','crop_intensity','gov_irrigation','prop_irrigation','small_irrigation','distance_major_roads','distance_second_roads','distance_major_town','water_points','num_equity','distance_rivers','num_offgrid','ward_area','dist_trans')
	comp_inf_II <- comp_inf_II[comp_vars]
	
	
	#Merge with complementary infrastructure I 
	setwd('/Users/Diego/Desktop/IBM/Raw_Data')
	previous_comp <- read.csv('complementary_infrastructure_I.csv')

	comp_inf <- merge(previous_comp,comp_inf_II,by='WARD',all= TRUE)
	
#Calculate Population density (people/km2) before exporting!

comp_inf$pop_density <- comp_inf$total_pop/comp_inf$ward_area
	
	
#CHECK THAT THERE ARE NO REPEATS BY WARD NAME
non_duplicates <- comp_inf[!duplicated(comp_inf$WARD), ]

#Getting rid of the NAs
non_duplicates[is.na(non_duplicates)] <-0 

#Export Complete File to Begin doing Some Stats
setwd('/Users/Diego/Desktop/IBM/Raw_Data/')
write.csv(non_duplicates,'comp_inf_for_stats.csv')
	
	
	
	

