#Analysis: Puts the rest of the complementary infrastructure data togehter.


library(maptools)
library(RColorBrewer)





#Reading the previous complementary infrastructure data from which we'll being our analysis.
setwd('/Users/Diego/Desktop/IBM/Raw_Data/')
comp_inf <- read.csv('complementary_infrastructure_I.csv')


#Agriculture
setwd('/Users/Diego/Desktop/IBM/Raw_Data/Kenya_Ag_Data')

	#Crop Diversity: Average number of crops grown
	ag <- readShapePoly('crop_diversity/crop_div_average/Join_Output_2.shp')
	crop_diversity <- as.data.frame(ag)
	vars <- c('COUNTY_A_1','Avg_SP_AVG')
	crop_diversity <- crop_diversity[vars]
	names(crop_diversity)[1] <- 'WARD'
	names(crop_diversity)[2] <- 'crop_diversity'
		
	comp_inf <- merge(comp_inf, crop_diversity,by = 'WARD')
	
	#Crop Intensity: Percent Land under cultivation
	intense <- readShapePoly('ke_crops_intensity/intensity_avg/Join_Output.shp')
	intensity <- as.data.frame(intense)
	vars <- c('COUNTY_A_1','Avg_TOTAL_')
	crop_intensity <- intensity[vars]
	names(crop_intensity)[1] <- 'WARD'
	names(crop_intensity)[2] <- 'crop_intensity'
		
	comp_inf <- merge(comp_inf, crop_intensity,by = 'WARD')	
	
#Irrigation 

	#Government irrigation projects *CHECK MISTAKE*
	gov_irr <- readShapePoly('ke_gov_irrigation/kev_gov_count/Join_Output.shp')
	gov_irrigation <- as.data.frame(gov_irr)
	vars <- c('COUNTY_A_1','Sum_long')
	gov_irrigation <- gov_irrigation[vars]
	names(gov_irrigation)[1] <- 'WARD'
	names(gov_irrigation)[2] <- 'gov_irrigation'
		
	comp_inf <- merge(comp_inf, gov_irrigation,by = 'WARD')
	
	#Proposed Irrigation *CHECK MISTAKE*
	prop_irr <- readShapePoly('ke_proposed_irrigation/prop_irrigation_count/Join_Output_2.shp')
	proposed_irrigation <- as.data.frame(prop_irr)
	vars <- c('COUNTY_A_1','Sum_long')
	gov_irrigation <- gov_irrigation[vars]
	names(gov_irrigation)[1] <- 'WARD'
	names(gov_irrigation)[2] <- 'gov_irrigation'
		
	comp_inf <- merge(comp_inf, gov_irrigation,by = 'WARD')
	
	#Small scale irrigation
	small_irr <- readShapePoly('ke_small_scale_irrigation/count_small_irrigation/Join_Output.shp')
	small_irrigation <- as.data.frame(small_irr)
	vars <- c('COUNTY_A_1','Sum_count')
	small_irrigation <- small_irrigation[vars]
	names(small_irrigation)[1] <- 'WARD'
	names(small_irrigation)[2] <- 'small_irrigation'
		
	comp_inf <- merge(comp_inf, small_irrigation,by = 'WARD')
	
	
	
#Roads and Towns

	#Major roads
	major <- readShapePoints('ke_major_roads/distance_major/distance_major.shp')
	major_roads <- as.data.frame(major)
	vars <- c('COUNTY_A_1','distance')
	major_roads <- major_roads[vars]
	names(major_roads)[1] <- 'WARD'
	names(major_roads)[2] <- 'distance_major_roads'
		
	comp_inf <- merge(comp_inf, major_roads,by = 'WARD')
	
	#Secondary roads
	major <- readShapePoints('ke_major_roads/distance_major/distance_major.shp')
	major_roads <- as.data.frame(major)
	vars <- c('COUNTY_A_1','distance')
	major_roads <- major_roads[vars]
	names(major_roads)[1] <- 'WARD'
	names(major_roads)[2] <- 'distance_major_roads'
		
	comp_inf <- merge(comp_inf, major_roads,by = 'WARD')
	
	
	#Major towns -- *CHECK MISTAKE* missing town distance
	major <- readShapePoints('ke_major_towns/town_distance/distance_major.shp')
	major_roads <- as.data.frame(major)
	vars <- c('COUNTY_A_1','distance')
	major_roads <- major_roads[vars]
	names(major_roads)[1] <- 'WARD'
	names(major_roads)[2] <- 'distance_major_roads'
		
	comp_inf <- merge(comp_inf, major_roads,by = 'WARD')
	

	
	
	
	

