############# 
# Data: Secondary Schools, Health Care, and Trade Centers
# Analysis: Cleans the complementary infrastructure data, population, and shapefile. Makes sure that the ward names are the same so that they are easy to merge.
# Author: Diego

# Note: In the original complementary infrastructure CSV file there are several wards that do not have a ward name. I ommit values that have spaces, that have kept the word 'SUMMARY'/'SUMMARY ' or that have 'NEW WARD'. Ward names that have 'NEW WARD' are there because the actual name was not included in the original. Also, I spent time cleaning the data and understanding which names were wrong. Lines 55-60 are the ones that I used to identify conflicting names in a previous analysis and correct them starting on line 29. 

#############

#Getting all the libraries ready
#library(WriteXLS)
#library(sqldf)
#library(RSQLite)
#library(RSQLite.extfuns)
#library(DBI)
library(maptools)
library(chron)
library(plyr)
library(RColorBrewer)

#Uploading our complementary infrastructure data
setwd('/Users/Diego/Desktop/IBM/Raw_Data')
comp <- read.csv('complementary_infrastructure_processed.csv', strip.white = TRUE)
	#Creating a file just with wards so that I can do a merge easily (turning it into a character first). Then creating a subset to get rid of variables that I don't need.
	ward_comp <- data.frame(lapply(comp, as.character), stringsAsFactors=FALSE)
	
	#Keeping variables I need and deleting some that are not useful (NEW WARD... etc)
	ward_subset1 <- subset(ward_comp,ward_comp$WARD != 'NEW WARD',select=c(COUNTY,WARD,STATUS,TYPE))
	ward_subset2 <- subset(ward_subset1, ward_subset1$WARD != 'SUMMARY ',select=c(COUNTY,WARD,STATUS,TYPE))
	ward_subset3 <- subset(ward_subset2, ward_subset2$WARD != 'SUMMARY',select=c(COUNTY,WARD,STATUS,TYPE))
	ward_subset4 <- subset(ward_subset3, ward_subset3$WARD != '',select=c(COUNTY,WARD,STATUS,TYPE))
	
	#Only ward names 
	ward_subset5 <- ward_subset4[2]
	

##### Updating Ward Names

	to <- read.csv('correct_ward_names.csv', strip.white = TRUE)
	surf <- data.frame(lapply(to, as.character), stringsAsFactors=FALSE)
	surf_2 <- surf$WARD
	
	from <- read.csv('wrong_ward_names.csv', strip.white = TRUE)
	climb <- data.frame(lapply(from, as.character), stringsAsFactors=FALSE)
	climb_2 <- climb$WARD
		
		try_vector <- data.frame(lapply(ward_subset5, as.character), stringsAsFactors=FALSE)

		#Writing a function that will correct the names of all the wards
		for (i in 1:length(surf_2)+1) {
			try_vector[try_vector == climb_2[i]] <- surf_2[i]
		}
		#Use try_vector for the merge below		

		#Putting our correct names with all the other data.
		names(try_vector)[1] <- 'NEW_WARD'
		complementary_infrastructure <- cbind(try_vector,ward_subset4)
		vars <- c('NEW_WARD','COUNTY','STATUS','TYPE')
		complementary_infrastructure <- complementary_infrastructure[vars]
		names(complementary_infrastructure)[1] <- 'WARD'
		
#Writing the file that will use for stats				
write.csv(complementary_infrastructure,'complementary_infrastructure_correct_names.csv')
		

#### Reading in the population file 
pop <- read.csv('population.csv',strip.white = TRUE)
	pop$WARD <- as.character(pop$WARD)
					
##########				
#Uploading and creating a data frame for the complementary infrastructure in the region
setwd('/Users/Diego/Desktop/IBM/Raw_Data/Kenya_Ag_Data')
wards_shapefile <- readShapePoly('wards/OGRGeoJSON.shp')
wards <- data.frame(wards_shapefile)

	
#### Checks to see if we're doing a correct merge

#NOTE: Uses the 'try_vector' file and 'unique_wards' to see how many correct merges there are.

#This part of the code was used to idenfity the wards that I could not merge and that had wrong names
#Getting the names of the wards that have not been merged, but that we need:

	#Unique ward names in the file
	#unique_wards <- data.frame(lapply(wards[7], as.character), stringsAsFactors=FALSE)
	#Changing the variable name so that we can merge
	#names(unique_wards)[1] <- 'WARD'

#Merging to understand what wards are spelled correctly and which are not
#merged <- merge(try_vector,unique_wards,by='WARD')# I previously used this before I corrected the ward names OLD
#unique_merged <- unique(merged) #859 Merged Values out of possible 921. Not too bad.
#write.csv(unique_merged,'unique_merged.csv')

#Merging with our population file 'pop' to see variable names
#check_merge <- merge(pop,unique_merged,by='WARD')
#unique_check <- unique(check_merge)


#a1 <- pop
#a2 <- unique_merged
#require(sqldf)
#diff_comp <- sqldf('SELECT * FROM a1 EXCEPT SELECT * FROM a2')

#Exporting the file so I understand which variables are which
#write.csv(unique_merged,'/Users/Diego/Desktop/IBM/Raw_Data/merged.csv' )
#write.csv(diff_comp, '/Users/Diego/Desktop/IBM/Raw_Data/diff_comp.csv')
#write.csv(unique_wards,'/Users/Diego/Desktop/IBM/Raw_Data/unique_wards.csv')
###

























