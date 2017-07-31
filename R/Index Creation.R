#########################################
#Analysis: Stats on the complementary infrastructure data set, descriptive statitistics, map of metric through Kenya, and 

#NOTE: Uses the 'complementary_infrastructure_II' code, that's where 'comp_inf_for_stats.csv' gets created. 

#Libraries
library(maptools)
library(RColorBrewer)
library(corrgram)
library(reshape)
library(data.table)

setwd('/Users/diego/Desktop/Projects_Code/natural_infrastructural_capital/Raw_Data')
stats_analysis <- read.csv('comp_inf_for_stats.csv')


####Rescaling variables before putting everything togther

#Number of facilities per 1000 People
stats_analysis$r_fac_1000 <- (stats_analysis$fac_1000 - min(stats_analysis$fac_1000))/(max(stats_analysis$fac_1000)) - min(stats_analysis$fac_1000)


#Number of electrified facilities per 1000 People
stats_analysis$r_elec_fac <- (stats_analysis$elec_fac_1000 - min(stats_analysis$elec_fac_1000))/(max(stats_analysis$elec_fac_1000)) - min(stats_analysis$elec_fac_1000)


#Population density (higher population density is better)
stats_analysis$r_pop_dens <- (stats_analysis$pop_density - min(stats_analysis$pop_density))/(max(stats_analysis$pop_density)) - min(stats_analysis$pop_density)


#Distance to major towns: Here a smaller distance is better so we'll take the opposite.
stats_analysis$r_dist_town <- ifelse(stats_analysis$distance_major_town !=0,1 - (stats_analysis$distance_major_town - min(stats_analysis$distance_major_town))/(max(stats_analysis$distance_major_town)) - min(stats_analysis$distance_major_town),0)


#Distance to grid: Here, the closer you are to the grid, the better it is -- as electricity is much more likely to be adopted the closer one is to it
stats_analysis$r_dist_grid <- ifelse(stats_analysis$dist_trans !=0,1- (stats_analysis$dist_trans - min(stats_analysis$dist_trans))/(max(stats_analysis$dist_trans)) - min(stats_analysis$dist_trans),0)



#Equity Branches *Remember -- per 1000 People*
stats_analysis$num_equity_1000 <- stats_analysis$num_equity/(stats_analysis$total_pop/1000)

stats_analysis$num_equity_1000[stats_analysis$num_equity_1000 == "NaN"] <- 0
stats_analysis$num_equity_1000[stats_analysis$num_equity_1000 == "Inf"] <- 0


stats_analysis$r_equity <- (stats_analysis$num_equity_1000 - min(stats_analysis$num_equity_1000,na.rm=TRUE))/(max(stats_analysis$num_equity_1000,na.rm=TRUE)) - min(stats_analysis$num_equity_1000,na.rm=TRUE)




#MPESA Agents Per Ward *Remember -- per 1000 People*





#Gini Coefficient
 
 
 

#Agro-Ecological Zones
 



#Distance to major roads: Here a smaller distance is better so we'll take the opposite.
stats_analysis$r_dist_roads <- ifelse(stats_analysis$distance_major_roads !=0,1 - (stats_analysis$distance_major_roads - min(stats_analysis$distance_major_roads))/(max(stats_analysis$distance_major_roads)) - min(stats_analysis$distance_major_roads),0)


#Distance to secondary roads: Here a smaller distance is better so we'll take the opposite.
stats_analysis$r_sec_roads <- ifelse(stats_analysis$distance_second_roads !=0,1 - (stats_analysis$distance_second_roads - min(stats_analysis$distance_second_roads))/(max(stats_analysis$distance_second_roads)) - min(stats_analysis$distance_second_roads),0)

#Crop Diversity
stats_analysis$r_crop_diversity <- (stats_analysis$crop_diversity - min(stats_analysis$crop_diversity))/(max(stats_analysis$crop_diversity)) - min(stats_analysis$crop_diversity)


#Crop Intensity: this is an average (average land under cultivation) so the only thing we do is divide by 100
stats_analysis$r_crop_intensity <- stats_analysis$crop_intensity/100 


#Distance to rivers
stats_analysis$r_dist_rivers <- ifelse(stats_analysis$distance_rivers !=0,1 - (stats_analysis$distance_rivers - min(stats_analysis$distance_rivers))/(max(stats_analysis$distance_rivers)) - min(stats_analysis$distance_rivers),0)

#Number of water points *Remeber per 1000 People*
stats_analysis$w_1000 <- stats_analysis$water_points/(stats_analysis$total_pop/1000)
stats_analysis$w_1000[stats_analysis$w_1000 == "NaN"] <- 0
stats_analysis$w_1000[stats_analysis$w_1000 == "Inf"] <- 0

stats_analysis$r_wpoints <- (stats_analysis$w_1000 - min(stats_analysis$w_1000,na.rm=TRUE))/(max(stats_analysis$w_1000,na.rm=TRUE) - min(stats_analysis$w_1000, na.rm=TRUE))


#Number of irrigation projects
stats_analysis$irrigation_proj <- stats_analysis$gov_irrigation + stats_analysis$prop_irrigation + stats_analysis$small_irrigation

stats_analysis$r_irrigation <- (stats_analysis$irrigation_proj - min(stats_analysis$irrigation_proj))/(max(stats_analysis$irrigation_proj)) - min(stats_analysis$irrigation_proj)


#Creating one of the dependent variables *OFFGRID*
stats_analysis$offgrid <- ifelse(stats_analysis$num_offgrid >0,1,0)



####################
#Creating a file that puts the entire index together *if you added variables, don't forget to include them here!*

vars <- c('WARD','r_fac_1000','r_elec_fac','r_dist_town','r_dist_roads','r_sec_roads','r_crop_diversity','r_crop_intensity','r_dist_rivers','r_irrigation','r_pop_dens','r_wpoints','r_equity','r_dist_grid','offgrid')
index_vars <- stats_analysis[vars]

#Note water points changes the results!

index_vars$index <-stats_analysis$r_fac_1000+stats_analysis$r_elec_fac+stats_analysis$r_dist_town+stats_analysis$r_dist_roads+stats_analysis$r_sec_roads+stats_analysis$r_crop_diversity+stats_analysis$r_crop_intensity+stats_analysis$r_dist_rivers+stats_analysis$r_irrigation + stats_analysis$r_pop_dens + stats_analysis$r_equity 
index_vars$nat_cap <- stats_analysis$r_crop_diversity+stats_analysis$r_crop_intensity+stats_analysis$r_dist_rivers+stats_analysis$r_irrigation
index_vars$inf_cap <- stats_analysis$r_fac_1000+stats_analysis$r_elec_fac+stats_analysis$r_dist_town+stats_analysis$r_dist_roads+stats_analysis$r_sec_roads + stats_analysis$r_pop_dens + stats_analysis$r_equity

#Suggest removing these two variables from the analysis as they are not updated.
#stats_analysis$r_dist_grid
#stats_analysis$r_wpoints 

######################Statistics
	
########
#Corgram of our variables
vars <- c('r_fac_1000','r_elec_fac','r_dist_town','r_dist_roads','r_sec_roads','r_crop_diversity','r_crop_intensity','r_dist_rivers','r_irrigation','r_pop_dens','r_wpoints','r_equity','r_dist_grid','index')
corr_vars <- index_vars[vars]

names(corr_vars)[1] <- 'Facilities'
names(corr_vars)[2] <- 'E.Facilities'
names(corr_vars)[3] <- 'Town Distance'
names(corr_vars)[4] <- 'Road Distance'
names(corr_vars)[5] <- 'S.Road Distance'
names(corr_vars)[6] <- 'C.Diversity'
names(corr_vars)[7] <- 'C.Intensity'
names(corr_vars)[8] <- 'River Distance'
names(corr_vars)[9] <- 'Irrigation'
names(corr_vars)[10] <- 'P.Density'
names(corr_vars)[11] <- 'Water Points'
names(corr_vars)[12] <- 'Equity'
names(corr_vars)[13] <- 'Grid'
names(corr_vars)[14] <- 'Index'

corrgram(corr_vars, order=TRUE, lower.panel=panel.shade,upper.panel=panel.pie, text.panel=panel.txt,main="Nothing")

	
##########
#Group means places with offgrid vs no offgrid are there any differences
#NOTE: Simple exploration denotes no real differences between the groups, actually, the other group is higher. This can be explained because there are many more wards that score much higher already -- they have. The quick stats below can be used  

offgrid <- subset(index_vars,index_vars$offgrid == 1)
no_offgrid <- subset(index_vars,index_vars$offgrid == 0)


#Density
plot(density(offgrid$index))
plot(density(below$index))

#Groups that are below the offgrid mean (to map them below)
below <- subset(index_vars,index_vars$index < 4 & index_vars$offgrid != 1 )
above <- subset(index_vars,index_vars$index > 4 & index_vars$offgrid != 1 )

#Boxplot
boxplot(offgrid$inf_cap,no_offgrid$inf_cap,ylab='MED Index',names=c('Offgrid','No-Offgrid'),main='MED Index by Offgrid Status')

index_vars$offgrid <- as.factor(index_vars$offgrid )
ggplot(index_vars, aes(x=index_vars$offgrid, y=index_vars$inf_cap, fill=index_vars$offgrid)) + geom_boxplot() 
compare1 <- sapply(offgrid,mean,na.rm=TRUE)
compare2 <- sapply(above,mean,na.rm=TRUE)

compare1 <- as.data.frame(melt(compare1,id='WARD'))
compare2 <- as.data.frame(melt(compare2,id='WARD'))
compare <- cbind(compare1,compare2)

names(compare)[1] <- 'offgrid'
names(compare)[2] <- 'above'
compare$diff <- ((compare$offgrid - compare$above)/compare$offgrid)*100


############## 
#Logistic Regression: What are the factors that contribute to having or not having 

logistic <- glm(offgrid ~ r_fac_1000 + r_elec_fac + r_dist_town + r_dist_roads + r_sec_roads + r_crop_diversity + r_crop_intensity + r_dist_rivers + r_irrigation, data=index_vars, family='binomial')
summary(logistic)






###################
#Mapping the index

setwd('/Users/diego/Desktop/Projects_Code/natural_infrastructural_capital/Raw_Data/')
wards_shapefile <- readShapePoly('kenya_ag_data/wards/OGRGeoJSON.shp')
wards <- data.frame(wards_shapefile)

###Starting to plot our stats

	#Matching
	match_order <- match(wards$COUNTY_A_1, index_vars$WARD)
	index_vars$index[match_order]

	wards$index <- index_vars$index[match_order]
	
	#Histogram to create breaks for color buckets
	hist(wards$index)
	
	#Creating variables for the buckets
	map_breaks <- c(0,1,2,3,4,5,6,7)
	buckets <- cut(wards$index, breaks = map_breaks)
	numeric_buckets <- as.numeric(buckets)
	
	#Choosing colors for the map
	display.brewer.all()
	colors <- brewer.pal(7,"YlOrRd")
	colors[numeric_buckets]
	
	#Plotting our Nicaragua Map
	plot(wards_shapefile, col = colors[numeric_buckets])
	
###Legend

list <- c(0:8)

nclr <- 8
min <- 0
max <- 8
breaks <- (max-min)/nclr
plotclr <- brewer.pal(nclr,'YlOrRd')
plotvar <- list
class <- classIntervals(plotvar,nclr,style='fixed',fixedBreaks = seq(min,max,breaks))
colcode <- findColours(class,plotclr) 

legend('bottomleft',legend = names(attr(colcode,'table')),title='MED Score',fill=attr(colcode,'palette'),cex=0.56,bty='n')




##############################################
##############################################
# Stats from paper



#### 1. Stats comparing Wards with Offgrid projects vs Wards with no Offgrid Projects
index_vars$off_grid_type <- ifelse(index_vars$offgrid==1,"Off-grid Facilities","No Off-grid Facilities")

#Infrastructural Labels
inf_cap_1 <- quantile(index_vars$inf_cap,probs=c(0.01,0.99),na.rm=TRUE)[1]
inf_cap_99 <- quantile(index_vars$inf_cap,probs=c(0.01,0.99),na.rm=TRUE)[2]
index_vars$label_inf_cap <- ifelse(index_vars$inf_cap<inf_cap_1 | index_vars$inf_cap>inf_cap_99,as.character(index_vars$WARD),"")

#Med Index Labels
med_1 <- quantile(index_vars$index,probs=c(0.01,0.99),na.rm=TRUE)[1]
med_99 <- quantile(index_vars$index,probs=c(0.01,0.999999999),na.rm=TRUE)[2]
index_vars$label_med <- ifelse(index_vars$index<med_1 | index_vars$index>med_99,as.character(index_vars$WARD),"")


inf_cap <- ggplot(index_vars, aes(x=index_vars$off_grid_type, y=index_vars$inf_cap, fill=index_vars$off_grid_type)) + geom_boxplot(position=dodge) +
  theme_bw() + theme(axis.line = element_line(colour = "grey"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank()) +
  theme(legend.position="bottom") + ggtitle("Wards with and without Off-grid Projects") + theme(legend.position="none") + geom_text(aes(label=index_vars$label_inf_cap),size=2,vjust=1,hjust=-0.2,alpha=0.6) +
  xlab("") + ylab("Infrastructural Capital Score") + ylim(0,7)

nat_cap <- ggplot(index_vars, aes(x=index_vars$off_grid_type, y=index_vars$nat_cap, fill=index_vars$off_grid_type)) + geom_boxplot(position=dodge) +
  theme_bw() + theme(axis.line = element_line(colour = "grey"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank()) +
  theme(legend.position="bottom") + ggtitle("Wards with and without Off-grid Projects") + theme(legend.position="none") +
  xlab("") + ylab("Natural Capital Score") + ylim(0,7)

med_index <- ggplot(index_vars, aes(x=index_vars$off_grid_type, y=index_vars$index, fill=index_vars$off_grid_type)) + geom_boxplot(position=dodge) +
  theme_bw() + theme(axis.line = element_line(colour = "grey"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank()) +
  theme(legend.position="bottom") + ggtitle("Wards with and without Off-grid Projects") + theme(legend.position="none") + geom_text(aes(label=index_vars$label_med),size=2,vjust=1,hjust=-0.2,alpha=0.6) +
  xlab("") + ylab("MED Index Score")+ ylim(0,7)



####2 Reading shapefile with different offgrid data

wards_power_shapefile <- readShapePoly('kenya_ag_data/offgrid_infrastructure/offgrid_infrastructure.shp')
wpower_df <- data.frame(wards_power_shapefile) %>% mutate(WARD=COUNTY_A_1) %>% select(WARD,Company,Type)

#Subsetting and Relabling for doing the boxplots
power_wards <- merge(index_vars,wpower_df,by=c('WARD'))
power_wards_offgrid <- subset(power_wards,power_wards$Type!='power')
power_wards_offgrid$Label <- ifelse(power_wards_offgrid$Company == 'government' & power_wards_offgrid$Type =='microgrid','Government Microgrid',ifelse(power_wards_offgrid$Type =='solar','Solar Entrepreneur',ifelse(power_wards_offgrid$Company != 'government' & power_wards_offgrid$Type =='microgrid','Entrepreneur Microgrid','Greenfield')))

#Infrastructural Offgrid Labels
inf_cap_1 <- quantile(power_wards_offgrid$inf_cap,probs=c(0.01,0.99),na.rm=TRUE)[1]
inf_cap_99 <- quantile(power_wards_offgrid$inf_cap,probs=c(0.01,0.99),na.rm=TRUE)[2]
power_wards_offgrid$label_inf_cap <- ifelse(power_wards_offgrid$inf_cap<inf_cap_1 | power_wards_offgrid$inf_cap>inf_cap_99,as.character(index_vars$WARD),"")

inf_cap_offgrid <- ggplot(power_wards_offgrid, aes(x=power_wards_offgrid$Label, y=power_wards_offgrid$inf_cap, fill=power_wards_offgrid$Label)) + geom_boxplot(position=dodge) +
  theme_bw() + theme(axis.line = element_line(colour = "grey"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank()) +
  theme(legend.position="bottom") + theme(legend.position="none") +  xlab("") + ylab("Infrastructural Capital Score")+ ylim(0,7)

nat_cap_offgrid <- ggplot(power_wards_offgrid, aes(x=power_wards_offgrid$Label, y=power_wards_offgrid$nat_cap, fill=power_wards_offgrid$Label)) + geom_boxplot(position=dodge) +
  theme_bw() + theme(axis.line = element_line(colour = "grey"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank()) +
  theme(legend.position="bottom") + theme(legend.position="none") +  xlab("") + ylab("Natural Capital Score") + ylim(0,7)

index_offgrid <- ggplot(power_wards_offgrid, aes(x=power_wards_offgrid$Label, y=power_wards_offgrid$index, fill=power_wards_offgrid$Label)) + geom_boxplot(position=dodge) +
  theme_bw() + theme(axis.line = element_line(colour = "grey"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank()) +
  theme(legend.position="bottom") + theme(legend.position="none") +  xlab("") + ylab("MED Index Score") + ylim(0,7)


#### Mean stats by group


