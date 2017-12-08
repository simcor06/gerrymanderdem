library(tigris)
library(devtools)
library(tidycensus)
library(tidyverse)
library(rgdal)
library(rgeos)

####### FOLLOWING ARE SPATIAL-RELATED OPERATIONS ########

#### Preparatory Objects#####
FIPS <- 55 ## Currently Wisconsin ##
#Must obtain a Key to access cenus data here: https://api.census.gov/data/key_signup.html
key <- "eac005cb98e4d960398fd3fef8d7cb1e9bbe8409"
## Proj4 from: http://www.spatialreference.org/ref/esri/102003/. Albers Equal Area for USA
aea_US <- ("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")

#State Census Tracts
state_tracts_NAD83<-tracts(state = FIPS, cb= TRUE)
state_tracts<-spTransform(state_tracts_NAD83, CRS(aea_US))
state_tracts@data$AREA<-gArea(state_tracts, byid = TRUE)/1000000

#State Lower House Districts
state_lower_NAD83<-state_legislative_districts(state = FIPS, house = "lower", cb = TRUE,
                            year = NULL)
state_lower<-spTransform(state_lower_NAD83, CRS(aea_US))
state_lower@data$SLDLST <- as.numeric(state_lower@data$SLDLST)

#State Upper House Districts
state_upper_NAD83<-state_legislative_districts(state = FIPS, house = "upper", cb = TRUE,
                                      year = NULL)
state_upper<-spTransform(state_upper_NAD83, CRS(aea_US))
state_upper@data$SLDUST <- as.numeric(state_upper@data$SLDUST)

#Federal Congressional Districts at state level
fed_congress_NAD83<-congressional_districts(cb = TRUE, year = NULL)
fed_congress_state_NAD83 <- fed_congress_NAD83[fed_congress_NAD83@data$STATEFP == FIPS,]
fed_congress_state<-spTransform(fed_congress_state_NAD83, CRS(aea_US))
fed_congress_state@data$CD114FP <- as.numeric(fed_congress_state@data$CD114FP)




###### FOLLOWING ARE DEMOGRAPHIC-RELATED OPERATIONS #######

#Key to access data
##api.key.install(key="eb3bcf3522034ac94e72887fc4881142b44caf29")
##geo<-geo.make(state=FIPS)

#to see census variables - census table codes
##pop_attempt <- load_variables(2010, dataset = "sf1")
##View(pop_attempt)

#population in households by age and  race works!
#variables used when searching: 2010 sf1, age, race, census tracts.
#black,native, asian, hispanic, pacific islander, white,does not include two or more races & some other race)

#population_race_over18<-get_decennial(geography = "tract", variables =c("P016B003","P036C003","P016D003","P016H003","P036E003", "P016A003"), state = "WI",year = 2010)
#View(population_race_over18)
#class(population_race_over18)
census_api_key(key)
all<-get_decennial(geography = "tract", variables = "P0100001", state= FIPS, year=2010)
White_over18<-get_decennial(geography = "tract", variables ="P016A003",state = FIPS, year = 2010)
Black_over18<-get_decennial(geography = "tract", variables ="P016B003",state = FIPS, year = 2010)
American_Indian_over18<-get_decennial(geography = "tract", variables ="P036C003",state = FIPS, year = 2010)
Asian_over18<-get_decennial(geography = "tract", variables ="P016D003",state = FIPS, year = 2010)
Pacific_Islander_over18<-get_decennial(geography = "tract", variables ="P036E003",state = FIPS, year = 2010)
Hispanic_over18<-get_decennial(geography = "tract", variables ="P016H003",state = FIPS, year = 2010)




#cbind - this works!

Universe_18<- cbind(all,White_over18$value,Black_over18$value,American_Indian_over18$value,Asian_over18$value,Pacific_Islander_over18$value,Hispanic_over18$value)


#change column names
names(Universe_18)[4:10]<-c("Pop_Total","Pop_White","Pop_Black","Pop_American_Indian","Pop_Asian","Pop_Hawaian_Pacific_Islander","Pop_Hispanic")



#merge census tracts df with demographics data
state_tracts_pop <- merge(state_tracts,Universe_18, by = "GEOID")





