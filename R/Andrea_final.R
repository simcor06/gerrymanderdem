install.packages("tigris")
install.packages("tidycensus")

library(tigris)
library(devtools)
library(tidycensus)
library(tidyverse)
library(rgdal)
library(rgeos)
library(maps)

#FOLLOWING ARE SPATIAL-RELATED OPERATIONS:
#wi state
wi<- states(WI)
plot(wi)
#WI Census Tracts
WI_tracts<-tracts(state = 55, cb= TRUE)
plot(WI_tracts)
View(WI_tracts)

#WI Lower
WI_lower<-state_legislative_districts("WI", house = "lower", cb = TRUE,
                            year = NULL)
plot(WI_lower)
View(WI_lower)
#WI Upper
WI_upper<-state_legislative_districts("WI", house = "upper", cb = TRUE,
                                      year = NULL)
plot(WI_upper)

View(WI_upper)
#congressional district not working!
fed_congress<-congressional_districts(cb = TRUE, year = NULL)
fed_congress_state <- fed_congress[fed_congress@data$STATEFP == 55,]
plot(fed_congress_state)
View(fed_congress)

#TRANSFORM
WI_tracts_transformed<-spTransform(WI_tracts,CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))
proj4string(WI_tracts_transformed)

#rgeos works!
wi_tracts_area<-gArea(WI_tracts_transformed, byid = TRUE)/1000000
wi_tracts_area

WI_tracts2<-WI_tracts_transformed
WI_tracts2$area<-gArea(WI_tracts2, byid = TRUE)/1000000

head(WI_tracts2)
View(WI_tracts2)

#area each census tract doesn't work!
area_wi_tracts<-WI_tracts$area_km <-area(WI_tracts)/1000000
area_wi_tracts

crs(WI_tracts)
class(WI_tracts)
class(wi_tracts_area)
View(area_wi_tracts)

#FOLLOWING ARE DEMOGRAPHIC-RELATED OPERATIONS

#Key to access data
api.key.install(key="eb3bcf3522034ac94e72887fc4881142b44caf29")
geo<-geo.make(state="WI")

#to see census variables - census table codes
pop_attempt <- load_variables(2010, dataset = "sf1")
View(pop_attempt)

#population in households by age and  race works!
#variables used when searching: 2010 sf1, age, race, census tracts.
#black,native, asian, hispanic, pacific islander, white,does not include two or more races & some other race)

#population_race_over18<-get_decennial(geography = "tract", variables =c("P016B003","P036C003","P016D003","P016H003","P036E003", "P016A003"), state = "WI",year = 2010)
#View(population_race_over18)
#class(population_race_over18)

all<-get_decennial(geography = "tract", variables = "P0100001", state= "WI", year=2010)
White_over18<-get_decennial(geography = "tract", variables ="P016A003",state = "WI",year = 2010)
Black_over18<-get_decennial(geography = "tract", variables ="P016B003",state = "WI",year = 2010)
American_Indian_over18<-get_decennial(geography = "tract", variables ="P036C003",state = "WI",year = 2010)
Asian_over18<-get_decennial(geography = "tract", variables ="P016D003",state = "WI",year = 2010)
Pacific_Islander_over18<-get_decennial(geography = "tract", variables ="P036E003",state = "WI",year = 2010)
Hispanic_over18<-get_decennial(geography = "tract", variables ="P016H003",state = "WI",year = 2010)

View(all)


#cbind - this works!

Universe_18<-cbind(all,White_over18$value,Black_over18$value,American_Indian_over18$value,Asian_over18$value,Pacific_Islander_over18$value,Hispanic_over18$value)
View(Universe_18)

#change column names
names(Universe_18)[4:10]<-c("Entire population","White","Black","American Indian","Asian","Hawaian/Pacific Islander","Hispanic")
View(Universe_18)


#merge census tracts df with demographics data
WI_demo_tracts<-merge(WI_tracts2,Universe_18, by = "GEOID")
View(WI_demo_tracts)
plot(WI_demo_tracts)
class(WI_demo_tracts)
NEW<-WI_demo_tracts[,-(5:6)]
View(NEW)




