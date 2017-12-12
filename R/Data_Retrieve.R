#' gerrymanderdem package
#'
#' This package allows you to find demographic data for voting age populations for different legislative districs
#' keywords voting gerrymander census
#' @export
#' @examples
#' gerrymanderdem

library(tigris)
library(tidycensus)
library(rgdal)
library(rgeos)
library(sp)
library(raster)
library(RColorBrewer)
library(classInt)
library(tmap)

####### FOLLOWING ARE SPATIAL-RELATED OPERATIONS ########

#### Preparatory Objects#####
FIPS <- 55 ## Currently Wisconsin ##
#Must obtain a Key to access cenus data here: https://api.census.gov/data/key_signup.html
key <- "eac005cb98e4d960398fd3fef8d7cb1e9bbe8409"
## Proj4 from: http://www.spatialreference.org/ref/esri/102003/. Albers Equal Area for USA
aea_US <- ("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
#############

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

census_api_key(key, install = TRUE)
all<-get_decennial(geography = "tract", variables = "P0100001", state= FIPS, year=2010)
White_over18<-get_decennial(geography = "tract", variables ="P016A003",state = FIPS, year = 2010)
Black_over18<-get_decennial(geography = "tract", variables ="P016B003",state = FIPS, year = 2010)
American_Indian_over18<-get_decennial(geography = "tract", variables ="P036C003",state = FIPS, year = 2010)
Asian_over18<-get_decennial(geography = "tract", variables ="P016D003",state = FIPS, year = 2010)
Pacific_Islander_over18<-get_decennial(geography = "tract", variables ="P036E003",state = FIPS, year = 2010)
Hispanic_over18<-get_decennial(geography = "tract", variables ="P016H003",state = FIPS, year = 2010)




#cbind

Universe_18<- cbind(all,White_over18$value,Black_over18$value,American_Indian_over18$value,Asian_over18$value,Pacific_Islander_over18$value,Hispanic_over18$value)


#change column names
names(Universe_18)[4:10]<-c("Pop_Total","Pop_White","Pop_Black","Pop_American_Indian","Pop_Asian","Pop_Hawaiian_Pacific_Islander","Pop_Hispanic")



#merge census tracts df with demographics data
state_tracts_pop <- merge(state_tracts,Universe_18, by = "GEOID")

#############################################################################################
############EVERYTHING BELOW THIS IS THE INTERSECTION OF TRACTS AND DISTRICTS################
#############################################################################################

# Function for Creating State Lower DataFrames with Population
# Based on area of each census tract the district boundary covers
sld_pop_table <- as.data.frame(t(sapply(1:length(state_lower@data$SLDLST), function(z) {
  sd_c_int <- intersect(x =  state_lower[state_lower@data$SLDLST == z,],
                        y = state_tracts_pop)
  dc_area <- gArea(spgeom = sd_c_int, byid = TRUE)/1000000
  percentage <- dc_area/sd_c_int@data$AREA
  Pop_Total <- round(sum(sd_c_int@data$Pop_Total*percentage))
  Pop_White <- round(sum(sd_c_int@data$Pop_White*percentage))
  Pop_Black <- round(sum(sd_c_int@data$Pop_Black*percentage))
  Pop_American_Indian <- round(sum(sd_c_int@data$Pop_American_Indian*percentage))
  Pop_Asian <- round(sum(sd_c_int@data$Pop_Asian*percentage))
  Pop_Hawaian_Pacific_Islander <- round(sum(sd_c_int@data$Pop_Hawaian_Pacific_Islander*percentage))
  Pop_Hispanic <- round(sum(sd_c_int@data$Pop_Hispanic*percentage))
  unlist(data.frame(state_lower[state_lower@data$SLDLST == z,], Pop_Total, Pop_White, Pop_Black,
                    Pop_American_Indian, Pop_Asian, Pop_Hawaian_Pacific_Islander,
                    Pop_Hispanic))

})))
# converting factors in  data.frame to numeric
sld_pop_table[] <- lapply(sld_pop_table, function(x)
  as.numeric(levels(x))[x])

# merging with  state lower districts
sld_pop <- merge(x = state_lower, y = sld_pop_table, by = "SLDLST")

View(sld_pop)

# Function for Creating State Upper DataFrames with Population
# Based on area of each census tract the district boundary covers
sud_pop_table <- as.data.frame(t(sapply(1:length(state_upper@data$SLDUST), function(z) {
  sd_c_int <- intersect(x =  state_upper[state_upper@data$SLDUST == z,],
                        y = state_tracts_pop)
  dc_area <- gArea(spgeom = sd_c_int, byid = TRUE)/1000000
  percentage <- dc_area/sd_c_int@data$AREA
  Pop_Total <- round(sum(sd_c_int@data$Pop_Total*percentage))
  Pop_White <- round(sum(sd_c_int@data$Pop_White*percentage))
  Pop_Black <- round(sum(sd_c_int@data$Pop_Black*percentage))
  Pop_American_Indian <- round(sum(sd_c_int@data$Pop_American_Indian*percentage))
  Pop_Asian <- round(sum(sd_c_int@data$Pop_Asian*percentage))
  Pop_Hawaian_Pacific_Islander <- round(sum(sd_c_int@data$Pop_Hawaian_Pacific_Islander*percentage))
  Pop_Hispanic <- round(sum(sd_c_int@data$Pop_Hispanic*percentage))
  unlist(data.frame(state_upper[state_upper@data$SLDUST == z,], Pop_Total, Pop_White, Pop_Black,
                    Pop_American_Indian, Pop_Asian, Pop_Hawaian_Pacific_Islander,
                    Pop_Hispanic))

})))

# converting factors in  data.frame to numeric
sud_pop_table[] <- lapply(sud_pop_table, function(x)
  as.numeric(levels(x))[x])

# merging with  state lower districts
sud_pop <- merge(x = state_upper, y = sud_pop_table , by = "SLDUST")


View(sud_pop)

# Function for creating State Federal Congressional District DataFrames with Population
# Based on area of each census tract the district boundary covers

sfcd_pop_table <- as.data.frame(t(sapply(1:length(fed_congress_state@data$CD114FP), function(z) {
  sd_c_int <- intersect(x =  fed_congress_state[fed_congress_state@data$CD114FP == z,],
                        y = state_tracts_pop)
  dc_area <- gArea(spgeom = sd_c_int, byid = TRUE)/1000000
  percentage <- dc_area/sd_c_int@data$AREA
  Pop_Total <- round(sum(sd_c_int@data$Pop_Total*percentage))
  Pop_White <- round(sum(sd_c_int@data$Pop_White*percentage))
  Pop_Black <- round(sum(sd_c_int@data$Pop_Black*percentage))
  Pop_American_Indian <- round(sum(sd_c_int@data$Pop_American_Indian*percentage))
  Pop_Asian <- round(sum(sd_c_int@data$Pop_Asian*percentage))
  Pop_Hawaian_Pacific_Islander <- round(sum(sd_c_int@data$Pop_Hawaian_Pacific_Islander*percentage))
  Pop_Hispanic <- round(sum(sd_c_int@data$Pop_Hispanic*percentage))
  unlist(data.frame(fed_congress_state[fed_congress_state@data$CD114FP == z,], Pop_Total, Pop_White, Pop_Black,
                    Pop_American_Indian, Pop_Asian, Pop_Hawaian_Pacific_Islander,
                    Pop_Hispanic))

})))

# converting factors in  data.frame to numeric
sfcd_pop_table[] <- lapply(sfcd_pop_table, function(x)
  as.numeric(levels(x))[x])

# merging with federal congressional districts
sfcd_pop <- merge(x = fed_congress_state, y = sfcd_pop_table , by = "CD114FP")

View(sfcd_pop)

#############################################################################################
##################EVERYTHING BELOW THIS IS FOR CREATING MAPS OF RESULTS#####################
#############################################################################################

# Good Walkthrough sppplot functionality https://cengel.github.io/rspatial/4_Mapping.nb.html#choropleth-mapping-with-spplot
## 3 district types image
par(mar = c(0, 0, 2, 0))
districts_pop <- list(sld_pop, sud_pop, sfcd_pop)
districts <- c("State Lower House", "State Upper House", "Federal Districts")
cols <- c("red", "blue", "green")
par(mfrow = c(1, 3))
for(i in 1:length(districts_pop)) {
  plot(districts_pop[[i]], main = districts[i], col = "grey")
}

## State upper District one overlaid with all the tracts it intersects plot
par(mar = c(0, 0, 0, 0))
plot(state_tracts[which(gIntersects(spgeom1 = state_upper[state_upper@data$SLDUST == 1,],
                                    spgeom2 = state_tracts, byid = TRUE )), ], col = "grey")
plot(state_tracts, col = "grey", border = "red", add = TRUE)
plot(state_upper[state_upper@data$SLDUST == 1,],
     border = "transparent", col = rgb(0, 0, 1, .4), add = TRUE)





## displaying upper District Total Populations

display.brewer.all(type="seq")
pal <- brewer.pal(5, "Blues")# we select 4 colors from the palette
breaks_qt <- classIntervals(sud_pop@data$Pop_Black, n = 5)
br <- breaks_qt$brks
offs <- 0.0000001
br[1] <- br[1] - offs
br[length(br)] <- br[length(br)] + offs
sud_pop@data$Pop_Black_R_bracket <- cut(sud_pop@data$Pop_Black, br)
spplot(sud_pop, "Pop_Black_R_bracket", main = "State Upper District Black Populations", col.regions = pal)

##Displaying Lower District Population
pal <- brewer.pal(5, "Blues")# we select 4 colors from the palette
breaks_qt <- classIntervals(sld_pop@data$Pop_Black, n = 5)
br <- breaks_qt$brks
offs <- 0.0000001
br[1] <- br[1] - offs
br[length(br)] <- br[length(br)] + offs
sld_pop@data$Pop_Black_R_bracket <- cut(sld_pop@data$Pop_Black, br)
spplot(sld_pop, "Pop_Black_R_bracket", main = "State Lower District Black Populations", col.regions = pal)

## Displaying Federal district populations
pal <- brewer.pal(5, "Blues")# we select 4 colors from the palette
breaks_qt <- classIntervals(sfcd_pop@data$Pop_Black, n = 5)
br <- breaks_qt$brks
offs <- 0.0000001
br[1] <- br[1] - offs
br[length(br)] <- br[length(br)] + offs
sfcd_pop@data$Pop_Black_R_bracket <- cut(sfcd_pop@data$Pop_Black, br)
spplot(sfcd_pop, "Pop_Black_R_bracket", main = "Federal Congressional District Black Populations", col.regions = pal)

# check this out for tmap help https://cran.r-project.org/web/packages/tmap/vignettes/tmap-nutshell.html
pal <- brewer.pal(5, "YlOrRd")
tm_shape(sld_pop) +
  tm_polygons("Pop_Total", style="quantile", palette = pal,  title="Wisconsin Lower House District Populations") +
  tm_legend(text.size=.8,
            title.size=1.5,
            position = c("right","top"),
            bg.color = "white",
            bg.alpha=.2,
            frame="transparent",
            height=.18)
