library(rgeos)
library(sp)
library(raster)


# Function for Creating State Lower SpatialPolygonDataFrames with Population
#based on area of each census tract the district boundary covers
sld_pop <- merge(x = state_lower, y = t(sapply(1:length(state_lower@data$SLDLST), function(z) {
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

})), by = "SLDLST")

View(sld_pop)

# Function for Creating State Upper SpatialPolygonDataFrames with Population
#based on area of each census tract the district boundary covers
sud_pop <- merge(x = state_upper, y = t(sapply(1:length(state_upper@data$SLDUST), function(z) {
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

})), by = "SLDUST")


View(sud_pop)

# Function for creating State Federal Congressional District SpatialPolygonDataFrames with Population
#based on area of each census tract the district boundary covers

sfcd_pop <- merge(x = fed_congress_state, y = t(sapply(1:length(fed_congress_state@data$CD114FP), function(z) {
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

})), by = "CD114FP")



View(sfcd_pop)

