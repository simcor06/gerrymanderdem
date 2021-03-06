#' US Census district population function
#' @description  Calculates district populations proportional to census tracts
#' @param leg SpatialPolygonsDataFrame of legislative districts
#' @param cname Character vector naming the column containing district numbers
#' @param ctracts SpatialPolygonsDataFrame of state census tracts with population data
#' @details This function uses district census data downloaded
#' using the tracts functions from the tigress package, and

sd_pop <- function(leg, cname, ctracts) {
  merge( x = leg,
         y = as.data.frame(lapply(as.data.frame(t(sapply(1:length(leg@data[, cname]),
                                                         function(z) {
      sd_c_int <- intersect(x = leg[leg@data[, cname] == z, ],
                              y = ctracts)
      dc_area <- gArea(spgeom = sd_c_int, byid = TRUE)
      perc <- dc_area / sd_c_int$AREA
      Pop_Total <- as.numeric(round(sum(sd_c_int$Pop_Total * perc)))
      Pop_White <- round(sum(sd_c_int$Pop_White * perc))
      Pop_Black <- round(sum(sd_c_int$Pop_Black * perc))
      Pop_American_Indian <- round(sum(sd_c_int$Pop_American_Indian * perc))
      Pop_Asian <- round(sum(sd_c_int$Pop_Asian * perc))
      v <- sd_c_int$Pop_Hawaiian_Pacific_Islander
      Pop_Hawaian_Pacific_Islander <- round(sum(v * perc))
      Pop_Hispanic <- round(sum(sd_c_int$Pop_Hispanic*perc))
      unlist(data.frame(leg[leg@data[, collev] == z,],
                        Pop_Total, Pop_White, Pop_Black,
                        Pop_American_Indian, Pop_Asian,
                        Pop_Hawaian_Pacific_Islander, Pop_Hispanic))
    }))), function(x)
      as.numeric(levels(x))[x])), by = cname)
}



