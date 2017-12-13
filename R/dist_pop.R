#' US Census district population function
#' @description  Calculates district populations proportional to census tracts
#' @param leg SpatialPolygonsDataFrame of legislative districts
#' @param collev Character vector naming the column containing district number
#' @param ctracts SpatialPolygonsDataFrame of state census tracts
#' @details This function uses district census data downloaded
#' using the tracts functions from the tigress package, and

dist_pop <- function(leg, collev, ctracts = state_tracts_pop) {
  merge( x = leg,
         y = as.data.frame(lapply(as.data.frame(t(sapply(1:length(leg@data[, collev]),
                                                         function(z) {
      sd_c_int <- intersect(x = leg[leg@data[, collev] == z, ],
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
      as.numeric(levels(x))[x])), by = collev)
}



dist_pop(state_lower, "SLDLST")

tm_shape(dist_pop(state_lower, "SLDLST"), projection = map_proj) +
  tm_polygons(dem, style="quantile", palette = pal,  title= dem) +
  tm_layout(title = "stuff", title.position = c("center", "top"),
            title.size = 1.3, frame = "transparent",
            inner.margins = c(.1, .1, .2, .1)) +
  tm_legend(text.size= 1.4,
            title.size=2,
            position = c(.2, .02),
            bg.color = "white",
            bg.alpha=.0,
            frame="transparent",
            height=.4,
            width =.6)
