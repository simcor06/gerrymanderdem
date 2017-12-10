library(RColorBrewer)
library(classInt)
library(GISTools)

## Plotting Results
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

### Population by district Plotting using sp package
display.brewer.all(type="seq")
pal <- brewer.pal(9, "YlOrRd") # we select 8 colors from the palette
breaks_qt <- classIntervals(sud_pop@data$Pop_Total, n = 9)
br <- breaks_qt$brks
offs <- 0.0000001
br[1] <- br[1] - offs
br[length(br)] <- br[length(br)] + offs
sud_pop@data$Pop_Total_R_bracket <- cut(sud_pop@data$Pop_Total, br)
spplot(sud_pop, "Pop_Total_R_bracket", col.regions = pal)

### Using GISTools Package
shd <-  auto.shading(sud_pop@data$Pop_Total)
choro.legend(bbox(sud_pop)["x","max"] -300000, bbox(sud_pop)["y","min"]+ 300000  , shd)
title("Wisconsin State Upper House District Populations")
choropleth(sud_pop, sud_pop@data$Pop_Total)
bbox(sud_pop)["x","max"]
bbox(sud_pop)["y","min"]
#Congressional, State Upper, adn State Lower House Maps
wisconsin <- list(sfcd_pop, sud_pop, sld_pop)
districts <- c("Federal Districts", "State Upper House", "State Lower House")
par(mfrow = c(1, 3))
for(i in 1:length(wisconsin)) {
  plot(wisconsin[[i]], main = districts[i], col = cols[i])
}






