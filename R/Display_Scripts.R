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

### DOESN"T WORK#### Population by district Plotting using sp package
par(mfrow = c(1, 3))
pal <- brewer.pal(5, "YlOrRd")
for(i in 1:length(districts_pop)) {
  breaks_qt <- classIntervals(districts_pop[[i]]@data$Pop_Total, n = 5)
  br <- breaks_qt$brks
  offs <- 0.0000001
  br[1] <- br[1] - offs
  br[length(br)] <- br[length(br)] + offs
  districts_pop[[i]]@data$Pop_Total_R_bracket <- cut(districts_pop[[i]]@data$Pop_Total, br)
  spplot(districts_pop[[i]], "Pop_Total_R_bracket", col.regions = pal)
}


## displaying upper District Total Populations
##display.brewer.all(type="seq")
par(mfrow = c(1, 3))
pal <- brewer.pal(5, "YlOrRd")# we select 4 colors from the palette
breaks_qt <- classIntervals(sud_pop@data$Pop_Total, n = 5)
br <- breaks_qt$brks
offs <- 0.0000001
br[1] <- br[1] - offs
br[length(br)] <- br[length(br)] + offs
sud_pop@data$Pop_Total_R_bracket <- cut(sud_pop@data$Pop_Total, br)
spplot(sud_pop, "Pop_Total_R_bracket", main = "State Upper District Populations", col.regions = pal)

##Displaying Lower District Population
pal <- brewer.pal(5, "YlOrRd")# we select 4 colors from the palette
breaks_qt <- classIntervals(sld_pop@data$Pop_Total, n = 5)
br <- breaks_qt$brks
offs <- 0.0000001
br[1] <- br[1] - offs
br[length(br)] <- br[length(br)] + offs
sld_pop@data$Pop_Total_R_bracket <- cut(sld_pop@data$Pop_Total, br)
spplot(sld_pop, "Pop_Total_R_bracket", main = "State Lower District Populations", col.regions = pal)

## Displaying Federal district populations
pal <- brewer.pal(5, "YlOrRd")# we select 4 colors from the palette
breaks_qt <- classIntervals(sfcd_pop@data$Pop_Total, n = 5)
br <- breaks_qt$brks
offs <- 0.0000001
br[1] <- br[1] - offs
br[length(br)] <- br[length(br)] + offs
sfcd_pop@data$Pop_Total_R_bracket <- cut(sfcd_pop@data$Pop_Total, br)
spplot(sfcd_pop, "Pop_Total_R_bracket", col.regions = pal)

###  DOESN't WORK### Using GISTools Package
shd <-  auto.shading(sud_pop@data$Pop_Total)
choro.legend(bbox(sud_pop)["x","max"] -300000, bbox(sud_pop)["y","min"]+ 300000  , shd)
title("Wisconsin State Upper House District Populations")
choropleth(sud_pop, sud_pop@data$Pop_Total)
bbox(sud_pop)["x","max"]
bbox(sud_pop)["y","min"]







