#' gerrymanderdem package
#'
#' This package allows you to find demographic data for voting age populations for different legislative districs
#' keywords voting gerrymander census
#' @export
#' @examples
#' gerrymanderdem

library(RColorBrewer)
library(classInt)
library(roxygen2)
library(tmap)

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





## displaying upper District Total Populations

display.brewer.all(type="seq")
#pal <- brewer.pal(5, "Blues")# we select 4 colors from the palette
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








