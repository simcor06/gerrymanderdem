library(RColorBrewer)

## Plotting Results
# Good Walkthrough sppplot functionality https://cengel.github.io/rspatial/4_Mapping.nb.html#choropleth-mapping-with-spplot
districts_pop <- list(sld_pop, sud_pop, sfcd_pop)
districts <- c("State Lower House", "State Upper House", "Federal Districts")
cols <- c("red", "blue", "green")
par(mfrow = c(1, 3))
for(i in 1:length(districts_pop)) {
  plot(districts_pop[[i]], main = districts[i], col = "grey")
}

### Population by district Plotting
spplot(sud_pop, "Pop_Black")
display.brewer.all(type="seq")
pal <- brewer.pal(5, "OrRd") # we select 5 colors from the palette
class(pal)


#Congressional, State Upper, adn State Lower House Maps
wisconsin <- list(sfcd_pop, sud_pop, sld_pop)
districts <- c("Federal Districts", "State Upper House", "State Lower House")
cols <- c("red", "blue", "green")
par(mfrow = c(1, 3))
for(i in 1:length(wisconsin)) {
  plot(wisconsin[[i]], main = districts[i], col = cols[i])
}


#Wisconsin Census Tracts Map


# Example Overlay of porblem geometry
par(mfrow = c(1,1))
#plot(Test_tracts, col = rgb(0, 0, 1, .3), border = "transparent", main = "Wisconsin State Assembly District 1")
plot(Test_int, col = rgb(0, .5, 0, .5), border = "red")
#plot(WI_sld[WI_sld@data$District_N == 1,], border = "red", add = TRUE)
plot(WI_ctracts, add = TRUE)

## State upper District one overlaid with all the tracts it intersects
par(mar = c(0, 0, 0, 0))
plot(sud_ctracts[[1]], col = "grey")
plot(state_upper[state_upper@data$SLDUST == 1,], add = TRUE,
     border = rgb(1, 0, 0, .5), col = rgb(0, 0, 1, .5))
plot(sud_c_int)
plot(state_upper[state_upper@data$SLDUST == 1,], add = TRUE,
     border = rgb(1, 0, 0, .5), col = rgb(0, 0, 1, .5))


