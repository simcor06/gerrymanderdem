library(sp)
library(rgdal)
library(rgeos)



#Congressional, State Upper, adn State Lower House Maps
wisconsin <- list(WI_fcd115, WI_sud, WI_sld)
districts <- c("Federal Districts", "State Upper House", "State Lower House")
cols <- c("red", "blue", "green")
par(mfrow = c(1, 3))
for(i in 1:length(wisconsin)) {
  plot(wisconsin[[i]], main = districts[i], col = cols[i])
}


#Wisconsin Census Tracts Map
par(mfrow = c(1,1))
plot(WI_ctracts, main = "Wisconsin Census Tracts", col = "light grey")

# Example Overlay of porblem geometry
par(mfrow = c(1,1))
#plot(Test_tracts, col = rgb(0, 0, 1, .3), border = "transparent", main = "Wisconsin State Assembly District 1")
plot(Test_int, col = rgb(0, .5, 0, .5), border = "red")
#plot(WI_sld[WI_sld@data$District_N == 1,], border = "red", add = TRUE)
plot(WI_ctracts, add = TRUE)
