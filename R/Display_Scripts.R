library(sp)
library(rgdal)
library(rgeos)


par(mfrow = c(1, 3))
plot(WI_fcd115)
plot(WI_sud)
plot(WI_sld)

wisconsin <- list(WI_fcd115, WI_sud, WI_sld)
districts <- c("Federal Districts", "State Upper House", "State Lower House")
cols <- c("red", "blue", "green")
par(mfrow = c(1, 3))
for(i in 1:length(wisconsin)) {
  hist(gArea(spgeom = wisconsin[[i]], byid= TRUE), main = districts[i], xlab = "Hectares",
       col = cols[i])
}

class(WI_fcd115)
WI_fcd115

