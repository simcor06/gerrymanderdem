library(sp)
library(rgdal)


## For state legislative Boundries, look here: http://data-ltsb.opendata.arcgis.com/datasets/6c05ba69356c4b52a9542bb4493f732b_0
# State Upper House
WI_sud<- readOGR(dsn = "data/Wisconsin/State_Upper/Senate_Districts_2011.shp")

#State Lower House
WI_sld<- readOGR(dsn = "data/Wisconsin/State_Lower/_Wisconsin_State_Assembly_Districts.shp")

## Congressional Distrcits
## State FIPS Codes: https://www.census.gov/geo/reference/ansi_statetables.html
fcd115_NAD83 <- readOGR(dsn = "data/Congressional_Dist/tl_2016_us_cd115.shp")
# transform to WGS84 Datum
fcd115 <- spTransform(x = fcd115_NAD83, CRSobj = proj4string(WI_sud))
#extract wisconsin (FIPS == 55)
WI_fcd115 <- fcd115[fcd115$STATEFP == 55, ]



par(mfrow = c(1, 3))
plot(WI_fcd115)
plot(WI_sud)
plot(WI_sld)
