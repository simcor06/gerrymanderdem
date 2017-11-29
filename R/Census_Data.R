library(sp)
library(rgdal)

## For Census Tract Shapefile: https://www.census.gov/geo/maps-data/data/tiger-data.html
## 2010 Census > Demographic Profile 1 -- Shapefile Format > Census Tracts
# State Upper House
ctracts2010_NAD83<- readOGR(dsn = "C:/Users/coryh/OneDrive for Business/Geospatial Analysis with R/Final Project/data/Census_Tracts/Tract_2010Census_DP1.shp")
ctracts2010 <- spTransform(x = ctracts2010_NAD83, CRSobj = WGS84proj)
