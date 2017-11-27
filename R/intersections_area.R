library(rgeos)
library(sp)
library(raster)


## this subsets data in which the GEOid begins with 55, which is the FIPS code for Wisconsin
WI_ctracts <- ctracts2010[grep("^55", ctracts2010@data$GEOID10), ]
##summary(WI_ctracts)
##plot(WI_ctracts)


## Plotting and subsetting Individudal Federal Districts Example
####plot(WI_fcd115[WI_fcd115@data$GEOID == 5501, ])

## Plotting and subsetting Individudal State Upper Districts Example
####plot(WI_sud[WI_sud@data$SEN_NUM == 1,])

## Plotting and subsetting Individudal State Lower Districts Example
###plot(WI_sld[WI_sld@data$District_N == 1,])

## intersect using raster package, unfortunately this cuts tracts in half, wchih wil be good for area caluclations
###Test_int <- intersect(x = WI_sld[WI_sld@data$District_N == 1,], y = WI_ctracts)
###plot(Test_int)

##Gintersects, this gives you the actual aligned tract boundaries
###Test_int_gInt <- which(gIntersects(spgeom1 = WI_sld[WI_sld@data$District_N == 1,], spgeom2 = WI_ctracts, byid = TRUE))
###Test_tracts <- WI_ctracts[Test_int_gInt, ]

## Creating a list of spatialpolygondataframes of census tracts for each state lower district

WI_sld_ctracts<- lapply(1:length(WI_sld@data$District_N), function(x) {
  int_sld <- which(gIntersects(spgeom1 = WI_sld[WI_sld@data$District_N == x,],
                                     spgeom2 = WI_ctracts, byid = TRUE))
  WI_ctracts[int_sld, ]
 })

## Creating a list of spatialpolygondataframes of census tracts for each state upper districts
WI_sud_ctracts<- lapply(1:length(WI_sud@data$SEN_NUM), function(x) {
  int_sud <- which(gIntersects(spgeom1 = WI_sud[WI_sud@data$SEN_NUM == x,],
                                spgeom2 = WI_ctracts, byid = TRUE))
  WI_ctracts[int_sud, ]
})

## Creating a list of spatialpolygondataframes of census tracts for each congressional district
## need to work on this still
