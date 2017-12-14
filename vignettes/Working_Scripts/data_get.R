demographics <- c("P0100001", "P016A003", "P016B003", "P036C003", "P016D003", "P036E003", "P016H003")





dframe<- as.data.frame(data.frame(lapply(1:length(demographics), function(x){
  dat1 <- get_decennial(geography = "tract", variables = demographics[x],state= FIPS, year=2010)$value
                       })))


testframe<- tracts_frame(demographics = demog)
View(testframe)

testframe<- as.data.frame(data.frame(get_decennial(geography = "tract",
                                                   variables = demographics[1],
                                                   state= FIPS, year=2010)$GEOID,
                                     sapply(1:length(demographics), function(x){
dat <- get_decennial(geography = "tract",
                     variables = demographics[x], state= FIPS, year=2010)$value
})), col.names = names(c("GEOID", demographics)))
