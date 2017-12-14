demographics <- c("P0100001", "P016A003", "P016B003", "P036C003", "P016D003", "P036E003", "P016H003")

dframe<-data.frame(get_decennial(geography = "tract",
                                     variables = demographics[1],
                                     state= FIPS, year=2010)$GEOID,
                       as.data.frame(lapply(1:length(demographics), function(x){
  dat1 <- get_decennial(geography = "tract", variables = demographics[x],
                state= FIPS, year=2010)$value
                       })))

names(dframe) <- c("GEOID", demographics)



