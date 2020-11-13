##loading packages

library("tidyverse")
library("spdep")
library("ape")
library("measurements")
library("devtools")


## reading 2018 data

locations18 <- read.csv2( "dados/Rice e lat long_2018.2.csv")

##reading 2017 data
locations17 <- read.csv2( "dados/Rice e lat long_2017.2.csv")

##Column Rice is the number of Independent records correct by effort in each station
###2018
###2018
###2018
### converting lat long from degrees to decimal using "measurements" package
#changing symbols to space and cleaning
locations18$Latitude <- gsub('°', ' ', locations18$Latitude)
locations18$Latitude <- gsub("\"S", '', locations18$Latitude)
locations18$Latitude <- gsub("'", ' ', locations18$Latitude)
locations18$Latitude <- gsub(" 22", '22', locations18$Latitude)

locations18$Longitude <- gsub('°', ' ', locations18$Longitude)
locations18$Longitude <- gsub("\"W", '', locations18$Longitude)
locations18$Longitude <- gsub("'", ' ', locations18$Longitude)
locations18$Longitude <- gsub(" 43", '43', locations18$Longitude)

#converting
lat_dec <- conv_unit(locations18$Latitude, from = "deg_min_sec", to="dec_deg")
long_dec <- conv_unit(locations18$Longitude, from = "deg_min_sec", to="dec_deg")

#adding columns
loc_dec18 <- locations18 %>% mutate(lat_dec = lat_dec, long_dec = long_dec)

#generating distance matrix
loc_dists18 <- as.matrix(dist(cbind(loc_dec18$lat_dec, loc_dec18$long_dec)))
loc_dists18.inv <-  1/loc_dists18
diag(loc_dists18.inv) <- 0

##runnig Moran test for space use
M2018_s <- Moran.I(loc_dec18$Rice, loc_dists18.inv); M2018_s

##runnig Moran test for dispersed seeds
M2018_d <- Moran.I(loc_dec18$Ndis, loc_dists18.inv); M2018_d

##runnig Moran test for preyed seeds
M2018_p <- Moran.I(loc_dec18$Npre, loc_dists18.inv); M2018_p

##runnig Moran test for intact seeds
M2018_i <- Moran.I(loc_dec18$intac, loc_dists18.inv); M2018_i


###2017
###2017
###2017
### converting lat long from degrees to decimal using "measurements" package
#changing symbols to space and cleaning
locations17$Latitude <- gsub('°', ' ', locations17$Latitude)
locations17$Latitude <- gsub("\"S", '', locations17$Latitude)
locations17$Latitude <- gsub("'", ' ', locations17$Latitude)
locations17$Latitude <- gsub(" 22", '22', locations17$Latitude)

locations17$Longitude <- gsub('°', ' ', locations17$Longitude)
locations17$Longitude <- gsub("\"W", '', locations17$Longitude)
locations17$Longitude <- gsub("'", ' ', locations17$Longitude)
locations17$Longitude <- gsub(" 43", '43', locations17$Longitude)

#converting
lat_dec <- conv_unit(locations17$Latitude, from = "deg_min_sec", to="dec_deg")
long_dec <- conv_unit(locations17$Longitude, from = "deg_min_sec", to="dec_deg")

#adding columns
loc_dec17 <- locations17 %>% mutate(lat_dec = lat_dec, long_dec = long_dec)

#generating distance matrix
loc_dists17 <- as.matrix(dist(cbind(loc_dec17$lat_dec, loc_dec17$long_dec)))
loc_dists17.inv <-  1/loc_dists17
diag(loc_dists17.inv) <- 0

##runnig Moran test for space use
M2017_s <- Moran.I(loc_dec17$Rice, loc_dists17.inv); M2017_s

##runnig Moran test for dispersed seeds
M2017_d <- Moran.I(loc_dec17$Ndis, loc_dists17.inv); M2017_d

##runnig Moran test for preyed seeds
M2017_p <- Moran.I(loc_dec17$Npre, loc_dists17.inv); M2017_p

##runnig Moran test for intact seeds
M2017_i <- Moran.I(loc_dec17$intac, loc_dists17.inv); M2017_i


