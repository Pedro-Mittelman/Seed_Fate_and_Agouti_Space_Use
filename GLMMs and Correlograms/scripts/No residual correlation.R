

##READING packages

library(ape)

## transforming data to quantity of seeds
a <- transmute(loc_dec17, station, lat_dec, long_dec)

b <- vector();c <- vector();d <- vector()

for (i in 1:length(a$station)) {
  b <- rep(a$station,10)
  c <- rep(a$lat_dec,10)
  d <- rep(a$long_dec,10)
}
z<- data.frame(station=b, lat_dec=c, long_dec=d)
locs2017s <- z %>% arrange(station)
#generating distance matrix
loc_dists17 <- as.matrix(dist(cbind(locs2017s$lat_dec, locs2017s$long_dec)))
loc_dists17.inv <-  1/loc_dists17
loc_dists17.inv [which(!is.finite(loc_dists17.inv ))] <- 0

## Removal reisudals 2017
Moran.I(residuals(m.rem2017, type="response"), loc_dists18.inv)

a <- transmute(loc_dec17, station, lat_dec, long_dec)

b <- vector();c <- vector();d <- vector()

for (i in 1:length(a$station)) {
  b <- rep(a$station,10)
  c <- rep(a$lat_dec,10)
  d <- rep(a$long_dec,10)
}
z<- data.frame(station=b, lat_dec=c, long_dec=d)
locs2017s <- z %>% arrange(station)
#generating distance matrix
loc_dists17 <- as.matrix(dist(cbind(locs2017s$lat_dec, locs2017s$long_dec)))
loc_dists17.inv <-  1/loc_dists17
loc_dists17.inv [which(!is.finite(loc_dists17.inv ))] <- 0

## Removal reisudals 2017
Moran.I(residuals(m.rem2017, type="response"), loc_dists18.inv)
## no residual correlation

## Survival residuals 2017
## only seeds with known destiny
a$Pontos <- a$station
Npreyed2017$Pontos <- as.character(Npreyed2017$Pontos)
Npreyed2017$Pontos[which(Npreyed2017$Pontos=="AFV05b")] <- "AFV05"
Npreyed2017$Pontos[which(Npreyed2017$Pontos=="AFV10a")] <- "AFV10"
Npreyed2017$Pontos[which(Npreyed2017$Pontos=="AFV16a")] <- "AFV16"
Npreyed2017$Pontos[which(Npreyed2017$Pontos=="AFV17a")] <- "AFV17"
zz <- merge(a,Npreyed2017)

X <- transmute(zz, station, lat_dec, long_dec)

#generating distance matrix
loc_dists17sur <- as.matrix(dist(cbind(X$lat_dec, X$long_dec)))
loc_dists17.invsur <-  1/loc_dists17sur
loc_dists17.invsur [which(!is.finite(loc_dists17.invsur ))] <- 0

Moran.I(residuals(m.survival2017, type="response"), loc_dists17.invsur)
## no residual correlation

## Dispersal reisudals 2017
Moran.I(residuals(m.finaldisp2017, type="response"), loc_dists17.invsur)




### 2018

a <- transmute(loc_dec18, station, lat_dec, long_dec)

b <- vector();c <- vector();d <- vector()

for (i in 1:length(a$station)) {
  b <- rep(a$station,10)
  c <- rep(a$lat_dec,10)
  d <- rep(a$long_dec,10)
}
z<- data.frame(station=b, lat_dec=c, long_dec=d)
locs2018s <- z %>% arrange(station)
#generating distance matrix
loc_dists18 <- as.matrix(dist(cbind(locs2018s$lat_dec, locs2018s$long_dec)))
loc_dists18.inv <-  1/loc_dists18
loc_dists18.inv [which(!is.finite(loc_dists18.inv ))] <- 0

Moran.I(residuals(m.rem2018, type="response"), loc_dists18.inv)
## no residual correlation

## Survival residuals 2018
## only seeds with known destiny
a$Pontos <- a$station
Npreyed2018$Pontos <- as.character(Npreyed2018$Pontos)
Npreyed2018$Pontos[which(Npreyed2018$Pontos=="AFV05b")] <- "AFV05"
Npreyed2018$Pontos[which(Npreyed2018$Pontos=="AFV10a")] <- "AFV10"
Npreyed2018$Pontos[which(Npreyed2018$Pontos=="AFV16a")] <- "AFV16"
Npreyed2018$Pontos[which(Npreyed2018$Pontos=="AFV17a")] <- "AFV17"
zz <- merge(a,Npreyed2018)

X <- transmute(zz, station, lat_dec, long_dec)

#generating distance matrix
loc_dists18sur <- as.matrix(dist(cbind(X$lat_dec, X$long_dec)))
loc_dists18.invsur <-  1/loc_dists18sur
loc_dists18.invsur [which(!is.finite(loc_dists18.invsur ))] <- 0

Moran.I(residuals(m.survival2018, type="response"), loc_dists18.invsur)
## no residual correlation

## Dispersal reisudals 2018
Moran.I(residuals(m.finaldisp2018, type="response"), loc_dists18.invsur)
