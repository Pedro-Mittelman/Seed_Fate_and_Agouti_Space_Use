#Diagnostics for HierArchical Regression Models”
library(DHARMa)

sim.rem2017 <- simulateResiduals(fittedModel = m.rem2017, n = 250)
plot(sim.rem2017)

sim.finaldisp2017 <- simulateResiduals(fittedModel = m.finaldisp2017, n = 250)
plot(sim.finaldisp2017)
 
testResiduals(sim.finaldisp2017)
testData = createData(sampleSize = 100, family = poisson(), spatialAutocorrelation = 5)
testSpatialAutocorrelation(simulationOutput = a, x = Nfinaldisp2017.2$X, y= Nfinaldisp2017.2$Y)

a <- recalculateResiduals(sim.finaldisp2017, group = NULL, aggregateBy = sum)
library(gstat)

E <- residuals(m.rem2017)
mydata <- data.frame(E,Nrem2017.2$X,Nrem2017.2$Y)
bubble(mydata, "E")
library(AED)

variogram(E ~ 1, mydata)
?variogram
