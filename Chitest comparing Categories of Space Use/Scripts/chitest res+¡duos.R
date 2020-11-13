 chi.2018 <- read.csv2("dados/chitest.atu2018.csv")

 #withdrawing unnecessary column 
 chi.2018 <- chi.2018[,2:4]
 
## chi test
chisq.test(chi.2018)

##obtaing residuals
chisq.test(chi.2018)$stdres

### saving residuals
ww <- chisq.test(chi.2018)$stdres
write.csv(ww, "./dados/residuos2018.csv")