##packages and extra data----
library(ncf)
library(mgcv)
library(nlme)
#library(itsadug)## compare gamm models
library(gamm4)## gamm with AIC values
loc_dec17 <- read.csv("dados/loc_dec17.csv")
loc_dec18 <- read.csv("dados/loc_dec18.csv")

## latitude and longetitude in meteres ( conversion only valid for short distances)--------

lat2018 <- as.numeric(loc_dec18$lat_dec)
difflat2018 <- lat2018- min(lat2018)
difflat_meters <- difflat2018*111111 # 1degree = 111111 meters
difflat_meters2 <- vector()
for (i in 1:length(difflat_meters)) {
  difflat_meters2 <- c(difflat_meters2, rep(difflat_meters[i],10)) 
  
}


long2018 <- as.numeric(loc_dec18$long_dec)
difflong2018 <- long2018- min(long2018)
difflong_meters <- difflong2018*111111  #1degree = 111111 meters
difflong_meters2 <- vector()
for (i in 1:length(difflong_meters)) {
  difflong_meters2 <- c(difflong_meters2, rep(difflong_meters[i],10)) 
  
}
##### model with spatial correlation structure------

#addign spatial variables to the data

lats <- vector()
for (i in 1:length(loc_dec18$lat_dec)) {
  lats <- c(lats, rep(loc_dec18$lat_dec[i],10)) }

longs <- vector()
for (i in 1:length(loc_dec18$long_dec)) {
  longs <- c(longs, rep(loc_dec18$long_dec[i],10)) }


Nrem2018.2 <- mutate(Nrem2018, X= longs2018, Y= lats2018)

Nrem2018.2$Pontos<- factor(Nrem2018.2$Pontos)

#GLMM 
m.rem2018.2<-glmer(Removidas ~ (Sessenta) + s(X,Y) + (1|Pontos), data=Nrem2018.2, family=binomial)
m.rem2018.3<-gamm(Removidas ~ s(Sessenta) + s(X,Y, k=19), random=list(Pontos=~ 1), family = binomial,
                  data=Nrem2018.2)

### GAMM4

###Seed removal-----

###2017

Nrem2017.2 <- mutate(Nrem2017, X= longs, Y= lats)

m.rem2017.gam<-gamm4(Removidas ~ s(Sessenta), family = binomial,
                     data=Nrem2017.2)
m.rem2017.gamm4.sc<-gamm4(Removidas ~ s(Sessenta) + s(X,Y,k=19), random= ~(1|Pontos), family = binomial,
                          data=Nrem2017.2)
m.rem2017.gamm4<-gamm4(Removidas ~ s(Sessenta), data=Nrem2017.2, random= ~(1|Pontos), family = binomial
)


AICctab(m.rem2017.gamm4$mer, m.rem2017.gamm4.sc$mer, m.rem2017.gam$mer, weights=T)


## plot

# General prediction GAMM4 
rem.2017.g<-expand.grid(Sessenta = seq(0, 65.5, 0.1))
pred.rem.2017.g<-predict(m.rem2017.gamm4$gam,newdata = rem.2017.g, type="response", re.form=NA)
df.predict.rem.2017.g <- data.frame(pred = pred.rem.2017.g, rem.2017.g)

#Plotting the graph 

ggplot(data = Nrem2017.2, aes(x = jitter(Sessenta, 10), y = jitter(as.numeric(Removidas),0.035)   # dados e eixos
                              ,col=Destino
)) +
  # points
  geom_point( size=11, alpha=0.5, aes(shape=Destino, col=Destino) 
              #, col="red4"
  ) +  
  ##settig shapes
  scale_shape_manual(name= "Seed Fate",
                     values = c(19, 17, 15, 17),
                     labels=c("Dispersed", "Intact", "Preyed", "Lost")
  )+
  scale_color_manual(name= "Seed Fate", 
                     values=c("chartreuse4", "deepskyblue", "firebrick2", "yellow3"),
                     labels=c("Dispersed", "Intact", "Preyed", "Lost")
  )+
  
  geom_line(data = df.predict.rem.2017.g, aes(y = pred, x = Sessenta ), 
            col= "black", size=3, alpha=0.8) +                    # general model line
  #geom_line(data = df.predict.rem.rf.2017, aes(y = pred, x = Sessenta,
  #group=Pontos ),
  #size=1.1, alpha=0.9, col="grey") + # random effects model lines
  scale_x_continuous(
    name = "Agouti Relative Abundance",
    limits = c(-1,66),
    breaks= seq(from=0, to=60, by=15)) +
  scale_y_continuous(name = "Probability of Seed Removal",
                     limits = c(-0.016,1.1),
                     breaks= seq(from=0, to=1, by=0.25))+
  guides (colour = guide_legend(override.aes = list(size=15)))+
  theme_classic()+
  theme(
    legend.position = "none"
    
  )



###2018
m.rem2018.gam<-gamm4(Removidas ~ s(Sessenta), data=Nrem2018.2, family = binomial)
m.rem2018.gamm4.sc<-gamm4(Removidas ~ s(Sessenta) + s(X,Y,k=19), random= ~(1|Pontos), family = binomial,
                          data=Nrem2018.2)
m.rem2018.gamm4<-gamm4(Removidas ~ s(Sessenta), data=Nrem2018.2, random= ~(1|Pontos), family = binomial
)


AICctab(m.rem2018.gamm4$mer, m.rem2018.gamm4.sc$mer, m.rem2018.gam$mer)


## plot

# General prediction GAMM4 
rem.2018.g<-expand.grid(Sessenta = seq(0, 65.5, 0.1))
pred.rem.2018.g<-predict(m.rem2018.gamm4$gam,newdata = rem.2018.g, type="response", re.form=NA)
df.predict.rem.2018.g <- data.frame(pred = pred.rem.2018.g, rem.2018.g)

#Plotting the graph 

ggplot(data = Nrem2018.2, aes(x = jitter(Sessenta, 10), y = jitter(as.numeric(Removidas),0.035)   # dados e eixos
                              ,col=Destino
)) +
  # points
  geom_point( size=11, alpha=0.5, aes(shape=Destino, col=Destino) 
              #, col="red4"
  ) +  
  ##settig shapes
  scale_shape_manual(name= "Seed Fate",
                     values = c(19, 17, 15, 18),
                     labels=c("Dispersed", "Intact", "Preyed", "Lost")
  )+
  scale_color_manual(name= "Seed Fate", 
                     values=c("chartreuse4", "deepskyblue", "firebrick2", "yellow3"),
                     labels=c("Dispersed", "Intact", "Preyed", "Lost")
  )+
  
  geom_line(data = df.predict.rem.2018.g, aes(y = pred, x = Sessenta ), 
            col= "black", size=3, alpha=0.8) +                    # general model line
  #geom_line(data = df.predict.rem.rf.2018, aes(y = pred, x = Sessenta,
  #group=Pontos ),
  #size=1.1, alpha=0.9, col="grey") + # random effects model lines
  scale_x_continuous(
    name = "Agouti Relative Abundance",
    limits = c(-1,66),
    breaks= seq(from=0, to=60, by=15)) +
  scale_y_continuous(name = "Probability of Seed Removal",
                     limits = c(-0.016,1.1),
                     breaks= seq(from=0, to=1, by=0.25))+
  guides (colour = guide_legend(override.aes = list(size=15)))+
  theme_classic()+
  theme(
    legend.position = "none"
    
  )




####Seed Survival-----

###2017

Nsurvival2017.2 <- Nrem2017.2 %>% mutate(Survival=Nrem2017.2$Destino!="P" ) %>% 
  filter(Destino!="S")

m.survival2017.gam<-gamm4(Survival ~ s(Sessenta), data=Nsurvival2017.2,  family = binomial)   
m.survival2017.gamm4.sc<-gamm4(Survival ~ s(Sessenta) + s(X,Y,k=18), random= ~(1|Pontos), family = binomial,
                               data=Nsurvival2017.2)

m.survival2017.gamm4<-gamm4(Survival ~ s(Sessenta), data=Nsurvival2017.2, random= ~(1|Pontos), family = binomial
)


AICctab(m.survival2017.gamm4$mer, m.survival2017.gamm4.sc$mer, m.survival2017.gam$mer)



## plot

# General prediction GAMM4 
survival.2017.g<-expand.grid(Sessenta = seq(0, 65.5, 0.1))
pred.survival.2017.g<-predict(m.survival2017.gamm4$gam,newdata = survival.2017.g, type="response", re.form=NA)
df.predict.survival.2017.g <- data.frame(pred = pred.survival.2017.g, survival.2017.g)

#Plotting the graph 

ggplot(data = Nsurvival2017.2, aes(x = jitter(Sessenta, 10), y = jitter(as.numeric(Survival),0.035)   # dados e eixos
                                   ,col=Destino
)) +
  # points
  geom_point( size=11, alpha=0.5, aes(shape=Destino, col=Destino) 
              #, col="red4"
  ) +  
  ##settig shapes
  scale_shape_manual(name= "Seed Fate",
                     values = c(19, 17, 15, 17),
                     labels=c("Dispersed", "Intact", "Preyed", "Lost")
  )+
  scale_color_manual(name= "Seed Fate", 
                     values=c("chartreuse4", "deepskyblue", "firebrick2", "yellow3"),
                     labels=c("Dispersed", "Intact", "Preyed", "Lost")
  )+
  
  geom_line(data = df.predict.survival.2017.g, aes(y = pred, x = Sessenta ), 
            col= "black", size=3, alpha=0.8) +                    # general model line
  #geom_line(data = df.predict.survival.rf.2017, aes(y = pred, x = Sessenta,
  #group=Pontos ),
  #size=1.1, alpha=0.9, col="grey") + # random effects model lines
  scale_x_continuous(
    name = "Agouti Relative Abundance",
    limits = c(-1,66),
    breaks= seq(from=0, to=60, by=15)) +
  scale_y_continuous(name = "Probability of Seed survivaloval",
                     limits = c(-0.016,1.1),
                     breaks= seq(from=0, to=1, by=0.25))+
  guides (colour = guide_legend(override.aes = list(size=15)))+
  theme_classic()+
  theme(
    legend.position = "none"
    
  )



###2018

Nsurvival2018.2 <- Nrem2018.2 %>% mutate(Survival=Nrem2018.2$Destino!="P" ) %>% 
  filter(Destino!="S")  

m.survival2018.gam<-gamm4(Survival ~ s(Sessenta), data=Nsurvival2018.2, family = binomial)  
m.survival2018.gamm4.sc<-gamm4(Survival ~ s(Sessenta) + s(X,Y,k=19), random= ~(1|Pontos), family = binomial,
                               data=Nsurvival2018.2)
m.survival2018.gamm4<-gamm4(Survival ~ s(Sessenta), data=Nsurvival2018.2, random= ~(1|Pontos), family = binomial
)


AICctab(m.survival2018.gamm4$mer, m.survival2018.gamm4.sc$mer, m.survival2018.gam$mer)


## plot

# General prediction GAMM4 
survival.2018.g<-expand.grid(Sessenta = seq(0, 65.5, 0.1))
pred.survival.2018.g<-predict(m.survival2018.gamm4$gam,newdata = survival.2018.g, type="response", re.form=NA)
df.predict.survival.2018.g <- data.frame(pred = pred.survival.2018.g, survival.2018.g)

#Plotting the graph 

ggplot(data = Nsurvival2018.2, aes(x = jitter(Sessenta, 10), y = jitter(as.numeric(Survival),0.035)   # dados e eixos
                                   ,col=Destino
)) +
  # points
  geom_point( size=11, alpha=0.5, aes(shape=Destino, col=Destino) 
              #, col="red4"
  ) +  
  ##settig shapes
  scale_shape_manual(name= "Seed Fate",
                     values = c(19, 17, 15, 18),
                     labels=c("Dispersed", "Intact", "Preyed", "Lost")
  )+
  scale_color_manual(name= "Seed Fate", 
                     values=c("chartreuse4", "deepskyblue", "firebrick2", "yellow3"),
                     labels=c("Dispersed", "Intact", "Preyed", "Lost")
  )+
  
  geom_line(data = df.predict.survival.2018.g, aes(y = pred, x = Sessenta ), 
            col= "black", size=3, alpha=0.8) +                    # general model line
  #geom_line(data = df.predict.survival.rf.2018, aes(y = pred, x = Sessenta,
  #group=Pontos ),
  #size=1.1, alpha=0.9, col="grey") + # random effects model lines
  scale_x_continuous(
    name = "Agouti Relative Abundance",
    limits = c(-1,66),
    breaks= seq(from=0, to=60, by=15)) +
  scale_y_continuous(name = "Probability of Seed survivaloval",
                     limits = c(-0.016,1.02),
                     breaks= seq(from=0, to=1, by=0.25))+
  guides (colour = guide_legend(override.aes = list(size=15)))+
  theme_classic()+
  theme(
    legend.position = "none"
    
  )




###Seed dispersal---- 

##data  
Nfinaldisp2017.2 <- Nrem2017.2 %>% mutate(Dispersas=Nrem2017.2$Destino=="D" ) %>% 
  filter(Destino!="S")

##models  

m.finaldisp2017.gam<-gamm4(Dispersas ~ s(Sessenta)+ s(I(Sessenta^2)), data=Nfinaldisp2017.2,
                           family = binomial)  

m.finaldisp2017.gamm4<-gamm4(Dispersas ~ s(Sessenta)+ s(I(Sessenta^2)), data=Nfinaldisp2017.2,
                             random= ~(1|Pontos), family = binomial)

m.finaldisp2017.gamm4.sc<-gamm4(Dispersas ~ s(Sessenta) + s(I(Sessenta^2))+ s(X,Y,k=19), random= ~(1|Pontos), family = binomial,
                                data=Nfinaldisp2017.2)  


AICctab(m.finaldisp2017.gamm4$mer, m.finaldisp2017.gamm4.sc$mer, m.finaldisp2017.gam$mer)



## plot

# General prediction GAMM4 
finaldisp.2017.g<-expand.grid(Sessenta = seq(0, 65.5, 0.1))
pred.finaldisp.2017.g<-predict(m.finaldisp2017.gamm4$gam,newdata = finaldisp.2017.g, type="response", re.form=NA)
df.predict.finaldisp.2017.g <- data.frame(pred = pred.finaldisp.2017.g, finaldisp.2017.g)

#Plotting the graph 

ggplot(data = Nfinaldisp2017.2, aes(x = jitter(Sessenta, 10), y = jitter(as.numeric(Dispersas),0.035)   # dados e eixos
                                    ,col=Destino
)) +
  # points
  geom_point( size=11, alpha=0.5, aes(shape=Destino, col=Destino) 
              #, col="red4"
  ) +  
  ##settig shapes
  scale_shape_manual(name= "Seed Fate",
                     values = c(19, 17, 15, 17),
                     labels=c("Dispersed", "Intact", "Preyed", "Lost")
  )+
  scale_color_manual(name= "Seed Fate", 
                     values=c("chartreuse4", "deepskyblue", "firebrick2", "yellow3"),
                     labels=c("Dispersed", "Intact", "Preyed", "Lost")
  )+
  
  geom_line(data = df.predict.finaldisp.2017.g, aes(y = pred, x = Sessenta ), 
            col= "black", size=3, alpha=0.8) +                    # general model line
  #geom_line(data = df.predict.finaldisp.rf.2017, aes(y = pred, x = Sessenta,
  #group=Pontos ),
  #size=1.1, alpha=0.9, col="grey") + # random effects model lines
  scale_x_continuous(
    name = "Agouti Relative Abundance",
    limits = c(-1,66),
    breaks= seq(from=0, to=60, by=15)) +
  scale_y_continuous(name = "Probability of Seed finaldispoval",
                     limits = c(-0.016,1.1),
                     breaks= seq(from=0, to=1, by=0.25))+
  guides (colour = guide_legend(override.aes = list(size=15)))+
  theme_classic()+
  theme(
    legend.position = "none"
    
  )



###2018
##data
Nfinaldisp2018.2 <- Nrem2018.2 %>% mutate(Dispersas=Nrem2018.2$Destino=="D" ) %>% 
  filter(Destino!="S")  

##models  
m.finaldisp2018.gam<-gamm4(Dispersas ~ s(Sessenta)+s(I(Sessenta^2)), data=Nfinaldisp2018.2, 
                           family = binomial) 

m.finaldisp2018.gamm4.sc<-gamm4(Dispersas ~ s(Sessenta) + s(I(Sessenta^2))+ s(X,Y,k=19), random= ~(1|Pontos), family = binomial,
                                data=Nfinaldisp2018.2)  

m.finaldisp2018.gamm4<-gamm4(Dispersas ~ s(Sessenta)+s(I(Sessenta^2)), data=Nfinaldisp2018.2, 
                             random= ~(1|Pontos), family = binomial)  


AICctab(m.finaldisp2018.gamm4$mer, m.finaldisp2018.gamm4.sc$mer, m.finaldisp2018.gam$mer)


## plot

# General prediction GAMM4 
finaldisp.2018.g<-expand.grid(Sessenta = seq(0, 65.5, 0.1))
pred.finaldisp.2018.g<-predict(m.finaldisp2018.gamm4$gam,newdata = finaldisp.2018.g, type="response", re.form=NA)
df.predict.finaldisp.2018.g <- data.frame(pred = pred.finaldisp.2018.g, finaldisp.2018.g)

#Plotting the graph 

ggplot(data = Nfinaldisp2018.2, aes(x = jitter(Sessenta, 10), y = jitter(as.numeric(Dispersas),0.035)   # dados e eixos
                                    ,col=Destino
)) +
  # points
  geom_point( size=11, alpha=0.5, aes(shape=Destino, col=Destino) 
              #, col="red4"
  ) +  
  ##settig shapes
  scale_shape_manual(name= "Seed Fate",
                     values = c(19, 17, 15, 18),
                     labels=c("Dispersed", "Intact", "Preyed", "Lost")
  )+
  scale_color_manual(name= "Seed Fate", 
                     values=c("chartreuse4", "deepskyblue", "firebrick2", "yellow3"),
                     labels=c("Dispersed", "Intact", "Preyed", "Lost")
  )+
  
  geom_line(data = df.predict.finaldisp.2018.g, aes(y = pred, x = Sessenta ), 
            col= "black", size=3, alpha=0.8) +                    # general model line
  #geom_line(data = df.predict.finaldisp.rf.2018, aes(y = pred, x = Sessenta,
  #group=Pontos ),
  #size=1.1, alpha=0.9, col="grey") + # random effects model lines
  scale_x_continuous(
    name = "Agouti Relative Abundance",
    limits = c(-1,66),
    breaks= seq(from=0, to=60, by=15)) +
  scale_y_continuous(name = "Probability of Seed finaldispoval",
                     limits = c(-0.016,1.02),
                     breaks= seq(from=0, to=1, by=0.25))+
  guides (colour = guide_legend(override.aes = list(size=15)))+
  theme_classic()+
  theme(
    legend.position = "none"
    
  )

###correlog for residuals of  removed seeds models-------------

#2017

#glm residuals
#model
m.rem2017.glm<-glm(Removidas ~ (Sessenta), data=Nrem2017, family=binomial)

#corelog
Correlog17.rem.glm <- spline.correlog(x=difflat_meters2, y=difflong_meters2, 
                                  z=residuals(m.rem2017.glm, type="pearson"), 
                                  xmax=max(c(difflat_meters, difflong_meters)))
summary(Correlog17.rem.glm)
plot(Correlog17.rem.glm)

#glmm residuals
Correlog17.rem.glmm <- spline.correlog(x=difflat_meters2, y=difflong_meters2, 
                                    z=residuals(m.rem2017, type="pearson"), resamp = 10000, max.it=1000, 
                                    xmax=max(c(difflat_meters, difflong_meters)))

## gamm model

Correlog17.rem.gamm.sc <- spline.correlog(x=difflat_meters2, y=difflong_meters2, 
                                       z=residuals(m.rem2017.gamm4.sc$mer, type="pearson"),resamp = 10000, max.it=1000, 
                                       xmax=max(c(difflat_meters, difflong_meters)))


summary(Correlog17.rem.gamm.sc)
 plot(Correlog17.rem.gamm.sc)
 ## plotting
par(mfrow=c(1,3), cex.main=1.1, cex.lab=1.5, cex.axis=1.4, mar=c(5,4,4,2))
plot(Correlog17.rem.glm, xlab="Distance(m)", ylab="Residual correlation", main="Model with no correlation structure", mar=c(5,4.2,4,2))
plot(Correlog17.rem.glmm, xlab="Distance(m)",ylab="Residual correlation", main="Model with intra-station correlation structure")

plot(Correlog17.rem.gamm.sc,xlab="Distance(m)",ylab="Residual correlation", main="Model with inter & intra-station correlation structure", lwd=2)


#2018

#glm residuals
#model
m.rem2018.glm<-glm(Removidas ~ (Sessenta), data=Nrem2018, family=binomial)

#corelog
Correlog18.rem.glm <- spline.correlog(x=difflat_meters2, y=difflong_meters2, 
                                      z=residuals(m.rem2018.glm, type="pearson"), 
                                      xmax=max(c(difflat_meters, difflong_meters)))
summary(Correlog18.rem.glm)
plot(Correlog18.rem.glm)

##glmM residuals
Correlog18.rem.glmm <- spline.correlog(x=difflat_meters2, y=difflong_meters2, 
                                    z=residuals(m.rem2018, type="pearson"), resamp = 10000, max.it = 1000, 
                                    xmax=max(c(difflat_meters, difflong_meters)))
summary(Correlog18.rem.glmm)
plot(Correlog18.rem.glmm)

###GAMM with spatial correlation
Correlog18.rem.gamm.sc <- spline.correlog(x=difflat_meters2, y=difflong_meters2, 
                                          z=residuals(m.rem2018.gamm4.sc$mer, type="pearson"),resamp = 10000, max.it=1000, 
                                          xmax=max(c(difflat_meters, difflong_meters)))
summary(Correlog18.rem.gamm.sc)
plot(Correlog18.rem.gamm.sc)
par(mfrow=c(1,3), cex.main=1.1, cex.lab=1.5, cex.axis=1.4, lwd=2)
plot(Correlog18.rem.glm, xlab="Distance(m)", ylab="Residual correlation", main="Model with no correlation structure", mar=c(5,4.2,4,2))
plot(Correlog18.rem.glmm, xlab="Distance(m)",ylab="Residual correlation", main="Model with intra-station correlation structure")

plot(Correlog18.rem.gamm.sc,xlab="Distance(m)",ylab="Residual correlation", main="Model with inter & intra-station correlation structure", lwd=2)

###correlog for residuals of  seed survival models-------------

#2017
#glm residuals
#model
m.survival2017.glm<-glm(Survival ~ (Sessenta), data=Nsurvival2017, family=binomial)

#corelog
Correlog17.survival.glm <- spline.correlog(x=difflat_meters2, y=difflong_meters2, 
                                      z=residuals(m.survival2017.glm, type="pearson"), 
                                      xmax=max(c(difflat_meters, difflong_meters)))
summary(Correlog17.survival.glm)
plot(Correlog17.survival.glm)


##GlmM residuals
Correlog17.surv.glmm <- spline.correlog(x=difflat_meters2, y=difflong_meters2, 
                                  z=residuals(m.survival2017, type="pearson"),resamp = 10000, max.it=1000, 
                                  xmax=max(c(difflat_meters, difflong_meters)))
summary(Correlog17.surv)

Correlog17.surv.glmm.2 <- spline.correlog(x=difflat_meters2, y=difflong_meters2, 
                                        z=residuals(m.survival2017, type="pearson"),df=10,
                                        xmax=max(c(difflat_meters, difflong_meters)))
plot(Correlog17.surv.glmm)
plot(Correlog17.surv.glmm.2)

## gamm model

Correlog17.surv.gamm.sc <- spline.correlog(x=difflat_meters2, y=difflong_meters2, 
                                          z=residuals(m.survival2017.gamm4.sc$mer, type="pearson"),resamp = 10000, max.it=1000, 
                                          xmax=max(c(difflat_meters, difflong_meters)))


summary(Correlog17.surv.gamm.sc)
plot(Correlog17.surv.gamm.sc)
## plotting
par(mfrow=c(1,3), cex.main=1.1, cex.lab=1.5, cex.axis=1.4, mar=c(5,4,4,2), lwd=2)
plot(Correlog17.survival.glm, xlab="Distance(m)", ylab="Residual correlation", main="Model with no correlation structure", mar=c(5,4.2,4,2))
plot(Correlog17.surv.glmm, xlab="Distance(m)",ylab="Residual correlation", main="Model with intra-station correlation structure")

plot(Correlog17.surv.gamm.sc,xlab="Distance(m)",ylab="Residual correlation", main="Model with inter & intra-station correlation structure", lwd=2)



#2018

#glm residuals
#model
m.survival2018.glm<-glm(Survival ~ (Sessenta), data=Nsurvival2018, family=binomial)

#corelog
Correlog18.survival.glm <- spline.correlog(x=difflat_meters2, y=difflong_meters2, 
                                           z=residuals(m.survival2018.glm, type="pearson"), df=15,
                                           #resamp = 100, max.it=10, 
                                           xmax=max(c(difflat_meters, difflong_meters)))
summary(Correlog18.survival.glm)
plot(Correlog18.survival.glm)


##GlmM residuals
Correlog18.surv.glmm <- spline.correlog(x=difflat_meters2, y=difflong_meters2, 
                                        z=residuals(m.survival2018, type="pearson"),resamp = 10000, max.it=1000, df=8, 
                                        xmax=max(c(difflat_meters, difflong_meters)))
summary(Correlog18.surv)
plot(Correlog18.surv)


## gamm model

Correlog18.surv.gamm.sc <- spline.correlog(x=difflat_meters2, y=difflong_meters2, 
                                           z=residuals(m.survival2018.gamm4.sc$mer, type="pearson"),resamp = 10000, max.it=1000, df=8,
                                           xmax=max(c(difflat_meters, difflong_meters)))


summary(Correlog18.surv.gamm.sc)
plot(Correlog18.surv.gamm.sc)
## plotting
par(mfrow=c(1,3), cex.main=1.1, cex.lab=1.5, cex.axis=1.4, mar=c(5,4,4,2), lwd=2)
plot(Correlog18.survival.glm, xlab="Distance(m)", ylab="Residual correlation", main="Model with no correlation structure", mar=c(5,4.2,4,2))
plot(Correlog18.surv.glmm, xlab="Distance(m)",ylab="Residual correlation", main="Model with intra-station correlation structure")

plot(Correlog18.surv.gamm.sc,xlab="Distance(m)",ylab="Residual correlation", main="Model with inter & intra-station correlation structure", lwd=2)


###correlog for residuals of  seed dispersal models-------------

#2017
#glm residuals
#model
m.finaldispi2017.glm<-glm(Dispersas ~ (Sessenta), data=Nfinaldisp2017, family=binomial)

#corelog
Correlog17.finaldisp.glm <- spline.correlog(x=difflat_meters2, y=difflong_meters2, 
                                           z=residuals(m.finaldispi2017.glm, type="pearson"), 
                                           xmax=max(c(difflat_meters, difflong_meters)))
summary(Correlog17.finaldisp.glm)
plot(Correlog17.finaldisp.glm)


##GlmM residuals
Correlog17.finaldisp.glmm <- spline.correlog(x=difflat_meters2, y=difflong_meters2, 
                                        z=residuals(m.finaldisp2017, type="pearson"),resamp = 10000, max.it=1000, df=10, 
                                        xmax=max(c(difflat_meters, difflong_meters)))
summary(Correlog17.finaldisp.glmm)


plot(Correlog17.finaldisp.glmm)


## gamm model

Correlog17.finaldisp.gamm.sc <- spline.correlog(x=difflat_meters2, y=difflong_meters2, 
                                           z=residuals(m.finaldisp2017.gamm4.sc$mer, type="pearson"),resamp = 10000, max.it=1000, df=10, 
                                           xmax=max(c(difflat_meters, difflong_meters)))


summary(Correlog17.finaldisp.gamm.sc)
plot(Correlog17.finaldisp.gamm.sc)
## plotting
par(mfrow=c(1,3), cex.main=1.1, cex.lab=1.5, cex.axis=1.4, mar=c(5,4,4,2), lwd=2)
plot(Correlog17.finaldisp.glm, xlab="Distance(m)", ylab="Residual correlation", main="Model with no correlation structure", mar=c(5,4.2,4,2))
plot(Correlog17.finaldisp.glmm, xlab="Distance(m)",ylab="Residual correlation", main="Model with intra-station correlation structure")

plot(Correlog17.finaldisp.gamm.sc,xlab="Distance(m)",ylab="Residual correlation", main="Model with inter & intra-station correlation structure", lwd=2)



#2018

#glm residuals
#model
m.finaldisp2018.glm<-glm(Dispersas ~ (Sessenta), data=Nfinaldisp2018, family=binomial)

#corelog
Correlog18.finaldisp.glm <- spline.correlog(x=difflat_meters2, y=difflong_meters2, 
                                           z=residuals(m.finaldisp2018.glm, type="pearson"), df=15,
                                           #resamp = 100, max.it=10, 
                                           xmax=max(c(difflat_meters, difflong_meters)))
summary(Correlog18.finaldisp.glm)
plot(Correlog18.finaldisp.glm)


##GlmM residuals
Correlog18.finaldisp.glmm <- spline.correlog(x=difflat_meters2, y=difflong_meters2, 
                                        z=residuals(m.finaldisp2018, type="pearson"),resamp = 10000, max.it=1000, df=8, 
                                        xmax=max(c(difflat_meters, difflong_meters)))
summary(Correlog18.finaldisp.glmm)
plot(Correlog18.finaldisp.glmm)


## gamm model

Correlog18.finaldisp.gamm.sc <- spline.correlog(x=difflat_meters2, y=difflong_meters2, 
                                           z=residuals(m.finaldisp2018.gamm4.sc$mer, type="pearson"),resamp = 10000, max.it=1000, df=8,
                                           xmax=max(c(difflat_meters, difflong_meters)))


summary(Correlog18.finaldisp.gamm.sc)
plot(Correlog18.finaldisp.gamm.sc)
## plotting
par(mfrow=c(1,3), cex.main=1.1, cex.lab=1.5, cex.axis=1.4, mar=c(5,4,4,2), lwd=2)
plot(Correlog18.finaldisp.glm, xlab="Distance(m)", ylab="Residual correlation", main="Model with no correlation structure", mar=c(5,4.2,4,2))
plot(Correlog18.finaldisp.glmm, xlab="Distance(m)",ylab="Residual correlation", main="Model with intra-station correlation structure")

plot(Correlog18.finaldisp.gamm.sc,xlab="Distance(m)",ylab="Residual correlation", main="Model with inter & intra-station correlation structure", lwd=2)




