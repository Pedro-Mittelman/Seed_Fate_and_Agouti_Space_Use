####Loading packages----


library(tidyverse)
library(lme4)    #  GLMM
library(bbmle)   #comparing  AICs
library(ggplot2) #Pgrphs
library(scales) # for colours
library(gridExtra) # plotting graphs together
library(grid) # plotting graphs together


####Removed seeds------------

## 2017

#reading data
Nrem2017 <- read.csv("dados/2017 data/N.rem2017.csv")
#GLMM 
m.rem2017<-glmer(Removidas ~ (Sessenta) + (1|Pontos), data=Nrem2017, family=binomial)

summary(m.rem2017)
summary(m.rem2017)$coefficients
## Making predictions to plot

# Values to be predicted considering Random Effects (rf)
rem.rf.2017<-expand.grid(Pontos = unique(Nrem2017$Pontos), Sessenta=seq(0, 65.5, 0.1))
#Predicting
predict.rem.rf.2017<-predict(m.rem2017, newdata = rem.rf.2017, type="response")
#saving into a data.frame
df.predict.rem.rf.2017<-data.frame(pred = predict.rem.rf.2017, rem.rf.2017)




# General prediction without random effects 
rem.2017<-expand.grid(Sessenta = seq(0, 65.5, 0.1))
pred.rem.2017<-predict(m.rem2017, newdata = rem.2017, type="response", re.form=NA)
df.predict.rem.2017 <- data.frame(pred = pred.rem.2017, rem.2017)

#Plotting the graph
plot.rem.2017 <- 
  ggplot(data = Nrem2017, aes(x = jitter(Sessenta, 20), y = jitter(as.numeric(Removidas),0.075)   # dados e eixos
                            ,col=Destino
)) + 
  scale_color_manual(name= "Seed Fate", 
                     values=c("chartreuse4", "deepskyblue", "firebrick2", "yellow3"),
                     labels=c("Dispersed", "Non-harvested", "Preyed", "Lost"))+
  # points
  geom_point( size=11, alpha=0.5, aes(shape=Destino) 
              #, col="red4"
              ) +          
  ##settig shapes
  scale_shape_manual(values = c(19, 17, 15, 18)) +
  geom_line(data = df.predict.rem.2017, aes(y = pred, x = Sessenta ), 
            col= "black", size=10, alpha=0.8) +                    # general model line
  geom_line(data = df.predict.rem.rf.2017, aes(y = pred, x = Sessenta,
                                       group=Pontos ),
            size=1.5, alpha=0.9, col="grey") + # random effects model lines
  scale_x_continuous(
    name = "Agouti Relative Abundance",
                     limits = c(-1,66),
                     breaks= seq(from=0, to=60, by=15)) +
  scale_y_continuous(name = "Probability of Seed Removal",
                     limits = c(-0.016,1.02),
                     breaks= seq(from=0, to=1, by=0.25))+
  theme_classic()+
  theme(
     axis.text = element_text(size = 35),
    #     axis.title = element_text(colour = "black", size = 28, face = "bold"),
    #     axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))
         legend.position = "none",
      axis.title = element_blank(),
    plot.margin = unit(c(0,1.15,1.1,0), "cm")
    
  )



## 2018


##reading data
cuti2018 <- read.csv("dados/cutiext.2018.csv")
#filtering
Nrem2018 <- cuti2018 %>%
  filter(Data=="2018-08-01") %>% 
  mutate(Removidas=Destino!='N')

#GLMM 
m.rem2018<-glmer(Removidas ~ (Sessenta) + (1|Pontos), data=Nrem2018, family=binomial)

summary(m.rem2018)
summary(m.rem2018)$coefficients
## Making predictions to plot

# Values to be predicted considering Random Effects (rf)
rem.rf.2018<-expand.grid(Pontos = unique(Nrem2018$Pontos), Sessenta=seq(0, 65.5, 0.1))
#Predicting
predict.rem.rf.2018<-predict(m.rem2018, newdata = rem.rf.2018, type="response")
#Saving into a data.frame
df.predict.rem.rf.2018<-data.frame(pred = predict.rem.rf.2018, rem.rf.2018)

# General prediction without random effects 
rem.2018<-expand.grid(Sessenta = seq(0, 65.5, 0.1))
pred.rem.2018<-predict(m.rem2018, newdata = rem.2018, type="response", re.form=NA)
df.predict.rem.2018 <- data.frame(pred = pred.rem.2018, rem.2018)

#Plotting the graph 
plot.rem.2018 <-
  ggplot(data = Nrem2018, aes(x = jitter(Sessenta, 20), y = jitter(as.numeric(Removidas),0.075)   # dados e eixos
                              ,col=Destino
  )) +
  # points
  geom_point( size=11, alpha=0.5, aes(shape=Destino, col=Destino) 
              #, col="red4"
  ) +  
  ##settig shapes
  scale_shape_manual(name= "Seed Fate",
                     values = c(19, 17, 15, 18),
                     labels=c("Dispersed", "Non-harvested", "Preyed", "Lost")
  )+
  scale_color_manual(name= "Seed Fate", 
                     values=c("chartreuse4", "deepskyblue", "firebrick2", "yellow3"),
                     labels=c("Dispersed", "Non-harvested", "Preyed", "Lost")
  )+
  
  geom_line(data = df.predict.rem.2018, aes(y = pred, x = Sessenta ), 
            col= "black", size=7, alpha=0.8) +                    # general model line
  geom_line(data = df.predict.rem.rf.2018, aes(y = pred, x = Sessenta,
                                               group=Pontos ),
            size=1.1, alpha=0.9, col="grey") + # random effects model lines
  scale_x_continuous(
    name = "Agouti Relative Abundance",
    limits = c(-1,66),
    breaks= seq(from=0, to=60, by=15)) +
  scale_y_continuous(name = "Probability of Seed Removal",
                     limits = c(-0.016,1.02),
                     breaks= seq(from=0, to=1, by=0.25))+
  guides (colour = guide_legend(override.aes = list(size=15)))+
  theme_classic()+
  theme(
    axis.text = element_text(size = 35),
    #     axis.title = element_text(colour = "black", size = 28, face = "bold"),
    #     axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))
    legend.position = "none",
    axis.title = element_blank(),
    plot.margin = unit(c(0,1.15,1.1,0), "cm")
  )

#Plotting the graph legend
plot.rem.2018.legend <-
  ggplot(data = Nrem2018, aes(x = jitter(Sessenta, 20), y = jitter(as.numeric(Removidas),0.075)   # dados e eixos
                              ,col=Destino
  )) +
  # points
  geom_point( size=11, alpha=0.5, aes(shape=Destino, col=Destino) 
              #, col="red4"
  ) +  
  ##settig shapes
  scale_shape_manual(name= "Seed Fate",
                     values = c(19, 17, 15, 18),
                     labels=c("Dispersed", "Non-harvested", "Eaten", "Lost"))+
  ##setting collors
  scale_color_manual(name= "Seed Fate", 
                     values=c("chartreuse4", "deepskyblue", "firebrick2", "yellow3"),
                     labels=c("Dispersed", "Non-harvested", "Eaten", "Lost")
                    )+
  geom_line(data = df.predict.rem.2018, aes(y = pred, x = Sessenta ), 
            col= "black", size=7, alpha=0.8) +                    # general model line
  geom_line(data = df.predict.rem.rf.2018, aes(y = pred, x = Sessenta,
                                               group=Pontos ),
            size=1.1, alpha=0.9, col="grey") + # random effects model lines
  scale_x_continuous(
    name = "Agouti Relative Abundance",
    limits = c(-1,66),
    breaks= seq(from=0, to=60, by=15)) +
  scale_y_continuous(name = "Probability of Seed Removal",
                     limits = c(-0.016,1.02),
                     breaks= seq(from=0, to=1, by=0.25))+
  guides (colour = guide_legend(override.aes = list(size=15)))+
  theme_classic()+
  theme(
    axis.text = element_text(size = 22),
    #     axis.title = element_text(colour = "black", size = 28, face = "bold"),
    #     axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))
    #legend.position = "none",
    axis.title = element_blank(),
    legend.title = element_text(size=64),
    legend.text = element_text(size=58)) +
     guides(shape = guide_legend(override.aes = list(size = 30)))
  
##removal, two years together
Nrem2017 <- Nrem2017 %>% select(-X.2,-X.1,-X, -Cento_e_vinte) %>% 
  mutate(Year="2017")

Nrem2018 <- Nrem2018 %>% select(-X.1,-X,-X12,-X13, -Cento_e_vinte) %>% 
  mutate(Year="2018")

Nremto<- rbind(Nrem2017, Nrem2018)
Nremto <- Nremto %>%  mutate(Stationyear=paste(Nremto$Pontos, Nremto$Year))

#GLMM 
m.remto<-glmer(Removidas ~ (Sessenta) + (1|Stationyear), data=Nremto, family=binomial)
m.remto<-glmer(Removidas ~ (Sessenta) + (1|Pontos/Year), data=Nremto, family=binomial,control=glmerControl(calc.derivs=FALSE))


summary(m.remto)
summary(m.remto)$coefficients
## Making predictions to plot

# Values to be predicted considering Random Effects (rf)
rem.rf.to<-expand.grid(Pontos = unique(Nremto$Pontos), Year= unique(Nremto$Year),
                       Sessenta=seq(0, 65.5, 0.1))
#Predicting
predict.rem.rf.to<-predict(m.remto, newdata = rem.rf.to, type="response")
#saving into a data.frame
df.predict.rem.rf.to<-data.frame(pred = predict.rem.rf.to, rem.rf.to)

df.predict.rem.rf.to$Year <- word(df.predict.rem.rf.to$Stationyear,2)


# General prediction without random effects 
rem.to<-expand.grid(Sessenta = seq(0, 65.5, 0.1))
pred.rem.to<-predict(m.remto, newdata = rem.to, type="response", re.form=NA)
df.predict.rem.to <- data.frame(pred = pred.rem.to, rem.to)


#plot

  ggplot(data = Nremto, aes(x = jitter(Sessenta, 20), y = jitter(as.numeric(Removidas),0.075)   # dados e eixos
                              ,col=Destino)) + 
  scale_color_manual(name= "Seed Fate", 
                     values=c("chartreuse4", "deepskyblue", "firebrick2", "yellow3" ),
                     labels=c("Dispersed", "Non-harvested", "Preyed", "Lost"))+
  # points
  geom_point( size=11, alpha=0.5, aes(shape=Destino) ) +          
  ##settig shapes
  scale_shape_manual(values = c(19, 17, 15, 18)) +
  geom_line(data = df.predict.rem.to, aes(y = pred, x = Sessenta ), 
            col= "black", size=10, alpha=0.8) +                    # general model line
  geom_line(data = df.predict.rem.rf.2018, aes(y = pred, x = Sessenta,
                                                 group=Pontos),
              size=1.1, alpha=0.9, col="grey")+
    geom_line(data = df.predict.rem.rf.2017, aes(y = pred, x = Sessenta,
                                                 group=Pontos ),
              size=1.5, alpha=0.9, col="grey")+
  scale_x_continuous(
    name = "Number of Agouti Visits",
    limits = c(-1,66),
    breaks= seq(from=0, to=60, by=15)) +
  scale_y_continuous(name = "Probability of Seed Removal",
                     limits = c(-0.016,1.02),
                     breaks= seq(from=0, to=1, by=0.25))+
  theme_classic()+
  theme(
    axis.text = element_text(size = 35),
    #     axis.title = element_text(colour = "black", size = 28, face = "bold"),
    #     axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))
    legend.position = "none",
    axis.title = element_blank(),
    plot.margin = unit(c(0,1.15,1.1,0), "cm")
    
  )

### Seed survival------

## 2017

#reading data
Npreyed2017 <- read.csv("dados/2017 data/N.preyed.2017.csv")
Nsurvival2017 <- mutate(Npreyed2017, Survival=!Npreyed2017$Predadas)
# creating GLMM model
m.survival2017<-glmer(Survival ~ (Sessenta) + (1|Pontos), data=Nsurvival2017, family=binomial)

summary(m.survival2017)
summary(m.survival2017)$coefficients
## Making predictions to plot

# Values to be predicted considering Random Effects (rf)
survival.rf.2017<-expand.grid(Pontos = unique(Nsurvival2017$Pontos), Sessenta=seq(0, 65.5, 0.1))
#Predicting
predict.survival.rf.2017<-predict(m.survival2017, newdata = survival.rf.2017, type="response")
#saving into a data.frame
df.predict.survival.rf.2017<-data.frame(pred = predict.survival.rf.2017, survival.rf.2017)

# General prediction without random effects 
survival.2017<-expand.grid(Sessenta = seq(0, 65.5, 0.1))
pred.survival.2017<-predict(m.survival2017, newdata = survival.2017, type="response", re.form=NA)
df.predict.survival.2017 <- data.frame(pred = pred.survival.2017, survival.2017)

#Plotting the graph
plot.survival.2017 <- 
  ggplot(data = Nsurvival2017, aes(x = jitter(Sessenta, 20), y = jitter(as.numeric(Survival),0.075)   # dados e eixos
                              ,col=Destino
  )) + 
  # points
  geom_point( size=11, alpha=0.5, aes(shape=Destino, col=Destino) 
              #, col="red4"
  ) +  
  ##settig shapes
  scale_shape_manual(name= "Seed Fate",
                     values = c(19, 17, 15, 18),
                     labels=c("Dispersed", "Non-harvested", "Preyed", "Lost"))+
  ##setting collors
  scale_color_manual(name= "Seed Fate", 
                     values=c("chartreuse4", "deepskyblue", "firebrick2", "yellow3"),
                     labels=c("Dispersed", "Non-harvested", "Preyed", "Lost")
  )+
  geom_line(data = df.predict.survival.2017, aes(y = pred, x = Sessenta ), 
            col= "black", size=10, alpha=0.8) +                    # general model line
  geom_line(data = df.predict.survival.rf.2017, aes(y = pred, x = Sessenta,
                                               group=Pontos ),
            size=1.5, alpha=0.9, col="grey") + # random effects model lines
  scale_x_continuous(
    name = "Agouti Relative Abundance",
    limits = c(-1,66),
    breaks= seq(from=0, to=60, by=15)) +
  scale_y_continuous(name = "Probability of Seed survivaloval",
                     limits = c(-0.016,1.02),
                     breaks= seq(from=0, to=1, by=0.25))+
  theme_classic()+
  theme(
    axis.text = element_text(size = 35),
    #     axis.title = element_text(colour = "black", size = 28, face = "bold"),
    #     axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))
    legend.position = "none",
    axis.title = element_blank(),
    plot.margin = unit(c(0,1.15,1.1,0), "cm")
    
  )



## 2018


# creating survival data = opposite of preyed seeds
Nsurvival2018 <- cuti2018 %>%
  filter(Data=="2018-08-01") %>%
  filter(Destino!="S") %>% 
  mutate(Survival=Destino!='P')



#GLMM 
m.survival2018<-glmer(Survival ~ (Sessenta) + (1|Pontos), data=Nsurvival2018, family=binomial)

summary(m.survival2018)
summary(m.survival2018)$coefficients
## Making predictions to plot

# Values to be predicted considering Random Effects (rf)
survival.rf.2018<-expand.grid(Pontos = unique(Nsurvival2018$Pontos), Sessenta=seq(0, 65.5, 0.1))
#Predicting
predict.survival.rf.2018<-predict(m.survival2018, newdata = survival.rf.2018, type="response")
#Saving into a data.frame
df.predict.survival.rf.2018<-data.frame(pred = predict.survival.rf.2018, survival.rf.2018)

# General prediction without random effects 
survival.2018<-expand.grid(Sessenta = seq(0, 65.5, 0.1))
pred.survival.2018<-predict(m.survival2018, newdata = survival.2018, type="response", re.form=NA)
df.predict.survival.2018 <- data.frame(pred = pred.survival.2018, survival.2018)

#Plotting the graph
plot.survival.2018 <- 
  ggplot(data = Nsurvival2018, aes(x = jitter(Sessenta, 20), y = jitter(as.numeric(Survival),0.075)   # dados e eixos
                                   ,col=Destino
  )) + 
  # points
  geom_point( size=11, alpha=0.5, aes(shape=Destino, col=Destino) 
              #, col="red4"
  ) +  
  ##settig shapes
  scale_shape_manual(name= "Seed Fate",
                     values = c(19, 17, 15, 18),
                     labels=c("Dispersed", "Non-harvested", "Preyed", "Lost"))+
  ##setting collors
  scale_color_manual(name= "Seed Fate", 
                     values=c("chartreuse4", "deepskyblue", "firebrick2", "yellow3"),
                     labels=c("Dispersed", "Non-harvested", "Preyed", "Lost")
  )+
  geom_line(data = df.predict.survival.2018, aes(y = pred, x = Sessenta ), 
            col= "black", size=10, alpha=0.8) +                    # general model line
  geom_line(data = df.predict.survival.rf.2018, aes(y = pred, x = Sessenta,
                                                    group=Pontos ),
            size=1.5, alpha=0.9, col="grey") + # random effects model lines
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
    axis.text = element_text(size = 35),
    #     axis.title = element_text(colour = "black", size = 28, face = "bold"),
    #     axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))
    legend.position = "none",
    axis.title = element_blank(),
    plot.margin = unit(c(0,1.15,1.1,0), "cm")
    
  )


##Survival, two years together



Nsurvival2017 <- Nsurvival2017 %>% select(-X.1,-X, -Predadas) %>% 
  mutate(Year="2017")

Nsurvival2018 <- Nsurvival2018 %>% select(-X.1,-X,-X12,-X13) %>% 
  mutate(Year="2018")

Nsurvivalto<- rbind(Nsurvival2017, Nsurvival2018)
Nsurto <- Nsurvivalto %>%  mutate(Stationyear=paste(Nsurvivalto$Pontos, Nsurvivalto$Year))

#GLMM 
m.surto<-glmer(Survival ~ (Sessenta) + (1|Stationyear), data=Nsurto, family=binomial)



summary(m.surto)
summary(m.surto)$coefficients
## Making predictions to plot

# Values to be predicted considering Random Effects (rf)
sur.rf.to<-expand.grid(Stationyear = unique(Nsurto$Stationyear),Year=unique(Nsurto$Year), Sessenta=seq(0, 65.5, 0.1))
#Predicting
predict.sur.rf.to<-predict(m.surto, newdata = sur.rf.to, type="response")
#saving into a data.frame
df.predict.sur.rf.to<-data.frame(pred = predict.sur.rf.to, sur.rf.to)




# General prediction without random effects 
sur.to<-expand.grid(Sessenta = seq(0, 65.5, 0.1))
pred.sur.to<-predict(m.surto, newdata = sur.to, type="response", re.form=NA)
df.predict.sur.to <- data.frame(pred = pred.sur.to, sur.to)


#plot

ggplot(data = Nsurto, aes(x = jitter(Sessenta, 20), y = jitter(as.numeric(Survival),0.075)   # dados e eixos
                          ,col=Destino)) + 
  scale_color_manual(name= "Seed Fate", 
                     values=c("chartreuse4", "deepskyblue", "firebrick2", "yellow3" ),
                     labels=c("Dispersed", "Non-harvested", "Preyed", "Lost"))+
  # points
  geom_point( size=11, alpha=0.5, aes(shape=Destino) ) +          
  ##settig shapes
  scale_shape_manual(values = c(19, 17, 15, 18)) +
  geom_line(data = df.predict.sur.to, aes(y = pred, x = Sessenta ), 
            col= "black", size=10, alpha=0.8) +                    # general model line
geom_line(data = df.predict.survival.rf.2018, aes(y = pred, x = Sessenta,
                                               group=Pontos),
            size=0.9, alpha=0.2, col="red")+
  geom_line(data = df.predict.survival.rf.2017, aes(y = pred, x = Sessenta,
                                               group=Pontos ),
            size=0.9, alpha=0.2, col="blue")+
  geom_line(data = df.predict.survival.2018, aes(y = pred, x = Sessenta ), 
            col= "red", size=4, alpha=0.5)+
  geom_line(data = df.predict.survival.2017, aes(y = pred, x = Sessenta ), 
            col= "blue", size=4, alpha=0.5)+
  scale_x_continuous(
    name = "Number of Agouti Visits",
    limits = c(-1,66),
    breaks= seq(from=0, to=60, by=15)) +
  scale_y_continuous(name = "Probability of Seed Survival",
                     limits = c(-0.016,1.02),
                     breaks= seq(from=0, to=1, by=0.25))+
  theme_classic()+
  theme(
    axis.text = element_text(size = 35),
    #     axis.title = element_text(colour = "black", size = 28, face = "bold"),
    #     axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))
    legend.position = "none",
    axis.title = element_blank(),
    plot.margin = unit(c(0,1.15,1.1,0), "cm")
    
  )


#### Final Dispersal ------

## 2017

#reading data
Nfinaldisp2017 <- read.csv("dados/2017 data/Ndisp2.2017.csv")

##creating GLMM model
m.finaldisp2017<-glmer(Dispersas ~ scale(Sessenta) + scale(I(Sessenta^2)) + (1|Pontos), data=Nfinaldisp2017, family=binomial)

summary(m.finaldisp2017)
summary(m.finaldisp2017)$coefficients
####

###testing for best model comparing AIC values
m0<-glmer(Dispersas ~ 1 + (1|Pontos), data=Nfinaldisp2017, family=binomial)
m1<-glmer(Dispersas ~ scale(Sessenta) + (1|Pontos), data=Nfinaldisp2017, family=binomial)
m2<-glmer(Dispersas ~ scale(I(Sessenta^2)) + (1|Pontos), data=Nfinaldisp2017, family=binomial)
m3<-glmer(Dispersas ~ scale(Sessenta)* scale(I(Sessenta^2)) + (1|Pontos), data=Nfinaldisp2017, family=binomial)
m4<-glmer(Dispersas ~ scale(Sessenta): scale(I(Sessenta^2)) + (1|Pontos), data=Nfinaldisp2017, family=binomial)
m5<-glmer(Dispersas ~ scale(Sessenta)+ scale(Sessenta): scale(I(Sessenta^2)) + (1|Pontos), data=Nfinaldisp2017, family=binomial)

AICctab(m0,m1, m2,m3, m5, m.finaldisp2017, base=TRUE, weights=TRUE)
AICctab(m0,m1, m2,m3,m4, m5, m.finaldisp2017, base=TRUE, weights=TRUE)


## Making predictions to plot

# Values to be predicted considering Random Effects (rf)
finaldisp.rf.2017<-expand.grid(Pontos = unique(Nfinaldisp2017$Pontos), Sessenta=seq(0, 65.5, 0.1))
#Predicting
predict.finaldisp.rf.2017<-predict(m.finaldisp2017, newdata = finaldisp.rf.2017, type="response")
#saving into a data.frame
df.predict.finaldisp.rf.2017<-data.frame(pred = predict.finaldisp.rf.2017, finaldisp.rf.2017)

# General prediction without random effects 
finaldisp.2017<-expand.grid(Sessenta = seq(0, 65.5, 0.1))
pred.finaldisp.2017<-predict(m.finaldisp2017, newdata = finaldisp.2017, type="response", re.form=NA)
df.predict.finaldisp.2017 <- data.frame(pred = pred.finaldisp.2017, finaldisp.2017)

#Plotting the graph
plot.finaldisp.2017 <- 
  ggplot(data = Nfinaldisp2017, aes(x = jitter(Sessenta, 20), y = jitter(as.numeric(Dispersas),0.075)   # dados e eixos
                                   ,col=Destino
  )) + 
  # points
  geom_point( size=11, alpha=0.5, aes(shape=Destino, col=Destino) 
              #, col="red4"
  ) +  
  ##settig shapes
  scale_shape_manual(name= "Seed Fate",
                     values = c(19, 17, 15, 18),
                     labels=c("Dispersed", "Non-harvested", "Preyed", "Lost"))+
  ##setting collors
  scale_color_manual(name= "Seed Fate", 
                     values=c("chartreuse4", "deepskyblue", "firebrick2", "yellow3"),
                     labels=c("Dispersed", "Non-harvested", "Preyed", "Lost")
  )+
  geom_line(data = df.predict.finaldisp.2017, aes(y = pred, x = Sessenta ), 
            col= "black", size=10, alpha=0.8) +                    # general model line
  geom_line(data = df.predict.finaldisp.rf.2017, aes(y = pred, x = Sessenta,
                                                    group=Pontos ),
            size=1.5, alpha=0.9, col="grey") + # random effects model lines
  scale_x_continuous(
    name = "Agouti Relative Abundance",
    limits = c(-1,66),
    breaks= seq(from=0, to=60, by=15)) +
  scale_y_continuous(name = "Probability of Seed finaldispoval",
                     limits = c(-0.016,1.02),
                     breaks= seq(from=0, to=1, by=0.25))+
  theme_classic()+
  theme(
    axis.text = element_text(size = 35),
    #     axis.title = element_text(colour = "black", size = 28, face = "bold"),
    #     axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))
    legend.position = "none",
    axis.title = element_blank(),
    plot.margin = unit(c(0,1.1,0,0), "cm")
    
  )

## 2018

#data
Nfinaldisp2018 <- cuti2018 %>%
  filter(Data=="2018-08-01") %>%
  filter(Destino!="S") %>% 
  mutate(Dispersas=Destino=='D')



#GLMM 
m.finaldisp2018<-glmer(Dispersas ~ scale(Sessenta) + scale(I(Sessenta^2)) + (1|Pontos), data=Nfinaldisp2018, family=binomial)

summary(m.finaldisp2018)
summary(m.finaldisp2018)$coefficients

###testing for best model comparing AIC values
m0<-glmer(Dispersas ~ 1 + (1|Pontos), data=Nfinaldisp2018, family=binomial)
m1<-glmer(Dispersas ~ scale(Sessenta) + (1|Pontos), data=Nfinaldisp2018, family=binomial)
m2<-glmer(Dispersas ~ scale(I(Sessenta^2)) + (1|Pontos), data=Nfinaldisp2018, family=binomial)

AICctab(m0,m1, m2, m.finaldisp2018, base=TRUE, weights=TRUE)

## Making predictions to plot

# Values to be predicted considering Random Effects (rf)
finaldisp.rf.2018<-expand.grid(Pontos = unique(Nfinaldisp2018$Pontos), Sessenta=seq(0, 65.5, 0.1))
#Predicting
predict.finaldisp.rf.2018<-predict(m.finaldisp2018, newdata = finaldisp.rf.2018, type="response")
#Saving into a data.frame
df.predict.finaldisp.rf.2018<-data.frame(pred = predict.finaldisp.rf.2018, finaldisp.rf.2018)

# General prediction without random effects 
finaldisp.2018<-expand.grid(Sessenta = seq(0, 65.5, 0.1))
pred.finaldisp.2018<-predict(m.finaldisp2018, newdata = finaldisp.2018, type="response", re.form=NA)
df.predict.finaldisp.2018 <- data.frame(pred = pred.finaldisp.2018, finaldisp.2018)

#Plotting the graph
plot.finaldisp.2018 <- 
  ggplot(data = Nfinaldisp2018, aes(x = jitter(Sessenta, 20), y = jitter(as.numeric(Dispersas),0.075)   # dados e eixos
                                    ,col=Destino
  )) + 
  # points
  geom_point( size=11, alpha=0.5, aes(shape=Destino, col=Destino) 
              #, col="red4"
  ) +  
  ##settig shapes
  scale_shape_manual(name= "Seed Fate",
                     values = c(19, 17, 15, 18),
                     labels=c("Dispersed", "Non-harvested", "Preyed", "Lost"))+
  ##setting collors
  scale_color_manual(name= "Seed Fate", 
                     values=c("chartreuse4", "deepskyblue", "firebrick2", "yellow3"),
                     labels=c("Dispersed", "Non-harvested", "Preyed", "Lost")
  )+
  geom_line(data = df.predict.finaldisp.2018, aes(y = pred, x = Sessenta ), 
            col= "black", size=10, alpha=0.8) +                    # general model line
  geom_line(data = df.predict.finaldisp.rf.2018, aes(y = pred, x = Sessenta,
                                                     group=Pontos ),
            size=1.5, alpha=0.9, col="grey") + # random effects model lines
  scale_x_continuous(
    name = "Agouti Relative Abundance",
    limits = c(-1,66),
    breaks= seq(from=0, to=60, by=15)) +
  scale_y_continuous(name = "Probability of Seed finaldispoval",
                     limits = c(-0.016,1.02),
                     breaks= seq(from=0, to=1, by=0.25))+
  theme_classic()+
  theme(
    axis.text = element_text(size = 35),
    #     axis.title = element_text(colour = "black", size = 28, face = "bold"),
    #     axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))
    legend.position = "none",
    axis.title = element_blank(),
    plot.margin = unit(c(0,1.1,0,0), "cm")
    
  )

# dispersal two years together

Nfinaldisp2017 <- Nfinaldisp2017 %>% select(-X, -log30,-log60,-log120) %>% 
  mutate(Year="2017")

Nfinaldisp2018 <- Nfinaldisp2018 %>% select(-X.1,-X,-X12,-X13) %>% 
  mutate(Year="2018")

Nfinaldispto<- rbind(Nfinaldisp2017, Nfinaldisp2018)
Nfinaldispto <- Nfinaldispto %>%  mutate(Stationyear=paste(Nfinaldispto$Pontos, Nfinaldispto$Year))

#GLMM 

m.finaldispto1<-glmer(Dispersas ~ scale(Sessenta) + scale(I(Sessenta^2)) + (1|Stationyear), data=Nfinaldispto, family=binomial)

m.finaldispto<-glmer(Dispersas ~ scale(Sessenta) + scale(I(Sessenta^2)) + (1|Pontos)+(1|Year), data=Nfinaldispto, family=binomial)

summary(m.finaldispto)
summary(m.finaldispto)$coefficients

###testing for best model comparing AIC values
m0<-glmer(Dispersas ~ 1 + (1|Pontos)+(1|Year), data=Nfinaldispto, family=binomial)
m1<-glmer(Dispersas ~ scale(Sessenta) +(1|Pontos)+(1|Year), data=Nfinaldispto, family=binomial)
m2<-glmer(Dispersas ~ scale(I(Sessenta^2)) + (1|Pontos)+(1|Year), data=Nfinaldispto, family=binomial)

AICctab(m0,m1, m2, m.finaldispto, base=TRUE, weights=TRUE)

## Making predictions to plot

# Values to be predicted considering Random Effects (rf)
finaldisp.rf.to<-expand.grid(Pontos = unique(Nfinaldispto$Pontos),Year=unique(Nfinaldispto$Year), Sessenta=seq(0, 65.5, 0.1))
#Predicting
predict.finaldisp.rf.to<-predict(m.finaldispto, newdata = finaldisp.rf.to, type="response", allow.new.levels = TRUE)
#saving into a data.frame
df.predict.finaldisp.rf.to<-data.frame(pred = predict.finaldisp.rf.to, finaldisp.rf.to)

# Values to be predicted considering Random Effects (rf)
finaldisp.rf.to1<-expand.grid(Stationyear = unique(Nfinaldispto$Stationyear), Sessenta=seq(0, 65.5, 0.1))
#Predicting
predict.finaldisp.rf.to1<-predict(m.finaldispto1, newdata = finaldisp.rf.to1, type="response", allow.new.levels = TRUE)
#saving into a data.frame
df.predict.finaldisp.rf.to1<-data.frame(pred = predict.finaldisp.rf.to1, finaldisp.rf.to1)
df.predict.finaldisp.rf.to1$Year <- word(df.predict.finaldisp.rf.to1$Stationyear,2)


# General prediction without random effects 
finaldisp.to<-expand.grid(Sessenta = seq(0, 65.5, 0.1))
pred.finaldisp.to<-predict(m.finaldispto, newdata = finaldisp.to, type="response", re.form=NA)
df.predict.finaldisp.to <- data.frame(pred = pred.finaldisp.to, finaldisp.to)

# General prediction without random effects 
finaldisp.to1<-expand.grid(Sessenta = seq(0, 65.5, 0.1))
pred.finaldisp.to1<-predict(m.finaldispto1, newdata = finaldisp.to1, type="response", re.form=NA)
df.predict.finaldisp.to1 <- data.frame(pred = pred.finaldisp.to1, finaldisp.to)

#plot

ggplot(data = Nfinaldispto, aes(x = jitter(Sessenta, 20), y = jitter(as.numeric(Dispersas),0.075)   # dados e eixos
                          ,col=Destino)) + 
  # scale_color_manual(name= "Seed Fate", 
  #                    values=c("chartreuse4", "deepskyblue", "firebrick2", "yellow3" ),
  #                    labels=c("Dispersed", "Non-harvested", "Preyed", "Lost"))+
  # points
  geom_point( size=11, alpha=0.5, aes(shape=Destino) ) +          
  ##settig shapes
  scale_shape_manual(values = c(19, 17, 15, 18)) +
  geom_line(data = df.predict.finaldisp.to1, aes(y = pred, x = Sessenta ), 
            col= "black", size=10, alpha=0.8) + 
  geom_line(data = df.predict.finaldisp.rf.to1, aes(y = pred, x = Sessenta, group=Stationyear,
                                                    col=Year),size=1, alpha=0.5) +
  scale_x_continuous(
    name = "Number of Agouti Visits",
    limits = c(-1,66),
    breaks= seq(from=0, to=60, by=15)) +
  scale_y_continuous(name = "Probability of Seed Dispersaç",
                     limits = c(-0.016,1.02),
                     breaks= seq(from=0, to=1, by=0.25))+
  theme_classic()+
  theme(
    axis.text = element_text(size = 35),
    #     axis.title = element_text(colour = "black", size = 28, face = "bold"),
    #     axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))
    legend.position = "none",
    axis.title = element_blank(),
    plot.margin = unit(c(0,1.15,1.1,0), "cm")
    
  )


### alltogehter-
grid.arrange(plot.rem.2017, plot.rem.2018, plot.survival.2017,
             plot.survival.2018, plot.finaldisp.2017,
             plot.finaldisp.2018, ncol=2)










###Post-dispersal predation----------



## 2017

#reading data
Nretripred2017 <- read.csv("dados/2017 data/cuti.RP.2017.csv")
Nretripred2017 <-  Nretripred2017 %>%
  mutate(RP=Destino=="RP") %>% 
  filter(Data=="2017/08/15") %>% 
  filter(Destino!="S")

#GLMM 
m.retripred2017<-glmer(RP ~ (Sessenta) + (1|Pontos), data=Nretripred2017, family=binomial)
m0<-glmer(RP ~ 1 + (1|Pontos), data=Nretripred2017, family=binomial)

summary(m.retripred2017)
summary(m.retripred2017)$coefficients

## Making predictions to plot

# Values to be predicted considering Random Effects (rf)
retripred.rf.2017<-expand.grid(Pontos = unique(Nretripred2017$Pontos), Sessenta=seq(0, 66, 0.1))
#Predicting
predict.retripred.rf.2017<-predict(m.retripred2017, newdata = retripred.rf.2017, type="response")
#saving into a data.frame


df.predict.retripred.rf.2017<-data.frame(pred = predict.retripred.rf.2017, retripred.rf.2017)

# General prediction without random effects 
retripred.2017<-expand.grid(Sessenta = seq(0, 66, 0.1))
pred.retripred.2017<-predict(m.retripred2017, newdata = retripred.2017, type="response", re.form=NA)
df.predict.retripred.2017 <- data.frame(pred = pred.retripred.2017, retripred.2017)

#Plotting the graph
plot.retripred.2017 <- 
  ggplot(data = Nretripred2017, aes(x = jitter(Sessenta, 20), y = jitter(as.numeric(RP),0.075)   # dados e eixos
                              ,col=Destino
  )) + 
  # points
  geom_point( size=11, alpha=0.5, aes(shape=Destino, col=Destino) 
              #, col="red4"
  ) +  
  ##settig shapes
  scale_shape_manual(name= "Seed Fate",
                     values = c(19, 17, 15, 18),
                     labels=c("Dispersed", "Non-harvested", "Eaten (seed)", "Eaten (cache)"))+
  ##setting collors
  scale_color_manual(name= "Seed Fate", 
                     values=c("chartreuse4", "deepskyblue", "firebrick2", "firebrick4"),
                     labels=c("Dispersed", "Non-harvested",  "Eaten (seed)", "Eaten (cache)")
  )+
  geom_line(data = df.predict.retripred.2017, aes(y = pred, x = Sessenta ), 
            col= "black", size=10, alpha=0.8) +                    # general model line
  geom_line(data = df.predict.retripred.rf.2017, aes(y = pred, x = Sessenta,
                                               group=Pontos ),
            size=1.5, alpha=0.9, col="grey") + # random effects model lines
  scale_x_continuous(
    name = "Agouti Relative Abundance",
    limits = c(-1,66),
    breaks= seq(from=0, to=60, by=15)) +
  scale_y_continuous(name = "Probability of Seed POst-Dispersal Predation",
                     limits = c(-0.016,1.02),
                     breaks= seq(from=0, to=1, by=0.25))+
  theme_classic()+
  theme(
    axis.text = element_text(size = 35),
    #     axis.title = element_text(colour = "black", size = 28, face = "bold"),
    #     axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))
    legend.position = "none",
    axis.title = element_blank(),
    plot.margin = unit(c(0,1.9,0,0), "cm")
    
  )



## 2018

#reading data
Nretripred2018 <- read.csv2("dados/cutidRP.csv")

## adding space use data
cutiextud <- cuti2018 %>% 
  filter(Data=="2018-08-01") %>% 
  arrange(Pontos, Semente)

Nretripred2018 <-  Nretripred2018 %>%
  filter(Data=="2018/08/01") %>% 
  arrange(Pontos, Semente) %>% 
  mutate(Sessenta=cutiextud$Sessenta, RP=Destino=="RP") %>% 
  filter(Destino!="S")

#GLMM 
m.retripred2018<-glmer(RP ~ (Sessenta) + (1|Pontos), data=Nretripred2018, family=binomial)
m0<-glmer(RP ~ 1 + (1|Pontos), data=Nretripred2018, family=binomial)

summary(m.retripred2018)
summary(m.retripred2018)$coefficients

## Making predictions to plot

# Values to be predicted considering Random Effects (rf)
retripred.rf.2018<-expand.grid(Pontos = unique(Nretripred2018$Pontos), Sessenta=seq(0, 65.5, 0.1))
#Predicting
predict.retripred.rf.2018<-predict(m.retripred2018, newdata = retripred.rf.2018, type="response")
#saving into a data.frame
df.predict.retripred.rf.2018<-data.frame(pred = predict.retripred.rf.2018, retripred.rf.2018)

# General prediction without random effects 
retripred.2018<-expand.grid(Sessenta = seq(0, 65.5, 0.1))
pred.retripred.2018<-predict(m.retripred2018, newdata = retripred.2018, type="response", re.form=NA)
df.predict.retripred.2018 <- data.frame(pred = pred.retripred.2018, retripred.2018)

#Plotting the graph
plot.retripred.2018 <- 
  ggplot(data = Nretripred2018, aes(x = jitter(Sessenta, 20), y = jitter(as.numeric(RP),0.075)   # dados e eixos
                                    ,col=Destino
  )) + 
  # points
  geom_point( size=11, alpha=0.5, aes(shape=Destino, col=Destino) 
              #, col="red4"
  ) +  
  ##settig shapes
  scale_shape_manual(name= "Seed Fate",
                     values = c(19, 17, 15, 18),
                     labels=c("Dispersed", "Non-harvested", "Eaten (seed)", "Eaten (cache)"))+
  ##setting collors
  scale_color_manual(name= "Seed Fate", 
                     values=c("chartreuse4", "deepskyblue", "firebrick2", "firebrick4"),
                     labels=c("Dispersed", "Non-harvested",  "Eaten (seed)", "Eaten (cache)")
  )+
  geom_line(data = df.predict.retripred.2018, aes(y = pred, x = Sessenta ), 
            col= "black", size=10, alpha=0.8) +                    # general model line
  geom_line(data = df.predict.retripred.rf.2018, aes(y = pred, x = Sessenta,
                                                     group=Pontos ),
            size=1.5, alpha=0.9, col="grey") + # random effects model lines
  scale_x_continuous(
    name = "Agouti Relative Abundance",
    limits = c(-1,66),
    breaks= seq(from=0, to=60, by=15)) +
  scale_y_continuous(name = "Probability of Seed POst-Dispersal Predation",
                     limits = c(-0.016,1.02),
                     breaks= seq(from=0, to=1, by=0.25))+
  theme_classic()+
  theme(
    axis.text = element_text(size = 35),
    #     axis.title = element_text(colour = "black", size = 28, face = "bold"),
    #     axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))
    legend.position = "none",
    axis.title = element_blank()
    
  )

grid.arrange(plot.retripred.2017, plot.retripred.2018, ncol=2)

###two years together
Nretripred2017 <- Nretripred2017 %>% select(-X, -X.2,-X.1) %>% mutate(Year="2017")
Nretripred2018 <- Nretripred2018 %>% select(-X, -X12,-X13)  %>% mutate(Year="2018")

Nretripredto<- rbind(Nretripred2017, Nretripred2018)
Nretripredto <- Nretripredto %>%  mutate(Stationyear=paste(Nretripredto$Pontos, Nretripredto$Year))

#GLMM 

m.retripredto<-glmer(RP ~ Sessenta  + (1|Stationyear), data=Nretripredto, family=binomial)

summary(m.retripredto)
summary(m.finaldispto)$coefficients

# Values to be predicted considering Random Effects (rf)
retripred.rf.to<-expand.grid(Stationyear = unique(Nretripredto$Stationyear), Sessenta=seq(0, 65.5, 0.1))
#Predicting
predict.retripred.rf.to<-predict(m.retripredto, newdata = retripred.rf.to, type="response", allow.new.levels = TRUE)
#saving into a data.frame
df.predict.retripred.rf.to<-data.frame(pred = predict.retripred.rf.to, retripred.rf.to)
df.predict.retripred.rf.to$Year <- word(df.predict.retripred.rf.to$Stationyear,2)

# General prediction without random effects 
retripred.to<-expand.grid(Sessenta = seq(0, 65.5, 0.1))
pred.retripred.to<-predict(m.retripredto, newdata = retripred.to, type="response", re.form=NA)
df.predict.retripred.to <- data.frame(pred = pred.retripred.to, retripred.to)

#plot
  ggplot(data = Nretripredto, aes(x = jitter(Sessenta, 20), y = jitter(as.numeric(RP),0.075)   # dados e eixos
                                    ,col=Destino)) + 
  geom_point( size=8, alpha=0.5, aes(shape=Destino) ) +          
    ##settig shapes
    scale_shape_manual(name= "Seed Fate",
                       values = c(19, 17, 15, 18),
                       labels=c("Dispersed", "Non-harvested", "Eaten (seed)", "Eaten (cache)"))+
    ##setting collors
    scale_color_manual(name= "Seed Fate", 
                       values=c("chartreuse4", "deepskyblue", "firebrick2", "firebrick4"),
                       labels=c("Dispersed", "Non-harvested",  "Eaten (seed)", "Eaten (cache)"))+
    geom_line(data = df.predict.retripred.to, aes(y = pred, x = Sessenta ), 
              col= "black", size=12, alpha=0.8) +                    # general model line
    geom_line(data = df.predict.retripred.rf.to, aes(y = pred, x = Sessenta,
                                                      group=Stationyear),
              size=2, alpha=0.35, col="grey")+
    scale_x_continuous(
      name = "Number of Agouti Visits",
      limits = c(-1,66),
      breaks= seq(from=0, to=60, by=15)) +
    scale_y_continuous(name = "Probability of Cache Predation",
                       limits = c(-0.016,1.02),
                       breaks= seq(from=0, to=1, by=0.25))+
    theme_classic()+
    theme(
      axis.text = element_text(size = 30),
      #     axis.title = element_text(colour = "black", size = 28, face = "bold"),
      #     axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))
      legend.position = c(0.25,0.7),
      axis.title.x   = element_text(size = 30),
      axis.title.y   = element_text(size = 30),
      legend.title = element_text(size=20),
      legend.text = element_text(size=16)) +
    guides(shape = guide_legend(override.aes = list(size = 10)))
  #plot.margin = unit(c(0,1.15,1.1,0), "cm")
  
### Primary and early dispersal------

##2017
####

Nearlydispersal.2017 <- read.csv("dados/2017 data/cuti.RP.2017.csv")
### filtering data

Ndisp.2017 <- Nearlydispersal.2017 %>% 
  mutate(Dispersas=Destino=="D" & Mudanca=="S") %>%  
  filter ((Destino=="N" & Data=="2017/08/15")|Mudanca=="S") %>% 
  filter (Destino!="S")


Ndisp.2017$PontoSemente <- paste(Ndisp.2017$Pontos, 
                                           Ndisp.2017$Semente)
Np.dispersal2017 <- Ndisp.2017[which(!duplicated(Ndisp.2017$PontoSemente)),]

## models
m.p.dispersal2017 <- glmer(data=Np.dispersal2017, Dispersas~scale(Sessenta) + (1|Pontos),
                             family= binomial)
mod0<- glmer(data=Np.dispersal2017, Dispersas~1+ (1|Pontos),
                       family = binomial)
mod2 <- glmer(data=Np.dispersal2017, Dispersas ~ scale(I(Sessenta^2))+ (1|Pontos),
              family = binomial)
modtot <- glmer(data=Np.dispersal2017,Dispersas ~ scale(Sessenta) + scale(I(Sessenta^2))+ (1|Pontos),
                family = binomial)

AICctab(mod0,mod2, modtot, m.p.dispersal2017 , base=TRUE, weights=TRUE)


## Making predictions to plot

# Values to be predicted considering Random Effects (rf)
p.dispersal.rf.2017<-expand.grid(Pontos = unique(Np.dispersal2017$Pontos), Sessenta=seq(0, 65.5, 0.1))
#Predicting
predict.p.dispersal.rf.2017<-predict(m.p.dispersal2017, newdata = p.dispersal.rf.2017, type="response")
#saving into a data.frame
df.predict.p.dispersal.rf.2017<-data.frame(pred = predict.p.dispersal.rf.2017, p.dispersal.rf.2017)

# General prediction without random effects 
p.dispersal.2017<-expand.grid(Sessenta = seq(0, 65.5, 0.1))
pred.p.dispersal.2017<-predict(m.p.dispersal2017, newdata = p.dispersal.2017, type="response", re.form=NA)
df.predict.p.dispersal.2017 <- data.frame(pred = pred.p.dispersal.2017, p.dispersal.2017)

#Plotting the graph
plot.p.dispersal.2017 <- 
  ggplot(data = Np.dispersal2017, aes(x = jitter(Sessenta, 20), y = jitter(as.numeric(Dispersas),0.075)   # dados e eixos
                                   ,col=Destino
  )) + 
  # points
  geom_point( size=11, alpha=0.5, aes(shape=Destino, col=Destino) 
              #, col="red4"
  ) +  
  ##settig shapes
  scale_shape_manual(name= "Seed Fate",
                     values = c(19, 17, 15, 18),
                     labels=c("Dispersed", "Non-harvested", "Preyed", "Lost"))+
  ##setting collors
  scale_color_manual(name= "Seed Fate", 
                     values=c("chartreuse4", "deepskyblue", "firebrick2", "yellow3"),
                     labels=c("Dispersed", "Non-harvested", "Preyed", "Lost")
  )+
  geom_line(data = df.predict.p.dispersal.2017, aes(y = pred, x = Sessenta ), 
            col= "black", size=10, alpha=0.8) +                    # general model line
  geom_line(data = df.predict.p.dispersal.rf.2017, aes(y = pred, x = Sessenta,
                                                    group=Pontos ),
            size=1.5, alpha=0.9, col="grey") + # random effects model lines
  scale_x_continuous(
    name = "Agouti Relative Abundance",
    limits = c(-1,66),
    breaks= seq(from=0, to=60, by=15)) +
  scale_y_continuous(name = "Probability of Seed p.dispersaloval",
                     limits = c(-0.016,1.02),
                     breaks= seq(from=0, to=1, by=0.25))+
  theme_classic()+
  theme(
    axis.text = element_text(size = 35),
    #     axis.title = element_text(colour = "black", size = 28, face = "bold"),
    #     axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))
    legend.position = "none",
    axis.title = element_blank(),
    plot.margin = unit(c(0,1.15,1.1,0), "cm")
    
  )


#
#
#
###Early dispersal

Nearly.dispersal2017 <- Nearlydispersal.2017 %>% 
  filter(Data=="2017/05/11") %>% 
  mutate(Dispersas=Destino=="D") %>% 
  filter(Destino!="S")

## models
m.early.dispersal2017 <- glmer(data=Nearly.dispersal2017, Dispersas~scale(Sessenta) + (1|Pontos),
                              family= binomial)
mod0<- glmer(data=Nearly.dispersal2017, Dispersas~1+ (1|Pontos),
             family = binomial)
mod2 <- glmer(data=Nearly.dispersal2017, Dispersas ~ scale(I(Sessenta^2))+ (1|Pontos),
              family = binomial)
modtot <- glmer(data=Nearly.dispersal2017,Dispersas ~ scale(Sessenta) + scale(I(Sessenta^2))+ (1|Pontos),
                family = binomial)

AICctab(mod0,mod2, modtot, m.early.dispersal2017 , base=TRUE, weights=TRUE)


## Making predictions to plot

# Values to be predicted considering Random Effects (rf)
early.dispersal.rf.2017<-expand.grid(Pontos = unique(Nearly.dispersal2017$Pontos), Sessenta=seq(0, 65.5, 0.1))
#Predicting
predict.early.dispersal.rf.2017<-predict(m.early.dispersal2017, newdata = early.dispersal.rf.2017, type="response")
#saving into a data.frame
df.predict.early.dispersal.rf.2017<-data.frame(pred = predict.early.dispersal.rf.2017, early.dispersal.rf.2017)

# General prediction without random effects 
early.dispersal.2017<-expand.grid(Sessenta = seq(0, 65.5, 0.1))
pred.early.dispersal.2017<-predict(m.early.dispersal2017, newdata = early.dispersal.2017, type="response", re.form=NA)
df.predict.early.dispersal.2017 <- data.frame(pred = pred.early.dispersal.2017, early.dispersal.2017)

#Plotting the graph
plot.early.dispersal.2017 <- 
  ggplot(data = Nearly.dispersal2017, aes(x = jitter(Sessenta, 20), y = jitter(as.numeric(Dispersas),0.075)   # dados e eixos
                                      ,col=Destino
  )) + 
  # points
  geom_point( size=11, alpha=0.5, aes(shape=Destino, col=Destino) 
              #, col="red4"
  ) +  
  ##settig shapes
  scale_shape_manual(name= "Seed Fate",
                     values = c(19, 17, 15, 18),
                     labels=c("Dispersed", "Non-harvested", "Preyed", "Lost"))+
  ##setting collors
  scale_color_manual(name= "Seed Fate", 
                     values=c("chartreuse4", "deepskyblue", "firebrick2", "yellow3"),
                     labels=c("Dispersed", "Non-harvested", "Preyed", "Lost")
  )+
  geom_line(data = df.predict.early.dispersal.2017, aes(y = pred, x = Sessenta ), 
            col= "black", size=10, alpha=0.8) +                    # general model line
  geom_line(data = df.predict.early.dispersal.rf.2017, aes(y = pred, x = Sessenta,
                                                       group=Pontos ),
            size=1.5, alpha=0.9, col="grey") + # random effects model lines
  scale_x_continuous(
    name = "Agouti Relative Abundance",
    limits = c(-1,66),
    breaks= seq(from=0, to=60, by=15)) +
  scale_y_continuous(name = "Probability of Seed early.dispersaloval",
                     limits = c(-0.016,1.02),
                     breaks= seq(from=0, to=1, by=0.25))+
  theme_classic()+
  theme(
    axis.text = element_text(size = 35),
    #     axis.title = element_text(colour = "black", size = 28, face = "bold"),
    #     axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))
    legend.position = "none",
    axis.title = element_blank(),
    plot.margin = unit(c(0,1.1,0,0), "cm")
    
    )

## 2018

Nearlydispersal.2018 <- read.csv("dados/cutiext.2018.csv")

### filtering data

Np.dispersal2018 <- Nearlydispersal.2018 %>% 
  mutate(Dispersas=Destino=="D" & Mudanca=="S") %>%  
  filter ((Destino=="N" & Data=="2018-08-01")|Mudanca=="S") %>% 
  filter (Destino!="S")


Np.dispersal2018$PontoSemente <- paste(Np.dispersal2018$Pontos, 
                                 Np.dispersal2018$Semente)
Np.dispersal2018 <- Np.dispersal2018[which(!duplicated(Np.dispersal2018$PontoSemente)),]

## models
m.p.dispersal2018 <- glmer(data=Np.dispersal2018, Dispersas~scale(Sessenta) + (1|Pontos),
                           family= binomial)
mod0<- glmer(data=Np.dispersal2018, Dispersas~1+ (1|Pontos),
             family = binomial)
mod2 <- glmer(data=Np.dispersal2018, Dispersas ~ scale(I(Sessenta^2))+ (1|Pontos),
              family = binomial)
modtot <- glmer(data=Np.dispersal2018,Dispersas ~ scale(Sessenta) + scale(I(Sessenta^2))+ (1|Pontos),
                family = binomial)

AICctab(mod0,mod2, modtot, m.p.dispersal2018 , base=TRUE, weights=TRUE)
## Making predictions to plot

# Values to be predicted considering Random Effects (rf)
p.dispersal.rf.2018<-expand.grid(Pontos = unique(Np.dispersal2018$Pontos), Sessenta=seq(0, 65.5, 0.1))
#Predicting
predict.p.dispersal.rf.2018<-predict(m.p.dispersal2018, newdata = p.dispersal.rf.2018, type="response")
#saving into a data.frame
df.predict.p.dispersal.rf.2018<-data.frame(pred = predict.p.dispersal.rf.2018, p.dispersal.rf.2018)

# General prediction without random effects 
p.dispersal.2018<-expand.grid(Sessenta = seq(0, 65.5, 0.1))
pred.p.dispersal.2018<-predict(m.p.dispersal2018, newdata = p.dispersal.2018, type="response", re.form=NA)
df.predict.p.dispersal.2018 <- data.frame(pred = pred.p.dispersal.2018, p.dispersal.2018)

#Plotting the graph
plot.p.dispersal.2018 <- 
  ggplot(data = Np.dispersal2018, aes(x = jitter(Sessenta, 20), y = jitter(as.numeric(Dispersas),0.075)   # dados e eixos
                                      ,col=Destino
  )) + 
  # points
  geom_point( size=11, alpha=0.5, aes(shape=Destino, col=Destino) 
              #, col="red4"
  ) +  
  ##settig shapes
  scale_shape_manual(name= "Seed Fate",
                     values = c(19, 17, 15, 18),
                     labels=c("Dispersed", "Non-harvested", "Preyed", "Lost"))+
  ##setting collors
  scale_color_manual(name= "Seed Fate", 
                     values=c("chartreuse4", "deepskyblue", "firebrick2", "yellow3"),
                     labels=c("Dispersed", "Non-harvested", "Preyed", "Lost")
  )+
  geom_line(data = df.predict.p.dispersal.2018, aes(y = pred, x = Sessenta ), 
            col= "black", size=10, alpha=0.8) +                    # general model line
  geom_line(data = df.predict.p.dispersal.rf.2018, aes(y = pred, x = Sessenta,
                                                       group=Pontos ),
            size=1.5, alpha=0.9, col="grey") + # random effects model lines
  scale_x_continuous(
    name = "Agouti Relative Abundance",
    limits = c(-1,66),
    breaks= seq(from=0, to=60, by=15)) +
  scale_y_continuous(name = "Probability of Seed p.dispersaloval",
                     limits = c(-0.016,1.02),
                     breaks= seq(from=0, to=1, by=0.25))+
  theme_classic()+
  theme(
    axis.text = element_text(size = 35),
    #     axis.title = element_text(colour = "black", size = 28, face = "bold"),
    #     axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))
    legend.position = "none",
    axis.title = element_blank(),
    plot.margin = unit(c(0,1.15,1.1,0), "cm")
    
  )


#
#
#Early dispersal


Nearly.dispersal2018 <- read.csv("dados/cutiext.2018.csv")

Nearly.dispersal2018 <- Nearly.dispersal2018 %>% 
  filter(Data=="2018-05-02") %>% 
  mutate(Dispersas=Destino=="D") %>% 
  filter(Destino!="S")

## models
m.early.dispersal2018 <- glmer(data=Nearly.dispersal2018, Dispersas~scale(Sessenta) + (1|Pontos),
                               family= binomial)
mod0<- glmer(data=Nearly.dispersal2018, Dispersas~1+ (1|Pontos),
             family = binomial)
mod2 <- glmer(data=Nearly.dispersal2018, Dispersas ~ scale(I(Sessenta^2))+ (1|Pontos),
              family = binomial)
modtot <- glmer(data=Nearly.dispersal2018,Dispersas ~ scale(Sessenta) + scale(I(Sessenta^2))+ (1|Pontos),
                family = binomial)

AICctab(mod0,mod2, modtot, m.early.dispersal2018 , base=TRUE, weights=TRUE)


## Making predictions to plot

# Values to be predicted considering Random Effects (rf)
early.dispersal.rf.2018<-expand.grid(Pontos = unique(Nearly.dispersal2018$Pontos), Sessenta=seq(0, 65.5, 0.1))
#Predicting
predict.early.dispersal.rf.2018<-predict(m.early.dispersal2018, newdata = early.dispersal.rf.2018, type="response")
#saving into a data.frame
df.predict.early.dispersal.rf.2018<-data.frame(pred = predict.early.dispersal.rf.2018, early.dispersal.rf.2018)

# General prediction without random effects 
early.dispersal.2018<-expand.grid(Sessenta = seq(0, 65.5, 0.1))
pred.early.dispersal.2018<-predict(m.early.dispersal2018, newdata = early.dispersal.2018, type="response", re.form=NA)
df.predict.early.dispersal.2018 <- data.frame(pred = pred.early.dispersal.2018, early.dispersal.2018)

#Plotting the graph
plot.early.dispersal.2018 <- 
  ggplot(data = Nearly.dispersal2018, aes(x = jitter(Sessenta, 20), y = jitter(as.numeric(Dispersas),0.075)   # dados e eixos
                                          ,col=Destino
  )) + 
  # points
  geom_point( size=11, alpha=0.5, aes(shape=Destino, col=Destino) 
              #, col="red4"
  ) +  
  ##settig shapes
  scale_shape_manual(name= "Seed Fate",
                     values = c(19, 17, 15, 18),
                     labels=c("Dispersed", "Non-harvested", "Preyed", "Lost"))+
  ##setting collors
  scale_color_manual(name= "Seed Fate", 
                     values=c("chartreuse4", "deepskyblue", "firebrick2", "yellow3"),
                     labels=c("Dispersed", "Non-harvested", "Preyed", "Lost")
  )+
  geom_line(data = df.predict.early.dispersal.2018, aes(y = pred, x = Sessenta ), 
            col= "black", size=10, alpha=0.8) +                    # general model line
  geom_line(data = df.predict.early.dispersal.rf.2018, aes(y = pred, x = Sessenta,
                                                           group=Pontos ),
            size=1.5, alpha=0.9, col="grey") + # random effects model lines
  scale_x_continuous(
    name = "Agouti Relative Abundance",
    limits = c(-1,66),
    breaks= seq(from=0, to=60, by=15)) +
  scale_y_continuous(name = "Probability of Seed early.dispersaloval",
                     limits = c(-0.016,1.02),
                     breaks= seq(from=0, to=1, by=0.25))+
  theme_classic()+
  theme(
    axis.text = element_text(size = 35),
    #     axis.title = element_text(colour = "black", size = 28, face = "bold"),
    #     axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))
    legend.position = "none",
    axis.title = element_blank(),
    plot.margin = unit(c(0,1.1,0,0), "cm")
    
    )


#Np.dispersal2017 <- Np.dispersal2017 %>% select(-X,-X.1,-X.2,-N) %>% 
Np.dispersal2017 <- Np.dispersal2017 %>% mutate(Year="2017")
Np.dispersal2018 <- Np.dispersal2018 %>% select(-X,-X.1,-X12,-X13,
                                                -Cento_e_vinte,-Trinta) %>% 
  mutate(Year="2018")

Np.dispersalto<- rbind(Np.dispersal2017, Np.dispersal2018)
Np.dispersalto <- Np.dispersalto %>%  mutate(Stationyear=paste(Np.dispersalto$Pontos, Np.dispersalto$Year))

#GLMM 

m.p.dispersalto <- glmer(Dispersas ~ Sessenta  + (1|Stationyear), data=Np.dispersalto, family=binomial)
m.p.dispersalto1 <- glmer(Dispersas ~ Sessenta  + (1|Pontos)+(1|Year), data=Np.dispersalto, family=binomial)


summary(m.p.dispersalto)
summary(m.p.dispersalto)$coefficients

# Values to be predicted considering Random Effects (rf)
p.dispersal.rf.to<-expand.grid(Stationyear = unique(Np.dispersalto$Stationyear), Sessenta=seq(0, 65.5, 0.1))
#Predicting
predict.p.dispersal.rf.to<-predict(m.p.dispersalto, newdata = p.dispersal.rf.to, type="response", allow.new.levels = TRUE)
#saving into a data.frame
df.predict.p.dispersal.rf.to<-data.frame(pred = predict.p.dispersal.rf.to, p.dispersal.rf.to)
df.predict.p.dispersal.rf.to$Year <- word(df.predict.p.dispersal.rf.to$Stationyear,2)




# General prediction without random effects 
p.dispersal.to<-expand.grid(Sessenta = seq(0, 65.5, 0.1))
pred.p.dispersal.to<-predict(m.p.dispersalto, newdata = p.dispersal.to, type="response", re.form=NA)
df.predict.p.dispersal.to <- data.frame(pred = pred.p.dispersal.to, p.dispersal.to)


#plot
 ggplot(data = Np.dispersalto, aes(x = jitter(Sessenta, 20), y = jitter(as.numeric(Dispersas),0.075)   # dados e eixos
                                         ,col=Destino)) + 
  geom_point( size=8, alpha=0.5, aes(shape=Destino) ) +          
  ##settig shapes
  scale_shape_manual(name= "Seed Fate",
                     values = c(19, 17, 15, 18),
                     labels=c("Dispersed", "Non-harvested", "Eaten", "Lost"))+
  ##setting collors
  scale_color_manual(name= "Seed Fate", 
                     values=c("chartreuse4", "deepskyblue", "firebrick2", "yellow3"),
                     labels=c("Dispersed", "Non-harvested", "Eaten", "Lost"))+
  geom_line(data = df.predict.p.dispersal.to, aes(y = pred, x = Sessenta ), 
            col= "black", size=12, alpha=0.8) +                    # general model line
  geom_line(data = df.predict.p.dispersal.rf.to, aes(y = pred, x = Sessenta,
                                                    group=Stationyear),
            size=2, alpha=0.35, col="grey")+
  scale_x_continuous(
    name = "Number of Agouti Visits",
    limits = c(-1,66),
    breaks= seq(from=0, to=60, by=15)) +
  scale_y_continuous(name = "Probability of\n Primary Dispersal",
                     limits = c(-0.016,1.02),
                     breaks= seq(from=0, to=1, by=0.25))+
  theme_classic()+
  theme(
    axis.text = element_text(size = 30),
    #     axis.title = element_text(colour = "black", size = 28, face = "bold"),
    #     axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))
    legend.position = "none",
    axis.title.x   = element_text(size = 30),
    axis.title.y   = element_text(size = 30))+
  guides(shape = guide_legend(override.aes = list(size = 10)))
#plot.margin = unit(c(0,1.15,1.1,0), "cm")

 
## together early dispersal


#Nearly.dispersal2017 <- Nearly.dispersal2017 %>% select(-X,-X.1,-X.2,-N) %>% 
  Nearly.dispersal2017 <- Nearly.dispersal2017 %>% mutate(Year="2017")
Nearly.dispersal2018 <- Nearly.dispersal2018 %>% select(-X,-X.1,-X12,-X13,
                                                -Cento_e_vinte,-Trinta) %>% 
  mutate(Year="2018")

Nearly.dispersalto<- rbind(Nearly.dispersal2017, Nearly.dispersal2018)
Nearly.dispersalto <- Nearly.dispersalto %>%  mutate(Stationyear=paste(Nearly.dispersalto$Pontos, Nearly.dispersalto$Year))

#GLMM 

m.early.dispersalto <- glmer(Dispersas ~ Sessenta  + (1|Stationyear), data=Nearly.dispersalto, family=binomial)


summary(m.early.dispersalto)
summary(m.early.dispersalto)$coefficients

# Values to be predicted considering Random Effects (rf)
early.dispersal.rf.to<-expand.grid(Stationyear = unique(Nearly.dispersalto$Stationyear), Sessenta=seq(0, 65.5, 0.1))
#Predicting
predict.early.dispersal.rf.to<-predict(m.early.dispersalto, newdata = early.dispersal.rf.to, type="response", allow.new.levels = TRUE)
#saving into a data.frame
df.predict.early.dispersal.rf.to<-data.frame(pred = predict.early.dispersal.rf.to, early.dispersal.rf.to)
df.predict.early.dispersal.rf.to$Year <- word(df.predict.early.dispersal.rf.to$Stationyear,2)




# General prediction without random effects 
early.dispersal.to<-expand.grid(Sessenta = seq(0, 65.5, 0.1))
pred.early.dispersal.to<-predict(m.early.dispersalto, newdata = early.dispersal.to, type="response", re.form=NA)
df.predict.early.dispersal.to <- data.frame(pred = pred.early.dispersal.to, early.dispersal.to)


#plot
ggplot(data = Nearly.dispersalto, aes(x = jitter(Sessenta, 20), y = jitter(as.numeric(Dispersas),0.075)   # dados e eixos
                                  ,col=Destino)) + 
  geom_point( size=8, alpha=0.5, aes(shape=Destino) ) +          
  ##settig shapes
  scale_shape_manual(name= "Seed Fate",
                     values = c(19, 17, 15, 18),
                     labels=c("Dispersed", "Non-harvested", "Eaten", "Lost"))+
  ##setting collors
  scale_color_manual(name= "Seed Fate", 
                     values=c("chartreuse4", "deepskyblue", "firebrick2", "yellow3"),
                     labels=c("Dispersed", "Non-harvested", "Eaten", "Lost"))+
  geom_line(data = df.predict.early.dispersal.to, aes(y = pred, x = Sessenta ), 
            col= "black", size=12, alpha=0.8) +                    # general model line
  geom_line(data = df.predict.early.dispersal.rf.to, aes(y = pred, x = Sessenta,
                                                     group=Stationyear),
            size=2, alpha=0.35, col="grey")+
  scale_x_continuous(
    name = "Number of Agouti Visits",
    limits = c(-1,66),
    breaks= seq(from=0, to=60, by=15)) +
  scale_y_continuous(name = "Probability of\n Early Dispersal",
                     limits = c(-0.016,1.02),
                     breaks= seq(from=0, to=1, by=0.25))+
  theme_classic()+
  theme(
    axis.text = element_text(size = 30),
    #     axis.title = element_text(colour = "black", size = 28, face = "bold"),
    #     axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))
    legend.position = "none",
    axis.title.x   = element_text(size = 30),
    axis.title.y   = element_text(size = 30))+
  guides(shape = guide_legend(override.aes = list(size = 10)))
 
### alltogehter-
grid.arrange(plot.p.dispersal.2017, plot.p.dispersal.2018, 
             plot.early.dispersal.2017, plot.early.dispersal.2018,
              ncol=2)

