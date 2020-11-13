
##removal, two years together-------
#Nrem2017 <- Nrem2017 %>% select(-X.2,-X.1,-X, -Cento_e_vinte) %>% 
  #mutate(Year="2017")

#Nrem2018 <- Nrem2018 %>% select(-X.1,-X,-X12,-X13, -Cento_e_vinte) %>% 
  #mutate(Year="2018")

Nremto<- rbind(Nrem2017, Nrem2018)
Nremto <- Nremto %>%  mutate(Stationyear=paste(Nremto$Pontos, Nremto$Year))

#GLMM 
m.remto<-glmer(Removidas ~ (Sessenta) + (1|Stationyear), data=Nremto, family=binomial)
m.remto1<-glmer(Removidas ~ (Sessenta) + (1|Pontos/Year), data=Nremto, family=binomial,control=glmerControl(calc.derivs=FALSE))

summary(m.remto1)
summary(m.remto)$coefficients
## Making predictions to plot

# Values to be predicted considering Random Effects (rf)
rem.rf.to<-expand.grid(Stationyear = unique(Nremto$Stationyear),
                       Sessenta=seq(0, 65.5, 0.1))
#Predicting
predict.rem.rf.to<-predict(m.remto, newdata = rem.rf.to, type="response")
#saving into a data.frame
df.predict.rem.rf.to<-data.frame(pred = predict.rem.rf.to, rem.rf.to)

df.predict.rem.rf.to$Year <- stringr::word(df.predict.rem.rf.to$Stationyear,2)


# General prediction without random effects 
rem.to<-expand.grid(Sessenta = seq(0, 65.5, 0.1))
pred.rem.to<-predict(m.remto, newdata = rem.to, type="response", re.form=NA)
df.predict.rem.to <- data.frame(pred = pred.rem.to, rem.to)


#plot

r.plot<- ggplot(data = Nremto, aes(x = jitter(Sessenta, 20), y = jitter(as.numeric(Removidas),0.075)   # dados e eixos
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
  geom_line(data = df.predict.rem.to, aes(y = pred, x = Sessenta ), 
            col= "black", size=12, alpha=0.8) +                    # general model line
  geom_line(data = df.predict.rem.rf.2018, aes(y = pred, x = Sessenta,
                                               group=Pontos),
            size=2, alpha=0.35, col="grey")+
  geom_line(data = df.predict.rem.rf.2017, aes(y = pred, x = Sessenta,
                                               group=Pontos ),
            size=2, alpha=0.35, col="grey")+
  scale_x_continuous(
    name = "Number of Agouti Visits",
    limits = c(-1,66),
    breaks= seq(from=0, to=60, by=15)) +
  scale_y_continuous(name = "Probability of Seed Removal",
                     limits = c(-0.016,1.02),
                     breaks= seq(from=0, to=1, by=0.25))+
  theme_classic()+
  theme(
    axis.text = element_text(size = 30),
    #     axis.title = element_text(colour = "black", size = 28, face = "bold"),
    #     axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))
    legend.position = c(0.8,0.6),
    axis.title.x   = element_blank(),
    axis.title.y   = element_text(size = 30),
    legend.title = element_text(size=20),
    legend.text = element_text(size=16)) +
  guides(shape = guide_legend(override.aes = list(size = 10)))
  
  
##Survival, two years together-----
  
  

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
  

  
s.plot<- ggplot(data = Nsurto, aes(x = jitter(Sessenta, 20), y = jitter(as.numeric(Survival),0.075)   # dados e eixos
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
  geom_line(data = df.predict.sur.to, aes(y = pred, x = Sessenta ), 
            col= "black", size=12, alpha=0.8) +                    # general model line
    geom_line(data = df.predict.survival.rf.2017, aes(y = pred, x = Sessenta,
                                                 group=Pontos),
              size=2, alpha=0.35, col="grey")+
    geom_line(data = df.predict.survival.rf.2018, aes(y = pred, x = Sessenta,
                                                 group=Pontos ),
              size=2, alpha=0.35, col="grey")+
    scale_x_continuous(
      name = "Number of Agouti Visits",
      limits = c(-1,66),
      breaks= seq(from=0, to=60, by=15)) +
    scale_y_continuous(name = "Probability of Seed Survival",
                       limits = c(-0.016,1.02),
                       breaks= seq(from=0, to=1, by=0.25))+
    theme_classic()+
    theme(
      axis.text = element_text(size = 30),
      #     axis.title = element_text(colour = "black", size = 28, face = "bold"),
      #     axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))
      legend.position = c(0.8,0.7),
      axis.title.x   = element_blank(),
      axis.title.y   = element_text(size = 30),
      legend.title = element_text(size=20),
      legend.text = element_text(size=16)) +
  guides(shape = guide_legend(override.aes = list(size = 10)))
      #plot.margin = unit(c(0,1.15,1.1,0), "cm")
      
    
##Dispersal, two years together-------

#GLMM 

m.finaldispto1<-glmer(Dispersas ~ scale(Sessenta) + scale(I(Sessenta^2)) + (1|Stationyear), data=Nfinaldispto, family=binomial)

m.finaldispto<-glmer(Dispersas ~ scale(Sessenta) + scale(I(Sessenta^2)) + (1|Pontos)+(1|Year), data=Nfinaldispto, family=binomial)

summary(m.finaldispto)
summary(m.finaldispto1)$coefficients

###testing for best model comparing AIC values
m0<-glmer(Dispersas ~ 1 + (1|Stationyear), data=Nfinaldispto, family=binomial)
m1<-glmer(Dispersas ~ scale(Sessenta) +(1|Stationyear), data=Nfinaldispto, family=binomial)
m2<-glmer(Dispersas ~ scale(I(Sessenta^2)) + (1|Stationyear), data=Nfinaldispto, family=binomial)

AICctab(m0,m1, m2, m.finaldispto, base=TRUE, weights=TRUE)

## Making predictions to plot

# # Values to be predicted considering Random Effects (rf)
# finaldisp.rf.to<-expand.grid(Pontos = unique(Nfinaldispto$Pontos),Year=unique(Nfinaldispto$Year), Sessenta=seq(0, 65.5, 0.1))
# #Predicting
# predict.finaldisp.rf.to<-predict(m.finaldispto, newdata = finaldisp.rf.to, type="response", allow.new.levels = TRUE)
# #saving into a data.frame
# df.predict.finaldisp.rf.to<-data.frame(pred = predict.finaldisp.rf.to, finaldisp.rf.to)

# Values to be predicted considering Random Effects (rf)
finaldisp.rf.to1<-expand.grid(Stationyear = unique(Nfinaldispto$Stationyear), Sessenta=seq(0, 65.5, 0.1))
#Predicting
predict.finaldisp.rf.to1<-predict(m.finaldispto1, newdata = finaldisp.rf.to1, type="response", allow.new.levels = TRUE)
#saving into a data.frame
df.predict.finaldisp.rf.to1<-data.frame(pred = predict.finaldisp.rf.to1, finaldisp.rf.to1)
df.predict.finaldisp.rf.to1$Year <- word(df.predict.finaldisp.rf.to1$Stationyear,2)


# # General prediction without random effects 
# finaldisp.to<-expand.grid(Sessenta = seq(0, 65.5, 0.1))
# pred.finaldisp.to<-predict(m.finaldispto, newdata = finaldisp.to, type="response", re.form=NA)
# df.predict.finaldisp.to <- data.frame(pred = pred.finaldisp.to, finaldisp.to)
# 

# General prediction without random effects 
finaldisp.to1<-expand.grid(Sessenta = seq(0, 65.5, 0.1))
pred.finaldisp.to1<-predict(m.finaldispto1, newdata = finaldisp.to1, type="response", re.form=NA)
df.predict.finaldisp.to1 <- data.frame(pred = pred.finaldisp.to1, finaldisp.to)

#plot

d.plot<- ggplot(data = Nfinaldispto, aes(x = jitter(Sessenta, 20), y = jitter(as.numeric(Dispersas),0.075)   # dados e eixos
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
  geom_line(data = df.predict.finaldisp.to1, aes(y = pred, x = Sessenta ), 
            col= "black", size=12, alpha=0.8) +                    # general model line
  geom_line(data = df.predict.finaldisp.rf.to1, aes(y = pred, x = Sessenta,
                                                    group=Stationyear),
            size=2, alpha=0.35, col="grey")+
  scale_x_continuous(
    name = "Number of Agouti Visits",
    limits = c(-1,66),
    breaks= seq(from=0, to=60, by=15)) +
  scale_y_continuous(name = "Probability of Seed Dispersal",
                     limits = c(-0.016,1.02),
                     breaks= seq(from=0, to=1, by=0.25))+
  theme_classic()+
  theme(
    axis.text = element_text(size = 30),
    #     axis.title = element_text(colour = "black", size = 28, face = "bold"),
    #     axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))
    legend.position = c(0.8,0.7),
    axis.title.x   = element_blank(),
    axis.title.y   = element_text(size = 30),
    legend.title = element_text(size=20),
    legend.text = element_text(size=16)) +
  guides(shape = guide_legend(override.aes = list(size = 10)))
    #plot.margin = unit(c(0,1.15,1.1,0), "cm")
    
  




### alltogehter-----
grid.arrange(r.plot,s.plot,d.plot, ncol=1)

##cache predation-----

###two years together
#Nretripred2017 <- Nretripred2017 %>% select(-X, -X.2,-X.1) %>% mutate(Year="2017")
#Nretripred2018 <- Nretripred2018 %>% select(-X, -X12,-X13)  %>% mutate(Year="2018")

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

##primary dispersal----
Np.dispersal2017<- Np.dispersal2017 %>% select(-X, -X.2,-X.1,-N) %>% mutate(Year="2017")

Np.dispersalto<- rbind(Np.dispersal2017, Np.dispersal2018)
Np.dispersalto <- Np.dispersalto %>%  mutate(Stationyear=paste(Np.dispersalto$Pontos, Np.dispersalto$Year))

#GLMM 

m.p.dispersalto <- glmer(Dispersas ~ Sessenta  + (1|Stationyear), data=Np.dispersalto, family=binomial)
m.p.dispersalto1 <- glmer(Dispersas ~ Sessenta  + (1|Pontos)+(1|Year), data=Np.dispersalto, family=binomial)

m0 <- glmer(Dispersas ~ 1 + (1|Stationyear), data=Np.dispersalto, family=binomial)
m1 <- glmer(Dispersas ~ scale(I(Sessenta^2)) + (1|Stationyear), data=Np.dispersalto, family=binomial)
m2 <- glmer(Dispersas ~ scale(I(Sessenta^2)) + scale(Sessenta)+ (1|Stationyear), data=Np.dispersalto, family=binomial)

AICctab(m0,m1, m2, m.p.dispersalto, base=TRUE, weights=TRUE)

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
Primary.plot<- ggplot(data = Np.dispersalto, aes(x = jitter(Sessenta, 20), y = jitter(as.numeric(Dispersas),0.075)   # dados e eixos
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


##early dispersal-----
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
Nearly.plot<- ggplot(data = Nearly.dispersalto, aes(x = jitter(Sessenta, 20), y = jitter(as.numeric(Dispersas),0.075)   # dados e eixos
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

grid.arrange(Nearly.plot, Primary.plot, ncol=1)
