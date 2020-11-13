# BOXPLOT OF SEED FATES ACCORDING TO NUMBER OF AGOUTI VISITS

# Packages
library(tidyverse)
library("ggExtra")
#labels=c("Dispersed", "Non-harvested",  "Eaten (seed)", "Eaten (cache)"))

Nretripredto<- rbind(Nretripred2017, Nretripred2018)
Nretripredto$Destino[which(Nretripredto$Destino=="D")] <- "Final\nDispersed\nn=39"
Nretripredto$Destino[which(Nretripredto$Destino=="N")] <- "Non\nharvested\nn=118"
Nretripredto$Destino[which(Nretripredto$Destino=="P")] <- "Eaten\n(seed)\nn=73"
Nretripredto$Destino[which(Nretripredto$Destino=="RP")] <- "Eaten\n(cache)\nn=48"

Nretripredto$Destino <- factor(Nretripredto$Destino,
                               levels = c("Eaten\n(cache)\nn=48",
                                          "Eaten\n(seed)\nn=73",
                                          "Final\nDispersed\nn=39",
                                          "Non\nharvested\nn=118"))

ggplot(data=Nretripredto, aes(x=Destino, y= Sessenta, fill=Destino))+
  geom_boxplot(alpha=0.75, lwd=1.5)+
  xlab("Seed fate")+
  ylab("Number of Agouti Visits")+
  scale_fill_manual( values=c("firebrick4","firebrick2","chartreuse4", "deepskyblue"))+
  theme_minimal()+
  theme(legend.position = "none",
        axis.title = element_text(colour = "black", size = 30),
        axis.text = element_text(size = 24))+
  coord_flip()


bremo<- filter(Nremto, Destino!="N") %>% ggplot(aes(x= jitter(Sessenta, 20), y= jitter(as.numeric(Removidas),0.075)))+
  geom_point()
  
ggMarginal(bremo, type="boxplot", margins = 'x', color="black",
           fill= "deepskyblue", alpha=0.2, size=4)


sremo<- filter(Nsurvivalto, Destino!="P") %>% ggplot(aes(x= jitter(Sessenta, 20), y= jitter(as.numeric(Survival),0.075)))+
  geom_point()

ggMarginal(sremo, type="boxplot", margins = 'x', color="black",
           fill= "deepskyblue", alpha=0.2, size=4)

dremo<- filter(Nfinaldispto, Destino=="D") %>% ggplot(aes(x= jitter(Sessenta, 20), y= jitter(as.numeric(Dispersas),0.075)))+
  geom_point()

ggMarginal(dremo, type="boxplot", margins = 'x', color="black",
           fill= "deepskyblue", alpha=0.2, size=4)
