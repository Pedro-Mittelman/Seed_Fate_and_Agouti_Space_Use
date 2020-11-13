library(tidyverse)
library(ggplot2) 
library(scales) # for colours
library(RCurl) # for colours
library(gridExtra) # plotting graphs together
library(grid) # plotting graphs together

##adjusting colors to be used

firebrick4k <- adjustcolor("firebrick4", alpha.f= 5/10)
firebrick2k <- adjustcolor("firebrick2", alpha.f= 5/10)
yellow3k <- adjustcolor("yellow3", alpha.f= 5/10)
deepskybluek <- adjustcolor("deepskyblue", alpha.f= 2.5/10)
chartreuse4k <- adjustcolor("chartreuse4", alpha.f= 7/10)
adj.colors <- c(chartreuse4k, firebrick4k, firebrick2k, yellow3k, deepskybluek )

## reading data
cuti.RP2.2017 <- read.csv("dados/cutiRP2.2017.csv")


## withdrawing stations with field experimental problems
cuti.RP2.2017<- cuti.RP2.2017[which(cuti.RP2.2017$Pontos!="AFV02"
                                    & cuti.RP2.2017$Pontos!="AFV14" ),]

### creating categories of abundance------

## Sepparating the categories of Agouti Abundance 2017

#low
low.2017 <- cuti.RP2.2017[which(cuti.RP2.2017$Sessenta<11),]
unique(low.2017$Pontos)

#Medium
medium.2017 <- cuti.RP2.2017[which(cuti.RP2.2017$Sessenta>11 & cuti.RP2.2017$Sessenta<30),]
unique(medium.2017$Pontos)

#High
high.2017 <- cuti.RP2.2017[which(cuti.RP2.2017$Sessenta>30),]
unique(high.2017$Pontos)




## Sepparating the categories of Agouti Abundance 2018

##reading 2018 data
cutiRP2 <- read.csv("dados/cutiRP2.2108.csv")

#low
low.2018 <- cutiRP2[which(cutiRP2$Sessenta<=15),]
unique(low.2018$Pontos)

#Medium
medium.2018 <- cutiRP2[which(cutiRP2$Sessenta>15 & cutiRP2$Sessenta<35),]
unique(medium.2018$Pontos)

#High
high.2018<- cutiRP2[which(cutiRP2$Sessenta>=35),]
unique(high.2018$Pontos)









###graphs------


###LOW
##
##filterin data to low abundance last date 2017
prop.low.2017 <- low.2017 %>%
  filter(Data=="2017/08/15") %>% 
  summarise(Nada=sum(Destino=="N"), Sumidas=sum(Destino=="S"),
            Predadas= sum(Destino=="P"), RetrievalPred=sum(Destino=="RP"), 
            Dispersas=sum(Destino=="D"));

# tranponding matrix and transforming in percentage
prop.low.2017 <- prop.low.2017 * 10 /6
prop.low.2017 <- data.frame(n=t(prop.low.2017), Year=rep("2017",5))



##
##filterin data to low abundance last date 2018
prop.low.2018 <- low.2018 %>%
  filter(Data=="2018/08/01") %>% 
  summarise(Nada=sum(Destino=="N"), Sumidas=sum(Destino=="S"),
            Predadas= sum(Destino=="P"), RetrievalPred=sum(Destino=="RP"), 
            Dispersas=sum(Destino=="D"));

# tranponding matrix and transforming in percentage
prop.low.2018 <- prop.low.2018 * 10 /6
prop.low.2018 <- data.frame(n=t(prop.low.2018), Year=rep("2018",5))

## putting all together in one data frame
prop.low.bothyears <- rbind(prop.low.2017, prop.low.2018)
prop.low.bothyears <- prop.low.bothyears %>% 
  mutate(Fate=rep(c("Intact", "Lost", "Pre-Preyed", "Pos-Preyed", "Dispersed" ),2))

# Stacked barplot with multiple groups
barplot.low <- ggplot(data=prop.low.bothyears, aes(x=Year, y=n, 
                                    fill=factor(Fate,
    level=c("Dispersed", "Pos-Preyed", "Pre-Preyed", "Lost", "Intact"  ))))+
  geom_bar(stat="identity", 
           #alpha= 5/10
           )+
  scale_fill_manual(name= "Seed Fate", 
   #values=c("chartreuse3", "firebrick4", "firebrick2", "yellow3", "deepskyblue")
   values = adj.colors)+
  #coord_flip()+
  theme_classic()+
  theme(
    axis.text = element_text(size = 35)
    , axis.title = element_blank(),
   axis.text.x=element_blank(), axis.line.y=element_blank(),
    axis.text.y=element_blank(),axis.ticks=element_blank()
    ,legend.position = "none"
  )



###MEDIUM
##
##filterin data of medium abundance last date 2017
prop.medium.2017 <- medium.2017 %>%
  filter(Data=="2017/08/15") %>% 
  summarise(Nada=sum(Destino=="N"), Sumidas=sum(Destino=="S"),
            Predadas= sum(Destino=="P"), RetrievalPred=sum(Destino=="RP"), 
            Dispersas=sum(Destino=="D"));

# tranponding matrix and transforming in percentage
prop.medium.2017 <- prop.medium.2017 * 10 /6
prop.medium.2017 <- data.frame(n=t(prop.medium.2017), Year=rep("2017",5))



##
##filterin data to medium abundance last date 2018
prop.medium.2018 <- medium.2018 %>%
  filter(Data=="2018/08/01") %>% 
  summarise(Nada=sum(Destino=="N"), Sumidas=sum(Destino=="S"),
            Predadas= sum(Destino=="P"), RetrievalPred=sum(Destino=="RP"), 
            Dispersas=sum(Destino=="D"));

# tranponding matrix and transforming in percentage
prop.medium.2018 <- prop.medium.2018 * 10 /7
prop.medium.2018 <- data.frame(n=t(prop.medium.2018), Year=rep("2018",5))

## putting all together in one data frame
prop.medium.bothyears <- rbind(prop.medium.2017, prop.medium.2018)
prop.medium.bothyears <- prop.medium.bothyears %>% 
  mutate(Fate=rep(c("Intact", "Lost", "Pre-Preyed", "Pos-Preyed", "Dispersed" ),2))

# Stacked barplot with multiple groups
barplot.medium <- ggplot(data=prop.medium.bothyears, aes(x=Year, y=n, 
                                    fill=factor(Fate,
                                                level=c("Dispersed", "Pos-Preyed", "Pre-Preyed", "Lost", "Intact"  ))))+
  geom_bar(stat="identity", 
           #alpha= 5/10
           )+
  scale_fill_manual(name= "Seed Fate", 
                    #values=c("chartreuse3", "firebrick4", "firebrick2", "yellow3", "deepskyblue")
                    values = adj.colors)+
  #coord_flip()+
  theme_classic()+
  theme(
    axis.text = element_text(size = 35)
    , axis.title = element_blank(),
   axis.text.x=element_blank(), axis.line.y=element_blank(),
    axis.text.y=element_blank(),axis.ticks=element_blank()
    ,legend.position = "none"
  )


###HIGH
##
##filterin data to high abundance last date 2017
prop.high.2017 <- high.2017 %>%
  filter(Data=="2017/08/15") %>% 
  summarise(Nada=sum(Destino=="N"), Sumidas=sum(Destino=="S"),
            Predadas= sum(Destino=="P"), RetrievalPred=sum(Destino=="RP"), 
            Dispersas=sum(Destino=="D"));

# tranponding matrix and transforming in percentage
prop.high.2017 <- prop.high.2017 * 10 /6
prop.high.2017 <- data.frame(n=t(prop.high.2017), Year=rep("2017",5))



##
##filterin data to high abundance last date 2018
prop.high.2018 <- high.2018 %>%
  filter(Data=="2018/08/01") %>% 
  summarise(Nada=sum(Destino=="N"), Sumidas=sum(Destino=="S"),
            Predadas= sum(Destino=="P"), RetrievalPred=sum(Destino=="RP"), 
            Dispersas=sum(Destino=="D"));

# tranponding matrix and transforming in percentage
prop.high.2018 <- prop.high.2018 * 10 /6
prop.high.2018 <- data.frame(n=t(prop.high.2018), Year=rep("2018",5))

## putting all together in one data frame
prop.high.bothyears <- rbind(prop.high.2017, prop.high.2018)
prop.high.bothyears <- prop.high.bothyears %>% 
  mutate(Fate=rep(c("Intact", "Lost", "Pre-Preyed", "Post-Preyed", "Dispersed" ),2))

# Stacked barplot with multiple groups


barplot.high <- ggplot(data=prop.high.bothyears, aes(x=Year, y=n, 
                                                         fill=factor(Fate,
                                                                     level=c("Dispersed", "Post-Preyed", "Pre-Preyed", "Lost", "Intact"  ))))+
  geom_bar(stat="identity", 
           #alpha= 5/10
           )+
  scale_fill_manual(name= "Seed Fate", 
                    values=adj.colors )+
  #coord_flip()+
  theme_classic()+
  theme(
    axis.text = element_text(size = 35)
    , axis.title = element_blank(),
    axis.text.x=element_blank(), axis.line.y =element_blank(),
    axis.text.y=element_blank(),axis.ticks=element_blank()
    ,legend.position = "none"
  )




## extra. legend
prop.high.bothyears$Fate[c(3,4,8,9)] <- c("Eaten (cache)","Eaten (seed)","Eaten (cache)","Eaten (seed)")
 ggplot(data=prop.high.bothyears, aes(x=Year, y=n, 
                                                   fill=factor(Fate,
                                                               level=c("Dispersed", "Eaten (cache)","Eaten (seed)", "Lost", "Intact"  ))))+
  geom_bar(stat="identity", 
           #alpha= 5/10
           )+
  scale_fill_manual(name= "Seed Fate", 
                    #values=c("chartreuse3", "firebrick4", "firebrick2", "yellow3", "deepskyblue")
                    values = adj.colors,
                     c(  "Dispersed","Eaten (cache)","Eaten (seed)", "Lost", "Intact"))+
  #coord_flip()+
  theme_classic()+
  scale_y_continuous(breaks= seq(from=0, to=100, by=20))+
  guides (fill = guide_legend(override.aes = list(size=22)))+
  theme(
    axis.text = element_text(size = 60),
    #axis.text = element_blank(),
    axis.title = element_blank(), 
    #axis.line.x=element_blank(),
        axis.text.x=element_blank(),axis.ticks=element_blank(),
    #legend.position = "none"
    legend.title = element_text(size=55),
    legend.text = element_text(size=46)
  )


## alltogether------
grid.arrange(barplot.low, barplot.medium, barplot.high, ncol=3)


 