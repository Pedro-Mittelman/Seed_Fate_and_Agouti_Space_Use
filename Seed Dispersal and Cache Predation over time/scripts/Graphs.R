####Loading packages----


library(tidyverse)
library(lme4)    #  GLMM
library(bbmle)   #comparing  AICs
library(ggplot2) #Pgrphs
library(scales) # for colours
library(gridExtra) # plotting graphs together
library(grid) # plotting graphs together


## Graph of dispersed seeds per time and post-dispersal predation per time

##2017 


## reading data from 2017
cuti.RP2.2017 <- read.csv("dados/cutiRP2.2017.csv")
## withdrawing stations with field experimental problems
CutiRP.2017<- cuti.RP2.2017[which(cuti.RP2.2017$Pontos!="AFV02"
                                    & cuti.RP2.2017$Pontos!="AFV14" ),]

##number of post-dispersal preadtion per date
nRP.2017 <- CutiRP.2017 %>%
  filter(Destino=="RP") %>% 
  group_by(Data) %>% 
  count(Data) # conta o número de linhas em cada categoria a coluna é o input
# barplot for exploring data
barplot(height = nRP.2017$n, names.arg = nRP.2017$Data, main="Post-dispersal seed predation",
        ylab="Número de Sementes", xlab="Data")

## number of dispersed seeds per date
ndis.2017 <- CutiRP.2017 %>%
  filter(Destino=="D") %>% 
  group_by(Data) %>% 
  count(Data) # conta o número de linhas em cada categoria a coluna é o input
barplot(height = ndis.2017$n, names.arg = ndis.2017$Data, main="Sementes dispersas",
        ylab="Número de Sementes", xlab="Data")
## adding day zero
ndis.2017 <- data.frame(c("2017/05/04",as.character(ndis.2017$Data)), c(0,ndis.2017$n))
colnames(ndis.2017) <- c("Data", "n")
ndis.2017$Data <-  as.Date(ndis.2017$Data, format = "%Y/%m/%d")

# merging the two dataframes nR e ndis
##adding zero for dates with no cache predation
RP.2017 <- c(0,0,0,0,nRP.2017$n)
#merging
nRP.2017dis <- data.frame(ndis.2017$Data, ndis.2017$n, RP.2017)
colnames(nRP.2017dis) <- c("Data", "dis", "RP")
nRP.2017dis$Data <-  as.Date(nRP.2017dis$Data, format = "%Y/%m/%d")

nRP.2017dis2 <- data.frame(rep(nRP.2017dis$Data, 2), c(nRP.2017dis$dis, nRP.2017dis$RP),
                           c(rep("dis",11),rep("RP",11)))
colnames(nRP.2017dis2) <- c("Data", "n", "fate")

##graph
plot.preddispperdate.2017 <- ggplot(data=nRP.2017dis, mapping = aes(x=Data, y=dis))+
  geom_line(col="darkgreen", size=6)+
  geom_line(aes(x=Data, y=RP), col="firebrick", size=6)+
  geom_point(size=8, alpha=7/10)+
  geom_point(aes(x=Data, y=RP), size=8, alpha=7/10)+
  scale_x_date(name = "Date")+
  scale_y_continuous(name = "Number of Seeds", limits = c(0,42),
                     breaks= seq(from=0, to=50, by=10))+
  theme_classic()+
  theme(
    axis.text = element_text(size = 35),
    #     axis.title = element_text(colour = "black", size = 28, face = "bold"),
    #     axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))
    legend.position = "none",
    axis.title = element_blank(),
    plot.margin = unit(c(0,1.9,0,0), "cm")
    
  )




### 2018

CutiRP.2018 <- read.csv2("dados/cutidRP.csv")

##number of post-dispersal preadtion per date
nRP.2018 <- CutiRP.2018 %>%
  filter(Destino=="RP") %>% 
  group_by(Data) %>% 
  count(Data) # conta o número de linhas em cada categoria a coluna é o input
# barplot for exploring data
barplot(height = nRP.2018$n, names.arg = nRP.2018$Data, main="Post-dispersal seed predation",
        ylab="Número de Sementes", xlab="Data")

## number of dispersed seeds per date
ndis.2018 <- CutiRP.2018 %>%
  filter(Destino=="D") %>% 
  group_by(Data) %>% 
  count(Data) # conta o número de linhas em cada categoria a coluna é o input
barplot(height = ndis.2018$n, names.arg = ndis.2018$Data, main="Sementes dispersas",
        ylab="Número de Sementes", xlab="Data")
## adding day zero
ndis.2018 <- data.frame(c("2018/04/19",as.character(ndis.2018$Data)), c(0,ndis.2018$n))
colnames(ndis.2018) <- c("Data", "n")
ndis.2018$Data <-  as.Date(ndis.2018$Data, format = "%Y/%m/%d")


# merging the two dataframes nR e ndis.2018
##adding zero for the dates with no cache predation
RP.2018 <- c(0,0,nRP.2018$n)
 #merging
nRP.2018dis <- data.frame(ndis.2018$Data, ndis.2018$n, RP.2018)
colnames(nRP.2018dis) <- c("Data", "dis", "RP")
nRP.2018dis$Data <-  as.Date(nRP.2018dis$Data, format = "%Y-%m-%d")

nRP.2018dis2 <- data.frame(rep(nRP.2018dis$Data, 2), c(nRP.2018dis$dis, nRP.2018dis$RP),
                      c(rep("dis",10),rep("RP",10)))
colnames(nRP.2018dis2) <- c("Data", "n", "fate")

##graph
plot.preddispperdate.2018 <- ggplot(data=nRP.2018dis, mapping = aes(x=Data, y=dis))+
geom_line(col="darkgreen", size=6)+
  geom_line(aes(x=Data, y=RP), col="firebrick", size=6)+
  geom_point(size=8, alpha=7/10)+
  geom_point(aes(x=Data, y=RP), size=8, alpha=7/10)+
  scale_x_date(name = "Date")+
  scale_y_continuous(name = "Number of Seeds", limits = c(0,42),
                     breaks= seq(from=0, to=50, by=10))+
     theme_classic()+
  theme(
    axis.text = element_text(size = 35),
    #     axis.title = element_text(colour = "black", size = 28, face = "bold"),
    #     axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))
    legend.position = "none",
    axis.title = element_blank()
    
  )


### together

grid.arrange(plot.preddispperdate.2017, plot.preddispperdate.2018, ncol=2)



