## Graph of dispersed seeds per time and post-dispersal predation per time

##loading additional data
cutiRP <- read.csv2("dados/cutidRP.csv")

##number of post-dispersal preadtion per date
nRP <- cutiRP %>%
  filter(Destino=="RP") %>% 
  group_by(Data) %>% 
  count(Data) # conta o número de linhas em cada categoria a coluna é o input
# barplot for exploring data
barplot(height = nRP$n, names.arg = nRP$Data, main="Post-dispersal seed predation",
        ylab="Número de Sementes", xlab="Data")

# merging the two dataframes nR e ndis
##adding zero for the dirst date for RP
RP <- c(0,0,nRP$n)
 #merging
nRPdis <- data.frame(c("2018-04-19",as.character(ndis$Data)), c(0,ndis$n), RP)
colnames(nRPdis) <- c("Data", "dis", "RP")
nRPdis$Data <-  as.Date(nRPdis$Data, format = "%Y-%m-%d")

nRPdis2 <- data.frame(rep(nRPdis$Data, 2), c(nRPdis$dis, nRPdis$RP),
                      c(rep("dis",10),rep("RP",10)))
                      
colnames(nRPdis2) <- c("Data", "n", "fate")

##graph
ggplot(data=nRPdis, mapping = aes(x=Data, y=dis))+
geom_line(col="darkgreen", size=2)+
  geom_line(aes(x=Data, y=RP), col="firebrick", size=2)+
  geom_point(size=3, alpha=7/10)+
  geom_point(aes(x=Data, y=RP), size=3, alpha=7/10)+
  scale_x_date(name = "Date")+
  scale_y_continuous(name = "Number of Seeds", limits = c(0,42),
                     breaks= seq(from=0, to=50, by=10))+
  theme_classic()+
  theme(axis.text = element_text(size = 12),
        axis.title = element_blank())
        #axis.title = element_text(colour = "black", size = 13, face = "bold"))
        
##graph
ggplot(data=nRPdis, mapping = aes(x=Data, y=dis))+
  geom_line(col="darkgreen", size=2)+
  geom_line(aes(x=Data, y=RP), col="firebrick", size=2)+
  geom_point(size=3, alpha=7/10)+
  geom_point(aes(x=Data, y=RP), size=3, alpha=7/10)+
  scale_x_date(name = "Date",
               date_minor_breaks = "7 days") +
  scale_y_continuous(name = "Number of Seeds", limits = c(0,42),
                     breaks= seq(from=0, to=50, by=10))+
  theme_classic()+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(colour = "black", size = 13, face = "bold")
        axis.ticks.x = elem


# ggplot(data=nRPdis2, mapping = aes(x=Data, y=n, col=fate))+
#   scale_color_manual(values=c("darkgreen", "red3"),name="Seed Fate"
#                      ,labels=c("Dispersed", "Predated post-dispersal"))+
#   geom_line(size=2)+
#   geom_point(size=3, col="black", alpha=6/10)+
#   scale_x_date(name = "Date")+
#   scale_y_continuous(name = "Number of Seeds", limits = c(0,40),
#                      breaks= seq(from=0, to=50, by=10))+
#   #scale_colour_discrete( name="Seed Fate"
#                         #,labels=c("Dispersed", "Predated post-dispersal"))+
#   theme_classic()+
#   theme(axis.text = element_text(size = 12),
#         axis.title = element_text(colour = "black", size = 13, face = "bold"))
       
      

