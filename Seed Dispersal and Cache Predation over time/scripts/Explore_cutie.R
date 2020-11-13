



##-xxxxxxxxxxx##---2018---##xxxxxxxxxxxxxxx-##


# Limpando o ambiente de trabalho -----------------------------------------

rm(list = ls())
# Pacotes Necessários -----------------------------------------------------

instaloadp <- function(pack){
  new.packages <- pack[!(pack %in% installed.packages()[,"Package"])]
  if(length(new.packages)>0) {install.packages(new.packages)}
  vec0 <- vector()
  for(i in 1:length(pack)) { 
    vec0[i] <- library(pack[i], character.only = TRUE, logical.return = TRUE)
  }
  print(vec0)
}

instaloadp(c("knitr","tidyverse","readr"))



#Criando default de par caso eu faça alguma meleca e precise retornar
#pra configuração padrão
defpar <- par()
# Lendo dados -------------------------------------------------------------


cuti <- read_delim("dados//Resultados cutieira 01.08.2018 ppi.csv", delim =";")

# Passando as colunas pro formato de data
cuti <- rename(cuti,EData=Data)
cuti <- mutate(cuti, Data=as.Date(cuti$EData, format = "%d.%m.%Y"))
#Excluindo a coluna de data em forma de caracter
cuti <- select(cuti, -EData)


# Pontos com alguma  semente ----------------------------------------------
uscut <- filter(cuti, Data=="2018-08-01" )
uscut_ver <- filter(uscut, Destino=="D" )
(pontosver <-unique(uscut_ver$Nome))



# #Vendo quantas sementes eu tenho enterradas hoje 01.08 ------------------


#Filtrando a última semana
uscut <- filter(cuti, Data=="2018-08-01" )
#Filtrando as sementes enterradas
uscut_ent <- filter(uscut, Enterrada=="S" & Destino=="D" )
nrow(uscut_ent)




# Distância de remoção por semana.1 -----------------------------------------


# explorando os dados
plot(cuti$Distancia~jitter(as.numeric(cuti$Data)),
     xlab="Tempo Desde o Início do Experimento", ylab="Distância",
     xaxp=c(0,10000,1), mgp=c(1.5,0.2,0), tcl=0.15)
axis(1, 
     labels = c("24h","72h","1 semana","2 semanas", "3 semanas", "4 semanas", "6 semanas",
                "8 semanas", "10 semanas", "14 semanas")
     ,at=c(17291, 17293, 17297, 17304, 17311, 17318, 17332, 17346, 17360, 17393),
     mgp=c(1.5,0.15,0), tcl=0.15 )
#gráfico de barras
mediarem0 <- cuti %>%
  group_by(Data) %>% 
  summarise(mediarem0=mean(Distancia)); mediarem0
barplot(height = mediarem0$mediarem0, names.arg = mediarem0$Data, main="Distância 
de remoção média", ylab="Distância (m)", xlab="Data")
#Desvios padrões
sdrem0 <- cuti %>%
  group_by(Data) %>% 
  summarise(sd=sd(Distancia)); sdrem0

# Distância de remoção por semana.2 (sem os 0s) ---------------------------

##posição dos eixos
zz <- c(17640.85, 17644.70, 17652.87, 17659.90, 17667.16,
        17680.99, 17696.19, 17713.89, 17744.28)
# explorando os dados
cuti2 <- filter(cuti, Distancia>0)
plot(cuti2$Distancia~jitter(as.numeric(cuti2$Data)),
     xlab="Tempo Desde o Início do Experimento", ylab="Distância",
     xaxp=c(0,10000,1), mgp=c(1.5,0.2,0), tcl=0.15)
axis(1, 
     labels = c("72h","1 semana","2 semanas", "3 semanas", "4 semanas", "6 semanas",
                "8 semanas", "10 semanas", "14 semanas")
     , at=(zz),
     mgp=c(1.5,0.15,0), tcl=0.15 )

#gráfico de barras
mediarem <- cuti %>%
  filter(Distancia>0) %>% 
  group_by(Data) %>% 
  summarise(mediarem=mean(Distancia)); mediarem
barplot(height = mediarem$mediarem, names.arg = mediarem$Data, main="Distância 
de remoção média", ylab="Distância (m)", xlab="Data")

##boxplot
boxplot(cuti2$Distancia~cuti2$Data, xlab="Data", ylab="Distãncia")

#desvio padrões
sdrem <- cuti %>%
  filter(Distancia>0) %>% 
  group_by(Data) %>% 
  summarise(sd=sd(Distancia)); sdrem






# Distãncia das sementes dispersas ----------------------------------------

#explorando os dados
cuti3 <- filter(cuti, Destino=="D")
plot(cuti3$Distancia~jitter(as.numeric(cuti3$Data)),
     xlab="Tempo Desde o Início do Experimento", ylab="Distância",
     xaxp=c(0,10000,1), mgp=c(1.5,0.2,0), tcl=0.15)
axis(1, 
     labels = c("72h","1 semana","2 semanas", "3 semanas", "4 semanas", "6 semanas",
                "8 semanas", "10 semanas", "14 semanas")
     , at=(zz),
     mgp=c(1.5,0.15,0), tcl=0.15 )
##boxplot.
boxplot(cuti3$Distancia~cuti3$Data, xlab="Data", ylab="Distância")

##gráfico de barras
mediadis <- cuti %>%
  filter(Destino=="D") %>% 
  group_by(Data) %>% 
  summarise(mediadis=mean(Distancia)); mediadis
barplot(height = mediadis$mediadis, names.arg = mediadis$Data, main="Distância 
de dispersão média", ylab="Distância (m)", xlab="Data")

## desviso padrões

sddis <- cuti %>%
  filter(Destino=="D") %>% 
  group_by(Data) %>% 
  summarise(sd=sd(Distancia)); sddis



# Número de Sementes predadas por data --------------------------------------------
pred0 <- cuti %>%
  filter(Destino=="P") %>% 
  group_by(Data) %>% 
count(Data) # conta o número de linhas em cada categoria a coluna é o input
barplot(height = pred0$n, names.arg = pred0$Data, main="Sementes predadas",
        ylab="Número de Sementes", xlab="Data")



# Número de Sementes Dispersas por Data -----------------------------------
ndis <- cuti %>%
  filter(Destino=="D") %>% 
  group_by(Data) %>% 
  count(Data) # conta o número de linhas em cada categoria a coluna é o input
barplot(height = ndis$n, names.arg = ndis$Data, main="Sementes dispersas",
        ylab="Número de Sementes", xlab="Data")
## adding day zero
ndis <- data.frame(c("2018-04-19",as.character(ndis$Data)), c(0,ndis$n))
colnames(ndis) <- c("Data", "n")
ndis$Data <-  as.Date(ndis$Data, format = "%Y-%m-%d")

## post dispersal predation
##loading additional data
cutiRP <- read.csv2("dados/cutidRP.csv")

##number of post-dispersal preadtion per date
nRP1 <- cutiRP %>%
  filter(Destino=="RP") %>% 
  group_by(Data) %>% 
  count(Data)
nRP2 <- mutate(ndis, RP=c(0,0,nRP1$n))[,c(1,3)]
# Número de Sementes Dispersas por Data -----------------------------------
par(bty="l")
plot(ndis, cex.axis=2.6, pch=16, cex.lab=1.6, cex=3, col="black",
     ylab="", xlab="", tck=-0.0375, mgp=c(5,2.2,0), ylim=c(0,41))
axis(1, cex.axis=2.6,
     labels = c("1", "2","3","4","6","8","10","12","14")
     , at=c(17646.79, 17654.13, 17661.2, 17668.59, 17682.17, 17696.18, 17709.09, 17724.22, 17741.35),
     mgp=c(1.5,1,0), tcl=-0.15 )
lines(ndis, col="darkgreen", lwd=15.1)
points(nRP2, col="black", pch=16, cex=3)
lines(nRP2, col="firebrick", lwd=15.1)




# Proporção de sementes Intactas/Perdidas/Predadas/Dispersas -------------------

prop <- cuti %>%
  group_by(Data) %>% 
  summarise(Nada=sum(Destino=="N"), Sumidas=sum(Destino=="S"),
            Predadas= sum(Destino=="P"),Dispersas=sum(Destino=="D"));
prop
### Transformando para matriz onde o barplot lê              #
## a soma de cada linha como uma barra, e cada coluna       ##  
# como a proporção sobre o total                           ###
prop2 <- as.matrix(prop[,2:5])
# função t tranpõe a matriz
prop3 <- t(prop2)
#botando nome nas colunas
colnames(prop3)=as.character(prop$Data)
## ou colnames(prop3)=c("24h","72h","1 semana","2 semanas", "3 semanas", "4 semanas", "6 semanas")
#botando em porcentagem
prop4 <- prop3/2
a <- c("72h","1 semana","2 semanas", "3 semanas", "4 semanas", "6 semanas",
       "8 semanas", "10 semanas", "14 semanas")
colnames(prop4) <- a
##plotando o gráfico
par(mar=c(5,4,4,6)) # ajeitando as margens pra caber a legenda
#gráfico
barplot(prop4, col=c("antiquewhite", "lightblue1", "lightblue3", "lightblue4")
        , ylab="Proporção (%)", xlab="Tempo desde o Ínicio do Experimento", #main="Destino das sementes",
        mgp=c(1.5,0,0), cex.lab=1.5, cex.axis = 1.1, yaxp=c(0,100,5), tcl=0.15)
par(xpd=TRUE) #permite que eu ponha a legenda fora da área de plotagem
#legenda
legend(x=11.5,y=95,legend=c("Dispersas","Predadas","Perdidas","Intactas"),
      pch=15,col=c( "lightblue4","lightblue3","lightblue1","antiquewhite2")
      , bty = "n", pt.cex=3, y.intersp = 1.8, cex=1.5, adj=0.4)

#retornando as margens pro padaão normal pros próximos gráficos
par(mar=c(5,4,4,2))


# Utilidades --------------------------------------------------------------

#### util para setar as coordenads x, y da legenda
##locator()
## retornando parametros gráficos para o padrão
##par(defpar)

# gerar arquivo só das sementes dispersas ---------------------------------
#Filtrando a última semana
uscut <- filter(cuti, Data=="2017-08-15" )
#Filtrando as sementes dispersas
uscut_ds <- filter(uscut, Destino=="D" )
#criando a planilha
write.csv2(uscut_ds, "Sementes dispersas.csv")
