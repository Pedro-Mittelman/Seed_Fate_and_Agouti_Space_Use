
# year as random effect
m.remto<-glmer(Removidas ~ (Sessenta) + (1|Stationyear), data=Nremto, family=binomial)

#year as fixed effect
m.remto2<-glmer(Removidas ~ (Sessenta) + Year + (1|Stationyear), data=Nremto, family=binomial)

summary(m.remto2)

AICctab(m.remto,m.remto2)

chi.table <-read.csv2("dados/chisquare.csv") 

chisq.test(chi.table)
