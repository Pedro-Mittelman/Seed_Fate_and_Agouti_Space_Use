## conficence interval
datasets<- list()

for (i in 1:1000) {
  datasets[[i]]<- sample_n(Nfinaldispto, 276, replace=TRUE)  
}

models <- list()
for (i in 1:1000) {
models[[i]]  <-glmer(Dispersas ~ scale(Sessenta) + scale(I(Sessenta^2)) + (1|Pontos)+(1|Year), data=datasets[[i]], family=binomial)

}
predictions <- list()
for (i in 1:1000) {
  predictions[[i]]<-predict(models[[i]], newdata = finaldisp.to, type="response", re.form=NA)
}

data.pred <- data.frame(predictions[[1]])
for (i in 1: 1000) {
data.pred[,i] <- predictions[[i]] 
  
}
data.pred <- t(data.pred)

for (i in 1:ncol(data.pred)) {
  data.pred[,i] <- sort(data.pred[,i])
}
data.pred <- data.pred[-(1:25),]
data.pred <- data.pred[-(951:975),]

final.pred <- data.frame(Min=data.pred[1,], Max=data.pred[950,])
finaldisp.to<-expand.grid(Sessenta = seq(0, 65.5, 0.1))
df.predict.min.max.disp.to <-final.pred %>% mutate(Sessenta = seq(0, 65.5, 0.1))





pred.finaldisp.to<-predict(m.finaldispto, newdata = finaldisp.to, type="response", re.form=NA)
df.predict.finaldisp.to <- data.frame(pred = pred.finaldisp.to, sur.to)
