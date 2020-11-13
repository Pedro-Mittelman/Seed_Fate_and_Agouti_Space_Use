###packages
library(rcompanion)
library(DescTools)



###2017



Input =("
Frequency  Disp  Eaten   Intact
Low         4     13      26
Med         13    9       24
High        0     37      2

")






Matriz = as.matrix(read.table(textConnection(Input),
                              header=TRUE,
                              row.names=1))

TMatriz<- t(Matriz)
C <- chisq.test(Matriz);C
G <- GTest(Matriz, correct = "williams");G

I <- fisher.test(Matriz,
            alternative="two.sided"); I



PT = pairwiseNominalIndependence(Matriz,
                                 
                                 fisher = TRUE,
                                 
                                 gtest  = T,
                                 
                                 chisq  = T,
                                 digits = 3)
PT

TPT = pairwiseNominalIndependence(TMatriz,
                                 fisher = TRUE,
                                gtest  = FALSE,
                                 chisq  = FALSE,
                                 digits = 3)

cldList(comparison = PT$Comparison,
        p.value    = PT$p.adj.Fisher,
        threshold  = 0.05)
cldList(comparison = TPT$Comparison,
        p.value    = TPT$p.adj.Fisher,
        threshold  = 0.05)

## 2018

Input =("
Frequency  Disp  Eaten   Intact
Low         5     6       37
Med         15    11      10
High        2     45      2

")

Matriz = as.matrix(read.table(textConnection(Input),
                              header=TRUE,
                              row.names=1))
C <- chisq.test(Matriz);C$p.value
G <- GTest(Matriz, correct = "williams");G$p.value
F <- fisher.test(Matriz,
                 alternative="two.sided");F$p.value

library("rcompanion")

PT = pairwiseNominalIndependence(Matriz,
                                 
                                 fisher = TRUE,
                                 
                                 gtest  = FALSE,
                                 
                                 chisq  = FALSE,
                                 digits = 3)
PT

cldList(comparison = PT$Comparison,
        p.value    = PT$p.adj.Fisher,
        threshold  = 0.05)

G$observed
G$expected

C$residuals
(C$observed-C$expected)/(sqrt(C$expected))
(G$observed-G$expected)/(sqrt(G$expected))

FisherZInv(0.05)

