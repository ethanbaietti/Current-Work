rm(list=ls())
library(neuralnet)

TKS = c(20, 10, 30,20,80,30)
CSS = c(90, 20, 40,50, 50, 80)
Placed = c(1,0,0,0,1,1)

df= data.frame(TKS,CSS,Placed)

nn=neuralnet(Placed ~ TKS + CSS, data = df, hidden = 3, act.fct = "logistic", linear.output = FALSE)
plot(nn)

TKS = c(30,40,85)
CSS = c(85,50,40)
test = data.frame(TKS,CSS)

Predict = compute(nn,test)
Predict$net.result
