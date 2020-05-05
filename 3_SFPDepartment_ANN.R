#install.packages("som")
#install.packages("nnet")
#install.packages("neuralnet")
#install.packages("dplyr")
library('som')
library(ggplot2)
library(ggmap)
library(lubridate)
library(stringr)
library(psych)
library(reshape2)
library(plotly)
library(sp)
library(dplyr)
library(nnet)
library("neuralnet")

#Loading datasets
DS_Treino <- read.csv("train.txt",stringsAsFactors = FALSE)   # DataSet Treino
DS_Teste <- read.csv("teste.csv",stringsAsFactors = FALSE)   # DataSet teste

#year
Ano <- year(as.Date(DS_Treino$Dates))

#month
mes <- month(as.Date(DS_Treino$Dates), label=TRUE)

#day of week
dia_semana=weekdays(as.Date(DS_Treino$Dates))

#Day
dia = as.Date(DS_Treino$Dates," %Y-%m-%d ")

#Hour
hora = str_pad(hour(DS_Treino$Dates), 2, pad = "0") 


#=============================ARTIFICIAL NEURAL NETWORK===============


treinoANN <- mutate(DS_Treino, Ano, mes, dia_semana, dia, hora,X,Y)
matrixFrametreino<- subset(treinoANN, select = c(Category,hora,dia_semana,PdDistrict,X,Y))
matrixtreinocategory<-factor(matrixFrametreino[,1])
matrixFrametreino[,1]<-as.numeric(matrixtreinocategory)
matrixFrametreino[,2]<-as.numeric(hora)
matrixFrametreino[,3] <-(sapply(matrixFrametreino[,3],switch,"domingo"=1,"segunda-feira"=2, "terça-feira"=3,"quarta-feira"=4,"quinta-feira"=5,"sexta-feira"=6,"sábado"=7))
matrixFrametreinodiasemana<-factor(matrixFrametreino[,3])
matrixFrametreino[,3]<-as.numeric(matrixFrametreinodiasemana)
matrixFrametreinodistrito<-factor(matrixFrametreino[,4])
matrixFrametreino[,4]<-as.numeric(matrixFrametreinodistrito)
matrixFrametreino[,5]<-as.numeric(matrixFrametreino$X)
matrixFrametreino[,6]<-as.numeric(matrixFrametreino$Y)
View (matrixFrametreino)


#ANN
# activation function
nn = neuralnet(Category~dia_semana+hora+PdDistrict+X+Y, data = matrixFrametreino, hidden = 2, err.fct = "sse", 
               linear.output = FALSE)   

#model
nn
nn$result.matrix

#a ANN graphic
plot(nn)

# weights
nn$weights


#vars for train
nn$covariate

nn1=ifelse(nn$net.result[[1]]>0.5,1,0)
nn1


# classification errosr
ClassificationError = mean(matrixFrametreino$Category !=nn1)  
ClassificationError

# percentage of good classification
PercentGoodClassification = (1 - ClassificationError)*100
PercentGoodClassification


#Correct predictions
OutputVsPred = cbind(matrixFrametreino$Category, nn1)
OutputVsPred

#Back Propagation 
nn.BackProp = neuralnet(Category~dia_semana+hora+PdDistrict+X+Y, data = matrixFrametreino,hidden = 2, learningrate = 0.01,
                        algorithm = "backprop", err.fct="sse",  
                        linear.output = FALSE)

nn.BackProp
plot(nn.BackProp)



# Confusion Matrix & Misclassification Error - testing data
#Acrescentar Novas Váriavies 
Ano2 <- year(as.Date(DS_Teste$Dates))

#Month
mes2 <- month(as.Date(DS_Teste$Dates), label=TRUE)

#day of week
dia_semana2=weekdays(as.Date(DS_Teste$Dates))

#Day
dia2 = as.Date(DS_Teste$Dates," %Y-%m-%d ")

#Hour 
hora2 = str_pad(hour(DS_Teste$Dates), 2, pad = "0")

#distrito
distrito2 = as.factor(DS_Teste$PdDistrict)

#X
x2 = as.numeric(DS_Teste$X)

#Y
y2 = as.numeric(DS_Teste$Y)

#data test variables
teste <- mutate(DS_Teste, Ano2, mes2, dia_semana2, dia2, hora2, distrito2,x2,y2)
#matrixFrame2<- subset(teste, select = c(hora,dia_semana,distrito))
matrixteste<- subset(teste, select = c(dia_semana2, dia2, hora2, distrito2,x2,y2))
matrixteste[,1] <-(sapply(matrixteste[,1],switch,"domingo"=1,"segunda-feira"=2, "terça-feira"=3,"quarta-feira"=4,"quinta-feira"=5,"sexta-feira"=6,"sábado"=7))
matrixtestediasemana<-factor(matrixteste[,1])
matrixteste[,1]<-as.numeric(matrixtestediasemana)
matrixteste[,2]<-as.numeric(dia2)
matrixteste[,3]<-as.numeric(hora2)
matrixteste[,4]<-as.numeric(distrito2)
matrixteste[,5]<-as.numeric(teste$x2)
matrixteste[,6]<-as.numeric(teste$Y)
View (matrixteste)

#================PREDICTION==============================
nn.output = compute(nn, covariate=matrix(c(matrixteste$dia_semana2,matrixteste$hora2,matrixteste$distrito2,matrixteste$y2,matrixteste$x2),byrow = TRUE,ncol=5))

#check correct prediction
#check<-subset(teste, select = c(PdDistrict,distrito2))
#View(check)
#train
nn.output1 = compute(nn, covariate=matrix(c(matrixFrametreino$dia_semana,matrixFrametreino$hora,matrixFrametreino$PdDistrict,matrixFrametreino$Y,matrixFrametreino$X),byrow = TRUE,ncol=5))

#probability of prediction / probability of the event
nn.output$net.result
nn.output1$net.result

par(mfrow=c(2,2))
#min = -2.5, max = 5
#variability?
gwplot(nn, selected.covariate = "dia_semana")
gwplot(nn, selected.covariate = "hora")
gwplot(nn, selected.covariate = "PdDistrict")
gwplot(nn, selected.covariate = "X")
gwplot(nn, selected.covariate = "Y")
#Error in plot.new() : figure margins too large


# hidden layers 4
nn.BackProp2 = neuralnet(Category~dia_semana+hora+PdDistrict+X+Y, data = matrixFrametreino, hidden = c(4,3,2,1) , learningrate = 0.01,act.fct="tanh",
                         algorithm = "backprop", err.fct="sse", linear.output = FALSE)

nn.BackProp2
nn.BackProp2$net.result

plot(nn.BackProp2)
nn

nn2=ifelse(nn$net.result[[1]]>0.5,1,0)
nn2
# classification error:
ClassificationError2 = mean(matrixFrametreino$Category !=nn2)  
ClassificationError2

#Percentage of good classification
PercentGoodClassification2 = (1 - ClassificationError2)*100
PercentGoodClassification2



OutputVsPred2 = cbind(matrixFrametreino$Category, nn2)
OutputVsPred2

#==========================================================================
# hidden layers 6
nn.BackProp3 = neuralnet(Category~dia_semana+hora+PdDistrict+X+Y, data = matrixFrametreino, hidden = c(6,5,4,3,2,1) , learningrate = 0.01,act.fct="tanh",
                         algorithm = "backprop", err.fct="sse", linear.output = FALSE)

nn.BackProp3
nn.BackProp3$net.result

plot(nn.BackProp3)


nn3=ifelse(nn$net.result[[1]]>0.5,1,0)
nn3

### classification error:
ClassificationError3 = mean(matrixFrametreino$Category !=nn3)  
ClassificationError3

#Percentage of good classification
PercentGoodClassification3 = (1 - ClassificationError3)*100
PercentGoodClassification3



OutputVsPred3 = cbind(matrixFrametreino$Category, nn3)
OutputVsPred3




