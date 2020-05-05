###################################LOGISTIC REGRESSION#################################################

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


#upload data train and data test
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

#hour
hora = str_pad(hour(DS_Treino$Dates), 2, pad = "0") 

#=====================PART 3==============================
#Having in mind the given the Dataset in question, # most appropriate for classification, regression and prediction.

#split do dataset 70|30
percentagem = 0.7
total = nrow(DS_Treino)

splitSize <- sample(total, percentagem * total)

treino1 <- DS_Treino[ splitSize,]
treino2  <- DS_Treino[-splitSize,]


#Transform train variables 
treino1 <- mutate(DS_Treino, Ano, mes, dia_semana, dia, hora) 
matrixFrame2<- subset(treino1, select = c(Category,hora,dia_semana,PdDistrict,X,Y))
matrixFramecategory2<-factor(matrixFrame2[,1])
matrixFrame2[,1]<-as.numeric(matrixFramecategory2)
matrixFrame2[,2]<-as.numeric(hora)
matrixFrame2[,3] <-(sapply(matrixFrame2[,3],switch,"domingo"=1,"segunda-feira"=2, "terça-feira"=3,"quarta-feira"=4,"quinta-feira"=5,"sexta-feira"=6,"sábado"=7))
matrixFramediasemana2<-factor(matrixFrame2[,3])
matrixFrame2[,3]<-as.numeric(matrixFramediasemana2)
matrixFramedistrito2<-factor(matrixFrame2[,4])
matrixFrame2[,4]<-as.numeric(matrixFramedistrito2)
matrixFrame2[,5]<-as.numeric(matrixFrame2$X)
matrixFrame2[,6]<-as.numeric(matrixFrame2$Y)
View(matrixFrame2)



#Transform test variables
treino2 <- mutate(DS_Treino, Ano, mes, dia_semana, dia, hora) 
matrixFrame3<- subset(treino2, select = c(Category,hora,dia_semana,PdDistrict,X,Y))
matrixFramecategory3<-factor(matrixFrame3[,1])
matrixFrame3[,1]<-as.numeric(matrixFramecategory3)
matrixFrame3[,2]<-as.numeric(hora)
matrixFrame3[,3] <-(sapply(matrixFrame3[,3],switch,"domingo"=1,"segunda-feira"=2, "terça-feira"=3,"quarta-feira"=4,"quinta-feira"=5,"sexta-feira"=6,"sábado"=7))
matrixFramediasemana3<-factor(matrixFrame3[,3])
matrixFrame3[,3]<-as.numeric(matrixFramediasemana3)
matrixFramedistrito3<-factor(matrixFrame3[,4])
matrixFrame3[,4]<-as.numeric(matrixFramedistrito3)
matrixFrame3[,5]<-as.numeric(matrixFrame3$X)
matrixFrame3[,6]<-as.numeric(matrixFrame3$Y)
View(matrixFrame3)


#Multinomial Logistic Regression split 70|30
Regressao_logisticamultinomialtreino11 <- multinom(matrixFramecategory2 ~ hora + matrixFramediasemana2 + matrixFramedistrito2 ,
                                             data= matrixFrame2)
summary(Regressao_logisticamultinomialtreino11)


#Prediction 70/30
matrixFrame2[,2]<-as.factor(hora)
predicaoclasses7030 <- Regressao_logisticamultinomialtreino11 %>% predict(matrixFrame3)
head(predicaoclasses7030)
mean(predicaoclasses7030 == matrixFramecategory3)




#split 50|50
percentagem = 0.5
total = nrow(DS_Treino)

splitSize <- sample(total, percentagem * total)

treino1 <- DS_Treino[ splitSize,]
treino2  <- DS_Treino[-splitSize,]


##Transform train variables 50/50
treino1 <- mutate(DS_Treino, Ano, mes, dia_semana, dia, hora) 
matrixFrame2<- subset(treino1, select = c(Category,hora,dia_semana,PdDistrict,X,Y))
matrixFramecategory2<-factor(matrixFrame2[,1])
matrixFrame2[,1]<-as.numeric(matrixFramecategory2)
matrixFrame2[,2]<-as.numeric(hora)
matrixFrame2[,3] <-(sapply(matrixFrame2[,3],switch,"domingo"=1,"segunda-feira"=2, "terça-feira"=3,"quarta-feira"=4,"quinta-feira"=5,"sexta-feira"=6,"sábado"=7))
matrixFramediasemana2<-factor(matrixFrame2[,3])
matrixFrame2[,3]<-as.numeric(matrixFramediasemana2)
matrixFramedistrito2<-factor(matrixFrame2[,4])
matrixFrame2[,4]<-as.numeric(matrixFramedistrito2)
matrixFrame2[,5]<-as.numeric(matrixFrame2$X)
matrixFrame2[,6]<-as.numeric(matrixFrame2$Y)


##Transform test variables e 50/50
treino2 <- mutate(DS_Treino, Ano, mes, dia_semana, dia, hora) 
matrixFrame3<- subset(treino2, select = c(Category,hora,dia_semana,PdDistrict,X,Y))
matrixFramecategory3<-factor(matrixFrame3[,1])
matrixFrame3[,1]<-as.numeric(matrixFramecategory3)
matrixFrame3[,2]<-as.numeric(hora)
matrixFrame3[,3] <-(sapply(matrixFrame3[,3],switch,"domingo"=1,"segunda-feira"=2, "terça-feira"=3,"quarta-feira"=4,"quinta-feira"=5,"sexta-feira"=6,"sábado"=7))
matrixFramediasemana3<-factor(matrixFrame3[,3])
matrixFrame3[,3]<-as.numeric(matrixFramediasemana3)
matrixFramedistrito3<-factor(matrixFrame3[,4])
matrixFrame3[,4]<-as.numeric(matrixFramedistrito3)
matrixFrame3[,5]<-as.numeric(matrixFrame3$X)
matrixFrame3[,6]<-as.numeric(matrixFrame3$Y)




#Multinomial Logistic Regression split 50/50
Regressao_logisticamultinomialtreino12 <- multinom(matrixFramecategory2 ~ hora + matrixFramediasemana2 + matrixFramedistrito2 ,
                                                   data= matrixFrame2)
summary(Regressao_logisticamultinomialtreino12)


#prediction 50/50
matrixFrame2[,2]<-as.factor(hora)
predicaoclasses5050 <- Regressao_logisticamultinomialtreino12 %>% predict(matrixFrame3)
head(predicaoclasses5050)
mean(predicaoclasses5050 == matrixFramecategory3)




# binomial logistic regression - classification of the crime category given the time of the incident and the location


#Transforming train data
treino1 <- mutate(DS_Treino, Ano, mes, dia_semana, dia, hora) 
matrixFrame2<- subset(treino1, select = c(Category,hora,PdDistrict))
matrixFramecategory2<-factor(matrixFrame2[,1])
matrixFrame2[,1]<-as.numeric(matrixFramecategory2)
matrixFrame2[,2]<-as.factor(hora)
matrixFramedistrito2<-factor(matrixFrame2[,3])
matrixFrame2[,3]<-as.numeric(matrixFramedistrito2)


#Transforming test data
treino2 <- mutate(DS_Treino, Ano, mes, dia_semana, dia, hora) 
matrixFrame3<- subset(treino2, select = c(Category,hora,PdDistrict))
matrixFramecategory3<-factor(matrixFrame3[,1])
matrixFrame3[,1]<-as.numeric(matrixFramecategory3)
matrixFrame3[,2]<-as.factor(hora)
matrixFramedistrito3<-factor(matrixFrame3[,3])
matrixFrame3[,3]<-as.numeric(matrixFramedistrito3)



#Binomial Logistic Regression
Regressao_logisticabinomialtreino11 <- glm(matrixFramecategory2 ~ hora + matrixFramedistrito2 ,
                                              family = binomial(link="logit"))
summary(Regressao_logisticabinomialtreino11)


#Binomial prediction
matrixFrame2[,2]<-as.factor(hora)
predicao <- Regressao_logisticabinomialtreino11 %>% predict(matrixFrame3)
head(predicao)
mean(predicao == matrixFramecategory3)

