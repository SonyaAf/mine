#install.packages("sjPlot")

library(ISLR)
library(psych)
library(lubridate)
library(dplyr)

library (class)


############## Business Understanding ################################# 
### !!!!!!!! Objectivo: Queremos usar o KNN para predizer o categorias de crime.

#names(Smarket)
DS_Treino <- read.csv("train.txt",stringsAsFactors = FALSE)   # DataSet Treino


# Implemente classificadores K-NN  como vari�veis preditoras 
#Day of week, 
#District, 
#Hour, 
#X and Y.
# Conclua sobre os resultados obtidos usando os dois classificadores.

#
################################################
####Removemos o ano de 2015 porque est� incompleto

#Ano <- as.numeric(year(as.Date(DS_Treino$Dates)))
#DS_Treino$Ano <- Ano
#DS_Treino <- DS_Treino %>%  
#  filter(Ano<2015 & Category %in% c("OTHER OFFENSES","LARCENY/THEFT","DRUG/NARCOTIC","ASSAULT","NON-CRIMINAL"))#Remover 2015, ano incompleto


#################################################
## Preparamos as vari�veis preditoras
Category <- DS_Treino$Category
District <- as.numeric(as.factor(DS_Treino$PdDistrict))
Dayofweek<- as.numeric(as.factor(DS_Treino$DayOfWeek))
Hour     <- as.numeric(as.factor(hour(DS_Treino$Dates)))
X        <- as.numeric(DS_Treino$X)
Y        <- as.numeric(DS_Treino$Y)
#Outra vari�vel a exploral
#period1 <- cut(Hour,breaks=c(1,5,11,18,24),labels=c('madrugada','manh�','tarde','noite'),include.lowest = TRUE)
#periodo  <- as.numeric(period1)



################################## APLICACAO KNN ######################################################

#Split do dataset
percentagem = 0.7
total = nrow(DS_Treino)

splitSize = sample(total, percentagem * total)

#Par�metros para o algoritmo
DataSet_Treino.x=cbind(District,Dayofweek,Hour, X, Y)[splitSize ,] # par�metro 1 do KNN
DataSet_Teste.x=cbind (District,Dayofweek,Hour, X, Y)[-splitSize ,]  # Par�metro 2 do KNN
DataSet_Category = DS_Treino$Category[splitSize]    # Categorias das observa��es (� o terceiro par�metro necess�rio)
category_teste=Category[-splitSize] 


########################################################################
###ELBOW METHOD para terminaro valor de K
########################################################################
set.seed(123)
# Compute and plot wss for k = 2 to k = 15.
k.max <- 15

wss <- sapply(1:k.max, 
              function(k){kmeans(DataSet_Treino.x, k, nstart=50,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

########################################################################
#             Plot das vari�veis
#ATEN��O - Demora muito a fazer o plot do gr�fico
plot(data.frame(x = DataSet_Treino.x), col=as.numeric(as.factor(DataSet_Category)))

                              ################
                              #      KNN     #
                              ################

#Para evitar obter o mesmo ponto de partida:
set.seed (1)

# !!!! Vamos ent�o aplicar a predi��o K-NN, colocando os par�metros de entrada acima, come�ando com apenas a avalia��o de 2 
# vizinho mais pr�ximo (K=2)
predicao_knn_2 =knn (DataSet_Treino.x,DataSet_Teste.x,DataSet_Category ,k = 2)

confusion.matrix <- table(category_teste,predicao_knn_2)
confusion.matrix
accuracy <- (sum(diag(confusion.matrix)) / sum(confusion.matrix))*100
accuracy

#################################################################################
predicao_knn_3 =knn (DataSet_Treino.x,DataSet_Teste.x,DataSet_Category ,k=3)

confusion.matrix <- table(category_teste,predicao_knn_3)
confusion.matrix
accuracy <- sum(diag(confusion.matrix)) / sum(confusion.matrix)*100
accuracy

##################################################################################
# aumente-se para K = 4

predicao_knn_4 =knn (DataSet_Treino.x,DataSet_Teste.x,DataSet_Category ,k=4)

confusion.matrix <- table(category_teste,predicao_knn_4)
confusion.matrix
accuracy <- sum(diag(confusion.matrix)) / sum(confusion.matrix)
accuracy

# aumente-se para K = 5
predicao_knn_5 =knn(DataSet_Treino.x,DataSet_Teste.x,DataSet_Category ,k=5)

confusion.matrix <- table(category_teste,predicao_knn_5)
confusion.matrix
accuracy <- sum(diag(confusion.matrix)) / sum(confusion.matrix)*100
accuracy
