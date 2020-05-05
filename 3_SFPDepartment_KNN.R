############ K NEAREST NEIGHBOR ######################################

#install.packages("sjPlot")

library(ISLR)
library(psych)
library(lubridate)
library(dplyr)

library (class)


############## Business Understanding ################################# 


#names(Smarket)
DS_Treino <- read.csv("train.txt",stringsAsFactors = FALSE)   # DataSet Treino


# USED VARIABLES K-NN  AS PREDICTIVE 
#Day of week, 
#District, 
#Hour, 
#X and Y.


#
################################################
####Removing year 2015 once it is not complete

#Ano <- as.numeric(year(as.Date(DS_Treino$Dates)))
#DS_Treino$Ano <- Ano
#DS_Treino <- DS_Treino %>%  
#  filter(Ano<2015 & Category %in% c("OTHER OFFENSES","LARCENY/THEFT","DRUG/NARCOTIC","ASSAULT","NON-CRIMINAL"))#Remover 2015, ano incompleto


#################################################
## predictor variables
Category <- DS_Treino$Category
District <- as.numeric(as.factor(DS_Treino$PdDistrict))
Dayofweek<- as.numeric(as.factor(DS_Treino$DayOfWeek))
Hour     <- as.numeric(as.factor(hour(DS_Treino$Dates)))
X        <- as.numeric(DS_Treino$X)
Y        <- as.numeric(DS_Treino$Y)

#period1 <- cut(Hour,breaks=c(1,5,11,18,24),labels=c('madrugada','manhã','tarde','noite'),include.lowest = TRUE)
#periodo  <- as.numeric(period1)



################################## APLPLY KNN ######################################################

#Split do dataset
percentagem = 0.7
total = nrow(DS_Treino)

splitSize = sample(total, percentagem * total)


DataSet_Treino.x=cbind(District,Dayofweek,Hour, X, Y)[splitSize ,] # parâmetro 1 do KNN
DataSet_Teste.x=cbind (District,Dayofweek,Hour, X, Y)[-splitSize ,]  # Parâmetro 2 do KNN
DataSet_Category = DS_Treino$Category[splitSize]    # Categorias das observações (È o terceiro parâmetro necessário)
category_teste=Category[-splitSize] 


########################################################################
###ELBOW METHOD TO FIND K VALUE
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
#             Plotting variables 

plot(data.frame(x = DataSet_Treino.x), col=as.numeric(as.factor(DataSet_Category)))

                              ################
                              #      KNN     #
                              ################

#Para evitar obter o mesmo ponto de partida:
set.seed (1)

# K = 2
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
# K = 4

predicao_knn_4 =knn (DataSet_Treino.x,DataSet_Teste.x,DataSet_Category ,k=4)

confusion.matrix <- table(category_teste,predicao_knn_4)
confusion.matrix
accuracy <- sum(diag(confusion.matrix)) / sum(confusion.matrix)
accuracy

# K = 5
predicao_knn_5 =knn(DataSet_Treino.x,DataSet_Teste.x,DataSet_Category ,k=5)

confusion.matrix <- table(category_teste,predicao_knn_5)
confusion.matrix
accuracy <- sum(diag(confusion.matrix)) / sum(confusion.matrix)*100
accuracy

