################################### SUPPORT VECTOR MACHINES ##########################################

library(lubridate)
library(dplyr)
library (class)
library(e1071)
#library(kernlab)


############## Business Understanding ################################# 


#data train
DS_Treino <- read.csv("train.txt",stringsAsFactors = FALSE)  
DS_Treino <- DS_Treino %>%filter(Category %in% c("OTHER OFFENSES","LARCENY/THEFT","DRUG/NARCOTIC","ASSAULT","NON-CRIMINAL"))

Category <- DS_Treino$Category
District <- as.numeric(as.factor(DS_Treino$PdDistrict))
Dayofweek<- as.numeric(as.factor(DS_Treino$DayOfWeek))
Hour     <- as.numeric(as.factor(hour(DS_Treino$Dates)))
X        <- as.numeric(DS_Treino$X)
Y        <- as.numeric(DS_Treino$Y)
#X <- (DS_Treino$X-mean(DS_Treino$X))/sd(DS_Treino$X)
#Y <- (DS_Treino$Y-mean(DS_Treino$Y))/sd(DS_Treino$Y)

#Split dataset
percentagem = 0.05
total = nrow(DS_Treino)

splitSize = sample(total, percentagem * total)

#Treino.x=cbind(District,Dayofweek)[splitSize ,]
#Treino.x=cbind(District,Dayofweek,Hour)[splitSize ,]
#Treino.x=cbind(X,Y)[splitSize ,]
#Treino.x=cbind(District,Hour, Dayofweek,  X, Y)[splitSize ,]
Treino.x=cbind(District,Dayofweek,Hour, X, Y)[splitSize ,]
Categoria.y = cbind(Category)[splitSize ,]

#Categoria.y = as.numeric(as.factor(Categoria.y))
#unique(Categoria.y)
#plot(Treino.x, col =as.numeric(as.factor(Categoria.y)))

df_treino = data.frame(x = Treino.x, y = as.factor(Categoria.y))
#plot(df_treino[names(df_treino)], col=as.numeric(as.factor(Categoria.y)))

#svmfit =ksvm(y~., data=df_treino , kernel='rbfdot')
#svmfit =svm(y~., data=df_treino) # colocar sempre escala a false porque temos dados standardizados
#svmfit =svm(y~., data=df_treino , type="C-classification", kernel="sigmoid") # colocar sempre escala a false porque temos dados standardizados
svmfit = svm(y~., data=df_treino, kernel="sigmoid", scale = FALSE) # colocar sempre escala a false porque temos dados standardizados

windows()
plot(svmfit , data=df_treino, x.District~x.Dayofweek, slice = list(x.District=1, x.Dayofweek=1))
plot(svmfit , data=df_treino, x.Hour~x.Dayofweek)
plot(svmfit , data=df_treino, x.X~x.Y)
plot(svmfit , data=df_treino, x.District~x.Hour)

svmfit$index# support vector
summary (svmfit)
names(df_treino)
# test with train data

pred <- predict(svmfit, df_treino)
# (same as:)
#pred <- fitted(model)

# Check accuracy:
confusion.matrix <-table(pred, Categoria.y)
confusion.matrix
accuracy <- sum(diag(confusion.matrix)) / sum(confusion.matrix)*100
accuracy
