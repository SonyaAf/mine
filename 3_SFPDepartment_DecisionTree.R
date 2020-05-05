#####################################################
# Trabalho de ECDE
#                 Cl�udio Rocha
#                 Marisa Nascimento
#                 S�nia Afonso
#####################################################
#install.packages("ggmap")
#install.packages('plotly')
#install.packages('ggplot2')

#install.packages("rpart.plot")

library(lubridate)
library(stringr)
library(psych)
library(sp)
library(dplyr)
library(plotly)

library(rpart)
library(rpart.plot)

#Carregar o DataSet
DS_Treino <- read.csv("train.txt",stringsAsFactors = FALSE)   # DataSet Treino

#DS_Teste <- read.csv("teste.csv",stringsAsFactors = FALSE)   # DataSet teste


DS_Treino <- DS_Treino %>%  
filter(!Category %in% c("OTHER OFFENSES","LARCENY/THEFT","DRUG/NARCOTIC"))


#Eliminar as vari�veis 
#Foram eliminadas do DS_Treino as vari�veis Descript e Resolution porque n�o est�o presentes no DS_Teste. 
#DS_Treino$Descript <- NULL
#DS_Treino$Resolution <- NULL

#Split do dataset
percentagem = 0.7
total = nrow(DS_Treino)

splitSize = sample(total, percentagem * total)

treino = DS_Treino[ splitSize,]
teste  = DS_Treino[-splitSize,]

#nrow(teste)
#nrow(treino)

## Using lubridate

treino$dia <- as.numeric(day(treino$Dates))
treino$ano <- as.numeric(year(treino$Dates))
treino$mes <- as.numeric(month(treino$Dates))
treino$hora <-  as.numeric(hour(treino$Dates))
treino$Dates <- as.numeric(paste(year(treino$Dates), str_pad(month(treino$Dates), 2, pad = "0"), day(treino$Dates),str_pad(hour(treino$Dates), 2, pad = "0"),str_pad(minute(treino$Dates), 2, pad = "0"), sep=""))
treino$X <- as.numeric(treino$X)
treino$Y <- as.numeric(treino$Y)

#treino$X <- (treino$X-mean(treino$X))/sd(treino$X)
#treino$Y <- (treino$Y-mean(treino$Y))/sd(treino$Y)

treino$period<-cut(treino$hora,breaks=c(0,5,11,18,23),labels=c('madrugada','manh�','tarde','noite'),include.lowest = TRUE)

treino$period <- as.numeric(treino$period)

treino$tipo_rua <- substr(treino$Address, nchar(as.character(treino$Address))-2, nchar(as.character(treino$Address)))
treino$tipo_rua <- as.numeric(as.factor(treino$tipo_rua ))
treino$DayOfWeek <- as.character(treino$DayOfWeek)
treino$PdDistrict <- as.character(treino$PdDistrict)
#treino$Address <- as.character(treino$Address)
treino$Category <- as.character(treino$Category)


teste$dia <- as.numeric(day(teste$Dates))
teste$ano <- as.numeric(year(teste$Dates))
teste$mes <- as.numeric(month(teste$Dates))
teste$hora <-  as.numeric(hour(teste$Dates))
teste$Dates <- as.numeric(paste(year(teste$Dates), str_pad(month(teste$Dates), 2, pad = "0"), day(teste$Dates),str_pad(hour(teste$Dates), 2, pad = "0") ,str_pad(minute(teste$Dates), 2, pad = "0"), sep=""))
teste$X <- as.numeric(teste$X)
teste$Y <- as.numeric(teste$Y)
#Normaliza��o das coordenadas
#teste$X=(teste$X-mean(teste$X))/sd(teste$X)
#teste$Y=(teste$Y-mean(teste$Y))/sd(teste$Y)

teste$period <- cut(teste$hora,breaks=c(0,5,11,18,23),labels=c('madrugada','manh�','tarde','noite'),include.lowest = TRUE)
teste$period  <- as.numeric(teste$period)


teste$tipo_rua <- substr(teste$Address, nchar(as.character(teste$Address))-2, nchar(as.character(teste$Address)))
teste$tipo_rua <- as.numeric(as.factor(teste$tipo_rua ))
teste$DayOfWeek <- as.character(teste$DayOfWeek)
teste$PdDistrict <- as.character(teste$PdDistrict)
teste$Category <- as.character(teste$Category)
#teste$Address <- as.character(teste$Address)


#View(teste)
#View(treino)

# criar m�dulo para decis�o de �rvore e predizer a classifica��o das esp�cies em fun��o de todos os seus par�metros

#Tree_Class<-rpart(Category~DayOfWeek + period+ PdDistrict + X + Y, data = treino, method="class", control=rpart.control(cp=0.005,minsplit=100))
Tree_Class<-rpart(Category~DayOfWeek + PdDistrict + tipo_rua + period + X + Y, data = treino, method="class", control=rpart.control(cp=0.005,minsplit=100))


#Para desenhar �rvore
#windows()
rpart.plot(Tree_Class)

rpart.plot(Tree_Class, type=4, extra=103)

prp(Tree_Class,type=4,extra=103)

# Agora que temos a �rvore, vamos ent�o fazer o que pretendemos: PREDICOES
# Criado o modelo com o Training DataSet, vamos predizer usando o Test DataSet
#todas as linhas e coluna 5 que � a esp�cie, type = classification
Category_Predita <- predict(Tree_Class,teste, type="class")
#View(Category_Predita)
# Feita a predi��o, vamos comparar de que forma a predi��o se adequa aos valores reais de teste
tabela <- as.data.frame(table(data.frame(teste[,2], Category_Predita)))
#View(tabela)  


confusion.matrix <- table(teste[,2],Category_Predita)
accuracy <- sum(diag(confusion.matrix)) / sum(confusion.matrix)*100
accuracy
#View(confusion.matrix)
# Resultado:
#Iris_Predita
#                     setosa      versicolor  virginica
#     setosa          13          0           0
#     versicolor      0           17          1
#     virginica       0           3           16

###!!! ANALISE:
# Colunas representam predi��es. Linhas representam observa��es reais. Em alguns casos verificam-se erros: 
# 3 predi��es incorrectas em que se previu ser versicolor quando era virginica e 1 erro em que se identificou virginica 
# quando na realidade era versicolor. 
# Isto tudo num conjunto total de 50 oberva��es que comp�em o nosso dataset de treino.
# Total de incorre��es: 4/50, cerca de 10%

#### Este erro tem a haver com o sampling que fizemos. Se se refazer o sampling, concerteza teremos outros 
# resultados. 

#### TakeaWay: � ESSENCIAL ter um bom TRAINING set para se garantir que se reduz o erro de classifica��o
# nestes algoritmos de classifica��o supervisionada.


