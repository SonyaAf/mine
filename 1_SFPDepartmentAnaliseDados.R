#####################################################

#install.packages("ggmap")
#install.packages('plotly')
#install.packages('ggplot2')
#install.packages("rpart.plot")


library(ggplot2)
library(lubridate)
library(stringr)
library(psych)
library(reshape2)
library(plotly)
library(sp)
library(dplyr)
library(plotly)



#Carregar o DataSet
DS_Treino <- read.csv("train.txt",stringsAsFactors = FALSE)   # DataSet Treino
DS_Teste <- read.csv("teste.csv",stringsAsFactors = FALSE)   # DataSet teste

View(DS_Treino)
View(DS_Teste)


#*******************************************************
#1.    Evaluation and quality of data.
#*******************************************************

length(DS_Teste)    
length(DS_Treino)


# summary of 2 datasets
summary(DS_Treino)
summary(DS_Teste)

# Atribute names
names(DS_Teste)
names(DS_Treino)

#Existence of nulls
is.null(DS_Teste)
is.null(DS_Treino)

#Missing data
sum(is.na(DS_Teste))
sum (is.na(DS_Treino))

#Test dataset analysis
#Exploratory analysis datasets. testing dataset:  

#Removing Id column for duplicates
#DS_Teste$Id<-NULL
names(DS_Teste)  #confirm removed data column

#Check duplicates
duplicated(DS_Teste[2:7])

#Check duplicates
Duplicados <- DS_Teste[duplicated(DS_Teste), ]
View(Duplicados)

nrow(unique(Duplicados))
nrow(Duplicados)

#count duplicates
nrow(Duplicados)

max(as_date(DS_Teste$Dates))
min(as_date(DS_Teste$Dates)) 


#Exploring train dataset

#checking nulls train dataset
is.null(DS_Treino)

#Missing data in train dataset
sum(is.na(DS_Treino))

#O dataset de treino não tem a coluna id. pelo que não será necessária à sua remoção
duplicated(DS_Treino)

Duplicados <- DS_Treino[duplicated(DS_Treino), ]
nrow(Duplicados)

#############################################################################################
#2 Aplicação de metodologias respeitante à "limpeza" e consistência do DataSet##########
#############################################################################################

#Acrescentar Novas Váriavies
Ano <- year(as.Date(DS_Treino$Dates))
mes <- month(as.Date(DS_Treino$Dates), label=TRUE)
dia_semana=weekdays(as.Date(DS_Treino$Dates))
dia = as.Date(DS_Treino$Dates," %Y-%m-%d ")
diadomes =day(as.Date(DS_Treino$Dates))
hora = str_pad(hour(DS_Treino$Dates), 2, pad = "0")
treino <- mutate(DS_Treino, Ano, mes, dia_semana, dia,diadomes, hora)


Ano <- year(as.Date(DS_Teste$Dates))
mes <- month(as.Date(DS_Teste$Dates), label=TRUE)
dia_semana=weekdays(as.Date(DS_Teste$Dates))
dia = as.Date(DS_Teste$Dates," %Y-%m-%d ")
diadomes =day(as.Date(DS_Teste$Dates))
hora = str_pad(hour(DS_Teste$Dates), 2, pad = "0") 

teste <- mutate(DS_Teste, Ano, mes, dia_semana, dia, diadomes, hora)

########################################################################
#######3 Verificação de Missing values no dataset#######################
#########################################################################

# sumarização dos valores nulos no dataset de teste
sum(is.na(DS_Teste))

# sumarização dos valores nulos no dataset de treino
sum(is.na(DS_Treino))

# O dataset não apresenta valores em falta 
is.null(DS_Treino) 
is.null(DS_Teste)  

##########################################################################################
###4 Verificação de Informação, em função das variáveis presentes no DataSet de teste######
#########################################################################################

# Labels do dataset de teste
names(DS_Teste)


# Labels do dataset de treino
names(DS_Treino)


########################################################################################
####5.Nº de categorias de crime existente em São Francisco? ############################
########################################################################################

#Agrupar por Categoria Crime, calcular o nº ocorrências de crimes por categoria e calcular #percentagem
categorias <- treino %>%  
  group_by(Category) %>%
  summarise(Crimes = n()) %>%
  mutate(percentagem=round(Crimes/sum(Crimes)*100, 4))

View(categorias)
#Visualizar as categorias de crime que ocorrem em maior número
ggplot(categorias, aes(x = reorder(Category, -Crimes), y = Crimes, fill = Crimes, label = Crimes) ) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  scale_fill_continuous(guide=FALSE) +
  labs(x = '', y = 'Total de crimes', title = 'Crimes por Categoria')+
  theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5))+
  geom_text(size = 3, position = position_dodge(width = 1))
#ou , 
View(categorias%>%top_n(1,Crimes))
View(categorias%>%top_n(1,-Crimes))


#####################################################################################
#7 Verificação de padrões nos datasets em relação à variável date####################
#####################################################################################

#DATASET TREINO
#Agrupar por dia, semana, mes e ano
AgregaPorDia <- treino %>% 
  group_by(dia, dia_semana, mes, Ano,diadomes) %>%
  summarise(Crimes = n())

#Histograma Por dia da Semana

ggplot(AgregaPorDia, aes(x = dia_semana, y = Crimes) ) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 150000))+
  geom_bar(stat = 'identity')

#Boxplot por dia da semana 
ggplot(AgregaPorDia, aes(x = dia_semana, y = Crimes))+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +geom_boxplot()+
  scale_x_discrete(name = "Dias Semana") +
  scale_y_continuous(name = "Total dia Semana")

##Histograma Por mes
ggplot(AgregaPorDia, aes(x = mes, y = Crimes) ) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100000))+
  geom_bar(stat = 'identity')

#Boxplot por mes
ggplot(AgregaPorDia, aes(x = mes, y = Crimes))+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +geom_boxplot()+
  scale_x_discrete(name = "Meses") +
  scale_y_continuous(name = "Total por mes")

##Histograma Por ano
ggplot(AgregaPorDia, aes(x = factor(Ano), y = Crimes) ) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100000))+
  geom_bar(stat = 'identity')

##Boxplot Por ano
ggplot(AgregaPorDia, aes(x = factor(Ano), y = Crimes) ) +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +geom_boxplot()+
  scale_x_discrete(name = "Crimes por mês") +
  scale_y_continuous(name = " Nº de Crimes")

##Histograma Por dia do mes   #### WARNING#####
ggplot(AgregaPorDia, aes(x = diadomes, y = Crimes) ) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100000))+
  geom_bar(stat = 'identity')



#DATASET TESTE
AgregaPorDiateste <- teste%>% 
  group_by(dia, dia_semana, mes, Ano,diadomes) %>%
  summarise(Crimes = n())

#Por dia da Semana
ggplot(AgregaPorDiateste, aes(x = dia_semana, y = Crimes))+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +geom_boxplot()+
  scale_x_discrete(name = "Dias Semana") +
  scale_y_continuous(name = "Total dia Semana")

#Por mes
ggplot(AgregaPorDiateste, aes(x = mes, y = Crimes))+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +geom_boxplot()+
  scale_x_discrete(name = "Meses") +
  scale_y_continuous(name = "Total por mes")
