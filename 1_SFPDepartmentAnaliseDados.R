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

#Column ID inexistent
duplicated(DS_Treino)

Duplicados <- DS_Treino[duplicated(DS_Treino), ]
nrow(Duplicados)

#############################################################################################
#2 Application of methodologies regarding the "cleanliness" and consistency of the DataSet##########
#############################################################################################

#Add new variables
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
#######Missing values ##################################################
#########################################################################

# NULL sum test data
sum(is.na(DS_Teste))

# NULL sum train data
sum(is.na(DS_Treino))

# Check NULL
is.null(DS_Treino) 
is.null(DS_Teste)  

##########################################################################################
###4 checking labels/columns#############################################################
#########################################################################################

# Labels test dataset
names(DS_Teste)


# Labels train dataset
names(DS_Treino)


########################################################################################
############## Number of crime categories in San Francisco? ############################
########################################################################################

# Group by Crime Category, calculate the number of crimes by category and calculate #percentage
categorias <- treino %>%  
  group_by(Category) %>%
  summarise(Crimes = n()) %>%
  mutate(percentagem=round(Crimes/sum(Crimes)*100, 4))

View(categorias)
#View the categories of crime that occur in the greatest number
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
#7 patterns analysis in datasets against date variable ##############################
#####################################################################################

#DATASET TREINO
#Group by day, week, month and year
AgregaPorDia <- treino %>% 
  group_by(dia, dia_semana, mes, Ano,diadomes) %>%
  summarise(Crimes = n())

#Histogram By Day of the Week

ggplot(AgregaPorDia, aes(x = dia_semana, y = Crimes) ) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 150000))+
  geom_bar(stat = 'identity')

#Boxplot per weekday
ggplot(AgregaPorDia, aes(x = dia_semana, y = Crimes))+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +geom_boxplot()+
  scale_x_discrete(name = "Dias Semana") +
  scale_y_continuous(name = "Total dia Semana")

##Histogram per month
ggplot(AgregaPorDia, aes(x = mes, y = Crimes) ) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100000))+
  geom_bar(stat = 'identity')

#Boxplot per month
ggplot(AgregaPorDia, aes(x = mes, y = Crimes))+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +geom_boxplot()+
  scale_x_discrete(name = "Meses") +
  scale_y_continuous(name = "Total por mes")

##Histograma per year
ggplot(AgregaPorDia, aes(x = factor(Ano), y = Crimes) ) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100000))+
  geom_bar(stat = 'identity')

##Boxplot per year
ggplot(AgregaPorDia, aes(x = factor(Ano), y = Crimes) ) +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +geom_boxplot()+
  scale_x_discrete(name = "Crimes por mês") +
  scale_y_continuous(name = " Nº de Crimes")

##Histogram per day month
ggplot(AgregaPorDia, aes(x = diadomes, y = Crimes) ) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100000))+
  geom_bar(stat = 'identity')



#DATASET TEST
AgregaPorDiateste <- teste%>% 
  group_by(dia, dia_semana, mes, Ano,diadomes) %>%
  summarise(Crimes = n())

#Histogram By Day of the Week
ggplot(AgregaPorDiateste, aes(x = dia_semana, y = Crimes))+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +geom_boxplot()+
  scale_x_discrete(name = "Dias Semana") +
  scale_y_continuous(name = "Total dia Semana")

#Histogram By month
ggplot(AgregaPorDiateste, aes(x = mes, y = Crimes))+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +geom_boxplot()+
  scale_x_discrete(name = "Meses") +
  scale_y_continuous(name = "Total por mes")
