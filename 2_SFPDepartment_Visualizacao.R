#####################################################
# Trabalho de ECDE
#                 Cláudio Rocha
#                 Marisa Nascimento
#                 Sónia Afonso
#####################################################

#install.packages("ggmap")
#install.packages('plotly')
#install.packages('ggplot2')

library(ggplot2)
library(ggmap)
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
View(DS_Treino)

#Acrescentar Novas Váriavies 
Ano <- year(as.Date(DS_Treino$Dates))

#Mês por extenso
mes <- month(as.Date(DS_Treino$Dates), label=TRUE)

#Mês por extenso
dia_semana=weekdays(as.Date(DS_Treino$Dates))

#Dia
dia = as.Date(DS_Treino$Dates," %Y-%m-%d ")

#Dia
dia_mes = day(DS_Treino$Dates)

#Hora 
hora = str_pad(hour(DS_Treino$Dates), 2, pad = "0")    

#Tipo de Rua
tipo_rua <- substr(DS_Treino$Address, nchar(DS_Treino$Address)-2, nchar(DS_Treino$Address))

#Acrescentar novas váriáveis ao DataSet
treino <- mutate(DS_Treino, Ano, mes, dia_mes, dia_semana, dia, hora, tipo_rua) 
View(unique(treino$tipo_rua))

####################
#!!!VISUALIZAÇÃO!!!#
####################

##############################################################################
# A. Verifique quais são as categorias de crime que ocorrem em maior número. #
##############################################################################

#Agrupar por Categoria Crime e calcular percentagem
categorias <- treino %>%  
  group_by(Category) %>% 
  summarise(Crimes = n()) %>% 
  mutate(percentagem=round(Crimes/sum(Crimes)*100, 4)) 

View(categorias)

#Quais são as categorias de crime que ocorrem em maior número
ggplot(categorias, aes(x = reorder(Category, -Crimes), y = Crimes, fill = Crimes, label = Crimes) ) + 
  geom_bar(stat = 'identity') + 
  coord_flip() + 
  scale_fill_continuous(guide=FALSE) + 
  labs(x = '', y = 'Total de crimes', title = 'Crimes por Categoria')+
  theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5))+
  geom_text(size = 3, position = position_dodge(width = 1))

##########################################################################################################
#.Questão GR2, GR4, GR6, GR8, GR10:
#  Em que dias são mais frequentes os crimes relacionados com Rapto (kidnapping)? 
#  Isole o histograma. Poderá haver alguma relação com relato de pessoas desaparecidas (MissingPerson)?
#########################################################################################################

#Agrupar por Categoria Crime e dia semana e calcular percentagem
CrimesDiasSemana <- treino %>%  
  group_by(dia_semana,Category) %>% 
  summarise(crimes_semana = n()) %>% 
  mutate(percentagem_semana=round(crimes_semana/sum(crimes_semana)*100, 4)) %>%
  filter(Category=='KIDNAPPING')#Isolar o Istograma para Rapto (kidnapping)

View(CrimesDiasSemana)

#Isolar o Istograma para Rapto (kidnapping)

ggplot(CrimesDiasSemana, aes(x = reorder(dia_semana, -crimes_semana), y = crimes_semana, fill = crimes_semana, label = crimes_semana) ) + 
  geom_bar(stat = 'identity') + 
  coord_flip() + 
  #scale_fill_continuous(guide=FALSE) + 
  labs(x = '', y = 'Total Raptos', title = 'Raptos por dia SEmana')+
  theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5))+
  geom_text(size = 3, position = position_dodge(width = 1))

##########################################################################################################
# Poderá haver alguma relação com relato de pessoas desaparecidas (MissingPerson)?
#########################################################################################################
####Por dia Semana
CrimesDiasSemana <- treino %>%  
  group_by(dia_semana, Category) %>% 
  summarise(crimes_semana = n()) %>% 
  mutate(percentagem_semana=round(crimes_semana/sum(crimes_semana)*100, 4)) %>%
  filter(Category %in% c("MISSING PERSON", "KIDNAPPING"))
View(CrimesDiasSemana)
#Gráfico de Barras
ggplot(CrimesDiasSemana, aes(dia_semana, crimes_semana)) +   
  geom_bar(aes(fill = Category), position = "dodge", stat="identity")+
  labs(x = '', y = 'Crimes', title = 'MISSING PERSON vs KIDNAPPING por dia semana')  
#Densidade
ggplot(CrimesDiasSemana, aes(crimes_semana, colour = Category)) +
  geom_density() +
  xlim(289, 4663)


#Correlação
Correlacao <- CrimesDiasSemana %>% mutate(KIDNAPPING=as.numeric(ifelse(Category=="KIDNAPPING",crimes_semana,0)),MISSINGPERSON=as.numeric(ifelse(Category=="MISSING PERSON",crimes_semana,0))) %>% 
  select(dia_semana,KIDNAPPING,MISSINGPERSON)%>%group_by(dia_semana)%>%summarise(KIDNAPPING = sum(KIDNAPPING), MISSINGPERSON = sum(MISSINGPERSON)) 
View(Correlacao)

cor(Correlacao[2:3])
pairs.panels(Correlacao[2:3])


####Por ano

CrimesAno <- treino %>%  
  group_by(Ano, Category) %>% 
  summarise(crimes_ano = n()) %>% 
  mutate(percentagem_ano=round(crimes_ano/sum(crimes_ano)*100, 4)) %>%
  filter(Category %in% c("MISSING PERSON", "KIDNAPPING"))

View(CrimesAno)
#Gráfico de Barras
ggplot(CrimesAno, aes(Ano, crimes_ano)) +   
  geom_bar(aes(fill = Category), position = "dodge", stat="identity")+
  labs(x = '', y = 'Crimes por ano', title = 'MISSING PERSON vs KIDNAPPING Ano')  
#Densidade
ggplot(CrimesAno, aes(crimes_ano, colour = Category)) +
  geom_density() +
  xlim(80, 2400)


#Correlação
Correlacao <- CrimesAno %>% mutate(KIDNAPPING=as.numeric(ifelse(Category=="KIDNAPPING",crimes_ano,0)),MISSINGPERSON=as.numeric(ifelse(Category=="MISSING PERSON",crimes_ano,0))) %>% 
  select(Ano,KIDNAPPING,MISSINGPERSON)%>%group_by(Ano)%>%summarise(KIDNAPPING = sum(KIDNAPPING), MISSINGPERSON = sum(MISSINGPERSON)) 
View(Correlacao)

cor(Correlacao[2:3])
pairs.panels(Correlacao[2:3])

##Por dia
CrimesDia <- treino %>%  
  group_by(dia, Category) %>% 
  summarise(crimes_dia = n()) %>% 
  mutate(percentagem_dia=round(crimes_dia/sum(crimes_dia)*100, 4)) %>%
  filter(Category %in% c("MISSING PERSON", "KIDNAPPING"))

View(CrimesDia)
#Gráfico de Barras
ggplot(CrimesDia, aes(dia, crimes_dia)) +   
  geom_bar(aes(fill = Category), position = "dodge", stat="identity")+
  labs(x = '', y = 'Crimes por ano', title = 'MISSING PERSON vs KIDNAPPING Ano')  
#Densidade
ggplot(CrimesDia, aes(crimes_dia, colour = Category)) +
  geom_density() +
  xlim(1, 43)


#Correlação
Correlacao <- CrimesDia %>% mutate(KIDNAPPING=as.numeric(ifelse(Category=="KIDNAPPING",crimes_dia,0)),MISSINGPERSON=as.numeric(ifelse(Category=="MISSING PERSON",crimes_dia,0))) %>% 
  select(dia,KIDNAPPING,MISSINGPERSON)%>%group_by(dia)%>%summarise(KIDNAPPING = sum(KIDNAPPING), MISSINGPERSON = sum(MISSINGPERSON)) 
View(Correlacao)

cor(Correlacao[2:3])
pairs.panels(Correlacao[2:3])

##########################################################################################################
#.Questão GR1-10:Em que dias são mais frequentes os crimes relacionados com roubo de veículos (VehicleTheft)? 
# Isole o histograma. 
# Poderá haver relação com roubos em geral (Robbery?)
##########################################################################################################

#Agrupar por Categoria Crime e dia semana e calcular percentagem
CrimesDiasSemana <- treino %>%  
  group_by(dia_semana,Category) %>% 
  summarise(crimes_semana = n()) %>% 
  mutate(percentagem_semana=round(crimes_semana/sum(crimes_semana)*100, 4)) %>%
  filter(Category=='VEHICLE THEFT')#Isolar o Istograma para Rapto (kidnapping)

View(CrimesDiasSemana)

#Isolar o Istograma para Rapto (kidnapping)

ggplot(CrimesDiasSemana, aes(x = reorder(dia_semana, -crimes_semana), y = crimes_semana, fill = crimes_semana, label = crimes_semana) ) + 
  geom_bar(stat = 'identity') + 
  coord_flip() + 
  #scale_fill_continuous(guide=FALSE) + 
  labs(x = '', y = 'Total Raptos', title = 'Roubo de Viaturas por dia SEmana')+
  theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5))+
  geom_text(size = 3, position = position_dodge(width = 1))

CrimesDiasMes <- treino %>%  
  group_by(dia_mes,Category) %>% 
  summarise(crimes_dia_mes = n()) %>% 
  filter(Category=='VEHICLE THEFT')#Isolar o Istograma para Rapto (kidnapping)

ggplot(CrimesDiasMes, aes(x =dia_mes, y = crimes_dia_mes, fill = crimes_dia_mes, label = crimes_dia_mes) ) + 
  geom_bar(stat = 'identity') + 
  #scale_fill_continuous(guide=FALSE) + 
  labs(x = '', y = 'Total Raptos', title = 'Roubo de Viaturas por dia SEmana')+
  theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5))+
  geom_text(size = 3, position = position_dodge(width = 1))

ggplot(CrimesDiasMes, aes(x = dia_mes, y = crimes_dia_mes ) ) + 
  geom_bar(stat = 'identity')


###########################
##Questão GR1-10
###########################
#Agrupar por Categoria Crime e dia semana e calcular percentagem
CrimesDiasSemana <- treino %>%  
  group_by(dia_semana,Category) %>%
  summarise(crimes_semana = n()) %>%
  mutate(percentagem_semana=round(crimes_semana/sum(crimes_semana)*100, 4)) %>%
  filter(Category=='VEHICLE THEFT')

#Isolar o Istograma 
ggplot(CrimesDiasSemana, aes(x = reorder(dia_semana, -crimes_semana), y = crimes_semana, fill = crimes_semana, label = crimes_semana) ) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  #scale_fill_continuous(guide=FALSE) +
  labs(x = '', y = 'Total', title = 'Roubo de Viaturas por dia Semana')+
  theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5))+
  geom_text(size = 3, position = position_dodge(width = 1))

##Por Zona
treino %>%filter(.,Category %in% c("VEHICLE THEFT", "ROBBERY"))%>%  
  group_by(PdDistrict, Category) %>%
  #summarise(crimes = n()) %>%
  tally()%>%
  ggplot(.,aes(x=PdDistrict,y=n,color=Category))+
  geom_line(aes(group=Category))+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+
  scale_x_discrete(name = "Zona") +
  scale_y_continuous(name = "Registos")+
  geom_point()


##Por semana
treino$dia_semana <- factor(treino$dia_semana, levels = c("segunda-feira","terça-feira","quarta-feira","quinta-feira","sexta-feira", "sábado","domingo"))
treino %>%filter(.,Category %in% c("VEHICLE THEFT", "ROBBERY"))%>%  
  group_by(dia_semana, Category) %>%
  #summarise(crimes = n()) %>%
  tally()%>%
  ggplot(.,aes(x=dia_semana,y=n,color=Category))+
  geom_line(aes(group=Category))+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+
  scale_x_discrete(name = "Dias Semana") +
  scale_y_continuous(name = "Registos")+
  geom_point()

##Por ano
treino %>%filter(.,Category %in% c("VEHICLE THEFT", "ROBBERY"))%>%  
  group_by(Ano, Category) %>%
  #summarise(crimes = n()) %>%
  tally()%>%
  ggplot(.,aes(x=as.character(Ano),y=n,color=Category))+
  geom_line(aes(group=Category))+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+
  scale_x_discrete(name = "Ano") +
  scale_y_continuous(name = "Registos")+
  geom_point()

##########################################################################################################
# Poderá haver relação com roubos em geral (Robbery?)
##########################################################################################################
#Agrupar por Categoria Crime e dia semana e calcular percentagem
CrimesDiasSemana <- treino %>%  
  group_by(dia_semana,Category) %>% 
  summarise(crimes_semana = n()) %>% 
  mutate(percentagem_semana=round(crimes_semana/sum(crimes_semana)*100, 4)) %>%
  filter(Category %in% c("VEHICLE THEFT", "ROBBERY"))
View(CrimesDiasSemana)
#Gráfico de Barras
ggplot(CrimesDiasSemana, aes(dia_semana, crimes_semana)) +   
  geom_bar(aes(fill = Category), position = "dodge", stat="identity")+
  labs(x = '', y = 'Crimes', title = 'VEHICLE THEFT vs ROBBERY por dia semana')  
#Densidade
ggplot(CrimesDiasSemana, aes(crimes_semana, colour = Category)) +
  geom_density() +
  xlim(3194, 8613)


#Correlação
Correlacao <- CrimesDiasSemana %>% mutate(VEHICLETHEFT=as.numeric(ifelse(Category=="VEHICLE THEFT",crimes_semana,0)),ROBBERY=as.numeric(ifelse(Category=="ROBBERY",crimes_semana,0))) %>% 
  select(dia_semana,VEHICLETHEFT,ROBBERY)%>%group_by(dia_semana)%>%summarise(VEHICLETHEFT = sum(VEHICLETHEFT), ROBBERY = sum(ROBBERY)) 
View(Correlacao)

cor(Correlacao[2:3])
pairs.panels(Correlacao[2:3])


####Por ano

CrimesAno <- treino %>%  
  group_by(Ano, Category) %>% 
  summarise(crimes_ano = n()) %>% 
  mutate(percentagem_ano=round(crimes_ano/sum(crimes_ano)*100, 4)) %>%
  filter(Category %in% c("VEHICLE THEFT", "ROBBERY"))

View(CrimesAno)
#Gráfico de Barras
ggplot(CrimesAno, aes(Ano, crimes_ano)) +   
  geom_bar(aes(fill = Category), position = "dodge", stat="identity")+
  labs(x = '', y = 'Crimes por ano', title = 'MISSING PERSON vs KIDNAPPING Ano')  
#Densidade
ggplot(CrimesAno, aes(crimes_ano, colour = Category)) +
  geom_density() +
  xlim(80, 2400)


#Correlação
Correlacao <- CrimesAno %>% mutate(VEHICLETHEFT=as.numeric(ifelse(Category=="VEHICLE THEFT",crimes_ano,0)),ROBBERY=as.numeric(ifelse(Category=="ROBBERY",crimes_ano,0))) %>% 
  select(Ano,VEHICLETHEFT,ROBBERY)%>%group_by(Ano)%>%summarise(VEHICLETHEFT = sum(VEHICLETHEFT), ROBBERY = sum(ROBBERY)) 
View(Correlacao)

cor(Correlacao[2:3])
pairs.panels(Correlacao[2:3])

##Por dia
CrimesDia <- treino %>%  
  group_by(dia, Category) %>% 
  summarise(crimes_dia = n()) %>% 
  mutate(percentagem_dia=round(crimes_dia/sum(crimes_dia)*100, 4)) %>%
  filter(Category %in% c("VEHICLE THEFT", "ROBBERY"))

View(CrimesDia)
#Gráfico de Barras
ggplot(CrimesDia, aes(dia, crimes_dia)) +   
  geom_bar(aes(fill = Category), position = "dodge", stat="identity")+
  labs(x = '', y = 'Crimes por ano', title = 'VEHICLE THEFT vs ROBBERY Ano')  
#Densidade
ggplot(CrimesDia, aes(crimes_dia, colour = Category)) +
  geom_density() +
  xlim(1, 43)


#Correlação
Correlacao <- CrimesDia %>% mutate(VEHICLETHEFT=as.numeric(ifelse(Category=="VEHICLE THEFT",crimes_dia,0)),ROBBERY=as.numeric(ifelse(Category=="ROBBERY",crimes_dia,0))) %>% 
  select(dia,VEHICLETHEFT,ROBBERY)%>%group_by(dia)%>%summarise(VEHICLETHEFT = sum(VEHICLETHEFT), ROBBERY = sum(ROBBERY)) 
View(Correlacao)

cor(Correlacao[2:3])
pairs.panels(Correlacao[2:3])


##########################################################################################################
# B.Qual é a categoria de crime que ocorre com maior frequência?
##########################################################################################################


CrimesDia <- treino %>%  
  group_by(Category, dia) %>% 
  summarise(crimes_dia = n()) %>% 
  mutate(soma=sum(crimes_dia), FreqRel=round(crimes_dia/sum(crimes_dia), 4),FreqAbs=round(sum(crimes_dia)/nrow(treino), 4))
View(CrimesDia)
ggplot(CrimesDia, aes(x = reorder(Category, -media), y = media, fill = media, label = media) ) + 
  geom_bar(aes(fill = media), position = "dodge", stat="identity")+
  coord_flip() + 
  labs(x = '', y = 'Média por dia', title = 'Media Crimes por dia') +
  geom_text(size = 3, position = position_dodge(width = 1))

ggplot(CrimesDia, aes(x = reorder(Category, crimes_dia), y = crimes_dia))+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +geom_boxplot()+
  scale_x_discrete(name = "Categoria Crime") +
  scale_y_continuous(name = "Crimes por dia")


##########################################################################################################
# C.Olhando para o Mapa quais as categorias se encontram mais dispersas geograficamente?Poderá esta informação estar relacionada com a resposta à alínea A?
########################################################################################################

categoriasDispersas <- treino %>%  
  group_by(Category, dia) %>% 
  summarise(Crimes = n(), x=mean(X), y=mean(Y)) #%>% 
  #filter(Category %in% c("TREA","PORNOGRAPHY/OBSCENE MAT","GAMBLING","SEX OFFENSES NON FORCIBLE","EXTORTION","BRIBERY","BAD CHECKS","FAMILY OFFENSES","SUICIDE", "EMBEZZLEMENT","ARSON"))
  #filter(Category %in% c("SEX OFFENSES NON FORCIBLE","EXTORTION"))
  #filter(Category %in% c("BRIBERY","BAD CHECKS"))
  #filter(Category %in% c("FAMILY OFFENSES","SUICIDE"))
  #filter(Category %in% c("EMBEZZLEMENT","LOITERING"))
  #filter(Category %in% c("ARSON","LIQUOR LAWS"))
  #filter(Category %in% c("RUNAWAY","DRIVING UNDER THE INFLUENCE"))
  #filter(Category %in% c("KIDNAPPING","RECOVERED VEHICLE"))
  #filter(Category %in% c("DRUNKENNESS","DISORDERLY CONDUCT"))
  #filter(Category %in% c("SEX OFFENSES FORCIBLE","STOLEN PROPERTY"))
  #filter(Category %in% c("TRESPASS","PROSTITUTION"))
  #filter(Category %in% c("WEAPON LAWS","SECONDARY CODES"))
  #filter(Category %in% c("FORGERY/COUNTERFEITING","FRAUD"))
  #filter(Category %in% c("ROBBERY","MISSING PERSON"))
  #filter(Category %in% c("SUSPICIOUS OCC","BURGLARY"))
  #filter(Category %in% c("WARRANTS","VANDALISM"))
  #filter(Category %in% c("VEHICLE THEFT","DRUG/NARCOTIC"))
  #filter(Category %in% c("ASSAULT","NON-CRIMINAL"))
  #filter(Category %in% c("OTHER OFFENSES","LARCENY/THEFT"))
  

#View(categoriasDispersas)

#map <- get_map( maptype = "terrain", source = "google", zoom = 13)
map <- qmap('San Francisco', zoom = 13, maptype = 'terrain')

map +
  geom_point(data = categoriasDispersas, aes(x = x, y = y, size = Crimes, color = Category)) + 
  #scale_size(name = '# Crimes', range = c(3,12)) +
  ggtitle('Locais dos Top 5 Crimes')



map <- qmap('San Francisco', zoom = 13, maptype = 'terrain')
map +
  geom_point(data = categoriasDispersas, aes(x=x, y=x), size=0.75, alpha=0.05) +
  #fte_theme() +
  #theme(...) +
  labs(title = "Locations of Police Arrests Made in San Francisco from 2003 - 2015, by Type of Crime") +
  facet_wrap(~ Category, nrow = 5)






##########################################################################################################
# D.Qual a categoria de crime que se encontra mais concentrada numa determinada zona geográfica?
########################################################################################################
#Agrupar por Categoria Crime e calcular percentagem
categoriasArea <- treino %>%  
  group_by(PdDistrict, Category) %>% 
  summarise(Crimes = n()) 
View(categoriasArea)

plt <- ggplot(categoriasArea, aes(x=reorder(PdDistrict, -Crimes), y=Crimes, fill = Crimes ))+geom_bar(stat = "identity")+
  #coord_flip()+
  labs(x = '', y = 'Número total de crimes', title = 'Categoria de Crimes por dia Semana')+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
plt + facet_wrap(~ Category, scales = "free_y")  


##########################################################################################################
#E.A categoria mais frequente representa que percentagem, no total de crimes de São Francisco?
##########################################################################################################
#Agrupar por Categoria Crime e calcular percentagem
categorias <- treino %>%  
  group_by(Category) %>% 
  summarise(Crimes = n()) %>% 
  mutate(percentagem=round(Crimes/sum(Crimes)*100, 4)) 

View(categorias)

#Quais são as categorias de crime que ocorrem em maior número
ggplot(categorias, aes(x = reorder(Category, -percentagem), y = percentagem, fill = percentagem, label = percentagem) ) + 
  geom_bar(stat = 'identity') + 
  coord_flip() + 
  scale_fill_continuous(guide=FALSE) + 
  labs(x = '', y = '% crimes', title = '% Crimes por Categoria')+
  theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5))+
  geom_text(size = 3, position = position_dodge(width = 1))

##########################################################################################################
#F. Análise os dados em termos de Densidade. 
# Que informação poderá retirar dessa análise em relação às categorias e sua forma/relevância para o Dataset?
##########################################################################################################

##Por dia
CrimesDia <- treino %>%  
  group_by(dia, Category) %>% 
  summarise(crimes_dia = n()) 
#View(CrimesDia)
#Densidade

ggplot(CrimesDia, aes(crimes_dia)) +
  geom_density() +
  xlim(1, 156)+
  facet_wrap(~ Category, scales = "free_y") 

##########################################################################################################
#Questão GR2,GR4,GR6,GR8,GR10:No respeitante aos crimes de invasão de propriedade privada(Trespass) 
#há alguma característica interessante que se destaque?Justifique e conclua.
##########################################################################################################
categoriasTrespass <- treino %>%  
  group_by(Category, hora) %>%
  summarise(Crimes = n()) %>%
  filter(Category %in% c("TRESPASS"))
ggplot(categorias_hora, aes(x = hora, y = Crimes ) ) + 
  geom_bar(stat = 'identity')

ggplot(categoriasTrespass, aes(x = hora, y = Crimes ) ) + 
  geom_bar(stat = 'identity')


categoriasTrespass <- treino %>%  
  group_by(Category, mes) %>%
  summarise(Crimes = n()) %>%
  filter(Category %in% c("TRESPASS"))

ggplot(categoriasTrespass, aes(x = mes, y = Crimes ) ) + 
  geom_bar(stat = 'identity')


categoriasTrespass <- treino %>%  
  group_by(Category, Ano) %>%
  summarise(Crimes = n()) %>%
  filter(Category %in% c("TRESPASS"))

ggplot(categoriasTrespass, aes(x = Ano, y = Crimes ) ) + 
  geom_bar(stat = 'identity')

categoriasTrespass <- treino %>%  
  group_by(Category, dia_semana) %>%
  summarise(Crimes = n()) %>%
  filter(Category %in% c("TRESPASS"))

ggplot(categoriasTrespass, aes(x = dia_semana, y = Crimes ) ) + 
  geom_bar(stat = 'identity')

categoriasTrespass <- treino %>%  
  group_by(Category, PdDistrict) %>%
  summarise(Crimes = n()) %>%
  filter(Category %in% c("TRESPASS"))

ggplot(categoriasTrespass, aes(x = reorder(PdDistrict, -Crimes), y = Crimes ) ) +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+
  geom_bar(stat = 'identity')



categoriasTrespass <- treino %>%  
  group_by(Category, dia_semana) %>%
  summarise(Crimes = n()) %>%
  filter(Category %in% c("TRESPASS"))
ggplot(categoriasTrespass, aes(x = dia_semana, y = Crimes ) ) +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+
  geom_bar(stat = 'identity')
##########################################################################################################
#.Questão GR1-10:No respeitante aos crimes de Mandatos de Captura(Warrants) 
# há alguma característica interessante que se destaque?Justifique e conclua.
##########################################################################################################

categoriasTrespass <- treino %>%  
  group_by(Category, hora) %>%
  summarise(Crimes = n()) %>%
  filter(Category %in% c("WARRANTS"))

ggplot(categoriasTrespass, aes(x = hora, y = Crimes ) ) + 
  geom_bar(stat = 'identity')

categoriasTrespass <- treino %>%  
  group_by(Category, mes) %>%
  summarise(Crimes = n()) %>%
  filter(Category %in% c("WARRANTS"))

ggplot(categoriasTrespass, aes(x = mes, y = Crimes ) ) + 
  geom_bar(stat = 'identity')

categoriasTrespass <- treino %>%  
  group_by(Category, Ano) %>%
  summarise(Crimes = n()) %>%
  filter(Category %in% c("WARRANTS"))

ggplot(categoriasTrespass, aes(x = Ano, y = Crimes ) ) + 
  geom_bar(stat = 'identity')

categoriasTrespass <- treino %>%  
  group_by(Category, dia_semana) %>%
  summarise(Crimes = n()) %>%
  filter(Category %in% c("WARRANTS"))

ggplot(categoriasTrespass, aes(x = reorder(dia_semana, -Crimes), y = Crimes ) ) + 
  geom_bar(stat = 'identity')


View(categoriasTrespass)
##########################################################################################################
#H.#Haverá alguma relação entre as categorias e os locais físicos onde mais se densificam? 
#  #Tome, como exemplo (não único) a categoria "Conduta Desordeira"
#ver 
##########################################################################################################
categoriasArea <- treino %>%  
  group_by(Category, PdDistrict) %>% 
  summarise(Crimes = n(), x=mean(X), y=mean(Y)) 
View(categoriasArea)

#Relação Categoria Area
categoriasArea <- categoriasArea %>% mutate(densifica=as.numeric(ifelse(Crimes==max(Crimes),Crimes,0))) %>% 
  filter(densifica!=0)%>%
  select(PdDistrict,Category,Crimes,x,y)
View(categoriasArea)


plt <- ggplot(categoriasArea, aes(x=reorder(Category, -Crimes), y=Crimes, fill = Crimes ))+geom_bar(stat = "identity")+
  #coord_flip()+
  labs(x = '', y = 'Número total de crimes', title = 'categorias e os locais físicos onde mais se densificam')+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
plt + facet_wrap(~ PdDistrict, scales = "free" )


gg <- ggplot(categoriasArea, aes(x = PdDistrict, y = Crimes, fill = Category, label = Category ) ) + 
  scale_y_continuous(trans='log2')+
  geom_bar(stat = 'identity')+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))#+
#geom_text(size = 3, position = position_dodge(width = 1))
ggplotly(gg)


sf = get_map(location = "san francisco", maptype = "terrain", source = "google", zoom = 13)
map = ggmap(sf)


map +
  geom_point(data = categoriasArea, aes(x = x, y = y, size = Crimes, color = Category)) + 
  scale_size(name = '# Crimes', range = c(3,12)) +
  ggtitle('Locais onde Densificam os crimes')


##########################################################################################################
#I.Analise os TOP3 crimes e a sua localização geográfica. 
#Haverá alguma correlação entre estes crimes? E se o TOP3 for TOP5 ? 
#Que alterações verifica?
##########################################################################################################
TOPcategorias <- treino %>%  
  group_by(Category) %>% 
  summarise(Crimes = n(), x=mean(X), y=mean(Y)) %>%
  top_n(5, Crimes)

View(TOPcategorias)

#map <- get_map(location = "San Francisco", maptype = "terrain", source = "google", zoom = 13)
map <- qmap('San Francisco', zoom = 12, maptype = 'terrain')

map +
  geom_point(data = TOPcategorias, aes(x = x, y = y, size = Crimes, color = Category)) + 
  scale_size(name = '# Crimes', range = c(3,12)) +
  ggtitle('Locais dos Top 5 Crimes')+
  theme(legend.position="bottom")


p <- ggmap(get_googlemap(center = c(lon = -122.335167, lat = 47.608013),
                         zoom = 11, scale = 2,
                         maptype ='terrain',
                         color = 'color'))
p + geom_point(aes(x = x, y = x,  colour = Category), data = TOPcategorias, size = 0.5) + 
  theme(legend.position="bottom")


#Correlação
##Por dia
Top5CrimesDia <- treino %>%  
  group_by(dia, Category) %>% 
  summarise(crimes_dia = n()) %>% 
  filter(Category %in% c("ASSAULT", "DRUG/NARCOTIC", "LARCENY/THEFT", "NON-CRIMINAL","OTHER OFFENSES"))


#Correlação
CorrelacaoTop5 <- Top5CrimesDia %>% mutate(
  ASSAULT=as.numeric(ifelse(Category=="ASSAULT",crimes_dia,0)),
  DRUGNARCOTIC=as.numeric(ifelse(Category=="DRUG/NARCOTIC",crimes_dia,0)), 
  LARCENYTHEFT=as.numeric(ifelse(Category=="LARCENY/THEFT",crimes_dia,0)),
  NONCRIMINAL=as.numeric(ifelse(Category=="NON-CRIMINAL",crimes_dia,0)), 
  OTHEROFFENSES=as.numeric(ifelse(Category=="OTHER OFFENSES",crimes_dia,0))) %>% 
  select(dia,ASSAULT,DRUGNARCOTIC,LARCENYTHEFT,NONCRIMINAL,OTHEROFFENSES)%>%group_by(dia)%>%
  summarise(ASSAULT = sum(ASSAULT), 
            DRUGNARCOTIC = sum(DRUGNARCOTIC), 
            LARCENYTHEFT = sum(LARCENYTHEFT), 
            NONCRIMINAL = sum(NONCRIMINAL), 
            OTHEROFFENSES = sum(OTHEROFFENSES) ) 
View(CorrelacaoTop5)

cor(CorrelacaoTop5[2:6])
pairs.panels(CorrelacaoTop5[2:6])

##########################################################################################################
#K.Existe alguma hora do dia em que a taxa de crimes seja menor? @SA
##########################################################################################################
categorias_hora <- treino %>%  
  group_by(Category, hora) %>%
  summarise(Crimes = n()) %>%
  mutate(percentagem=round(Crimes/sum(Crimes)*100, 4))
View(categorias_hora)

ggplot(categorias_hora, aes(x=hora, y=Crimes)) +
  geom_density() +
  xlim(1, 13875)

ggplot(categorias_hora, aes(x = hora, y = Crimes ) ) + 
  geom_bar(stat = 'identity')


##########################################################################################################
#L.Se se quiser analisar as zonas em que a categoria Prostituição é mais prevalente:
#.Questão GR2, GR4, GR6, GR8, GR10:Que tipo de ruas se encontram nessas zonas? Caracterize essas ruas, 
#relacionando-as com a categoria em causa.@CR
##########################################################################################################
View(treino)

categoriasArea <- treino %>%  
  group_by(Category, PdDistrict)%>%
  summarise(total =n())%>%
  filter(Category =="PROSTITUTION")

ggplot(categoriasArea, aes(x = reorder(PdDistrict, total), y = total ) ) + 
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+
  labs(x = '', y = '# Prostituição', title = 'Prostituição por distrito')+
  geom_bar(stat = 'identity')


categoriasArea <- treino %>%  
  group_by(PdDistrict,Category,X,Y)%>%
  summarise()%>%
  #filter(Category =="PROSTITUTION" & PdDistrict %in% c("MISSION", "NORTHERN","TENDERLOIN"))
  filter(Category =="PROSTITUTION" & PdDistrict %in% c("MISSION"))

  
View(categoriasArea)

map <- qmap('San Francisco, MISSION DISTRICT', zoom = 16, maptype = 'terrain')
map +
  geom_point(data = categoriasArea, aes(x = X, y = Y)) 

map <- qmap('San Francisco, TENDERLOIN', zoom = 16, maptype = 'terrain')
map +
  geom_point(data = categoriasArea, aes(x = X, y = Y, colour = PdDistrict)) 


View(unique(treino$Address))

##########################################################################################################
#.Questão GR1-10: As ruas mais prevalentes são ruas principais ou secundárias? 
#Haverá alguma relação com a categoria em causa?
##########################################################################################################

categoriasArea <- treino %>%  
  group_by( PdDistrict, tipo_rua)%>%
  summarise(total =n())%>%
  filter(Category =="PROSTITUTION")

p4 <- ggplot() + 
  geom_bar(aes(y = total, x = reorder(tipo_rua, total), fill = PdDistrict), data = categoriasArea,stat="identity")+
  labs(x = '', y = '# Total', title = 'Prostituição por Tipo Rua e Zona')
p4


CrimesDia <- treino %>%  
  group_by( Ano, dia, Category) %>%
  summarise(crimes_dia = n())
View(CrimesDia)

ggplot(CrimesDia, aes(x = reorder(dia, crimes_dia), y = crimes_dia))+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +geom_boxplot()+
  scale_x_discrete(name = "Categoria Crime") +
  scale_y_continuous(name = "Crimes por dia")



#######################
### G.Haverá alguma relação entre os crimes de NarcoTráfico (Drug/Narcotic) e Prostituição (Prostitution)?

CrimesDP <- treino %>%  
  group_by(dia_semana, Category) %>%
  summarise(crimes = n()) %>%
  filter(Category %in% c("DRUG/NARCOTIC", "PROSTITUTION"))
View(CrimesDP)

CorrelacaoDP <- CrimesDP %>% mutate(DRUGNARCOTIC= as.numeric(ifelse(Category =="DRUG/NARCOTIC",crimes,0)), 
                                    PROSTITUTION=as.numeric(ifelse(Category=="PROSTITUTION",crimes,0))) %>%
  select(dia_semana, DRUGNARCOTIC, PROSTITUTION)%>%
  group_by(dia_semana)%>%
  summarise(DRUGNARCOTIC = sum(DRUGNARCOTIC), PROSTITUTION = sum(PROSTITUTION)) 

pairs.panels(CorrelacaoDP[2:3])


#Correlação por dia da semana
CrimesDP <- treino %>%  
  group_by(dia_semana, Category) %>%
  summarise(crimes_semana = n()) %>%
  filter(Category %in% c("DRUG/NARCOTIC", "PROSTITUTION"))


CorrelacaoDP <- CrimesDP %>% mutate(DRUGNARCOTIC= as.numeric(ifelse(Category=="DRUG/NARCOTIC",crimes_semana,0)), 
                                    PROSTITUTION=as.numeric(ifelse(Category=="PROSTITUTION",crimes_semana,0))) %>%
  select(dia_semana, DRUGNARCOTIC, PROSTITUTION)%>%
  group_by(dia_semana)%>%
  summarise(DRUGNARCOTIC = sum(DRUGNARCOTIC), PROSTITUTION = sum(PROSTITUTION))

pairs.panels(CorrelacaoDP[2:3])



treino %>%filter(.,Category %in% c("DRUG/NARCOTIC", "PROSTITUTION"))%>%  
  group_by(PdDistrict, Category) %>%
  #summarise(crimes = n()) %>%
  tally()%>%
  ggplot(.,aes(x=PdDistrict,y=n,color=Category))+
  geom_line(aes(group=Category))+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+
  scale_x_discrete(name = "Zona") +
  scale_y_continuous(name = "Registos")+
  geom_point()



treino$dia_semana <- factor(treino$dia_semana, levels = c("segunda-feira","terça-feira","quarta-feira","quinta-feira","sexta-feira", "sábado","domingo"))
treino %>%filter(.,Category %in% c("DRUG/NARCOTIC", "PROSTITUTION"))%>%  
  group_by(dia_semana, Category) %>%
  #summarise(crimes = n()) %>%
  tally()%>%
  ggplot(.,aes(x=dia_semana,y=n,color=Category))+
  geom_line(aes(group=Category))+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+
  scale_x_discrete(name = "Dia Semana") +
  scale_y_continuous(name = "Registos")+
  geom_point()






