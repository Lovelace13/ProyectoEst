library(agricolae)
library(dplyr)
library(tidyverse)
library(fdth) #(para el calculo de distribución de frecuencias.)
library(DescTools)
library(cumstats)
library(gapminder)
library(modeest)
library(tidyverse)
library(openxlsx)

#dir.create("data")
#dir.create("Exports")
#dir.create("Scripts")

data <- read.xlsx(xlsxFile="data/DefuncionesGenerales_Datos.xlsx")
dim(data) ##Filas y columnas
object.size(data) ##espacio de data ~ 8.39 mb
colnames(data) ## nombre de columnas
str(data) ##Tipo de dato
#subset(data, anio_insc == 2016, "prov_insc") 
head(data,5)

##1. Análisis comparativo por meses, sexo y estados civil del número de fallecidos


##2. Resumen de las 10 mayores causas de muerte de manera general y por sexo.


###3. Análisis comparativo de grupos de edades de fallecimiento por sexo 
###  y por cantón de fallecimiento a excepción de Guayaquil.

#Data solo de Guayaquil
NewData <- subset(data, data$cant_fall == 'Guayaquil')
NewData
CantonGqul <- subset(NewData, cant_fall == 'Guayaquil' , "cant_fall")
colnames(CantonGqul)
##Grupo de edad 1 
grupo1TV <- data[data$edad < 10,] ## menores de 10 años, TV:todas las variables
grupo1Sex <- subset(data, edad < 10, "sexo") 
subset(data, edad < 10, "edad") 
grupo1Edad <- subset(data, edad < 10, "edad")
colnames(grupo1Edad)

#Frecuencias
table(data$sexo) #general total hombres y mujeres
table(data$sexo, data$edad < 10) ##Total de Hombres y mujeres muertos menores de 10 años
table(data$cant_fall, data$edad < 10)##Total de individuos muertos menores de 10 años x canton5
table(data$cant_fall, data$sexo)
table(grupo1TV$cant_fall, grupo1TV$sexo)

#Fallecimiento por Canton

data %>%#Sin Guayaquil
  filter(!cant_fall %in% NewData$cant_fall,
         between(edad,0,10)) %>% 
  group_by(cant_fall) %>% 
  summarise(Total = n())

data %>%#Con Guayaquil
  group_by(cant_fall) %>% 
  filter(between(edad,0,10)) %>% 
  summarise(Total = n())

#Fallecimiento por sexo según Canton
NewData %>%
  group_by(cant_fall, sexo) %>% 
  filter(between(edad,0,10)) %>% 
  summarise(Total = n())

##Fallecimiento por sexo
data %>%
  group_by(sexo) %>% 
  filter(between(edad,0,10)) %>% 
  summarise(Total = n())


#Masculino Cantones

#Grupo de edad 2 Femenino Cantones
grupo2TV <- data %>%
  filter(between(edad,10,20),
         sexo == 'Mujer')
grupo2TV ## menores de 10 años, TV:todas las variables

data %>% 
  group_by(cant_fall) %>% 
  filter(between(edad,0,10),
         sexo == 'Mujer') %>% 
  summarise(conteo = n())

#Grupo de edad 3 Masculino Cantones

##4. Análisis comparativo de grupos de edades de fallecimiento por sexo en el cantón Guayaquil.




