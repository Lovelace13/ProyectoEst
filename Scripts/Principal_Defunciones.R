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

#Data solo de Guayaquil (subset(data, edad == 999 , "edad") => para 1 variable edad)
DataGquil <- subset(data, data$cant_fall == 'Guayaquil')
DataGquil
colnames(DataGquil)

#Data sin informacion de edad
DataSinEdad <- subset(data, data$edad == 999)

#En general Sin Guayaquil y datos sin informacion en edad
Grupo <- data %>%#Sin Guayaquil
  filter(! cant_fall %in% DataGquil$cant_fall) %>% 
  group_by(cant_fall)

Grupo <- Grupo %>%#Sin edad
  filter(! edad %in% DataSinEdad$edad) %>% 
  group_by(cant_fall)

colnames(Grupo)

#Tablas
TabEdad3 = table(Grupo$edad)
TabEdad3
TablaGeneral3 = table(Grupo$edad, Grupo$sexo) #Tabla x:edad y:cantidad de muertos
TablaGeneral3

#Diagrama de barras
  #Freq de defunciones por edad
barplot(TabEdad3, main="Freq. defunciones por edad" , xlab="Edad")
  #Histogram
hist(Grupo$edad, breaks = "Sturges", main = "Histogram de defunciones por Edad",
     xlab = "Edad") #Usamos la regla de Sturges para las clases
tablaFreq=table.freq(hist(Grupo$edad, breaks = "Sturges"))
tablaFreq
  #Frecuencias relativas
tablaReltv3 <- fdt(Grupo$edad, breaks = "Sturges")
tablaReltv3
  #Densidad
hist(Grupo$edad, breaks = "Sturges",  main = "Histogram de defunciones por Edad", freq = FALSE)
lines(density(Grupo$edad), col="blue", lwd=3) # dibujamos la distribuci?n normal emp?rica con los datos que tenemos
lines(density(Grupo$edad, adjust=2), col="red", lwd=3, lty=2) #suavizar la curva con adjust entre 1 a 5
abline(v=mean(Grupo$edad), lwd=2, lty=3, col="darkblue") 

plot(tablaR1, type="cfp") #poligono de frecuencias acumulado
plot(tablaR1, type="rfh") #histograma de frecuencias relativa
plot(tablaR1, type="fh")



##Grupo de edad 1: Hombres y mujeres fallecidoss menores de 10 años

#Fallecimiento por Canton sin Guayaquil

Grupo1 <- data %>%#Sin Guayaquil
  filter(! cant_fall %in% DataGquil$cant_fall,
         between(edad,0,10)) %>% 
  group_by(cant_fall) 

#Tabla de frecuencias para las variables sexo y Canton de fallecimiento
tablaSexo=table(Grupo1$sexo)
tablaSexo
tablaSexFrq=prop.table(tablaSexo) #frecuencias relativas de defunciones por sexo
tablaSexFrq
tablaFemen=table(Grupo1$sexo, exclude = c("Hombre")) #tabla solo Masculino
tablaFemen
tablaMasc=table(Grupo1$sexo, exclude = c("Mujer")) #tabla solo Masculino
tablaMasc
tablaCanton=table(Grupo1$cant_fall)
tablaCanton
tablaSexoCanton = table(Grupo1$cant_fall, Grupo1$sexo)
tablaSexoCanton


#Diagrama de barras
barplot(tablaSexo, main="Diagrama de barras defunciones por sexo" , xlab="Sexo")

data %>%#Con Guayaquil
  group_by(cant_fall) %>% 
  filter(between(edad,0,10)) %>% 
  summarise(Total = n())

#Fallecimiento por Canton
Grupo1 %>%
  group_by(cant_fall) %>% 
  filter(between(edad,0,10)) %>% 
  summarise(Total = n())

##Fallecimiento por sexo
Grupo1 %>%
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




