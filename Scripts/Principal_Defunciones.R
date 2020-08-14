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
library(readxl)

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

Edad3 <- Grupo$edad ## Solo la edad

colnames(Grupo)

#Tablas
TabEdad3 = table(Grupo$edad)
TabEdad3
tablaCanton=table(Grupo$cant_fall)
tablaCanton #Ocurrencia de defunciones por canton
tablaSexo=table(Grupo$sexo)
tablaSexo


#Diagrama de barras
  #Freq de defunciones por edad
barplot(TabEdad3, main="Freq. defunciones por edad" , xlab="Edad")
  #Histogram
#Por edad
hist(Grupo$edad, breaks = "Sturges", right = T, main = "Histogram de defunciones por Edad",
     xlab = "Edad") #Usamos la regla de Sturges para las clases
tablaFreq=table.freq(hist(Grupo$edad, breaks = "Sturges", right = T))
tablaFreq
#Por canton
barplot(tablaCanton, main="Freq. defunciones por Canton" , xlab="Sexo")
MaxDefCanton = mlv(Grupo$cant_fall , method = "mfv") #Canton con Maxima de fallecidos
tablaCanton[MaxDefCanton] #Canton con Maxima de fallecidos
#Por Sexo
barplot(tablaSexo, main="Freq. defunciones por Sexo en el Guayas" , xlab="Sexo")

  #Frecuencias relativas
tablaReltv3 <- fdt(Grupo$edad, breaks = "Sturges")
tablaReltv3
  #Densidad
hist(Grupo$edad, breaks = "Sturges",  main = "Histogram de defunciones por Edad", freq = FALSE, plot = F)
lines(density(Grupo$edad), col="blue", lwd=3) # dibujamos la distribuci?n normal emp?rica con los datos que tenemos
lines(density(Grupo$edad, adjust=2), col="red", lwd=3, lty=2) #suavizar la curva con adjust entre 1 a 5
abline(v=mean(Grupo$edad), lwd=2, lty=3, col="darkblue") 
  #Ojiva (Distribucion de frecuencia acumulada)
tablaFreq
plot(tablaFreq$Main, cumsum(tablaFreq$Frequency), main="Ojiva de frecuencia dist", 
     xlab = "Marca de clase", ylab = "Frecuencia absoluta")
lines(tablaFreq$Main,cumsum(tablaFreq$Frequency), type="l")
  #Diagrama de Cajas(Edad por sexo)
boxplot(Grupo$edad ~ Grupo$sexo, horizontal = T, main="Diagrama de Cajas Defunciones por edad
        y sexo", xlab = "Edad", ylab = "Sexo", col=c("lightblue", "pink"))

#Medidas de tendencia central y dispersión
mediaEdad <- mean(Grupo$edad)
medianaEdad <- median(Grupo$edad)
MediaEdadc <- mean(Grupo$edad, trim=0.1) ##Eliminamos los recien nacidos fallecidos y viejos
MediaEdadc
ModadEdad <- mlv(Grupo$edad, method = "mfv")
ModadEdad    #La edad más frecuente de muertes en Guayas sin Guayaquil
TablaGeneral3 = table(Grupo$edad, Grupo$sexo) #Ocurrencia de edad por Sexo
TablaGeneral3
#Varianza y desviacion estandar
obs3=sort(Edad3)
quantile(obs3)
VarEdad <-var(obs3)
DesvEdad <- sd(Edad3)
sesgoEdad <- skewness(obs3)

#Estandarizacion de la muestra
Y3 = (Edad3 - mediaEdad)/DesvEdad

#coeficiente de variación por edad
cvEdad=(DesvEdad/mediaEdad)*100 
cvEdad

##Diagrama de Cajas, se puede observar valores atípicos en relación a los recien nacidos
boxplot(Edad3, horizontal = TRUE, main="Diagrama de Cajas",col="lightblue")
abline(h=365, col="red", lw=2)

tablaReltv3
plot(tablaReltv3, type="cfp") #poligono de frecuencias acumulado
plot(tablaReltv3, type="rfh") #histograma de frecuencias relativa



###Grupo de edades: 13

##Grupo de edad 1: Hombres y mujeres fallecidoss menores de 10 años(Niños)
#Fallecimiento por Canton sin Guayaquil
Grupo1 <- data %>%#Sin Guayaquil
  filter(! cant_fall %in% DataGquil$cant_fall,
         between(edad,0,10)) %>% 
  group_by(cant_fall) 

Edad31 <- Grupo1$edad ## Solo la edad

#Tablas1
TabEdad31 = table(Grupo1$edad)
TabEdad31

#Diagrama de barras1
#Freq de defunciones por edad1
barplot(TabEdad31, main="Freq. defunciones por edad" , xlab="Edad")
#Histogram
hist(Grupo1$edad, breaks = "Sturges", main = "Histogram de defunciones en menores
     de 10 años por Edad",
     xlab = "Edad") #Usamos la regla de Sturges para las clases










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
tablaSexoCanton = table(Grupo$edad, Grupo$sexo)
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




