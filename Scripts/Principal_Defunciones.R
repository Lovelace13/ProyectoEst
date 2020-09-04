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

colnames(Grupo)
Edad3 <- Grupo$edad ## Solo la edad
Edad3

#Tablas
TabEdad3 = table(Grupo$edad)
TabEdad3
tablaCanton=table(Grupo$cant_fall)
tablaCanton #Ocurrencia de defunciones por canton
tablaSexo=table(Grupo$sexo)
tablaSexo
tablaSexoCanton = table(Grupo$cant_fall, Grupo$sexo)
tablaSexoCantoN
tablaEdadSexo = table(Grupo$edad, Grupo$sexo)
tablaEdadSexo
tablaEdaSexM = table(Grupo$edad, Grupo$sexo, exclude = c("Mujer"))
tablaEdaSexF = table(Grupo$edad, Grupo$sexo, exclude = c("Hombre"))

#Diagrama de barras
  #Freq de defunciones por edad
barplot(TabEdad3, main="Diagrama de barras: Frecuencia de defunciones 
        por edad en el Guayas" , xlab="Edad")
  #Por canton
barplot(tablaCanton, main="Freq. defunciones por Canton" , xlab="Sexo")
MaxDefCanton = mlv(Grupo$cant_fall , method = "mfv") #Canton con Maxima de fallecidos
tablaCanton[MaxDefCanton] #Canton con Maxima de fallecidos
sort(tablaCanton)
SegMaxDef = "Daule"
tablaCanton[SegMaxDef]
  #Por Sexo
barplot(tablaSexo, main="Diagrama de barras: Frecuencia de defunciones 
        por Sexo en el Guayas a excepcion de Guayaquil" , xlab="Sexo", col=c("lightblue", "pink"))

#Histogram
  #Por edad
hist(Grupo$edad, breaks = "Sturges", right = T, main = "Histograma de defunciones por Edad",
     xlab = "Edad") #Usamos la regla de Sturges para las clases
histEdad = hist(Grupo$edad, breaks = "Sturges", right = T, plot = F)
histEdad
tablaFreq=table.freq(hist(Grupo$edad, breaks = "Sturges", right = T))
tablaFreq

#Frecuencias relativas
tablaReltv3 <- fdt(Grupo$edad, breaks = "Sturges", right = F)
tablaReltv3
  #Densidad
hist(Grupo$edad, breaks = "Sturges", main = "Histogram de defunciones por Edad", freq = FALSE)
lines(density(Grupo$edad), col="blue", lwd=3) # dibujamos la distribuci?n normal emp?rica con los datos que tenemos
lines(density(Grupo$edad, adjust=2), col="red", lwd=3, lty=2) #suavizar la curva con adjust entre 1 a 5
abline(v=mean(Grupo$edad), lwd=2, lty=3, col="darkblue") 
  #Ojiva (Distribucion de frecuencia acumulada)
tablaFreq
plot(tablaFreq$Main, cumsum(tablaFreq$Frequency), main="Ojiva de frecuencia dist", 
     xlab = "Marca de clase", ylab = "Frecuencia absoluta")

##Diagrama de Cajas, se puede observar valores atípicos en relación a los recien nacidos
boxplot(Grupo$edad, horizontal = TRUE, main="Diagrama de Caja de Defunciones por
        edad", xlab = "Edad", col="lightblue")


  #Diagrama de Cajas(Edad por sexo)
boxplot(Grupo$edad ~ Grupo$sexo, horizontal = T, main="Diagrama de Cajas Defunciones por edad
        y sexo", xlab = "Edad", ylab = "Sexo", col=c("lightblue", "pink"))

  #Diagrama de Cajas(Cantones con mas defunciones)
dataDauMil <- subset(Grupo, cant_fall == "Milagro" | cant_fall == "Daule", )
dataDauMil
boxplot(dataDauMil$edad ~ dataDauMil$cant_fall, horizontal = T, main="Diagrama de Cajas Defunciones
        en Daule y Milagro", xlab = "Edad", ylab = "Canton", col=c("lightblue", "grey"))

dataDauMil%>%  
  filter(cant_fall=="Milagro")-> Milagro  #A) Grupo de fallecidos en milagro

mean(Milagro$edad)

dataDauMil%>%  
  filter(cant_fall=="Daule")-> Daule  #A) Grupo de fallecidos en Daule

mean(Daule$edad)

#Medidas de tendencia central y dispersión Edad
mediaEdad <- mean(Grupo$edad)
mediaEdad
medianaEdad <- median(Grupo$edad)
medianaEdad
MediaEdadc <- mean(Grupo$edad, trim=0.05) ##Eliminamos los recien nacidos fallecidos y viejos
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
Y3
#coeficiente de variación por edad
cvEdad=(DesvEdad/mediaEdad)*100 
cvEdad

#Poligono de frecuencias y freq acumulada y freq absoluta 
plot(tablaReltv3, type="fp", main="Poligono de frecuencia", xlab = "Marca de clase" )

plot(tablaReltv3, type="cfp", main="Poligono de frecuencia acumulada", xlab = "Marca de clase")

plot(histEdad$mids, histEdad$counts, main="Poligono de frecuencia absoluta",
     xlab = "Marca de clase") 
lines(histEdad$mids,histEdad$counts, type="l")

  #Ojivas

plot(histEdad$mids,cumsum(histEdad$counts), main="Ojiva de frecuencia absoluta", 
     xlab = "Marca de clase")
lines(histEdad$mids,cumsum(histEdad$counts), type="l")

plot(histEdad$mids,cumsum(histEdad$counts)/sum(histEdad$counts), main="Ojiva de frecuencia relativa", xlab = "Marca de clase", 
     ylab = "Frecuencias relativas") 
lines(histEdad$mids,cumsum(histEdad$counts)/sum(histEdad$counts), type="l", )

#Medidas de tendencia central y dispersión Edad, sexo y canton

#Promedios de muerte por edad y sexo
tabMeanSexEda = aggregate(x = Grupo$edad,         
          by = list(Grupo$sexo),
          FUN = mean)
tabMeanSexEda

mediaEdaSexF <- (tabMeanSexEda %>%  
  filter(Group.1=="Mujer"))$x

mediaEdaSexF #Media de muertes femeninas
mediaEdaSexM <-(tabMeanSexEda %>%  
                  filter(Group.1=="Hombre"))$x
mediaEdaSexM #Media de muertes masculinas
MediaEdaSexc = aggregate(x = Grupo$edad,         
                         by = list(Grupo$sexo),
                         FUN = mean, trim=0.05)
MediaEdaSexcF <-(MediaEdaSexc %>%  
                   filter(Group.1=="Mujer"))$x ##Eliminamos los recien nacidos fallecidos y viejos
MediaEdaSexcF
MediaEdaSexcM <-(MediaEdaSexc %>%  
                   filter(Group.1=="Hombre"))$x
MediaEdaSexcM
#La edad más frecuente de muertes Femeninas en Guayas sin Guayaquil
v_logico = tablaEdaSexF==max(tablaEdaSexF) #Variable temporal
which(v_logico==TRUE)
ModadEdaSexF <- which(v_logico==TRUE)
ModadEdaSexF
#La edad más frecuente de muertes Masculinas en Guayas sin Guayaquil
v_logico = tablaEdaSexM==max(tablaEdaSexM) #Variable temporal
ModadEdaSexM <- which(v_logico==TRUE)
ModadEdaSexM



###Para este analisis solo se escogerán tres grupos de edades.

###Grupo de edades: 0 a 10 años - Grupo1

##Grupo de edad 1: Hombres y mujeres fallecidoss menores de 10 años(Niños)
#Fallecimiento por Canton sin Guayaquil
Grupo1 <- data %>%#Sin Guayaquil
  filter(! cant_fall %in% DataGquil$cant_fall,
         between(edad,0,10)) %>% 
  group_by(cant_fall) 

Grupo1

Edad31 <- Grupo1$edad ## Solo la edad

#Tablas1
TabEdad31 = table(Grupo1$edad)
TabEdad31
tablaCanton1=table(Grupo1$cant_fall)
tablaCanton1 #Ocurrencia de defunciones por canton
tablaSexo1=table(Grupo1$sexo)
tablaSexo1
tablaSexoCanton1 = table(Grupo1$cant_fall, Grupo1$sexo)
tablaSexoCantoN1
tablaEdadSexo1 = table(Grupo1$edad, Grupo1$sexo)
tablaEdadSexo1
tablaEdaSexM1 = table(Grupo1$edad, Grupo1$sexo, exclude = c("Mujer"))
tablaEdaSexF1 = table(Grupo1$edad, Grupo1$sexo, exclude = c("Hombre"))

#Diagrama de barras1
  #Freq de defunciones por edad
barplot(TabEdad31, main="Diagrama de barras: Frecuencia de defunciones 
        por edad en el Guayas" , xlab="Edad")
  #Por canton
barplot(tablaCanton1, main="Freq. defunciones por Canton grupo 1" , xlab="Sexo")
MaxDefCanton1 = mlv(Grupo1$cant_fall , method = "mfv") #Canton con Maxima de fallecidos
MaxDefCanton1
tablaCanton1[MaxDefCanton1] #Canton con Maxima de fallecidos
sort(tablaCanton1)
SegMaxDef1 = "Duran"
tablaCanton1[SegMaxDef1]
  #Por Sexo
barplot(tablaSexo1, main="Freq de muertes por Sexo: 0 - 10 años en el Guayas" , xlab="Sexo", col=c("lightblue", "pink"))

#Histogram
#Por edad
hist(Grupo1$edad, breaks = "Sturges", right = T, main = "Histograma de defunciones grupo 1",
     xlab = "Edad") #Usamos la regla de Sturges para las clases
histEdad1 = hist(Grupo1$edad, breaks = "Sturges", right = T, plot = F)
histEdad1
tablaFreq1=table.freq(hist(Grupo1$edad, breaks = "Sturges", right = T))
tablaFreq1

#Frecuencias relativas
tablaReltv31 <- fdt(Grupo1$edad, breaks = "Sturges", right = F)
tablaReltv31
#Densidad
hist(Grupo1$edad, breaks = "Sturges",  main = "Histogram de defunciones por Edad grupo 1", freq = FALSE)
lines(density(Grupo1$edad), col="blue", lwd=3) # dibujamos la distribuci?n normal emp?rica con los datos que tenemos
lines(density(Grupo1$edad, adjust=2), col="red", lwd=3, lty=2) #suavizar la curva con adjust entre 1 a 5
abline(v=mean(Grupo1$edad), lwd=2, lty=3, col="darkblue") 

#Ojiva (Distribucion de frecuencia acumulada)
tablaFreq1
plot(tablaFreq1$Main, cumsum(tablaFreq1$Frequency), main="Ojiva de frecuencia acumulada Grupo1", 
     xlab = "Marca de clase: Edad", ylab = "Frecuencia absoluta")
lines(tablaFreq1$Main,cumsum(tablaFreq1$Frequency), type="l")

##Diagrama de Cajas, se puede observar valores atípicos en relación a los recien nacidos
boxplot(Grupo1$edad, horizontal = TRUE, main="Diagrama de Caja de Defunciones por
        edad", xlab = "Edad", col="lightblue")


#Diagrama de Cajas(Edad por sexo)
boxplot(Grupo1$edad ~ Grupo1$sexo, horizontal = T, main="Diagrama de Cajas Defunciones por edad
        y sexo", xlab = "Edad", ylab = "Sexo", col=c("lightblue", "pink"))

#Diagrama de Cajas(Cantones con mas defunciones)
dataDurMil <- subset(Grupo1, cant_fall == "Milagro" | cant_fall == "Daule", )
dataDurMil
boxplot(dataDauMil$edad ~ dataDauMil$cant_fall, horizontal = T, main="Diagrama de Cajas Defunciones
        en Daule y Milagro", xlab = "Edad", ylab = "Canton", col=c("lightblue", "grey"))

#dataDurMil%>%  
#  filter(cant_fall=="Milagro")-> Milagro  #A) Grupo de fallecidos en milagro

#mean(Milagro$edad)

#dataDurMil%>%  
#  filter(cant_fall=="Daule")-> Duran  #A) Grupo de fallecidos en Daule

#mean(Duran$edad)

#Medidas de tendencia central y dispersión Edad
mediaEdad1 <- mean(Grupo1$edad)
mediaEdad1
medianaEdad1 <- median(Grupo1$edad)
medianaEdad1
MediaEdadc1 <- mean(Grupo1$edad, trim=0.05) ##Eliminamos los recien nacidos fallecidos y viejos
MediaEdadc1
ModadEdad1 <- mlv(Grupo1$edad, method = "mfv")
ModadEdad1    #La edad más frecuente de muertes en Guayas sin Guayaquil
TablaGeneral31 = table(Grupo1$edad, Grupo1$sexo) #Ocurrencia de edad por Sexo
TablaGeneral31

#Varianza y desviacion estandar
obs31=sort(Edad31)
quantile(obs31)
VarEdad1 <-var(obs31)
DesvEdad1 <- sd(Edad31)
sesgoEdad1 <- skewness(obs31)

#Estandarizacion de la muestra
Y31 = (Edad31 - mediaEdad1)/DesvEdad1
Y31
#coeficiente de variación por edad
cvEdad1=(DesvEdad1/mediaEdad1)*100 
cvEdad1

#Poligono de frecuencias y freq acumulada y freq absoluta 
plot(tablaReltv31, type="fp", main="Poligono de frecuencia G1", xlab = "Marca de clase" )

plot(tablaReltv31, type="cfp", main="Poligono de frecuencia acumulada G1", xlab = "Marca de clase")

plot(histEdad1$mids, histEdad1$counts, main="Poligono de frecuencia absoluta G1",
     xlab = "Marca de clase") 
lines(histEdad1$mids,histEdad1$counts, type="l")

#Ojivas

plot(histEdad1$mids,cumsum(histEdad1$counts), main="Ojiva de frecuencia absoluta G1", 
     xlab = "Marca de clase")
lines(histEdad1$mids,cumsum(histEdad1$counts), type="l")

plot(histEdad1$mids,cumsum(histEdad1$counts)/sum(histEdad1$counts), main="Ojiva de frecuencia relativa G1", xlab = "Marca de clase", 
     ylab = "Frecuencias relativas") 
lines(histEdad1$mids,cumsum(histEdad1$counts)/sum(histEdad1$counts), type="l", )

#Medidas de tendencia central y dispersión Edad, sexo y canton

#Promedios de muerte por edad y sexo
tabMeanSexEda1 = aggregate(x = Grupo1$edad,         
                          by = list(Grupo1$sexo),
                          FUN = mean)
tabMeanSexEda1

mediaEdaSexF1 <- (tabMeanSexEda1 %>%  
                   filter(Group.1=="Mujer"))$x

mediaEdaSexF1 #Media de muertes femeninas
mediaEdaSexM1 <-(tabMeanSexEda1 %>%  
                  filter(Group.1=="Hombre"))$x
mediaEdaSexM1 #Media de muertes masculinas
MediaEdaSexc1 = aggregate(x = Grupo1$edad,         
                         by = list(Grupo1$sexo),
                         FUN = mean, trim=0.1)
MediaEdaSexcF1 <-(MediaEdaSexc1 %>%  
                   filter(Group.1=="Mujer"))$x ##Eliminamos los recien nacidos fallecidos y viejos
MediaEdaSexcF1
MediaEdaSexcM1 <-(MediaEdaSexc1 %>%  
                   filter(Group.1=="Hombre"))$x
MediaEdaSexcM1
#La edad más frecuente de muertes Femeninas en Guayas sin Guayaquil
v_logico = tablaEdaSexF1==max(tablaEdaSexF1) #Variable temporal
which(v_logico==TRUE)
ModadEdaSexF1 <- which(v_logico==TRUE)
ModadEdaSexF1
#La edad más frecuente de muertes Masculinas en Guayas sin Guayaquil
v_logico = tablaEdaSexM1==max(tablaEdaSexM1) #Variable temporal
ModadEdaSexM1 <- which(v_logico==TRUE)
ModadEdaSexM1


###Grupo de edades: 60 a 80 años - Grupo2

##Grupo de edad 1: Hombres y mujeres fallecidoss entre 60 y 80 años(Adultos mayores)
#Fallecimiento por Canton sin Guayaquil
Grupo2 <- data %>%#Sin Guayaquil
  filter(! cant_fall %in% DataGquil$cant_fall,
         between(edad,60,80)) %>% 
  group_by(cant_fall) 

Grupo2

Edad32 <- Grupo2$edad ## Solo la edad

#Tablas2
TabEdad32 = table(Grupo2$edad)
TabEdad32
tablaCanton2=table(Grupo2$cant_fall)
tablaCanton2 #Ocurrencia de defunciones por canton
tablaSexo2=table(Grupo2$sexo)
tablaSexo2
tablaSexoCanton2 = table(Grupo2$cant_fall, Grupo2$sexo)
tablaSexoCantoN2
tablaEdadSexo2 = table(Grupo2$edad, Grupo2$sexo)
tablaEdadSexo2
tablaEdaSexM2 = table(Grupo2$edad, Grupo2$sexo, exclude = c("Mujer"))
tablaEdaSexF2 = table(Grupo2$edad, Grupo2$sexo, exclude = c("Hombre"))

#Diagrama de barras2
#Freq de defunciones por edad
barplot(TabEdad32, main="Diagrama de barras: Frecuencia de defunciones 
        por edad en el Guayas" , xlab="Edad")
#Por canton
barplot(tablaCanton2, main="Freq. defunciones por Canton grupo 2" , xlab="Sexo")
MaxDefCanton2 = mlv(Grupo1$cant_fall , method = "mfv") #Canton con Maxima de fallecidos
MaxDefCanton2
tablaCanton1[MaxDefCanton2] #Canton con Maxima de fallecidos
sort(tablaCanton2)
#SegMaxDef2 = "Daule"
#tablaCanton2[SegMaxDef1] #Me muestra el segundo canton con mas fallecidos
#Por Sexo
barplot(tablaSexo2, main="Freq de muertes por Sexo: 60 - 80 años en el Guayas" , xlab="Sexo", col=c("lightblue", "pink"))

#Histogram
#Por edad
hist(Grupo2$edad, breaks = "Sturges", right = T, main = "Histograma de defunciones Edad grupo 2",
     xlab = "Edad") #Usamos la regla de Sturges para las clases
histEdad2 = hist(Grupo2$edad, breaks = "Sturges", right = T, plot = F)
histEdad2
tablaFreq2=table.freq(hist(Grupo2$edad, breaks = "Sturges", right = T))
tablaFreq2

#Frecuencias relativas
tablaReltv32 <- fdt(Grupo2$edad, breaks = "Sturges", right = F)
tablaReltv32
#Densidad
hist(Grupo2$edad, breaks = "Sturges",  main = "Histogram de defunciones por Edad grupo 2", freq = FALSE)
lines(density(Grupo2$edad), col="blue", lwd=3) # dibujamos la distribuci?n normal emp?rica con los datos que tenemos
lines(density(Grupo2$edad, adjust=2), col="red", lwd=3, lty=2) #suavizar la curva con adjust entre 1 a 5
abline(v=mean(Grupo2$edad), lwd=2, lty=3, col="darkblue") 

#Ojiva (Distribucion de frecuencia acumulada)
tablaFreq2
plot(tablaFreq2$Main, cumsum(tablaFreq2$Frequency), main="Ojiva de frecuencia acumulada", 
     xlab = "Marca de clase", ylab = "Frecuencia absoluta")
lines(tablaFreq2$Main,cumsum(tablaFreq2$Frequency), type="l")

##Diagrama de Cajas: Defunciones por edad
boxplot(Grupo2$edad, horizontal = TRUE, main="Diagrama de Caja de Defunciones por
        edad", xlab = "Edad", col="lightblue")


#Diagrama de Cajas(Edad por sexo)
boxplot(Grupo2$edad ~ Grupo2$sexo, horizontal = T, main="Diagrama de Cajas Defunciones por edad
        y sexo", xlab = "Edad", ylab = "Sexo", col=c("lightblue", "pink"))

#Diagrama de Cajas(Cantones con mas defunciones)
dataDauMil2 <- subset(Grupo2, cant_fall == "Milagro" | cant_fall == "Daule", )
dataDauMil2
boxplot(dataDauMil2$edad ~ dataDauMil2$cant_fall, horizontal = T, main="Diagrama de Cajas Defunciones
        en Daule y Milagro Grupo 2", xlab = "Edad", ylab = "Canton", col=c("lightblue", "grey"))

#dataDauMil2%>%  
#  filter(cant_fall=="Milagro")-> Milagro  #A) Grupo de fallecidos en milagro

#mean(Milagro$edad)

#dataDauMil%>%  
#  filter(cant_fall=="Daule")-> Daule  #A) Grupo de fallecidos en Daule

#mean(Daule$edad)

#Medidas de tendencia central y dispersión Edad
mediaEdad2 <- mean(Grupo2$edad)
mediaEdad2
medianaEdad2 <- median(Grupo2$edad)
medianaEdad2
MediaEdadc2 <- mean(Grupo2$edad, trim=0.05) ##Eliminamos los recien nacidos fallecidos y viejos
MediaEdadc2
ModadEdad2 <- mlv(Grupo2$edad, method = "mfv")
ModadEdad2    #La edad más frecuente de muertes en Guayas sin Guayaquil
TablaGeneral32 = table(Grupo2$edad, Grupo2$sexo) #Ocurrencia de edad por Sexo
TablaGeneral32

#Varianza y desviacion estandar
obs32=sort(Edad32)
quantile(obs32)
VarEdad2 <-var(obs32)
DesvEdad2 <- sd(Edad32)
sesgoEdad2 <- skewness(obs32)

#Estandarizacion de la muestra
Y32 = (Edad32 - mediaEdad2)/DesvEdad2
Y32
#coeficiente de variación por edad
cvEdad2 = (DesvEdad2/mediaEdad2)*100 
cvEdad2

#Poligono de frecuencias y freq acumulada y freq absoluta 
plot(tablaReltv32, type="fp", main="Poligono de frecuencia G2", xlab = "Marca de clase" )

plot(tablaReltv32, type="cfp", main="Poligono de frecuencia acumulada G2", xlab = "Marca de clase")

plot(histEdad2$mids, histEdad2$counts, main="Poligono de frecuencia absoluta G2",
     xlab = "Marca de clase") 
lines(histEdad2$mids,histEdad2$counts, type="l")

#Ojivas

plot(histEdad2$mids,cumsum(histEdad2$counts), main="Ojiva de frecuencia absoluta G2", 
     xlab = "Marca de clase")
lines(histEdad2$mids,cumsum(histEdad2$counts), type="l")

plot(histEdad2$mids,cumsum(histEdad2$counts)/sum(histEdad2$counts), main="Ojiva de frecuencia relativa G2", xlab = "Marca de clase", 
     ylab = "Frecuencias relativas") 
lines(histEdad2$mids,cumsum(histEdad2$counts)/sum(histEdad2$counts), type="l", )

#Medidas de tendencia central y dispersión Edad, sexo y canton

#Promedios de muerte por edad y sexo
tabMeanSexEda2 = aggregate(x = Grupo2$edad,         
                           by = list(Grupo2$sexo),
                           FUN = mean)
tabMeanSexEda2

mediaEdaSexF2 <- (tabMeanSexEda2 %>%  
                    filter(Group.1=="Mujer"))$x

mediaEdaSexF2 #Media de muertes femeninas
mediaEdaSexM2 <-(tabMeanSexEda2 %>%  
                   filter(Group.1=="Hombre"))$x
mediaEdaSexM2 #Media de muertes masculinas
MediaEdaSexc2 = aggregate(x = Grupo2$edad,         
                          by = list(Grupo2$sexo),
                          FUN = mean, trim=0.1)
MediaEdaSexcF2 <-(MediaEdaSexc2 %>%  
                    filter(Group.1=="Mujer"))$x ##Eliminamos los recien nacidos fallecidos y viejos
MediaEdaSexcF2
MediaEdaSexcM2 <-(MediaEdaSexc2 %>%  
                    filter(Group.1=="Hombre"))$x
MediaEdaSexcM2
#La edad más frecuente de muertes Femeninas en Guayas sin Guayaquil
v_logico = tablaEdaSexF2==max(tablaEdaSexF2) #Variable temporal
v_logico
which(v_logico==TRUE)
ModadEdaSexF2 <- 77
ModadEdaSexF2
#La edad más frecuente de muertes Masculinas en Guayas sin Guayaquil
v_logico = tablaEdaSexM2==max(tablaEdaSexM2) #Variable temporal
v_logico
ModadEdaSexM2 <- 77
ModadEdaSexM2


###Grupo de edades: 80 a 100 años - Grupo3

##Grupo de edad 3: Hombres y mujeres fallecidoss entre 60 y 80 años(Adultos mayores)
#Fallecimiento por Canton sin Guayaquil
Grupo3 <- data %>%#Sin Guayaquil
  filter(! cant_fall %in% DataGquil$cant_fall,
         between(edad,80,100)) %>% 
  group_by(cant_fall) 

Grupo3

Edad33 <- Grupo3$edad ## Solo la edad

#Tablas2
TabEdad33 = table(Grupo3$edad)
TabEdad33
tablaCanton3=table(Grupo3$cant_fall)
tablaCanton3 #Ocurrencia de defunciones por canton
tablaSexo3 = table(Grupo3$sexo)
tablaSexo3
tablaSexoCanton3 = table(Grupo3$cant_fall, Grupo3$sexo)
tablaSexoCanton3
tablaEdadSexo3 = table(Grupo3$edad, Grupo3$sexo)
tablaEdadSexo3
tablaEdaSexM3 = table(Grupo3$edad, Grupo3$sexo, exclude = c("Mujer"))
tablaEdaSexF3 = table(Grupo3$edad, Grupo3$sexo, exclude = c("Hombre"))

#Diagrama de barras2
#Freq de defunciones por edad
barplot(TabEdad33, main="Diagrama de barras: Frecuencia de defunciones 
        por edad en el Guayas Gruo 3" , xlab="Edad")
#Por canton
barplot(tablaCanton3, main="Freq. defunciones por Canton grupo 3" , xlab="Canton")
MaxDefCanton3 = mlv(Grupo3$cant_fall , method = "mfv") #Canton con Maxima de fallecidos
MaxDefCanton3
tablaCanton3[MaxDefCanton3] #Canton con Maxima de fallecidos
sort(tablaCanton3)
#SegMaxDef2 = "Daule"
#tablaCanton2[SegMaxDef1] #Me muestra el segundo canton con mas fallecidos
#Por Sexo
barplot(tablaSexo3, main="Freq de muertes por Sexo: 80 - 100 años en el Guayas" , xlab="Sexo", col=c("lightblue", "pink"))

#Histogram
#Por edad
hist(Grupo3$edad, breaks = "Sturges", right = T, main = "Histograma de defunciones Edad grupo 3",
     xlab = "Edad") #Usamos la regla de Sturges para las clases
histEdad3 = hist(Grupo3$edad, breaks = "Sturges", right = T, plot = F)
histEdad3
tablaFreq3=table.freq(hist(Grupo3$edad, breaks = "Sturges", right = T))
tablaFreq3

#Frecuencias relativas
tablaReltv33 <- fdt(Grupo3$edad, breaks = "Sturges", right = F)
tablaReltv33
#Densidad
hist(Grupo3$edad, breaks = "Sturges",  main = "Histogram de defunciones por Edad grupo 3", freq = FALSE)
lines(density(Grupo3$edad), col="blue", lwd=3) # dibujamos la distribuci?n normal emp?rica con los datos que tenemos
lines(density(Grupo3$edad, adjust=2), col="red", lwd=3, lty=2) #suavizar la curva con adjust entre 1 a 5
abline(v=mean(Grupo3$edad), lwd=2, lty=3, col="darkblue") 

#Ojiva (Distribucion de frecuencia acumulada)
tablaFreq3
plot(tablaFreq3$Main, cumsum(tablaFreq3$Frequency), main="Ojiva de frecuencia acumulada", 
     xlab = "Marca de clase", ylab = "Frecuencia absoluta")
lines(tablaFreq3$Main,cumsum(tablaFreq3$Frequency), type="l")

##Diagrama de Cajas, se puede observar valores atípicos en relación a los recien nacidos
boxplot(Grupo3$edad, horizontal = TRUE, main="Diagrama de Caja de Defunciones por
        edad grupo 3", xlab = "Edad", col="lightblue")


#Diagrama de Cajas(Edad por sexo)
boxplot(Grupo3$edad ~ Grupo3$sexo, horizontal = T, main="Diagrama de Cajas Defunciones por edad
        y sexo", xlab = "Edad", ylab = "Sexo", col=c("lightblue", "pink"))

#Diagrama de Cajas(Cantones con mas defunciones)
dataDauMil3 <- subset(Grupo3, cant_fall == "Milagro" | cant_fall == "Daule", )
dataDauMil3
boxplot(dataDauMil3$edad ~ dataDauMil3$cant_fall, horizontal = T, main="Diagrama de Cajas Defunciones
        en Daule y Milagro Grupo 3", xlab = "Edad", ylab = "Canton", col=c("lightblue", "grey"))

#dataDauMil3%>%  
#  filter(cant_fall=="Milagro")-> Milagro3  #A) Grupo de fallecidos en milagro

#mean(Milagro$edad)

#dataDauMil3%>%  
#  filter(cant_fall=="Daule")-> Daule3  #A) Grupo de fallecidos en Daule

#mean(Daule$edad)

#Medidas de tendencia central y dispersión Edad
mediaEdad3 <- mean(Grupo3$edad)
mediaEdad3
medianaEdad3 <- median(Grupo3$edad)
medianaEdad3
MediaEdadc3 <- mean(Grupo3$edad, trim=0.05) ##Eliminamos los recien nacidos fallecidos y viejos
MediaEdadc3
ModadEdad3 <- mlv(Grupo3$edad, method = "mfv")
ModadEdad3    #La edad más frecuente de muertes en Guayas sin Guayaquil
TablaGeneral33 = table(Grupo3$edad, Grupo3$sexo) #Ocurrencia de edad por Sexo
TablaGeneral33

#Varianza y desviacion estandar
obs33=sort(Edad33)
quantile(obs33)
VarEdad3 <-var(obs33)
DesvEdad3 <- sd(Edad33)
sesgoEdad3 <- skewness(obs33)

#Estandarizacion de la muestra
Y33 = (Edad33 - mediaEdad3)/DesvEdad3
Y33
#coeficiente de variación por edad
cvEdad3 = (DesvEdad3/mediaEdad3)*100 
cvEdad3

#Poligono de frecuencias y freq acumulada y freq absoluta 
plot(tablaReltv33, type="fp", main="Poligono de frecuencia G3", xlab = "Marca de clase" )

plot(tablaReltv33, type="cfp", main="Poligono de frecuencia acumulada G3", xlab = "Marca de clase")

plot(histEdad3$mids, histEdad3$counts, main="Poligono de frecuencia absoluta G2",
     xlab = "Marca de clase") 
lines(histEdad3$mids,histEdad3$counts, type="l")

#Ojivas

plot(histEdad3$mids,cumsum(histEdad3$counts), main="Ojiva de frecuencia absoluta G3", 
     xlab = "Marca de clase")
lines(histEdad3$mids,cumsum(histEdad3$counts), type="l")

plot(histEdad3$mids,cumsum(histEdad3$counts)/sum(histEdad3$counts), main="Ojiva de frecuencia relativa G2", xlab = "Marca de clase", 
     ylab = "Frecuencias relativas") 
lines(histEdad3$mids,cumsum(histEdad3$counts)/sum(histEdad3$counts), type="l", )

#Medidas de tendencia central y dispersión Edad, sexo y canton

#Promedios de muerte por edad y sexo
tabMeanSexEda3 = aggregate(x = Grupo3$edad,         
                           by = list(Grupo3$sexo),
                           FUN = mean)
tabMeanSexEda3

mediaEdaSexF3 <- (tabMeanSexEda3 %>%  
                    filter(Group.1=="Mujer"))$x

mediaEdaSexF3 #Media de muertes femeninas
mediaEdaSexM3 <-(tabMeanSexEda3 %>%  
                   filter(Group.1=="Hombre"))$x
mediaEdaSexM3 #Media de muertes masculinas
MediaEdaSexc3 = aggregate(x = Grupo3$edad,         
                          by = list(Grupo3$sexo),
                          FUN = mean, trim=0.1)
MediaEdaSexcF3 <-(MediaEdaSexc3 %>%  
                    filter(Group.1=="Mujer"))$x ##Eliminamos los recien nacidos fallecidos y viejos
MediaEdaSexcF3
MediaEdaSexcM3 <-(MediaEdaSexc3 %>%  
                    filter(Group.1=="Hombre"))$x
MediaEdaSexcM3
#La edad más frecuente de muertes Femeninas en Guayas sin Guayaquil
v_logico = tablaEdaSexF3==max(tablaEdaSexF3) #Variable temporal
which(v_logico==TRUE)
ModadEdaSexF3 <- 81
ModadEdaSexF3
#La edad más frecuente de muertes Masculinas en Guayas sin Guayaquil
v_logico = tablaEdaSexM3==max(tablaEdaSexM3) #Variable temporal
which(v_logico==TRUE)
ModadEdaSexM3 <- 81
ModadEdaSexM3












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




