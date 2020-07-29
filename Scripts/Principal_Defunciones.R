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
#subset(data, anio_insc == 2016, "prov_insc") 
head(data,5)

##1. Análisis comparativo por meses, sexo y estados civil del número de fallecidos


##2. Resumen de las 10 mayores causas de muerte de manera general y por sexo.


##3. Análisis comparativo de grupos de edades de fallecimiento por sexo 
##  y por cantón de fallecimiento a excepción de Guayaquil.


##4. Análisis comparativo de grupos de edades de fallecimiento por sexo en el cantón Guayaquil.




