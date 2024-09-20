#CLASES 1, 2 y 3

install.packages("rio")
library(rio)
library(tidyverse)
library(ggplot2)
library(stringr)
library(readr)
library(dplyr)
library(readxl)

data <- read_xlsx("CPI.xlsx")
data_con_rio <- import("CPI.xlsx")

view(data)
str(data) #ver estructura de datos
glimpse (data)
names(data)
summary(data) #ver estadísticos descriptivos de la data
class(data)

## Operador pipe: |> 
#sirve para expresar una secuencia de múltiples operaciones

#select para columnas y filter para filas
data |> 
  select(1:4) #seleccionar o excluir columnas del data frame

data_1 <- data |>
  select(country, year, region, cpi_score) #nueva data con solo 4 variables

data_2 <- data_1 |>
  filter(year==2022) #seleccionar solo filas con el año 2022

data_3 <- data_2 |>
  mutate(cpi_score = 100-cpi_score) #permite realizar operaciones

data_4 <- data_3|>
  arrange(desc(cpi_score)) #permite ordenar o reordenar un data frame de forma simple, descendente o múltiple (varias variables)

data_3 |>
  group_by(year) |>
  summarise(Media=mean(cpi_score, na.rm=T))

data_3 |>
  count(cpi_score) |>   #contar el número de observaciones de cada elemento en el df
  arrange(desc(n))  #ordenar de forma descendente

summary(data_3$cpi_score)

data_5 <- data_3 |>
  drop_na(cpi_score) #eliminas NAs

summary(data_5$cpi_score)

#Crear una nueva columna con una serie de atributos
data_5 <- data_5 |>
  mutate(corrupcion=case_when(cpi_score<30~"Bajo",
                              cpi_score<60~"Medio",
                              cpi_score<=100~"Alto"))

table(data_5$corrupcion)

data_5$corrupcion2 = factor(data_5$corrupcion, 
                            levels = c("Bajo","Medio","Alto"),
                            ordered = TRUE)

