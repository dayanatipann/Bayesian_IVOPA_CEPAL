#**************************************************************************************#
#**************************************************************************************#
#
#            Preprocesamiento de bases de vivienda del censo y precenso 2010           #####   
#                       
#
#     Responsable:            Dayana Tipán 
#     Fecha de elaboración:   17/03/2023
#     ultima actualización:   11/04/2023 
#     Actualizado por:        Dayana Tipán y Andrés Peña               
#     Organización:           INEC
#                             
#
#**************************************************************************************#
#**************************************************************************************#

########################################################## cargar base de datos ##########################################################

rm(list = ls())

library(stringr) # contar caracteres
library(readxl)
library(dplyr)
library(foreign) #leer spss
# install.packages("iNZightTools")
library(iNZightTools) # filtrar niveles
library(readr)

# Censo 2010, bdd viviendas

base_viv <- read.spss("dat/CPV2010/CPV2010m_Vivienda.sav", to.data.frame = TRUE)

# guardar en formato rds
# setwd("dat/CPV2010/")
# save(base_viv, file = "base_viv")
# load("base_viv")

# base_viv <- file.choose()
# list.files()
# base_viv <- readRDS("base_viv.rds")

# base_viv <- base %>% select (I01, I02, I03, I04, I05, I06, I09, I10, VTV, VCO, TOTPER) 

dpa <- read_excel("dat/CODIFICACIÓN_2022.xlsx", sheet = "PROVINCIAS")

precenso <- read_excel("dat/Precenso/Marco Maestro de Muestreo 2010.xlsx")

########################################################## limpieza de los datos ##########################################################

#primero revisamos el tipo de datos
glimpse(base_viv)

#cambiar nombre de variable Número de Habitantes a pob_2018
names(base_viv)[names(base_viv) == "I01"] <- "dpa_despro"
base_viv$dpa_despro <- tolower(base_viv$dpa_despro)
dpa$dpa_despro <- tolower(dpa$dpa_despro)

base_viv <- merge (base_viv, dpa, by = "dpa_despro")

#crear id upm hasta nivel de sector
base_viv$upm <- paste(base_viv$dpa_provin, base_viv$I02, base_viv$I03, base_viv$I04, 
                      base_viv$I05)
# quitar espacios
base_viv$upm  <- str_replace_all(base_viv$upm , " ", "")
# base_viv$id_vi  <- str_replace_all(base_viv$id_vi , " ", "")
# base_viv$id_hog  <- str_replace_all(base_viv$id_hog , " ", "")

# extraer número de personas de TOTPER
base_viv$totper<-substring(base_viv$TOTPER, 1, 2)

summary(base_viv)
colnames(base_viv) #revisamos los nombres de las columnas de la base de datos

# contar carácteres
str_length(base_viv$upm)
length(base_viv$upm)

# no se puede guardar como conteo la varaible totper porque son diferntes variables
censo_2010 <- base_viv %>% 
  filter(dpa_provin== "17") %>% 
  select(upm, dpa_provin, dpa_despro, totper, I02, I03, I04, I05, I06, I09, totper, I10, URV, VTV, VAP, VCO, 
         V01, V03, V05, V02, V04, V06, V07, V08, V09, V10, V11, V12A, V12B, V13, 
         V14, V15, V16, TOTDOR, TOTEMI, id_man10)

# # en el caso que no se tomaran todas la variables, coincide con la base precenso son 7020, 7022 es precenso
# base_viv$totper <- as.numeric(base_viv$totper)
# censo_sele <- base_viv %>%
#   filter(dpa_provin== "17") %>%
#   select(upm, dpa_provin, totper) %>%
#   group_by(upm) %>%
#   mutate(Habitantes=as.numeric(totper)) %>%
#   summarise(Habitantes=sum(totper))


# crear variable área de residencia
censo_2010$area <- ifelse (censo_2010$I05 == 999 & censo_2010$I05 <= 999, "rural", "urbano") 

# cambiar 0 por na
censo_2010$totper[censo_2010$totper == "0"] <- NA

#Contar el total de NAs en la base de datos
sum(is.na(censo_2010))

#Saber el número de NAs por columna
colSums(is.na(censo_2010))

# Porcentaje de valores NA
mean(is.na(censo_2010)) 

# guardar en formato rds

setwd("Preprocesamiento_bases/")
save(censo_2010, file = "censo_2010")
load("censo_2010")

## Base precenso ####
str_length(precenso$`UPM - sector censal`)
summary(precenso)
colnames(precenso) #revisamos los nombres de las columnas de la base de datos

names(precenso)[names(precenso) == "UPM - sector censal"] <- "upm"
names(precenso)[names(precenso) == "Habitantes"] <- "totper"

# names(precenso)[names(precenso) == "Cod. Prov"] <- "dpa_provin"

# filtrar niveles provinicia
precenso_filter <- filterLevels(precenso, var = "Provincia",
                              levels = "Pichincha")
# base final y limpia
precenso_2010 <- precenso_filter %>% 
  select("upm", "totper", "Provincia", "Cod. Prov", "Cantón", "Cod. Cantón", 
         "Ciudad", "Cod. Ciudad", "Viviendas totales", "Viviendas ocupadas")

# crear área de residencia
# extraer zona
precenso_2010$zona <- substring(precenso_2010$upm, 7, 9)

# crear variable área de residencia
precenso_2010$area <- ifelse (precenso_2010$zona == 999 & precenso_2010$zona <= 999, "rural", "urbano") 

# cambiar 0 por na
precenso_2010$totper[precenso_2010$totper == "0"] <- NA

#Contar el total de NAs en la base de datos
sum(is.na(precenso_2010))

#Saber el número de NAs por columna
colSums(is.na(precenso_2010))

# Porcentaje de valores NA
mean(is.na(precenso_2010)) 

# guardar en formato rds
setwd("out/Preprocesamiento_bases/")
save(precenso_2010, file = "precenso_2010")
load("precenso_2010")

# Comparar si el upm del censo y precenso son  ####

##identical  regresa solo un boleano
identical(censo_2010$upm, precenso_2010$upm)

## x %in% y regresa un vector de booleanos de la misma longitud que x 
censo_2010$upm %in% precenso_2010$upm

##podemos utilizar esto para extraer los valores distintos
censo_2010$upm [censo_2010$upm %in% precenso_2010$upm==FALSE]

# sí está bien los valores 1 y 0:
censo_2010$comp <- as.integer(censo_2010$upm %in% precenso_2010$upm)

# Si quieres un NA en lugar del 0:
# censo_2010$comp_na  <- ifelse(censo_2010$comp  %in% precenso_2010$upm, 1, NA)


