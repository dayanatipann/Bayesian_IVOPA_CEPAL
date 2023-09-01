###############################################################
# Modelos de estimación de áreas pequeñas para la estimación  #
# del total población                                         #
# Autor: Stalyn Guerrero & Andrés Gutiérrez                   #
###############################################################

### Cleaning R environment ###
rm(list = ls())

#################
### Libraries ###
#################
library(tidyverse)
library(data.table)
library(openxlsx)
library(magrittr)
library(lme4)
library(rstan)
library(rstanarm)
source("0funciones/01_Agregados.R")
cat("\f")

## Lectura del censo. 
censo_vivienda <- readRDS("Data/02_censo_vivienda_personas_poisson.rds") 

#########################################################
## Predicción de la tasa de viviendas desocupadas 
#########################################################

Pred_desocupado(censo = censo_vivienda,
               agrega = NULL,
               Plot= TRUE,
               filtro = NULL)

Pred_desocupado(censo_vivienda, agrega = "id_can")
Pred_desocupado(censo_vivienda, agrega = "id_prov")
Pred_desocupado(censo_vivienda, agrega = "id_parr")

Pred_desocupado(censo_vivienda,
               agrega = "id_parr",
               Plot= TRUE,
               filtro = "100153")

#########################################################
## Predicción del total de personas 
#########################################################

## Resultado a nivel nacional 

Pred_totPob(censo_vivienda, 
           agrega = NULL,
           Plot = TRUE)

## Resultado a nivel canton  

Pred_totPob(censo_vivienda, 
           agrega = "id_can",
           Plot = FALSE)

Pred_totPob(censo_vivienda, 
           agrega = "id_can",
           filtro = "1001",
           Plot = TRUE)

## Resultado a nivel zona  

Pred_totPob(censo_vivienda, 
            agrega = "id_zona",
            Plot = FALSE)

Pred_totPob(censo_vivienda, 
            agrega = "id_zona",
            filtro = "100150001",
            Plot = TRUE)


## Resultado a nivel provincia

Pred_totPob(censo_vivienda,
           agrega =  "id_prov",
           Plot = TRUE,
           filtro = "10")

Pred_totPob(censo_vivienda,
           agrega =  "id_prov",
           Plot = FALSE,
           filtro = NULL) 

## Resultados parroquia

Pred_totPob(censo_vivienda,
            agrega =  "id_parr",
            Plot = TRUE,
            filtro = "100151")

Pred_totPob(censo_vivienda,
            agrega =  "id_parr",
            Plot = FALSE,
            filtro = NULL) 

