###############################################################
# Modelos de estimación de áreas pequeñas para la estimación  #
# del total población                                         #
# Lectura y preparación de las bases de datos                 #
# Autor: Stalyn Guerrero & Andrés Gutiérrez                   #
# Descripción: Consolidación de bases de datos CENSO          #
# viviendas y exploración (estimación puntual y error estándar)
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
select <- dplyr::select
cat("\f")

## Lectura del censo. 
censo_vivienda <- readRDS("Data/viv_covar.rds") %>% 
  filter(!v0201 %in% c(3,5))

censo_sector <- readRDS("Data/data_sector.rds") %>% select(id_sector,Completa)
censo_vivienda <- inner_join(censo_vivienda, censo_sector, by = "id_sector")

censo_covariables <- readRDS("Data/sector_covariables.rds") 

modelo_binomial <- readRDS("Data/Modelos/Binomial_bayes_vivienda_desocupadas_300823.rds") 


## Difiniendo viendas ocupadas y desocupadas. 
table(censo_vivienda$v0201, useNA = "a")
censo_vivienda %<>% mutate(
  Desocupada = case_when(
    v0201 == 1  ~ 0,
    v0201 == 2 ~ 0,
    v0201 == 4 ~ 1,
    TRUE ~ NA_real_
      )
)

## Conteo 
sector_incompleto <-
  censo_vivienda %>% group_by(Completa, id_sector) %>%
  summarise(Nd = n())  %>%  inner_join(censo_covariables) %>% 
  filter(Completa != "Sector completo")

epred_mat <- posterior_epred(modelo_binomial,
                             newdata = sector_incompleto,
                             type = "response")

dim(epred_mat)
dim(sector_incompleto)

hist(epred_mat[,1])
sd(epred_mat[,1])
mean(epred_mat[,1])

intervalo <- data.frame(
  id_sector =  sector_incompleto$id_sector,
qinf =  apply(epred_mat,2, function(x)quantile(x, prob =0.025)),
qsup =  apply(epred_mat,2, function(x)quantile(x, prob =0.975)),
estima_se =  apply(epred_mat,2, sd),
Pred_desocupadas = colMeans(epred_mat)
)

censo_vivienda <- censo_vivienda %>% full_join(intervalo)


censo_vivienda %<>%
  mutate(
    Desocupada2  = case_when(is.na(Desocupada) ~ Pred_desocupadas,
                             TRUE ~ Desocupada),
    MEInf_desocupadas  = case_when(is.na(Desocupada) ~ Pred_desocupadas - qinf,
                             TRUE ~ 0),
    MESup_desocupadas  = case_when(is.na(Desocupada) ~ qsup - Pred_desocupadas,
                             TRUE ~ 0)

  )

mean(censo_vivienda$Desocupada, na.rm = TRUE)
mean(censo_vivienda$Desocupada2)
min(censo_vivienda$MEInf_desocupadas, na.rm = TRUE)
min(censo_vivienda$MESup_desocupadas)


sum(censo_vivienda$Desocupada, na.rm = TRUE)
sum(censo_vivienda$Desocupada2)


censo_vivienda %>%
  summarise(
  total = sum(Desocupada2),
  Porcen = mean(Desocupada2) ,
  LimInf_desocupadas = (Porcen - (mean(MEInf_desocupadas))) *100,
  LimSup_desocupadas = (Porcen + (mean(MESup_desocupadas))) *100,
  Leng_IC = LimSup_desocupadas - LimInf_desocupadas,
  Porcen = Porcen*100
)

censo_vivienda %>% group_by(id_can) %>% 
  summarise(
    total = sum(Desocupada2),
    Porcen = mean(Desocupada2) ,
    LimInf_desocupadas = (Porcen - (mean(MEInf_desocupadas) )) *100,
    LimSup_desocupadas = (Porcen  + (mean(MESup_desocupadas))) *100,
    Porcen = Porcen * 100,
    Leng_IC = LimSup_desocupadas - LimInf_desocupadas
  ) 



censo_vivienda %>% group_by(id_prov) %>% 
  summarise(
    total = sum(Desocupada2),
    Porcen = mean(Desocupada2) ,
    LimInf_desocupadas = (Porcen - (mean(MEInf_desocupadas) )) *100,
    LimSup_desocupadas = (Porcen  + (mean(MESup_desocupadas))) *100,
    Porcen = Porcen * 100,
    Leng_IC = LimSup_desocupadas - LimInf_desocupadas
  ) %>% view()


censo_vivienda %>% group_by(id_parr) %>% 
  summarise(
    total = sum(Desocupada2),
    Porcen = mean(Desocupada2) ,
    LimInf_desocupadas = (Porcen - (mean(MEInf_desocupadas) )) *100,
    LimSup_desocupadas = (Porcen  + (mean(MESup_desocupadas))) *100,
    Porcen = Porcen * 100,
    Leng_IC = LimSup_desocupadas - LimInf_desocupadas
  ) 




censo_vivienda %>% group_by(id_sector) %>%   
  summarise(min = min(MEInf_desocupadas),
            min2 = min(MESup_desocupadas))


saveRDS(
  censo_vivienda %>% dplyr::select(-Pred_desocupadas,-Desocupada,
                                   Desocupadas = Desocupada2),
  file = "Data/01_censo_vivienda_desocupadas.rds"
)


