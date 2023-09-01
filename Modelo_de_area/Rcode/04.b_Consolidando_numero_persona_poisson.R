###############################################################
# Modelos de estimación de áreas pequeñas para la estimación  #
# del total población                                         #
# Lectura y preparación de las bases de datos                 #
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
library(rstan)
library(rstanarm)
select <- dplyr::select
cat("\f")

## Lectura del censo. 
censo_vivienda <-
  readRDS("Data/01_censo_vivienda_desocupadas.rds")

censo_covariables <- readRDS("Data/sector_covariables.rds") 

censo_sector <- readRDS("Data/data_sector.rds") 

conteo_viv_ocu <- readRDS("Data/tot_viv_sec.rds")

modelo_completa_pois <- readRDS("Data/Modelos/fit_bayes_poiss.rds")

### Selección de sectores incompletos 
sector_incompleto <- full_join(sector_completo, censo_covariables) 
## Predicción 

y_pred_B_pois <- posterior_epred(modelo_completa_pois,
                                 newdata = sector_incompleto, 
                                 type = "response")

hist(y_pred_B_pois[,1])
sd(y_pred_B_pois[,1])
mean(y_pred_B_pois[,1])

intervalo_tot_pers <- data.frame(
  id_sector =  sector_incompleto$id_sector,
  qinf =  apply(y_pred_B_pois,2, function(x)quantile(x, prob =0.025)),
  qsup =  apply(y_pred_B_pois,2, function(x)quantile(x, prob =0.975)),
  estima_se =  apply(y_pred_B_pois,2, sd),
  Pred_tot_pers = colMeans(y_pred_B_pois)
)


y_pred_pers_pois <- t(t(y_pred_B_pois/sector_incompleto$tot_viv))


hist(y_pred_pers_pois[,1])
sd(y_pred_pers_pois[,1])
mean(y_pred_pers_pois[,1])

intervalo_pers <- data.frame(
  id_sector =  sector_incompleto$id_sector,
  pers_qinf =  apply(y_pred_pers_pois,2, function(x)quantile(x, prob =0.025)),
  pers_qsup =  apply(y_pred_pers_pois,2, function(x)quantile(x, prob =0.975)),
  pers_se =  apply(y_pred_pers_pois,2, sd),
  Pred_pers = colMeans(y_pred_pers_pois)
)


############################################################
########## Estimación de   las upm ########################
############################################################
censo_temp <- censo_vivienda %>%
  full_join(intervalo_pers) %>%
  mutate(
    pred_per_viv = case_when(
      is.na(v0201) & is.na(per_viv) ~ Pred_pers,
      v0201 == 2 & is.na(per_viv) ~ Pred_pers,
      v0201 == 4 & is.na(per_viv) ~ 0,
      TRUE ~ per_viv
    ),
    MEInf_pred_per_viv = case_when(
      is.na(v0201) & is.na(per_viv) ~ pers_qinf,
      v0201 == 2 & is.na(per_viv)  ~ pers_qinf,
      v0201 == 4 & is.na(per_viv) ~ 0,
      TRUE ~ 0
    ),
    ## Lim Sup
    MESup_pred_per_viv = case_when(
      is.na(v0201) & is.na(per_viv) ~ pers_qsup,
      v0201 == 2 & is.na(per_viv) ~ pers_qsup,
      v0201 == 4 & is.na(per_viv) ~ 0,
      TRUE ~ 0
    )
  )
censo_temp %>% group_by(v0201) %>% 
  summarise(num_nas = sum(is.na(pred_per_viv)))

summary((censo_temp$pred_per_viv))
summary((censo_temp$per_viv))
sum(censo_temp$pred_per_viv)

censo_temp %>%  
  group_by(id_sector) %>% 
  summarise(Min_sec = min(pred_per_viv),
            Max_sec = max(pred_per_viv)) %>% 
  View()

censo_temp %>% 
  group_by(v0201) %>% 
  summarise(Min_sec = min(pred_per_viv),
            Max_sec = max(pred_per_viv))%>% 
  View()

censo_temp %>% 
summarise(
  total = sum(pred_per_viv),
  L1 = total  - sum(MEInf_pred_per_viv),
  L2 = total  + sum(MESup_pred_per_viv),
  Sd = (L2 - L1)/(2*qnorm(0.975)),
  LimInf = total - 1.64*Sd,
  LimSup = total + 1.64*Sd,
  Len_IC = LimSup - LimInf
) %>% mutate(L1 = NULL, 
             L2 = NULL, 
)

censo_temp %>% 
  group_by(id_can) %>% 
  summarise(
    total = sum(pred_per_viv),
    L1 = total  - sum(MEInf_pred_per_viv),
    L2 = total  + sum(MESup_pred_per_viv),
    Sd = (L2 - L1)/(2*qnorm(0.975)),
    LimInf = total - 1.64*Sd,
    LimSup = total + 1.64*Sd,
    Len_IC = LimSup - LimInf
  ) %>% mutate(L1 = NULL, 
               L2 = NULL, 
  )


censo_temp %>% 
  group_by(id_prov) %>% 
  summarise(
    total = sum(pred_per_viv),
    L1 = total  - sum(MEInf_pred_per_viv),
    L2 = total  + sum(MESup_pred_per_viv),
    Sd = (L2 - L1)/(2*qnorm(0.975)),
    LimInf = total - 1.64*Sd,
    LimSup = total + 1.64*Sd,
    Len_IC = LimSup - LimInf
  ) %>% mutate(L1 = NULL, 
               L2 = NULL, 
  )

censo_temp %>% 
  group_by(id_parr) %>% 
  summarise(
    total = sum(pred_per_viv),
    L1 = total  - sum(MEInf_pred_per_viv),
    L2 = total  + sum(MESup_pred_per_viv),
    Sd = (L2 - L1)/(2*qnorm(0.975)),
    LimInf = total - 1.64*Sd,
    LimSup = total + 1.64*Sd,
    Len_IC = LimSup - LimInf
  ) %>% mutate(L1 = NULL, 
               L2 = NULL, 
  )


saveRDS(censo_temp, 
        file = "Data/02_censo_vivienda_personas_poisson.rds")



