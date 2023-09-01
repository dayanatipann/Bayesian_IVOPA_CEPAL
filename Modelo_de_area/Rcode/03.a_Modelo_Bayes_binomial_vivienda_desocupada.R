###############################################################
# Modelos de estimación de áreas pequeñas para la estimación  #
# del total población                                         #
# Lectura y preparación de las bases de datos                 #
# Autor: Stalyn Guerrero & Andrés Gutiérrez                   #
# Descripción: Un modelo binomial para viviendas 
#              ocupadas y desocupadas                         #
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
library(posterior)
library(bayesplot)
library(patchwork)
source("0Funciones/Plot_dens_draws.R")
select <- dplyr::select
cat("\f")
################################################################################
## Lectura del censo. 
censo_vivienda <- readRDS("Data/viv_covar.rds")  %>% 
  filter(!v0201 %in% c(3,5))

censo_covariables <- readRDS("Data/sector_covariables.rds") 

censo_sector <- readRDS("Data/data_sector.rds") %>% select(id_sector,Completa)
censo_vivienda <- inner_join(censo_vivienda, censo_sector, by = "id_sector")

## Definiendo viendas ocupadas y desocupadas. 
# v0201: CONDICION DE OCUPACION_PARTICULAR	
# 1. Ocupada con personas presentes
# 2. Ocupada con personas ausentes
# 3. De temporada o vacacional
# 4. Desocupada
# 5. En construcción
# Blanco

# per_viv: Total de personas en la vivienda	

table(censo_sector$Completa)
summary(censo_vivienda$per_viv)

# 0 = Ocupada 
# 1 = Desocupadas 


sector_completo <-
  censo_vivienda %>% filter(Completa == "Sector completo") %>%
  mutate(Desocupada = case_when(v0201 == 1  ~ 0,
                                v0201 == 2 ~ 0,
                                v0201 == 4 ~ 1,
                                TRUE ~ NA_real_))

sector_completo %>% group_by(v0201,Completa, Desocupada) %>% 
  tally(name = "conteo_vivienda")

sector_completo %>% 
  group_by(v0201, Desocupada) %>% 
  summarise(total = n())
## 13% desocupadas 
mean(sector_completo$Desocupada, na.rm = TRUE)


####### Base modelo binomial 
base_conteo_viviendas <- sector_completo %>% group_by(id_sector) %>% 
  summarise(Desocupadas = sum(Desocupada, na.rm = TRUE),
            Ocupadas = sum(1-Desocupada,  na.rm = TRUE),
            n_vivienda = n())

base_conteo_viviendas %>% summarise_if(is.numeric, sum)
dim(base_conteo_viviendas)


base_conteo_viviendas <- inner_join(base_conteo_viviendas, 
                                    censo_covariables)


modelo_binomial <- stan_glmer(
  cbind(Desocupadas, Ocupadas) ~ 1 +
    #(1|id_prov) +                                           
    (1|id_parr) +                                            
    (1|id_can) +                                             
    pob_cons_parr +                                     
    NBI_parr +                                         
    #msp_tot_can +                                      
    #mean_km_22_23 +                                    
    n_hbt +                                            
    #DEGURBA +                                          
    #MEDIA_EDU +                                        
    #MODA_EDU +                                         
    #SUM_EDU +                                          
    #MEDIA_SALUD +                                      
    #MODA_SALUD +                                       
    #SUM_SALUD +                                        
    #MODA_POP_KM2 +                                     
    MEDIA_POP_KM2 +                                    
    #SUM_POP_KM2 +                                      
    #MODA_POLI +                                        
    #MEDIA_POLI +                                       
    #SUMA_POLI +                                        
    #MEDIA_PELIGROSIDAD +                               
    #MODA_PELIGROSIDAD +                                
    wpop_sum +                                         
    #wpop_mean +                                        
    #sum.crops.coverfraction +                            
    #sum.urban.coverfraction +                            
     mean.crops.coverfraction +                           
     mean.urban.coverfraction +                           
    # mode.crops.coverfraction +                           
    # mode.urban.coverfraction +                           
    #sum.avg_vis +                                        
    mean.avg_vis +                                       
    #mode.avg_vis +                                       
    #sum.GHS_POP_E2020_GLOBE_R2023A_32717_nearestn +      
    mean.GHS_POP_E2020_GLOBE_R2023A_32717_nearestn +     
    #mode.GHS_POP_E2020_GLOBE_R2023A_32717_nearestn +     
    #sum.GHS_BUILT_S_E2020_GLOBE_R2023A_32717_nearestn +  
    mean.GHS_BUILT_S_E2020_GLOBE_R2023A_32717_nearestn  
    #mode.GHS_BUILT_S_E2020_GLOBE_R2023A_32717_nearestn + 
    #MEDIA_ZONA_URBANA +                                  
    #MODA_ZONA_URBANA +                                   
    #SUM_ZONA_URBANA
    ,
  data = base_conteo_viviendas,
  family = binomial(link = "logit"),
  iter = 1000,            # total number of iterations per chain
  cores = 4 
)

saveRDS(object = modelo_binomial, 
        file = "Data/Modelos/Binomial_bayes_vivienda_desocupadas_300823.rds")

modelo_binomial <- readRDS("Data/Modelos/Binomial_bayes_vivienda_desocupadas_300823.rds")

####################################################################
# Validación del modelo 
summary(modelo_binomial)

mcmc_rhat(rhat(modelo_binomial))

x <- as.matrix(modelo_binomial)
mcmc_intervals_data(x, point_est = "mean")

#################################
sector_completo2 <- sector_completo %>% as.data.frame() %>%
  rename(n_hbt = n_hbt.y)

y_pred_B <- posterior_epred(modelo_binomial,
                            newdata = sector_completo2, type = "response")

rowsrandom <- sample(nrow(y_pred_B), 100)
y_pred2 <- y_pred_B[rowsrandom,]
p0 <- ppc_dens_overlay(y = as.numeric(sector_completo2$Desocupada), y_pred2)
p0
#################################
y_pred_B <- posterior_epred(modelo_binomial)

yrep <- base_conteo_viviendas$Desocupadas/base_conteo_viviendas$n_vivienda

rowsrandom <- sample(nrow(y_pred_B), 100)
y_pred2 <- y_pred_B[rowsrandom,]
p1 <- ppc_dens_overlay(y = yrep, y_pred2)


#################################
yrep <- base_conteo_viviendas$Desocupadas/base_conteo_viviendas$n_vivienda

rowsrandom <- sample(nrow(y_pred_B), 100)
y_pred2 <- t(t(y_pred_B[rowsrandom,])*base_conteo_viviendas$n_vivienda)

p2 <- ppc_dens_overlay(y = base_conteo_viviendas$Desocupadas, y_pred2)

p3 <- (p0 + labs(title = "Binaria"))/
(p1 + labs(title = "theta")|p2 + labs(title = "Nd"))

ggsave(plot = p3, "Output/ppc_binomial.png", scale = 2)
#################################

Plot_dens_draws(modelo = modelo_binomial,
                pars = c("pob_cons_parr","NBI_parr"))

