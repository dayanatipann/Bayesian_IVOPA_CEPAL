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
cat("\f")

## Lectura del censo. 

censo_vivienda <- readRDS("Data/viv_covar.rds") %>% 
  filter(!v0201 %in% c(3,5))

censo_covariables <- readRDS("Data/sector_covariables.rds") 


###############################################################################
n_distinct(censo_vivienda$id_sector)

censo_vivienda %>% group_by(v0201) %>% 
  summarise(num_nas_viv = sum(is.na(per_viv)),
            tot_pers = sum(per_viv, na.rm = TRUE),
            num_viv = n()) %>% 
  ungroup() %>% 
  mutate(porc_viv = 100*num_viv/sum(num_viv))

censo_viv_pers_presente <- censo_vivienda %>% filter(v0201 == 1) 

n_distinct(censo_viv_pers_presente$id_sector)

###############################################################################
## Preparando bases para el Modelo 1 
###############################################################################
sector_completo <- censo_viv_pers_presente %>%
  group_by(id_prov, id_can, id_parr, id_zona, id_sector) %>% 
  summarise(tot_person = sum(per_viv, na.rm = T), 
            tot_viv = n(), 
           .groups = "drop") %>% 
  mutate(num_pers_vivi = tot_person/tot_viv)

nrow(sector_completo)

saveRDS(sector_completo,"Data/tot_viv_sec.rds")


sum(is.na(sector_completo$tot_person))
hist(sector_completo$tot_person)
hist(sector_completo$num_pers_vivi)

sector_completo <- inner_join(sector_completo, censo_covariables)
#### sector_completo
modelo_completa_pois  <- stan_glmer(
  tot_person ~ 1 +
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
  data = sector_completo,
  iter = 1000,            # total number of iterations per chain
  cores = 4, 
  family = poisson(link = "log")
)

saveRDS(modelo_completa_pois, "Data/Modelos/fit_bayes_poiss.rds")

modelo_completa_pois <- readRDS("Data/Modelos/fit_bayes_poiss.rds")
####################################################################
# Validación del modelo 
summary(modelo_completa_pois)

mcmc_rhat(rhat(modelo_completa_pois))

x <- as.matrix(modelo_completa_pois)
mcmc_intervals_data(x, point_est = "mean")


################################################################################
sector_completo2 <- sector_completo 

y_pred_B_pois <- posterior_epred(modelo_completa_pois,
                            newdata = sector_completo2, 
                            type = "response")

rowsrandom <- sample(nrow(y_pred_B_pois), 100)
y_pred2 <- (y_pred_B_pois[rowsrandom,])
p0 <- ppc_dens_overlay(y = as.numeric(sector_completo2$tot_person), y_pred2)
p0


y_pred2 <- t(t(y_pred_B_pois[rowsrandom,])/sector_completo2$tot_viv)

p1 <- ppc_dens_overlay(y = as.numeric(sector_completo2$num_pers_vivi), y_pred2)
p1

p3 <- p0 + labs(title = "Total de personas") |
p1 + labs(title = "Promedio de personas por sector")  

ggsave(plot = p3, "Output/ppc_poison.png", scale = 2)

Plot_dens_draws(modelo = modelo_completa_pois,
                pars = c("pob_cons_parr","NBI_parr"))



