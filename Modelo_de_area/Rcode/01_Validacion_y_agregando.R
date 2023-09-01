##################################################################
#### Creación de modelo insumos para el modelo área ##############
##################################################################

## Libreria 
rm(list = ls())
library(tidyverse)
library(corrplot)
library("PerformanceAnalytics")
library(magrittr)
select <- dplyr::select
##################################################################
# lectura de bases de datos
##################################################################
viv_covar <- readRDS("Data/viv_covar.rds")  %>% 
  filter(!v0201 %in% c(3,5))

# v0201: CONDICION DE OCUPACION_PARTICULAR	
# 1. Ocupada con personas presentes
# 2. Ocupada con personas ausentes
# 3. De temporada o vacacional
# 4. Desocupada
# 5. En construcción
# Blanco	

conteo0 <-  viv_covar %>% group_by(id_sector) %>% mutate(n = n()) %>% 
  group_by(id_sector) %>% 
  summarise(num_nas = sum(is.na(v0201)))

conteo0 %<>% mutate(Completa = ifelse(num_nas >0, "Sector incompleto",
                           "Sector completo"))   
conteo0 %>% group_by(Completa) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(porc = n/sum(n)*100)


table(conteo0$num_nas)

## Conteos por v0201
viv_covar %>% group_by(v0201) %>%  tally() %>% mutate(porc = n/sum(n)*100)

conte1 <- viv_covar %>% group_by(id_sector) %>% mutate(n = n()) %>%
  group_by(id_sector, v0201) %>%
  summarise(num_nas = sum(is.na(per_viv)),
            num_nas = num_nas / unique(n) * 100) %>%
  spread(key = "v0201", value = "num_nas", fill = 0)

viv_covar %>% group_by(id_sector) %>% summarise(n = n(), num_nas = sum(is.na(per_viv))) %>% 
  inner_join(conte1) %>% view()
  
viv_covar %>% filter(id_sector == "100150045005", tothog == "0")

summary(viv_covar$per_viv)
sum(viv_covar$per_viv > 10, na.rm = T)

### Creando base agregada por sector

viv_sector <- viv_covar %>% group_by(id_prov, id_can, id_parr, id_zona, id_sector) %>% 
  summarise(tot_person = sum(per_viv, na.rm = T), 
            tot_viv = n(), 
            tot_viv_ocup = sum(v0201 %in% c(1:2)),
            tot_viv_deso = sum(v0201 == 4,na.rm = TRUE),.groups = "drop")

hist(viv_sector$tot_person)
hist(viv_sector$tot_viv)
hist(viv_sector$tot_viv_ocup)
hist(viv_sector$tot_viv_deso)

### covariables 

viv_covariables <- viv_covar %>% dplyr::select(id_prov: id_sector, 
                                  pob_cons_parr: SUM_ZONA_URBANA, -'...1',
                                  n_hbt = n_hbt.x, -n_hbt.y) %>% 
  distinct() %>%  
  mutate_if(is.numeric, function(x)as.numeric(scale(x)))



viv_final <- inner_join(viv_sector, viv_covariables)

nomb <- names(viv_covariables)

# Seleccionar las primeras 14 columnas
viv0 <- viv_final %>% select(tot_person, tot_viv_deso, all_of(nomb[-c(1:4)]))
viv1 <- viv_final %>% select(tot_person, tot_viv_deso, all_of(nomb[c(5:15)]))
viv2 <- viv_final %>% select(tot_person, tot_viv_deso, all_of(nomb[c(16:25)]))
viv3 <- viv_final %>% select(tot_person, tot_viv_deso, all_of(nomb[c(26:35)]))
viv4 <- viv_final %>% select(tot_person, tot_viv_deso, all_of(nomb[c(36:44)]))

#Visualize the correlation matrix
M0 <- cor(viv0)[,c("tot_person", "tot_viv_deso")] %>%
  as.data.frame() %>%  arrange(abs(tot_viv_deso))
M0
M0 %>%  arrange(abs(tot_person))


saveRDS(viv_final %>% inner_join(conteo0), "Data/data_sector.rds")
saveRDS(viv_covariables, "Data/sector_covariables.rds")
