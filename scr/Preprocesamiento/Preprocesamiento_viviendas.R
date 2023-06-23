#**************************************************************************************#
#**************************************************************************************#
#
#            Censo 2022 y precenso 2019
#                       
#
#     Responsable:            Dayana Tipán J. 
#     Fecha de elaboración:   18/06/2023
#     ultima actualización:   22/06/2023 
#     Actualizado por:        Dayana Tipán J.               
#     Contacto:               Dayana Tipán J. (dtipan@colmex.mx)
#     Organización:            INEC
#                             
#
#**************************************************************************************#
#**************************************************************************************#

# Preprocesamiento base censo y precenso

rm(list = ls())  # con comentario xq luego se desactualiza el paquete dplyr

library(data.table)
library(readxl)
library(dplyr)
library(reshape2)
library(sf) # abrir formato  gpkg

setwd("D:/INEC/Proyectos/borrador_Bayesian_IVOPA_CEPAL")

censo <- setDT(readRDS("dat/censo_2022.rds"))
precenso <- setDT(read_xlsx("dat/Precenso_2019_Reporte de sector y sede operativa.xlsx"))

# base censo
censo <- censo[, .(id_vivienda = id_vivienda_2, exis_viv_cen = i11, cen_lin_cen = i12, 
                   tip_viv_cen = v01, ocup_viv_cen = v0201, ocup_viv_colec_cen = v0202, 
                   num_hog_viv_cen = inh, tot_per_cen = tot_per)][, 
                                                .(id_vivienda, exis_viv_cen, cen_lin_cen, 
                   tip_viv_cen, ocup_viv_cen, ocup_viv_colec_cen, num_hog_viv_cen, tot_per_cen)][, 
                   c("id_sector", "id_prov") := .(substr(id_vivienda, start = 1, stop = 12), 
                                                  substr(id_vivienda, start = 1, stop = 2))]
# Aplicar dcast para la variable ocup_viv
censo_dcast <- dcast(censo, id_vivienda + id_sector + id_prov + exis_viv + cen_lin + tip_viv + ocup_viv_colec + 
                       num_hog_viv + tot_per ~ ocup_viv, fun.aggregate = sum, value.var = "ocup_viv")

# length para conteo en fun.aggregate

# comprobar si hay sectores menores a 12 caracteres
# nchar(censo$id_sector)  
# censo[nchar(id_sector) <= 11]

# base precenso
precenso <- precenso[, id_sector := ifelse(nchar(Sector) == 11, sprintf("0%s", Sector), Sector)][, 
                    id_prov := substr(id_sector, start = 1, stop = 2)][
                    id_prov %in% c("03", "10", "16", "24"), 
                     .(id_prov, dpa_despro = Provincia, dpa_descan = Cantón, dpa_despar= Parroquia, 
                     id_sector, tot_viv_prec = `Total de Viviendas`, tot_per_prec = `Número de Habitantes`,
                   ocup_prec = Ocupadas, temp_prec = Temporales, desop_prec = Desocupadas,  const_prec = `En Construcción`, 
                   colec_prec = Colectivas)]

# Comprobar el número de caracteres
# nchar(precenso$id_sector)  
# precenso[nchar(id_sector) <= 11]

# Unir bases
viv_cen_pre <- merge(censo, precenso, by = "id_sector")

# Guardar bases
# Guardar base 
saveRDS(viv_cen_pre, file = "dat/viv_cen_pre.rds")

# covariables geo-espaciales ####

# Ruta al archivo gpkg
cov_geo <- "dat/Covariables/Covariables.gpkg"

# Abrir el archivo gpkg
centroides <- st_read(dsn = cov_geo, "Centroides") # falta agregar y centroides de zonas amanzanadas
cen_edu <- st_read(dsn = cov_geo, "Centros de educación") # falta agregar
cen_salud <- st_read(dsn = cov_geo, "Centros de salud") # falta agregar
ISOYETA <- st_read(dsn = cov_geo, "ISOYETA") # Variabilidad mensual de la productividad de la materia seca
pelig <-  st_read(dsn = cov_geo, "peligrosidad_man_sec") # peligrosidad manzana sector
rio_a <- st_read(dsn = cov_geo, "rio_a") # Distance to rivers, Cuerpos de agua 1:50000
rio_l <- st_read(dsn = cov_geo, "rio_l") # Distance to rivers, Cuerpos de agua 1:50000
nhab <-  st_read(dsn = cov_geo, "sec_a_nh") # Sectores con número de habitantes (de que base es?)
via <- st_read(dsn = cov_geo, "via_l") # distancia de vias

