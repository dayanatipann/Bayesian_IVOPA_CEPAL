### Lectura de la base de datos
library(magrittr)
library(tidyverse)
viv_covar <- readRDS("C:/Users/sguerrero/Downloads/viv_covar.rds")

#########################################
resumen <- data.frame(Nombre_Columna = names(viv_covar))
resumen %<>% mutate(tipo = map_chr(Nombre_Columna, function(x)class(viv_covar[[x]])))


max_values <- viv_covar %>%
  summarise(across(where(is.numeric) | where(is.integer), max), na.rm = TRUE) %>%
  pivot_longer(everything(), names_to = "Nombre_Columna", values_to = "Valor_Maximo")

min_values <- viv_covar %>%
  summarise(across(where(is.numeric) | where(is.integer), min), na.rm = TRUE) %>%
  pivot_longer(everything(), names_to = "Nombre_Columna", values_to = "Valor_Minimo")

media_values <- viv_covar %>%
  summarise(across(where(is.numeric) | where(is.integer), mean), na.rm = TRUE) %>%
  pivot_longer(everything(), names_to = "Nombre_Columna", values_to = "Valor_Media")

mediana_values <- viv_covar %>%
  summarise(across(where(is.numeric) | where(is.integer), median), na.rm = TRUE) %>%
  pivot_longer(everything(), names_to = "Nombre_Columna", values_to = "Valor_Mediana")

SD_values <- viv_covar %>%
  summarise(across(where(is.numeric) | where(is.integer), sd), na.rm = TRUE) %>%
  pivot_longer(everything(), names_to = "Nombre_Columna", values_to = "Valor_sd")

nas_values <- viv_covar %>%
  summarise(across(where(is.numeric) | where(is.integer), function(x)sum(is.na(x)))) %>%
  pivot_longer(everything(), names_to = "Nombre_Columna", values_to = "Num_nas")


max_char <- viv_covar %>%
  summarise(across(where(is.character), function(x)max(nchar(x), na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "Nombre_Columna", values_to = "leng_max")

min_char <- viv_covar %>%
  summarise(across(where(is.character), function(x)min(nchar(x), na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "Nombre_Columna", values_to = "leng_min")

nas_values_char <- viv_covar %>%
  summarise(across(where(is.character) , function(x)sum(is.na(x), na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "Nombre_Columna", values_to = "Num_nas_char")

resumen2 <- reduce(
  list(nas_values_char, min_char,max_char,
       nas_values, SD_values, mediana_values, media_values,min_values, max_values),
  full_join, by = join_by(Nombre_Columna)) %>%
  full_join(x = resumen, y = ., by = join_by(Nombre_Columna))

###########################################################
# 1. Casa/villa 
# 2. Dept. en casa o edificio
# 3. Cuarto/s en casa de inquilinato.
# 4. Mediagua
# 5. Rancho
# 6. Covacha
# 7. Choza
# 8. Otra vivienda particular
# 9. Hotel, pensión, residencia u hostal 
# 10. Cuartel militar, policía o bomberos 
# 11 Centro de privación de libertad/cárcel 
# 12.Hospital, clínica, etc
# 13. Convento o institución religiosa 
# 14. Centro de acogida y protección para niños/as y adolescentes
# 15. Residencia de adultos mayores/Asilo de ancianos 
# 16. Internado de estudiantes 
# 17. Campamento de trabajo 
# 18. Otra vivienda colectiva
# 19. Sin vivienda 
sum(viv_covar$totper)

viv_covar %>% group_by(v01) %>% 
  summarise(media = mean(totper, na.rm = TRUE),
            n = n(),
            sd = sd(totper, na.rm = TRUE),
            max = max(totper, na.rm = TRUE),
            min = min(totper, na.rm = TRUE),
            n10 = sum(totper<=10,na.rm = TRUE)/n()*100,
            n20 = sum(totper<=20,na.rm = TRUE)/n()*100,
            n100 = sum(totper>100,na.rm = TRUE)/n()*100)

viv_covar %>% group_by(v01) %>% 
  summarise(media = mean(tot_per_prec, na.rm = TRUE),
            n = n(),
            nas = sum(is.na(tot_per_prec))/n()*100,
            sd = sd(tot_per_prec, na.rm = TRUE),
            max = max(tot_per_prec, na.rm = TRUE),
            min = min(tot_per_prec, na.rm = TRUE),
            n10 = sum(tot_per_prec<=10,na.rm = TRUE)/(sum(!is.na(tot_per_prec)))*100,
            n20 = sum(tot_per_prec<=20,na.rm = TRUE)/(sum(!is.na(tot_per_prec)))*100,
            n100 = sum(tot_per_prec>100,na.rm = TRUE)/(sum(!is.na(tot_per_prec)))*100)

##########################################
viv_covar %>% group_by(opp_cen, opa_cen) %>% 
  tally()

viv_covar %>% group_by(opp_cen, opa_cen, temp_cen, desop_cen, const_cen) %>%
  summarise(
    n = n(),
    suma = sum(totper, na.rm = TRUE),
    suma_pre = sum(tot_per_prec, na.rm = TRUE)
  )






