###############################################################
# Modelos de estimación de áreas pequeñas para la estimación  #
# del total población                                         #
# Lectura y preparación de las bases de datos                 #
# Autor: Stalyn Guerrero & Andrés Gutiérrez                   #
# Descripción: Un modelo binomial para viviendas 
#              ocupadas y desocupadas                         #
###############################################################

### Cleaning R environment ###
# rm(list = ls())

#################
### Libraries ###
#################
library(tidyverse)
library(data.table)
library(openxlsx)
library(magrittr)
cat("\f")
#' Graficar la densidad de una distribución normal
#'
#' Esta función grafica la densidad de una distribución normal con media y desviación estándar especificadas. 
#' Además, resalta un intervalo específico de la distribución con un área sombreada y segmentos en el gráfico.
#'
#' @param media La media de la distribución normal.
#' @param desviacion La desviación estándar de la distribución normal.
#' @param lim_inf El límite inferior del intervalo a resaltar en el gráfico.
#' @param lim_sup El límite superior del intervalo a resaltar en el gráfico.
#' @param dist La distribución a graficar. Puede ser "Desocupadas" o "Total".
#' @return Un gráfico de la densidad de la distribución normal con el intervalo resaltado.
#' @examples
#' plot_densidad(media = 0, desviacion = 1, lim_inf = -1, lim_sup = 1, dist = "Desocupadas")
#' plot_densidad(media = 2, desviacion = 0.5, lim_inf = 1, lim_sup = 3, dist = "Total")
#' @import ggplot2
#' @importFrom latex2exp TeX
#' @export
#' 
#' 
plot_densidad <- function(media = 0,
                          desviacion = 1,
                          lim_inf = -1,
                          lim_sup = 1, 
                          dist = "Desocupadas"
){
  if(dist == "Desocupadas"){
    media <- media
    cuantil_2.5 <- lim_inf
    cuantil_97.5 <- lim_sup
     # desviacion <-
     #   ((cuantil_97.5 - cuantil_2.5) / (2 * qnorm(0.975) - 2 * qnorm(0.025))) 
    
    li <- cuantil_2.5 - desviacion
    ls <- cuantil_97.5 + desviacion
    x <- seq(li, ls, length = 100)
  
  }else if(dist == "Total"){
    
    # Crear una secuencia de valores x
  z <- seq(-4, 4, length.out = 150)
  x <- z*desviacion + media
  }
  
  # Calcular los valores de densidad correspondientes
  y <- dnorm(x, mean = media, sd = desviacion)
  
  densidad_inf <- dnorm(lim_inf, mean = media, sd = desviacion)
  densidad_sup <- dnorm(lim_sup, mean = media, sd = desviacion)
  # Crear un data frame con los valores de x e y
  datos <- data.frame(x = x, y = y)
  
  # Crear el gráfico
  ggplot(datos, aes(x = x, y = y)) +
    geom_line() +
    geom_area(data = subset(datos, x >= lim_inf & x <= lim_sup),
              aes(y = y), fill = "skyblue", alpha = 0.3) +
    geom_segment(aes(x = lim_inf, xend = lim_inf, y = 0, yend = densidad_inf),
                 linetype = "dashed", color = "red") +
    geom_segment(aes(x = lim_sup, xend = lim_sup, y = 0, yend = densidad_sup),
                 linetype = "dashed", color = "red") +
    geom_text(aes(x = lim_inf, y = densidad_inf, label = round(lim_inf,2)), 
              vjust = -0.5, hjust = 1, color = "red") +
    geom_text(aes(x = lim_sup, y = densidad_sup, label = round(lim_sup,2)), 
              vjust = -0.5, hjust = -0.2, color = "red") +
    xlab(latex2exp::TeX("\\theta")) + theme_classic()+
    theme(axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()) 
  
}


#' Función auxiliar para cálculos y visualizaciones de desocupación en un censo
#'
#' Esta función realiza cálculos y visualizaciones relacionadas con datos de desocupación en un censo.
#'
#' @param censo El conjunto de datos del censo que contiene información sobre la desocupación.
#' @param agrega Variable opcional para agrupar los datos del censo. Debe ser una cadena que representa el nombre de una columna en el conjunto de datos del censo.
#' @param filtro Valor opcional para filtrar los datos del censo. Debe ser un valor único correspondiente a la variable de agrupación especificada en 'agrega'.
#' @param Plot Un valor lógico que indica si se debe generar un gráfico. El valor predeterminado es FALSE.
#' @return Un data frame que contiene los resultados de los cálculos realizados en los datos del censo.
#' @examples
#' aux_desocupado(censo_vivienda,  agrega = NULL, Plot= TRUE, filtro = NULL)
#' aux_desocupado(censo_vivienda,  agrega = "DIST_ID",  Plot= TRUE, filtro = "10101")
#' @import dplyr
#' @importFrom ggplot2 ggtitle
#' @export

Pred_desocupado <- function(censo,
                           agrega = NULL,
                           filtro = NULL,
                           Plot = FALSE) {
  
  if (!is.null(agrega)) {
    censo <- censo %>% group_by(!!sym(agrega))
  }
  
  tab <- censo %>%
      summarise(
        total = sum(Desocupadas),
        Porcen = mean(Desocupadas) ,
        LimInf = (Porcen  - mean(MEInf_desocupadas)) * 100,
        LimSup = (Porcen  + mean(MESup_desocupadas)) * 100,
        Porcen = Porcen * 100,
        Len_IC = LimSup - LimInf,
        SE  = Len_IC/(2*1.96))
        
  
  if (Plot) {
    if (nrow(tab) == 1) {
      tab_temp <- tab
    } 
    
    if (!is.null(agrega)) {
      if (is.null(filtro)) {
        stop("El filtro no puede ser NULL")
      }  
      
      if (length(filtro) != 1) {
        stop("El filtro debe tener una longitud igual a 1")
      }
      
      tab_temp <- filter(tab, !!sym(agrega) == filtro)
    }
    
    p1 <- plot_densidad(
      media = tab_temp$Porcen,
      desviacion = tab_temp$SE, 
      lim_inf = tab_temp$LimInf,
      lim_sup = tab_temp$LimSup,
      dist = "Desocupadas"
    ) +
      ggtitle(label = paste0(agrega, " = ", filtro))
    
    print(p1)
    tab <- tab_temp
  }
  
  return(tab)
}


#' Función auxiliar para cálculos y visualizaciones de población total en un censo
#'
#' Esta función realiza cálculos y visualizaciones relacionadas con datos de la población total en un censo.
#'
#' @param censo El conjunto de datos del censo que contiene información sobre la población total.
#' @param agrega Variable opcional para agrupar los datos del censo. Debe ser una cadena que representa el nombre de una columna en el conjunto de datos del censo.
#' @param filtro Valor opcional para filtrar los datos del censo. Debe ser un valor único correspondiente a la variable de agrupación especificada en 'agrega'.
#' @param Plot Un valor lógico que indica si se debe generar un gráfico. El valor predeterminado es FALSE.
#' @return Un data frame que contiene los resultados de los cálculos realizados en los datos del censo.
#' @examples
#' aux_totPob(censo_data, agrega = "PROV_ID", filtro = "1", Plot = TRUE)
#' @import dplyr
#' @importFrom ggplot2 ggtitle
#' @export


Pred_totPob <- function(censo,
                       agrega = NULL,
                       filtro = NULL,
                       Plot = FALSE) {
  
  if (!is.null(agrega)) {
    censo <- censo %>% group_by(!!sym(agrega))
  }
  
  tab <- censo %>%
    summarise(
      total = sum(pred_per_viv),
      L1 = total - sum(MEInf_pred_per_viv),
      L2 = total  + sum(MESup_pred_per_viv),
      SE = (L2 - L1)/(2*qnorm(0.975)),
      LimInf = total - 1.64*SE,
      LimSup = total + 1.64*SE,
      Len_IC = LimSup - LimInf
    ) %>% mutate(L1 = NULL, 
                 L2 = NULL, 
    )
  
  if (Plot) {
    if (nrow(tab) == 1) {
      tab_temp <- tab
    }
    
    if (!is.null(agrega)) {
      if (is.null(filtro)) {
        stop("El filtro no puede ser NULL ")
      }
      
      if (length(filtro) != 1) {
        stop("El filtro debe ser de longitud igual a 1 ")
      }
      
      tab_temp <- filter(tab, !!sym(agrega) == filtro)
    }
    
    p1 <- plot_densidad(
      media = tab_temp$total,
      desviacion = tab_temp$SE,
      lim_inf = tab_temp$LimInf,
      lim_sup = tab_temp$LimSup
    ) +
      ggtitle(label = paste0(agrega, " = ", filtro))
    print(p1)
    
    tab <- tab_temp
  }
  
  tab
}


#' plot_piramide_pob
#'
#' Genera un gráfico de pirámide poblacional con barras y límites de confianza.
#'
#' @param data Un data frame que contiene los datos de población desagregados por sexo y edad.
#'             Debe contener las columnas "grupo", "total", "LimInf" y "LimSup".
#'             La columna "grupo" debe seguir el formato "Sexo_Edad" (ejemplo: "HOMBRES_GRUPO1").
#'             Las columnas "total", "LimInf" y "LimSup" deben contener los valores de población total,
#'             límite inferior y límite superior respectivamente.
#'
#' @return Un gráfico de pirámide poblacional.
#'
#' @examples
#' data <- data.frame(grupo = c("HOMBRES_GRUPO1", "MUJERES_GRUPO1"),
#'                    total = c(100, 150),
#'                    LimInf = c(90, 140),
#'                    LimSup = c(110, 160))
#' plot_piramide_pob(data)
#'
#' @import ggplot2
#' @import tidyr
#' @import dplyr
#'
#' @export
plot_piramide_pob <- function(data) {
  # Separar las columnas de Sexo y Edad en un nuevo data frame
  temp2 <- data %>%
    separate(grupo, sep = "_", into = c("Sexo", "Edad"))
  

  temp3 <- temp2 %>%
    select(Sexo:total) %>%
    spread(key = "Sexo", value = total) %>%
    mutate(
      Edad = factor(Edad, levels = paste0("GRUPO", 1:20))
    )
  
  # Obtener los límites de confianza para hombres y mujeres
  hombre_lims <- temp2 %>% filter(Sexo == "HOMBRES") %>% 
    select(Edad, LimInf, LimSup) %>% 
    mutate(
      Edad = factor(Edad, levels = paste0("GRUPO", 1:20))
    )
  
  mujeres_lims <- temp2 %>% filter(Sexo == "MUJERES") %>% 
    select(Edad, LimInf, LimSup) %>% 
    mutate(
      Edad = factor(Edad, levels = paste0("GRUPO", 1:20))
    )
  
  # Calcular el límite máximo para la escala del eje y
  lim_Max <- max(c(mujeres_lims$LimSup, hombre_lims$LimSup)) 
  
  # Generar el gráfico de pirámide poblacional utilizando ggplot
  ggplot(temp3) +
    geom_bar(aes(x = Edad, y = MUJERES, fill = "Mujeres"), stat = "identity") +
    geom_bar(aes(x = Edad, y = -HOMBRES, fill = "Hombres"), stat = "identity") +
    geom_errorbar(data = hombre_lims, aes(x = Edad, ymin = -LimInf, ymax = -LimSup), width = 0.2) +
    geom_errorbar(data = mujeres_lims, aes(x = Edad, ymin = LimInf, ymax = LimSup), width = 0.2) +
    scale_fill_manual(values = c("Mujeres" = "pink", "Hombres" = "lightblue")) +
    coord_flip() +
    labs(x = "Edad", y = "Población", title = "Pirámide Poblacional") +
    theme_minimal() +
    scale_y_continuous(
      breaks = round(seq(-lim_Max, lim_Max, len = 10), 2), 
      labels = round(abs(seq(-lim_Max, lim_Max, len = 10)))
    )
}


#' piramide_pob
#'
#' Calcula y visualiza la pirámide poblacional a partir de datos de censo.
#'
#' @param censo Un data frame que contiene los datos de censo, con columnas que representan
#'              variables de agregación y columnas que terminan en "_sum_personas", "_sum_MEInf",
#'              y "_sum_MESup" que representan los conteos de personas y los límites de confianza.
#' @param agrega Una cadena de texto opcional que indica la variable de agregación a utilizar para agrupar los datos.
#'               Si se proporciona, se calcularán las estadísticas por grupos de esa variable.
#' @param Plot Un valor lógico que indica si se debe generar el gráfico de la pirámide poblacional. Por defecto es TRUE.
#' @param filtro Un valor opcional que permite filtrar los datos por un valor específico de la variable de agregación.
#'               Solo se aplica si se proporciona la variable de agregación. Por defecto es "10110".
#'
#' @return Un data frame que contiene los datos resumidos de la pirámide poblacional.
#'
#' @examples
#' data <- data.frame(HOMBRES_GRUPO1_sum_personas = c(100, 150),
#'                    HOMBRES_GRUPO1_sum_MEInf = c(90, 140),
#'                    HOMBRES_GRUPO1_sum_MESup = c(110, 160),
#'                    MUJERES_GRUPO1_sum_personas = c(120, 140),
#'                    MUJERES_GRUPO1_sum_MEInf = c(110, 130),
#'                    MUJERES_GRUPO1_sum_MESup = c(130, 150))
#' piramide_pob(data, agrega = "VariableAgregacion")
#'
#' @import dplyr
#' @import tidyr
#' @import ggplot2
#' @export
piramide_pob <- function(censo,
                         agrega = NULL,
                         Plot = TRUE,
                         filtro = "10110"
) {
  if (!is.null(agrega)) {
    censo <- censo %>%  group_by(!!sym(agrega))
  }
  
  var_sel <-   grep(pattern = "sum$",
                    x = names(censo),
                    value = TRUE)
  var_sel <-
    gsub(pattern = "_sum",
         x = var_sel,
         replacement = "")
  
  tab <- purrr::map_dfr(var_sel, ~ {
    x <- .x
    censo %>% 
      summarise(
        total = sum(!!sym(paste0(x, "_sum"))),
        L1 = total - sum(!!sym(paste0(x, "_sum_MEInf"))),
        L2 = total + sum(!!sym(paste0(x, "_sum_MESup")))
      ) %>% mutate(grupo = .x)
  }) 
  
  if (!is.null(agrega)) {
    tab <- tab %>% 
      dplyr::transmute(
        !!sym(agrega),
        grupo,
        total,
        SE = (L2 - L1)/(2*qnorm(0.975)),
        LimInf = total - 1.64*SE,
        LimSup = total + 1.64*SE,
        Len_IC = LimSup - LimInf
      )
  } else {
    tab <- tab %>% 
      dplyr::transmute(
        grupo,
        total,
        SE = (L2 - L1)/(2*qnorm(0.975)),
        LimInf = total - 1.64*SE,
        LimSup = total + 1.64*SE,
        Len_IC = LimSup - LimInf
      ) 
  }
  
  if (Plot) {
    if (nrow(tab) == 40) {
      tab_temp <- tab
    } 
    
    if (!is.null(agrega)) {
      if (is.null(filtro)) {
        stop("El filtro no puede ser NULL")
      }  
      
      if (length(filtro) != 1) {
        stop("El filtro debe tener una longitud igual a 1")
      }
      
      tab_temp <- dplyr::filter(tab, !!sym(agrega) == filtro)
    }
    
    p1 <- plot_piramide_pob(tab_temp)+
      labs(subtitle = paste0(agrega, " = ", filtro))
    
    print(p1)
    tab <- tab_temp
  }
  
  tab
  
}


