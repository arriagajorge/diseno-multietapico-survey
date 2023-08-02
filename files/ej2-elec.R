setwd("D:/Notas/Muestreo/Exámen/examen02/20210611_1000_CW_diputaciones")
options(digits=10)

library(tidyverse)
library(data.table)
library(survey)

# leer el csv
df <- data.table::fread("diputaciones - copia.csv", sep="|")

Summary(df$TOTAL_VOTOS_CALCULADOS)

#solo no interesa las casillas donde si hubo votaciones
df <- filter(df, TOTAL_VOTOS_CALCULADOS != 0)
df <- df %>% 
  drop_na(TOTAL_VOTOS_CALCULADOS)

df <- filter(df, TOTAL_VOTOS_CALCULADOS != 0)
df <- df %>%drop_na(TOTAL_VOTOS_CALCULADOS)


#tamaño de población
N <- length(df$CLAVE_CASILLA)
#tamaño de muestra
n <- 1200


diseno.mas <-function(){
  #seleccionamos muestra
  seleccion_muestra <- sample(1:N ,size = 1200)
  #obtenemos los datos que nos interesan
  muestra <- df[seleccion_muestra, c("MORENA", "PVEM", "PT", "PT-MORENA",
                                     "PVEM-PT", "PVEM-MORENA", "PVEM-PT-MORENA", "VOTOS NULOS", "TOTAL_VOTOS_CALCULADOS")]
  
  #calculamos el total de votos, quitamos los votos nulos
  muestra$totalvotos = muestra$TOTAL_VOTOS_CALCULADOS - muestra$`VOTOS NULOS`
  
  #eliminamos las columnas  que ya no nos interesan
  muestra <- muestra%>% select (-TOTAL_VOTOS_CALCULADOS)
  muestra <- muestra%>% select (-`VOTOS NULOS`)

  #estimamos
  muestra$estimacion = apply(muestra,1, function(x) (sum(x)- x[length(x)])/x[length(x)])
  #pesos
  muestra$wk = N/n
  
  
  #definimos nuestro diseño muestral
  mas <- svydesign(id=~1, weights=~wk, data=muestra)
  #calculamos la media con el estimador de razon
  media = svymean(~estimacion, mas)  
  return(media)
}

simulacion.mas <- replicate(1000, diseno.mas())
simulacion.mas

mean(simulacion.mas)
hist(simulacion.mas)
# muestro estratificado



df2 <- data.table::fread("diputaciones - copia.csv", sep="|")
length( unique(df2$NOMBRE_DISTRITO) )

name_dist <- df2$NOMBRE_DISTRITO
id_distbyname <- df2$ID_DISTRITO
name_est <- df$NOMBRE_ESTADO

est <- data.frame(name_dist, id_distbyname)

lent <- paste(name_dist, id_distbyname, sep="-")
lent2 <- paste(lent, df$)
length(unique(lent))



install.packages("sampling")
