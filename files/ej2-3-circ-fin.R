library(tidyverse)
library(survey)
library(sampling)

# leer el csv
setwd("D:/Notas/Muestreo/Exámen/examen02/20210611_1000_CW_diputaciones")
options(digits=10)

df <- data.table::fread("diputaciones - copia.csv", sep="|")
#solo nos interesa las casillas donde si hubo votaciones
df <- filter(df, TOTAL_VOTOS_CALCULADOS != 0)
df <- df %>%drop_na(TOTAL_VOTOS_CALCULADOS)
df <- df %>%drop_na(`VOTOS NULOS`)

df$totalvotos = df$TOTAL_VOTOS_CALCULADOS - df$`VOTOS NULOS`
df <- filter(df, totalvotos != 0)
#tama~no de poblaci ́on
N <- dim(df)[1]
#tama~no de muestra
n <- 1200

#datos que nos interesan
midf <- df[, c("MORENA", "PVEM", "PT", "PT-MORENA",
               "PVEM-PT", "PVEM-MORENA", "PVEM-PT-MORENA", "totalvotos")]
midf$estimacion = apply(midf,1, function(x) (sum(x)- x[length(x)])/x[length(x)])
#pesos para el mas
midf$wk.mas = N/n


diseno.mas <- function(){
  #seleccionamos muestra
  seleccion_muestra <- sample(1:N ,size = 1200)
  #estimamos
  muestra <- midf[seleccion_muestra, c("estimacion", "wk.mas")]
  #definimos nuestro dise~no muestral
  mas <- svydesign(id=~1, weights=~wk.mas, data=muestra)
  #calculamos la media con el estimador de razon
  media = svymean(~estimacion, mas)
  return(media)
}
#correr cuando sea necesario
simulacion.mas <- replicate(1000, diseno.mas())
mean(simulacion.mas)
hist(simulacion.mas)

# #estratos
# df$distritoelec <- paste(df$ID_ESTADO, df$ID_DISTRITO, df$NOMBRE_DISTRITO, sep="-")
# #revisamos el tamaño deben ser  300 unicos
# length(unique(df$distritoelec))
# midf2 <- df[, c("distritoelec")]
# midf2$estimacion <- midf$estimacion


#diseno.est  <- function(x){
  
#  s = strata(midf2, c("distritoelec"), size=rep(4,300), method = "srswor")
#  muestra <- getdata(midf2, s)
  
  #esimaciones
#  est.dsg <- svydesign(id = ~1, strata =~Stratum, probs = Prob, 
                       #data=muestra)
#  media = svymean(~estimacion, est.dsg)
#  return(media)
#}

# simulacion.est <- rep(0, 1000)
# for (i in 1:1000){
#   s = strata(midf2, c("distritoelec"), size=rep(4,300), method = "srswor")
#   muestra <- getdata(midf2, s)
# 
#   #esimaciones
#   est.dsg <- svydesign(id = ~1, strata =~Stratum, prob =~ Prob, 
#                      data=muestra)
#   media = svymean(~estimacion, est.dsg)
#   simulacion.est[i] <- coef(media)
# }
# 
# 
# for (i in 1:1000){
#   muestra <- getdata(midf2, strata(midf2, c("distritoelec"), size=rep(4,300), method = "srswor"))
#   
#   #esimaciones
#   est.dsg <- svydesign(id = ~1, strata =~Stratum, prob =~ Prob, 
#                        data=muestra)
#   media = svymean(~estimacion, est.dsg)
#   simulacion.est[i] <- coef(media)
# }
# simulacion.est
# mean(simulacion.est)
# hist(simulacion.est)
# shapiro.test(simulacion.est)
# nortest::ad.test(simulacion.est)
# plot(ecdf(simulacion.est))
# curve(pnorm(x, mean(simulacion.est), sd(simulacion.est)), add=T)
# 

# #estratos
df$distritoelec <- paste(df$ID_ESTADO, df$ID_DISTRITO, df$NOMBRE_DISTRITO, sep="-")
#revisamos el tamaño deben ser  300 unicos
length(unique(df$distritoelec))
midf2 <- df[, c("distritoelec")]
midf2$estimacion <- midf$estimacion

# otorgamos id a cada distrito
iddistrito = 1:300
distritoelec = unique(df$distritoelec)
idsdist <- data.frame(iddistrito, distritoelec)
midf2 <- merge(midf2, idsdist)

#calculo de las probabilidades de inclusión para todos los elementos
prob.inc <- data.frame(midf2 %>% group_by(iddistrito) %>% count())
prob.inc$wk <- 1/(4/prob.inc$n)

midf2 <- merge(midf2, prob.inc, by="iddistrito")

#creamos un subid (id dentro del distrito electoral)
subid <- c()
for (i in 1:300){
  s0 = midf2$distritoelec[midf2$iddistrito == i]
  subid = c(subid, 1:length(s0))
}
midf2$subid <- subid

# creamos un id unico en toda la  ppoblacion
midf2$idun <- paste(midf2$iddistrito, midf2$subid, sep = "-")

# df5 <- data.frame(c(), c())
# for (i in 1:300){
#   s0 = df4$distritoelec[df4$iddistrito == i]
#   muestra = df4[sample(1:length(s0), size = 4), c("estimacion", "wk", "iddistrito")]
#   df5 <- union_all(muestra, df5)
# }
# vv

#prob.inc
disenost <- function(){
  #seleccionamos muestra con base en el iduniico
  idun <- c()
  for (i in 1:300) {
    idun <- c(idun, paste(i, sample(1:prob.inc$n[i], size = 4), sep="-"))
  }
  seleccion <- data.frame(idun)
  #obtenemos los datos de la muestra
  finaldf <- left_join(seleccion, midf2, by = "idun")
  
  #esimaciones
  est.dsg <- svydesign(id = ~1, strata =~iddistrito, weights =~ wk, 
                       data=finaldf)
  media = svymean(~estimacion, est.dsg)
  return(coef(media))
}
simulacion.estr <- replicate(1000, disenost())
mean(simulacion.estr)
sd(simulacion.estr)
hist(simulacion.estr)

 
######################################################
#III.
length(unique(df$NOMBRE_ESTADO))

# uno <- c("BAJA CALIFORNIA", "BAJA CALIFORNIA SUR", "CHIHUAHUA", "DURANGO", 
#          "JALISCO", "NAYARIT", "SINALOA", "SONORA")
# dos <- c()

circunscripcion <- c(2, 1, 1, 1,
                     


#circ <- c(2, 1, 1, 3, 2, 5, 3, 1, 4, 1, 2, 4, 5, 1, 5, 5, 4, 1, 2, 3, 4, 2, 3, 2,
          1, 1, 3, 2, 4, 3, 3, 2)
#circ <- 0

circ <- c(2, 1, 1, 3, 2, 5, 3, 1, 4, 1, 2, 4, 5, 1, 5, 5, 4, 1, 2, 3, 4, 2, 3, 2, 1, 1, 3, 2, 4, 3, 3, 2)


midf3 <- df[, c("NOMBRE_ESTADO")]
midf3$estimacion <- midf$estimacion

NOMBRE_ESTADO <- unique(df$NOMBRE_ESTADO)
circdf <- data.frame(circ, NOMBRE_ESTADO)

#sum(circdf[,1] == 5)


midf3 <- merge(circdf, midf3, by="NOMBRE_ESTADO")
midf3$distritoelec <- midf2$distritoelec
midf3$iddistrito <- midf2$iddistrito

# midf3$idun  <- midf2$idun

# xd <- midf3 %>% group_by(circ)
# 
# iddistrito <-  sample(1:30, size=8)
# df.idf.dist <- left_join(data.frame(iddistrito), midf3, by="iddistrito")
# length(df.id.dist$iddistrito)
# 

#agrupara cada estrato
circ1 <- midf3 %>% filter(circ == 1)
#circ1$wk <- 1/(8/length(unique(circ1$iddistrito))*30/circ1$n)
subid <- c()
npob <- c()
for (i in unique(circ1$iddistrito)){
  s0 = circ1$distritoelec[circ1$iddistrito == i]
  subid = c(subid, 1:length(s0))
  npob <- c(npob, rep(length(s0), length(s0)))
}
circ1$subid <- subid
circ1$npob <- npob
#pesos
circ1$wk <- 1/(8/length(unique(circ1$iddistrito))*30/circ1$npob)
#relaciona el numero de distrito con el numero de casillas
prob.c1 <- data.frame(circ1 %>% group_by(iddistrito) %>% count())
#idunico
circ1$idun <- paste(circ1$iddistrito, circ1$subid, sep="-")



circ2 <- midf3 %>% filter(circ == 2)
circ2$wk <- 1/(8/length(unique(circ2$iddistrito))*30/circ2$n)
circ3 <- midf3 %>% filter(circ == 3)
circ3$wk <- 1/(8/length(unique(circ3$iddistrito))*30/circ3$n)
circ4 <- midf3 %>% filter(circ == 4)
circ4$wk <- 1/(8/length(unique(circ4$iddistrito))*30/circ4$n)
circ5 <- midf3 %>% filter(circ == 5)
circ5$wk <- 1/(8/length(unique(circ5$iddistrito))*30/circ5$n)
  
diseno.est.als <- function(){
  iddistrito <-  sample(unique(circ1$iddistrito), size=8)
  cir1.dist.sample <- left_join(data.frame(iddistrito), circ1, by="iddistrito")
  idun <- c()
  for (i in unique(cir1.dist.sample$iddistrito)) {
    idun <- c(idun, paste(i, sample(prob.c1$n[i], size = 30), sep="-"))
  }
  #obtenemos los datos de la muestra
  finaldfc1 <- left_join(data.frame(idun), cir1.dist.sample, by = "idun")
  #estimacion con survey
  diseno.c1 <- svydesign(data = finaldfc1, id=~iddistrito, weights =~wk)
  c1.mean <- svymean(~estimacion, diseno.c1)
  
  
  iddistrito <-  sample(unique(circ2$iddistrito), size=8)
  cir2.dist.sample <- left_join(data.frame(iddistrito), circ2, by="iddistrito")
  idun <- c()
  for (i in unique(cir2.dist.sample$iddistrito)) {
    idun <- c(idun, paste(i, sample(1:prob.inc$n[i], size = 30), sep="-"))
  }
  #obtenemos los datos de la muestra
  finaldfc2 <- left_join(data.frame(idun), cir2.dist.sample, by = "idun")
  #estimacion con survey
  diseno.c2 <- svydesign(data = finaldfc2, id=~iddistrito, weights =~wk)
  c2.mean <- svymean(~estimacion, diseno.c2)
  
  iddistrito <-  sample(unique(circ3$iddistrito), size=8)
  cir3.dist.sample <- left_join(data.frame(iddistrito), circ3, by="iddistrito")
  idun <- c()
  for (i in unique(cir3.dist.sample$iddistrito)) {
    idun <- c(idun, paste(i, sample(1:prob.inc$n[i], size = 30), sep="-"))
  }
  #obtenemos los datos de la muestra
  finaldfc3 <- left_join(data.frame(idun), cir3.dist.sample, by = "idun")
  #estimacion con survey
  diseno.c3 <- svydesign(data = finaldfc3, id=~iddistrito, weights =~wk)
  c3.mean <- svymean(~estimacion, diseno.c3)
  
  iddistrito <-  sample(unique(circ4$iddistrito), size=8)
  cir4.dist.sample <- left_join(data.frame(iddistrito), circ4, by="iddistrito")
  idun <- c()
  for (i in unique(cir4.dist.sample$iddistrito)) {
    idun <- c(idun, paste(i, sample(1:prob.c1$n[i], size = 30), sep="-"))
  }
  #obtenemos los datos de la muestra
  finaldfc4 <- inner_join(data.frame(idun), cir4.dist.sample, by = "idun")
  #estimacion con survey
  diseno.c4 <- svydesign(data = finaldfc4, id=~iddistrito, weights =~wk)
  c4.mean <- svymean(~estimacion, diseno.c4)
  
  
  
  
  iddistrito <-  sample(unique(circ5$iddistrito), size=8)
  cir5.dist.sample <- left_join(data.frame(iddistrito), circ5, by="iddistrito")
  idun <- c()
  for (i in unique(cir5.dist.sample$iddistrito)) {
    idun <- c(idun, paste(i, sample(prob$n[i], size = 30), sep="-")) #cambbar el prob
  }
  #obtenemos los datos de la muestra
  finaldfc5 <- left_join(data.frame(idun), cir5.dist.sample, by = "idun")
  #estimacion con survey
  diseno.c5 <- svydesign(data = finaldfc5, id=~iddistrito, weights =~wk)
  c5.mean <- svymean(~estimacion, diseno.c5)
  
  return(mean(c(c1.mean, c2.mean, c3.mean, c4.mean, c5.mean)))
  
}


sample(c("a","b","c"), 2)



xd <- replicate(10, diseno.est.als())

#probabilidades de inclusión
#diseño survey

finaldfc1$posize = length(unique(circ1$iddistrito)) 

diseno.iii <- svydesign(data = finaldfc1, id=~iddistrito, weights =~wk, fpc=~posize)
summary(diseno.iii)
svymean(~estimacion, diseno.iii)


diseno.c1 <- svydesign(data = finaldfc1, id=~iddistrito, weights =~wk)
svymean(~estimacion, diseno.c1)


