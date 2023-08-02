setwd("D:/Notas/Muestreo/Exámen/examen02/Bases de datos")

thogar <- data.table::fread("THOGAR.csv")

#P3A1_1 es la variable a la que nos interesa replicar la estimación

# primero observamos que no hay N.A. por lo cual no es necesario hacer correciones
thogar2 <- thogar %>% drop_na(P3A1_1)
length(thogar2$P3A1_1) == length(thogar$P3A1_1)

#vemos las variables para ...
summary(thogar[, c("UPM_DIS", "EST_DIS", "FACTOR")])



#los que si tienen necesidad
# segun la estructura de archivo
# el número 1 corresponde a si tiene necesida
a <- sum(thogar$P3A1_1 == 1)

b <- sum(thogar$P3A1_1 == 2)

c <- sum(thogar$P3A1_1 == 9)

sum(a + b + c) == length(thogar$P3A1_1)

#los que tienen necesida
thogar$si <- as.numeric(thogar$P3A1_1 == 1)
# los que no tienen necesidad
thogar$no <- as.numeric(thogar$P3A1_1 == 2)
# los que no especificaran
thogar$ne <- as.numeric(thogar$P3A1_1 == 9)
# para calcular el total
thogar$total <- 1

library(survey)
options(survey.lonely.psu="adjust")

#definimos el diseño
dsg.envi <- svydesign(id=~UPM_DIS, strat=~EST_DIS, weight =~FACTOR,
                      data = thogar, nest=T)

summary(dsg.envi)

#por nivel nacional
svymean(~si + no + ne, dsg.envi)*100
svytotal(~si + no + ne, dsg.envi)
svytotal(~total, dsg.envi)

rel.nac <- svymean(~si + no + ne, dsg.envi)*100
abs.nac <- svytotal(~si + no + ne, dsg.envi)
total.nac <- svytotal(~total, dsg.envi)

# por entidadad 
svyby(~si +no +ne,~ENT,design=dsg.envi, svymean)*100
svyby(~si + no + ne,~ENT,design=dsg.envi, svytotal)
svyby(~total, ~ENT,design=dsg.envi, svytotal)


rel.ent <- svyby(~si +no + ne,~ENT,design=dsg.envi, svymean)
abs.ent <- svyby(~si+ no + ne,~ENT,design=dsg.envi, svytotal)
total.ent <- svyby(~total, ~ENT,design=dsg.envi, svytotal)

#anexar los nombres de las entidades (para hacer las tablas)
entidades=c("AGU", "BCN", "BCS", "CAM", "COA", "COL", "CHP", "CHH", "CMX", "DUR", "GUA", "GRO", "HID", "JAL", "MEX", "MIC", "MOR", "NAY", "NLE",
            "OAX", "PUE", "QUE", "ROO", "SLP", "SIN", "SON", "TAB", "TAM", "TLA", "VER", "YUC", "ZAC")

# intervalos de confianza
# a nivel nacional
confint(rel.nac)
confint(abs.nac)
confint(total.nac)
# a nivel entidad
confint(rel.ent)*100
confint(abs.ent)
confint(total.ent)

class(rel.ent)