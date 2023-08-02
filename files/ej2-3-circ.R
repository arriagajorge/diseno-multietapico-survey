circ <- c(2, 1, 1, 3, 2, 5, 3, 1, 4, 1, 2, 4, 5, 1, 5, 5, 4, 1, 2, 3, 4, 
          2, 3, 2, 1, 1, 3, 2, 4, 3, 3, 2)
midf3 <- df[, c("NOMBRE_ESTADO", "NOMBRE_DISTRITO", "ID_DISTRITO", "ID_ESTADO", "distritoelec")]
midf3$estimacion <- midf$estimacion
NOMBRE_ESTADO <- unique(df$NOMBRE_ESTADO)
circdf <- data.frame(circ, NOMBRE_ESTADO)

#sum(circdf[,1] == 5)
midf3 <- merge(circdf, midf3, by="NOMBRE_ESTADO")
#df$distritoelec <- paste(df$ID_ESTADO, df$ID_DISTRITO, df$NOMBRE_DISTRITO, sep="-")
#midf3$distritoelec <- df$distritoelec

# otorgamos id a cada distrito
iddistrito = 1:300
distritoelec = unique(df$distritoelec)
idsdist <- data.frame(iddistrito, distritoelec)
midf3 <- merge(midf3, idsdist)

#agrupara cada estrato
circ <- midf3 %>% filter(circ == 1)
circ2 <- midf3 %>% filter(circ == 2)
circ3 <- midf3 %>% filter(circ == 3)
circ4 <- midf3 %>% filter(circ == 4)

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

subid <- c()
npob <- c()
for (i in unique(circ2$iddistrito)){
  s0 = circ2$distritoelec[circ2$iddistrito == i]
  subid = c(subid, 1:length(s0))
  npob <- c(npob, rep(length(s0), length(s0)))
}
circ2$subid <- subid
circ2$npob <- npob
#pesos
circ2$wk <- 1/(8/length(unique(circ2$iddistrito))*30/circ2$npob)
#relaciona el numero de distrito con el numero de casillas
prob.c2 <- data.frame(circ2 %>% group_by(iddistrito) %>% count())
#idunico
circ2$idun <- paste(circ2$iddistrito, circ2$subid, sep="-")

subid <- c()
npob <- c()
for (i in unique(circ3$iddistrito)){
  s0 = circ3$distritoelec[circ3$iddistrito == i]
  subid = c(subid, 1:length(s0))
  npob <- c(npob, rep(length(s0), length(s0)))
}
circ3$subid <- subid
circ3$npob <- npob
#pesos
circ3$wk <- 1/(8/length(unique(circ3$iddistrito))*30/circ3$npob)
#relaciona el numero de distrito con el numero de casillas
prob.c3 <- data.frame(circ3 %>% group_by(iddistrito) %>% count())
#idunico
circ3$idun <- paste(circ3$iddistrito, circ3$subid, sep="-")

subid <- c()
npob <- c()
for (i in unique(circ4$iddistrito)){
  s0 = circ4$distritoelec[circ4$iddistrito == i]
  subid = c(subid, 1:length(s0))
  npob <- c(npob, rep(length(s0), length(s0)))
}
circ4$subid <- subid
circ4$npob <- npob
#pesos
circ4$wk <- 1/(8/length(unique(circ4$iddistrito))*30/circ4$npob)
#relaciona el numero de distrito con el numero de casillas
prob.c4 <- data.frame(circ4 %>% group_by(iddistrito) %>% count())
#idunico
circ4$idun <- paste(circ4$iddistrito, circ4$subid, sep="-")









diseno.est.c1 <- function(){
  seleccion <-  sample(unique(circ1$iddistrito), size=8)
  idun <- c()
  for (i in seleccion) {
    iddist <- prob.c1 %>% filter(iddistrito == i)
    idun <- c(idun, paste(i, sample(iddist$n, size = 30), sep="-"))
  }
  #obtenemos los datos de la muestra
  finaldfc1 <- left_join(data.frame(idun), circ1, by = "idun")
  #estimacion con survey
  diseno.c1 <- svydesign(data = finaldfc1, id=~iddistrito, weights =~wk)
  c1.mean <- svymean(~estimacion, diseno.c1)
  return(coef(c1.mean))
}
diseno.est.c1()

diseno.est.c2 <- function(){
  seleccion <-  sample(unique(circ2$iddistrito), size=8)
  idun <- c()
  for (i in seleccion) {
    iddist <- prob.c2 %>% filter(iddistrito == i)
    idun <- c(idun, paste(i, sample(iddist$n, size = 30), sep="-"))
  }
  #obtenemos los datos de la muestra
  finaldfc2 <- left_join(data.frame(idun), circ2, by = "idun")
  #estimacion con survey
  diseno.c2 <- svydesign(data = finaldfc2, id=~iddistrito, weights =~wk)
  c2.mean <- svymean(~estimacion, diseno.c2)
  summary(diseno.c2)
  
  #sum(finaldfc2$estimacion*finaldfc2$wk)/sum(finaldfc2$wk)
  #sum(1/Pob[Indices[[x]],"pik"])
  svymean(~estimacion, diseno.c2)
  return(coef(c2.mean))
}
diseno.est.c2()

diseno.est.c3 <- function(){
  seleccion <-  sample(unique(circ3$iddistrito), size=8)
  idun <- c()
  for (i in seleccion) {
    iddist <- prob.c3 %>% filter(iddistrito == i)
    idun <- c(idun, paste(i, sample(iddist$n, size = 30), sep="-"))
  }
  #obtenemos los datos de la muestra
  finaldfc3 <- left_join(data.frame(idun), circ3, by = "idun")
  #estimacion con survey
  diseno.c3 <- svydesign(data = finaldfc3, id=~iddistrito, weights =~wk)
  c3.mean <- svymean(~estimacion, diseno.c3)
  return(coef(c3.mean))
}
diseno.est.c3()

diseno.est.c4 <- function(){
  seleccion <-  sample(unique(circ4$iddistrito), size=8)
  idun <- c()
  for (i in seleccion) {
    iddist <- prob.c4 %>% filter(iddistrito == i)
    idun <- c(idun, paste(i, sample(iddist$n, size = 30), sep="-"))
  }
  #obtenemos los datos de la muestra
  finaldfc4 <- left_join(data.frame(idun), circ4, by = "idun")
  #estimacion con survey
  diseno.c4 <- svydesign(data = finaldfc4, id=~iddistrito, weights =~wk)
  c4.mean <- svymean(~estimacion, diseno.c4)
  return(coef(c4.mean))
}
diseno.est.c4()


a <- matrix(0, nrow = 2, ncol = 2)
b <- matrix(0, nrow = 2, ncol = 2)
