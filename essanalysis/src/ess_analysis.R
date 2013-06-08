# Análisis multivariado sobre valores y política de la Encuesta Social Europea
# Véase: http://ess.nsd.uib.no/downloadwizard/
# ============================================================================
library (rpart)
library (lattice)
library (randomForest)
library(reshape)

#setwd("/home/fontanon/Dropbox/Devel/dataanalysis-ess/essanalysis")
ess <- read.csv("data/ess.csv")

# Función para mostrar de forma ordenada Mean Decrease Accuracy y Mean Decrease Gini 
sort_importance <- function(model, sort_by='MeanDecreaseAccuracy') {
  model_importance <- data.frame(importance(model))
  model_importance <- sort_df(model_importance,vars=sort_by)
  model_importance
}


# Regresión logística en variables "Escala de Valores Humanos" de 
# la Encuesta Social Europea
# -----------------------------------------------------------------

# Filtro para evitamos contabilizar las respuestas tipo "no lo se", "no contesta"
wrk <- ess[ess$wrkorg <=2,]
wrk <- wrk[wrk$wrkprty <= 2,]

# Regresión logística para afiliados a partidos políticos
partyglm <- glm(as.factor(wrkprty) ~ ipcrtiv + imprich + 
                            ipeqopt + ipshabt + impsafe + impdiff + 
                            ipfrule + ipudrst + ipmodst + ipgdtim + 
                            impdiff + iphlppl + ipsuces + ipstrgv + 
                            ipcrtiv + ipbhprp + iprspot + iplylfr + 
                            impenv +  imptrad + impfun, 
                          data=subset(wrk, mmbprty ==1 & wrkorg==2), 
                          na.action=na.omit, family=binomial)

# Regresión logística para miembros de grupos de acción política
pacglm <- glm(as.factor(wrkprty) ~ ipcrtiv + imprich + 
                          ipeqopt + ipshabt + impsafe + impdiff + 
                          ipfrule + ipudrst + ipmodst + ipgdtim + 
                          impdiff + iphlppl + ipsuces + ipstrgv + 
                          ipcrtiv + ipbhprp + iprspot + iplylfr + 
                          impenv +  imptrad + impfun, 
                        data=subset(wrk, mmbprty ==2 & wrkorg ==2), 
                        na.action=na.omit, family=binomial)

# Regresión logística para miembros de otras asociaciones u organizaciones
orgglm <- glm(as.factor(wrkorg) ~ ipcrtiv + imprich + ipeqopt + 
                          ipshabt + impsafe + impdiff + ipfrule + ipudrst + 
                          ipmodst + ipgdtim + impdiff + iphlppl + ipsuces + 
                          ipstrgv + ipcrtiv + ipbhprp + iprspot + iplylfr + 
                          impenv +  imptrad + impfun, 
                        data=subset(wrk, mmbprty ==2), 
                        na.action=na.omit, family=binomial)

# Correlaciones poco significativas para miembros de grupos de acción política, 
# muy significativas en miembros de otras organizaciones y sin correlación para afiliados
# a partidos políticos.
summary(partyglm)
summary(pacglm)
summary(orgglm)


# Regresión logística en variables "Politica: incluyendo interes, 
# eficiencia, confianza ..." de la Encuesta Social Europea.
# -----------------------------------------------------------------

# Regresión logística para afiliados a partidos políticos
party2glm <- glm(as.factor(wrkprty) ~ polintr + polcmpl + poldcs + trstprl + trstlgl +
                   trstplc + trstplt + trstprt + trstep + trstep + trstun + vote + contplt +
                   badge + sgnptit + pbldmn + bctprd + clsprty + prtdgcl + mmbprty +
                   lrscale + stflife + stfeco + stfgov + stfdem + stfedu + stfhlth + gincdif +
                   freehms + prtyban + scnsenv + euftf + imsmetn + imdfetn + impcntr + imbgeco + 
                   imueclt + imwbcnt,
                data=subset(wrk, mmbprty ==1 & wrkorg==2), 
                na.action=na.omit, family=binomial, control=glm.control(maxit=50))

# Regresión logística para miembros de grupos de acción política
pac2glm <- glm(as.factor(wrkprty) ~ polintr + polcmpl + poldcs + trstprl + trstlgl +
              trstplc + trstplt + trstprt + trstep + trstep + trstun + vote + contplt +
              badge + sgnptit + pbldmn + bctprd + clsprty + prtdgcl + mmbprty +
              lrscale + stflife + stfeco + stfgov + stfdem + stfedu + stfhlth + gincdif +
              freehms + prtyban + scnsenv + euftf + imsmetn + imdfetn + impcntr + imbgeco + 
              imueclt + imwbcnt,
              data=subset(wrk, mmbprty ==2 & wrkorg ==2), 
              na.action=na.omit, family=binomial, control=glm.control(maxit=50))

# Regresión logística para miembros de otras asociaciones u organizaciones
org2glm <- glm(as.factor(wrkorg) ~ polintr + polcmpl + poldcs + trstprl + trstlgl +
                 trstplc + trstplt + trstprt + trstep + trstep + trstun + vote + contplt +
                 badge + sgnptit + pbldmn + bctprd + clsprty + prtdgcl + mmbprty +
                 lrscale + stflife + stfeco + stfgov + stfdem + stfedu + stfhlth + gincdif +
                 freehms + prtyban + scnsenv + euftf + imsmetn + imdfetn + impcntr + imbgeco + 
                 imueclt + imwbcnt,
              data=subset(wrk, mmbprty ==2), 
              na.action=na.omit, family=binomial, control=glm.control(maxit=50))

# Correlaciones poco significativas para miembros de grupos de acción política, 
# muy significativas en miembros de otras organizaciones y sin correlación para afiliados
# a partidos políticos.
summary(partyglm)
summary(pacglm)
summary(orgglm)


# Exploración de variables de interés para el estudio
# -----------------------------------------------------------------

# Satisfacción con la democracia en España
wrk <- ess[ess$stfdem <= 10,]
stfdem <- cbind(
  prop.table(table(subset(wrk, mmbprty ==1 & wrkprty ==1)$stfdem, subset(wrk, mmbprty ==1 & wrkprty ==1)$wrkprty), 2),
  prop.table(table(subset(wrk, mmbprty !=1 & wrkprty ==1)$stfdem, subset(wrk, mmbprty !=1 & wrkprty ==1)$wrkprty), 2),
  prop.table(table(subset(wrk, wrkorg ==1)$stfdem, subset(wrk, wrkorg ==1)$wrkorg), 2),
  prop.table(table(subset(wrk, mmbprty !=1 & wrkprty !=1 & wrkorg !=1)$stfdem))
)
dimnames(stfdem)[[1]]=c("Ninguna", "n1","n2","n3","n4","n5","n6","n7","n8","n9","Total")
dimnames(stfdem)[[2]]=c("Afiliado a partido", "Miembro Grupo Acción Política", "Miembro Otras Organizaciones", "No Organizado")

barplot(stfdem, beside=TRUE,
        main="Satisfacción con la forma en la que la democracia funciona en España", 
        ylab="Porcentaje de respuestas")

# Confianza en partidos políticos
prop.table(table(subset(wrk, mmbprty ==1 & wrkprty ==1)$trstprt, subset(wrk, mmbprty ==1 & wrkprty ==1)$wrkprty), 2)
prop.table(table(subset(wrk, mmbprty !=1 & wrkprty ==1)$trstprt, subset(wrk, mmbprty !=1 & wrkprty ==1)$wrkprty), 2)
prop.table(table(subset(wrk, wrkorg ==1)$trstprt, subset(wrk, wrkorg ==1)$wrkorg), 2)
prop.table(table(subset(wrk, mmbprty !=1 & wrkprty !=1 & wrkorg !=1)$trstprt))

# Posición en el eje izquierda/derecha
wrk <- ess[ess$lrscale <= 10,]
lrscale <- cbind(
  prop.table(table(subset(wrk, mmbprty ==1 & wrkprty ==1)$lrscale, subset(wrk, mmbprty ==1 & wrkprty ==1)$wrkprty), 2),
  prop.table(table(subset(wrk, mmbprty !=1 & wrkprty ==1)$lrscale, subset(wrk, mmbprty !=1 & wrkprty ==1)$wrkprty), 2),
  prop.table(table(subset(wrk, wrkorg ==1)$lrscale, subset(wrk, wrkorg ==1)$wrkorg), 2),
  prop.table(table(subset(wrk, mmbprty !=1 & wrkprty !=1 & wrkorg !=1)$lrscale))
)
dimnames(lrscale)[[1]]=c("Ninguna", "n1","n2","n3","n4","n5","n6","n7","n8","n9","Total")
dimnames(lrscale)[[2]]=c("Partido Político", "Grupo Acción Política",	"Otras Organizaciones", "Resto de ciudadanos")

barplot(lrscale, beside=TRUE,
        main="Posicionamiento ideológico en base a la afiliación", 
        ylab="Porcentaje de respuestas")

# Confianza en el Parlamento Europeo
wrk <- ess[ess$trstep <= 10,]
trstep <- cbind(
  prop.table(table(subset(wrk, mmbprty ==1 & wrkprty ==1)$trstep, subset(wrk, mmbprty ==1 & wrkprty ==1)$wrkprty), 2),
  prop.table(table(subset(wrk, mmbprty !=1 & wrkprty ==1)$trstep, subset(wrk, mmbprty !=1 & wrkprty ==1)$wrkprty), 2),
  prop.table(table(subset(wrk, wrkorg ==1)$trstep, subset(wrk, wrkorg ==1)$wrkorg), 2),
  prop.table(table(subset(wrk, mmbprty !=1 & wrkprty !=1 & wrkorg !=1)$trstep))
)
dimnames(trstep)[[1]]=c("Ninguna", "n1","n2","n3","n4","n5","n6","n7","n8","n9","Total")
dimnames(trstep)[[2]]=c("Afiliado a partido", "Miembro Grupo Acción Política", "Miembro Otras Organizaciones", "No Organizado")

barplot(trstep, beside=TRUE,
        main="Confianza en el Parlamento Europeo", 
        ylab="Porcentaje de respuestas")

# La Unión Europea: ¿debería ir más lejos o ha ido demasiado lejos?
wrk <- ess[ess$euftf <= 10,]
prop.table(table(subset(wrk, mmbprty ==1 & wrkprty ==1)$euftf, subset(wrk, mmbprty ==1 & wrkprty ==1)$wrkprty), 2)
prop.table(table(subset(wrk, mmbprty !=1 & wrkprty ==1)$euftf, subset(wrk, mmbprty !=1 & wrkprty ==1)$wrkprty), 2)
prop.table(table(subset(wrk, wrkorg ==1)$euftf, subset(wrk, wrkorg ==1)$wrkorg), 2)
prop.table(table(subset(wrk, mmbprty !=1 & wrkprty !=1 & wrkorg !=1)$euftf))

# Confianza en Naciones Unidas
wrk <- ess[ess$trstun <= 10,]
trstun <- cbind(
  prop.table(table(subset(wrk, mmbprty ==1 & wrkprty ==1)$trstun, subset(wrk, mmbprty ==1 & wrkprty ==1)$wrkprty), 2),
  prop.table(table(subset(wrk, mmbprty !=1 & wrkprty ==1)$trstun, subset(wrk, mmbprty !=1 & wrkprty ==1)$wrkprty), 2),
  prop.table(table(subset(wrk, wrkorg ==1)$trstun, subset(wrk, wrkorg ==1)$wrkorg), 2),
  prop.table(table(subset(wrk, mmbprty !=1 & wrkprty !=1 & wrkorg !=1)$trstun))
)
dimnames(trstun)[[1]]=c("Ninguna", "n1","n2","n3","n4","n5","n6","n7","n8","n9","Total")
dimnames(trstun)[[2]]=c("Afiliado a partido", "Miembro Grupo Acción Política", "Miembro Otras Organizaciones", "No Organizado")

barplot(trstun, beside=TRUE,
        main="Confianza en Naciones Unidas", 
        ylab="Porcentaje de respuestas")

# Confianza en el Parlamento Español
wrk <- ess[ess$trstprl <= 10,]
trstprl <- cbind(
  prop.table(table(subset(wrk, mmbprty ==1 & wrkprty ==1)$trstprl, subset(wrk, mmbprty ==1 & wrkprty ==1)$wrkprty), 2),
  prop.table(table(subset(wrk, mmbprty !=1 & wrkprty ==1)$trstprl, subset(wrk, mmbprty !=1 & wrkprty ==1)$wrkprty), 2),
  prop.table(table(subset(wrk, wrkorg ==1)$trstprl, subset(wrk, wrkorg ==1)$wrkorg), 2),
  prop.table(table(subset(wrk, mmbprty !=1 & wrkprty !=1 & wrkorg !=1)$trstprl))
)
dimnames(trstprl)[[1]]=c("Ninguna", "n1","n2","n3","n4","n5","n6","n7","n8","n9","Total")
dimnames(trstprl)[[2]]=c("Afiliado a partido", "Miembro Grupo Acción Política", "Miembro Otras Organizaciones", "No Organizado")

barplot(trstprl, beside=TRUE,
        main="Confianza en el Parlamento Español", 
        ylab="Porcentaje de respuestas")

# Es importante probar cosas nuevas en la vida
wrk <- ess[ess$impdiff <= 7,]
prop.table(table(subset(wrk, mmbprty ==1 & wrkprty ==1)$impdiff, subset(wrk, mmbprty ==1 & wrkprty ==1)$wrkprty), 2)
prop.table(table(subset(wrk, mmbprty !=1 & wrkprty ==1)$impdiff, subset(wrk, mmbprty !=1 & wrkprty ==1)$wrkprty), 2)
prop.table(table(subset(wrk, wrkorg ==1)$impdiff, subset(wrk, wrkorg ==1)$wrkorg), 2)
prop.table(table(subset(wrk, mmbprty !=1 & wrkprty !=1 & wrkorg !=1)$impdiff))

# Es importante mostrar las habilidades y ser admirado
wrk <- ess[ess$ipshabt <= 7,]
prop.table(table(subset(wrk, mmbprty ==1 & wrkprty ==1)$ipshabt, subset(wrk, mmbprty ==1 & wrkprty ==1)$wrkprty), 2)
prop.table(table(subset(wrk, mmbprty !=1 & wrkprty ==1)$ipshabt, subset(wrk, mmbprty !=1 & wrkprty ==1)$wrkprty), 2)
prop.table(table(subset(wrk, wrkorg ==1)$ipshabt, subset(wrk, wrkorg ==1)$wrkorg), 2)
prop.table(table(subset(wrk, mmbprty !=1 & wrkprty !=1 & wrkorg !=1)$ipshabt))

# Es importante tomar tus propias decisiones y ser libre
wrk <- ess[ess$impfree <= 7,]
impfree <- cbind(
  prop.table(table(subset(wrk, mmbprty ==1 & wrkprty ==1)$impfree, subset(wrk, mmbprty ==1 & wrkprty ==1)$wrkprty), 2),
  prop.table(table(subset(wrk, mmbprty !=1 & wrkprty ==1)$impfree, subset(wrk, mmbprty !=1 & wrkprty ==1)$wrkprty), 2),
  prop.table(table(subset(wrk, wrkorg ==1)$impfree, subset(wrk, wrkorg ==1)$wrkorg), 2),
  prop.table(table(subset(wrk, mmbprty !=1 & wrkprty !=1 & wrkorg !=1)$impfree))
)
dimnames(impfree)[[1]]=c("Mucho","n1","n2","n4","n4","n5","Para nada")
dimnames(impfree)[[2]]=c("Afiliado a partido", "Miembro Grupo Acción Política", "Miembro Otras Organizaciones", "No Organizado")

barplot(impfree, beside=TRUE,
        main="Es importante tomar tus propias decisiones y ser libre", 
        ylab="Porcentaje de respuestas")

# Es importante hacer lo que se le dice a uno y seguir las reglas
wrk <- ess[ess$ipfrule <= 7,]
ipfrule <- cbind(
  prop.table(table(subset(wrk, mmbprty ==1 & wrkprty ==1)$ipfrule, subset(wrk, mmbprty ==1 & wrkprty ==1)$wrkprty), 2),
  prop.table(table(subset(wrk, mmbprty !=1 & wrkprty ==1)$ipfrule, subset(wrk, mmbprty !=1 & wrkprty ==1)$wrkprty), 2),
  prop.table(table(subset(wrk, wrkorg ==1)$ipfrule, subset(wrk, wrkorg ==1)$wrkorg), 2),
  prop.table(table(subset(wrk, mmbprty !=1 & wrkprty !=1 & wrkorg !=1)$ipfrule))
)
dimnames(ipfrule)[[1]]=c("Mucho","n1","n2","n4","n4","n5","Para nada")
dimnames(ipfrule)[[2]]=c("Afiliado a partido", "Miembro Grupo Acción Política", "Miembro Otras Organizaciones", "No Organizado")

barplot(ipfrule, beside=TRUE,
        main="Es importante hacer lo que se le dice a uno y seguir las reglas", 
        ylab="Porcentaje de respuestas")

# Es importante tener el respeto de otros
wrk <- ess[ess$iprspot <= 7,]
iprspot <- cbind(
  prop.table(table(subset(wrk, mmbprty ==1 & wrkprty ==1)$iprspot, subset(wrk, mmbprty ==1 & wrkprty ==1)$wrkprty), 2),
  prop.table(table(subset(wrk, mmbprty !=1 & wrkprty ==1)$iprspot, subset(wrk, mmbprty !=1 & wrkprty ==1)$wrkprty), 2),
  prop.table(table(subset(wrk, wrkorg ==1)$iprspot, subset(wrk, wrkorg ==1)$wrkorg), 2),
  prop.table(table(subset(wrk, mmbprty !=1 & wrkprty !=1 & wrkorg !=1)$iprspot))
)
dimnames(iprspot)[[1]]=c("Mucho","n1","n2","n4","n4","n5","Para nada")
dimnames(iprspot)[[2]]=c("Afiliado a partido", "Miembro Grupo Acción Política", "Miembro Otras Organizaciones", "No Organizado")

barplot(iprspot, beside=TRUE,
        main="Es importante tener el respeto de otros", 
        ylab="Porcentaje de respuestas")

# Es importante tener éxito y que la gente reconozca méritos
wrk <- ess[ess$ipsuces <= 7,]
ipsuces <- cbind(
  prop.table(table(subset(wrk, mmbprty ==1 & wrkprty ==1)$ipsuces, subset(wrk, mmbprty ==1 & wrkprty ==1)$wrkprty), 2),
  prop.table(table(subset(wrk, mmbprty !=1 & wrkprty ==1)$ipsuces, subset(wrk, mmbprty !=1 & wrkprty ==1)$wrkprty), 2),
  prop.table(table(subset(wrk, wrkorg ==1)$ipsuces, subset(wrk, wrkorg ==1)$wrkorg), 2),
  prop.table(table(subset(wrk, mmbprty !=1 & wrkprty !=1 & wrkorg !=1)$ipsuces))
)
dimnames(ipsuces)[[1]]=c("Mucho","n1","n2","n4","n4","n5","Para nada")
dimnames(ipsuces)[[2]]=c("Afiliado a partido", "Miembro Grupo Acción Política", "Miembro Otras Organizaciones", "No Organizado")

barplot(ipsuces, beside=TRUE,
        main="Es importante tener éxito y que la gente reconozca méritos", 
        ylab="Porcentaje de respuestas")

# Es importante buscar la diversión y cosas que dan placer
wrk <- ess[ess$impfun <= 7,]
impfun <- cbind(
  prop.table(table(subset(wrk, mmbprty ==1 & wrkprty ==1)$impfun, subset(wrk, mmbprty ==1 & wrkprty ==1)$wrkprty), 2),
  prop.table(table(subset(wrk, mmbprty !=1 & wrkprty ==1)$impfun, subset(wrk, mmbprty !=1 & wrkprty ==1)$wrkprty), 2),
  prop.table(table(subset(wrk, wrkorg ==1)$impfun, subset(wrk, wrkorg ==1)$wrkorg), 2),
  prop.table(table(subset(wrk, mmbprty !=1 & wrkprty !=1 & wrkorg !=1)$impfun))
)
dimnames(impfun)[[1]]=c("Mucho","n1","n2","n4","n4","n5","Para nada")
dimnames(impfun)[[2]]=c("Afiliado a partido", "Miembro Grupo Acción Política", "Miembro Otras Organizaciones", "No Organizado")

barplot(impfun, beside=TRUE,
        main="Es importante buscar la diversión y cosas que dan placer", 
        ylab="Porcentaje de respuestas")

# Es importante ser rico, tener dinero y cosas caras
wrk <- ess[ess$imprich <= 7,]
prop.table(table(subset(wrk, mmbprty ==1 & wrkprty ==1)$imprich, subset(wrk, mmbprty ==1 & wrkprty ==1)$wrkprty), 2)
prop.table(table(subset(wrk, mmbprty !=1 & wrkprty ==1)$imprich, subset(wrk, mmbprty !=1 & wrkprty ==1)$wrkprty), 2)
prop.table(table(subset(wrk, wrkorg ==1)$imprich, subset(wrk, wrkorg ==1)$wrkorg), 2)
prop.table(table(subset(wrk, mmbprty !=1 & wrkprty !=1 & wrkorg !=1)$imprich))

# Es importante vivir en un entorno seguro
wrk <- ess[ess$impsafe <= 7,]
prop.table(table(subset(wrk, mmbprty ==1 & wrkprty ==1)$impsafe, subset(wrk, mmbprty ==1 & wrkprty ==1)$wrkprty), 2)
prop.table(table(subset(wrk, mmbprty !=1 & wrkprty ==1)$impsafe, subset(wrk, mmbprty !=1 & wrkprty ==1)$wrkprty), 2)
prop.table(table(subset(wrk, wrkorg ==1)$impsafe, subset(wrk, wrkorg ==1)$wrkorg), 2)
prop.table(table(subset(wrk, mmbprty !=1 & wrkprty !=1 & wrkorg !=1)$impsafe))
