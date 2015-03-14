# Análisis sobre valores y política de la Encuesta Social Europea para el artículo
# "Ciudadanía activista por el Gobierno Abierto. Hipótesis sobre su 
# caracterización en España" (Ontañón, J. Félix, 2015).
# 
# Datos de la Encuesta Social Europea en: 
# Véase: http://ess.nsd.uib.no/downloadwizard/
# ============================================================================

# Path al directorio del data-analisis
setwd("/home/fontanon/Dropbox/Devel/dataanalysis-ess/essanalysis")
ess <- read.csv("data/ess.csv")

# Filtro para evitamos contabilizar las respuestas tipo "no lo se", "no contesta"
wk <- ess[ess$wrkorg <=2,]
wk <- wk[wk$wrkprty <= 2,]

# Utilizar el siguiente para filtrar por año de encuesta (ESS-ROUNDs)
# wrk <- wrk[wrk$essround >= 5,]

# Gente que en los últimos 12 meses colaboró con partidos políticos o plataformas de acción ciudadana
wkprty <- subset(wk, wrkprty ==1)

# Gente que en los últimos 12 meses colaboró con asociacones de otro tipo
wkorg <- subset(wk, wrkorg ==1)

# Gente que en los últimos 12 meses no colaboró con ninguno de los anteriores
others <- subset(wk, wrkprty !=1 & wrkorg !=1)

# Exploración de variables de interés para el estudio
# -----------------------------------------------------------------

# Satisfacción con la democracia en España
wkprtystfdem <- prop.table(table(subset(wkprty, wkprty$stfdem <= 10)$stfdem, subset(wkprty, wkprty$stfdem <= 10)$wrkprty), 2)
wkorgstfdem <- prop.table(table(subset(wkorg, wkorg$stfdem <= 10)$stfdem, subset(wkorg, wkorg$stfdem <= 10)$wrkorg), 2)
othersstfdem <- prop.table(table(subset(others, others$stfdem <= 10)$stfdem))

lengths <- max(c(length(wkprtystfdem), length(wkorgstfdem), length(othersstfdem)))
length(wkprtystfdem) <- lengths; length(wkorgstfdem) <- lengths; length(othersstfdem) <- lengths;
stfdem <- cbind(wkprtystfdem, wkorgstfdem, othersstfdem)

dimnames(stfdem)[[1]]=c("Ninguna", "n1","n2","n3","n4","n5","n6","n7","n8","n9","Total")
dimnames(stfdem)[[2]]=c("Colabora partido o plataforma ciudadana", "Colabora otro tipo de Organizaciones", "No colabora con ninguno de los anteriores")

barplot(stfdem, beside=TRUE,
        main="Satisfacción con la forma en la que la democracia funciona en España", 
        ylab="Porcentaje de respuestas")

# Posición en el eje izquierda/derecha
wkprtylrscale <- prop.table(table(subset(wkprty, wkprty$lrscale <= 10)$lrscale, subset(wkprty, wkprty$lrscale <= 10)$wrkprty), 2)
wkorglrscale <- prop.table(table(subset(wkorg, wkorg$lrscale <= 10)$lrscale, subset(wkorg, wkorg$lrscale <= 10)$wrkorg), 2)
otherslrscale <- prop.table(table(subset(others, others$lrscale <= 10)$lrscale))

lengths <- max(c(length(wkprtylrscale), length(wkorglrscale), length(otherslrscale)))
length(wkprtylrscale) <- lengths; length(wkorglrscale) <- lengths; length(otherslrscale) <- lengths;
lrscale <- cbind(wkprtylrscale, wkorglrscale, otherslrscale)

dimnames(lrscale)[[1]]=c("Izquierda", "n1","n2","n3","n4","n5","n6","n7","n8","n9","Derecha")
dimnames(lrscale)[[2]]=c("Colabora partido o plataforma ciudadana", "Colabora otro tipo de Organizaciones", "No colabora con ninguno de los anteriores")

barplot(lrscale, beside=TRUE,
        main="Posicionamiento ideológico en base a la afiliación", 
        ylab="Porcentaje de respuestas")