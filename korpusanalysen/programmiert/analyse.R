rm(list=ls(all=T))

# Optionen
Sys.setlocale("LC_ALL", "de_DE") # nur Mac, eruebrigt sich auf den meisten Systemen
options(stringsAsFactors = F)

# Pakete installieren und laden
sapply(c("dplyr", "ggplot2", "reshape2", "scales", "devtools"), 
       function(x) if(!is.element(x, installed.packages())) install.packages(x, dependencies = T))

if(!is.element("concordances", installed.packages())) {
  devtools::install_github("hartmast/concordances")
}

lapply(list("dplyr", "ggplot2", "reshape2", "scales", "concordances"), 
       require, character.only=T)


# Daten einlesen
data <- getCOSMAS("programmiert.txt", years = TRUE)

# Daten mit Komma oder anderem Satzzeichen zwischen ist/sind 
# und programmiert entfernen
data <- data[-grep("ist[[:punct:]]|sind[[:punct:]]", data$Key1),]

# Daten zur manuellen Ueberpruefung exportieren
write.table(data, "programmiert_check.csv", sep="\t", row.names=F,
            quote=F, fileEncoding = "UTF-8")

# Spalte ohne ist/sind
data$Lemma <- sapply(1:nrow(data), 
       function(i) ifelse(length(grep("vorprogrammiert", data$Key1[i])==1), 
                          "vorprogrammiert", "programmiert"))


# auf 1990ff. beschraenken
data <- filter(data, Year>=1990)


# Tabelle
tbl <- select(data, Year, Lemma) %>% table %>% prop.table(mar=1) 

# Plot
png("vorprogrammiert.png", width=6.5, height=5, un="in", res=300)
tbl[,1] %>% plot(as.numeric(levels(factor(data$Year))), ., type="l", yaxt="n",
                 main=expression(paste(italic("vorprogrammiert "), "vs.", 
                                       italic(" programmiert"))),
                 ylab="Anteil", xlab="Jahr", lwd=2, ylim=c(0,1), xlim=c(1991,2013))
axis(2, at=seq(0,1,0.1), labels=paste(seq(0,100,10), " %", sep=""), 
     cex.axis=0.7, las=2) # Achse mit Prozent-Labels
# Das Polygon, das wir nun zeichnen, beginnt quasi links bei 100%, 
# geht dann auf der y-Achse runter zum ersten Wert (deshalb muss auf der x-Achse
# der erste Wert zweimal stehen), geht dann entlang der Linie und dann wieder
# zu 100% (deshalb muss auf der x-Achse auch der letzte Wert zweimal stehen).
polygon(x = c(as.numeric(levels(factor(data$Year))[1])-1000, 
              levels(factor(data$Year)),
              as.numeric(levels(factor(data$Year))[length(levels(factor(data$Year)))])+1000),
        y = c(2, tbl[,1], 2), col="grey60", border = NA)
polygon(x = c(as.numeric(levels(factor(data$Year))[1])-1000,
              levels(factor(data$Year)),
              as.numeric(levels(factor(data$Year))[length(levels(factor(data$Year)))])+1000),
        y = c(-10, tbl[,1], -10), col="grey80", border = NA)
tbl[,1] %>% lines(levels(factor(data$Year)), ., type="l", lwd=2)
text(2010, 0.85, expression(paste(italic("vorprogrammiert"))), cex=0.9, adj=1)
text(2010, 0.3, expression(paste(italic("programmiert"))), cex=0.9, adj=1)
box()
dev.off()
