# Paket dplyr installieren und/oder laden
if(!is.element("dplyr", installed.packages())) { install.packages("dplyr") }
library(dplyr)

# Optionen
options(stringsAsFactors = F)


#############################
## zu 2.2.1: Daten filtern ##
#############################

# Daten einlesen
ung <- read.delim("ung_bsp.csv", fileEncoding = "UTF-8", quote="")

# in welchen Zeilen hat die Spalte "Anmerkungen" den Wert "Fehltreffer"?
which(ung$Anmerkungen=="Fehltreffer")
which(ung$Anmerkungen!="Fehltreffer")

# Fehltreffer aussortieren (klassisch, ohne dplyr)
ung2 <- ung[which(ung$Anmerkungen!="Fehltreffer"),]

# Fehltreffer aussortieren (klassisch: alternative, aber u.U. problematische Variante)
ung2a <- ung[-which(ung$Anmerkungen=="Fehltreffer"),]

# Diese Variante ist dann problematisch und fehlertraechtig, wenn es die 
# Variablenauspraegung, die als Ausschlusskriterium dient, 
# in den Daten gar nicht (mehr) gibt:
ung2b <- ung2a[-which(ung2a$Anmerkungen=="Fehltreffer"),]
str(ung2b) # Was ist hier passiert? - Ganz einfach:
which(ung2a$Anmerkungen=="Fehltreffer") # ist 0, deshalb ist
ung2a[-which(ung2a$Anmerkungen=="Fehltreffer"),] # das gleiche wie
ung2a[-0,] # Dieser Code sagt R, dass es die nullte Zeile ausgeben soll, denn
-0         # ist null. R faengt aber bei 1 an zu zaehlen, deshalb wird ein
           # leerer Dataframe ausgegeben.


# Deshalb rate ich davon ab, das Minuszeichen zum Subsetten zu benutzen.
# Alternativ kann man das negierende ! verwenden (dann aber ohne which-Statement):
ung2a[!ung2a$Anmerkungen=="Fehltreffer",]

# Fehltreffer aussortieren (mit Paket "dplyr")
ung3 <- filter(ung, Anmerkungen!="Fehltreffer")

# ueberpruefen, dass beide das gleiche Ergebnis generieren
ung2==ung3 # zeigt Ergebnis fuer jede Zelle einzeln
all(ung2==ung3) # zeigt Ergebnis fuer alle Zellen


################################################
# zu 2.2.2: Tabellen gleichen Typs kombinieren #
################################################

# die beiden Dataframes einlesen
gv <- read.csv("gehoert_verboten.csv")
vg <- read.csv("verboten_gehoert.csv")

# beide kombinieren
beide <- rbind(gv, vg)

# beide kombinieren und dabei eine Spalte hinzufuegen, die angibt,
# aus welchem df es urspruenglich stammt
# (mit der Funktion mutate aus dem Paket dplyr)

beide <- rbind(mutate(gv, DF="gv"),
               mutate(vg, DF="vg"))
beide

#####################################################
# 2.2.3 Tabellen unterschiedlichen Typs kombinieren #
#####################################################

# Dataframes einlesen:
farben <- read.csv("farben.csv")
farben_lemmas <- read.delim("farben_lemmas.csv")

# dafuer sorgen, dass beide einen Spaltennamen teilen:
colnames(farben) # hier ist "Hit" die relevante Spalte
colnames(farben_lemmas) # hier ist "Token" die relevante Spalte
colnames(farben_lemmas)[which(colnames(farben_lemmas)=="Token")] <- "Hit"

# mergen:
merge(farben, farben_lemmas, by = "Hit")

# oder, mit den ursprünglichen Spaltennamen:
farben_lemmas <- read.delim("farben_lemmas.csv")
merge(farben, farben_lemmas, by.x = "Hit", by.y = "Token")
