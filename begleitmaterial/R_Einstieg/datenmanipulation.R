# Paket dplyr installieren und/oder laden
if(!is.element("dplyr", installed.packages())) { install.packages("dplyr") }
library(dplyr)

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
