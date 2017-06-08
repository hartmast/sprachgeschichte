
# Pakete installieren / laden
# install / load packages
sapply(c("dplyr", "stringdist"), function(x) 
           if(!is.element(x, installed.packages())) install.packages(x, dependencies = T))

lapply(c("dplyr", "stringdist"), require, character.only=T)

# Optionen
options(stringsAsFactors = F) 
Sys.setlocale("LC_ALL", "de_DE") # nur Mac/Linux, wenn Systemsprache nicht Deutsch


##################
# Daten einlesen #
##################

wenig <- read.table("ein_wenig_COSMAS_DTA.csv", sep="\t", head=T,
                    quote="", encoding = "UTF-8", fill=T)




####################################
# Levenshtein-Distanz ausprobieren #
####################################

# Levenshtein-Distanz ist im Paket "stringdist" implementiert,
# allerdings nicht als normalisierte Levenshtein-Distanz
stringdist("Haus", "Maus", method = "lv")

# alternativ kann man auch "adist" verwenden:
adist("Haus", "Maus")

##################################################
# Funktion für normalisierte Levenshtein-Distanz #
##################################################

# Mit nchar() erhaelt man die Länge eines Strings:
nchar("Maus")


lev.norm <- function(a, b) {
  lev <- adist(a, b) # nicht-normalisierte Levenshtein-Distanz
  nc  <- max(sapply(c(a,b), nchar)) # Laenge des laengeren Zeichens
  return(as.numeric(lev/nc))
}


# Beispiel
lev.norm("Haus", "Maus")


###########################################################
# Funktion, die die ersten bzw. letzten n Woerter ausgibt #
###########################################################


# Die folgende Funktion gibt die ersten oder letzten n Elemente
# aus einem Vektor aus (bzw. den gesamten Vektor, wenn seine
# Laenge < n ist).

dynamic_lengths <- function(vector, n, cut=c("left", "right")) {
  
  if(cut=="left") {
    if(length(vector)>=n) {
      return(vector[c((length(vector)-(n-1)):length(vector))])
    } else {
      return(vector[c(1:length(vector))])
    }
  } else if(cut=="right") {
    if(length(vector)>=n) {
      return(vector[c(1:n)])
    } else {
      return(vector[c(1:length(vector))])
    }
  }
  
  
}


###############################
# Anwendung auf "wenig"-Daten #
###############################


# Zuerst werden die Daten so "beschnitten", dass nur noch 5 Woerter links
# und rechts vorhanden sind

# neue Spalten fuer die "kurzen" Kontexte
wenig$left5 <- wenig$right5 <- NA

# Spalten fuellen
for(i in 1:nrow(wenig)) {
  wenig$left5[i] <- gsub("[^[:alnum:]]", "",  # alle nicht-alphanumer. Zeichen
                                              # werden getilgt
                         unlist(strsplit(wenig$Kontext_links[i], " "))) %>% 
    dynamic_lengths(n = 5, cut = "left") %>% paste(collapse = " ")
  
  wenig$right5[i] <- gsub("[^[:alnum:]]", "",  unlist(strsplit(wenig$Kontext_rechts[i], " "))) %>% 
    dynamic_lengths(n = 5, cut = "right") %>% paste(collapse = " ")
}


# Leerzeichen am Anfang und Ende der Strings entfernen
wenig$left5  <- gsub("^ *| *$", "", wenig$left5)
wenig$right5 <- gsub("^ *| *$", "", wenig$right5)


# ueberpruefen
head(select(wenig, Kontext_links, left5, Kontext_rechts, right5), 10)

# sortieren nach Kombination aus linkem und rechtem Kontext
wenig <- wenig[order(paste(wenig$left5, wenig$right5, sep=" ")),]

# Levenshtein-Distanz zwischen Zeile n und n+1
wenig$lev <- NA

for(i in 1:(nrow(wenig)-1)) {
  wenig$lev[i+1] <- 1-lev.norm(paste(wenig$left5[i],
                                   wenig$right5[i],
                                   collapse=" "),
                             paste(wenig$left5[i+1],
                                   wenig$right5[i+1],
                                   collapse=" "))
}



write.table(wenig, "wenig_lev.csv", sep="\t", quote=F, row.names=F,
            fileEncoding = "UTF-8")
