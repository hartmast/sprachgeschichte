# Hinweis: Dieses Skript enthaelt nur die n-Gramm-Analysen zu den
# DECOW-Daten. Die n-Gramm-Analysen mit den Daten aus dem Deutschen Textarchiv
# basieren auf dem gleichen Prinzip, aber auf den vollstaendigen Korpusdaten.
# Daher sind die verwendeten Konkordanzen extrem groß. Auf Anfrage stelle ich
# sie aber gerne zur Verfuegung.


################
# ZUSATZPAKETE #
################

# Pakete installieren, 
# falls noch nicht vorhanden

# dplyr und devtools
sapply(c("dplyr", "devtools"), function(x) 
  if(!is.element(x, installed.packages())) install.packages(x, dependencies = T))

# concordances
if(!is.element("concordances", installed.packages())) {
  devtools::install_github("hartmast/concordances")
}

# collostructions
# Diese Funktion ueberprueft, ob der Link zum Paket "collostructions" noch aktuell ist.
# Wenn nicht, gibt sie eine Fehlermeldung aus. In diesem Fall muessen
# Sie den Link durch den aktuellen Link ersetzen (zu finden unter bit.ly/sflach)

if(!is.element("collostructions", installed.packages())) {
  if(.Platform$OS.type=="Windows") {
    cxlink <- "http://userpage.fu-berlin.de/~flach/wp-content/uploads/collostructions_0.1.0.zip"
    } else {
      cxlink <- "http://userpage.fu-berlin.de/~flach/wp-content/uploads/collostructions_0.1.0.tar.gz"
    }
  
  # print error message if link is not up to date
  if(!RCurl::url.exists(cxlink)) { 
      stop("Link to package 'collostructions' is not up to date any more.\n 
       Please update the link. You should find the package on bit.ly/sflach")
  }
  
  install.packages(cxlink, repos = NULL, type="source")
}



# Zusatzpakete laden
library(dplyr)
library(collostructions)
library(concordances)

# Optionen
Sys.setlocale("LC_ALL", "de_DE") # nur fuer Mac/Linux, eruebrigt sich aber auch dort meist
options(stringsAsFactors = F)


############
# N-GRAMME #
############

# Funktion zur Generierung von n-Grammen
# (alternativ kann man auch das Paket "tidytext" verwenden,
# aber hier will ich zeigen, wie man n-Gramm-Analysen manuell durchfuehren kann)

# Wie in vielen Faellen gibt es auch hier mehrere Wege zum Ziel.
# Im Folgenden zeige ich zwei Funktionen zur Erstellung von n-Grammen.
# Diese hier ist viel schneller, aber etwas komplexer:

ngrams <- function(vec, n = 2) {
  vec <- grep("[[:punct:]]", vec, value = T, invert = T) # Interpunktion soll weg
  vec <- tolower(vec) # alles zu Kleinbuchstaben konvertieren
  
  if(length(vec)>=n) {
    sapply(1:(length(vec)-(n-1)), function(wrd) paste(vec[wrd:(wrd+(n-1))], collapse = " "))
  } else {
    return(NA) # Warnung ausgeben, wenn z.B. Trigramm von einem Vektor mit nur 2 Elementen erstellt werden soll
    warning(paste("can't make ", n, "-gram from vector of ", length(vec), sep="", collapse=""))
  }
  
}


# Die folgende hingegen ist moeglicherweise etwas intuitiver zu verstehen,
# aber seeeehr viel langsamer. Es ist die, die zur Erstellung der im Buch
# berichteten Ergebnisse verwendet wurde. Da sie anders als die obige Funktion
# einen Unterschied zwischen Gross- und Kleinschreibung macht, koennen
# die Ergebnisse variieren.

ngr <- function(x, n) {
  l <- length(x) - (n-1) # Laenge: die Liste der n-Gramme ist so lang wie die Liste der
                         # Tokens minus n
  ng <- c(l) # es wird ein Vektor der Laenge l erstellt, der mit dem folgenden Loop
             # gefuellt wird.
  for(i in 1:(length(x)-(n-1))) {
    ng[i] <- paste(x[i:(i+(n-1))], collapse=" ")
  }
  
  return(ng)
  
}


################
# DECOW corpus #
################

# Daten einlesen
decow_ohne <- getNSE("decow_saetze_ohne_ausrufezeichen.txt")
decow_mit <- getNSE("decow_mit_ausrufezeichen_neu.txt")

# Dubletten entfernen
decow_mit <- decow_mit[!duplicated(decow_mit$Key),]
decow_ohne <- decow_ohne[!duplicated(decow_ohne$Key),]

# n-Gramme erstellen
decow_mit <- unlist(strsplit(decow_mit$Tag1, " "))
decow_ohne <- unlist(strsplit(decow_ohne$Tag1, " "))

# Interpunktion entfernen
decow_mit <- decow_mit[-grep("[[:punct:]]", decow_mit)]
decow_ohne <- decow_ohne[-grep("[[:punct:]]", decow_ohne)]

# leere entfernen
decow_mit <- decow_mit[which(decow_mit!="")]
decow_ohne <- decow_ohne[which(decow_ohne!="")]

# Trigramme
# Anstelle von Trigrammen lassen sich auch andere n-Gramme erstellen,
# indem man einfach die 3 ersetzt. Probieren Sie es doch einmal aus!
dng_mit <- ngrams(decow_mit, 3)
dng_ohne <- ngrams(decow_ohne, 3)


# Dataframes mit Frequenzen
dng1 <- dng_mit %>% table %>% as.data.frame(stringsAsFactors = FALSE)
colnames(dng1) <- c("Lemma", "Freq_mit")
dng2 <- dng_ohne %>% table %>% as.data.frame(stringsAsFactors = FALSE)
colnames(dng2) <- c("Lemma", "Freq_ohne")

# zu einem einzigen Dataframe verbinden
dng_all <- bind_rows(dng1, dng2)

# NAs durch 0 ersetzen
dng_all[is.na(dng_all)] <- 0

# collostructional analysis
coll.decow <- collex.dist(dng_all, am = "chisq")
head(coll.decow, 30)
