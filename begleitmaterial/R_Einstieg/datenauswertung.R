# Pakete installieren bzw. laden
sapply(c("dplyr", "ggplot2", "scales", "koRpus"), function(x) 
  if(!is.element(x, installed.packages())) install.packages(x, dependencies = T))

lapply(list("dplyr", "ggplot2", "scales", "koRpus"), require, character.only=T)


# Optionen
options(stringsAsFactors = F)
options(encoding = "UTF-8")

# Daten einlesen
frau <- read.csv("adja_frau.csv")
mann <- read.csv("adja_mann.csv")

# Datei mit Lemmas einlesen
lemmas <- read.delim("mann_frau_lemma.csv")

# Einige Lemmas sind doppelt, daher muessen wir zunächst Duplikate ausschliessen:
lemmas <- lemmas[!duplicated(lemmas$Hit),]

# Die beiden Konkordanzen zusammenfuehren und mit den Lemmainformationen mergen
mannfrau <- rbind(mutate(frau, df = "Frau"), 
                  mutate(mann, df = "Mann")) %>% 
  merge(lemmas, 
        by = "Hit", 
        all.x = T) # all.x = T gibt an, dass keine Daten aus dem ersten Dataframe (mannfrau)
                   # fallengelassen werden sollen, wenn sie im zweiten Dataframe
                   # nicht vorkommen sollten.



# als DISCARD getaggte Lemmata ausschliessen:
mannfrau <- filter(mannfrau, Lemma!="DISCARD")

# einfache Tabelle: Mann vs. Frau
table(mannfrau$Genre)

# Textfrequenzen des DWDS-KernKorpus
# (Hinweis: falls sich die DWDS-Schnittstelle aendert, wird dieser Code evtl. unbrauchbar)
dwds_freq <- read.table("https://www.dwds.de/r/stat?corpus=kern&cnt=tokens&date=all&textclass=1&format=text", 
           skip = 3, sep="\t", head=T)

# falls es nicht mehr funktioniert, hier das Ganze auf dem Stand von Mitte Okt. 2017 zum
# Selberbasteln - einfach die drei folgenden Zeilen komplett markieren und mit Strg+C
# bzw. Cmd + C "ent-kommentieren":

# dwds_freq <- data.frame(Textklasse = c("Belletristik", "Gebrauchsliteratur",
#                                        "Wissenschaft", "Zeitung", "gesprochen"),
#                         Anzahl = c(31982982, 26433500, 29852365, 33126807, 1950))



# dwds_freq alphabetisch nach Textsorten sortieren
dwds_freq <- dwds_freq[order(dwds_freq$Textklasse),]


# Frequenz pro 10000 Wörter:
table(mannfrau$Genre) / dwds_freq$Anzahl
barplot((table(mannfrau$Genre) / dwds_freq$Anzahl)*10000, cex.names = 0.6)

# komplexere Tabelle
table(mannfrau$df, mannfrau$Genre)

# einfache Tabelle: Adjektive mit "Mann" vs. Adjektive mit "Frau"
mannfrau %>% 
  select(Lemma, df) %>% # waehlt zwei Spalten aus: Lemma und df, alle anderen werden jetzt ignoriert
  table                 # erstellt Tabelle aus diesen beiden Spalten

# wenn wir nur die ersten zehn Zeilen sehen wollen:
mannfrau %>% select(Lemma, df) %>% table %>% head(10)

# nach der Frequenz der Adjektivkollokate zu "Frau" sortieren
mannfrau %>% select(Lemma, df) %>% table %>% 
  as.data.frame.matrix %>% # ueberfuehrt alles ins data.frame-Format, 
                           # behaelt aber das "wide format" bei, statt die Daten
                           # ins "long format" zu ueberfuehren, wie das
                           # as.data.frame() tun wuerde (um den Unterschied zwischen long
                           # und wide format herauszufinden, probieren Sie doch einfach
                           # as.data.frame() hier aus oder googeln Sie...)
  mutate(Lemma = row.names(.)) %>%  # Da die Lemmas bei der Tabelle nicht als eigene Spalte,
                                    # sondern nur als Spalten*namen* verfuegbar sind,
                                    # fuegen wir eine Spalte hinzu, die die Spaltennamen
                                    # noch einmal enthaelt. Der Punkt bezieht sich auf das
                                    # Objekt, das ueber die Pipe als Input gegeben wird.
  arrange(desc(Frau)) %>% # mit "arrange" sortieren wir die Daten nach der als Argument
                          # angegebenen Spalte; da defaultmaessig aufsteigend sortiert wird,
                          # muessen wir mit der desc()-Funktion noch sicherstellen, dass
                          # absteigend sortiert wird, sodass wir die frequentesten Treffer
                          # zuerst bekommen.
  head(20)

# das gleiche fuer "Mann":
mannfrau %>% select(Lemma, df) %>% table %>% as.data.frame.matrix %>% 
  mutate(Lemma = row.names(.)) %>% arrange(desc(Mann)) %>% head(20)


# als Dataframe speichern:
mf_table <- mannfrau %>% select(Lemma, df) %>% table %>% as.data.frame()




#####################################################
# OPTIONALE ERGAENZUNG: distinktive Kollexemanalyse #
#####################################################

# Paket "collostructions" (Flach 2017) installieren, falls noch nicht installiert:
if(!is.element("collostructions", installed.packages())) {
  if(.Platform$OS.type!="Windows") {
    install.packages("http://userpage.fu-berlin.de/~flach/wp-content/uploads/collostructions_0.0.10.tar.gz", repos = NULL)
  } else {
    install.packages("http://userpage.fu-berlin.de/~flach/wp-content/uploads/collostructions_0.0.10.zip", repos = NULL)
  }
}

library(collostructions)

# Distinktive Kollexemanalyse:
mannfrau %>% select(Lemma, df) %>% table %>% as.data.frame.matrix %>%
  tibble::rownames_to_column() %>% collex.dist()

# mit reverse = T sehen wir die distinktiven Kollexeme fuer "Mann" zuerst:
mannfrau %>% select(Lemma, df) %>% table %>% as.data.frame.matrix %>% 
  tibble::rownames_to_column() %>% collex.dist(reverse = T)








