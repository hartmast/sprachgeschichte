
# Zusatzpakete installieren und laden
sapply(c("dplyr", "devtools"), 
       function(x) if(!is.element(x, installed.packages())) 
         install.packages(x, dependencies = T))



if(!is.element("concordances", installed.packages())) {
  devtools::install_github("hartmast/concordances")
}

library(dplyr)
library(concordances)

# Optionen
Sys.setlocale("LC_ALL", "de_DE") # funktioniert nur bei Mac/Linux und eruebrigt sich meist
options(stringsAsFactors = F)

# Daten aufbereiten
hat_Preis <- getCOSMAS("hat_NP_Preis.TXT", years=TRUE)

# letztes Wort des linken Kontexts extrahieren
hat_Preis$last_left <- sapply(1:nrow(hat_Preis), function(i)
       unlist(strsplit(hat_Preis$Left[i], " "))[length(unlist(strsplit(hat_Preis$Left[i], " ")))])


# Interpunktionszeichen entfernen
hat_Preis$Key1 <- gsub("[[:punct:]]", "", hat_Preis$Key1)
hat_Preis$last_left <- gsub("[[:punct:]]", "", hat_Preis$last_left)

# exportieren
write.table(hat_Preis, "hat_NP_Preis.csv", sep="\t", row.names=F,
            quote=F, fileEncoding = "UTF-8")



# Stichprobe nehmen
write.table(hat_Preis[sample(1:1000),], "hat_NP_Preis_sample.csv", sep="\t", row.names=F,
            quote=F, fileEncoding = "UTF-8")


# annotierte Stichprobe einlesen
hat_Preis <- read.table("hat_NP_Preis_sample_annotated.csv",
                        sep="\t", head=T, quote="",
                        encoding="UTF-8")


# unklare Faelle und Fehltreffer ausschliessen
hat_Preis <- hat_Preis[-which(hat_Preis$Genus=="unklar" | hat_Preis$Lemma_last_left %in% c("unklar", "DISCARD")),]

# Auswertung
filter(hat_Preis, Numerus!="pl")$Lemma_last_left %>% table %>% sort(decreasing=TRUE) %>% head

# Frequenz m/n vs w
filter(hat_Preis, Numerus!="pl")$Genus %>% table

#REG( (^habe?(n|st)$ | ^hatt(e(st|n)?))  )

# haeufigste Varianten in "seinen"
c1 <- hat_Preis[grep("seinen", hat_Preis$Key1),] %>% select(Lemma_last_left) %>% 
  table %>% sort(decreasing=T) %>%  as.data.frame %>% head(10)
c2 <- filter(hat_Preis, Numerus!="pl")[grep("ihren", filter(hat_Preis, Numerus!="pl")$Key1),] %>% 
  select(Lemma_last_left) %>%  table %>% sort(decreasing=T) %>%  
  as.data.frame %>% head(10)

write.table(cbind(c1,c2), "Preis_top10.csv", sep="\t", row.names=F, quote=F,
            fileEncoding = "UTF-8")


# Mismatches finden
nrow(filter(hat_Preis[grep("seinen", hat_Preis$Key1),], Genus=="w"))

nrow(hat_Preis[grep("seinen", hat_Preis$Key1),])


# Gesamtzahl von seinen und ihren
length(grep("seinen", filter(hat_Preis, Numerus!="pl")$Key1))
length(grep("ihren", filter(hat_Preis, Numerus!="pl")$Key1))
