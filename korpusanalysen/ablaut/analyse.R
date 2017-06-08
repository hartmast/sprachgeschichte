# Optionen
options(stringsAsFactors = F)
Sys.setlocale("LC_ALL", "de_DE")

# Pakete
library(dplyr)
library(ggplot2)
library(scales)

# Daten einlesen
fnhd <- read.csv("starkeverben_fnhd.csv", sep="\t", head=T, encoding = "UTF-8", quote="")

# auswählen: Ablautreihe 3a
fnhd3a <- filter(fnhd, Vokal!="")

# Jahrhundert hinzufügen

# nicht-numerische Jahreseintragungen finden
yx <- grep("^[[:digit:]][[:digit:]][[:digit:]][[:digit:]]", fnhd3a$text_Jahr, value=T, invert = T)
yx <- unique(yx)
yz <- gsub("^ ", "", gsub("^um|^Ende|^wohl zwischen|A\\.\\-M\\. |2\\. H.", "", yx))


for (i in 1:nrow(fnhd3a)) {
  if(fnhd3a$text_Jahr[i] %in% yx) {
    fnhd3a$text_Jahr[i] <-  yz[which(yx==fnhd3a$text_Jahr[i])]
  }
}


# Jahrhundert aus den ersten beiden Ziffern hinzufügen

fnhd3a$Jh <- sapply(1:nrow(fnhd3a), function(i) as.numeric(paste(unlist(strsplit(fnhd3a$text_Jahr[i], ""))[1:2], 
                                                                 collapse=""))+1)

# Tabelle mit relativen Werten
fnhd3a_table <- select(filter(fnhd3a, Tempus=="praeteritum"), Jh, Vokal) %>% table %>% prop.table(mar=1) %>% as.data.frame

# Tabelle mit absoluten Werten
fnhd3a_table_abs <- select(filter(fnhd3a, Tempus=="praeteritum"), Jh, Vokal) %>% table %>% as.data.frame

# absolute Werte zu relativen hinzufügen
fnhd3a_table$abs <- fnhd3a_table_abs$Freq

# Plot
ggplot(fnhd3a_table, aes(x=Jh, y=Freq, fill=Vokal, label=abs)) + 
  geom_bar(stat="identity") + 
  scale_fill_grey(start = 0.4, end=0.9) +
  geom_text(data=subset(fnhd3a_table, abs>0), size=4, vjust=1, position=position_stack()) +
  theme_classic() +
  guides(fill=guide_legend(rev=T)) + 
  theme(axis.line.x=element_line(size=1),
        axis.line.y=element_line(size=1)) +
  scale_y_continuous(labels=percent) + ylab("Relative Frequenz") + xlab("Jahrhundert") +
  ggtitle("Ablautreihe 3a im Bonner Fr\u00fchneuhochdeutschkorpus, \n Plural Präteritum") +
  theme(plot.title = element_text(face="bold"))

ggsave("ALR3a.png")
