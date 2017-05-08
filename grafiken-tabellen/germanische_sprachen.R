# Optionen
Sys.setlocale("LC_ALL", "de_DE")

# Zusatzpakete installieren und laden
sapply(c("dplyr", "ggplot2", "ggmap", "jsonlite", "ggrepel", "devtools"), function(x) if(!is.element(x, installed.packages())) install.packages(x, dependencies = T))
lapply(list("dplyr", "ggplot2", "ggmap", "jsonlite", "ggrepel"), require, character.only=T)

# sicherstellen, dass ggplot2 2.2.0 installiert ist, da
# ggmap sonst nicht funktioniert
if(length(grep("2\\.2\\..", packageVersion("ggplot2")))==0) {
  detach("package:ggplot2", unload=TRUE)
  devtools::install_version("ggplot2", "2.2.0")
  library(ggplot2)
}


# Daten einlesen
lang <- jsonlite::fromJSON("germaniclanguages_wals.json")

# coordinates
coordinX <- data.frame(LONG=lang$features$properties$language$longitude,
                    LAT=lang$features$properties$language$lat,
                    NAME=lang$features$properties$language$name)


# map of Germany / Europe
germ <- get_map("germany", zoom=2, language = "DE", maptype = "terrain", color = "bw")
germ_map <- ggmap(germ) + theme_bw() + theme(panel.grid.major=element_blank())
p <- germ_map + geom_label_repel(aes(x=LONG, y=LAT, label=NAME), 
                                 data=filter(filter(filter(coordinX, NAME!="Deutsch"), 
                                             NAME!="Elsässisch"), NAME!="Niederdeutsch"), size=2)
p + geom_label(aes(x=LONG-2.3, y=LAT+0.65, label=NAME), 
               data=filter(coordinX, NAME=="Deutsch"), size=2) + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) + ylab("") + xlab("")
ggsave("germanische_sprachen3.png")
