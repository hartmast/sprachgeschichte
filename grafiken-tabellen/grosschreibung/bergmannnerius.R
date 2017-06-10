# Optionen
options(stringsAsFactors = F)
Sys.setlocale("LC_ALL", "de_DE") # nur Mac/Linux, wenn Systemsprache nicht Deutsch

# Zusatzpakete installieren und laden
sapply(c("dplyr", "ggplot2", "reshape2", "scales"), function(x) if(!is.element(x, installed.packages())) install.packages(x, dependencies = T))
lapply(list("dplyr", "ggplot2", "reshape2", "scales"), require, character.only=T)

# Daten bearbeiten
bn <- read.csv("bergmann_nerius.csv", sep=";", head=T)
bn2 <- melt(bn, id.vars = "Jahr")
colnames(bn2)[2] <- "Nomentyp"
bn2$Nomentyp <- gsub("Nomina_sacra", "Nomina sacra", bn2$Nomentyp)
bn2$Nomentyp <- factor(bn2$Nomentyp, levels = c("Eigennamen", "Nomina sacra", "Personenbezeichnungen", "Konkreta", "Abstrakta"))

# Lineplot
ggplot(bn2) + geom_line(aes(x=factor(Jahr), y=value, group=Nomentyp, linetype=Nomentyp, color=Nomentyp), 
                       stat="identity", size=1.3) + theme_bw() + scale_color_grey(start = 0.1, end=0.7) + 
  scale_y_continuous(labels = percent) +
  ylab("Anteil Großschreibung") + xlab("Jahr") + ggtitle("Entwicklung der Großschreibung nach Nomentyp\n(nach Bergmann & Nerius 1998)") +
  theme(plot.title=element_text(lineheight = 1, face="bold", hjust=0.5, vjust = 0.5, size = 14))
ggsave("bergmann_nerius.png", width=10, height=6.5, un="in")

# alternativ: Barplot
ggplot(bn2) + geom_bar(aes(x=factor(Jahr), y=value, fill=Nomentyp), 
                       stat="identity", position = "dodge") + theme_bw() +
  scale_fill_grey() + ylab("Anteil Großschreibung") + xlab("Jahr")
