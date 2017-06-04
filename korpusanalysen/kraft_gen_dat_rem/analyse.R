
# Optionen
Sys.setlocale("LC_ALL", "de_DE") # nur fuer Linux/Unix, wenn nicht ohnehin auf Deutsch eingestellt
options(stringsAsFactors = F)

# Zusatzpakete installieren und laden
sapply(c("dplyr", "ggplot2", "reshape2", "scales"), 
       function(x) if(!is.element(x, installed.packages())) install.packages(x, dependencies = T))
lapply(list("dplyr", "ggplot2", "reshape2", "scales"), require, character.only=T)


# concordances
if(!is.element("concordances", installed.packages())) {
  devtools::install_github("hartmast/concordances")
}


library(concordances)


# Daten einlesen
kraft <- getCWB("kraft_gen_dat_sg_rem.txt")

# welche Keywords gibt es?
table(kraft$Keyword)

# Spalte hinzufuegen:
# kommt a in der Wortform vor?
kraft$a <- NA
kraft[grep(".*a.*", kraft$Keyword),]$a <- "Ja"
kraft[grep(".*a.*", kraft$Keyword, invert = T),]$a <- "Nein"

# nach Zeit ordnen; Tabelle mit absoluten und relativen Werten
kraft_t_abs <- kraft %>% select(text_time, a) %>% table %>% as.data.frame.matrix()
kraft_t_abs$Zeit <- row.names(kraft_t_abs)
kraft_t_abs <- filter(kraft_t_abs, Zeit!="-")

kraft_t_rel <- kraft %>% select(text_time, a) %>% table %>% prop.table(mar=1) %>% as.data.frame.matrix()
kraft_t_rel$Zeit <- row.names(kraft_t_rel)
kraft_t_rel <- filter(kraft_t_rel, Zeit!="-")

# in langes Datenformat ueberfuehren & Spalte mit abs. Werten hinzufuegen
kraft_t_rel2 <- melt(kraft_t_rel)
kraft_t_rel2$abs <- melt(kraft_t_abs)$value

ggplot(kraft_t_rel2, aes(x=Zeit, y=value, fill=variable, label=abs)) + 
  geom_bar(stat="identity") + 
  geom_text(data=subset(kraft_t_rel2, abs!=0), size = 3, 
            position = position_stack(vjust = 0.5)) +
  theme_bw() + scale_fill_grey(start=0.8, end=0.4) +
  scale_y_continuous(labels=percent) + ylab("Anteil") +
  guides(fill=guide_legend(title="mit <a>")) +
  theme(axis.text.x = element_text(angle=45, hjust=.9, size=6)) +
  theme(panel.grid.major = element_blank()) +
  ggtitle(expression(paste(italic("Kraft, "), "REM"), sep="", collapse="")) +
  theme(plot.title = element_text(hjust=0.5))
ggsave("kraft_gen_dat_rem.png")
