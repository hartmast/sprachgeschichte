# Pakete installieren bzw. laden
sapply(c("dplyr", "ggplot2", "scales", "reshape2", "gridExtra"), function(x) 
  if(!is.element(x, installed.packages())) install.packages(x, dependencies = T))

lapply(list("dplyr", "ggplot2", "scales", "reshape2", "gridExtra"), require, character.only=T)

# Optionen
options(stringsAsFactors = F)


#############################################
### TEIL 1: PRINZIPIEN DER VISUALISIERUNG ###
#############################################

# Hinweis: Die folgenden Plots dienen nur der Veranschaulichung dessen, was im
# Tutorial erklaert wird. Sie sind z.T. relativ kompliziert und ich gebe nur wenige
# Erlaeuterungen dazu; im naechsten Abschnitt (Teil 2: Einfache Plots erstellen)
# stelle ich systematisch grundlegende Plot-Funktionen vor und biete ausfuehrliche
# Erklaerungen (auch im Tutorial selbst).


# fiktive Daten generieren
df <- data.frame(X = c(250, 330, 270, 350, 380, 230, 390), 
                 Y = c(330, 330, 340, 370, 350, 325, 300))

# Daten zusammenfassen: Dataframe mit Mittelwert, Standardabweichung, Konfidenzintervallen
df2 <- df %>% melt(variable.name = "Category", value.name = "Count") %>% group_by(Category) %>% summarise(
  mean = mean(Count),
  sd = sd(Count),
  se = sqrt(var(Count)/length(Count)),
  t.val = qt(0.025, df = (length(Count)-1), lower.tail=F),
  ci = se*t.val
)

# einfacher Barplot, der zeigt, wie wichtig die Dimensionen der y-Achse sind
p1 <- ggplot(df2, aes(x = Category, y = mean, fill = Category)) + geom_bar(stat="identity") +
  scale_y_continuous(limits=c(300,335),oob = rescale_none) + ylab("Mittelwert") + xlab("Kategorie") +
  guides(fill = F) + theme_bw()

p2 <- ggplot(df2, aes(x = Category, y = mean, fill = Category)) + geom_bar(stat="identity") +
  ylab("Mittelwert") + xlab("Kategorie") +
  guides(fill = F) + theme_bw()

# beide Plots nebeneinander:
grid.arrange(p1, p2, ncol=2)

# Plot mit Standardabweichung:
ggplot(df2, aes(x = Category, y = mean, fill = Category)) + geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean-ci, ymax = mean+ci), width=.1) +
  ylab("Mittelwert") + xlab("Kategorie") + guides(fill = F) + theme_bw()
# ggsave("plot_mit_cis.png") # zum Speichern bitte "ent-kommentieren" (Strg+Umschalt+C)

# Plot mit 95%-Konfidenzintervallen
# (d.h., vereinfacht gesagt: bei einer Stichprobe können wir mit 95%iger Sicherheit davon 
# ausgehen, dass die tatsächliche Verteilung irgendwo in diesem Bereich liegt)
ggplot(df2, aes(x = Category, y = mean, fill = Category)) + geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean-ci, ymax = mean+ci), width=.1)



###################################
### TEIL 2: DIAGRAMME ERSTELLEN ###
###################################

# Daten einlesen
mannfrau <- rbind(mutate(read.csv("adja_mann.csv", fileEncoding = "UTF-8"), df = "Mann"),
      mutate(read.csv("adja_frau.csv", fileEncoding = "UTF-8"), df = "Frau"))


# Tortendiagramm mit Base Graphics
mannfrau$Genre %>% table %>% pie

# Farben modifizieren und Überschrift hinzufuegen
mannfrau$Genre %>% table %>% pie(col = c("darkgreen", "orange", "red", "blue", "yellow"))
title("Tortendiagramm")

# Balkendiagramm mit Base Graphics
mannfrau$Genre %>% table %>% barplot

# Balkendiagramm mit verkleinerter Schriftgroesse der Argumentnamen und Achsenbeschriftungen
mannfrau$Genre %>% table %>% barplot(cex.names=0.6, cex.axis=0.8)

# Balkendiagramm mit ggplot2
mannfrau_df <- mannfrau$Genre %>% table %>% as.data.frame()
colnames(mannfrau_df) <- c("Textsorte", "Frequenz")

# einfacher Barplot mit Ueberschrift:
ggplot(mannfrau, aes(x = Genre, fill = Genre)) + geom_bar() +
  guides(fill = FALSE) + scale_fill_grey(start = 0.4, end = 0.7) +
  xlab("Textsorte") + ylab("Frequenz") + theme_bw() +
  ggtitle("Verteilung von ADJA+Mann/Frau im DTA") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

# einfacher Barplot mit Ueberschrift und Kursivierung
ggplot(mannfrau, aes(x = Genre, fill = Genre)) + geom_bar() +
  guides(fill = FALSE) + scale_fill_grey(start = 0.4, end = 0.7) +
  xlab("Textsorte") + ylab("Frequenz") + theme_bw() +
  ggtitle(expression(paste(bold("Verteilung von ADJA+"), bolditalic("Mann/Frau"), bold(" im DTA"), collapse=""))) +
  theme(plot.title = element_text(hjust = .5))


# das Ganze als Funktion:
my_barplot <- function(mydata, mycolumn, title, ...) {
  
  # dieser Trick erlaubt uns, spaeter beim Aufrufen der Funktion
  # auf Anfuehrungszeichen zu verzichten:
  mycolumn <- as.character(substitute(mycolumn))
  
  # hier der eigentliche Code - wichtig: Wenn wir ggplot innerhalb einer Funktion
  # benutzen, muessen wir bei den aes-Parametern genau auf die Daten Bezug nehmen,
  # die wir benutzen, d.h. es genuegt nicht, z.B. x = mycolumn zu schreiben, 
  # sondern wir muessen den Dataframe angeben, zu dem die Spalte gehoert:
  p <- ggplot(mydata, aes(x = mydata[,which(colnames(mydata)==mycolumn)],
                        fill = mydata[,which(colnames(mydata)==mycolumn)])) + 
    geom_bar() +
    guides(fill = FALSE) + scale_fill_grey(start = 0.4, end = 0.7) + 
    theme_bw()
  
  # je nachdem, ob "title" spezifiziert ist, mit oder ohne Titel ausgeben:
  if(missing(title)) {
    return(p)
  } else {
    p <- p +
      ggtitle(title) +
      theme(plot.title = element_text(hjust = .5))
    return(p)
  }
  
}

# Funktion anwenden:
my_barplot(mydata = mannfrau, mycolumn = Genre, 
           title = expression(paste(bold("Verteilung von ADJA+"), 
                                    bolditalic("Mann/Frau"), 
                                    bold(" im DTA"), collapse=""))) +
  xlab("Textsorte") + ylab("Frequenz")




# Stacked Barplot mit Base Graphics
mannfrau %>% select(df, Genre) %>% table %>% prop.table(margin = 2) %>%  barplot()



# Stacked Barplot mit ggplot2
mf1 <- mannfrau %>% select(df, Genre) %>% table() %>% as.data.frame
mf2 <- mannfrau %>% select(df, Genre) %>% table() %>% prop.table(mar=2) %>% as.data.frame
mf1 <- mutate(mf1, Rel_Freq = mf2$Freq)

ggplot(mf1, aes(x = Genre, y = Rel_Freq, fill = df, label = Freq)) +
  geom_col() + 
  geom_text(data = subset(mf1, Freq>0), position=position_stack(vjust=0.5)) +
  ylab("Relative Frequenz") +
  guides(fill=guide_legend(title="Lemma"))


# Boxplot



