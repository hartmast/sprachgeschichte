
# Optionen
options(stringsAsFactors = F)
Sys.setlocale("LC_ALL", "de_DE") # nur Mac/Linux, wenn Systemsprache nicht deutsch

# Pakete installieren (falls noch nicht installiert)
lapply(c("dplyr", "ggplot2", "scales"), 
       function(x) if(!is.element(x, installed.packages())) install.packages(x, dependencies = T))

# Pakete laden
lapply(c("dplyr", "ggplot2", "scales"), 
       require, character.only=T)


# Daten einlesen
belege <- read.csv("data.csv", sep="\t", head=T)



# Belebtheit und Grossschreibung

# Faktoren neu anordnen
belege$Animacy_finegrained <- factor(belege$Animacy_finegrained)
belege$Animacy_finegrained <- factor(belege$Animacy_finegrained,
                                     levels(belege$Animacy_finegrained)[c(1,2,4,3,6,5,9,8,7,11,10)])

belege$upperCase <- factor(belege$upperCase, levels = rev(levels(factor(belege$upperCase))))

# Plot
png("belebtheit_grossschreibung.png", width=6.5, height=5, un="in", res=300)
par(xpd=T)
par(mar=c(7, 4, 4, 2) + 0.1)
belege %>% select(Animacy_finegrained, upperCase) %>% table %>% prop.table(mar=1) %>%
  t() %>% barplot(space=0.1, 
                  names.arg=rep("", 
                                length(levels(factor(belege$Animacy_finegrained)))),
                  main = "Belebtheit und Großschreibung", ylab = "Anteil",
                  col=c("grey25", "grey75"), yaxt="n")
axis(2, at=seq(0,1,0.1), labels=paste(seq(0,100,10), "%", sep=" "), las=2, cex.axis=0.7)
text(sapply(0:(length(levels(factor(belege$Animacy_finegrained)))-1)+1,
            function(i) i+(0.1*i)),
     -0.1, labels = levels(factor(belege$Animacy_finegrained)),
     cex=0.7, srt=45, pos=2)
par(mar=c(5, 4, 4, 2) + 0.1)
legend(-2,-0.3, fill=c("grey75", "grey25"), legend=c("klein", "gross"), cex=0.7)
dev.off()
par(xpd=F)


# Warum so viel Grossschreibung bei abstrakt Maß?
filter(belege, Animacy_finegrained=="abstrakt Maß") %>% select(Lemma, upperCase) %>%
  table


# Was ist die "Avantgarde" bei Abstrakta?
abs <- filter(belege, Animacy_finegrained=="abstrakt") %>% select(upperCase, Freq) %>% table
abs <- as.data.frame.table(t(abs))

# Wie verteilen sich die abs. Frequenzen auf die Belebtheitskategorien?
table(belege$Animacy_finegrained) %>% barplot(cex.names=0.6, las=2)

# Type-Token-Ratios der Belebtheitskategorien
sapply(1:length(levels(belege$Animacy_finegrained)), function(i) 
  length(levels(factor(filter(belege, 
                            Animacy_finegrained==levels(belege$Animacy_finegrained)[i])$Lemma))) /
  nrow(filter(belege, Animacy_finegrained==levels(belege$Animacy_finegrained)[i]))) %>% 
  barplot(names.arg=levels(factor(belege$Animacy_finegrained)), las=2, cex.names=0.7,
          main="Type-Token-Ratio nach Belebtheitskategorie", cex=.7)


# Korrelation Grossschreibung, Type-Token-Ratio? ----------

# Vektor mit rel. Freq. Großschreibung
relFreqGS <- belege %>% select(Animacy_finegrained, upperCase) %>% table %>% prop.table(mar=1) %>% .[,2]

# Vektor mit Type-Token-Ratio
ttr <- sapply(1:length(levels(belege$Animacy_finegrained)), function(i) 
  length(levels(factor(filter(belege, 
                              Animacy_finegrained==levels(belege$Animacy_finegrained)[i])$Lemma))) /
    nrow(filter(belege, Animacy_finegrained==levels(belege$Animacy_finegrained)[i])))

# Korrelationstest
cor.test(ttr, relFreqGS, method="kendall")


# haeufigste Lemmata
tbl <- belege %>% group_by(Lemma) %>% summarise(
  upper = sum(upperCase==TRUE),
  lower = sum(upperCase == FALSE),
  N = n()
)

# 20 haeufigste Lemmata
tbl[order(tbl$N, decreasing = T),] %>% head(20)

