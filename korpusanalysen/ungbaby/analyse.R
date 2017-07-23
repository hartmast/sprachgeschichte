
# Pakete laden
if(!is.element("dplyr", installed.packages())) { install.packages("dplyr") }
library(dplyr)

# Optionen
options(stringsAsFactors = F) 
Sys.setlocale("LC_ALL", "de_DE") # nur Mac/Linux, wenn Systemsprache nicht Deutsch


##################
# Daten einlesen #
##################

ungbaby_annotated <- read.csv("ungbaby4.csv", 
                              sep="\t", head=T, quote="'", encoding = "UTF-8")

# Liste mit allen Tokens
alltokens_freq <- c()
for(i in 1:6) {
  period_current <- read.csv(paste("pos_period", i, ".txt", sep="", collapse=""), sep="\t", encoding = "UTF-8", head=F, quote="")
  period_current <- period_current[-grep("\\$", period_current$V2),] #remove punctuation tokens
  alltokens_freq[i] <- sum(as.numeric(period_current$V1))
  rm(period_current)
}

# 50-Jahres-Perioden
periods50 <- seq(1600, 1900, 50)

# Vektor für die Namen der 50-Jahres-Perioden
periods50_print <- sapply(1:6, function(i) paste(periods50[i], "-", periods50[i+1], sep=""))

##################################################
# Konstruktionen, in denen ung-Nomina auftreten: #
# P V-ung, Plural, Determinatorkonstruktionen    #
##################################################

# rel. Freq. von P V-ung
prep.ung <- c()

for(i in 1:length(levels(factor(ungbaby_annotated$Period)))) {
  prep.ung[i] <- length(which(filter(ungbaby_annotated, Period==levels(factor(ungbaby_annotated$Period))[i])$Prep_V_ung!="")) /
    nrow(filter(ungbaby_annotated, Period==levels(factor(ungbaby_annotated$Period))[i]))
}


# Plot: PREP V-ung
# png("prep_v_ung.png", width=5, height=5, un="in", res=300)
plot(1:6, prep.ung, ylim=c(0,0.2), type="b", xaxt="n", pch=18,
     main="[P V-ung]-Konstruktion", ylab="Relative Frequenz", 
     xlab="Periode", lwd=2, yaxt="n")
axis(1, at=c(1:6), labels = periods50_print, cex.axis=0.5)
axis(2, at=seq(0,1,0.1), labels=paste(seq(0,100,10), "%", sep=" "), las=2, cex.axis=.7)
grid(ny=10, nx=0, col="darkgrey")
# dev.off()

# Korrelationstest (Kendall's Tau)
cor.test(1:6, prep.ung, method="kendall")


# Determinatorkonstruktionen
det.ung <- c()

for(i in 1:length(levels(factor(ungbaby_annotated$Period)))) {
  det.ung[i] <- length(which(filter(ungbaby_annotated, Period==levels(factor(ungbaby_annotated$Period))[i])$Determiner!="")) /
    nrow(filter(ungbaby_annotated, Period==levels(factor(ungbaby_annotated$Period))[i]))
}

# Plot: Rel. Freq. von Determinatotren
# png("determiners_ungbaby.png", width=5, height=5, un="in", res=300)
plot(1:6, det.ung*100, ylim=c(0,100), type="b", xaxt="n", pch=18,
     main="ung-Nomina mit Determinator", 
     ylab="Relative Frequenz", xlab="Periode", yaxt="n", lwd=2)
axis(1, at=c(1:6), labels = periods50_print, cex.axis=0.5)
axis(2, at=c(0:10)*10, labels=paste((0:10)*10, " %", sep=""), las=2, cex.axis=0.7)
grid(ny=10, nx=0, col="darkgrey")
# dev.off()

cor.test(1:6, det.ung, method="kendall")


# Pluralisierung

# automatische Erkennung von Pluralformen anhand des -en-Suffixes
ungbaby_annotated$Number <- NA
for(i in 1:nrow(ungbaby_annotated)) {
  if(ungbaby_annotated$Key[i] %in% grep(".*ungen$", ungbaby_annotated$Key, value=T)) {
    ungbaby_annotated$Number[i] <- "plural"
  } else {
    ungbaby_annotated$Number[i] <- "singular"
  }
  print(paste(i, " of ", nrow(ungbaby_annotated), sep="", collapse=""))
}

# Vektor mit rel. Frequenzen
pl.ung <- sapply(1:6, function(i) 
  nrow(filter(ungbaby_annotated, Number=="plural" & 
                Period==levels(factor(ungbaby_annotated$Period))[i])) /
  nrow(filter(ungbaby_annotated, 
              Period==levels(factor(ungbaby_annotated$Period))[i])))

# Frequenz von Determinatorkonstruktion, P V-ung und Pluralisierung

# png("p_det_plur.png", width=5, height=15, un="in", res=300)
par(mfrow=c(3,1))
plot(1:6, prep.ung, ylim=c(0,0.2), type="b", xaxt="n", pch=18,
     main="[P V-ung]-Konstruktion", ylab="Relative Frequenz", 
     xlab="Periode", lwd=2, yaxt="n")
axis(1, at=c(1:6), labels = periods50_print, cex.axis=0.5)
axis(2, at=seq(0,1,0.1), labels=paste(seq(0,100,10), "%", sep=" "), las=2, cex.axis=.7)
grid(ny=10, nx=0, col="darkgrey")

plot(1:6, det.ung*100, ylim=c(0,100), type="b", xaxt="n", pch=18,
     main="ung-Nomina mit Artikel", 
     ylab="Relative Frequenz", xlab="Periode", yaxt="n", lwd=2)
axis(1, at=c(1:6), labels = periods50_print, cex.axis=0.5)
axis(2, at=c(0:10)*10, labels=paste((0:10)*10, " %", sep=""), las=2, cex.axis=0.7)
grid(ny=10, nx=0, col="darkgrey")

plot(1:6, pl.ung*100, xaxt="n", main="Pluralisierung", pch=20, type="b", ylim=c(0, 25),
     xlab="Periode", ylab="Relative Frequenz", yaxt="n", lwd=2)
axis(1, at=c(1:6), labels = periods50_print, cex.axis=0.5)
axis(2, at=c(0:20)*5, labels=paste((0:20)*5, " %", sep=""), las=2, cex.axis=0.7)
grid(ny=10, nx=0, col="darkgrey")
# dev.off()
par(mfrow=c(1,1))




#######################################################
# Type- und Tokenfrequenz, potentielle Produktivitaet #
#######################################################

# Type- und Tokenfrequenz
tok.freq.ungbaby <- type.freq.ungbaby <- c()

for(i in 1:length(levels(factor(ungbaby_annotated$Period)))) {
  tok.freq.ungbaby[i] <-  length(filter(ungbaby_annotated, Period==levels(factor(ungbaby_annotated$Period))[i])$Lemma) / 
    alltokens_freq[i]
  
  type.freq.ungbaby[i] <- length(levels(factor(filter(
      ungbaby_annotated, Period==levels(factor(ungbaby_annotated$Period))[i])$Lemma))) / 
    alltokens_freq[i]
}

 png("rel_tok_type_freq_ungbaby.png", width=13, height=6.5, un="in", res=300)
par(mfrow=c(1,2))
plot(1:6, tok.freq.ungbaby, ylim=c(0,0.03), xaxt="n", 
     main=expression(paste("Tokenfrequenz, DTA", italic("baby"), sep="", collapse="")),
     xlab="Periode", ylab="Relative Frequenz", type="b", lwd=2, pch=18)
axis(1, at=c(1:6), labels=periods50_print, cex.axis=0.7)
grid(ny=10, nx=0, col="darkgrey")

plot(1:6, type.freq.ungbaby, xaxt="n", 
     main=expression(paste("Typefrequenz, DTA", italic("baby"), sep="", collapse="")),
     xlab="Periode", ylab="Relative Frequenz", type="b", lwd=2, pch=18, ylim=c(0,0.005))
axis(1, at=c(1:6), labels=periods50_print, cex.axis=0.7)
grid(ny=10, nx=0, col="darkgrey")
dev.off()
 par(mfrow=c(1,1))

# Korrelationstests: Aendern sich Type- / Tokenfrequenz signifikant?
cor.test(1:6, tok.freq.ungbaby, method="kendall")
cor.test(1:6, type.freq.ungbaby, method="kendall")

# Potentielle Produktivitaet

# Vektor fuer Ergebnisse
pot.prod.ungbaby.annotated <- c()

# Vektor mit Hapaxen
hapaxes_ungbaby <- as.character(filter(as.data.frame(table(ungbaby_annotated$Lemma)), 
                                       Freq==1)$Var1)

# relative Frequenz von Hapaxen pro Periode
for(i in 1:length(levels(factor(ungbaby_annotated$Period)))) {
  pot.prod.ungbaby.annotated[i] <- length(filter(ungbaby_annotated, Period==levels(factor(ungbaby_annotated$Period))[i] &
                  Lemma %in% hapaxes_ungbaby)$Lemma) / length(
                    filter(ungbaby_annotated, Period==levels(factor(ungbaby_annotated$Period))[i])$Lemma)
}

# Plot
png("pot_prod_ung_DTAbaby.png", width=6.5, height=5, un="in", res=300)
plot(1:6, pot.prod.ungbaby.annotated, xaxt="n", 
     main="Potentielle Produktivit\u00e4t, ung, DTAbaby",
     pch=18, type="b", ylim=c(0,0.1), ylab="Potentielle Produktivi\u00e4t", 
     xlab="Periode", lwd=2)
axis(1, at=c(1:6), labels = periods50_print, cex.axis=0.7)
grid(ny=10, nx=0, col="darkgrey")
dev.off()


# alternative Operationalisierung: Neue Types

# Fnhd. Daten einlesen
ung_fnhd <- read.csv("ung_fnhd_MASTER.csv", sep=";", head=T,
                     encoding="UTF-8", quote="")

# in ung_fnhd sind alle Umlaute und ß ersetzt,
# daher muss dies zur Vergleichbarkeit auch für die
# DTAbaby-Daten gemacht werden

ungbaby_annotated2 <- ungbaby_annotated
ungbaby_annotated2$Lemma <- gsub("ü", "ue", ungbaby_annotated2$Lemma)
ungbaby_annotated2$Lemma <- gsub("ö", "oe", ungbaby_annotated2$Lemma)
ungbaby_annotated2$Lemma <- gsub("ä", "ae", ungbaby_annotated2$Lemma)
ungbaby_annotated2$Lemma <- gsub("ß", "ss", ungbaby_annotated2$Lemma)


# Welche Types kommen auch im MzFNHD vor?
ungbaby_annotated2$New <- ifelse(ungbaby_annotated2$Lemma %in% 
                                   levels(factor(ung_fnhd$Lemma)), 0, 1)


# welche Types sind in der jeweiligen Periode neu?
for(i in 1:nrow(ungbaby_annotated2)) {
  years_before <- levels(factor(ungbaby_annotated2$Period))[which(levels(factor(ungbaby_annotated2$Period))<ungbaby_annotated$Period[i])]
  if(ungbaby_annotated2$New[i]==1 & ungbaby_annotated2$Lemma[i] %in% levels(factor(filter(ungbaby_annotated2, Period %in% years_before)$Lemma))) {
    ungbaby_annotated2$New[i] <- 0
  }
  print(i)
}


# Verlaufskurve
png("neue_types_ungbaby.png", width=6.5, height=5, un="in", res=300)
par(mar = c(5, 6, 4, 2) + 0.1)
ungbaby_annotated2 %>% select(Period, New) %>% table %>% prop.table(mar=1) %>% 
  .[,2] %>% plot(type="b", ylim=c(0.1,0.2), ylab="Anteil neuer Types\nan allen Types", pch=18,
                 main="Neue Types", xaxt="n", xlab="Zeitschnitt")
axis(1, at=c(1:length(periods50_print)), labels=periods50_print, cex.axis=0.7)
par(mar = c(5, 4, 4, 2) + 0.1)
dev.off()

