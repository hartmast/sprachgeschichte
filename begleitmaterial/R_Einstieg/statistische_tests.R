# Pakete installieren bzw. laden
sapply(c("vcd", "reshape2", "ggplot2", "scales", "dplyr"), function(x) 
  if(!is.element(x, installed.packages())) install.packages(x, dependencies = T))

lapply(list("vcd", "reshape2", "ggplot2", "scales", "dplyr"), require, character.only=T)

# Optionen
options(stringsAsFactors = F)


######################
## Chi-Quadrat-Test ##
######################

# fiktiven Datensatz generieren
df <- data.frame(KURS_A=c(BESTANDEN=25,NICHT_BESTANDEN=5), 
                 KURS_B=c(BESTANDEN=17,NICHT_BESTANDEN=13))
chisq.test(as.matrix(df))

# Abhaengigkeit des Chi-Quadrat-Tests von der Stichprobengroesse
df2 <- data.frame(KURS_A=c(BESTANDEN=250,NICHT_BESTANDEN=50), 
                 KURS_B=c(BESTANDEN=170,NICHT_BESTANDEN=130))
chisq.test(as.matrix(df2))

# mit assocstats:
assocstats(as.matrix(df))
assocstats(as.matrix(df2))


##################################
# t-Test und Mann-Whitney U-Test #
##################################

## a) t-Test: ##

# fiktive Daten generieren
set.seed(1138) # damit bekommen Sie beim Zufallsgenerator (naechste Zeile) immer die gleichen Daten
df <- round(rnorm(60, mean = 20, sd = 5))
df <- sort(df)
df <- as.data.frame(df)
df$KURS <- c(sample(c("KURS_A", "KURS_B"), 30, prob = c(0.2,0.8), replace = T),
             sample(c("KURS_A", "KURS_B"), 30, prob = c(0.8,0.2), replace = T))
colnames(df) <- c("PUNKTE", "KURS")


# zuerst plotten wir die Daten...
# als Boxplot:
ggplot(df, aes(x = KURS, y = PUNKTE)) + geom_boxplot(notch = TRUE)


# ueberpruefen, ob die Daten der Normalverteilung entsprechen
# (denn das ist Voraussetzung fuer t-Test)
shapiro.test(df$PUNKTE) # ja


# t-Test
t.test(subset(df, KURS == "KURS_A")$PUNKTE,
       subset(df, KURS == "KURS_B")$PUNKTE)



## b) Mann-Whitney U-Test: ##

# fiktive Daten generieren
set.seed(1138)
df <- data.frame(KURS_A = c(round(rnorm(25, mean = 10, sd = 5)),
                      round(rnorm(5, mean = 25, sd = 5))),
           KURS_B = c(abs(round(rnorm(17, mean = 10, sd = 5))),
                      abs(round(rnorm(13, mean = 25, sd = 5)))))



# zuerst plotten wir die Daten...
# dafuer muessen wir sie zunaechst ins lange Format ueberfuehren:
df2 <- melt(df) 

# als Boxplot:
ggplot(df2, aes(x = variable, y = value)) + geom_boxplot(notch = TRUE)

# ueberpruefen, ob Voraussetzungen fuer t-Test gegeben sind:
shapiro.test(df2$value) # nicht normalverteilt

# Wilcoxon Rank-Sum Test
wilcox.test(subset(df2, variable=="KURS_A")$value,
            subset(df2, variable=="KURS_B")$value, exact = F)

