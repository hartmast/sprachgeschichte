###Zusatzpakete installieren und laden
sapply(c("dplyr", "ggplot2", "reshape2"), function(x) if(!is.element(x, installed.packages())) install.packages(x, dependencies = T))
lapply(list("dplyr", "ggplot2", "reshape2"), require, character.only=T)


###Gesamtfrequenzen
all.freq <- data.frame(Tokens=c(32067087,26930645,29993866,33243616), 
           Texttype=c("Belletristik", 	"Gebrauchsliteratur", "Wissenschaft", "Zeitung"))

###csv einlesen
suesswaren <- read.csv("suesswaren.csv", sep="\t", head=T, encoding = "UTF-8", quote = "'")

###Vektor mit Textsorten
texttypes <- all.freq$Texttype

###relative Frequenzen errechnen
rel.freq <- sapply(1:4, function(i) nrow(filter(suesswaren, Textsorte==texttypes[i])) / all.freq$Tokens[i])

###pro Million Wörter
rel.freq <- rel.freq*1000000

###relative Frequenzen für einzelne Lemmata pro Textsorte
texttype.matrix <- as.data.frame(matrix(ncol=length(texttypes), nrow=length(levels(factor(suesswaren$Lemma)))))
colnames(texttype.matrix) <- texttypes; row.names(texttype.matrix) <- levels(factor(suesswaren$Lemma))

for(i in 1:length(texttypes)) {
  for(j in 1:length(levels(factor(suesswaren$Lemma)))) {
    texttype.matrix[j,i] <- (nrow(filter(suesswaren, Lemma==levels(factor(suesswaren$Lemma))[[j]] & Textsorte==texttypes[i])) /
                               all.freq$Tokens[i])*1000000
  }
}


###plot data
melt(mutate(texttype.matrix, Lemma=rownames(texttype.matrix)))
texttype.matrix

png("suesswaren.png", width=6, height=6.5, un="in", res=300)
ggplot(filter(melt(mutate(texttype.matrix, Lemma=rownames(texttype.matrix))), variable %in% c("Belletristik", "Wissenschaft"))) + 
  geom_bar(aes(x=variable, y=value, fill=Lemma, order=-as.numeric(factor(Lemma))), stat = "identity")  + scale_fill_grey(start = .9, end = 0) +
  ylab("Frequenz pro Million Wörtern") + xlab("Textsorte") + theme_bw() + ggtitle("Süßwaren") +
  theme(plot.title=element_text(lineheight = 1, face="bold", vjust = 1))
dev.off()

###Chi-Quadrat-Test
chisq.suesswaren <- chisq.test(matrix(c(nrow(filter(suesswaren, Textsorte=="Belletristik")), 
         all.freq[1,1]-nrow(filter(suesswaren, Textsorte=="Belletristik")),
         nrow(filter(suesswaren, Textsorte=="Wissenschaft")), 
         all.freq[3,1]-nrow(filter(suesswaren, Textsorte=="Wissenschaft"))),
       ncol=2, nrow=2, byrow = F))


###Funktion zum Berechnen der Effektstärke
get_V <- function(testmatrix) {
  v <- sqrt(testmatrix$statistic/
              sum(testmatrix$observed)*(min(dim(testmatrix$observed))-1))
  return(v)
}

###Effektstärke berechnen
get_V(chisq.suesswaren) ##sehr, sehr geringe Effektstärke!

