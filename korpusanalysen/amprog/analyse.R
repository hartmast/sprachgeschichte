# set options
options(stringsAsFactors = F)
Sys.setlocale("LC_ALL", "de_DE") # nur Mac/Linux, wenn Systemsprache nicht Deutsch


# load packages
sapply(c("lme4", "lmerTest"), 
       function(x) if(!is.element(x, installed.packages())) install.packages(x, dependencies = T))

lapply(list("lme4", "lmerTest"), 
       require, character.only=T)


# Daten einlesen
duration     <- read.csv("duration.csv", sep="\t", head=T, quote="",
                     encoding = "UTF-8")

acceptability <- read.csv("acceptability.csv", sep="\t", head=T, quote="",
                     encoding = "UTF-8")



# Fragen ergaenzen
aspect <- read.csv("all_questions_aspect.csv", sep="\t", head=T,
                   quote="", encoding = "UTF-8")

for(i in 1:nrow(acceptability)) {
  acceptability$Question[i] <- aspect[which(aspect$Question_Type_ID==acceptability$Question_Type_ID[i] & aspect$Type==acceptability$Type[i]),]$Question
}


#########################
# Plot: Akzeptabilitaet #
#########################

# nach Mittelwert der Akzeptabilitaet sortieren
acceptability_mean <- c()

for(i in 1:nrow(acceptability)) {
  acceptability_mean[i] <- mean(na.omit(as.numeric(acceptability[i,(5:length(acceptability))])))
}

acceptability <- acceptability[order(acceptability_mean),]


# nach Praesens vs Progressiv sortieren
acceptability <- acceptability[order(acceptability$Type),]


# als Liste
acceptability_list <- list()

for(i in 1:nrow(acceptability)) {
  acceptability_list[i] <- list(as.numeric(acceptability[i,(5:length(acceptability))]))
}


# plot

png("akzeptabilitaet.jpg", width=10, height=6, un="in", res=300)
par(mar=c(12, 4.1, 4.1, 2.1))
boxplot(acceptability_list, names=acceptability$Question,
       xaxt="n", main="Akzeptabilit\u00e4t")
axis(1, at=1:44, labels=FALSE)
text(x=seq_along(acceptability$Question), y = par("usr")[3] - 0.2,
     labels=acceptability$Question, srt=45, adj=1, xpd=TRUE,
     cex=0.6)
dev.off()
par(mar=c(5, 4, 4, 2) + 0.1)



########################
# SCHAETZUNG DER DAUER #
########################

# make a list (again)

allquestionslist <- list()
for(i in 1:nrow(duration)) {
  allquestionslist[i] <- list(as.numeric(duration[i,6:length(duration)]))
}


# plot

png("duration.jpg", width=15, height=20, un="in", res=300)
par(mfrow=c(2,1))
par(xpd=T)
par(mar=c(17, 4.1, 4.1, 2.1))
boxplot(allquestionslist[which(duration$Length=="short")], xaxt="n", col=rep(c("grey25", "grey75"), 7),
        main="Kurze Ereignisse: Sch\u00e4tzung der Dauer", cex.main=0.8, cex.axis=0.8)
axis(1, at=1:14, labels=FALSE)
lines(-1:44, rep(500, 46), lty=2)
text(x=seq_along(duration$Question[which(duration$Length=="short")]), y = par("usr")[3] - 0.25,
     labels=duration$Question[which(duration$Length=="short")], srt=45, adj=c(1.05,1.7), xpd=TRUE,
     cex=1)
par(mar=c(5, 4, 4, 2) + 0.1)
par(xpd=F)


par(xpd=T)
par(mar=c(18, 4.1, 4.1, 2.1))
boxplot(allquestionslist[which(duration$Length %in% c("medium", "long"))], xaxt="n", col=rep(c("grey25", "grey75"), 15),
        main="(Mittel-)Lange Ereignisse: Sch\u00e4tzung der Dauer", cex.main=0.8, cex.axis=0.8)
axis(1, at=1:30, labels=FALSE)
lines(-1:44, rep(500, 46), lty=2)
text(x=seq_along(duration$Question[which(duration$Length %in% c("medium", "long"))]), y = par("usr")[3] - 0.3,
     labels=duration$Question[which(duration$Length %in% c("medium", "long"))], srt=45, adj=c(1.05,1.7), xpd=TRUE,
     cex=1)
dev.off()
par(mar=c(5, 4, 4, 2) + 0.1)
par(xpd=F)
par(mfrow=c(1,1))
