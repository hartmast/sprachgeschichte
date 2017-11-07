##############################################################################################
#                                                                                            #
#                                      getFnhdC 2.1                                          #
#                                                                                            #
# Aktualisierung 11.08.2017:                                                                 #
# Verbesserte Umlaut-Unterstuetzung; dafuer wird Zusatzpaket                                 #
# data.table eingesetzt.                                                                     #
#                                                                                            #
# Dieses Skript dient zur einfachen Erstellung von Konkordanzen aus der Online-Suche         #
# des Bonner Fruehneuhochdeutschkorpus (FnhdC). Es kann frei verwendet und distribuiert      #
# werden und kommt ohne jede Garantie. Die Funktion pause() und die Funktion zum             #
# Einlesen der Daten wurden uebernommen aus dem Skript zur Kollostruktionsanalyse von        #
# Stefan Th. Gries:                                                                          #
# Gries, Stefan Th. 2007. Coll.analysis 3.2a. A program for R for Windows 2.x.               #
#                                                                                            #
# Bei Fragen und Problemen wenden Sie sich gern an mich: stefan.hartmann[at]uni-hamburg.de   #
#                                                                                            #
##############################################################################################

# Zusatzpaket "data.table" installieren, falls noch nicht installiert
if(!is.element("data.table", installed.packages())) {
  install.packages("data.table")
}

#  Paket laden
library(data.table)


###Pause-Funktion zum Anzeigen von Text
pause<-function() {
  cat("Um weiterzumachen, dr\u00fccken Sie bitte <Enter>... ")
  readline()
  invisible()
}

getFnhd <- function() {
  # Auswaehlen der Konkordanzdatei
  cat("Bitte w\u00e4hlen Sie die Datei mit der Konkordanz aus.\n
      Wenn Sie nicht wissen, was hiermit gemeint ist, \n
      konsultieren Sie bitte die Anleitung, verf\u00fcgbar unter\n
      https://pfriemelpfuhl.files.wordpress.com/2016/10/fnhdc_konkordanzen.pdf\n"); pause()
  data <- readLines(file.choose(), encoding="UTF-8", warn = F)
  
  # Auswaehlen der Exportdatei
  cat("Bitte w\u00e4hlen Sie eine Datei aus, in der Sie die Ergebnisse \nspeichern m\u00f6chten. 
      Bitte beachten: Wenn Sie eine BEREITS EXISTIERENDE Datei waehlen, wird diese \u00dcBERSCHRIEBEN. 
      Es erfolgt keine R\u00fcckfrage, ob Sie die Datei ueberschreiben wollen."); pause()
  export <- file.path(file.choose())
  
  
  # Auffinden der Treffer
  gefunden <- grep("<li>Gefunden in", as.character(data))
  satz <- gefunden+4
  
  for(k in 1:length(gefunden)) {
    ###Keyword des Treffers finden
    tryCatch({ 
      find_keyword <- unlist(strsplit(data[gefunden[k]], "wf=|#"))[2]
    
    ##Zeitstufe und Quelle des Treffers finden
    find_keyword <-unlist(strsplit(data[gefunden[k]], "wf=|#"))[2]
    temp01 <- unlist(strsplit(data[gefunden[k]], "<a href.*'>"))[2]
    quelle <- unlist(strsplit(temp01, "\\(Gegend\\:"))[1]
    temp02 <- unlist(strsplit(temp01, "\\(Gegend\\:"))
    zeitstufe <- gsub(" ", "", gsub(")", "", unlist(strsplit(temp02, "Zeitstufe\\:|<\\/a"))[3]))
    gegend <- gsub(",", "", unlist(strsplit(temp02, "Zeitstufe\\:|<\\/a"))[2]) 
    
    ###jeder einzelne Beleg als temporaeres Element
    temp <- unlist(strsplit(data[satz[k]], "<span id="))
    temp <- temp[2:length(temp)]
    
    ##Position des Keywords im Satz
    position_keyword <- grep(find_keyword, temp)[1]
    
    if(!is.na(position_keyword)) {
      ###finde linken Kontext
      if(position_keyword>1) {
        words_left <- c()
        
        for(i in 1:(position_keyword-1)) {
          words_left[i] <- gsub(" ", "", gsub("<\\/span>", "", unlist(strsplit(temp[i], "\\\">"))[2]))
        }
      } else {
        words_left <- "LEER"
      }
      
      
      
      ## finde Keyword
      keyword <- gsub(" ", "", gsub("<\\/span>", "", unlist(strsplit(temp[position_keyword], "\\\">"))[2]))
      
      # falls noch Metadaten im Keyword, diese entfernen
      if(length(grep('<', keyword))>0) {
        keyword <- unlist(strsplit(keyword, "<"))[1]
      }
      
      # Interpunktion aus Keyword entfernen
      keyword <- gsub("[[:punct:]]", "", keyword)
      
      # finde Lemma und POS
      metadaten <- unlist(strsplit(temp[position_keyword], "\\\">"))[1]
      lemma <- gsub(" ", "", unlist(strsplit(unlist(strsplit(temp[position_keyword], "Lemma: &nbsp;"))[2], "&"))[1])
      pos <- gsub(" ", "", unlist(strsplit(unlist(strsplit(temp[position_keyword], "Typ: &nbsp;"))[2], "&"))[1])
      
      
      # finde rechten Kontext
      words_right <- c()
      
      for(i in 1:(length(temp)-position_keyword)) {
        words_right[i] <- gsub(" ", "", gsub("<\\/span>", "", unlist(strsplit(temp[i+position_keyword], "\\\">"))[2]))
      }
    } else {
      words_left <- "LEER"; keyword <- "LEER"; words_right <- "LEER"
    }
    
    
    ###in Output-Tabelle eintragen
    if(k==1) {
      fwrite(as.list("Quelle\tGegend\tZeitstufe\tKontext_links\tKeyword\tKontext_rechts\tLemma\tPOS"), file = export,
                  row.names = F, col.names = F, quote = F)
    }
    
    

    # write.table(paste(c(quelle, gegend, zeitstufe, 
    #   paste(words_left, sep=" ", collapse=" "),
    #   keyword,
    #   paste(words_right, sep=" ", collapse=" "),
    #   lemma, pos), collapse="\t"), file = export, fileEncoding = "UTF-8", 
    #   row.names = F, col.names = F, quote = F, append = T)
    
   fwrite(as.list(paste(c(quelle, gegend, zeitstufe, 
                        paste(words_left, sep=" ", collapse=" "),
                        keyword,
                        paste(words_right, sep=" ", collapse=" "),
                        lemma, pos), collapse="\t")), file = export,  
                row.names = F, col.names = F, quote = F, append = T)

    

  },
  error=function(e) {}
    )
  }
  

  
}

# Funktion ausfuehren
getFnhd()
