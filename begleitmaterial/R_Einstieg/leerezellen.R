# Im Folgenden lesen wir die Dateien "leerezellen.csv" und "leerezellen2.csv" ein.
# Bei beiden handelt es sich um Dateien, in denen Zellen leer gelassen sind bzw. fehlen.


# Datei mit FEHLENDER Zelle:
read.table("leerezellen.csv", sep = "\t", quote = "",
           fileEncoding = "UTF-8", head = T)  # generiert Fehler...

# Datei mit LEERER Zelle:
read.table("leerezellen2.csv", sep = "\t", quote = "", 
           fileEncoding = "UTF-8", head = T)

# Datei mit FEHLENDER Zelle - mit der Option fill = TRUE:
read.table("leerezellen.csv", sep = "\t", quote = "",
           fileEncoding = "UTF-8", head = T, fill = T)  # funktioniert,
                                                  # füllt fehlende Zelle(n) mit NA auf

# "Ideale" Datei ohne fehlende und/oder leere Zellen
read.table("leerezellen3.csv", sep = "\t", quote = "",
           fileEncoding = "UTF-8", head = T)  # funktioniert
