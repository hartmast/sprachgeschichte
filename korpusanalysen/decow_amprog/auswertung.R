options(stringsAsFactors = F)
Sys.setlocale("LC_ALL", locale="de_DE")


#################
# Preliminaries #
#################

# (install and) load packages
sapply(c("dplyr", "ggplot2", "rjson", "ggmap", "reshape2", "devtools"), 
       function(x) if(!is.element(x, installed.packages())) 
         install.packages(x, dependencies = T))

lapply(list("dplyr", "ggplot2", "rjson", "ggmap", "reshape2"), 
       require, character.only=T)


# make sure that older version of ggplot is installed
# because ggmap won't work with the current one
if(length(grep("2\\.2\\..", packageVersion("ggplot2")))<1) {
  devtools::install_version("ggplot2", version="2.2.0")
}

# if ggmap yields an error related to "ggproto", try
# devtools::install_github("dkahle/ggmap")



####################
# Helper Functions #
####################

# freegeoip (from https://heuristically.wordpress.com/2013/05/20/geolocate-ip-addresses-in-r/)

freegeoip <- function(ip, format = ifelse(length(ip)==1,'list','dataframe'))
{
  if (1 == length(ip))
  {
    # a single IP address
    require(rjson)
    url <- paste(c("http://freegeoip.net/json/", ip), collapse='')
    ret <- fromJSON(readLines(url, warn=FALSE))
    if (format == 'dataframe')
      ret <- data.frame(t(unlist(ret)))
    return(ret)
  } else {
    ret <- data.frame()
    for (i in 1:length(ip))
    {
      r <- freegeoip(ip[i], format="dataframe")
      ret <- rbind(ret, r)
    }
    return(ret)
  }
}


############################
# read and manipulate data #
############################

# read data

# CQL search query: [word="sein"] [word="am"] [word=".*en" & tag="NN|V.*"]
df <- scan("decow16_test_amprog.txt", 
           what="char", encoding = "UTF-8", sep="\n", quote = "")

# CQL search query: [word="sein"] []{1,7} [word="am"] [word=".*en" & tag="NN|V.*"]
df1b <- scan("decow16_text_amprog_cleft.txt", what="char", encoding = "UTF-8", sep="\n", quote = "")

# combine both dataframes
df <- c(df, df1b)

# split up into columns:
# separate at the first tab but ignore all others
df2 <- as.data.frame(matrix(ncol=2, nrow=0))
colnames(df2) <- c("meta", "text")

for(i in 1:length(df)) {
  h1 <- unlist(strsplit(df[i], "\t"))[1]
  h2 <- paste(unlist(strsplit(df[i], "\t"))[2], sep="", collapse="")
  
  df2[i,] <- c(h1,h2)
}


# split up "text" into columns:
# separate at first < and ignore all others
df2$left <- NA
df2$key <- NA
df2$right <- NA

for(i in 1:nrow(df2)) {
  h1 <- unlist(strsplit(df2$text[i], "<"))[1]
  h2 <- unlist(strsplit(paste(unlist(strsplit(df2$text[i], "<"))[2], sep="", collapse=""), ">"))[1]
  h3 <- paste(unlist(strsplit(paste(unlist(strsplit(df2$text[i], "<"))[2], sep="", collapse=""), ">"))[2], sep="", collapse="")
  
  df2$left[i] <- h1
  df2$key[i] <- h2
  df2$right[i] <- h3
}


# omit empty cells
df2 <- df2[grep("\\|", df2$key),]


# split up "key"
df2$key_phrase <- sapply(1:nrow(df2), 
        function(i) paste(gsub(" ", "", 
              unlist(strsplit(df2$key[i], "/|  "))[seq(1, length(unlist(strsplit(df2$key[i], "/|  "))), 2)]), collapse=" "))


df2$key_lemma <- gsub("\\|", "", sapply(1:nrow(df2), function(i) paste(gsub(" ", "", unlist(strsplit(df2$key[i], "/|  "))[length(unlist(strsplit(df2$key[i], "/|  ")))]))))


df2$key_word <- unlist(sapply(1:nrow(df2), function(i) paste(gsub(" ", "", unlist(strsplit(df2$key[i], "/|  "))[length(unlist(strsplit(df2$key[i], "/|  ")))-1]))))



# split up metadata
df2$id <- sapply(1:nrow(df2), function(i) unlist(strsplit(df2$meta[i], ","))[1])
df2$ip <- sapply(1:nrow(df2), function(i) unlist(strsplit(df2$meta[i], ","))[2])
df2$country <- sapply(1:nrow(df2), function(i) unlist(strsplit(df2$meta[i], ","))[3])

# omit attestations from countries other than Germany
df2 <- filter(df2, country=="DE")


# add random numbers
# (the first 2000 will be part of the sample re-imported below)
df2$random <- sample(1:nrow(df2), nrow(df2))

# export for manual checking
# (2000-token sample will be re-imported below)
write.table(df2, "amprog_all.csv", sep="\t", row.names=F, quote=F, fileEncoding = "UTF-8")

# omit false hits
df2 <- df2[-which(tolower(df2$key_word) %in% c("leben", "linken")),]


###############
# add geodata #
###############

# get longitudes and latitudes of geoips
ips <- unique(df2$ip)
ips <- as.data.frame(ips); colnames(ips) <- "ip"
ips$lon <- ips$lat <- ips$city <- NA

for(i in 1:nrow(ips)) {
  fgi <- freegeoip(ips$ip[i])
  
  ips$lat[i] <- fgi$latitude
  ips$lon[i] <- fgi$longitude
  ips$city[i] <- fgi$city
  
  rm(fgi)
  print(i)
}


# add geodata to df
df2$lon <- df2$lat <- df2$city <- NA

for(i in 1:nrow(ips)) {
  df2[which(df2$ip==ips$ip[i]),]$lon <- ips$lon[i]
  df2[which(df2$ip==ips$ip[i]),]$lat <- ips$lat[i]
  df2[which(df2$ip==ips$ip[i]),]$city <- ips$city[i]
}


##################################
# plot geographical distribution #
##################################

# get geographical distribution / frequencies
lonlat <- df2[!duplicated(df2[which(colnames(df2) %in% c("lon", "lat"))]),
    which(colnames(df2) %in% c("lon", "lat"))]

lonlat$am <- NA

# get frequency of "am" per lon/lat
for(i in 1:nrow(lonlat)) {
  lonlat$am[i] <- length(which(df2$lon==lonlat$lon[i] & df2$lat==lonlat$lat[i]))
}


# plot Germany
germ <- get_map(location="germany", zoom=6, 
                language = "de-DE", maptype = "terrain") 
germ_map <- ggmap(germ) + theme_bw() + theme(panel.grid.major=element_blank())

# add am-progressive geodata
p <- germ_map + geom_point(aes(x=lon, y=lat, size=log10(am)), 
                           data=lonlat, col="darkblue")
p + labs(size=expression(paste("Log"[10], "Freq"), sep="", collapse=""),
         title="Alle Belege") + 
  ylab("") + xlab("") + theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()) +  
  theme(plot.title = element_text(hjust = 0.5, face="bold"))
ggsave("amprog_all.png")


#############################
# manually corrected sample #
#############################

# manually corrected sample
spl <- read.table("amprog_decow_sample_annotated.csv",
                  sep="\t", head=T, quote="", encoding = "UTF-8", fill=T)

# omit false hits
spl <- spl[-which(spl$cleft %in% c("special", "discard")),]

# add geographical data
spl$lat <- spl$lon <- spl$city <- NA

for(i in 1:nrow(ips)) {
  if(ips$ip[i] %in% spl$ip) {
    spl[which(spl$ip==ips$ip[i]),]$lon <- ips$lon[i]
    spl[which(spl$ip==ips$ip[i]),]$lat <- ips$lat[i]
    spl[which(spl$ip==ips$ip[i]),]$city <- ips$city[i]
  }
}

# frequencies
lonlat2 <- spl[!duplicated(spl[which(colnames(spl) %in% c("lon", "lat"))]),
              which(colnames(spl) %in% c("lon", "lat"))]
lonlat2$without_obj <- lonlat2$with_obj  <- NA

for(i in 1:nrow(lonlat2)) {
  lonlat2$without_obj[i] <- length(which(spl$lon==lonlat2$lon[i] & 
                                           spl$lat==lonlat2$lat[i] & spl$cleft!="obj"))
  lonlat2$with_obj[i] <- length(which(spl$lon==lonlat2$lon[i] & 
                                        spl$lat==lonlat2$lat[i] & spl$cleft=="obj"))
}


# melt df
lonlat2 <- melt(lonlat2, id.vars=c("lon", "lat"))

# plot data
lonlat2$variable <- relevel(lonlat2$variable, "without_obj")
germ_map + geom_point(aes(x=lon, y=lat, size=value), col="darkred",
                      data=filter(lonlat2, variable=="with_obj")) +
  labs(size="Freq (abs.)", title="mit Objektergänzung")  + theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()) +
  ylab("") + xlab("") + theme(plot.title = element_text(hjust = 0.5, face="bold"))
ggsave("with_obj.png")


