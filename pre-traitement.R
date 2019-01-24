# :::::::::::::::::::: GROUPE A ::::::::::::::::::::
# ::::::::::::::::::::::::::::::::::::::::::::::::::
# ::::::::::::::::: Antoine BELLOIS ::::::::::::::::
# :::::::::::::::::: Hélène FOULON :::::::::::::::::
# :::::::::::::::::: Amélie LACHAUD ::::::::::::::::
# ::::::::::::::::: Delphine LE BAIL :::::::::::::::
# ::::::::::::::::: Hugo MICCINILLI ::::::::::::::::
# ::::::::::::::: Hugo MORANGE-GAPIHAN :::::::::::::
# ::::::::::::::::::: Fiona TUFFIN :::::::::::::::::
# ::::::::::::::::::::::::::::::::::::::::::::::::::

#Library nécéssaire
library(sf)
library(dplyr)
library(raster)
library(highcharter)
library(readr)
#fonction nécessaire
Unaccent <- function(text) {
  text<-gsub("oe","œ",text)
  text<-gsub("é","e",text)
  text<-gsub("ê","e",text)
  text<-gsub("è","e",text)
  text<-gsub("à","a",text)
  text<-gsub("ô","o",text)
  text<-gsub("-"," ",text)
  # text <- gsub("[`^~\"]", " ", text)
  # text <- iconv(text, to="ASCII//TRANSLIT//IGNORE")
  # text <- gsub("[`^~\"]", "", text)
  return(text)
}
#Importation du nouveau jeu de donne
path<-gsub("pretraitement","France",getwd())
fichier<-list.files(path)
for(i in 1:length(fichier)){
  file.remove(paste(path,"/",fichier[i],sep = ""))
}
download.file("http://data.noise-planet.org/noisecapture/France.zip",destfile = paste(path,"/france.zip",sep = ""))
unzip(paste(path,"/france.zip",sep = ""),exdir = path)
file.remove(paste(path,"/france.zip",sep = ""))
fichier2<-list.files(path)
for(i in 1:length(fichier2)){
  file.rename(paste(path,"/",fichier2[i],sep = ""),paste(path,"/",fichier[i],sep = ""))
}
remove(fichier,fichier2)

#Importation des variables points, areas et tracks
##Areas
fichier<-list.files(path = "France",pattern = "*.areas.geojson")
areas<-st_read(paste("France/",fichier[1],sep = ""))
###recuper le nom departement et région
char<-Unaccent(substr(fichier[1],8,nchar(fichier[1])-14))
sep<-regexpr("_",substr(fichier[1],8,nchar(fichier[1])-14))[1]
reg<-rep(substr(char,1,sep-1),dim(areas)[1])
depart<-rep(substr(char,sep+1,nchar(char)),dim(areas)[1])
areas$departement<-depart
areas$region<-reg
###filtre les varibles utilies
areas<-as.data.frame(cbind(areas$departement,areas$region,areas$laeq,areas$geometry))
names(areas)<-c("departement","region","laeq","geometry")
areas$departement<-as.character(areas$departement)
areas$region<-as.character(areas$region)
areas$laeq<-as.numeric(as.character(areas$laeq))
###Rassembler tout les jeu de données 
for(i in 2:length(fichier)){
  areas1<-st_read(paste("France/",fichier[i],sep = ""))
  #recuper le nom departement et région
  char<-Unaccent(substr(fichier[i],8,nchar(fichier[i])-14))
  sep<-regexpr("_",substr(fichier[i],8,nchar(fichier[i])-14))[1]
  reg<-rep(substr(char,1,sep-1),dim(areas1)[1])
  depart<-rep(substr(char,sep+1,nchar(char)),dim(areas1)[1])
  areas1$departement<-depart
  areas1$region<-reg
  #filtre les varibles utilies
  areas1<-as.data.frame(cbind(areas1$departement,areas1$region,areas1$laeq,areas1$geometry))
  names(areas1)<-c("departement","region","laeq","geometry")
  areas1$departement<-as.character(areas1$departement)
  areas1$region<-as.character(areas1$region)
  areas1$laeq<-as.numeric(as.character(areas1$laeq))
  areas<-as.data.frame(rbind(areas,areas1))
}
remove(areas1)
##Tracks
fichier<-list.files(path = "France",pattern = "*.tracks.geojson")
tracks<-st_read(paste("France/",fichier[1],sep = ""))
###recuper le nom departement et région
char<-Unaccent(substr(fichier[1],8,nchar(fichier[1])-15))
sep<-regexpr("_",substr(fichier[1],8,nchar(fichier[1])-15))[1]
reg<-rep(substr(char,1,sep-1),dim(tracks)[1])
depart<-rep(substr(char,sep+1,nchar(char)),dim(tracks)[1])
tracks$departement<-depart
tracks$region<-reg
###Filtrer les variable utiles
tracks<-as.data.frame(cbind(tracks$pk_track,tracks$tags,tracks$pleasantness,as.character(st_geometry_type(tracks$geometry)),tracks$departement,tracks$region))
names(tracks)<-c("pk_track","tags","pleasantness","type","departement","region")
tracks$pk_track<-as.character(tracks$pk_track)
tracks$pleasantness<-as.numeric(as.character((tracks$pleasantness)))
tracks$type<-as.character(tracks$type)
tracks$departement<-as.character(tracks$departement)
tracks$region<-as.character(tracks$region)
###Rassembler tous les jeux de données tracks
for(i in 2:length(fichier)){
  tracks1<-st_read(paste("France/",fichier[i],sep = ""))
  ###recuper le nom departement et région
  char<-Unaccent(substr(fichier[i],8,nchar(fichier[i])-15))
  sep<-regexpr("_",substr(fichier[i],8,nchar(fichier[i])-15))[1]
  reg<-rep(substr(char,1,sep-1),dim(tracks1)[1])
  depart<-rep(substr(char,sep+1,nchar(char)),dim(tracks1)[1])
  tracks1$departement<-depart
  tracks1$region<-reg
  ###Filtrer les variable utiles
  tracks1<-as.data.frame(cbind(tracks1$pk_track,tracks1$tags,tracks1$pleasantness,as.character(st_geometry_type(tracks1$geometry)),tracks1$departement,tracks1$region))
  names(tracks1)<-c("pk_track","tags","pleasantness","type","departement","region")
  tracks1$pk_track<-as.character(tracks1$pk_track)
  tracks1$pleasantness<-as.numeric(as.character(tracks1$pleasantness))
  tracks1$type<-as.character(tracks1$type)
  tracks1$departement<-as.character(tracks1$departement)
  tracks1$region<-as.character(tracks1$region)
  tracks<-as.data.frame(rbind(tracks,tracks1))
}
remove(tracks1)
##Points
fichier<-list.files(path = "France",pattern = "*.points.geojson")
points<-st_read(paste("France/",fichier[1],sep = ""))
###recuper le nom departement et région
char<-Unaccent(substr(fichier[1],8,nchar(fichier[1])-15))
sep<-regexpr("_",substr(fichier[1],8,nchar(fichier[1])-15))[1]
reg<-rep(substr(char,1,sep-1),dim(points)[1])
depart<-rep(substr(char,sep+1,nchar(char)),dim(points)[1])
points$departement<-depart
points$region<-reg
###recupération de la ville
france1 <- getData("GADM", country="France", level=5)
coordonne<-matrix(unlist(points$geometry),nrow = dim(points)[1],byrow = TRUE)
coordonne2<-as.data.frame(coordonne)
coordonne2$ville<-rep("",dim(coordonne2)[1])
france<-france1[france1$NAME_2==substr(substr(fichier[1],8,nchar(fichier[1])-15),sep+1,nchar(char)),]
meth<-as.data.frame(cbind(list(france@polygons[1][[1]]@Polygons[[1]]@coords[,1]),list(france@polygons[1][[1]]@Polygons[[1]]@coords[,2]),france@polygons[1][[1]]@ID))
meth<-as.data.frame(rbind(meth,cbind(list(france@polygons[2][[1]]@Polygons[[1]]@coords[,1]),list(france@polygons[2][[1]]@Polygons[[1]]@coords[,2]),france@polygons[2][[1]]@ID)))
for(k in 3:length(france)){
  meth<-as.data.frame(rbind(meth,cbind(list(france@polygons[k][[1]]@Polygons[[1]]@coords[,1]),list(france@polygons[k][[1]]@Polygons[[1]]@coords[,2]),france@polygons[k][[1]]@ID)))
}
r<-c()
pos<-c()
l<-list()
coordonne2$id<-seq(1:dim(coordonne2)[1])
for(k in 1:length(france)){
  if(length(point.in.polygon(coordonne2$V1,coordonne2$V2,meth$V1[[k]],meth$V2[[k]])[point.in.polygon(coordonne2$V1,coordonne2$V2,meth$V1[[k]],meth$V2[[k]])!=0])!=0){
    r<-c(k)
    pos<-c(coordonne2$id[point.in.polygon(coordonne2$V1,coordonne2$V2,meth$V1[[k]],meth$V2[[k]])==1])
    l<-c(l,list(r,pos))
  }
}
for (j in 1:(length(l)/2)){
  coordonne2$ville[l[[2*j]]]<-rep(l[[2*j-1]],length(l[[2*j]]))
}
coordonne2$ville2<-rep("",dim(coordonne2)[1])
for (j  in 1:dim(coordonne2)[1]) {
  if(coordonne2$ville[j]!=""){
    coordonne2$ville2[j]<-france@data$NAME_5[france@data$ID_5==as.character(coordonne2$ville[j])]
  }
}
points$ville<-coordonne2$ville2
#Filtrer les variables utiles
points<-as.data.frame(cbind(points$noise_level,points$departement,points$region,points$ville,points$pk_track,as.character(as.Date(points$time_ISO8601)),points$time_gps_epoch))
names(points)<-c("noise_level","departement","region","ville","pk_track","time_ISO8601","time_epoch")
points$noise_level<-as.numeric(as.character(points$noise_level))
points$departement<-as.character(points$departement)
points$region<-as.character(points$region)
points$ville<-as.character(points$ville)
points$pk_track<-as.character(points$pk_track)
points$time_epoch<-as.numeric(as.character(points$time_epoch))
points$time_ISO8601<-as.Date(points$time_ISO8601)
for(i in 2:length(fichier)){
  points1<-st_read(paste("France/",fichier[i],sep = ""))
  ###recuper le nom departement et région
  char<-Unaccent(substr(fichier[i],8,nchar(fichier[i])-15))
  sep<-regexpr("_",substr(fichier[i],8,nchar(fichier[i])-15))[1]
  reg<-rep(substr(char,1,sep-1),dim(points1)[1])
  depart<-rep(substr(char,sep+1,nchar(char)),dim(points1)[1])
  points1$departement<-depart
  points1$region<-reg
  ###recupération de la ville
  coordonne<-matrix(unlist(points1$geometry),nrow = dim(points1)[1],byrow = TRUE)
  coordonne2<-as.data.frame(coordonne)
  coordonne2$ville<-rep("",dim(coordonne2)[1])
  france<-france1[france1$NAME_2==substr(substr(fichier[i],8,nchar(fichier[i])-15),sep+1,nchar(char)),]
  meth<-as.data.frame(cbind(list(france@polygons[1][[1]]@Polygons[[1]]@coords[,1]),list(france@polygons[1][[1]]@Polygons[[1]]@coords[,2]),france@polygons[1][[1]]@ID))
  meth<-as.data.frame(rbind(meth,cbind(list(france@polygons[2][[1]]@Polygons[[1]]@coords[,1]),list(france@polygons[2][[1]]@Polygons[[1]]@coords[,2]),france@polygons[2][[1]]@ID)))
  for(k in 3:length(france)){
    meth<-as.data.frame(rbind(meth,cbind(list(france@polygons[k][[1]]@Polygons[[1]]@coords[,1]),list(france@polygons[k][[1]]@Polygons[[1]]@coords[,2]),france@polygons[k][[1]]@ID)))
  }
  r<-c()
  pos<-c()
  l<-list()
  coordonne2$id<-seq(1:dim(coordonne2)[1])
  for(k in 1:length(france)){
    if(length(point.in.polygon(coordonne2$V1,coordonne2$V2,meth$V1[[k]],meth$V2[[k]])[point.in.polygon(coordonne2$V1,coordonne2$V2,meth$V1[[k]],meth$V2[[k]])!=0])!=0){
      r<-c(unlist(meth$V3[k]))
      pos<-c(coordonne2$id[point.in.polygon(coordonne2$V1,coordonne2$V2,meth$V1[[k]],meth$V2[[k]])==1])
      l<-c(l,list(r,pos))
    }
  }
  for (j in 1:(length(l)/2)){
    coordonne2$ville[l[[2*j]]]<-rep(l[[2*j-1]],length(l[[2*j]]))
  }
  coordonne2$ville2<-rep("",dim(coordonne2)[1])
  for (j  in 1:dim(coordonne2)[1]) {
    if(coordonne2$ville[j]!=""){
      coordonne2$ville2[j]<-france@data$NAME_5[france@data$ID_5==as.character(coordonne2$ville[j])]
    }
  }
  points1$ville<-coordonne2$ville2
  #Filtrer les variables utiles
  points1<-as.data.frame(cbind(points1$noise_level,points1$departement,points1$region,points1$ville,points1$pk_track,as.character(as.Date(points1$time_ISO8601)),points1$time_gps_epoch))
  names(points1)<-c("noise_level","departement","region","ville","pk_track","time_ISO8601","time_epoch")
  points1$noise_level<-as.numeric(as.character(points1$noise_level))
  points1$departement<-as.character(points1$departement)
  points1$region<-as.character(points1$region)
  points1$ville<-as.character(points1$ville)
  points1$pk_track<-as.character(points1$pk_track)
  points1$time_epoch<-as.numeric(as.character(points1$time_epoch))
  points1$time_ISO8601<-as.Date(points1$time_ISO8601)
  points<-as.data.frame(rbind(points,points1))
}
remove(france)
remove(france1)
remove(points1)
remove(char)
remove(depart)
remove(sep)
remove(reg)
remove(i)
remove(j)
remove(k)
remove(r)
remove(pos)
remove(coordonne)
remove(coordonne2)
remove(l)
remove(meth)

#Importation de la variable data
ville<-read.table("villes.txt",header = FALSE,sep=",")
villes<-as.data.frame(cbind(ville$V2,ville$V5,ville$V19,ville$V20,ville$V21,ville$V15))
names(villes)<-c("Departement","Ville","Surface","Longitude","Latitude","Population")
nomville<-as.character(ville$V5)
depart<-as.character(ville$V2)
villes$Ville<-nomville
villes$Departement<-depart

datadepartement <- read_delim("departement.csv", ";", escape_double = FALSE, trim_ws = TRUE)
for(i in 1:dim(villes)[1]){
  villes$nomdepart[i]<-datadepartement$nom[datadepartement$departement==villes$Departement[i]]
}

villes$nomdepart[36700]<-toupper("st pierre et miquelon")

villecarte<-unique(points[points$ville!="",2:4])
bonneville<-villes[tolower(Unaccent(villes$Ville))%in%tolower(Unaccent(villecarte$ville)),]
bruitcarte=longcarte=latcartes=villecarte2= popcarte=popcarte2=dep=reg<-c()
for (i in 1:length(villecarte$ville)){
  longcarte[i]<-bonneville$Longitude[Unaccent(tolower(bonneville$Ville))==Unaccent(tolower(villecarte$ville[i])) & Unaccent(tolower(bonneville$nomdepart))==Unaccent(tolower(villecarte$departement[i]))]
  latcartes[i]<-bonneville$Latitude[Unaccent(tolower(bonneville$Ville))==Unaccent(tolower(villecarte$ville[i])) & Unaccent(tolower(bonneville$nomdepart))==Unaccent(tolower(villecarte$departement[i]))]
  popcarte[i]<-length(points$pk_track[points$ville==villecarte$ville[i] & points$departement==villecarte$departement[i]])
  bruitcarte[i]<-mean(points$noise_level[points$ville==villecarte$ville[i] & points$departement==villecarte$departement[i]])
  villecarte2[i]<-villecarte$ville[i]
  dep[i]<-villecarte$departement[i]
  reg[i]<-villecarte$region[i]
  popcarte2[i]<-length(points$region[points$ville==villecarte$ville[i] & points$departement==villecarte$departement[i]])/bonneville$Population[Unaccent(tolower(bonneville$Ville))==Unaccent(tolower(villecarte$ville[i])) & Unaccent(tolower(bonneville$nomdepart))==Unaccent(tolower(villecarte$departement[i]))]
}
data<-as.data.frame(cbind(villecarte2,longcarte,latcartes,bruitcarte,popcarte,reg,dep,popcarte2),stringsAsFactors = FALSE)
names(data)<-c("Ville","Longitude","Latitude","Bruits","Population","Region","Departement","Enregistrement/habiatnt")
data$Longitude<-as.numeric(data$Longitude)
data$Latitude<-as.numeric(data$Latitude)
data$Bruits<-as.numeric(data$Bruits)
data$Population<-as.numeric(data$Population)
data$`Enregistrement/habiatnt`<-as.numeric(data$`Enregistrement/habiatnt`)
remove(bonneville)
remove(datadepartement)
remove(ville)
remove(villes)
remove(villecarte)
remove(a)
remove(bruitcarte)
remove(dep)
remove(depart)
remove(latcartes)
remove(longcarte)
remove(nomville)
remove(popcarte)
remove(popcarte2)
remove(reg)

#Importation de la variable agecsp
agecsp <- read_delim("populations.csv", 
                     ";", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), 
                     trim_ws = TRUE)
agecsp$`Pourcentage(en%)`<-agecsp$`Pourcentage(en%)`*0.01

#Importation des variables ds et dr
regions<-c(unique(data$Region))
regions<-regions[regions!="Corse"]
regions<-regions[order(regions)]
mapdata <- get_data_from_map(download_map_data("countries/fr/custom/fr-all-mainland"))
mapdata$name[Unaccent(mapdata$`hc-a2`)=="AfA?D"]<-"Ile-de-France"
a<-mapdata[order(mapdata$name),]

a$name<-c(regions)

carte2<-mean(data$Bruits[data$Region==regions[1]])
code<-a$`hc-a2`[a$name==regions[1]]

for (i in 2:length(regions)){
  carte2<-c(carte2,mean(data$Bruits[data$Region==regions[i]]))
  code<-c(code,a$`hc-a2`[a$name==regions[i]])
}
code<-code[is.na(code)==FALSE]
carte21<-as.data.frame(cbind(carte2,code),stringsAsFactors = FALSE)
carte21$carte2<-as.numeric(carte21$carte2)
##donné regionale
dr<-as.data.frame(cbind(carte21,regions),stringsAsFactors = FALSE)
dr$regions<-as.character(dr$regions)
dr$carte2<-round(dr$carte2,3)
##donnée departementale
dp<-data[,c(4,6,7)]
dp<-dp %>% group_by(Region,Departement) %>% summarise(Bruits = mean(Bruits))
dp<-dp[dp$Region!="Corse",]
###code pour la carte
regions<-c(unique(dp$Region))
dp$code<-c("") 
dp$url<-c("")
add<-c()
y<-get_data_from_map(download_map_data("countries/fr/fr-all-all"))
position<-regexpr("-",substr(y$`hc-key`[i],4,nchar(y$`hc-key`[i])))[1]
ortho<-cbind(c("Provence-Alpes-Cote-d'Azur","Midi-Pyrenees","Rhone-Alpes","Ile-de-France","Franche-Comte","Reunion","Guyane francaise"),c("Provence-Alpes-CÃ´te-d'Azur","Midi-PyrÃ©nÃ©es","RhÃ´ne-Alpes","Ile-de-France","Franche-ComtÃ©","RÃ©union","Guyane francaise"))
ortho<-as.data.frame(ortho,stringsAsFactors = FALSE)
for(i in 1:length(ortho$V1)){
  y$region[y$region==ortho$V2[i] & is.na(y$region)==FALSE]<-ortho$V1[i]
}
y$region[Unaccent(y$fips)=="FRA8"]<-"Ile-de-France"
for (i in 1:length(regions)){
  bt<- y$`hc-key`[gsub("-"," ",y$region)==gsub("-"," ",regions[i])][1]
  position<-regexpr("-",substr(bt,4,nchar(bt)))[1]
  p<-substr(bt,4,4+position-2)
  add<-paste("countries/fr/fr-",p,"-all",sep = "")
  mapdata2 <- get_data_from_map(download_map_data(add))
  a2<-mapdata2[order(mapdata2$name),]
  dep<-unique(data$Departement[data$Region==regions[i]])
  a2$name<-dep
  dp$code[which(dp$Departement%in%a2$name)]<-a2$`hc-a2`[which(a2$name!="Paris")]
  dp$url[which(dp$Departement%in%a2$name)]<-add
}
dp$Bruits<-round(dp$Bruits,3)

#build drilldown series
build_series <- function(Regions) {
  
  #subset uscountygeojson
  frdepart.subset <- download_map_data(unique(dp$url[which(tolower(Regions)==tolower(dp$Region))]))
  
  ds.dp<-dp[which(tolower(Regions)==tolower(dp$Region)),]%>% 
    transmute(value = Bruits, code = code, cnlabel = Departement, stlabel = Region) %>%
    list_parse()
  
  ##constructioin series
  list(
    id = Regions,
    mapData = frdepart.subset,
    data = ds.dp,
    joinBy = c("hc-a2","code"),
    dataLabels = list(enabled = TRUE, format = '{point.name}'),
    tooltip = list(
      useHTML = TRUE,
      headerFormat = "<p>",
      pointFormat = paste0("<b>{point.stlabel}, {point.cnlabel}</b><br>",
                           "<b style=\"color:red\"> Bruit:</b><strong> {point.value} dB</strong><br>"
      ),
      footerFormat = "</p>")
    
  )
}


dt.rg <- dr %>% 
  transmute(value = carte2, code = code, stlabel = regions, drilldown = regions)
ds.rg <- list_parse(dt.rg)

##drilldown series
series.list <- lapply(dt.rg$drilldown, build_series)

remove(a)
remove(a2)
remove(carte21)
remove(ortho)
remove(y)
remove(add)
remove(bt)
remove(carte2)
remove(code)
remove(dep)
remove(fichier)
remove(p)
remove(i)
remove(position)
remove(regions)
remove(villecarte2)

#Remplacer l'ancien environnement par le nouveau
setwd(gsub("prétraitement","Tableau",getwd()))
file.remove("environnement1.RData")
save.image("environnement1.RData")
