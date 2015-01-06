library(sp)
library(rgeos)
library(maptools)
library(ggplot2)
library(ggmap)
library(rjson)



##################################################
# Exercice 0, échelle temporelle
##################################################
data=fromJSON(file="http://vlsstats.ifsttar.fr/data/spatiotemporalstats_London.json")
extract = function(x){data.frame( x$'_id', x$download_date,x$available_bikes )}
gg=do.call(rbind,lapply(data,extract))
names(gg)=c('id','time','nbbikes')
ggs =subset(gg,gg$id %in% c('383','610','701'))
ggplot(data=ggs,aes(x=time,y=nbbikes,group=id,color=factor(id)),size=2)+
  geom_line()+facet_grid(id ~ .)

# correction d l'échelle en temps
gg$timenew=as.POSIXct(gg$time,origin="1970-01-01")
ggs =subset(gg,gg$id %in% c('383','610','701'))
ggplot(data=ggs,aes(x=timenew,y=nbbikes,group=id,color=factor(id)),size=2)+
  geom_line()+facet_grid(id ~ .)
# changement sur l'échelle
library(scales)
ggplot(data=ggs,aes(x=timenew,y=nbbikes,group=id,color=factor(id)),size=2)+
  geom_line()+facet_grid(id ~ .)+scale_x_datetime(breaks=date_breaks("12 hours"),labels = date_format("%d/%m-%H h"),limits=c(min(ggs$timenew),max(ggs$timenew)))


##################################################
# Exercice 1, carte buble
##################################################
data=fromJSON(file="http://vlsstats.ifsttar.fr/data/input_Paris.json")
dd=sapply(data,function(x){c(x$number,x$available_bikes,x$position$lat,x$position$lng)})
dd=as.data.frame(t(dd))
names(dd)=c('id','bikes','lat','lng')
map=get_map("Paris",12,color="bw")
ggmap(map)+geom_point(data=dd,aes(x=lng,y=lat,size=bikes),color="purple")+scale_size_area()
# mise en forme du thème
mytheme= theme(axis.line=element_blank(),
               axis.text.x=element_blank(),
               axis.text.y=element_blank(),
               axis.ticks=element_blank(),
               axis.title.x=element_blank(),
               axis.title.y=element_blank(),
               plot.background=element_blank(),
               legend.justification=c(0,0), legend.position=c(0,0))
ggmap(map)+geom_point(data=dd,aes(x=lng,y=lat,size=bikes),color="purple")+scale_size_area("Vélos :")+mytheme


##################################################
# Exercice 2, carte choroplèthe
##################################################

# lecture des données INSEE
data=read.csv("./donnees_naissances_morts_communes.csv",header=T)
data$code_dept=as.character(data$code_dept)

nbnaiss=aggregate(data$nombre_naissances_2011,list(data$code_dept),sum)
names(nbnaiss)=c('dept','nbnaiss')
pop=aggregate(data$Population_2011,list(data$code_dept),sum)
names(pop)=c('dept','pop')
datadept=merge(nbnaiss,pop,by="dept")
datadept$txnatalite=datadept$nbnaiss/datadept$pop*1000

# selection des données d'ile de france

# lecture du fond de carte
dept=readShapeSpatial("./departements-20140306-50m-shp/departements-20140306-50m.shp",
                          proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

# mise en forme pour ggplot 
gg = fortify(dept,region="code_insee")
## si probleme avec rgeos
#gg = fortify(dept)
#gg=merge(gg,data.frame(id=0:100,idinsee=dept@data$code_insee),by="id")
#gg$id=gg$idinsee

ggdata=merge(gg,datadept,by.x="id",by.y="dept")
pe=order(ggdata$group,ggdata$order)
ggdata=ggdata[pe,]
# creation de la carte
ggplot(ggdata,aes(x=long,y=lat,group=group,fill=txnatalite))+geom_polygon()

# probleme ?
id1=unique(gg$id)
id2=unique(datadept$dept)
id1[!(id1 %in% id2)]

#correction des identifiant de datadept
datadept$dept[datadept$dept %in% 1:9]=paste("0",datadept$dept[datadept$dept %in% 1:9],sep="")
ggdata=merge(gg,datadept,by.x="id",by.y="dept")
pe=order(ggdata$group,ggdata$order)
ggdata=ggdata[pe,]
# creation de la carte
ggplot(ggdata,aes(x=long,y=lat,group=group,fill=txnatalite))+geom_polygon()

# amelioration de l'échelle de couleur
datadept$txnataliteQ=cut(datadept$txnatalite,quantile(datadept$txnatalite,na.rm=T),include.lowest=T)
ggdata=merge(gg,datadept,by.x="id",by.y="dept")
pe=order(ggdata$group,ggdata$order)
ggdata=ggdata[pe,]
# creation de la carte
ggplot(ggdata,aes(x=long,y=lat,group=group,fill=txnataliteQ))+geom_polygon()
# changement de l'échelle
ggplot(ggdata,aes(x=long,y=lat,group=group,fill=txnataliteQ))+geom_polygon()+scale_fill_brewer()
# projection meractor
ggplot(ggdata,aes(x=long,y=lat,group=group,fill=txnataliteQ))+geom_polygon()+scale_fill_brewer()+coord_map()
# mise en forme du thème
ggplot(ggdata,aes(x=long,y=lat,group=group,fill=txnataliteQ))+geom_polygon()+scale_fill_brewer("Taux de natalité")+coord_map()+mytheme

##################################################
# Exercice 3, hexagonal binning
##################################################

# recuperation des données
json = fromJSON(file="./export.geojson")
extractCenter = function(x){
  apply(matrix(unlist(x$geometry$coordinates),2),1,mean)
}
data = lapply(json$features, extractCenter)
data = data.frame(t(matrix(unlist(data),2)))
names(data)=c('long','lat')


map = get_map("Rennes",14)
ggmap(map)+geom_point(data=data,aes(x=long,y=lat))
ggmap(map)+stat_binhex(data=data,aes(x=long,y=lat))


##################################################
# Exercice 4, clustering
##################################################
json = fromJSON(file="http://vlsstats.ifsttar.fr/data/spatiotemporalstats_Paris.json")

#dates des mesures
dates=sort(unique(unlist(lapply(json,function(x){x$download_date}))))
dates=dates[-length(dates)]

# recuperation des données
dd=lapply(json,function(x){
  cdd=data.frame(date=x$download_date,bikes=x$available_bikes)
  cdd=merge(cdd,data.frame(date=dates),all.y=T)
  cdd$bikes})
data=do.call(rbind,dd)

# bormalisation / nbr de slots
nbslots=apply(data,1,function(x){max(x,na.rm=T)})
dataN=apply(data,2,function(c){c/nbslots})

# suppression des nan
nal=apply(dataN,1,function(l){sum(is.na(l))})
dataN=dataN[nal<2,]

nac=apply(dataN,2,function(c){sum(is.na(c))})
dataN=dataN[,nac<2]
dates=dates[nac<2]
sum(is.na(dataN))

#clustering
clust=kmeans(dataN,8)

# visualisation
# mise en forme des courbes
gg=data.frame(val=as.vector(dataN),time=rep(dates,each=dim(dataN)[1]),id=rep(1:dim(dataN)[1],dim(dataN)[2]),cl=factor(rep(clust$cluster,dim(dataN)[2]),1:8))
gg$timec=as.POSIXct(gg$time,origin="1970-01-01")

# visualisation des courbes
ggplot(data=gg,aes(x=timec,y=val,group=id,color=cl))+geom_path(alpha=0.1)+facet_grid( cl ~ .)

# mise en forme des moyennes
ggm=data.frame(val=as.vector(clust$centers),time=rep(dates,each=8),id=rep(1:8,dim(dataN)[2]),cl=factor(rep(1:8,dim(dataN)[2]),1:8))
ggm$timec=as.POSIXct(ggm$time,origin="1970-01-01")

# visualisation des courbes et des moyennes
ggplot()+geom_path(data=gg,aes(x=timec,y=val,group=id,color=cl),alpha=0.05)+
  geom_path(data=ggm,aes(x=timec,y=val,group=id,color=cl),size=2)+
  facet_grid( cl ~ .)+scale_x_datetime(breaks=date_breaks("1 days"))+
  scale_color_discrete(guide="none")+scale_y_continuous("Taux de remplissage")



