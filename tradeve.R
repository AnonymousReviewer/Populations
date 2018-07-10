
setwd(paste0(Sys.getenv('CS_HOME'),'/Reviews/Cybergeo/submission3334/reproduction'))

library(dplyr)
library(ggplot2)
library(reshape2)

source('plots.R')
source('fitdistr.R')

# load data
agglo <- as.tbl(read.csv('data/agglo.csv',sep=';',dec = ','))

dates = c("1961","1971","1981","1991","2001","2011")
datesg = paste0(dates[1:(length(dates)-1)],"-",dates[2:length(dates)])

pops = agglo[,c(5,8:13)]

# remove missing values (fusions)
pops = pops[t(apply(as.matrix(pops[,2:ncol(pops)]),1,function(r){sum(as.numeric(is.na(r)))==0})),]
# -> only 20 removed

growthrates = t(apply(pops[,2:ncol(pops)],1,function(r){diff(r)/r[1:(length(r)-1)]}))

quantile(c(growthrates),0.99)
allg = data.frame(growthrate = c(growthrates),country = rep(pops$Country,ncol(growthrates)))

# distribution by country, on all times, without boundary agglos
g=ggplot(allg[allg$growthrate<quantile(allg$growthrate,0.99)&nchar(as.character(allg$country))==2,],aes(x=growthrate,fill=country,group=country))
g+geom_density(alpha=0.2)+xlab('Growth rate')+ylab('Density')+scale_fill_discrete(name='Country')+stdtheme
ggsave('res/growthrate_all.png',width=30,height=20,units = 'cm')


# not that interesting -> aggregate by areas and in time
# -> no clear geographical areas in the paper : do some clustering ?

# quantity of 'ouliers'
length(which(abs(allg$growthrate)>0.15))/length(allg$growthrate)
# 0.266633
# quantity of border agglo
length(which(nchar(as.character(allg$country))==2))/length(allg$country)
# -> 0.9919233

# growthrate in time
colnames(growthrates) = datesg
timeg = cbind(melt(growthrates,measure.vars=1:5,variable.name = "year",value.name="growthrate"),country = rep(pops$Country,ncol(growthrates)))

g=ggplot(timeg[timeg$growthrate<quantile(timeg$growthrate,0.99)&nchar(as.character(allg$country))==2,],aes(x=growthrate,fill=country,group=country))
g+geom_density(alpha=0.2)+facet_wrap(~Var2)+xlab('Growth rate')+ylab('Density')+scale_fill_discrete(name='Country')+stdtheme
ggsave('res/growthrate_time.png',width=30,height=20,units = 'cm')


# cluster agglos ? makes no sense, only one dimension (or use some spatial weighting to include spatial correlation)
# first test countries clustering, on all times

# remove annoying outliers
timeg = timeg[timeg$growthrate<quantile(timeg$growthrate,0.99)&nchar(as.character(allg$country))==2,]
# remove countries with too few agglos
agglocount = timeg %>% group_by(country) %>% summarise(count=n())
bigcountries = as.character(agglocount$country[agglocount$count>20])

timeg= timeg[timeg$country%in%bigcountries,]

bycountry = timeg %>% group_by(country)%>% summarize(avg=mean(growthrate),sd=sd(growthrate))
#step = 20

kvals=c();bwss = c();wss=c();steps=c()
for(step in seq(5,20,5)){
for(i in 1:step){
  h =  (timeg %>% group_by(country)%>% summarize(hist = hist(growthrate,breaks=step,plot = F)$density[i]))$hist
  h[which(is.na(h))]=0.0
  bycountry=cbind(bycountry,h)
}
colnames(bycountry)=c("country","avg","sd",paste0("h",1:step))

for(k in 2:10){
  clust = kmeans(bycountry[2:ncol(bycountry)],centers = k,nstart = 1000)
  #show(clust)
  kvals=append(k,kvals);bwss=append(bwss,clust$betweenss/clust$totss);wss=append(wss,clust$tot.withinss/clust$totss);steps=append(steps,step)
}
}

#plot(kvals,bwss,type='l',ylim=c(0,1));points(kvals,wss,col='red',type='l')
#plot(kvals[2:length(kvals)],diff(wss),type='l')
# rq : removing small countries changes the transition in cluster numbers

g=ggplot(data.frame(kvals,steps,bwss),aes(x=kvals,y=bwss,colour=steps,group=steps))
g+geom_line()+geom_point()
ggsave('res/clustering_vars.png',width=15,height=10)

# -> take k=6, step=10
k=6;step=6
bycountry = timeg %>% group_by(country)%>% summarize(avg=mean(growthrate),sd=sd(growthrate))
for(i in 1:step){
    h =  (timeg %>% group_by(country)%>% summarize(hist = hist(growthrate,breaks=step,plot = F)$density[i]))$hist
    h[which(is.na(h))]=0.0
    bycountry=cbind(bycountry,h)
}
colnames(bycountry)=c("country","avg","sd",paste0("h",1:step))
  
clust = kmeans(bycountry[2:ncol(bycountry)],centers = k,nstart = 1000)

bycountry$cluster = clust$cluster
cluster = clust$cluster;names(cluster)=bycountry$country

timeg$cluster = as.character(cluster[timeg$country])

g=ggplot(timeg[timeg$growthrate<quantile(timeg$growthrate,0.99)&!is.na(timeg$cluster),],aes(x=growthrate,fill=cluster,group=cluster))
g+geom_density(alpha=0.2)+facet_wrap(~Var2)+xlab('Growth rate')+ylab('Density')+scale_fill_discrete(name='Cluster\n(fixed)')+stdtheme
ggsave('res/growthrate_clusteredall_time.png',width=30,height=20,units = 'cm')


# same with clustering at each time step
k=6;step=6
clusters = c()
bwss=c();periods=c()
for(period in unique(timeg$Var2)){
  bycountry = timeg[timeg$Var2==period,] %>% group_by(country)%>% summarize(avg=mean(growthrate),sd=sd(growthrate))
  for(i in 1:step){
    h =  (timeg[timeg$Var2==period,] %>% group_by(country)%>% summarize(hist = hist(growthrate,breaks=step,plot = F)$density[i]))$hist
    h[which(is.na(h))]=0.0
    bycountry=cbind(bycountry,h)
  }
  colnames(bycountry)=c("country","avg","sd",paste0("h",1:step))
  
  clust = kmeans(bycountry[2:ncol(bycountry)],centers = k,nstart = 1000)
  
  bycountry$cluster = clust$cluster
  cluster = clust$cluster;names(cluster)=bycountry$country
  show(cluster)
  bwss=append(bwss,clust$betweenss/clust$totss);periods=append(periods,period)
  clusters=append(clusters,as.character(cluster[as.character(timeg$country[timeg$Var2==period])]))
}
timeg$cluster = clusters

g=ggplot(timeg[timeg$growthrate<quantile(timeg$growthrate,0.99)&!is.na(timeg$cluster),],aes(x=growthrate,fill=cluster,group=cluster))
g+geom_density(alpha=0.2)+facet_wrap(~Var2)+xlab('Growth rate')+ylab('Density')+scale_fill_discrete(name='Cluster\n(variable)')+stdtheme
ggsave('res/growthrate_clusteredvariable_time.png',width=30,height=20,units = 'cm')

# evolution of betweenss variance
g=ggplot(data.frame(periods,bwss),aes(x=periods,y=bwss))
g+geom_line()+geom_point()
ggsave('res/clusteringvariable_bwss.png',width=15,height=10,units='cm')


# fit statistical distributions -> evolution of fitted parameters
distrs=c();periods=c();mu=c();sigma=c()
for(period in unique(timeg$Var2)){
  currentdata=timeg[timeg$Var2==period,]
  for(cluster in 1:k){
    #show(length(currentdata$growthrate[currentdata$cluster==as.character(cluster)]))
    if(length(currentdata$growthrate[currentdata$cluster==as.character(cluster)])>20){
      #show(1 + currentdata$growthrate[currentdata$cluster==as.character(cluster)])
      #show(getDistribType(1 + currentdata$growthrate[currentdata$cluster==as.character(cluster)]))
      currentdistr = getDistribType(1 + currentdata$growthrate[currentdata$cluster==as.character(cluster)])
      if(currentdistr$name=="lnorm"){
        distrs=append(distrs,currentdistr$name);periods=append(periods,period)
        mu=append(mu,currentdistr$fit$estimate[1])
        sigma=append(sigma,currentdistr$fit$estimate[2])
      }
    }
  }  
}

table(data.frame(distrs,periods))

#periods
#distrs  1961-1971 1971-1981 1981-1991 1991-2001 2001-2011
#lnorm         3         5         4         2         2
#logis         0         0         0         3         1
#norm          0         0         0         1         1
#unif          1         0         0         0         0


g=ggplot(data.frame(periods,mu,sigma))
g+geom_point(aes(x=periods,y=mu),color=1)+geom_point(aes(x=periods,y=sigma),color=2)+ylab('mu - sigma')
ggsave('res/fittedlognormals.png',width=15,height=10,units='cm')











