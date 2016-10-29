# This R environment comes with all of CRAN preinstalled, as well as many other helpful packages
# The environment is defined by the kaggle/rstats docker image: https://github.com/kaggle/docker-rstats
# For example, here's several helpful packages to load in 

library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(data.table)

super_fread_all <- function( file , key_var=NULL){
  dt <- fread(file)
  if(!is.null(key_var)) setkeyv(dt,c(key_var))
  return(dt)
}

super_fread <- function( file , key_var=NULL){
  dt <- fread(file)
  if(!is.null(key_var)) setkeyv(dt,c(key_var))
  return(dt)
}

print("training")
train  <- super_fread( "../input/clicks_train.csv")

displayGroup <- train[,.N,by=display_id]
head(displayGroup)
displayGroup$group <- seq.int(nrow(displayGroup))%%5
setkey(displayGroup,display_id)
setkey(train,display_id)
train<-train[displayGroup]
train<-train[group==1,]
train[clicked==0.0,clickedtoZero := -1/(N-1)]
train[clicked==1.0,clickedtoZero := 1-1/(N-1)]
head(train)

print("events")
events  <- super_fread_all( "../input/events.csv")
events[,time:=as.POSIXct((timestamp/1000),origin="2016-06-14 04:00:00 UTC")]
events[,hour:=as.integer(format(time,"%H "))]
events[,day:=format(time,"%u ")]
events[,state:=strtrim(geo_location,5)]
events[,country:=strtrim(geo_location,2)]
setkey(events, document_id) 
events[country!="US" & country!="CA",state:=country]
unique(events$state)
#stop("Event Finished")
print("document cat")

documents_categories  <- super_fread_all( "../input/documents_categories.csv")
documents_categories_lower <- documents_categories[documents_categories[, .I[which.min(confidence_level)], by=document_id]$V1]
documents_categories_upper <- documents_categories[documents_categories[, .I[which.max(confidence_level)], by=document_id]$V1]

setnames(documents_categories_lower, "confidence_level", "cat_confidence_level2")
setnames(documents_categories_lower, "category_id", "category_id2")
setnames(documents_categories_upper, "confidence_level", "cat_confidence_level1")
setkey(documents_categories_lower,document_id)
setkey(documents_categories_upper,document_id)
documents_categories <-documents_categories_upper[documents_categories_lower]
setkey(documents_categories,document_id)
events <- events[documents_categories,nomatch=0]

rm(documents_categories)
rm(documents_categories_lower)
rm(documents_categories_upper)

documents_entities  <- super_fread_all( "../input/documents_entities.csv")
documents_entities_lower <- documents_entities[documents_entities[, .I[which.min(confidence_level)], by=document_id]$V1]
documents_entities_upper <- documents_entities[documents_entities[, .I[which.max(confidence_level)], by=document_id]$V1]
head(documents_entities)
setnames(documents_entities_lower, "confidence_level", "entity_confidence_level2")
setnames(documents_entities_lower, "entity_id", "entity_id2")
setnames(documents_entities_upper, "confidence_level", "entity_confidence_level1")
setkey(documents_entities_lower,document_id)
setkey(documents_entities_upper,document_id)
documents_entities <-documents_entities_upper[documents_entities_lower]
documents_entities[entity_id==entity_id2,entity_confidence_level2:=0]
documents_entities[entity_id==entity_id2,entity_id2:=""]
setkey(documents_entities,document_id)
events <- events[documents_entities,nomatch=0]

rm(documents_entities)
rm(documents_entities_lower)
rm(documents_entities_upper)

documents_topic  <- super_fread_all( "../input/documents_topics.csv")
documents_topic_lower <- documents_topic[documents_topic[, .I[which.min(confidence_level)], by=document_id]$V1]
documents_topic_upper <- documents_topic[documents_topic[, .I[which.max(confidence_level)], by=document_id]$V1]

setnames(documents_topic_lower, "confidence_level", "topic_confidence_level2")
setnames(documents_topic_lower, "topic_id", "topic_id2")
setnames(documents_topic_upper, "confidence_level", "cat_confidence_level1")
setkey(documents_topic_lower,document_id)
setkey(documents_topic_upper,document_id)
documents_topic <-documents_topic_upper[documents_topic_lower]
setkey(documents_topic,document_id)
events <- events[documents_topic,nomatch=0]

rm(documents_topic)
rm(documents_topic_lower)
rm(documents_topic_upper)

documents_meta  <- super_fread_all( "../input/documents_meta.csv")

setkey(documents_meta,document_id)
events <- events[documents_meta,nomatch=0]

rm(documents_meta)

setkey(events,display_id)
setkey(train,display_id)
events <- events[train,nomatch=0]
clickthru <- events[,list(total=sum(clicked),score=mean(clickedtoZero),Summed=sum(clickedtoZero),sdScore=sd(clickedtoZero)),by=ad_id]
setkey(clickthru,ad_id)

clickthrubyState <- events[,list(total=sum(clicked),score=mean(clickedtoZero),Summed=sum(clickedtoZero)),by=.(ad_id,state)]
stateVariation <- clickthrubyState[,list(meanSt=mean(Summed),sdSt=sd(Summed)),by=ad_id]
setkey(stateVariation,ad_id)

clickthrubyTopic <- events[,list(total=sum(clicked),score=mean(clickedtoZero),Summed=sum(clickedtoZero)),by=.(ad_id,topic_id)]
topicVariation <- clickthrubyTopic[,list(meanTopic=mean(Summed),sdTopic=sd(Summed)),by=ad_id]
setkey(topicVariation,ad_id)

clickthrubycategory_id <- events[,list(total=sum(clicked),score=mean(clickedtoZero),Summed=sum(clickedtoZero)),by=.(ad_id,category_id)]
categoryVariation <- clickthrubycategory_id[,list(meanCat=mean(Summed),sdCat=sd(Summed)),by=ad_id]
setkey(categoryVariation,ad_id)

clickthru <- clickthru[stateVariation] 
clickthru <- clickthru[topicVariation] 
clickthru <- clickthru[categoryVariation] 

clickthru <- clickthru[order(score)]
clickthru$ID <- seq.int(nrow(clickthru))








p <- ggplot(data = clickthru, aes(ID, score)) + 
     geom_point(aes(x = ID, y = score), legend=  TRUE,  xlab="order", ylab="mean", colour=alpha('red', 0.05)) + 
     geom_point(aes(x = ID, y = sdScore), legend = TRUE, colour=alpha('blue', 0.05) ) + 
     geom_point(aes(x = ID, y = meanSt), legend=  TRUE,  xlab="order", ylab="mean", colour=alpha('green', 0.05)) + 
     geom_point(aes(x = ID, y = sdSt), legend = TRUE, colour=alpha('yellow', 0.05) ) + 
     geom_point(aes(x = ID, y = meanTopic), legend=  TRUE,  xlab="order", ylab="mean", colour=alpha('orange', 0.05)) + 
     geom_point(aes(x = ID, y = sdTopic), legend = TRUE, colour=alpha('pink', 0.05) )
ggsave("scatter.png", p, width=17, height=14, units="in")    
gc()

