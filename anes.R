library(ggplot2)
library(data.table)
library(dplyr)

anesdata.raw <- read.csv("anes/anes_timeseries_2016/anes_timeseries_2016_rawdata.txt", sep="|")
colnames(anesdata.raw)


#anes.labels<-read.csv(
#  "anes/anes_timeseries_2016/anes_timeseries_2016_spss/anes_timeseries_2016_varlabels_modified.sps",sep=" ")
anes.labels.inc<-read.csv("anes/anes_timeseries_2016/anes_labels.csv")
#write.csv(anes.labels,"anes/anes_timeseries_2016/anes_labels.csv")
anes.labels$LABELS[which(grepl("county",anes.labels$LABELS,ignore.case = TRUE))]
#now, we want to include items related to attitudes and opinions but not party affiliations and codes
#In order to do this we will also need location/county data so need to find out how to get that.

table(anesdata.raw$V161010f,anesdata.raw$V161010d)

anesdata.raw.colnames<-colnames(anesdata.raw)
#we have the district-level voting data.
rownames(anes.labels.inc)<-anes.labels.inc$VARIABLE
#table(anes.labels.inc$VARIABLE)
anes.labels.tolabel<-anes.labels.inc[anesdata.raw.colnames,]

#so next thing: how do we data-reduce?
anesdata.selected<-anesdata.raw[,which(anes.labels.tolabel$AttitudesBeliefInclude==1)]
#exclude cols with less than 4 options - not enough tdata.
unique_vals<-apply(anesdata.selected,2,function(c){length(unique(c))})
anesdata.selected<-anesdata.selected[,unique_vals>=4]

anes.prcomp<-prcomp(anesdata.selected,center=TRUE,scale. = TRUE)
#let's keep everything with SD of 2 or more.
plot(anes.prcomp,type="l")
summary(anes.prcomp)
comps.to.keep<-anes.prcomp$sdev>=2
dim(anes.prcomp$rotation)
rotate.mat<-anes.prcomp$rotation[,comps.to.keep]
rotate.mat.all<-anes.prcomp$rotation
#get the value of each component by subject
rd.ds<-anes.prcomp$x[,comps.to.keep]
#now we have to multiply
plot(rd.ds[,1],rd.ds[,2])
#now get the top 10 question positive loadings for each PC
anes.labels.tolabel[anes.labels.tolabel$VARIABLE %in% names(sort(rotate.mat[,1],decreasing=TRUE)[1:10]),]
anes.labels.tolabel[anes.labels.tolabel$VARIABLE %in% names(sort(rotate.mat[,2],decreasing=TRUE)[1:10]),]
anes.labels.tolabel[anes.labels.tolabel$VARIABLE %in% names(sort(rotate.mat[,3],decreasing=TRUE)[1:10]),]
presvote.code<-colnames(anesdata.raw)[which(anes.labels.tolabel$VoteInclude==1)]
anesdata.extraInfo<-anesdata.raw[,c(presvote.code,"V163001a","V163002")]
colnames(anesdata.extraInfo)<-c("PresidentVote","FIPSState","FIPSDistrict")

pca.affiliation.combined<-data.frame(cbind(rd.ds,anesdata.extraInfo))
pca.affiliation.combined$PresidentVote<-factor(pca.affiliation.combined$PresidentVote,
                                                  levels=c(6,7,8,9,5,4,3),
                                                  labels=c("Clinton","Trump","Johnson","Stein","(Inapplicable)","(NoInterview)","(NoPostData)"))

ggplot(pca.affiliation.combined,aes(PC1,PC2,color=PresidentVote))+geom_point(alpha=0.8)
ggplot(pca.affiliation.combined,aes(PC3,PC4,color=PresidentVote))+geom_point(alpha=0.8)
ggplot(pca.affiliation.combined,aes(PC5,PC6,color=PresidentVote))+geom_point(alpha=0.8)
ggplot(pca.affiliation.combined,aes(PC7,PC8,color=PresidentVote))+geom_point(alpha=0.8)
ggplot(pca.affiliation.combined,aes(PC9,PC10,color=PresidentVote))+geom_point(alpha=0.8)
ggplot(pca.affiliation.combined,aes(PC11,PC12,color=PresidentVote))+geom_point(alpha=0.8)
ggplot(pca.affiliation.combined,aes(PC13,PC14,color=PresidentVote))+geom_point(alpha=0.8)
ggplot(pca.affiliation.combined,aes(PC15,PC1,color=PresidentVote))+geom_point(alpha=0.8)
#remove PC1 scoing below 25 because it separates people who were not reachable
pca.affiliation.trimmed<-pca.affiliation.combined[pca.affiliation.combined$PC1 > -25,]
ggplot(pca.affiliation.trimmed,aes(PC1,PC2,color=PresidentVote))+geom_point(alpha=0.8)
ggplot(pca.affiliation.trimmed,aes(PC3,PC4,color=PresidentVote))+geom_point(alpha=0.8)

anes.labels.tolabel[anes.labels.tolabel$VARIABLE %in% names(sort(rotate.mat[,4],decreasing=TRUE)[1:10]),]

#PC4 separates alright, and measures preference for more liberal media.
ggplot(pca.affiliation.trimmed,aes(PC5,PC6,color=PresidentVote))+geom_point(alpha=0.8)

ggplot(pca.affiliation.trimmed,aes(PC1,PC2,color=PresidentVote))+geom_point(alpha=0.8)
ggplot(pca.affiliation.trimmed,aes(PC3,PC4,color=PresidentVote))+geom_point(alpha=0.8)
ggplot(pca.affiliation.trimmed,aes(PC5,PC6,color=PresidentVote))+geom_point(alpha=0.8)
ggplot(pca.affiliation.trimmed,aes(PC7,PC8,color=PresidentVote))+geom_point(alpha=0.8)
ggplot(pca.affiliation.trimmed,aes(PC9,PC10,color=PresidentVote))+geom_point(alpha=0.8)
ggplot(pca.affiliation.trimmed,aes(PC11,PC12,color=PresidentVote))+geom_point(alpha=0.8)
ggplot(pca.affiliation.trimmed,aes(PC13,PC14,color=PresidentVote))+geom_point(alpha=0.8)
ggplot(pca.affiliation.trimmed,aes(PC15,PC1,color=PresidentVote))+geom_point(alpha=0.8)

#predict using a linear model who was most likely to vote each way.
#just the clinton and trump voters.

pc.complete<-anes.prcomp$x

AllPCs<-data.frame(cbind(pc.complete,anesdata.extraInfo))#<-data.frame(cbind(pc.complete,"PresidentVote"=as.factor(anesdata.affiliationinfo)))
AllPCs$PresidentVote<-factor(AllPCs$PresidentVote,
                                               levels=c(1,2,3,4,-1,-6,-7),
                                               labels=c("Clinton","Trump","Johnson","Stein","(Inapplicable)","(NoInterview)","(NoPostData)"))
#table(AllPCs$PresidentVote)
AllPCs.ClintonTrump<-AllPCs[AllPCs$PresidentVote %in% c("Clinton","Trump"),]

AllPCs.ClintonTrump$PresidentVote<-as.character(AllPCs.ClintonTrump$PresidentVote)
AllPCs.ClintonTrump$PresidentVote<-factor(AllPCs.ClintonTrump$PresidentVote,levels=c("Trump","Clinton"))
table(AllPCs.ClintonTrump$PresidentVote)
#which PCs most strongly support voting for Trump?
pc.predictVote<-NULL

for (pc in 1:570){
 tres<-t.test(AllPCs.ClintonTrump[,pc]~AllPCs.ClintonTrump$PresidentVote)
 tresdf<-data.frame(tres$statistic,tres$conf.int[1],tres$conf.int[2],tres$p.value)
 if(is.null(pc.predictVote)){
   pc.predictVote<-tresdf
 }else{
   pc.predictVote<-rbind(pc.predictVote,tresdf)
 }
 
}
pc.predictVote.dt<-data.table(pc.predictVote)
pc.predictVote.dt$PC<-1:570
best.predicting.pcs<-pc.predictVote.dt[order(-abs(tres.statistic)),] %>% .[1:10,PC]

PCLabelsBestPredictors<-data.frame(Id=1:10)
for(i in 1:length(best.predicting.pcs)){
  pc<-best.predicting.pcs[i]
  PCLabelsBestPredictors[,i]<-anes.labels.tolabel[anes.labels.tolabel$VARIABLE %in% names(sort(rotate.mat.all[,pc],decreasing=TRUE)[1:10]),"LABELS"]
}

colnames(PCLabelsBestPredictors)<-paste0("PC",best.predicting.pcs)
bestpcs.graph<-tidyr::gather(AllPCs.ClintonTrump,"PC","Value",which(colnames(AllPCs.ClintonTrump) %in% colnames(PCLabelsBestPredictors)))
ggplot(bestpcs.graph,aes(x=PC,y=Value,color=PresidentVote,group=interaction(PresidentVote,PC)))+
  geom_boxplot(alpha=0)+
  geom_jitter(width=0.2,alpha=0.1)+
  coord_cartesian(ylim=c(-15,15))+labs(title="Distribution of Values on most predictive Principal Components",
                                       y="Principal Component Value")
#View(PCLabelsBestPredictors)
#take out 
pcpredict<-glm((PresidentVote=="Trump")~PC4+PC5+PC6+PC7+PC44+PC12+PC8+PC61+PC9+PC97,AllPCs.ClintonTrump,family = binomial(link="logit"))
pcpredict<-lm((PresidentVote=="Trump")~PC4+PC5+PC6+PC7+PC44+PC12+PC8+PC61+PC9+PC97,AllPCs.ClintonTrump)
summary(pcpredict)
ggplot(AllPCs.ClintonTrump,aes(PC4,PC5,color=PresidentVote))+geom_point(alpha=0.5)
ggplot(AllPCs.ClintonTrump,aes(PC6,PC7,color=PresidentVote))+geom_point(alpha=0.5)
ggplot(AllPCs.ClintonTrump,aes(PC44,PC97,color=PresidentVote))+geom_point(alpha=0.5)
#yes, together these separate pretty well.
#so now, can we test the hypothesis?

# 
# anesdata2012.raw <- read.csv("anes/anes_timeseries_2012/anes_timeseries_2012_rawdata.txt", sep="|")
# setdiff(colnames(anesdata2012.raw),colnames(anesdata.raw))
# setdiff(colnames(anesdata.raw),colnames(anesdata2012.raw))
# 
# anesdata2012.raw<-read.csv("anes/anes_timeseries_2012/anes_timeseries_2012_varlabels.sps", sep="|")
# anes.labels.2012<-read.csv("anes/anes_timeseries_2012/anes_timeseries_2012_varlabels.csv")
# #we gotta get these to approximately match up with the PCAs
# intersect(anes.labels.2012$X,anes.labels$LABELS)

View(anes.labels$LABELS)
library(usmap)
#now we need to map congressional districts to counties.
districts_to_counties<-read.csv("anes/natl_cocd_delim.txt")

colnames(districts_to_counties)<-paste0("FIPS",colnames(districts_to_counties))
#cor.test(anesdata.raw$V163002,anesdata.raw$V161010f)# these are both identical, that makes this a bit easier.
#table(anesdata.raw$V163001a)
#for mapped data, we'll have to do with duplicated values because want to assign the same thing to each county within the district.

dim(AllPCs.ClintonTrump[AllPCs.ClintonTrump$FIPSState==30,])

AllPCs.CT.matched<-data.table(merge(
  AllPCs.ClintonTrump,districts_to_counties,by.x=c("FIPSState","FIPSDistrict"),by.y=c("FIPSState","FIPSCongressional.District"),
  all.x = "TRUE"))

AllPCs.CT.matched$FIPS5DigitCounty<-paste0(formatC(AllPCs.CT.matched$FIPSState, width = 2, format = "d", flag = "0"),
                                           formatC(AllPCs.CT.matched$FIPSCounty, width = 3, format = "d", flag = "0")
                                           )
AllPCs.CT.matched<-merge(AllPCs.CT.matched,read.csv("anes/fips_states.csv"),by.x="FIPSState",by.y="FIPS.Code",all.x=TRUE)
#we haven't got some information for some counties so...
#(1) for districts unmapped to a county, map them to all counties not contained on the map.
# all.geocodes<-read.csv("anes/all-geocodes-v2016-county-fips.csv")
# 
# AllPCs.CT.matched[is.na(FIPSCounty),FIPS5DigitCounty:=FIPSState,]
# #(2) For counties with no data, we probably just leave them. Not sure it makes sense to populate them with any information.
# Let's do state maps.

# dim(AllPCs.CT.matched[AllPCs.CT.matched$FIPSState==30,])
# dim(AllPCs.ClintonTrump[AllPCs.ClintonTrump$FIPSState==30,])
# AllPCs.CT.matched.voteByDistrict<-
#   AllPCs.CT.matched[,.(VoteCount=.N),by=c("PresidentVote","FIPS5DigitCounty")] %>%
#   tidyr::spread(PresidentVote,VoteCount)
# 
# AllPCs.CT.matched.voteByDistrict$ProportionTrump<-
#   AllPCs.CT.matched.voteByDistrict$Trump/(AllPCs.CT.matched.voteByDistrict$Trump+AllPCs.CT.matched.voteByDistrict$Clinton)
# setnames(AllPCs.CT.matched.voteByDistrict,"FIPS5DigitCounty","fips")
#install.packages(c("maps", "mapdata"))
library(maps)
library(mapdata)

AllPCs.CT.matched.voteByState<-
   AllPCs.CT.matched[,.(VoteCount=.N,
                        PC4Mean=mean(PC4),
                        PC5Mean=mean(PC5),
                        PC6Mean=mean(PC6)
                        ),by=c("PresidentVote","State.or.District")] %>%
   tidyr::spread(PresidentVote,VoteCount)
AllPCs.CT.matched.voteByState$State.or.District<-tolower(AllPCs.CT.matched.voteByState$State.or.District)
AllPCs.CT.matched.voteByState$ProportionTrump<-
  AllPCs.CT.matched.voteByState$Trump/(AllPCs.CT.matched.voteByState$Trump+AllPCs.CT.matched.voteByState$Clinton)
AllPCs.CT.matched.voteByState[is.na(Clinton),ProportionTrump:=1]
AllPCs.CT.matched.voteByState[is.na(Trump),ProportionTrump:=0]
setnames(AllPCs.CT.matched.voteByState,"State.or.District","state")

#setnames(AllPCs.CT.matched.voteByState,"state","fips")
states <- map_data("state")
setnames(states,"order","polyorder")

#View(AllPCs.CT.matched.voteByState)
states_data<-data.table(merge(states,AllPCs.CT.matched.voteByState,by.x="region",by.y="state",all.x=TRUE))
states_data<-states_data[order(polyorder),]

ditch_the_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)

ggplot(data=states_data)+
  geom_polygon(aes(x = long, y = lat, fill = PC4Mean,group=group), color = "white") + 
   coord_fixed(1.7) +
  scale_fill_gradientn(colours=c("blue","#660088","red","red"))+
  ditch_the_axes
PCLabelsBestPredictors$PC4

ggplot(data=states_data)+
  geom_polygon(aes(x = long, y = lat, fill = PC5Mean,group=group), color = "white") + 
  coord_fixed(1.7) +
  scale_fill_gradientn(colours=c("blue","orange"))+
  ditch_the_axes
PCLabelsBestPredictors$PC5


ggplot(data=states_data)+
  geom_polygon(aes(x = long, y = lat, fill = PC6Mean,group=group), color = "white") + 
  coord_fixed(1.7) +
  scale_fill_gradientn(colours=c("blue","#660088","red","red","red"))+
  ditch_the_axes
PCLabelsBestPredictors$PC6
