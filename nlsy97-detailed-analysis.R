list.of.packages <- c("data.table","mice","ggplot2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(data.table)
library(mice)
library(ggplot2)
data.dir<-"nlsy97-detailed/"
source("nlsy97-detailed/nlsy97-detailed.R")

colnames(new_data)<-
  c("PubID",
    "PeersToChurch1997","PeersGetDrunk1997",
    "HeightFt1997","HeightInches1997","WeightPounds1997",
    "Sex","BdateMonth","BdateYear",
    "HHIncome1997","HHNetWorth1997AccToParent",
    "RelationshipToHHParentFigure","SampleType1997",
    "RaceEthnicity1997",
    "GirlsBehavioralProblems1997",
    "BoysBehavioralProblems1997",
    "ReligiousActivityPerWeek1998",
    "ParentsOwnIncome1997",
    "MathVerbal1999",
    "PersonalityConscientiousness2002","PersonalityAgreeableness2002","LifeRating2008",
    "PersonalityExtraversion2002","PersonalityAnxiousness2002","PersonalityOpenness2002",
    "PersonalityCalmness2002",
    "HeightFt2011","HeightInches2011",
    "FamilyIncome2015",
    "HighestDegreeReceived",
    "BiologicalChildrenInHousehold",
    "BiologicalChildrenOutsideHousehold",
    "WagesSalary2015",
    "WeightPounds2015",
    "SATMath2007","SATVerbal2007")


#save.image("imputedData.RData")
maindata_dt<-data.table(new_data)
# 
# #take a look at the variables.
# for (varname in colnames(maindata_dt)){
#   if (dim(maindata_dt[,.N,by=get(varname)])[1]<10){
#     print(ggplot(maindata_dt,aes_string(varname))+geom_bar()+labs(title=varname))
#   }else{
#     print(ggplot(maindata_dt,aes_string(varname))+geom_histogram()+labs(title=varname))
#   }
# }

#we need a better religion variable than this. Seems too skewed to quite high attendance
maindata_dt$ReligiousAttendanceWeekly<-maindata_dt$ReligiousActivityPerWeek1998>0
table(maindata_dt$ReligiousAttendanceWeekly)
#use the codebook to code these up.
maindata_dt$Sex<-factor(maindata_dt$Sex,levels=c(1,2,0),labels=c("Male","Female","NoInformation"),ordered=FALSE)

##HHParent
# 4395       1 Both biological parents
# 991       2 Two parents, biological mother
# 214       3 Two parents, biological father
# 2531       4 Biological mother only
# 295       5 Biological father only
# 103       6 Adoptive parent(s)
# 44       7 Foster parent(s)
# 197       8 No parents, grandparents
# 114       9 No parents, other relatives
# 69      10 Anything else
  
maindata_dt[RelationshipToHHParentFigure %in% c(2,3),RelationshipToHHParentFigure:=2] #"Two parents, one biological"
maindata_dt[RelationshipToHHParentFigure %in% c(4,5),RelationshipToHHParentFigure:=3] #"One biological parent only"
maindata_dt[RelationshipToHHParentFigure %in% c(6,7),RelationshipToHHParentFigure:=4] #"Adoption or Foster"
maindata_dt[RelationshipToHHParentFigure %in% c(8,9,10),RelationshipToHHParentFigure:=5] #"Other"
maindata_dt$RelationshipToHHParentFigure<-factor(maindata_dt$RelationshipToHHParentFigure,
                                                 levels=c(1,2,3,4,5),
                                                 labels=c("Two biological parents",
                                                   "Two parents, one biological",
                                                          "One biological parent only",
                                                          "Adoption or Foster",
                                                          "Other"),ordered = FALSE)


#height
maindata_dt$HeightFtIn1997<-maindata_dt$HeightFt1997+maindata_dt$HeightInches1997/12
maindata_dt$HeightFtIn2011<-maindata_dt$HeightFt2011+maindata_dt$HeightInches2011/12
#BMI
maindata_dt$BMI1997<-703*maindata_dt$WeightPounds1997/(maindata_dt$HeightFtIn1997*12)^2
maindata_dt$BMIAdult<-703*maindata_dt$WeightPounds2015/(maindata_dt$HeightFtIn2011*12)^2

#BehavioralProblems
maindata_dt[Sex=="Male",BehavioralProblems1997:=BoysBehavioralProblems1997]
maindata_dt[Sex=="Female",BehavioralProblems1997:=GirlsBehavioralProblems1997]

#Children
maindata_dt$BiologicalChildren<-0
maindata_dt[!is.na(BiologicalChildrenInHousehold),BiologicalChildren:=BiologicalChildren+BiologicalChildrenInHousehold]
maindata_dt[!is.na(BiologicalChildrenOutsideHousehold),BiologicalChildren:=BiologicalChildren+BiologicalChildrenOutsideHousehold]


#now apply multiple imputation.
#must make sure no columns are linear combinations of other included columns.
maindata_dt.trimmed<-maindata_dt[,c("PeersToChurch1997","PeersGetDrunk1997",
              "BMI1997",
              "Sex",
              "HHIncome1997","HHNetWorth1997AccToParent",
              "RelationshipToHHParentFigure",
              "RaceEthnicity1997",
              "BehavioralProblems1997",
              "ReligiousAttendanceWeekly",
              "MathVerbal1999",
              "PersonalityConscientiousness2002","PersonalityAgreeableness2002","LifeRating2008",
              "PersonalityExtraversion2002","PersonalityAnxiousness2002","PersonalityOpenness2002",
              "PersonalityCalmness2002",
              "BMIAdult",
              "FamilyIncome2015",
              "HighestDegreeReceived",
              "BiologicalChildren",
              "WagesSalary2015",
              "SATMath2007","SATVerbal2007"),with=FALSE]

maindata_dt.nm <- mice(maindata_dt.trimmed, m=5, maxit = 100, method = 'pmm', seed = 42)



load(file="multiple-imputed.RData")
#save.image("multiple-imputed.RData")
maindata_dt.mu<-complete(maindata_dt.nm)
maindata_dt.mu<-data.table(maindata_dt.mu)

#Race
maindata_dt.mu$REIsBlack<-maindata_dt.mu$RaceEthnicity1997=="Black"
maindata_dt.mu$REIsHispanic<-maindata_dt.mu$RaceEthnicity1997=="Hispanic"
maindata_dt.mu$REIsMixedNonHispanic<-maindata_dt.mu$RaceEthnicity1997=="Mixed Race (Non-Hispanic)"

#SAT

maindata_dt.mu[,SAT2007:=rowMeans(maindata_dt.mu[,.(SATMath2007,SATVerbal2007)])]


#Display the cleaned variables we plan to model.
for (varname in   c("PeersToChurch1997","PeersGetDrunk1997",
                    "BMI1997",
                    "Sex",
                    "HHIncome1997","HHNetWorth1997AccToParent",
                    "RelationshipToHHParentFigure",
                    "RaceEthnicity1997",
                    "BehavioralProblems1997",
                    "ReligiousAttendanceWeekly",
                    "MathVerbal1999",
                    "PersonalityConscientiousness2002","PersonalityAgreeableness2002","LifeRating2008",
                    "PersonalityExtraversion2002","PersonalityAnxiousness2002","PersonalityOpenness2002",
                    "PersonalityCalmness2002",
                    "BMIAdult",
                    "FamilyIncome2015",
                    "HighestDegreeReceived",
                    "BiologicalChildren",
                    "WagesSalary2015",
                    "SATMath2007","SATVerbal2007","SAT2007")){
  if (dim(maindata_dt.mu[,.N,by=get(varname)])[1]<10){
    print(ggplot(maindata_dt.mu,aes_string(varname))+geom_bar()+labs(title=varname))
  }else{
    print(ggplot(maindata_dt.mu,aes_string(varname))+geom_histogram()+labs(title=varname))
  }
}
maindata_dt.mu<-maindata_dt.mu[FamilyIncome2015<max(FamilyIncome2015)]
maindata_dt.mu<-maindata_dt.mu[HHNetWorth1997AccToParent<max(HHNetWorth1997AccToParent)]
maindata_dt.mu<-maindata_dt.mu[HHIncome1997<max(HHIncome1997)]
maindata_dt.mu<-maindata_dt.mu[WagesSalary2015<max(WagesSalary2015)]

#let's center key variables where it makes sense, which is for most of them.
maindata_dt.mu[,PeersToChurch1997.C:=scale(PeersToChurch1997,scale=FALSE)]
maindata_dt.mu[,PeersGetDrunk1997.C:=scale(PeersGetDrunk1997,scale=FALSE)]
maindata_dt.mu[,BMI1997.C:=scale(BMI1997,scale=FALSE)]
maindata_dt.mu[,HHIncome1997.C:=scale(HHIncome1997,scale=FALSE)]
maindata_dt.mu[,HHNetWorth1997AccToParent.C:=scale(HHNetWorth1997AccToParent,scale=FALSE)]
maindata_dt.mu[,BehavioralProblems1997.C:=scale(BehavioralProblems1997,scale=FALSE)]
maindata_dt.mu[,MathVerbal1999.C:=scale(MathVerbal1999,scale=FALSE)]
            
maindata_dt.mu[,LifeRating2008.C:=scale(LifeRating2008,scale=FALSE)]

maindata_dt.mu[,PersonalityConscientiousness2002.C:=scale(PersonalityConscientiousness2002,scale=FALSE)]
maindata_dt.mu[,PersonalityAgreeableness2002.C:=scale(PersonalityAgreeableness2002,scale=FALSE)]

maindata_dt.mu[,PersonalityExtraversion2002.C:=scale(PersonalityExtraversion2002,scale=FALSE)]
maindata_dt.mu[,PersonalityAnxiousness2002.C:=scale(PersonalityAnxiousness2002,scale=FALSE)]
maindata_dt.mu[,PersonalityOpenness2002.C:=scale(PersonalityOpenness2002,scale=FALSE)]
maindata_dt.mu[,PersonalityCalmness2002.C:=scale(PersonalityCalmness2002,scale=FALSE)]


maindata_dt.mu[,BMIAdult.C:=scale(BMIAdult,scale=FALSE)]
maindata_dt.mu[,FamilyIncome2015.C:=scale(FamilyIncome2015,scale=FALSE)]
maindata_dt.mu[,WagesSalary2015.C:=scale(WagesSalary2015,scale=FALSE)]
maindata_dt.mu[,SATMath2007.C:=scale(SATMath2007,scale=FALSE)]
maindata_dt.mu[,SATVerbal2007.C:=scale(SATVerbal2007,scale=FALSE)]
maindata_dt.mu[,SAT2007.C:=scale(SAT2007,scale=FALSE)]

#remove income values that are at the maximum point (they're hitting a ceiling)

#need to do this.

library(mice)

#let's start with income
options(scipen=10)
summary(lm(FamilyIncome2015.C~
             HHIncome1997.C+HHNetWorth1997AccToParent.C
             ,maindata_dt.mu))
summary(lm(WagesSalary2015.C~
             HHIncome1997.C+HHNetWorth1997AccToParent.C
           ,maindata_dt.mu))
summary(lm(LifeRating2008.C~
             HHIncome1997.C+HHNetWorth1997AccToParent.C
           ,maindata_dt.mu)) #as before, Household Wealth in 1997 predicts wellbeing in 2008 but Household Income does not.
#add demographics
summary(lm(FamilyIncome2015.C~
             HHIncome1997.C+HHNetWorth1997AccToParent.C+
             Sex+REIsBlack+REIsHispanic+REIsMixedNonHispanic
           ,maindata_dt.mu))

summary(lm(WagesSalary2015.C~
             HHIncome1997.C+HHNetWorth1997AccToParent.C+
             Sex+REIsBlack+REIsHispanic+REIsMixedNonHispanic
           ,maindata_dt.mu))

summary(lm(LifeRating2008.C~
             HHIncome1997.C+HHNetWorth1997AccToParent.C+
             Sex+REIsBlack+REIsHispanic+REIsMixedNonHispanic
           ,maindata_dt.mu))
#as before: Minority race and sex (female) negatively predict personal wages and salary
#Minority race but not sex (female) negatively predict family income
#Hispanic and Female predicts positive well-being while Black predicts negative wellbeing.
#Income predicts income and does not predict wellbeing.
#Net worth doesn't substantially predict income. 
#For every $100,000 of parental net worth in 1997, respondent income in 2015 is just $0.08 higher.
0.0000008440*10^5

summary(lm(FamilyIncome2015.C~
             HHIncome1997.C+HHNetWorth1997AccToParent.C+
             Sex+REIsBlack+REIsHispanic+REIsMixedNonHispanic+ReligiousAttendanceWeekly
           ,maindata_dt.mu))

summary(lm(WagesSalary2015.C~
             HHIncome1997.C+HHNetWorth1997AccToParent.C+
             Sex+REIsBlack+REIsHispanic+REIsMixedNonHispanic+ReligiousAttendanceWeekly
           ,maindata_dt.mu))

summary(lm(LifeRating2008.C~
             HHIncome1997.C+HHNetWorth1997AccToParent.C+
             REIsBlack+REIsHispanic+REIsMixedNonHispanic+ReligiousAttendanceWeekly
           ,maindata_dt.mu))
#Weekly religious attendance as a child predicts an extra 0.3 of wellbeing and predicts $5200 of additional yearly income 
#Sex definitely makes no difference to life rating.

#next tranch: does biological children explain the sex difference?
summary(lm(FamilyIncome2015.C~
             HHIncome1997.C+HHNetWorth1997AccToParent.C+
             Sex*BiologicalChildren+REIsBlack+REIsHispanic+REIsMixedNonHispanic+ReligiousAttendanceWeekly
           ,maindata_dt.mu))

summary(lm(WagesSalary2015.C~
             HHIncome1997.C+HHNetWorth1997AccToParent.C+
             Sex*BiologicalChildren+REIsBlack+REIsHispanic+REIsMixedNonHispanic+ReligiousAttendanceWeekly
           ,maindata_dt.mu))

summary(lm(LifeRating2008.C~
             HHIncome1997.C+HHNetWorth1997AccToParent.C+
             Sex*BiologicalChildren+REIsBlack+REIsHispanic+REIsMixedNonHispanic+ReligiousAttendanceWeekly
           ,maindata_dt.mu))
#Yes, it appears to. Specifically, controlling for having children, women are in households making $9800 more than men, but for each child,
#women lose this advantage $6200, so that a women with 2 children is predicted be in a household making about $3000 less than a man with two children.
#I think probably we want to see the breakdown here to draw any conclusions, but it's definitely complicated.
#we see the same relationship will well-being, with no children, women are predicted to have slightly higher well-being than men,
#but the gap reverses with children.

#psychology and aptitude.
summary(lm(FamilyIncome2015.C~
             HHIncome1997.C+HHNetWorth1997AccToParent.C+
             Sex*BiologicalChildren+REIsBlack+REIsHispanic+REIsMixedNonHispanic+ReligiousAttendanceWeekly+
             MathVerbal1999+PersonalityConscientiousness2002+PersonalityAgreeableness2002+
             PersonalityExtraversion2002+PersonalityAnxiousness2002+PersonalityOpenness2002+
             +PersonalityCalmness2002
           ,maindata_dt.mu))
summary(lm(WagesSalary2015.C~
             HHIncome1997.C+HHNetWorth1997AccToParent.C+
             Sex*BiologicalChildren+REIsBlack+REIsHispanic+REIsMixedNonHispanic+ReligiousAttendanceWeekly+
             MathVerbal1999+PersonalityConscientiousness2002+PersonalityAgreeableness2002+
             PersonalityExtraversion2002+PersonalityAnxiousness2002+PersonalityOpenness2002+
             +PersonalityCalmness2002
           ,maindata_dt.mu))
summary(lm(LifeRating2008.C~
             HHIncome1997.C+HHNetWorth1997AccToParent.C+
             Sex*BiologicalChildren+REIsBlack+REIsHispanic+REIsMixedNonHispanic+ReligiousAttendanceWeekly+
             MathVerbal1999+PersonalityConscientiousness2002+PersonalityAgreeableness2002+
           PersonalityExtraversion2002+PersonalityAnxiousness2002+PersonalityOpenness2002+
             +PersonalityCalmness2002
           ,maindata_dt.mu))
#we can prune these to just the important values.
summary(lm(FamilyIncome2015.C~
             HHIncome1997.C+HHNetWorth1997AccToParent.C+
             Sex*BiologicalChildren+REIsBlack+REIsHispanic+REIsMixedNonHispanic+ReligiousAttendanceWeekly+
             MathVerbal1999+PersonalityConscientiousness2002+
             PersonalityExtraversion2002+PersonalityAnxiousness2002+PersonalityOpenness2002+PersonalityCalmness2002
           ,maindata_dt.mu))
summary(lm(WagesSalary2015.C~
             HHIncome1997.C+HHNetWorth1997AccToParent.C+
             Sex*BiologicalChildren+REIsBlack+REIsHispanic+REIsMixedNonHispanic+ReligiousAttendanceWeekly+
             MathVerbal1999+PersonalityAgreeableness2002+
             PersonalityExtraversion2002+PersonalityAnxiousness2002+PersonalityOpenness2002
           ,maindata_dt.mu))
summary(lm(LifeRating2008.C~
             HHNetWorth1997AccToParent.C+
             Sex*BiologicalChildren+REIsBlack+REIsHispanic+REIsMixedNonHispanic+ReligiousAttendanceWeekly+
             PersonalityAgreeableness2002+
             PersonalityExtraversion2002+PersonalityAnxiousness2002+PersonalityOpenness2002+
             +PersonalityCalmness2002
           ,maindata_dt.mu))
#Conscientiousness predicts FamilyIncome, not WagesSalary. Is that because it predicts getting into a double-income family?

#Better. What about weight and peers and education?
#we're predicting BMI.
summary(lm(FamilyIncome2015.C~
             HHIncome1997.C+HHNetWorth1997AccToParent.C+
             Sex*BiologicalChildren+REIsBlack+REIsHispanic+REIsMixedNonHispanic+ReligiousAttendanceWeekly+
             MathVerbal1999+PersonalityConscientiousness2002+
             PersonalityExtraversion2002+PersonalityAnxiousness2002+PersonalityOpenness2002+PersonalityCalmness2002+
             BMI1997+PersonalityCalmness2002+HighestDegreeReceived
           ,maindata_dt.mu))
summary(lm(WagesSalary2015.C~
             HHIncome1997.C+HHNetWorth1997AccToParent.C+
             Sex*BiologicalChildren+REIsBlack+REIsHispanic+REIsMixedNonHispanic+ReligiousAttendanceWeekly+
             MathVerbal1999+PersonalityAgreeableness2002+
             PersonalityExtraversion2002+PersonalityAnxiousness2002+PersonalityOpenness2002+
             BMI1997+PersonalityCalmness2002+HighestDegreeReceived
           ,maindata_dt.mu))
summary(lm(LifeRating2008.C~
             HHNetWorth1997AccToParent.C+
             Sex*BiologicalChildren+REIsBlack+REIsHispanic+REIsMixedNonHispanic+ReligiousAttendanceWeekly+
             PersonalityAgreeableness2002+
             PersonalityExtraversion2002+PersonalityAnxiousness2002+PersonalityOpenness2002+
             +PersonalityCalmness2002+
             BMI1997+PersonalityCalmness2002+HighestDegreeReceived
           ,maindata_dt.mu))
#BMI isn't really a predictor
summary(lm(BMIAdult~
             HHIncome1997.C+HHNetWorth1997AccToParent.C+
             Sex*BiologicalChildren+REIsBlack+REIsHispanic+REIsMixedNonHispanic+ReligiousAttendanceWeekly+
             MathVerbal1999+PersonalityConscientiousness2002+
             PersonalityExtraversion2002+PersonalityAnxiousness2002+PersonalityOpenness2002+PersonalityCalmness2002+
             BMI1997+PersonalityCalmness2002+HighestDegreeReceived
           ,maindata_dt.mu))

#some of our effects disappear when controlling for education. 
#Hispanics no longer have a disadvantage and personality values diminish, suggesting that personality contrivutes through education and related factors.
#We get 31% explanatory power on BMI, but most of the efect is through childhood BMI.
#Income is not important. Women, blacks have higher Adult BMIs than would be predicted by teenage BMIs and people with more education have lower BMIs.


#We still have to do:
summary(lm(FamilyIncome2015.C~
             HHIncome1997.C+HHNetWorth1997AccToParent.C+
             Sex*BiologicalChildren+REIsBlack+REIsHispanic+REIsMixedNonHispanic+ReligiousAttendanceWeekly+
             MathVerbal1999+PersonalityConscientiousness2002+
             PersonalityExtraversion2002+PersonalityAnxiousness2002+PersonalityOpenness2002+PersonalityCalmness2002+
             BMI1997+PersonalityCalmness2002+HighestDegreeReceived+
             PeersToChurch1997+PeersGetDrunk1997
           ,maindata_dt.mu))
summary(lm(WagesSalary2015.C~
             HHIncome1997.C+HHNetWorth1997AccToParent.C+
             Sex*BiologicalChildren+REIsBlack+REIsHispanic+REIsMixedNonHispanic+ReligiousAttendanceWeekly+
             MathVerbal1999+PersonalityAgreeableness2002+
             PersonalityExtraversion2002+PersonalityAnxiousness2002+PersonalityOpenness2002+
             BMI1997+PersonalityCalmness2002+HighestDegreeReceived+
             PeersToChurch1997+PeersGetDrunk1997
           ,maindata_dt.mu))
summary(lm(LifeRating2008.C~
             HHNetWorth1997AccToParent.C+
             Sex*BiologicalChildren+REIsBlack+REIsHispanic+REIsMixedNonHispanic+ReligiousAttendanceWeekly+
             PersonalityAgreeableness2002+
             PersonalityExtraversion2002+PersonalityAnxiousness2002+PersonalityOpenness2002+
             +PersonalityCalmness2002+
             BMI1997+PersonalityCalmness2002+HighestDegreeReceived+
             PeersToChurch1997+PeersGetDrunk1997
           ,maindata_dt.mu))
summary(lm(BMIAdult~
             HHIncome1997.C+HHNetWorth1997AccToParent.C+
             Sex*BiologicalChildren+REIsBlack+REIsHispanic+REIsMixedNonHispanic+ReligiousAttendanceWeekly+
             MathVerbal1999+PersonalityConscientiousness2002+
             PersonalityExtraversion2002+PersonalityAnxiousness2002+PersonalityOpenness2002+PersonalityCalmness2002+
             BMI1997+PersonalityCalmness2002+HighestDegreeReceived+
             PeersToChurch1997+PeersGetDrunk1997
           ,maindata_dt.mu))


#And what about LifeRating as a *predictor* over and above childhood factors?
liferating.mediator<-lm(FamilyIncome2015.C~
             HHIncome1997.C+HHNetWorth1997AccToParent.C+
             Sex*BiologicalChildren+REIsBlack+REIsHispanic+REIsMixedNonHispanic+ReligiousAttendanceWeekly+
             MathVerbal1999+PersonalityConscientiousness2002+
             PersonalityExtraversion2002+PersonalityAnxiousness2002+PersonalityOpenness2002+PersonalityCalmness2002+
             BMI1997+PersonalityCalmness2002+HighestDegreeReceived+
             PeersToChurch1997+PeersGetDrunk1997+LifeRating2008.C
           ,maindata_dt.mu)

noliferating<-lm(FamilyIncome2015.C~
             HHIncome1997.C+HHNetWorth1997AccToParent.C+
             Sex*BiologicalChildren+REIsBlack+REIsHispanic+REIsMixedNonHispanic+ReligiousAttendanceWeekly+
             MathVerbal1999+PersonalityConscientiousness2002+
             PersonalityExtraversion2002+PersonalityAnxiousness2002+PersonalityOpenness2002+PersonalityCalmness2002+
             BMI1997+PersonalityCalmness2002+HighestDegreeReceived+
             PeersToChurch1997+PeersGetDrunk1997
           ,maindata_dt.mu)
anova(noliferating,liferating.mediator)
beta.compare<-data.frame(cbind(liferating.mediator$coefficients[names(liferating.mediator$coefficients)!="LifeRating2008.C"],noliferating$coefficients))
beta.compare$Ratio<-beta.compare[,1]/beta.compare[,2]
View(beta.compare)

#With a linear model we can predict about 22% of variance in wages and salary and slightly less for incomes. Life rating is a bit harder to predict.
summary(lm(FamilyIncome2015.C~
             HHIncome1997.C+HHNetWorth1997AccToParent.C+
             Sex*BiologicalChildren+REIsBlack+REIsHispanic+REIsMixedNonHispanic+ReligiousAttendanceWeekly+
             MathVerbal1999+PersonalityConscientiousness2002+
             PersonalityExtraversion2002+HighestDegreeReceived
           ,maindata_dt.mu))
m.norm.a<-lm(FamilyIncome2015.C~
                       HHIncome1997.C+HHNetWorth1997AccToParent.C+
                       Sex*BiologicalChildren+REIsBlack+REIsHispanic+REIsMixedNonHispanic+ReligiousAttendanceWeekly+
                       MathVerbal1999+PersonalityConscientiousness2002+
                       PersonalityExtraversion2002+HighestDegreeReceived
                     ,maindata_dt.mu)

maindata_dt.mu$WagesSalary2015P1<-maindata_dt.mu$WagesSalary2015+1
m.norm<-lm(WagesSalary2015P1~
             HHIncome1997.C+HHNetWorth1997AccToParent.C+
             Sex*BiologicalChildren+REIsBlack+REIsHispanic+REIsMixedNonHispanic+ReligiousAttendanceWeekly+
             MathVerbal1999+PersonalityAgreeableness2002.C+
             PersonalityExtraversion2002.C+HighestDegreeReceived
           ,maindata_dt.mu)
summary(m.norm)
m.Gamma<-glm(WagesSalary2015P1~
             HHIncome1997.C+HHNetWorth1997AccToParent.C+
             Sex*BiologicalChildren+REIsBlack+REIsHispanic+REIsMixedNonHispanic+ReligiousAttendanceWeekly+
             MathVerbal1999+PersonalityAgreeableness2002.C+
             PersonalityExtraversion2002.C+HighestDegreeReceived
           ,maindata_dt.mu,family=Gamma)
summary(m.Gamma)
summary(m.Gamma)
#This didn't work.
afit<-anova(m.norm,m.Gamma)
summary(afit)
#What about parental attitudes? Can we get data on that?
library(minerva)

results_table<-NULL
for (IV in c("HHIncome1997.C","HHNetWorth1997AccToParent.C",
             "MathVerbal1999","PersonalityConscientiousness2002.C","PersonalityAgreeableness2002.C",
             "PersonalityExtraversion2002.C","PersonalityAnxiousness2002.C","PersonalityOpenness2002.C","PersonalityCalmness2002.C")){
  for (DV in c("FamilyIncome2015.C","WagesSalary2015.C","LifeRating2008.C")){
    resrow<-mine(as.double(maindata_dt.mu[,get(IV)]),as.double(maindata_dt.mu[,get(DV)]),use="pairwise.complete.obs")
    resrow$IV<-IV
    resrow$DV<-DV
    resrow<-data.frame(resrow)
    if(is.null(results_table)){
      results_table<-resrow
    }else{
      results_table<-rbind(results_table,resrow)
    }
  }
  cat(".")
}
results_dt<-data.table(results_table)
apply(results_dt[MIC.R2>0.02 & MIC>0.05],1,function(rn){
  print(rn[["DV"]])
  print(ggplot(maindata_dt.mu,aes_string(as.character(rn[["IV"]]),as.character(rn[["DV"]])))+geom_jitter(alpha=0.3,width=0.1,height=0.1)+
          labs(title=paste0(rn[["DV"]]," and ",rn[["IV"]]),
               subtitle=as.character(paste0("MIC score:", rn[["MIC"]]))))

})

#outstanding questions:
