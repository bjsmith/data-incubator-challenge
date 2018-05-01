list.of.packages <- c("data.table","mice")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(data.table)
library(mice)
data.dir<-"nlsy97/nlsy97-initial/"
source("nlsy97/nlsy97-initial/nlsy97-initial.R")
varlabels

colnames(new_data)<-
  c("PubID","Sex","BdateMonth","BdateYear",
    "Income1997","HHNetWorth1997",
    "RelationshipToHHParentFigure","SampleType1997",
    "RaceEthnicity1997","ReligiousAttendancePerWeek1998",
    "MathVerbal1997","LifeRating2008",
    "FamilyIncome2015","WagesSalary2015")

maindata_dt<-data.table(new_data)
table(new_data$ReligiousAttendancePerWeek1998)
#we need a better religion variable than this. Seems too skewed to quite high attendance
maindata_dt$ReligiousAttendanceWeekly<-maindata_dt$ReligiousAttendancePerWeek1998>0
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

#Race
maindata_dt$RaceEthnicity1997<-factor(maindata_dt$RaceEthnicity1997,levels=c(1,2,3,4),labels=c("Black","Hispanic","Mixed Race (Non-Hispanic)","Not Black or Hispanic"))
maindata_dt$REIsBlack<-maindata_dt$RaceEthnicity1997=="Black"
maindata_dt$REIsHispanic<-maindata_dt$RaceEthnicity1997=="Hispanic"
maindata_dt$REIsMixedNonHispanic<-maindata_dt$RaceEthnicity1997=="Mixed Race (Non-Hispanic)"

#take a look at the variables.
for (varname in c("FamilyIncome2015","Sex","RaceEthnicity1997","MathVerbal1997",
                  "HHNetWorth1997","Income1997",
                  "ReligiousAttendanceWeekly","RelationshipToHHParentFigure","LifeRating2008")){
  print(varname)
  
  if (dim(maindata_dt[,.N,by=get(varname)])[1]<10){
    print(table(maindata_dt[,get(varname)]))
  }else{
    hist(maindata_dt[,get(varname)],main = varname)
  }
}
summary(lm(FamilyIncome2015~
             Sex+MathVerbal1997+
             HHNetWorth1997+Income1997+
             ReligiousAttendanceWeekly+RelationshipToHHParentFigure+REIsBlack+REIsHispanic+REIsMixedNonHispanic,maindata_dt))


summary(lm(FamilyIncome2015~
             Sex+MathVerbal1997+
             HHNetWorth1997+Income1997+RelationshipToHHParentFigure+
             ReligiousAttendanceWeekly*REIsBlack+ReligiousAttendanceWeekly*REIsHispanic+ReligiousAttendanceWeekly*REIsMixedNonHispanic,maindata_dt))

#Drop religious attendance and Sex; doens't seem to make any difference.
summary(lm(FamilyIncome2015~MathVerbal1997+
             HHNetWorth1997+Income1997+RelationshipToHHParentFigure+
             REIsBlack+REIsHispanic+REIsMixedNonHispanic,maindata_dt))


summary(lm(WagesSalary2015~MathVerbal1997+
             Sex+
             HHNetWorth1997+Income1997+
             RelationshipToHHParentFigure+
             REIsBlack+REIsHispanic+REIsMixedNonHispanic,maindata_dt))
summary(lm(WagesSalary2015~MathVerbal1997+
             Sex+
             HHNetWorth1997+Income1997+
             RelationshipToHHParentFigure+LifeRating2008+
             REIsBlack+REIsHispanic+REIsMixedNonHispanic,maindata_dt))

#should replace the 1997 family income variable with "ParentsOwnIncome" to be confident about what we're looking at.

#OK, now, predicting welbeing in 2008
summary(lm(LifeRating2008~MathVerbal1997+
             Sex+
             HHNetWorth1997+Income1997+
             RelationshipToHHParentFigure+
             REIsBlack+REIsHispanic+REIsMixedNonHispanic,maindata_dt))
#interesting: income and household net worth are not related to wellbeing, but having two biological parents is a predictor of wellbeing
#what about the log of 1997 income?

maindata_dt$LnIncome1997=log(maindata_dt$Income1997+1)
summary(lm(LifeRating2008~
             Sex+MathVerbal1997+
             HHNetWorth1997+
             RelationshipToHHParentFigure+
             REIsBlack+REIsHispanic+REIsMixedNonHispanic,maindata_dt))

summary(lm(LifeRating2008~
             Sex+
             HHNetWorth1997+
             RelationshipToHHParentFigure+
             REIsBlack+REIsHispanic+REIsMixedNonHispanic,maindata_dt))
#there may be relationship from household net worth to liferating2008; it is mediated by Math/Verbal scores.


#So, from our intial analysis we saw:
#Race is a predictor of both Income and wellbeing
#Sex is a predictor of personal Wages and salary but not 2015 household income. Is 2015 household income for the child?
#Life Rating in 2008 is a strong predictor of income in 2015.
#Childhood Income and household net worth are strong predictors of HH income and personal salary in 2015
#Childhood Household net worth but not income is a predictor of 2008 wellbeing
#Sex doesn't predict well-being but women do earn less in 2015, but they are not in lower income households.
#Math/verbal scores explain about 4% of the variance, or about 25% of explainable variance.
#Most significant effects are in the expected direction, except that hispanics show moderately better well-being than whites
#Some combination of Math/Verbal scores, Childhood Household net worth, and Childhood Household income predict well-being
#but these effects are inconsistent if two or more of those variables are included, so we can't prove they exist independently of one another.

#Overall effect sizes are small; we can't predict more than 17% of the variance and most of that is childhood income predicting adult income.
#Wellbeing is hard to capture because most people sit in a narrow band at the upper level of the well-being measure.


#To do:
#See if this is mediated by interaction of having a child and being a woman.
#Add adult education
#Need to do some kind of FIML

#let's try out some multiple imputation first.
#This may exaggerate links between our variables!

maindata_dt.nm <- mice(new_data, m=5, maxit = 100, method = 'pmm', seed = 42)
library(mice)
