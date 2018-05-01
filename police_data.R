list.of.packages <- c("data.table")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(data.table)

data.cleaned.montana<-data.table(read.csv("MT-clean.csv",stringsAsFactors = FALSE))
data.cleaned.vermont<-data.table(read.csv("VT-clean.csv",stringsAsFactors = FALSE))
summary(data.cleaned.montana)

data.cleaned.montana[,.N,by=(driver_gender)]

#unsure whether to interpret gender not recorded as "undetermined" or "non-binary"
#As it is much smaller than the proportion of non-binary people in the population
#I'm going to assume everyone was classified either "F" or "M" and will assume
#that 50% of the undetermined individuals are in fact male.
proportion.male.montana<-data.cleaned.montana[driver_gender %in% c("M") ,.N]/
  data.cleaned.montana[driver_gender %in% c("M","F") ,.N]
formatC(proportion.male.montana,10)

#How many more times likely are you to be arrested in Montana during a traffic stop if you have out of state plates?
#OK so we are talking about 
#t = P(Arrest|Out of State Plates)/P(Arrest|In-state Plates)
data.cleaned.montana[,.N,by=.(out_of_state,is_arrested)]
p_arrest_given_out_of_state_plates<-
  nrow(data.cleaned.montana[is_arrested==TRUE & out_of_state==TRUE])/
  nrow(data.cleaned.montana[out_of_state==TRUE])

p_arrest_given_in_state_plates<-
  nrow(data.cleaned.montana[is_arrested==TRUE & out_of_state==FALSE])/
  nrow(data.cleaned.montana[out_of_state==FALSE])

out_of_state_multiplier<-p_arrest_given_out_of_state_plates/p_arrest_given_in_state_plates
formatC(out_of_state_multiplier,10,format="f")

data.cleaned.montana.arrest.state<-
  data.cleaned.montana[!is.na(is_arrested) & !is.na(out_of_state),
                       .(is_arrested,out_of_state)]
table(data.cleaned.montana.arrest.state)


#Perform a (χ2) test to determine whether the proportions of arrests in these two populations are equal. 
#What is the value of the test statistic?
#not sure which two populations they're talking about! 
#From context, I'm guessing they want the licence plate comparison.
#But come back to this!
cst<-chisq.test(data.cleaned.montana.arrest.state$is_arrested,data.cleaned.montana.arrest.state$out_of_state)
formatC(cst$statistic,10,format="f")

#What proportion of traffic stops in Montana resulted in speeding violations? 
#In other words, find the number of violations that include "Speeding" in the violation description 
#and divide that number by the total number of stops (or rows in the Montana dataset).
table(data.cleaned.montana$violation_raw)
#going to use violation cleaned data because I'll take the capitalization of "Speeding" in the question
#as a hint that we use the cleaned dta, because that's the one in which Speeding is formatted with a capital and lower case letters.
violations.inc.speeding.N<-sum(grepl("Speeding",data.cleaned.montana$violation))
violations.inc.speeding.prop<-violations.inc.speeding.N/nrow(data.cleaned.montana)
formatC(violations.inc.speeding.prop,10,format="f")


#How much more likely does a traffic stop in Montana result in a DUI than a traffic stop in Vermont? 
#To compute the proportion of traffic stops that result in a DUI, 
#divide the number of stops with "DUI" in the violation description by the total number of stops.
montana.violations.inc.DUI.N<-sum(grepl("DUI",data.cleaned.montana$violation))
vermont.violations.inc.DUI.N<-sum(grepl("DUI",data.cleaned.vermont$violation))
montana.violations.inc.DUI.prop<-montana.violations.inc.DUI.N/nrow(data.cleaned.montana)
vermont.violations.inc.DUI.prop<-vermont.violations.inc.DUI.N/nrow(data.cleaned.vermont)
montana.vermont.comparison.DUI<-montana.violations.inc.DUI.prop/vermont.violations.inc.DUI.prop
formatC(montana.vermont.comparison.DUI,10,format="f")

#What is the extrapolated, average manufacture year of vehicles involved in traffic stops in Montana in 2020? 
#To answer this question, calculate the average vehicle manufacture year for each year's traffic stops. 
#Extrapolate using a linear regression.
#let's take a look at the data now.
data.cleaned.montana$vehicle_year_int<-as.integer(data.cleaned.montana$vehicle_year)
data.cleaned.montana$stop_datePOSIX<-as.Date(data.cleaned.montana$stop_date)
hist(data.cleaned.montana$stop_datePOSIX,breaks=100)
#i'm going to take the instructions literally 
#and use the YEARLY traffic stops and ignore variation within years.
data.cleaned.montana$stop_year<-as.integer(substr(data.cleaned.montana$stop_date,1,4))
hist(data.cleaned.montana$stop_year)

#library(ggplot2)
#ggplot(data.cleaned.montana,aes(stop_year,vehicle_year))+geom_point(alpha=0.3)
#have commented this out in case you don't have ggplot
#I don't think a linear model is a great fit here, but that is the instruction so we'll go with it.

model.vehicle.year<-lm(vehicle_year_int~stop_year,data.cleaned.montana[!is.na(stop_year) & !is.na(vehicle_year_int)])
average.year.2020<-model.vehicle.year$coefficients["(Intercept)"]+model.vehicle.year$coefficients["stop_year"]*2020
#I think they're asking for a p-value just calculating the regression model from the averages with each year as one data point.
#the p-value if we use all the data points is essentially zero.
data.by.stop.year<-data.cleaned.montana[!is.na(stop_year) & !is.na(vehicle_year_int),
                                        .(vehicle_year_int=mean(vehicle_year_int)),by=stop_year]
model.vehicle.year.simple<-lm(vehicle_year_int~stop_year,data.by.stop.year)
summary(model.vehicle.year.simple)
average.year.2020.simple<-model.vehicle.year.simple$coefficients["(Intercept)"]+model.vehicle.year.simple$coefficients["stop_year"]*2020
formatC(average.year.2020.simple,10,format="f")
model.p.val<-summary(model.vehicle.year.simple)["coefficients"][[1]]["stop_year",4]
formatC(model.p.val,10,format="f")

#combine the data sets
data.cleaned.mt.vt<-rbind(data.cleaned.vermont,data.cleaned.montana,fill=TRUE)
data.cleaned.mt.vt$stop_time[1:100]
#these are HH:MM format so we can simply take the first 2 characters to get the hours.
data.cleaned.mt.vt$stop_hour<-as.integer(substr(data.cleaned.mt.vt$stop_time,1,2))
hist(data.cleaned.mt.vt$stop_hour)
max.stop.count<-max(table(data.cleaned.mt.vt$stop_hour))
min.stop.count<-min(table(data.cleaned.mt.vt$stop_hour))
max.stops<-max.stop.count==table(data.cleaned.mt.vt$stop_hour)
min.stops<-min.stop.count==table(data.cleaned.mt.vt$stop_hour)


hourly.stop.maxmin.diff<-max.stop.count-min.stop.count
formatC(hourly.stop.maxmin.diff,10,format="f")

#We can use the traffic stop locations to estimate the areas of the counties in Montana. 
#Represent each county as an ellipse with semi-axes given by a single standard deviation of the longitude and latitude of stops within that county. 
#What is the area, in square kilometers, of the largest county measured in this manner? 
#Please ignore unrealistic latitude and longitude coordinates.
#so we can use latitude and longitude
#First cut out "unrealistic" coordinates

#we'll divide montana into two shapes
#the first is the Western Portion. Thse coordinates are approximate and include some of Idaho. This is not rectangular.
#NW LL [49.000,-116.05]
#NE LL [49.000,-111.05]
#SW LL [45.2,-116.05]
#SE LL [43.7,-111.05]


#The second is the Eastern portion, a rectangle with the following coordinates:
#Montana Eastern Portion
#NW LL [49.000,-111.05]
#NE LL [49.000,-104.05]
#SW LL [45,-111.05]
#SE LL [45,-104.04]

#first let's check if the lat and long are in these positions.
data.cleaned.montana[,EasternPortionMatch:=lat<=(49+1) & lat>=(45-1) & (lon<=-104.04+1) & (lon>=-111.05-1)] #this should be right.

getlat<-function(longitude){(longitude- -116.05)/(-111.05- -116.05) *-(45.2-43.7)+45.2}
data.cleaned.montana[,WesternPortionMatch:=lat<=(49+1) & lat>=getlat(lon) & (lon<=-111.05+1) & (lon>=-116.05-1)] #this needs work.
data.cleaned.montana$IsInMontana<-data.cleaned.montana$EasternPortionMatch | data.cleaned.montana$WesternPortionMatch

county_areas<-data.cleaned.montana[!is.na(lat) & !is.na(lon) & IsInMontana,
                                   .(latSD=sd(lat),lonSD=sd(lon),latMean=mean(lat),lonMean=mean(lon))
                                   , by=county_name]
#convert these into kilometer measurements
county_areas$latkmSD<-county_areas$latSD*110.574
county_areas$lonkmSD<-county_areas[,lonSD*(111.320*(cos((latMean*pi)/180)))]
county_areas$EllipseArea<-pi*county_areas$latkmSD*county_areas$lonkmSD
county_areas[order(-EllipseArea)]
formatC(county_areas[county_name=="Lincoln County",EllipseArea],digits=10,format="f")

