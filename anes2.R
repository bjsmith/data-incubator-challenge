anes19482012<-read.csv("anes/anes_timeseries_cdf/anes_timeseries_cdf_rawdata.txt",sep="|")
colnames(anes19482012)

table(anes19482012$VCF0004)
intersect(colnames(anes19482012),colnames(anesdata.raw))