options(scipen = 999)
install.packages("ggpubr")
library(ggpubr)
library(tidyverse)

dat <-read.csv("C:\\Users\\mahno\\OneDrive\\Desktop\\MS Business Analytics\\Fall Semester\\Intro to Data Analytics and Business Intelligence\\Assignment 1\\cancerdeaths.csv")
head(dat,n=3)

colnames(dat)
df<-dat%>%group_by(medIncome,countyName,State)%>%
  summarise(zipCode,countyCode,povertyPercent,PovertyEst,popEst2015,avgAnnCount,recentTrend,fiveYearTrend,deathRate,avgDeathsPerYear,recTrend,incidenceRate,sum_of_studycount = sum(studyCount))%>%arrange(countyName)%>%distinct()

datstc<-dat%>%distinct()%>%group_by(medIncome,countyName,State)%>%
  summarise(countyCode,povertyPercent,PovertyEst,popEst2015,avgAnnCount,recentTrend,fiveYearTrend,deathRate,avgDeathsPerYear,recTrend,incidenceRate,sum_of_studycount = sum(studyCount))%>%arrange(countyName)%>%distinct()

#Countywise
datcs<-dat%>%select(State,countyName,incidenceRate)%>%distinct()%>%group_by(countyName,State)%>%
  summarise(sum_of_Incidence_Rate = sum(incidenceRate))%>%arrange(Regions,State,sum_of_Incidence_Rate)

datcm<-dat%>%select(Regions,State,countyName,incidenceRate)%>%distinct()%>%group_by(Regions,State,countyName)%>%
  summarise(mean_of_Incidence_Rate = mean(incidenceRate))%>%arrange(Regions,State,mean_of_Incidence_Rate)
dat$Regions<- with(dat, ifelse(datr$State %in% East, "East",
                                 ifelse(datr$State %in% South, "South", 
                                        ifelse(datr$State %in% Midwest, "Midwest","West"))))

#Statewise
datss<-dat%>%select(State,countyName,incidenceRate)%>%distinct()%>%group_by(State)%>%
  summarise(sum_of_Incidence_Rate = sum(incidenceRate))%>%arrange(sum_of_Incidence_Rate)

datsm<-dat%>%select(Regions,State,countyName,incidenceRate)%>%distinct()%>%group_by(Regions,State)%>%
  summarise(mean_of_Incidence_Rate = mean(incidenceRate))%>%arrange(Regions,State,mean_of_Incidence_Rate)


#Regionwise

East<-c("VT","DE","ME",'NH','MA','RI','CT','NJ','NY','PA')
length(East)
South<-c("DC",'DE','MD','WV','VA','NC','SC','KY','TN','GA','FL','AL','MS','AR','LA','TX','OK')
length(South)
Midwest<-c("ND",'SD','NE','KS','MN','IA','MO','WI','IL','MI','IN','OH')
length(Midwest)
West<-c("HI",'WA','OR','MT','WY','ID','CA','NV','UT','AZ','CO','NM')
length(West)
total<-length(West)+length(Midwest)+length(East)+length(South)
total
unique(dat$State)

datr<-dat%>%select(State,countyName,incidenceRate)

datr$Regions<- with(datr, ifelse(datr$State %in% East, "East",
                            ifelse(datr$State %in% South, "South", 
                                   ifelse(datr$State %in% Midwest, "Midwest","West"))))

datrs<-datr%>%select(Regions,State,countyName,incidenceRate)%>%distinct()%>%group_by(Regions)%>%
  summarise(sum_of_Incidence_Rate = sum(incidenceRate))%>%arrange(sum_of_Incidence_Rate)

datrm<-datr%>%select(Regions,State,countyName,incidenceRate)%>%distinct()%>%group_by(Regions)%>%
  summarise(mean_of_Incidence_Rate = mean(incidenceRate))%>%arrange(mean_of_Incidence_Rate)


#Standardisation
dat3<-dat2%>%select(Regions,incidenceRate_Region, popEst2015_Region)%>%group_by(Regions)%>%
  summarise(incidenceRate_Region = incidenceRate_Region/popEst2015_Region)

dat5<-dat2%>%select(incidenceRate,State)%>%group_by(State)%>%
  summarise(State,incidenceRate = incidenceRate/sum(incidenceRate))
write.csv(datrm,"C:\\Users\\mahno\\OneDrive\\Desktop\\MS Business Analytics\\Fall Semester\\Intro to Data Analytics and Business Intelligence\\Assignment 1\\Regionwise Q1.csv", row.names = FALSE)

write.csv(dat5,"C:\\Users\\mahno\\OneDrive\\Desktop\\MS Business Analytics\\Fall Semester\\Intro to Data Analytics and Business Intelligence\\Assignment 1\\Cancer Incidence Rate Region and StateWise.csv", row.names = FALSE)

ggplot(data = dat4) + 
  geom_bar(mapping = aes(x=Regions))




#Correlation
cor(x, y,  method = "pearson", use = "complete.obs")

#Zipcode
res_zip <- cor.test(dat$incidenceRate, dat$zipCode, 
                method = "pearson", use = "complete.obs")
res_zip

#State
res_state <- cor.test(dat$incidenceRate, dat$State, 
                      method = "pearson", use = "complete.obs")\

#countycode
res_countycode <- cor.test(datstc$incidenceRate, datstc$countyCode, 
                            method = "pearson", use = "complete.obs")
res_countycode

#zipcode and countycode
res_zip_countycode <-  cor.test(dat$zipCode, dat$countyCode, 
                            method = "pearson", use = "complete.obs")
res_zip_countycode

#Studycount
res_studycount <-  cor.test(datstc$incidenceRate, datstc$sum_of_studycount, 
                            method = "pearson", use = "complete.obs")
res_studycount

#povertyEst
res_povertyEst <-  cor.test(dat$incidenceRate, dat$PovertyEst, 
                            method = "pearson", use = "complete.obs")
res_povertyEst

#Poverty percent
res_povertypercent<-cor.test(dat$incidenceRate, dat$povertyPercent, 
                             method = "pearson", use = "complete.obs")
res_povertypercent

#medIncome
res_medIncome<-cor.test(dat$incidenceRate, dat$medIncome, 
         method = "pearson", use = "complete.obs")
res_medIncome

#a<-22640	
#b<-48388.75	
#c<-74137.5	
#d<-99886.25
#e<-125635



datstc$MedIncome_Intervals<- with(datstc, ifelse(datstc$medIncome >= 22640 & datstc$medIncome < 48388.75, "Very Low",
                                 ifelse(datstc$medIncome >= 48388.75 & datstc$medIncome < 74137.5, "Low", 
                                        ifelse(datstc$medIncome >= 74137.5 & datstc$medIncome < 99886.25, "High","Very High"))))



dat_reg_new<-read.csv("C:\\Users\\mahno\\OneDrive\\Desktop\\MS Business Analytics\\Fall Semester\\Intro to Data Analytics and Business Intelligence\\Assignment 1\\cancer regression clean dataset.csv")
#dat_reg_new$countyCode<- as.character(dat_reg_new$countyCode)
datreg1<-dat%>%distinct()%>%group_by(medIncome,countyName,State)%>%
  summarise(countyCode,povertyPercent,PovertyEst,popEst2015,avgAnnCount,recentTrend,fiveYearTrend,deathRate,avgDeathsPerYear,recTrend,incidenceRate,studyCount = sum(studyCount))%>%arrange(countyName)%>%distinct()

datreg2 <- datreg1[!is.na(as.numeric(as.character(datreg1$fiveYearTrend))),]
datreg2 <- datreg2[!is.na(as.numeric(as.character(datreg1$))),]

#without medincome intervals
fit1 = lm(incidenceRate ~ countyCode + povertyPercent + medIncome + PovertyEst + popEst2015 + avgAnnCount + recentTrend + fiveYearTrend + deathRate + avgDeathsPerYear + recTrend + studyCount, data=dat_reg_new)
summary(fit1)

#without medincome intervals
fit2 = lm(incidenceRate ~ countyCode + povertyPercent + medIncome + MedIncome_Intervals + PovertyEst + popEst2015 + avgAnnCount + recentTrend + fiveYearTrend + deathRate + avgDeathsPerYear + recTrend + studyCount, data=dat_reg_new)
summary(fit2)

#removing varaibles that require dummy variables
fit3 = lm(incidenceRate ~ countyCode + povertyPercent + medIncome + PovertyEst + popEst2015 + avgAnnCount + fiveYearTrend + deathRate + avgDeathsPerYear + studyCount, data=dat_reg_new)
summary(fit3)

#removing povertypercent and avgDeathsPerYear
fit4 = lm(incidenceRate ~ countyCode + medIncome + PovertyEst + popEst2015 + avgAnnCount + fiveYearTrend + deathRate + studyCount, data=dat_reg_new)
summary(fit4)

#removing povertyEst and avgDeathsPerYear
fit5 = lm(incidenceRate ~ countyCode + medIncome + povertyPercent + popEst2015 + avgAnnCount + fiveYearTrend + deathRate + studyCount, data=dat_reg_new)
summary(fit5)

#removing studycount (options)
fit6 = lm(incidenceRate ~ countyCode + medIncome + povertyPercent + popEst2015 + avgAnnCount + fiveYearTrend + deathRate, data=dat_reg_new)
summary(fit6)

#removing poverty data
fit7 = lm(incidenceRate ~ countyCode + medIncome + popEst2015 + avgAnnCount + fiveYearTrend + deathRate, data=dat_reg_new)
summary(fit7)

# Everything Numeric
fit8 = lm(incidenceRate ~  countyCode + medIncome + povertyPercent + popEst2015 + avgAnnCount + fiveYearTrend + deathRate, data=dat_reg_new)
summary(fit8)

#Excluding population data
fit9 = lm(incidenceRate ~  countyCode + medIncome + povertyPercent  + avgAnnCount + fiveYearTrend + deathRate, data=dat_reg_new)
summary(fit9)


