library("rio")
library(tibble)
library(dplyr)
library(hms)
library(VIM)
library(tidyr)
library(ggplot2)
library(mgcv)
library(ggstance)
migration <- import("migration.xlsx")
# turn migration into tibble data frame
tbl_migration = as_tibble(migration)
tbl_migration = tibble::rownames_to_column(tbl_migration)
#---------------------------------------------------------------------------------------------
# deal with outliers
#---------------------------------------------------------------------------------------------
# delete the 327th row
tbl_migration <- tbl_migration[(-327),]
#---------------------------------------------------------------------------------------------
# Handle outliers for Humidity and Cloud Cover value
#---------------------------------------------------------------------------------------------
# the record whose Humidity is larger than 100 is the 429th row
tbl_migration %>%
  filter(tbl_migration$Humidity >100)

# the record whose Cloud Cover is larger than 100 is the 428th row
tbl_migration %>%
  filter(tbl_migration$`Cloud Cover` >100)

# delete the records with above 100 Humidity and Cloud Cover
tbl_migration <- tbl_migration[(-428:-429),]

# turn the outlier BARO into NA
tbl_migration$BARO <- ifelse(tbl_migration$BARO<26|tbl_migration$BARO>34,
                             NA, tbl_migration$BARO)
#---------------------------------------------------------------------------------------------
# deal with NA values for weather conditions
#---------------------------------------------------------------------------------------------
# turn 0 values in tbl_migration$Humidity and tbl_migration$BARO into NA values
tbl_migration$Humidity <- na_if(tbl_migration$Humidity, 0)
tbl_migration$BARO <- na_if(tbl_migration$BARO, 0)

# there are 23 records in our data with NA values for Precipitation2
tbl_migration$Precipitation2 <- na_if(tbl_migration$Precipitation2, "NA")
tbl_migration$Visibility <- na_if(tbl_migration$Visibility, "NA")
tbl_migration %>%
  filter(is.na(tbl_migration$Precipitation2))


tbl_migration <- separate(tbl_migration, Date, sep = " ",into = c("Date", "time"))
# deal with NA values for Precipitation2 and Visibility
tbl_migration <- hotdeck(
  tbl_migration,
  variable = c("Visibility","Precipitation2", "Precipitation"),
  domain_var = "Date"
)

# deal with the NA values for Humdity and BARO
# there are 45 records with NA Humidity
tbl_migration %>%
  filter(is.na(tbl_migration$Humidity))

boxplot1 <- tbl_migration %>%
  filter(!is.na(tbl_migration$Humidity))
# the boxplot of Precipitation VS Humidity
boxplot(tbl_migration$Humidity~tbl_migration$Precipitation2,data=boxplot1,
        main="Humidity VS Precipitation",
        xlab="Precipitation", ylab="Humidity")
tbl_migration <- hotdeck(
  tbl_migration,
  variable = "Humidity",
  domain_var = "Precipitation2"
)

# there are 57 rows with NA BARO
tbl_migration %>%
  filter(is.na(tbl_migration$BARO))

# impute NA BARO with the overall mean
tbl_migration$BARO <- na_if(tbl_migration$BARO, "NA")
tbl_migration$BARO <- ifelse(is.na(tbl_migration$BARO),mean(tbl_migration$BARO, 
                                                            na.rm = TRUE), tbl_migration$BARO)
#---------------------------------------------------------------------------------------------
# deal with NA values for counters
#---------------------------------------------------------------------------------------------
# we add the rowname of records after anonymous, which is in the form of "anonymous in:rowname"
tbl_migration$Counter <- ifelse(test = tbl_migration$Counter == "NA", 
                                yes = sprintf("anonymous in:%s", tbl_migration$rowname), 
                                no = tbl_migration$Counter)
tbl_migration$Counter<- factor(tbl_migration$Counter)
levels(tbl_migration$Counter)
# we regard every anonymous counter as different counters, 
# thus there are 24 different counters in total
#---------------------------------------------------------------------------------------------
# deal with NA values for observers
#---------------------------------------------------------------------------------------------
# get the actual total number of volunters involving in a particlular observation activity
tbl_migration$volunter_actual <- tbl_migration$Observer/tbl_migration$Duration


# we find that there are 6 records that the observation 
# time of different voliunters in that record are not equal
unequal_observetime <- tbl_migration %>%
  filter(tbl_migration$volunter_actual-floor(tbl_migration$volunter_actual)>0)

# get the recorded number of volunters including observers and counter
tbl_migration = tbl_migration %>% 
  mutate(volunter_recorded = ifelse((Counter != `Observer 1` & Counter != Observer2.new &
                                       Counter != `Observer 3` & Counter != `Observer 4`),
                                    as.numeric(`Observer 1` != "NA") + as.numeric(Observer2.new != "NA")+
                                      as.numeric(`Observer 3` != "NA") + as.numeric(`Observer 4` != "NA")+1,
                                    as.numeric(`Observer 1` != "NA")+as.numeric(Observer2.new != "NA")+
                                      as.numeric(`Observer 3` != "NA")+as.numeric(`Observer 4` != "NA")))
tbl_migration$volunter_recorded <- as.integer(tbl_migration$volunter_recorded)


# get the actual total number of volunters involving in a particlular observation activity
tbl_migration$volunter_actual <- tbl_migration$Observer/tbl_migration$Duration
#---------------------------------------------------------------------------------------------
# mutate the observer ID
#---------------------------------------------------------------------------------------------

# if the counter ID is the same as one of the observer IDs, we need to
# replace that observer ID with "anonymous", which means that there is 
# another observer denoted as "anonynous"
tbl_migration <- tbl_migration %>%
  mutate(`Observer 1` = ifelse(Counter == `Observer 1`,sprintf("anonymous in:%s", rowname),
                               `Observer 1`),
         Observer2.new = ifelse(Counter == Observer2.new,sprintf("anonymous in:%s", rowname),
                                Observer2.new),
         `Observer 3` = ifelse(Counter == `Observer 3`,sprintf("anonymous in:%s", rowname),
                               `Observer 3`),
         `Observer 4` = ifelse(Counter == `Observer 4`,sprintf("anonymous in:%s", rowname),
                               `Observer 4`))
# then calculate number of recorded volunteers 
tbl_migration = tbl_migration %>% 
  mutate(volunter_recorded = as.numeric(`Observer 1` != "NA") + 
           as.numeric(Observer2.new != "NA")+
           as.numeric(`Observer 3` != "NA") + as.numeric(`Observer 4` != "NA")+1)
# calculate number of actual volunteers
# consider the situation when volunter_actual is not a integer
tbl_migration$volunter_actual <- ifelse((tbl_migration$volunter_actual-
                                           floor(tbl_migration$volunter_actual))>0,
                                        as.integer(tbl_migration$volunter_actual)+1,
                                        as.integer(tbl_migration$volunter_actual))
#---------------------------------------------------------------------------------------------
# calculate the difference between actual number of volunters and recorded number of volunters
#---------------------------------------------------------------------------------------------
tbl_migration$volunter_difference <- tbl_migration$volunter_actual - 
  tbl_migration$volunter_recorded
# if volunter_difference == 1, we need to replace one of NAs into "anonymous"
tbl_migration <- tbl_migration %>%
  mutate(`Observer 1` = ifelse(volunter_difference == 1,ifelse(`Observer 1` == "NA",
                                                               sprintf("anonymous in:%s", rowname),
                                                               `Observer 1`),`Observer 1`),
         Observer2.new = ifelse(volunter_difference == 1,ifelse(`Observer 1` != 
                                                                  sprintf("anonymous in:%s", rowname) & Observer2.new == "NA",
                                                                sprintf("anonymous in:%s", rowname),Observer2.new),Observer2.new),
         `Observer 3` = ifelse(volunter_difference == 1,ifelse(`Observer 1` != 
                                                                 sprintf("anonymous in:%s", rowname) & Observer2.new != sprintf("anonymous in:%s", rowname)
                                                               & `Observer 3` == "NA",
                                                               sprintf("anonymous in:%s", rowname), `Observer 3`), `Observer 3`),
         `Observer 4` = ifelse(volunter_difference == 1, ifelse(`Observer 1` != 
                                                                  sprintf("anonymous in:%s", rowname) & 
                                                                  Observer2.new != sprintf("anonymous in:%s", rowname) & `Observer 3` != 
                                                                  sprintf("anonymous in:%s", rowname)& `Observer 4` == "NA", 
                                                                sprintf("anonymous in:%s", rowname), `Observer 4`), `Observer 4`))



#---------------------------------------------------------------------------------------------
# combine the four observer columns into one column with several factor
# levels for random effects
#---------------------------------------------------------------------------------------------
# select columns observer1 to observer 4 and create a new data frame "observer"
observer <- tbl_migration %>%
  select(`Observer 1`: `Observer 4`)
# get the frequence and proportion of each observer_id
observer_id = data.frame(table(unlist(observer)))
col_name <- c("Observer_id", "Freq")
colnames(observer_id) <- col_name
observer_id$prop = prop.table(observer_id$Freq)
# remove the observer id "NA"
# sort the id according to the prop
experienced <- observer_id %>%
  filter(observer_id$Observer_id != "NA") %>%
  arrange(desc(prop)) %>%
  head(10)
# get the experienced observers' id and put them into a vector
experienced_id <- c(experienced$Observer_id)
#---------------------------------------------------------------------------------------------
# create a new column tbl_migration$num_experience which equals to the number of
# experienced observers in each record
#---------------------------------------------------------------------------------------------
for (observer in 1:nrow(tbl_migration)) {
  tbl_migration[observer, "num_experience"] <- 0
  if (is.element(tbl_migration[observer,"Observer 1"],experienced_id)) {
    tbl_migration[observer, "num_experience"] <- 
      tbl_migration[observer, "num_experience"] + 1}
  if (is.element(tbl_migration[observer,"Observer2.new"],experienced_id)) {
    tbl_migration[observer, "num_experience"] <-
      tbl_migration[observer, "num_experience"] + 1}
  if (is.element(tbl_migration[observer,"Observer 3"],experienced_id)) {
    tbl_migration[observer, "num_experience"] <- 
      tbl_migration[observer, "num_experience"] + 1}
  if (is.element(tbl_migration[observer,"Observer 4"],experienced_id)) {
    tbl_migration[observer, "num_experience"] <- 
      tbl_migration[observer, "num_experience"] + 1}
}
#---------------------------------------------------------------------------------------------
# calculate the number of unexperienced observers in each record
#---------------------------------------------------------------------------------------------
# tbl_migration$volunter_actual-1 is the actual total number of observers in each record
tbl_migration$non_experience <- tbl_migration$volunter_actual-1-tbl_migration$num_experience
# create a new column with several levels that evaluate the experience level in each record
tbl_migration$experience_level <- sprintf("experienced observers:%s, 
                                unexperienced observers:%s", tbl_migration$num_experience, 
                                          tbl_migration$non_experience )
tbl_migration$experience_level <- factor(tbl_migration$experience_level)
levels(tbl_migration$experience_level)
# we can find that there are 11 different experience levels in total

#---------------------------------------------------------------------------------------------
# plot Number of raptors recorded from Spring migration 2018 to end of Spring migration 2019
#---------------------------------------------------------------------------------------------

tbl_migration$Date <- as.Date(tbl_migration$Date)
migration_2018 <- tbl_migration %>%
  filter(tbl_migration$Year=="2018")
migration_2019 <- tbl_migration %>%
  filter(tbl_migration$Year=="2019")
p1 <- ggplot(migration_2018, aes(x=Date, y=TOTAL)) +
  geom_point()+
  xlab(" Year 2018") +
  ggtitle("Total raptors in year 2018") +
  scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 day", date_labels="%m") 
p2 <- ggplot(migration_2019, aes(x=Date, y=TOTAL)) +
  geom_point() +
  xlab(" Year 2019") +
  ggtitle("Total raptors in year 2019") +
  scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 day", date_labels="%m") 
grid.arrange(p1,p2,nrow=2)  

#---------------------------------------------------------------------------------------------
# the plot of total against precipitation
#---------------------------------------------------------------------------------------------
ggplot(tbl_migration, aes(x=Precipitation2, y=TOTAL)) +
  geom_point() +
  ggtitle("Total&Precipitation2")
#---------------------------------------------------------------------------------------------
# add a new column period
#---------------------------------------------------------------------------------------------
tbl_migration$Month <- as.numeric(tbl_migration$Month)
tbl_migration$period <- ifelse(tbl_migration$Month>=2&tbl_migration$Month<=5,
                               "Spring","Fall")

p3 <- ggplot(tbl_migration, aes(x=Start, y=TOTAL)) +
  geom_point() +
  xlab("Start time") +
  ggtitle("Total&Start time") 
p4 <- ggplot(tbl_migration, aes(x=Duration, y=TOTAL)) +
  geom_point() +
  xlab("Duration") +
  ggtitle("Total&Duration")
p5 <- ggplot(tbl_migration, aes(x=Start, y=Duration)) +
  geom_point() +
  xlab("Start time") +
  ggtitle("Duration&Start time") 
grid.arrange(p3,p5,p4,nrow=3) 
#---------------------------------------------------------------------------------------------
# pairplot of weather conditions against total
#---------------------------------------------------------------------------------------------
tbl_migration <- tbl_migration %>%
  dplyr::rename(Wind_spd = `Wind Spd2`) %>%
  dplyr::rename(Cloud_Cover = `Cloud Cover`) 
tbl_migration <-tbl_migration %>% 
  filter(tbl_migration$Cloud_Cover<=100)
tbl_migration$Visibility <- as.numeric(tbl_migration$Visibility)
tbl_migration$Precipitation2 <- as.numeric(tbl_migration$Precipitation2)
tbl_migration$Wind_spd <- as.numeric(tbl_migration$Wind_spd)
pairs(tbl_migration[ ,c("BARO","Humidity","Visibility","Precipitation2",
                        "Cloud_Cover","Wind_spd" ,"Temp","TOTAL")], pch = 25, lower.panel=NULL)
#---------------------------------------------------------------------------------------------
#combine precipitation 2456 into one level
#---------------------------------------------------------------------------------------------
tbl_migration$Precipitation2 <- as.factor(ifelse(tbl_migration$Precipitation2 %in% c(2,4,5,6), 
                                                 "2456",as.character(tbl_migration$Precipitation2)))

tbl_migration$period <- as.factor(tbl_migration$period)
tbl_migration$Month <- as.factor(tbl_migration$Month)
model_migration <- tbl_migration %>%
  select(TOTAL,Month,Year,Start,End,Duration,Observer,Wind_spd,Temp,Humidity,BARO,
         Cloud_Cover,Visibility,Precipitation2,
         Counter,experience_level,BV:UR,period)
#---------------------------------------------------------------------------------------------
# fit a poisson model using gam
#---------------------------------------------------------------------------------------------
pgam <- gam(TOTAL~log(Duration)+Start+period+Wind_spd+Temp+Humidity+BARO+Visibility+
              Precipitation2+Cloud_Cover+
              s(Counter, bs = "re")+s(experience_level, bs = "re"),
            data=model_migration, family=poisson, method="REML")
#---------------------------------------------------------------------------------------------
# residual diagnostic for poisson model
#---------------------------------------------------------------------------------------------
par(mfrow=c(2,2))
plot(predict(pgam,type="response"),residuals(pgam), xlab = "predicted value",
     ylab = "residuals",main="residuals vs predicted values")
plot(predict(pgam,type="response"),pgam$y, main="y vs predicted values");abline(0,1,col=2)
plot(pgam$linear.predictors,pgam$y, main="y vs linear predictors")
qq.gam(pgam,rep=20,level=1, main="qq plot")
#---------------------------------------------------------------------------------------------
# fit a full negative binomial model using gam
#---------------------------------------------------------------------------------------------
nbgam1 <- gam(TOTAL~log(Duration)+period+Wind_spd+Temp+Humidity+BARO+Visibility+
                Precipitation2+Cloud_Cover+
                s(Counter, bs = "re")+s(experience_level, bs = "re"),
              data=model_migration, family=nb, method="REML")
# residual diagnostic for full negative binomial model
par(mfrow=c(2,2))
plot(predict(nbgam1,type="response"),residuals(nbgam1), xlab = "predicted value",
     ylab = "residuals",main="residuals vs predicted values")
plot(predict(nbgam1,type="response"),nbgam1$y, main="y vs predicted values");
abline(0,1,col=2)
plot(nbgam1$linear.predictors,nbgam1$y, main="y vs linear predictors")
qq.gam(nbgam1,rep=20,level=1, main="qq plot")
#---------------------------------------------------------------------------------------------
# check zero inflation for full model
#---------------------------------------------------------------------------------------------
thb <- nbgam1$family$getTheta(TRUE)
nzeros<-numeric()
for (i in 1:100){
  simcounts<-rnbinom(size=thb, n=length(nbgam1$residuals),mu=predict(nbgam1,type="response")) 
  nzeros<-c(nzeros, sum(simcounts==0))
}
hist(nzeros); mean(nzeros);sum(model_migration$TOTAL==0)
#---------------------------------------------------------------------------------------------
# negative bonomial model selection
#---------------------------------------------------------------------------------------------
nbgam2 <- gam(TOTAL~log(Duration)+period+Wind_spd+Cloud_Cover+Temp+BARO+Visibility+
                Precipitation2+
                s(Counter, bs = "re")+s(experience_level, bs = "re"),
              data=model_migration, family=nb, method="REML") # drop Humidity
nbgam3 <- gam(TOTAL~log(Duration)+period+Wind_spd+Temp+Humidity+BARO+Visibility+
                Precipitation2+
                s(Counter, bs = "re")+s(experience_level, bs = "re"),
              data=model_migration, family=nb, method="REML") # drop Cloud Cover
nbgam4 <- gam(TOTAL~log(Duration)+period+Wind_spd+Temp+Humidity+Visibility+Precipitation2+
                Cloud_Cover+
                s(Counter, bs = "re")+s(experience_level, bs = "re"), # drop BARO
              data=model_migration, family=nb, method="REML")
nbgam5 <- gam(TOTAL~log(Duration)+period+Wind_spd+Temp+Humidity+BARO+Visibility+Cloud_Cover+
                s(Counter, bs = "re")+s(experience_level, bs = "re"),
              data=model_migration, family=nb, method="REML")   # drop Precipitation
nbgam6 <- gam(TOTAL~log(Duration)+period+Wind_spd+Humidity+BARO+Visibility+Precipitation2+
                Cloud_Cover+
                s(Counter, bs = "re")+s(experience_level, bs = "re"),
              data=model_migration, family=nb, method="REML")  # drop Temp
nbgam7 <- gam(TOTAL~log(Duration)+period+Wind_spd+Temp+Humidity+BARO+Precipitation2+
                Cloud_Cover+
                s(Counter, bs = "re")+s(experience_level, bs = "re"),
              data=model_migration, family=nb, method="REML") # drop Visibility

aic.vec1<-(c(AIC(nbgam1),AIC(nbgam2), AIC(nbgam3), AIC(nbgam4),
             AIC(nbgam5),AIC(nbgam6), AIC(nbgam7)))
plot(aic.vec1, ylab="AIC", xlab="model", main = "AIC for different models")
# use model nagam3 according to AIC
#---------------------------------------------------------------------------------------------
# calcute the AIC value for dropped random effects models
#---------------------------------------------------------------------------------------------
rdgam1 <- gam(TOTAL~log(Duration)+period+Wind_spd+Temp+Humidity+BARO+Visibility+
                Precipitation2+
                s(experience_level, bs = "re"),
              data=model_migration, family=nb, method="REML")
# drop random effects experience_level
rdgam2 <- gam(TOTAL~log(Duration)+period+Wind_spd+Temp+Humidity+BARO+Visibility+
                Precipitation2+
                s(Counter, bs = "re"),
              data=model_migration, family=nb, method="REML") 
# drop random effects Counter
aic.vec2<-(c(AIC(rdgam1),AIC(rdgam2)))

summary(nbgam3)
#---------------------------------------------------------------------------------------------
# get the standard deviation of random effects
#---------------------------------------------------------------------------------------------
gam.vcomp(nbgam3)
#---------------------------------------------------------------------------------------------
# get the coefficients in summary
#---------------------------------------------------------------------------------------------
coefficient <- round(exp(coefficients(nbgam3)[2:10]),3)
#log(Duration)       periodSpring           Wind_spd               Temp 
#15.34               0.96               1.10               1.05 
#Humidity               BARO         Visibility    Precipitation21 Precipitation22456 
#1.01               1.58               1.02               0.59               0.23 
#Precipitation23 
#1.07 

# output the coefficient and summary result to latex
stargazer(coefficient, title = "Transformed coefficients for fixed effect variables")
stargazer(nbgam3, title  = "Regression results")

# get confidence interval for fixed effect variables in nbgam3
creatCoeftab = function(GAM, GAMrange) {
  bGAM = coef(GAM)[GAMrange]
  seGAM = diag(vcov(GAM))[GAMrange]
  nms = names(bGAM)
  df = data.frame(model = rep("mgcv::gam", each = length(bGAM)),
                  term = rep(nms, 2),
                  estimate = unname(bGAM))
  df = transform(df,
                 upper = estimate + sqrt(seGAM),
                 lower = estimate - sqrt(seGAM))
  
}
m1.coefs = creatCoeftab(nbgam3, GAMrange = 2:11)
ggplot() + 
  geom_pointrange(data= m1.coefs, aes(x = estimate, y = term, xmax = upper,
                                      xmin = lower))+
  ggtitle("Confidence interals for fixed effect preditor variables")
#---------------------------------------------------------------------------------------------
# analyze the effect of weather condition on different groups of raptors
#---------------------------------------------------------------------------------------------
model_migration$eagles <- model_migration$BE+model_migration$GE+model_migration$UE
model_migration$falcons <- model_migration$AK+model_migration$PG+model_migration$UF+
  model_migration$ML
model_migration$buzzards <- model_migration$BV+model_migration$TV+model_migration$UB
model_migration$hawks <- model_migration$TOTAL-model_migration$eagles-
  model_migration$falcons-model_migration$buzzards

nbgam_hawks <- gam(hawks~log(Duration)+period+Wind_spd+Temp+Humidity+BARO+Visibility+
                     Precipitation2+
                     s(Counter, bs = "re")+s(experience_level, bs = "re"), 
                   data=model_migration, family=nb, method="REML")
summary(nbgam_hawks)

coefficient1 <- round(exp(coefficients(nbgam_hawks)[2:11]),3)

par(mfrow=c(2,2))
plot(predict(nbgam_hawks,type="response"),residuals(nbgam_hawks), main="a")
plot(predict(nbgam_hawks,type="response"),nbgam_hawks$y, main="b");abline(0,1,col=2)
plot(nbgam_hawks$linear.predictors,nbgam_hawks$y, main="c")
qq.gam(nbgam_hawks,rep=20,level=1, main="d")


nbgam_eagles <- gam(eagles~log(Duration)+period+Wind_spd+Temp+Humidity+BARO+Visibility+
                      Precipitation2+
                      s(Counter, bs = "re")+s(experience_level, bs = "re"), 
                    data=model_migration, family=nb, method="REML")
summary(nbgam_eagles)

par(mfrow=c(2,2))
plot(predict(nbgam_eagles,type="response"),residuals(nbgam_eagles), main="a")
plot(predict(nbgam_eagles,type="response"),nbgam_eagles$y, main="b");abline(0,1,col=2)
plot(nbgam_eagles$linear.predictors,nbgam_eagles$y, main="c")
qq.gam(nbgam_eagles,rep=20,level=1, main="d")
coefficient2 <- round(exp(coefficients(nbgam_eagles)[2:11]),3)

nbgam_buzzards <- gam(buzzards~log(Duration)+period+Wind_spd+Temp+Humidity+BARO+Visibility+
                        Precipitation2+
                        s(Counter, bs = "re")+s(experience_level, bs = "re"), 
                      data=model_migration, family=nb, method="REML")
summary(nbgam_buzzards)
# check zero inflation for buzzards
thb <- nbgam_buzzards$family$getTheta(TRUE)
nzeros<-numeric()
for (i in 1:100){
  simcounts<-rnbinom(size=thb, n=length(nbgam_buzzards$residuals),
                     mu=predict(nbgam_buzzards,type="response")) 
  nzeros<-c(nzeros, sum(simcounts==0))
}
hist(nzeros); mean(nzeros);sum(model_migration$buzzards==0)

coefficient3 <- round(exp(coefficients(nbgam_buzzards)[2:11]),3)
par(mfrow=c(2,2))
plot(predict(nbgam_buzzards,type="response"),residuals(nbgam_buzzards), main="a")
plot(predict(nbgam_buzzards,type="response"),nbgam_buzzards$y, main="b");
abline(0,1,col=2)
plot(nbgam_buzzards$linear.predictors,nbgam_buzzards$y, main="c")
qq.gam(nbgam_buzzards,rep=20,level=1, main="d")


nbgam_falcons <- gam(falcons~log(Duration)+period+Wind_spd+Temp+Humidity+BARO+Visibility+
                       Precipitation2+
                       s(Counter, bs = "re")+s(experience_level, bs = "re"), 
                     data=model_migration, family=nb, method="REML")
summary(nbgam_falcons)
coefficient4 <- round(exp(coefficients(nbgam_falcons)[2:11]),3)

par(mfrow=c(2,2))
plot(predict(nbgam_falcons,type="response"),residuals(nbgam_falcons), main="a")
plot(predict(nbgam_falcons,type="response"),nbgam_falcons$y, main="b");abline(0,1,col=2)
plot(nbgam_falcons$linear.predictors,nbgam_falcons$y, main="c")
qq.gam(nbgam_falcons,rep=20,level=1, main="d")
stargazer(nbgam_hawks, nbgam_eagles, nbgam_falcons, nbgam_buzzards, 
          title = "Regression results 
          for four groups of raptors")


