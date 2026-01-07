#install packages 
install.packages("nortest") 
install.packages("ggpubr") 
install.packages("patchwork") 
install.packages("rsample") 
install.packages("rlang") 
install.packages("effects") 
install.packages("vcd") 
install.packages("rpart.plot") 
install.packages("rattle") 
install.packages("randomForest") 
install.packages("brant") 
install.packages("ordinal") 
install.packages("writexl") 
install.packages("read_excel")
#libraries 
library(read_excel)
library(tidyverse) #for dyplr and data minipulation 
library(nortest) #for doing anderson darling tests 
library(ggpubr) #for density plots 
library(patchwork) #for showing multiple graphs at once 
library(car) #for making QQ Plots 
library(corrplot) #for making multicollinarity 
library(MASS) #used for logisitc regression 
library(rsample)#Used for stratified sampling 
library(rlang)#Told needed it because of error 
library(caret)#Needed for various ML methods 
library(plyr)#Used for progress bar 
library(effects)#Used for plots on logistic regression 
library(rpart) #Used for decision trees 
library(rpart.plot) #Used for decsion trees 
library(randomForest) #Used for random forests 
library(lubridate) 
library(naivebayes) 
library(brant) 
library(ordinal) 
library(class) 
library(writexl) 
library("readxl")
library("sqldf")
library(TTR)
######################################Import Data############################### 

#Import all 5 years of GPS Training Data 
Trainingdata1819<-read_excel("GPS Data Export (2018-23) (De-Identified).xlsx", "2018-19") 
Trainingdata1920<-read_excel("GPS Data Export (2018-23) (De-Identified).xlsx", "2019-20") 
Trainingdata2021<-read_excel("GPS Data Export (2018-23) (De-Identified).xlsx", "2020-21") 
Trainingdata2122<-read_excel("GPS Data Export (2018-23) (De-Identified).xlsx", "2021-22") 
Trainingdata2223<-read_excel("GPS Data Export (2018-23) (De-Identified).xlsx", "2022-23") 

#Import all 5 years of injury data 

Injurydata1819 <- read_excel("Injury Data 2018-23 (De-Identified).xlsx", "2018-19") 
Injurydata1920 <- read_excel("Injury Data 2018-23 (De-Identified).xlsx", "2019-20") 
Injurydata2021 <- read_excel("Injury Data 2018-23 (De-Identified).xlsx", "2020-21") 
Injurydata2122 <- read_excel("Injury Data 2018-23 (De-Identified).xlsx", "2021-22") 
Injurydata2223 <- read_excel("Injury Data 2018-23 (De-Identified).xlsx", "2022-23") 

######################################Data Cleaning#################################### 
#Use same code for all Training (just swap dates)

Trainingdata2223copy=sqldf('SELECT
PlayerID,
Date,
SUM(`Total Duration`) AS Tot_Dur,
SUM(`Total Distance (m)`) AS Tot_Dis,
AVG(`Meterage Per Minute`) AS MPM,
SUM(`Velocity Band 3 Total Distance (m)`) AS VBTD,
SUM(`High Speed Running Distance (m)`) AS HSRD,
SUM(`Sprint Distance (m)`) AS Spt_Dis,
SUM(`Sprint Total Efforts`) as Spt_Tot_Eff,
AVG(`% Max Velocity`) AS Max_Vel,
SUM(`Total # Accels`) AS Accels,
SUM(`Accelerations (>3m/s/s)`) AS Accl3ms,
SUM(`IMA Accel High`) AS IMA_Accels,
SUM(`Total # Decels`) AS Decels,
SUM(`Decelerations (>3m/s/s)`) AS Decel3ms,
SUM(`IMA Decel High`) AS IMA_Decels, 
SUM(`Total Player Load`) AS TPL
                           FROM Trainingdata2223
                           Group BY PlayerID, Date')


#Use same code for all Training (just swap dates) 
#Make copy of data file 
Injurydata1920copy <- Injurydata1920

Injurydata1920copy <- Injurydata1920copy %>%
  mutate(DaybeforeInjury= Date-days(1))

Trainingdata2223copy <- Trainingdata2223copy %>%
  mutate(
    Month = month(Date),
    Season = case_when(
      Month >= 8 & Month <= 12 ~ "Before Christmas",
      Month >= 1 & Month <= 5 ~ "After Christmas",
      TRUE ~ "Pre Season"
    )
  ) %>%
  dplyr::select(-Month) # Remove the temporary Month column


# Set the number of periods for acute and chronic EMAs
n_acute <- 7
n_chronic <- 28

# Calculate the acute and chronic EMAs
Trainingdata2223copy$Acute_EMA_Work <- EMA(Trainingdata2223copy$TPL, n = n_acute)
Trainingdata2223copy$Chronic_EMA_Work <- EMA(Trainingdata2223copy$TPL, n = n_chronic)

# Calculate the acute and chronic work ratios
Trainingdata2223copy$Acute_Work_Ratio <- Trainingdata2223copy$TPL / Trainingdata2223copy$Acute_EMA_Work
Trainingdata2223copy$Chronic_Work_Ratio <- Trainingdata2223copy$TPL / Trainingdata2223copy$Chronic_EMA_Work



#Put Training as Complete 
Trainingdata2223Complete <- Trainingdata2223copy 

#Merge all 5 seasons together 
TrainingAll5Seasons <- rbind(Trainingdata1819Complete, Trainingdata1920Complete, 
                             Trainingdata2021Complete,Trainingdata2122Complete,Trainingdata2223Complete) 

#Put Injury as Complete 
Injurydata1920Complete <- Injurydata1920copy 

#Merge all 5 seasons together 
InjuryAll5Seasons <- rbind(Injurydata1819Complete, Injurydata1920Complete, 
                             Injurydata2021Complete,Injurydata2122Complete,Injurydata2223Complete)

#Rename some columns as don't want spaces for data frame manipulation
InjuryAll5Seasons <- InjuryAll5Seasons %>%
  dplyr::rename(
    "MuscleStrain" = `Muscle Strain (Y/N)`,"InjuryType" = `Contact/Non-Contact` , "DaysLost" = `Days Lost` ,
    "InjuryStatus" = `Injury Status`, "InjuryLocation" = `Injury Location`)

#Make backup as hard to make Training & Injury All 5 Seasons 
TrainingAll5SeasonsBackup <- TrainingAll5Seasons 
InjuryAll5SeasonsBackup <- InjuryAll5Seasons 

# Joining the Training and Injury Data
MasterData5Seasons=sqldf("SELECT TrainingAll5Seasons.*,InjuryAll5Seasons.InjuryLocation,
                         InjuryAll5Seasons.InjuryType,InjuryAll5Seasons.MuscleStrain,InjuryAll5Seasons.DaysLost,
                         InjuryAll5Seasons.InjuryStatus
                         FROM TrainingAll5Seasons
                         LEFT JOIN InjuryAll5Seasons 
                         ON TrainingAll5Seasons.PlayerID=InjuryAll5Seasons.PlayerID
                         AND TrainingAll5Seasons.Date=InjuryAll5Seasons.DaybeforeInjury")

MasterData5Seasons <- MasterData5Seasons %>%
  mutate(
       Load_type  = case_when(
      TPL >= 0 & TPL <= 375 ~ "Low",
      TPL > 375  & TPL <= 763.5 ~ "Medium",
      TRUE ~ "High"
    )
  ) 

#Make backup as hard to make MAster Data 
MasterData5SeasonsBackup <- MasterData5Seasons 


######################################Normalising data########################## 
#Normalise all the data except Injury Data 

#Create function 
normalize <- function(x) { 
  return ((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))) 
} 

MasterData5Seasons$Tot_Dur <- normalize(MasterData5Seasons$Tot_Dur) 
MasterData5Seasons$Tot_Dis <- normalize(MasterData5Seasons$Tot_Dis) 
MasterData5Seasons$MPM <- normalize(MasterData5Seasons$MPM) 
MasterData5Seasons$HSRD <- normalize(MasterData5Seasons$HSRD) 
MasterData5Seasons$Spt_Dis <- normalize(MasterData5Seasons$Spt_Dis) 
MasterData5Seasons$Spt_Tot_Eff <- normalize(MasterData5Seasons$Spt_Tot_Eff) 
MasterData5Seasons$Max_Vel <- normalize(MasterData5Seasons$Max_Vel) 
MasterData5Seasons$Accels <- normalize(MasterData5Seasons$Accels) 
MasterData5Seasons$Decels <- normalize(MasterData5Seasons$Decels) 
MasterData5Seasons$Accl3ms <- normalize(MasterData5Seasons$Accl3ms) 
MasterData5Seasons$Decel3ms <- normalize(MasterData5Seasons$Decel3ms) 
MasterData5Seasons$Acute_Work_Ratio <- normalize(MasterData5Seasons$Acute_Work_Ratio) 
MasterData5Seasons$Chronic_Work_Ratio <- normalize(MasterData5Seasons$Chronic_Work_Ratio)
 
######################################Summary Statistics######################## 
summary(MasterData5Seasons)
summary(TrainingAll5Seasons)
#Replacing NA with 0 
MasterData5Seasons[is.na(MasterData5Seasons)] <- 0
apply(TrainingAll5Seasons,2,sd,na.rm = TRUE) 
MasterData5Seasons$MuscleStrain <- ifelse(MasterData5Seasons$MuscleStrain %in% c("Y", "Yes"), 1, ifelse(MasterData5Seasons$MuscleStrain %in% c("N", "No"), 0, MasterData5Seasons$MuscleStrain))
InjuryAll5Seasons$MuscleStrain <- ifelse(InjuryAll5Seasons$MuscleStrain %in% c("Y", "Yes"), 1, ifelse(InjuryAll5Seasons$MuscleStrain %in% c("N", "No"), 0, InjuryAll5Seasons$MuscleStrain))
MasterData5Seasons$InjuryStatus <- ifelse(MasterData5Seasons$InjuryStatus %in% c("Injured"), 1,  MasterData5Seasons$InjuryStatus)
MasterData5Seasons$InjuryStatus <- as.numeric(as.character(MasterData5Seasons$InjuryStatus))
MasterData5Seasons$MuscleStrain <- as.numeric(as.character(MasterData5Seasons$MuscleStrain))
MasterData5Seasons$DaysLost <- as.numeric(as.character(MasterData5Seasons$DaysLost))

plot_intro(InjuryAll5Seasons)

Injtypestat=sqldf('SELECT
InjuryType,AVG(Tot_Dur),AVG(Tot_Dis),AVG(MPM),AVG(VBTD),AVG(HSRD),AVG(Spt_Dis),AVG(Spt_Tot_Eff),AVG(TPL)
                           FROM MasterData5Seasons
                           Group BY InjuryType')
Musclestat=sqldf('SELECT
MuscleStrain,AVG(Tot_Dur),AVG(Tot_Dis),AVG(MPM),AVG(VBTD),AVG(HSRD),AVG(Spt_Dis),AVG(Spt_Tot_Eff),AVG(TPL)
                           FROM MasterData5Seasons
                           Group BY MuscleStrain')

avg_days_lost <- InjuryAll5Seasons %>%
  group_by(InjuryLocation) %>%
  summarise(AvgDaysLost = mean(DaysLost))

#plot graphs
ggplot(MasterData5Seasons, aes(InjuryType, Tot_Dur)) + geom_violin(scale="area")
ggplot(MasterData5Seasons, aes(InjuryType, Tot_Dis)) + geom_violin(scale="area")
ggplot(MasterData5Seasons, aes(InjuryType, MPM)) + geom_violin(scale="area")
ggplot(MasterData5Seasons, aes(InjuryType, VBTD)) + geom_violin(scale="area")
ggplot(MasterData5Seasons, aes(InjuryType, HSRD))+ geom_violin(scale="area")
ggplot(MasterData5Seasons, aes(InjuryType, Spt_Dis)) + geom_violin(scale="area")
ggplot(MasterData5Seasons, aes(InjuryType, Spt_Tot_Eff)) + geom_violin(scale="area")
ggplot(MasterData5Seasons, aes(InjuryType, Accels)) + geom_violin(scale="area")
ggplot(MasterData5Seasons, aes(InjuryType, Decels)) + geom_violin(scale="area")
ggplot(MasterData5Seasons, aes(InjuryType, Acute_Work_Ratio)) + geom_violin(scale="area")
ggplot(MasterData5Seasons, aes(InjuryType, Chronic_Work_Ratio)) + geom_violin(scale="area")
ggplot(MasterData5Seasons, aes(InjuryType, TPL)) + geom_violin(scale="area")
ggplot(MasterData5Seasons, aes(Season, TPL)) + geom_violin(scale="area")
ggplot(InjuryAll5Seasons, aes(MuscleStrain ))+geom_bar()+labs(x = "Muscle Strain", y = "Count")+scale_x_discrete(labels = c("0" = "No", "1" = "Yes"))
ggplot(InjuryAll5Seasons, aes(InjuryType ))+geom_bar() +labs(x = "Injury Type", y = "Count")
ggplot(MasterData5Seasons, aes(MuscleStrain, Tot_Dur)) + geom_violin(scale="area")+scale_x_discrete(labels = c("0" = "No", "1" = "Yes"))
ggplot(MasterData5Seasons, aes(MuscleStrain, Tot_Dis)) + geom_violin(scale="area")+scale_x_discrete(labels = c("0" = "No", "1" = "Yes"))
ggplot(MasterData5Seasons, aes(MuscleStrain, MPM)) + geom_violin(scale="area")+scale_x_discrete(labels = c("0" = "No", "1" = "Yes"))
ggplot(MasterData5Seasons, aes(MuscleStrain, VBTD)) + geom_violin(scale="area")+scale_x_discrete(labels = c("0" = "No", "1" = "Yes"))
ggplot(MasterData5Seasons, aes(MuscleStrain, HSRD))+ geom_violin(scale="area")+scale_x_discrete(labels = c("0" = "No", "1" = "Yes"))
ggplot(MasterData5Seasons, aes(MuscleStrain, Spt_Dis)) + geom_violin(scale="area")+scale_x_discrete(labels = c("0" = "No", "1" = "Yes"))
ggplot(MasterData5Seasons, aes(MuscleStrain, Spt_Tot_Eff)) + geom_violin(scale="area")+scale_x_discrete(labels = c("0" = "No", "1" = "Yes"))
ggplot(MasterData5Seasons, aes(MuscleStrain, Accels)) + geom_violin(scale="area")+scale_x_discrete(labels = c("0" = "No", "1" = "Yes"))
ggplot(MasterData5Seasons, aes(MuscleStrain, Decels)) + geom_violin(scale="area")+scale_x_discrete(labels = c("0" = "No", "1" = "Yes"))
ggplot(MasterData5Seasons, aes(MuscleStrain, Acute_Work_Ratio)) + geom_violin(scale="area")+scale_x_discrete(labels = c("0" = "No", "1" = "Yes"))
ggplot(MasterData5Seasons, aes(MuscleStrain, Chronic_Work_Ratio)) + geom_violin(scale="area")+scale_x_discrete(labels = c("0" = "No", "1" = "Yes"))
ggplot(MasterData5Seasons, aes(MuscleStrain, TPL)) + geom_violin(scale="area")+scale_x_discrete(labels = c("0" = "No", "1" = "Yes"))
ggplot(MasterData5SeasonsMatch, aes(GD, TotalMatchWeek2)) + geom_violin(scale="area")


######################################Analysis##################################
######################################Normality Testing#########################
#Normality tests on variables using Anderson darling
#Convert factors back to numeric
ad.test(MasterData5Seasons$Tot_Dur)
ad.test(MasterData5Seasons$Tot_Dis)
ad.test(MasterData5Seasons$MPM)
ad.test(MasterData5Seasons$VBTD)
ad.test(MasterData5Seasons$HSRD)
ad.test(MasterData5Seasons$Spt_Dis)
ad.test(MasterData5Seasons$Spt_Tot_Eff)
ad.test(MasterData5Seasons$Accels)
ad.test(MasterData5Seasons$Accl3ms)
ad.test(MasterData5Seasons$Decels)
ad.test(MasterData5Seasons$Decel3ms)
ad.test(MasterData5Seasons$TPL)
ad.test(MasterData5Seasons$Acute_Work_Ratio)
ad.test(MasterData5Seasons$Chronic_Work_Ratio)

#Density plots of variables
g1<- ggdensity(MasterData5Seasons$Tot_Dur,
               main = "Density plot of Total Duration",
               xlab = "Total Duration") + font("title", size=8, color="black")
g2<-ggdensity(MasterData5Seasons$Tot_Dis,
              main = "Density plot of Total Distance",
              xlab = "Total Distance")+font("title", size=8, color="black")
g3<-ggdensity(MasterData5Seasons$MPM,
              main = "Density plot of Meterage Per Min",
              xlab = "Meterage Per Min")+font("title", size=8, color="black")
g4<-ggdensity(MasterData5Seasons$VBTD,
              main = "Density plot of Velocity Band 3 Total Distance",
              xlab = "Velocity Band 3 Total Distance")+font("title", size=8, color="black")
g5<-ggdensity(MasterData5Seasons$HSRD,
              main = "Density plot of High Speed Running Distance",
              xlab = "High Speed Running Distance")+font("title", size=8, color="black")
g6<-ggdensity(MasterData5Seasons$Spt_Dis,
              main = "Density plot of Sprint Distance",
              xlab = "Sprint Distance")+font("title", size=8, color="black")
g7<-ggdensity(MasterData5Seasons$Spt_Tot_Eff,
              main = "Density plot of Sprint Total Efforts",
              xlab = "Sprint Total Efforts")+font("title", size=8, color="black")
g8<-ggdensity(MasterData5Seasons$Accels,
              main = "Density plot of Accelerations",
              xlab = "Accelrations")+font("title", size=8, color="black")
g9<-ggdensity(MasterData5Seasons$Decels,
              main = "Density plot of Decelerations",
              xlab = "Decelerations")+font("title", size=8, color="black")
g10<-ggdensity(MasterData5Seasons$Acute_Work_Ratio,
              main = "Density plot of Acute Work Ratio",
              xlab = "Acute Work Ratio")+font("title", size=8, color="black")
g11<-ggdensity(MasterData5Seasons$Chronic_Work_Ratio,
              main = "Density plot of Chronic Work Ratio",
              xlab = "Chronic Work Ratio")+font("title", size=8, color="black")
g12<-ggdensity(MasterData5Seasons$TPL,
              main = "Density plot of Total Player Load",
              xlab = "Total Player Load")+font("title", size=8, color="black")
(g1 | g2 | g3) /
  (g4 | g5 | g6) /
  (g7 | g8 | g9) /
  (g10 | g11 | g12)
#remove graphs
rm(g1)
rm(g2)
rm(g3)
rm(g4)
rm(g5)
rm(g6)
rm(g7)
rm(g8)
rm(g9)
rm(g10)
rm(g11)
rm(g12)
par(mfrow=c(3,3))
#Q-Q plots of variables
q1<-qqPlot(MasterData5Seasons$Tot_Dur, main = "QQ plot of TotalDuration",
           ylab = "Total Duration Quantiles",
           xlab = "Theoretical Quantiles") +
  font("title", size=8, color="black")
q2<-qqPlot(MasterData5Seasons$Tot_Dis, main = "QQ plot of Total Distance",
           ylab = "Total Distance Quantiles",
           xlab = "Theoretical Quantiles") +
  font("title", size=8, color="black")
q3<-qqPlot(MasterData5Seasons$MPM, main = "QQ plot of Metreage Per Min",
           ylab = "Metreage Per Min Quantiles",
           xlab = "Theoretical Quantiles") +
  font("title", size=8, color="black")
q4<-qqPlot(MasterData5Seasons$VBTD, main = "QQ plot of Velocity Band 3 Total Distance",
           ylab = "Velocity Band 3 Total Distance Quantiles",
           xlab = "Theoretical Quantiles") +
  font("title", size=8, color="black")
q5<-qqPlot(MasterData5Seasons$HSRD, main = "QQ plot of High Speed Running Distance",
           ylab = "High Speed Running Distance Quantiles",
           xlab = "Theoretical Quantiles") +
  font("title", size=8, color="black")
q6<-qqPlot(MasterData5Seasons$Spt_Dis, main = "QQ plot of Sprint Distance",
           ylab = "Sprint Distance Quantiles",
           xlab = "Theoretical Quantiles") +
  font("title", size=8, color="black")
q7<-qqPlot(MasterData5Seasons$Spt_Tot_Eff, main = "QQ plot of Sprint Total Efforts",
           ylab = "Sprint Total Efforts Quantiles",
           xlab = "Theoretical Quantiles") +
  font("title", size=8, color="black")
q8<-qqPlot(MasterData5Seasons$Accels, main = "QQ plot of Accelerations",
           ylab = "Accelerations Quantiles",
           xlab = "Theoretical Quantiles") +
  font("title", size=8, color="black")
q9<-qqPlot(MasterData5Seasons$Decels, main = "QQ plot of Decelerations",
           ylab = "Decelerations Quantiles",
           xlab = "Theoretical Quantiles") +
  font("title", size=8, color="black")
q10<-qqPlot(MasterData5Seasons$Acute_Work_Ratio, main = "QQ plot of Acute Work Ratio",
           ylab = "Acute Work Ratio Quantiles",
           xlab = "Theoretical Quantiles") +
  font("title", size=8, color="black")
q11<-qqPlot(MasterData5Seasons$Chronic_Work_Ratio, main = "QQ plot of Chronic Work Ratio",
           ylab = "Chronic Work Ratio Quantiles",
           xlab = "Theoretical Quantiles") +
  font("title", size=8, color="black")
q12<-qqPlot(MasterData5Seasons$TPL, main = "QQ plot of Total Player Load",
           ylab = "Total Player Load Quantiles",
           xlab = "Theoretical Quantiles") +
  font("title", size=8, color="black")
(q1 | q2 | q3) /
  (q4 | q5 | q6) /
  (q7 | q8 | q9) /
  (q10 | q11 | q12)
#remove graphs
rm(q1)
rm(q2)
rm(q3)
rm(q4)
rm(q5)
rm(q6)
rm(q7)
rm(q8)
rm(q9)
rm(q10)
rm(q11)
rm(q12)
######################################Hypothesis Testing########################
#Kendall Rank Correlation test Injury Status
cor.test(MasterData5Seasons$Tot_Dur, MasterData5Seasons$InjuryStatus,method="kendall")
cor.test(MasterData5Seasons$Tot_Dis, MasterData5Seasons$InjuryStatus,method="kendall")
cor.test(MasterData5Seasons$MPM, MasterData5Seasons$InjuryStatus,method="kendall")
cor.test(MasterData5Seasons$VBTD, MasterData5Seasons$InjuryStatus,method="kendall")
cor.test(MasterData5Seasons$HSRD, MasterData5Seasons$InjuryStatus,method="kendall")
cor.test(MasterData5Seasons$Spt_Dis, MasterData5Seasons$InjuryStatus,method="kendall")
cor.test(MasterData5Seasons$Spt_Tot_Eff, MasterData5Seasons$InjuryStatus,method="kendall")
cor.test(MasterData5Seasons$Accels, MasterData5Seasons$InjuryStatus,method="kendall")
cor.test(MasterData5Seasons$Decels, MasterData5Seasons$InjuryStatus,method="kendall")
cor.test(MasterData5Seasons$Acute_Work_Ratio, MasterData5Seasons$InjuryStatus,method="kendall")
cor.test(MasterData5Seasons$Chronic_Work_Ratio, MasterData5Seasons$InjuryStatus,method="kendall")
cor.test(MasterData5Seasons$TPL, MasterData5Seasons$InjuryStatus,method="kendall")
######################################Regression################################
#Check for multicollinierity
#make a DF with only parameters showing
MasterData5SeasonsOnlyParam <- MasterData5Seasons[,c("Tot_Dur","Tot_Dis", "MPM", "HSRD", "Spt_Dis", "Spt_Tot_Eff",
           "Accels", "Decels", "Acute_Work_Ratio", "Chronic_Work_Ratio",
           "TPL", "MuscleStrain","InjuryStatus")]
par(mfrow=c(1,1))
corrplot(cor(MasterData5SeasonsOnlyParam,use="complete"), type="upper",method="number")
corrplot(cor(MasterData5SeasonsOnlyParam,use="complete"), type="upper")
#Multiple Regression
rm(MasterData5SeasonsOnlyParam)
install.packages("fastDummies")
library('fastDummies')
# Create dummy variables for the "Load type" column
MasterData5Seasons <- dummy_cols(MasterData5Seasons, select_columns =  "Load_type")
MasterData5Seasons <- subset(MasterData5Seasons1, select = -InjuryStatus)
#####################################Injury Status Regression############################
set.seed(200)
##Do stratfied sampling as not equal proportions of Injury
split <- rsample::initial_split(MasterData5Seasons, prop=0.7, strata=InjuryStatus)
train <- rsample::training(split)
test <- rsample::testing(split)
#test to see if stratified sampling done correctly
prop.table(table(train$InjuryStatus))
prop.table(table(test$InjuryStatus))
##Use Polr function for Ordinal logistic regression for Injury - Use training data - Using validation/training set
RegInj <- glm(InjuryStatus ~Tot_Dur+MPM+Spt_Dis+Acute_Work_Ratio+Chronic_Work_Ratio+Load_type_Low+Load_type_High+Load_type_Medium,data= train, family ="binomial")
summary(RegInj)
ctableInj <- coef(summary(RegInj))
pInj <- pnorm(abs(ctableInj[, "Pr(>|z|)"]), lower.tail = FALSE) * 2
ctableInj <- cbind(ctableInj, "p value" = pInj)
view(ctableInj)
#Test the data
predictInj <- predict(RegInj,test)
table(test$InjuryStatus, predictInj)
predictInj<- predict(RegInj, test, type="response")
view(predictInj)
sum(diag(table(test$InjuryStatus, predictInj))) / sum(table(test$InjuryStatus, predictInj))
plot(Effect(focal.predictors = "Tot_Dur",RegInj))
plot(Effect(focal.predictors = "MPM",RegInj))
plot(Effect(focal.predictors = "Spt_Dis",RegInj))
plot(Effect(focal.predictors = "Acute_Work_Ratio",RegInj))
plot(Effect(focal.predictors = "Chronic_Work_Ratio",RegInj))
plot(Effect(focal.predictors = "Chronic_Work_Ratio",RegInj))
plot(Effect(focal.predictors = "Load_type_Low",RegInj))
plot(Effect(focal.predictors = "Load_type_High",RegInj))
plot(Effect(focal.predictors = "Load_type_Medium",RegInj))

#Using Cross Validation fo 10 as quick validation
train.control <-trainControl(method="cv",number=10)
modelInj <-train(InjuryStatus~Tot_Dur+MPM+Spt_Dis+Acute_Work_Ratio+Chronic_Work_Ratio+Load_type_Low+Load_type_High+Load_type_Medium,
data=MasterData5Seasons,method = "glm",family = binomial(),trControl = train.control)
print(modelInj)
modelInj
predictInj <- predict(modelInj,test)
table(test$InjuryStatus, predictInj)
sum(diag(table(test$InjuryStatus, predictInj))) / sum(table(test$InjuryStatus, predictInj))

######################################Classifications###########################
######################################Decision Trees Injury Status ########
set.seed(200)
#Create the train/test set
split <- rsample::initial_split(MasterData5Seasons, prop=0.7, strata=InjuryStatus)
train <- rsample::training(split)
test <- rsample::testing(split)
#Check stratified correctly
prop.table(table(train$InjuryStatus))
prop.table(table(test$InjuryStatus))
#Build the model
#Make the tree
InjTree = train(InjuryStatus~Tot_Dur+MPM+Spt_Dis+Acute_Work_Ratio+Chronic_Work_Ratio+Load_type_Low+Load_type_High+Load_type_Medium, data=train, method="rpart",
                trControl = trainControl(method = "cv"),na.action = na.omit)
InjTree
summary(InjTree$finalModel)
#plot the tree
plot(InjTree$finalModel, uniform=TRUE,
     main="Classification Tree")
text(InjTree$finalModel, use.n = TRUE, all=TRUE, cex=0.8)
suppressMessages(library(rattle))
fancyRpartPlot(InjTree$finalModel)
#predict results
InjTreePredict = predict(InjTree,test)
view(InjTreePredict)
table(InjTreePredict, test$InjuryStatus)
errorrate = round(mean(InjTreePredict != test$InjuryStatus),2)
1 - errorrate
######################################Decision Trees Injury Status (ACC And DEC)##########
set.seed(200)
#Create the train/test set
split <- rsample::initial_split(MasterData5Seasons, prop=0.7, strata=InjuryStatus)
train <- rsample::training(split)
test <- rsample::testing(split)
#Check stratified correctly
prop.table(table(train$InjuryStatus))
prop.table(table(test$InjuryStatus))
#Build the model
#Make the tree
InjTree = train(InjuryStatus~Tot_Dur+MPM+Spt_Dis+Acute_Work_Ratio+Chronic_Work_Ratio+Load_type_Low+Load_type_High+Load_type_Medium+Accels+Decels+Accl3ms+Decel3ms, data=train, method="rpart",
                trControl = trainControl(method = "cv"),na.action = na.omit)
InjTree
summary(InjTree$finalModel)
#plot the tree
plot(InjTree$finalModel, uniform=TRUE,
     main="Classification Tree")
text(InjTree$finalModel, use.n = TRUE, all=TRUE, cex=0.8)
suppressMessages(library(rattle))
fancyRpartPlot(InjTree$finalModel)
#predict results
InjTreePredict = predict(InjTree,test)
InjTreePredict
table(InjTreePredict, test$InjuryStatus)
errorrate = round(mean(InjTreePredict != test$InjuryStatus),2)
1 - errorrate

######################################Random Forests Injury (No ACC or DEC)########
#make table only relevant variables
set.seed(200)
#Create the train/test set
split <- rsample::initial_split(MasterData5Seasons, prop=0.7, strata=InjuryStatus)
train <- rsample::training(split)
test <- rsample::testing(split)
table(train$InjuryStatus)
table(test$InjuryStatus)
RFInj = randomForest(InjuryStatus~Tot_Dur+MPM+Spt_Dis+Acute_Work_Ratio+Chronic_Work_Ratio+Load_type_Low+Load_type_High+Load_type_Medium, data=train, ntree=500)
RFInj
predInj = predict(RFInj, newdata=test)
table(predInj, test$InjuryStatus)
sum(diag(table(predInj, test$InjuryStatus))) / sum(table(predInj, test$InjuryStatus))
plot(RFInj)
varImpPlot(RFInj)
varImp(RFInj)
######################################Random Forests Injury (ACC And DEC)########
#make table only relevant variables
set.seed(200)
#Create the train/test set
split <- rsample::initial_split(MasterData5Seasons, prop=0.7, strata=InjuryStatus)
train <- rsample::training(split)
test <- rsample::testing(split)
table(train$InjuryStatus)
table(test$InjuryStatus)
RFInj = randomForest(InjuryStatus~Tot_Dur+MPM+Spt_Dis+Acute_Work_Ratio+Chronic_Work_Ratio+Load_type_Low+Load_type_High+Load_type_Medium+Accels+Decels+Accl3ms+Decel3ms, data=train, ntree=500)
RFInj$forest
fancyRpartPlot(RFInj$finalModel)
predInj = predict(RFInj, newdata=test)
table(predInj, test$InjuryStatus)
sum(diag(table(predInj, test$InjuryStatus))) / sum(table(predInj, test$InjuryStatus))
plot(RFInj)
varImpPlot(RFInj)
varImp(RFInj)


######################################KNN Injury(No ACC or DEC)######################
set.seed(200)
#Create the train/test set
split <- rsample::initial_split(MasterData5Seasons, prop=0.7, strata=InjuryStatus)
train <- rsample::training(split)
test <- rsample::testing(split)
trainNoInj<- subset(train, select=-c(8))
testNoInj<- subset(test, select=-c(8))
#Use sqrt as optimal k value
knnInj <- knn(train=trainNoInj, test=testNoInj, cl=train$Inj, k=101)
#Tot_During testing found k=5 is best
knnInj
cm<- table(test$Inj, knnInj)
cm
sum(diag(cm)) / sum(cm)
#find optimal k
i=1
k.optm=1
for (i in 1:200) {
  knnInj <- knn(train=trainNoInj, test=testNoInj, cl=train$Inj, k=i)
  k.optm[i] <- sum(diag(table(test$Inj, knnInj))) / sum(table(test$Inj, knnInj))
  k=i
  cat(k,'=',k.optm[i],'')
}
plot(k.optm, type="b", xlab="K- Value",ylab="Accuracy level")
######################################KNN Inj(ACC And DEC)######################
set.seed(200)
MasterData5Seasonsknn <- MasterData5Seasons %>%
  dplyr::ungroup() %>%
  dplyr::select(-c(Player,Activity,TrainingType,Date,Result,HA,GD,MinDate,MinDate2)) %>%
  na.omit()
#Create the train/test set
split <- rsample::initial_split(MasterData5SeasonsMatchVar, prop=0.7, strata=Inj)
train <- rsample::training(split)
test <- rsample::testing(split)
trainNoInj<- subset(train, select=-c(10))
testNoInj<- subset(test, select=-c(10))
#Use sqrt as optimal k value
knnInj <- knn(train=trainNoInj, test=testNoInj, cl=train$Inj, k=2)
#Tot_During testing found k=5 is best
knnInj
cm<- table(test$Inj, knnInj)
cm
sum(diag(cm)) / sum(cm)
i=1
k.optm=1
for (i in 1:200) {
  knnInj <- knn(train=trainNoInj, test=testNoInj, cl=train$Inj, k=i)
  k.optm[i] <- sum(diag(table(test$Inj, knnInj))) / sum(table(test$Inj, knnInj))
  k=i
  cat(k,'=',k.optm[i],'')
}
plot(k.optm, type="b", xlab="K- Value",ylab="Accuracy level")