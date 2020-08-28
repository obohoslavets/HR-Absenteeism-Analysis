library(tidyverse)
library(gridExtra)
library(reshape2)
#library(GGally)
library(modelr)
#library(MASS)
library(caret)
library(mboost)
library(partykit)
library(party)
library(randomForest)



hrdata = read.csv("D:/Data Analytics/absenteesm dataset/hrdata.csv")
hrdata[sapply(hrdata, is.character)] <- lapply(hrdata[sapply(hrdata, is.character)], as.factor)

#1.Data Visualization
dim(hrdata)
str(hrdata)
head(hrdata)
summary(hrdata)


p1 <- ggplot(hrdata, aes(x = Division))+
  geom_bar()+
  theme_bw(base_size = 12) +
  theme(axis.text.x = element_text(angle = 40, hjust = 1))
p2 <- hrdata[hrdata[, "Division"]!="Stores",] %>%
  ggplot(aes(x = Division))+
  geom_bar()+
  theme_bw(base_size = 12) +
  theme(axis.text.x = element_text(angle = 40, hjust = 1))
grid.arrange(p1, p2, nrow = 1, top = "Counts of emploees by Division")
#Majority of the emploees work in the Store Division

ggplot(hrdata, aes(y = StoreLocation))+
  geom_bar()+
  ggtitle("Counts of emploees by Store Location")+
  theme_bw(base_size = 13) +
  ylab("Store Location")

ggplot(hrdata, aes(y = DepartmentName))+
  geom_bar()+
  ggtitle("Counts of emploees by Department Name")+
  theme_bw(base_size = 13) +
  ylab("Department Name")

#Density plots of numeric variables
ggplot(hrdata, aes(x=Age))+
  geom_histogram(aes(y=..density..),
                 binwidth=.5,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")+
  ggtitle("Density plot of Age")+
  xlab("Age")+
  ylab("Density")

ggplot(hrdata, aes(x=LengthService))+
  geom_histogram(aes(y=..density..),
                 binwidth=.5,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")+
  ggtitle("Density plot of Length of Service")+
  xlab("Length of Service")+
  ylab("Density")

ggplot(hrdata, aes(x=AbsentHours))+
  geom_histogram(aes(y=..density..),
                 binwidth=2,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")+
  ggtitle("Density plot of Absent Hours")+
  xlab("Absent Hours")+
  ylab("Density")

#Absenteeism vs Other Variables
ggplot(hrdata)+
  geom_boxplot(aes(x = AbsentHours, y = Division))+
  ggtitle("Absenteeism by Division")+
  theme_bw(base_size = 13) +
  xlab("Absent Hours")
#Store Division has the biggest number of outliers due to the number of people working there

ggplot(hrdata)+
  geom_boxplot(aes(y = reorder(DepartmentName, AbsentHours, FUN = "length"),x = AbsentHours))+
  ggtitle("Absenteeism by Division, ordered by number of data points per group")+
  theme_bw(base_size = 10) +
  xlab("Absent Hours")+
  ylab("Department")
#There are differences in the Absent Hours among the Departments

ggplot(hrdata)+
  geom_boxplot(aes(y = reorder(JobTitle, AbsentHours, FUN = "length"),x = AbsentHours))+
  ggtitle("Absenteeism by Job Title")+
  theme_bw(base_size = 10) +
  xlab("Absent Hours")+
  ylab("Job Title") 
#There are differences in the Absent Hours among the Job Titles

ggplot(hrdata)+
  geom_boxplot(aes(y = StoreLocation,x = AbsentHours))+
  ggtitle("Absenteeism by Store Location")+
  theme_bw(base_size = 10) +
  xlab("Absent Hours")+
  ylab("Store Location") 
#There are differences in the Absent Hours among the Store Locations

#Is there gender differences for absenteeism?
ggplot(hrdata)+
  geom_boxplot(aes(x = AbsentHours, y = Division, fill = Gender))+
  ggtitle("Absenteeism by Division")+
  theme_bw(base_size = 13) +
  xlab("Absent Hours")

ggplot(hrdata)+
  geom_boxplot(aes(y = reorder(DepartmentName, AbsentHours, FUN = "length"),x = AbsentHours, fill = Gender))+
  ggtitle("Absenteeism by Division, ordered by number of data points per group")+
  theme_bw(base_size = 10) +
  xlab("Absent Hours")+
  ylab("Department")

ggplot(hrdata)+
  geom_boxplot(aes(y = reorder(JobTitle, AbsentHours, FUN = "length"),x = AbsentHours, fill = Gender))+
  ggtitle("Absenteeism by Job Title")+
  theme_bw(base_size = 10) +
  xlab("Absent Hours")+
  ylab("Job Title") 

ggplot(hrdata)+
  geom_boxplot(aes(y = StoreLocation,x = AbsentHours, fill = Gender))+
  ggtitle("Absenteeism by Store Location")+
  theme_bw(base_size = 10) +
  xlab("Absent Hours")+
  ylab("Store Location") 

hrdata1 <- hrdata %>%
  group_by(Gender)%>%
  mutate(grp.mean = mean(AbsentHours))%>%
  ungroup()
ggplot(hrdata1, aes(x = AbsentHours, fill = Gender))+
  geom_density(alpha = 0.5)+
  geom_vline(aes(xintercept = grp.mean, color = Gender), 
             linetype = "dashed")+
  ggtitle("Density plot of absenteeism by Gender")+
  theme_bw(base_size = 12) +
  xlab("Absent Hours")+
  ylab("Density")
#Males are on average 10 hours less absent compared to women

#Is there age differences for absenteeism?
ggplot(hrdata, aes(x = Age, y = AbsentHours))+
  geom_point(alpha = 1/10)+
  ggtitle("Absenteeism by Age")+
  theme_bw(base_size = 14) +
  xlab("Age")+
  ylab("Absent Hours")

#It is still hard to see how many data points are there in the cluster, let's visualize the number of cases in each rectangle of space
ggplot(hrdata, aes(x = Age, y = AbsentHours))+
  geom_bin2d()+
  ggtitle("Absenteeism by Age")+
  theme_bw(base_size = 14) +
  xlab("Age")+
  ylab("Absent Hours")
#Oleder people tend to have more absent hours

#Is there a difference in absent hours depending on length of service?
ggplot(hrdata, aes(x = LengthService, y = AbsentHours))+
  geom_bin2d()+
  ggtitle("Absenteeism by Length of Service")+
  theme_bw(base_size = 14) +
  xlab("Length of Service")+
  ylab("Absent Hours")

#Is there relationship between the Age and Length of Service
ggplot(hrdata, aes(x = Age, y = LengthService))+
  geom_point(aes(color = Division), alpha = 0.4)+
  ggtitle("Age vs Length of Service by Division")+
  theme_bw(base_size = 14) +
  xlab("Age")+
  ylab("Length of Service")
#Let's take a closer look at this relationship for Store Division
ggplot(hrdata[hrdata[, "Division"]=="Stores",], aes(x = Age, y = LengthService, size=AbsentHours))+
  geom_point(alpha = 0.1)+
  ggtitle("Absenteeism by Age")+
  theme_bw(base_size = 14) +
  xlab("Age")+
  ylab("Length of Service")
#The chart clearly shows that employees of the Store Division do not stay there over 10 years 
#therefore in the previous chart they are all clustered below 10 years colored in pink
#There appears to be no corelation between Age and Length of Service because points are randomly distributed along the axes
#Finally young people tend to have less absent hours which we saw on the previous charts

#Let's take a closer look at other Divisions
ggplot(hrdata[hrdata[, "Division"]!="Stores",], aes(x=Age, y=LengthService, size = AbsentHours, color=Division)) +
  geom_point(alpha=0.7)+
  ggtitle("Absenteeism by Division, Age and Length of Service (without Store)")+
  theme_bw(base_size = 12) +
  xlab("Age")+
  ylab("Length of Service")
#There are differences by Divisions, Age and Length of Service

#Model Building

#Absenteeism by Gender
AnovaModel_1 <- lm(AbsentHours~Gender, data = hrdata)
anova(AnovaModel_1)

#Absenteeism by City
AnovaModel_2 <- lm(AbsentHours~City, data = hrdata)
anova(AnovaModel_2)

#Absenteeism by JobTitle
AnovaModel_3 <- lm(AbsentHours~JobTitle, data = hrdata)
anova(AnovaModel_3)


#Absenteeism by DepartmentName
AnovaModel_4 <- lm(AbsentHours~DepartmentName, data = hrdata)
anova(AnovaModel_4)


#Absenteeism by StoreLocation
AnovaModel_5 <- lm(AbsentHours~StoreLocation, data = hrdata)
anova(AnovaModel_5)


#Absenteeism by Division
AnovaModel_6 <- lm(AbsentHours~Division, data = hrdata)
anova(AnovaModel_6)


#Absenteeism by Age
AnovaModel_7 <- lm(AbsentHours~Age, data = hrdata)
anova(AnovaModel_7)


#Absenteeism by LengthService
AnovaModel_8 <- lm(AbsentHours~LengthService, data = hrdata)
anova(AnovaModel_8)

#Building Models
#Prepare Data
hrdata_modeling <- hrdata[,c("Gender", "Division", "Age", "LengthService", "AbsentHours")]
#training/test split
set.seed(3456)
trainIndex <- createDataPartition(hrdata_modeling$AbsentHours, p = .8, 
                                  list = FALSE, 
                                  times = 1)

hrdataTrain <- hrdata_modeling[ trainIndex,]
hrdataTest  <- hrdata_modeling[-trainIndex,]

#Each model will be cross-validated 5 times
glmFit1 <- train(AbsentHours ~ ., data = hrdataTrain, 
                 trControl = trainControl(method = "cv", number = 5),
                 method = "glm",
                 metric = "RMSE")
glmFit1

glmboostFit1 <- train(AbsentHours ~ ., data = hrdataTrain, 
                      trControl = trainControl(method = "cv", number = 5),
                      method = "glmboost",
                      metric = "RMSE")
glmboostFit1

knnFit1 <- train(AbsentHours ~ ., data = hrdataTrain, 
                 trControl = trainControl(method = "cv", number = 5),
                 method = "knn",
                 metric = "RMSE")
knnFit1
plot(knnFit1)

blackboostFit1 <- train(AbsentHours ~ ., data = hrdataTrain, 
                     trControl = trainControl(method = "cv", number = 5),
                     method = "blackboost",
                     metric = "RMSE")
blackboostFit1

rfFit1 <- train(AbsentHours ~ ., data = hrdataTrain, 
                trControl = trainControl(method = "cv", number = 5),
                method = "rf",
                metric = "RMSE")
rfFit1

resamps <- resamples(list(GLM=glmFit1,
                          GLMboost=glmboostFit1,
                          KNN=knnFit1,
                          BT=blackboostFit1,
                          RF=rfFit1))

resamps
summary(resamps)
dotplot(resamps, metric = "Rsquared")
dotplot(resamps, metric = "RMSE")
#Best performing model based on RMSE and Rsquared is Boosted tree model

#Now let's predict Test set which plays a role of new data and see how our final model performs
FinalPred <- predict(blackboostFit1, newdata = hrdataTest)
hrdataTest <- bind_cols(hrdataTest, FinalPred)
hrdataTest <- rename(hrdataTest, c(Predicted=...6,Actual=AbsentHours))

fit <- lm(Actual~Predicted, data = hrdataTest)
summary(fit)
#The final model explains 78% of the variability of the new data
plot(fit)


