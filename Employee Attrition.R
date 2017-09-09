#############################HR Analytics Solution###################
################################################################
#Business Understanding
#Data Understanding
#Data Preparation & EDA
#Model Building 
#Model Evaluation
################################################################

### Business Understanding:

# Based on the existing and past data,
# the company has maintained a database containing Employee information,
# such as Performance,Salary,Manager Feedback information,Log time,Attrition etc.

## AIM:

# The Company on an Average has an Attrition rate of 15% which is hampering it's growth
# The company wants to find out the factors which lead to this Attrition,so as to make
# appropriate changes.

################################################################

### Data Understanding

# Install and Load the required packages
#install.packages("MASS")
#install.packages("car")
#install.packages("e1071")
#install.packages("caret", dependencies = c("Depends", "Suggests"))
#install.packages("cowplot")
#install.packages("reshape")
#install.packages("stats")
#install.packages("ggthemes")
#install.packages("ROCR")
#install.packages("dplyr")
#install.packages("caTools")
#install.packages("BurStMisc")
#install.packages("ggthemes")

library(MASS)
library(car)
library(e1071)
library(caret)
library(ggplot2)
library(cowplot)
library(caTools)
library(reshape)
library(stats)
library(dplyr)
library(BurStMisc)
library(ROCR)

#Calacualte the average log time in the office

#Read the In_time and Out_time datasets

in_time<-read.csv('in_time.csv',stringsAsFactors = FALSE)
str(in_time)

out_time<-read.csv('out_time.csv',stringsAsFactors = FALSE)
str(out_time)

#Remove the columns when the day is Public holiday,as these dates
#contain NA values in IN_TIME variables:

in_time$X2015.01.01<-NULL
in_time$X2015.01.14<-NULL
in_time$X2015.01.26<-NULL
in_time$X2015.03.05<-NULL
in_time$X2015.05.01<-NULL

#Remove the columns when the day is Public holiday,as these dates
#contain NA values in OUT_TIME variables:

out_time$X2015.01.01<-NULL
out_time$X2015.01.14<-NULL
out_time$X2015.01.26<-NULL
out_time$X2015.03.05<-NULL
out_time$X2015.05.01<-NULL

sum(is.na(in_time))
sum(is.na(out_time))

#Convert the Character columns of each of the dataframe into POSIX variable 

in_time_pos <-sapply(in_time[,-1],as.POSIXlt.character)
out_time_pos<-sapply(out_time[,-1],as.POSIXlt.character)

#Calculating the number of hours an Employee spends in office  

Emp_logtime<-Map(difftime,out_time_pos,in_time_pos)
Emp_logtime<-sapply(Emp_logtime,round,2)

#Calculating the Average number of hours spend by each Employee 

Avg_logtime<-data.frame(rowMeans(Emp_logtime,na.rm = TRUE,dims = 1))

#Appending the Employee ID's with the Average number of Hours

Avg_logtime<-cbind(in_time$X,Avg_logtime$rowMeans.Emp_logtime..na.rm...TRUE..dims...1.)
colnames(Avg_logtime) <- c("Emp_ID","Avg Hours")

#Merging the new formed Avg_LogTime and the other Dataframes 
#General_Data,Employee_Data and Manager_Data

#Reading the 3 other Dataframes

General_Data<-read.csv('general_data.csv',stringsAsFactors = FALSE)
na_count_gen <-sapply(General_Data, function(y) sum(length(which(is.na(y)))))

#Replacing the NA values in the columns of General Data 

for(i in 1:ncol(General_Data)){
  General_Data[is.na(General_Data[,i]), i] <- mean(General_Data[,i], na.rm = TRUE)
}

Empl_Surv_Data<-read.csv('employee_survey_data.csv',stringsAsFactors = FALSE)
na_count_emp <-sapply(Empl_Surv_Data, function(y) sum(length(which(is.na(y)))))

#Replacing the NA values in the columns of Empl_Surv_Data

for(i in 1:ncol(Empl_Surv_Data)){
  Empl_Surv_Data[is.na(Empl_Surv_Data[,i]), i] <- mean(Empl_Surv_Data[,i], na.rm = TRUE)
}

Mana_Surv_Data<-read.csv('manager_survey_data.csv',stringsAsFactors = FALSE)
na_count_mana <-sapply(Mana_Surv_Data, function(y) sum(length(which(is.na(y)))))

length(unique(tolower(General_Data$Employee.ID))) # 4410, confirming EmployeeID is key 
length(unique(tolower(Empl_Surv_Data$EmployeeID)))# 4410, confirming EmployeeID is key
length(unique(tolower(Mana_Surv_Data$EmployeeID)))# 4410, confirming EmployeeID is key

setdiff(General_Data$Employee.ID,Empl_Surv_Data$EmployeeID) # Identical EmployeeID across these datasets
setdiff(General_Data$Employee.ID,Mana_Surv_Data$EmployeeID) # Identical EmployeeID across these datasets

#Merging the dataframe by Employee Data

Emp_merge_data1<-merge(General_Data,Empl_Surv_Data,by.x = c("Employee.ID"),by.y = c("EmployeeID"))
Mana_merge_data<-merge(Emp_merge_data1,Mana_Surv_Data,by.x = c("Employee.ID"),by.y = c("EmployeeID"))
Final_merge_data<-merge(Mana_merge_data,Avg_logtime,by.x = c("Employee.ID"),by.y = c("Emp_ID"))

#Rounding off the Average Hours to 2 decimal places

Final_merge_data$`Avg Hours`<-round(Final_merge_data$`Avg Hours`,2)
str(Final_merge_data)

#Calculating and Appending the leaves columns to the merged dataset

Final_merge_data$leave_avail<- apply(in_time, 1, function(x) sum(is.na(x)))

#Understanding the Data

bar_theme1<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                   legend.position="none")
box_theme<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                   legend.position="none")

k1<-range(Final_merge_data$`Avg Hours`)
plot_grid(ggplot(Final_merge_data, aes(x=`Avg Hours`))+ geom_point(stat = "count"))

#Plotting the Categorical Variables against the Dependenet Variable.

title <- ggdraw() + draw_label("Categorical Variables Plot Vs Attrition", fontface='bold')

Cat_plot<- plot_grid(ggplot(Final_merge_data, aes(x=BusinessTravel,fill=Attrition))+ geom_bar() + bar_theme1, 
                     ggplot(Final_merge_data, aes(x=Department,fill=Attrition))+ geom_bar()+bar_theme1,
                     ggplot(Final_merge_data, aes(x=Education.Field,fill=Attrition))+ geom_bar()+bar_theme1,
                     ggplot(Final_merge_data, aes(x=Job.Role,fill=Attrition))+ geom_bar()+bar_theme1,
                     align = "h")
Cat_title_plot<-plot_grid(title,Cat_plot,ncol=1, rel_heights=c(0.1, 1))
Cat_title_plot

cat_plot1<-plot_grid(ggplot(Final_merge_data, aes(x=Final_merge_data$Education,fill=Attrition))+ geom_histogram() +bar_theme1,
                     ggplot(Final_merge_data, aes(x=Final_merge_data$Job.Level,fill=Attrition))+ geom_histogram()+bar_theme1,
                     ggplot(Final_merge_data, aes(x=StockOptionLevel,fill=Attrition))+ geom_histogram()+bar_theme1,
                     ggplot(Final_merge_data, aes(x=MaritalStatus,fill=Attrition))+ geom_bar()+bar_theme1,
                     ggplot(Final_merge_data, aes(x=Gender,fill=Attrition))+ geom_bar()+bar_theme1,align = 'h')

Cat_title_plot1<-plot_grid(title,cat_plot1,ncol=1, rel_heights=c(0.1, 1))
Cat_title_plot1
          
#Plotting the Continuous Variables Against Attrition:

title_Cont <- ggdraw() + draw_label("Continuous Variables Plot", fontface='bold')

Cont_plot<-plot_grid(ggplot(Final_merge_data, aes(x=Age,fill=Attrition))+ geom_histogram()+
                     scale_x_continuous(name = "Age", breaks = seq(18,60,6), limits = c(18,60))  , 
                     ggplot(Final_merge_data, aes(x=Distance.From.Home,fill=Attrition))+ geom_histogram()+bar_theme1,
                     ggplot(Final_merge_data, aes(x=PercentSalaryHike,fill=Attrition))+ geom_histogram()+
                     scale_x_continuous(name = "Salary Hike", breaks = seq(11,25,2), limits = c(11,25))+ bar_theme1,
                     ggplot(Final_merge_data, aes(x=`Avg Hours`,fill=Attrition))+ geom_histogram()+bar_theme1)
Con_title_plot<-plot_grid(title,Cont_plot,ncol=1, rel_heights=c(0.1, 1))
Con_title_plot


Cont_plot_2<-plot_grid(ggplot(Final_merge_data, aes(x=Final_merge_data$WorkLifeBalance,fill=Attrition))+ geom_histogram()+bar_theme1,
                       ggplot(Final_merge_data, aes(x=Final_merge_data$NumCompaniesWorked,fill=Attrition))+ geom_histogram()+bar_theme1,
                       ggplot(Final_merge_data, aes(x=Final_merge_data$TrainingTimesLastYear,fill=Attrition))+ geom_histogram()+bar_theme1,
                       ggplot(Final_merge_data, aes(x=Final_merge_data$YearsSinceLastPromotion,fill=Attrition))+ geom_histogram()+bar_theme1,
                       align = "h")
Con_title_plot2<-plot_grid(title,Cont_plot_2,ncol=1, rel_heights=c(0.1, 1))
Con_title_plot2

Cont_plot_3<-plot_grid(ggplot(Final_merge_data, aes(x=leave_avail,fill=Attrition))+ geom_histogram()+bar_theme1,
                       ggplot(Final_merge_data, aes(x=MonthlyIncome,fill=Attrition))+ geom_histogram()+bar_theme1,
                       ggplot(Final_merge_data, aes(x=TotalWorkingYears,fill=Attrition))+ geom_histogram()+bar_theme1,
                       ggplot(Final_merge_data, aes(x=YearsAtCompany,fill=Attrition))+ geom_histogram()+bar_theme1,
                       ggplot(Final_merge_data, aes(x=YearsWithCurrManager,fill=Attrition))+ geom_histogram()+bar_theme1,
                       ggplot(Final_merge_data, aes(x=EnvironmentSatisfaction,fill=Attrition))+ geom_histogram()+bar_theme1,align = "h")
Con_title_plot3<-plot_grid(title,Cont_plot_3,ncol=1, rel_heights=c(0.1, 1))
Con_title_plot3


Cont_plot_4<-plot_grid(ggplot(Final_merge_data, aes(x=JobSatisfaction,fill=Attrition))+ geom_histogram()+bar_theme1,
                       ggplot(Final_merge_data, aes(x=WorkLifeBalance,fill=Attrition))+ geom_histogram()+bar_theme1,
                       ggplot(Final_merge_data, aes(x=JobInvolvement,fill=Attrition))+ geom_histogram()+bar_theme1,
                       ggplot(Final_merge_data, aes(x=PerformanceRating,fill=Attrition))+ geom_histogram()+bar_theme1,align = "h")

Con_title_plot4<-plot_grid(title,Cont_plot_4,ncol=1, rel_heights=c(0.1, 1))
Con_title_plot4


#DATA CLEANING

#Plotting the diferent Numeric columns against the Attrition in BoxPlot

plot_grid(ggplot(Final_merge_data, aes(x = Attrition, y = PercentSalaryHike)) + geom_boxplot(),
          ggplot(Final_merge_data, aes(x = Attrition, y = `Avg Hours`)) + geom_boxplot(),
          ggplot(Final_merge_data, aes(x = Attrition, y = MonthlyIncome)) + geom_boxplot(),
          ggplot(Final_merge_data, aes(x = Attrition, y = Distance.From.Home)) + geom_boxplot(),
          ggplot(Final_merge_data, aes(x = Attrition, y = NumCompaniesWorked)) + geom_boxplot(),
          ggplot(Final_merge_data, aes(x = Attrition, y = TotalWorkingYears)) + geom_boxplot(),
          ggplot(Final_merge_data, aes(x = Attrition, y = YearsAtCompany)) + geom_boxplot(),
          ggplot(Final_merge_data, aes(x = Attrition, y = YearsSinceLastPromotion)) + geom_boxplot(),
          ggplot(Final_merge_data, aes(x = Attrition, y = YearsWithCurrManager)) + geom_boxplot(),
          ggplot(Final_merge_data, aes(x = Attrition, y = EnvironmentSatisfaction)) + geom_boxplot(),
          ggplot(Final_merge_data, aes(x = Attrition, y = Age)) + geom_boxplot(),align='h')

ggplot(Final_merge_data, aes(x=`Avg Hours`))+ geom_line(stat = "count")

plot_grid(ggplot(Final_merge_data, aes(MonthlyIncome))+ geom_histogram(),
          ggplot(Final_merge_data, aes(x="",y=MonthlyIncome))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(Final_merge_data, aes(TotalWorkingYears))+ geom_histogram(),
          ggplot(Final_merge_data, aes(x="",y=TotalWorkingYears))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(Final_merge_data, aes(NumCompaniesWorked))+ geom_histogram(),
          ggplot(Final_merge_data, aes(x="",y=NumCompaniesWorked))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(Final_merge_data, aes(`Avg Hours`))+ geom_histogram(),
          ggplot(Final_merge_data, aes(x=`Avg Hours`))+ geom_point(stat = "count",color="Green")+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(Final_merge_data, aes(`Avg Hours`))+ geom_histogram(),
          ggplot(Final_merge_data, aes(x="",y=`Avg Hours`))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(Final_merge_data, aes(StockOptionLevel))+ geom_histogram(),
          ggplot(Final_merge_data, aes(x="",y=StockOptionLevel))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(Final_merge_data, aes(TrainingTimesLastYear))+ geom_histogram(),
          ggplot(Final_merge_data, aes(x="",y=as.numeric(TrainingTimesLastYear)))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(Final_merge_data, aes(YearsAtCompany))+ geom_histogram(binwidth = 10),
          ggplot(Final_merge_data, aes(x="",y=YearsAtCompany))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(Final_merge_data, aes(YearsSinceLastPromotion))+ geom_histogram(binwidth = 10),
          ggplot(Final_merge_data, aes(x="",y=YearsSinceLastPromotion))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(Final_merge_data, aes(leave_avail))+ geom_histogram(binwidth = 10),
          ggplot(Final_merge_data, aes(x="",y=leave_avail))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)


#Looking at the above Boxplots there seem to be Outliers in the below variables

#Monthly income
#total Working Years
#Number of Companies worked
#Average Hours
#Training times last year
#Years at Company
#Years Since Last Promotion
#Stock Level Options
#Yearswithcurrmanager

#Outlier Treatment
#Suppresing the outliers using capping technique using the Quantile function

quantile(Final_merge_data$MonthlyIncome,seq(0,1,0.01))
Final_merge_data$MonthlyIncome[which(Final_merge_data$MonthlyIncome > 132370)]<-132370
ggplot(Final_merge_data, aes(x = Attrition, y = MonthlyIncome)) + geom_boxplot()

quantile(Final_merge_data$`Avg Hours`,seq(0,1,0.01))
Final_merge_data$`Avg Hours`[which(Final_merge_data$`Avg Hours` > 10)]<-10
ggplot(Final_merge_data, aes(x = Attrition, y = `Avg Hours`)) + geom_boxplot()

quantile(Final_merge_data$TotalWorkingYears,seq(0,1,0.01))
Final_merge_data$TotalWorkingYears[which(Final_merge_data$TotalWorkingYears> 22)]<-22
ggplot(Final_merge_data, aes(x = Attrition, y = TotalWorkingYears)) + geom_boxplot()

quantile(Final_merge_data$NumCompaniesWorked,seq(0,1,0.01))
Final_merge_data$NumCompaniesWorked[which(Final_merge_data$NumCompaniesWorked> 8)]<-8
ggplot(Final_merge_data, aes(x = Attrition, y = NumCompaniesWorked)) + geom_boxplot()

quantile(Final_merge_data$TrainingTimesLastYear,seq(0,1,0.01))
Final_merge_data$TrainingTimesLastYear[which(Final_merge_data$TrainingTimesLastYear> 5)]<-5
ggplot(Final_merge_data, aes(x = Attrition, y = TrainingTimesLastYear)) + geom_boxplot()

quantile(Final_merge_data$YearsAtCompany,seq(0,1,0.01))
Final_merge_data$YearsAtCompany[which(Final_merge_data$YearsAtCompany> 17)]<- 17
ggplot(Final_merge_data, aes(x = Attrition, y = YearsAtCompany)) + geom_boxplot()

quantile(Final_merge_data$YearsSinceLastPromotion,seq(0,1,0.01))
Final_merge_data$YearsSinceLastPromotion[which(Final_merge_data$YearsSinceLastPromotion> 6)]<- 6
ggplot(Final_merge_data, aes(x = Attrition, y = YearsSinceLastPromotion)) + geom_boxplot()

quantile(Final_merge_data$YearsWithCurrManager,seq(0,1,0.01))
Final_merge_data$YearsWithCurrManager[which(Final_merge_data$YearsWithCurrManager> 10)]<- 10
ggplot(Final_merge_data, aes(x = Attrition, y = YearsWithCurrManager)) + geom_boxplot()

quantile(Final_merge_data$Age,seq(0,1,0.01))
Final_merge_data$YearsWithCurrManager[which(Final_merge_data$YearsWithCurrManager> 10)]<- 10
ggplot(Final_merge_data, aes(x = Attrition, y = YearsWithCurrManager)) + geom_boxplot()


#Removing the Below columns from the final dataset as they are not useful During the Analysis:

#Standard Hours - The value is same for all the employees
#Employee ID    - Is irrelevant during the analysis
#Over 18        - All the employee's seem to over 18
#Employee Count - The value is 1 for the entire range

Cleaned_data<-Final_merge_data[,-c(1,9,16,18)]
View(Cleaned_data)
str(Final_merge_data)
#Correlation Matrix between Numeric Variables

cor_mat<-round(cor(Final_merge_data[,c(2,6,7,11,14,15,17,19:31)]),2)

head(cor_mat)
cor_melt<-melt(cor_mat)

Tile_Plot<-ggplot(data = cor_melt, aes(x=X1, y=X2, fill=value)) + 
           geom_tile() + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
           scale_x_discrete(name = "")+ 
           scale_y_discrete(name = "") +ggtitle("Correlation Plot B/W Numeric Variables")
  Tile_Plot

#Looking at the Correlation plot,there seems to be a correaltion between 

#Years With Current Manager and Years at Company
#Total Working years and Age

########################################################################
# Feature standardisation

# Normalising continuous Variables

Cleaned_data[,c(1,5,12:14,16:27)]<-scale(Cleaned_data[,c(1,5,12:14,16:27)])

# converting target variable Attrition from No/Yes character to factorwith levels 0/1 
Cleaned_data$Attrition<- ifelse(Cleaned_data$Attrition=="Yes",1,0)

# Checking Attrition Rate of an Employee

Attrition<- sum(Cleaned_data$Attrition)/nrow(Cleaned_data)
Attrition

#Attrition # 16.12% Attrition rate

# creating a dataframe of categorical features
HR_cat<- Cleaned_data[,c(3,4,6:11,15)]

# converting categorical attributes to factor
HR_fact<- data.frame(sapply(HR_cat, function(x) factor(x)))

# creating dummy variables for factor attributes
dummies<- data.frame(sapply(HR_fact, 
function(x) data.frame(model.matrix(~x-1,data =HR_fact))[,-1]))

# Final dataset
HR_final<- cbind(Cleaned_data[,c(1,2,5,12:14,16:27)],dummies) 

########################################################################
# splitting the data between train and test
set.seed(100)

#install CATOOLS Package

indices = sample.split(HR_final$Attrition, SplitRatio = 0.7)

train = HR_final[indices,]

test = HR_final[!(indices),]

########################################################################
########################################################################
# Logistic Regression: 

#Initial model
model_1_log = glm(Attrition ~ ., data = train, family = "binomial")
summary(model_1_log)

#Using the Step AIC function to eliminate the insignificant variables:

model_2_log<-stepAIC(model_1_log,direction = "both")
summary(model_2_log)
vif(model_2_log)

#Eliinating the variable Job.Level.x3

model_3_log<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                   TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                   EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + 
                   `Avg Hours` + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                   Department.xResearch...Development + Department.xSales + 
                   Education.x3 + Education.x4 + Education.x5 + Education.Field.xOther + 
                   Education.Field.xTechnical.Degree + Job.Level.x2 +  
                   Job.Level.x5 + Job.Role.xManufacturing.Director + Job.Role.xResearch.Director + 
                   Job.Role.xResearch.Scientist + Job.Role.xSales.Executive + 
                   MaritalStatus.xSingle + StockOptionLevel.x1, family = "binomial", 
                 data = train)
summary(model_3_log)
vif(model_3_log)

#Eliinating the variable Job.Role.xResearch.Scientist

model_4_log<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                   TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                   EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + 
                   `Avg Hours` + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                   Department.xResearch...Development + Department.xSales + 
                   Education.x3 + Education.x4 + Education.x5 + Education.Field.xOther + 
                   Education.Field.xTechnical.Degree + Job.Level.x2 +  
                   Job.Level.x5 + Job.Role.xManufacturing.Director + Job.Role.xResearch.Director + 
                  Job.Role.xSales.Executive + 
                   MaritalStatus.xSingle + StockOptionLevel.x1, family = "binomial", 
                 data = train)

summary(model_4_log)
vif(model_4_log)

#Eliinating the variable Job.Level.x5

model_5_log<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                   TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                   EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + 
                   `Avg Hours` + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                   Department.xResearch...Development + Department.xSales + 
                   Education.x3 + Education.x4 + Education.x5 + Education.Field.xOther + 
                   Education.Field.xTechnical.Degree + Job.Level.x2 +  
                   Job.Role.xManufacturing.Director + Job.Role.xResearch.Director + 
                   Job.Role.xSales.Executive + 
                   MaritalStatus.xSingle + StockOptionLevel.x1, family = "binomial", 
                 data = train)

summary(model_5_log)
vif(model_5_log)

#Eliinating the variable Stock Level X1

model_6_log<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                   TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                   EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + 
                   `Avg Hours` + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                   Department.xResearch...Development + Department.xSales + 
                   Education.x3 + Education.x4 + Education.x5 + Education.Field.xOther + 
                   Education.Field.xTechnical.Degree + Job.Level.x2 +  
                   Job.Role.xManufacturing.Director + Job.Role.xResearch.Director + 
                   Job.Role.xSales.Executive + 
                   MaritalStatus.xSingle , family = "binomial", 
                 data = train)

summary(model_6_log)
vif(model_6_log)

#Eliinating the variable Education.Field.xTechnical.Degree

model_7_log<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                   TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                   EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + 
                   `Avg Hours` + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                   Department.xResearch...Development + Department.xSales + 
                   Education.x3 + Education.x4 + Education.x5 + Education.Field.xOther + 
                   Job.Level.x2 +  
                   Job.Role.xManufacturing.Director + Job.Role.xResearch.Director + 
                   Job.Role.xSales.Executive + 
                   MaritalStatus.xSingle , family = "binomial", 
                 data = train)

summary(model_7_log)
vif(model_7_log)

#Eliinating the variable Education.X3 

model_8_log<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                   TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                   EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + 
                   `Avg Hours` + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                   Department.xResearch...Development + Department.xSales + 
                   Education.x4 + Education.x5  +
                   Job.Level.x2 +  
                   Job.Role.xManufacturing.Director + Job.Role.xResearch.Director + 
                   Job.Role.xSales.Executive + 
                   MaritalStatus.xSingle , family = "binomial", 
                 data = train)

summary(model_8_log)
vif(model_8_log)

#Eliinating the variable Education.x4

model_9_log<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                   TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                   EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + 
                   `Avg Hours` + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                   Department.xResearch...Development + Department.xSales + 
                   Education.x5  +
                   Job.Level.x2 +  
                   Job.Role.xManufacturing.Director + Job.Role.xResearch.Director + 
                   Job.Role.xSales.Executive + 
                   MaritalStatus.xSingle , family = "binomial", 
                 data = train)

summary(model_9_log)
vif(model_9_log)

#Eliminating the variable Education.x5

model_10_log<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                    TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                    EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + 
                    `Avg Hours` + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                    Department.xResearch...Development + Department.xSales + 
                    Job.Level.x2 +  
                    Job.Role.xManufacturing.Director + Job.Role.xResearch.Director + 
                    Job.Role.xSales.Executive + 
                    MaritalStatus.xSingle , family = "binomial", 
                  data = train)

summary(model_10_log)
vif(model_10_log)

#Eliminating the variable Job.Role.xLaboratory.Technician

model_11_log<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                    TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                    EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + 
                    `Avg Hours` + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                    Department.xResearch...Development + Department.xSales + 
                    Job.Role.xManufacturing.Director + Job.Role.xResearch.Director + 
                    Job.Role.xSales.Executive + 
                    MaritalStatus.xSingle , family = "binomial", 
                  data = train)

summary(model_11_log)
vif(model_11_log)

#Eliminating the variablE Job.Role.xResearch.Director

model_12_log<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                    TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                    EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + 
                    `Avg Hours` + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                    Department.xResearch...Development + Department.xSales + 
                    Job.Role.xManufacturing.Director +  
                    Job.Role.xSales.Executive + 
                    MaritalStatus.xSingle , family = "binomial", 
                  data = train)

summary(model_12_log)
vif(model_12_log)

#Eliminating the variable Job.Role.xSales.ExecutiVE

model_13_log<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                    TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                    EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + 
                    `Avg Hours` + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                    Department.xResearch...Development + Department.xSales + 
                    Job.Role.xManufacturing.Director +  
                    MaritalStatus.xSingle , family = "binomial", 
                  data = train)

summary(model_13_log)
vif(model_13_log)

#Eliminating the variable WorkLifeBalance

model_14_log<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                    TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                    EnvironmentSatisfaction + JobSatisfaction + 
                    `Avg Hours` + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                    Department.xResearch...Development + Department.xSales + 
                    Job.Role.xManufacturing.Director +  
                    MaritalStatus.xSingle , family = "binomial", 
                  data = train)

summary(model_14_log)
vif(model_14_log)

#Eliminating the variable TrainingTimesLastYear

model_15_log<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                    YearsSinceLastPromotion + YearsWithCurrManager + 
                    EnvironmentSatisfaction + JobSatisfaction + 
                    `Avg Hours` + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                    Department.xResearch...Development + Department.xSales + 
                    Job.Role.xManufacturing.Director +  
                    MaritalStatus.xSingle , family = "binomial", 
                  data = train)

summary(model_15_log)
vif(model_15_log)

#Eliminating the variable BusinessTravel.xTravel_Rarely

model_16_log<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                    YearsSinceLastPromotion + YearsWithCurrManager + 
                    EnvironmentSatisfaction + JobSatisfaction + 
                    `Avg Hours` + BusinessTravel.xTravel_Frequently + 
                    Department.xResearch...Development + Department.xSales + 
                    Job.Role.xManufacturing.Director +  
                    MaritalStatus.xSingle , family = "binomial", 
                  data = train)

summary(model_16_log)
vif(model_16_log)

#Eliminating the variable BusinessTravel.xTravel_Rarely

model_17_log<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                    YearsSinceLastPromotion + YearsWithCurrManager + 
                    EnvironmentSatisfaction + JobSatisfaction + 
                    `Avg Hours` + BusinessTravel.xTravel_Frequently + 
                    Department.xSales + 
                    Job.Role.xManufacturing.Director +  
                    MaritalStatus.xSingle , family = "binomial", 
                  data = train)

summary(model_17_log)
vif(model_17_log)

#Eliminating the variable Department.xSales

model_18_log<-glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                    YearsSinceLastPromotion + YearsWithCurrManager + 
                    EnvironmentSatisfaction + JobSatisfaction + 
                    `Avg Hours` + BusinessTravel.xTravel_Frequently + 
                    Job.Role.xManufacturing.Director +  
                    MaritalStatus.xSingle , family = "binomial", 
                  data = train)

summary(model_18_log)
vif(model_18_log)

########################################################################################

### Model Evaluation

### Test Data ####

#predicted probabilities of Attrition for test data

View(test)

test_pred = predict(model_18_log, type = "response", 
                    newdata = test[,-2])

View(test)
summary(test_pred)

test$prob<-test_pred

test_pred_Attr <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_Attr <- factor(ifelse(test$Attrition==1,"Yes","No"))

test_pred_Attr <- factor(ifelse(test_pred >= 0.40, "Yes", "No"))
test_actual_Attr <- factor(ifelse(test$Attrition==1,"Yes","No"))

test_pred_Attr <- factor(ifelse(test_pred >= 0.20, "Yes", "No"))
test_actual_Attr <- factor(ifelse(test$Attrition==1,"Yes","No"))

table(test_actual_Attr,test_pred_Attr)

test_conf <- confusionMatrix(test_pred_Attr, test_actual_Attr, positive = "Yes")
test_conf


##########################################################################################

#Find the optimal Cutoff

perform_fn <- function(cutoff) 
{
  predicted_Attr <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_Attr, test_actual_Attr, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

summary(test_pred)

s = seq(.01,.80,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]
cutoff

# choose a cutoff value of 0.1695 for final model

test_cutoff_Attr <- factor(ifelse(test_pred >=0.1695, "Yes", "No"))

conf_final <- confusionMatrix(test_cutoff_Attr, test_actual_Attr, positive = "Yes")

acc <- conf_final$overall[1]
sens <- conf_final$byClass[1]
spec <- conf_final$byClass[2]

acc
sens
spec

View(test)
##################################################################################################
### KS -statistic - Test Data ######

test_cutoff_Attr<- factor(ifelse(test_cutoff_Attr=="Yes",1,0))
test_actual_Attr <- factor(ifelse(test_actual_Attr=="Yes",1,0))

test_cutoff_Attr
str(test_actual_Attr)
test_actual_Attr

library(ROCR)
#on testing  data
pred_object_test<- prediction(as.numeric(test_cutoff_Attr), as.numeric(test_actual_Attr))

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
                (attr(performance_measures_test, "x.values")[[1]])
#AOC Graph

plot(performance_measures_test)
max(ks_table_test)


###################################################################################################
# Lift & Gain Chart 

# plotting the lift chart

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}


Attri_decile = lift(test_actual_Attr, test_pred, groups = 10)
Attri_decile

#Below is the Sensitivity,specificity and Accuracy of the model:
  
#Accuracy 
#0.733938 

#Sensitivity 
#0.7323944 

#Specificity 
#0.7342342 

#K-statistics
#0.4665