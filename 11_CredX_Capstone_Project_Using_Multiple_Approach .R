
# Loading the required library
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(data.table)
library(stringr)
library(corrplot)
library(Information)
library(ROCR)
library(caret)
library(car)
library("MASS")
library(cowplot)
library(rpart)
library(rpart.plot)
library(randomForest)

#-----------------------------------------------------#
#             COMBINING THE DATAFRAMES
#-----------------------------------------------------#

# Loading the required files
credit_data <- read.csv("Credit Bureau data.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)
Demo_data <- read.csv("Demographic data.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)

# Checking the number of rows in Application.ID column for dataframes
length(unique(credit_data$Application.ID)) # confirm Application.ID can be a key to merge different dataframe
length(unique(Demo_data$Application.ID)) # confirm Application.ID can be a key to merge different dataframe

# Checking if all values of Application.ID are same in all dataframes
setdiff(credit_data$Application.ID,Demo_data$Application.ID) # Identical Application.ID across these datasets

# Merging into single dataframe, joined by Application.ID values.
cb_data <- merge(credit_data,Demo_data,by="Application.ID", all.x = TRUE)

# As we need to keep only one performance tag, we will first check the difference.
# If there is no difference we will remove one of them.
setdiff(cb_data$Performance.Tag.x,cb_data$Performance.Tag.y) # Identical Performance.Tag found

# We will now remove the Performance.Tag.x column from the dataframe.
cb_data <- cb_data[,-19]

#-----------------------------------------------------#
#                  BASIC UNDERSTANDING
#-----------------------------------------------------#

# Basic understanding of combined dataframe (cb_data) variables

# Checking the structure and summary of cb_data
str(cb_data)
summary(cb_data)

# Now lets check the number of NA's in each variable of cb_data set.
sapply(cb_data, function(x) sum(is.na(x)))

# Columns having NA's are    
# Avgas.CC.Utilization.in.last.12.months - 1058
# No.of.trades.opened.in.last.6.months - 1
# Presence.of.open.home.loan - 272
# Outstanding.Balance - 272
# No.of.dependents - 3
# Performance.Tag.y - 1425

# Lets look at the information value attached to each variable
# Before that we need to remove the NA's in performance.tag.y variable
# as that is the dependent variable and we cannot have NA's in Y variable.

# Information value is a useful technique to select important variables in a predictive model.
# It helps to rank variables on the basis of their importance.

cb_data <- na.omit(cb_data, cols="Performance.Tag.y")
IV <- create_infotables(data = cb_data, valid = NULL, trt = NULL, y = "Performance.Tag.y", bins=10, parallel=FALSE)
IV

#$Summary
#                                                          Variable          IV
#14 No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. 3.160014e-01
#8                           Avgas.CC.Utilization.in.last.12.months 3.146595e-01
#12                        No.of.PL.trades.opened.in.last.12.months 3.123134e-01
#10                           No.of.trades.opened.in.last.12.months 3.112907e-01
#16                                             Outstanding.Balance 2.559027e-01
#17                                              Total.No.of.Trades 2.501465e-01
#4                     No.of.times.30.DPD.or.worse.in.last.6.months 2.464151e-01
#11                         No.of.PL.trades.opened.in.last.6.months 2.289942e-01
#5                    No.of.times.90.DPD.or.worse.in.last.12.months 2.183576e-01
#13  No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. 2.144835e-01
#3                     No.of.times.60.DPD.or.worse.in.last.6.months 2.098427e-01
#7                    No.of.times.30.DPD.or.worse.in.last.12.months 2.026172e-01
#9                             No.of.trades.opened.in.last.6.months 1.956223e-01
#6                    No.of.times.60.DPD.or.worse.in.last.12.months 1.892646e-01
#2                     No.of.times.90.DPD.or.worse.in.last.6.months 1.638065e-01
#27                               No.of.months.in.current.residence 8.057104e-02
#23                                                          Income 4.181344e-02
#28                                 No.of.months.in.current.company 2.182739e-02
#15                                      Presence.of.open.home.loan 1.753179e-02
#19                                                             Age 3.803100e-03
#22                                                No.of.dependents 3.000590e-03
#25                                                      Profession 2.117783e-03
#18                                      Presence.of.open.auto.loan 1.662935e-03
#1                                                   Application.ID 1.308824e-03
#26                                               Type.of.residence 8.948310e-04
#24                                                       Education 7.930358e-04
#20                                                          Gender 2.554642e-04
#21                     Marital.Status..at.the.time.of.application. 7.847299e-05

# The above table gives us a view of what could be the most important variables
# while doing the model building exercise.

# Creating a dataframe to store the original values after removing
# all the blanks from performance tag column
original_data <- cb_data

#-----------------------------------------------------#
#               DESCRIPTIVE STATISTICS
#-----------------------------------------------------#

#-----------------------------------------------------#
# Graphs and summary for credit bureau variables - Univariate Analysis
#-----------------------------------------------------#

#Understanding the variables No of times 90 DPD or worse in last 6 months & Performance Tag  
times_90dpd_6mth <- ggplot(cb_data ,aes(cb_data$No.of.times.90.DPD.or.worse.in.last.6.months ,fill=factor(cb_data$No.of.times.90.DPD.or.worse.in.last.6.months)))
labels_times_90dpd_6mth <- labs(x = "Times DPD", y = "No. of Customers", title = "90 DPD 6 Months Distribution", fill = "Performance.Tag.y")
legend_position <- theme(legend.position = "bottom")
times_90dpd_6mth_final <- times_90dpd_6mth + labels_times_90dpd_6mth + legend_position + geom_bar(position = "dodge")
times_90dpd_6mth_final

#Understanding the variables No of times 60 DPD or worse in last 6 months & Performance Tag  
times_60dpd_6mth <- ggplot(cb_data ,aes(cb_data$No.of.times.60.DPD.or.worse.in.last.6.months ,fill=factor(cb_data$No.of.times.60.DPD.or.worse.in.last.6.months)))
labels_times_60dpd_6mth <- labs(x = "Times DPD", y = "No. of Customers", title = "60 DPD 6 Months Distribution", fill = "Performance.Tag.y")
times_60dpd_6mth_final <- times_60dpd_6mth + labels_times_60dpd_6mth + legend_position + geom_bar(position = "dodge")
times_60dpd_6mth_final

#Understanding the variables No of times 30 DPD or worse in last 6 months & Performance Tag  
times_30dpd_6mth <- ggplot(cb_data ,aes(cb_data$No.of.times.30.DPD.or.worse.in.last.6.months ,fill=factor(cb_data$No.of.times.30.DPD.or.worse.in.last.6.months)))
labels_times_30dpd_6mth <- labs(x = "Times DPD", y = "No. of Customers", title = "30 DPD 6 Months Distribution", fill = "Performance.Tag.y")
times_30dpd_6mth_final <- times_30dpd_6mth + labels_times_30dpd_6mth + legend_position + geom_bar(position = "dodge")
times_30dpd_6mth_final

#Understanding the variables No of times 90 DPD or worse in last 12 months & Performance Tag  
times_90dpd_12mth <- ggplot(cb_data ,aes(cb_data$No.of.times.90.DPD.or.worse.in.last.12.months ,fill=factor(cb_data$No.of.times.90.DPD.or.worse.in.last.12.months)))
labels_times_90dpd_12mth <- labs(x = "Times DPD", y = "No. of Customers", title = "90 DPD 12 Months Distribution", fill = "Performance.Tag.y")
times_90dpd_12mth_final <- times_90dpd_12mth + labels_times_90dpd_12mth + legend_position + geom_bar(position = "dodge")
times_90dpd_12mth_final

#Understanding the variables No of times 60 DPD or worse in last 12 months & Performance Tag  
times_60dpd_12mth <- ggplot(cb_data ,aes(cb_data$No.of.times.60.DPD.or.worse.in.last.12.months ,fill=factor(cb_data$No.of.times.60.DPD.or.worse.in.last.12.months)))
labels_times_60dpd_12mth <- labs(x = "Times DPD", y = "No. of Customers", title = "60 DPD 12 Months Distribution", fill = "Performance.Tag.y")
times_60dpd_12mth_final <- times_60dpd_12mth + labels_times_60dpd_12mth + legend_position + geom_bar(position = "dodge")
times_60dpd_12mth_final

#Understanding the variables No of times 30 DPD or worse in last 12 months & Performance Tag  
times_30dpd_12mth <- ggplot(cb_data ,aes(cb_data$No.of.times.30.DPD.or.worse.in.last.12.months ,fill=factor(cb_data$No.of.times.30.DPD.or.worse.in.last.12.months)))
labels_times_30dpd_12mth <- labs(x = "Times DPD", y = "No. of Customers", title = "30 DPD 12 Months Distribution", fill = "Performance.Tag.y")
times_30dpd_12mth_final <- times_30dpd_12mth + labels_times_30dpd_12mth + legend_position + geom_bar(position = "dodge")
times_30dpd_12mth_final

#Understanding the variables Avgas CC Utilization in last 12 months & Performance Tag  
avg_utility_12mth <- ggplot(cb_data ,aes(cb_data$Avgas.CC.Utilization.in.last.12.months ,fill=factor(cb_data$Avgas.CC.Utilization.in.last.12.months)))
labels_avg_utility_12mth <- labs(x = "Average_Utilization", y = "No. of Customers", title = "Average Credit Utilization in last 12 months", fill = "Performance.Tag.y")
avg_utility_12mth_final <- avg_utility_12mth + labels_avg_utility_12mth + geom_bar(position = "dodge")
avg_utility_12mth_final

#Understanding the variables No of trades opened in last 6 months & Performance Tag  
trade_opened_6mth <- ggplot(cb_data ,aes(cb_data$No.of.trades.opened.in.last.6.months ,fill=factor(cb_data$No.of.trades.opened.in.last.6.months)))
labels_trade_opened_6mth <- labs(x = "Trades Opened", y = "No. of Customers", title = "Trades Opened 6 Months Distribution", fill = "Performance.Tag.y")
trade_opened_6mth_final <- trade_opened_6mth + labels_trade_opened_6mth + legend_position + geom_bar(position = "dodge")
trade_opened_6mth_final

#Understanding the variables No of trades opened in last 12 months & Performance Tag  
trade_opened_12mth <- ggplot(cb_data ,aes(cb_data$No.of.trades.opened.in.last.12.months ,fill=factor(cb_data$No.of.trades.opened.in.last.12.months)))
labels_trade_opened_12mth <- labs(x = "Trades Opened", y = "No. of Customers", title = "Trades Opened 12 Months Distribution", fill = "Performance.Tag.y")
trade_opened_12mth_final <- trade_opened_12mth + labels_trade_opened_12mth + geom_bar(position = "dodge")
trade_opened_12mth_final

#Understanding the variables No of PL trades opened in last 6 months & Performance Tag  
trade_opened_6mth_pl <- ggplot(cb_data ,aes(cb_data$No.of.PL.trades.opened.in.last.6.months ,fill=factor(cb_data$No.of.PL.trades.opened.in.last.6.months)))
labels_trade_opened_6mth_pl <- labs(x = "PL Trades Opened", y = "No. of Customers", title = "PL Trades Opened 6 Months Distribution", fill = "Performance.Tag.y")
trade_opened_6mth_pl_final <- trade_opened_6mth_pl + labels_trade_opened_6mth_pl + legend_position + geom_bar(position = "dodge")
trade_opened_6mth_pl_final

#Understanding the variables No of PL trades opened in last 12 months & Performance Tag  
trade_opened_12mth_pl <- ggplot(cb_data ,aes(cb_data$No.of.PL.trades.opened.in.last.12.months ,fill=factor(cb_data$No.of.PL.trades.opened.in.last.12.months)))
labels_trade_opened_12mth_pl <- labs(x = "PL Trades Opened", y = "No. of Customers", title = "PL Trades Opened 12 Months Distribution", fill = "Performance.Tag.y")
trade_opened_12mth_pl_final <- trade_opened_12mth_pl + labels_trade_opened_12mth_pl + geom_bar(position = "dodge")
trade_opened_12mth_pl_final

#Understanding the variables No of Inquiries in last 6 months (excluding home & auto loans) & Performance Tag  
inquiries_6mth <- ggplot(cb_data ,aes(cb_data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. ,fill=factor(cb_data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.)))
labels_inquiries_6mth <- labs(x = "No of Inquiries", y = "No. of Customers", title = "Inquiries in 6 Months Distribution", fill = "Performance.Tag.y")
inquiries_6mth_final <- inquiries_6mth + labels_inquiries_6mth + legend_position + geom_bar(position = "dodge")
inquiries_6mth_final

#Understanding the variables No of Inquiries in last 12 months (excluding home & auto loans) & Performance Tag  
inquiries_12mth <- ggplot(cb_data ,aes(cb_data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. ,fill=factor(cb_data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.)))
labels_inquiries_12mth <- labs(x = "No of Inquiries", y = "No. of Customers", title = "Inquiries in 12 Months Distribution", fill = "Performance.Tag.y")
inquiries_12mth_final <- inquiries_12mth + labels_inquiries_12mth + geom_bar(position = "dodge")
inquiries_12mth_final

#Understanding the variables Presence of open home loan & Performance Tag  
open_home_loan <- ggplot(cb_data ,aes(cb_data$Presence.of.open.home.loan ,fill=factor(cb_data$Presence.of.open.home.loan)))
labels_open_home_loan <- labs(x = "Open Home Loans", y = "No. of Customers", title = "Presence of open home loan", fill = "Performance.Tag.y")
open_home_loan_final <- open_home_loan + labels_open_home_loan + geom_bar(position = "dodge")
open_home_loan_final

#Understanding the variables Presence of open auto loan & Performance Tag  
open_auto_loan <- ggplot(cb_data ,aes(cb_data$Presence.of.open.auto.loan ,fill=factor(cb_data$Presence.of.open.auto.loan)))
labels_open_auto_loan <- labs(x = "Open Auto Loans", y = "No. of Customers", title = "Presence of open auto loan", fill = "Performance.Tag.y")
open_auto_loan_final <- open_auto_loan + labels_open_auto_loan + geom_bar(position = "dodge")
open_auto_loan_final

#Understanding the variables Total No of Trades & Performance Tag  
total_trades <- ggplot(cb_data ,aes(cb_data$Total.No.of.Trades ,fill=factor(cb_data$Total.No.of.Trades)))
labels_total_trades <- labs(x = "Total Trades", y = "No. of Customers", title = "Total No of Trades", fill = "Performance.Tag.y")
total_trades_final <- total_trades + labels_total_trades + geom_bar(position = "dodge")
total_trades_final

#Understanding the variables Outstanding Balance & Performance Tag
cb_data$Outstanding.Balance <- as.numeric(cb_data$Outstanding.Balance)
summary_oust_bal <- as.data.frame(summarise(group_by(cb_data, Performance.Tag.y),sum(Outstanding.Balance)))
colnames(summary_oust_bal)[colnames(summary_oust_bal)=="sum(Outstanding.Balance)"] <- "Outstanding_Balance_In_millions"
summary_oust_bal$Outstanding_Balance_In_millions <- summary_oust_bal$Outstanding_Balance_In_millions/1000000
summary_oust_bal

#         Performance.Tag.y Outstanding_Balance_In_millions
#1                 0           83487.912
#2                 1            3695.068
# The above numbers are in millions

#-----------------------------------------------------#
# Graphs and summary for demographic variables - Univariate Analysis
#-----------------------------------------------------#

demo_data_mod<-Demo_data

#-----------------------------------------------------#

# NA treatment

#Removing NA in performance Tagc from demo_data to build model with demo_data only
demo_data_mod <- demo_data[complete.cases(demo_data$Performance.Tag),]

str(demo_data_mod)
sapply(demo_data_mod, function(x) sum(is.na(x)))
#No.of.dependents 3 N/A value

summary(demo_data_mod)
#Replacing N/A in column No.of.dependents with  Median 3 
#No.of.dependents
#Min.   :1.00   
#1st Qu.:2.00   
#Median :3.00   
#Mean   :2.86   
#3rd Qu.:4.00   
#Max.   :5.00   
#NA's   :3      

demo_data_mod$No.of.dependents[is.na(demo_data_mod$No.of.dependents)] <- 3

#-----------------------------------------------------#

# UDF "plot_response" to do the same task for each variable
plot_response <- function(cat_var, var_name){
  a <- aggregate(Performance.Tag~cat_var, demo_data_mod, mean)
  count <- data.frame(table(cat_var))
  count <- count[,-1]
  agg_response <- cbind(a, count)
  
  colnames(agg_response) <- c(var_name, "default_rate","No.of_applicant")
  agg_response[, 2] <- format(round(agg_response[, 2], 3))
  
  ggplot(agg_response, aes(agg_response[, 1], count, label = default_rate)) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(size = 3, vjust = -0.5) + xlab(var_name)
  
}

#-----------------------------------------------------#

# Variable Age

# Plotting Age histogram
ggplot(demo_data_mod,aes(Age))+geom_histogram()

# Let's check the outlier in the variables 
quantile(demo_data_mod$Age,seq(0,1,0.01))

# Box plot 
boxplot(demo_data_mod$Age)

# Capping the lower values of age with 27.
demo_data_mod[(which(demo_data_mod$Age<27)),]$Age <- 27

# Binning the age variable and store it into "binning.age".
demo_data_mod$binning.age <- as.factor(cut(demo_data_mod$Age, breaks = c(20,30,40,50,60,70)))

# Check the numeric value of default rate in each bucket
agg_age <- merge(aggregate(Performance.Tag ~ binning.age, demo_data_mod, mean),aggregate(Performance.Tag~binning.age, demo_data_mod, sum),by = "binning.age") 

# Adding No.of_prospect
count <- data.frame(table(demo_data_mod$binning.age))
count <- count[,-1]
agg_age <- cbind(agg_age,count)

# changing column name of each variables in agg_age dataframe
colnames(agg_age) <- c("age", "default_rate", "count_default","No.of_applicant")

# Round Off the values
agg_age$default_rate <- format(round(agg_age$default_rate, 3))
agg_age

# Let's see the default rate of each age bucket in the plot
ggplot(agg_age, aes(age, No.of_applicant,label = default_rate)) + 
  geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5)

#-----------------------------------------------------# 

# Variable "Profession"

# Checking the levels of the Profession
demo_data_mod$Profession <- as.factor(demo_data_mod$Profession)
levels(demo_data_mod$Profession)
summary(demo_data_mod$Profession)
levels(demo_data_mod$Profession)[1] <- " SAL"

# Plotting bar graph for job variable.
plot_response(as.character(demo_data_mod$Profession), "Profession")

#-----------------------------------------------------# 

# Variable Marital Status

# Checking Marital status
demo_data_mod$Marital.Status..at.the.time.of.application <- as.factor(demo_data_mod$Marital.Status..at.the.time.of.application)

# Checking the summary of the variable
summary(demo_data_mod$Marital.Status..at.the.time.of.application)

# Let's replace Unknown level to married
levels(demo_data_mod$Marital.Status..at.the.time.of.application)[1] <- "Married"

# Plotting marital status
plot_response(demo_data_mod$Marital.Status..at.the.time.of.application,"marital")

#-----------------------------------------------------#

# Variable Education

# Let's see the education variables
demo_data_mod$Education <- as.factor(demo_data_mod$Education)
levels(demo_data_mod$Education)
summary(demo_data_mod$Education)
levels(demo_data_mod$Education)[1] <- "Others"

# Plotting Education
plot_response(demo_data_mod$Education,"Education")

#-----------------------------------------------------#

# Variable Performance TAG

# Checking the summary
table(demo_data_mod$Performance.Tag)

# Plotting Performance TAG
plot_response(demo_data_mod$Performance.Tag, "Default")

#-----------------------------------------------------#

# Variable Type of Residence

# Let's understand the housing variables 
demo_data_mod$Type.of.residence <- as.factor(demo_data_mod$Type.of.residence)
summary(demo_data_mod$Type.of.residence)
levels(demo_data_mod$Type.of.residence)[1] <- "Rented"

# Plotting Housing variable
plot_response(demo_data_mod$Type.of.residence, "Housing")

#-----------------------------------------------------#

# variable "Gender"

# Let's understand the gender variables
demo_data_mod$Gender <- as.factor(demo_data_mod$Gender)
summary(demo_data_mod$Gender)
levels(demo_data_mod$Gender)[1] <- "M"

# Plotting Gender variable
plot_response(demo_data_mod$Gender, "Gender")

#-----------------------------------------------------#

# Variable No Of Dependents

# Let's see the next variable which is "Dependents"
plot_response(demo_data_mod$No.of.dependents, "Dependents")

#-----------------------------------------------------#

# Variables Income

# Let's check the outlier in the variables 
quantile(demo_data_mod$Income,seq(0,1,0.01))

# Box plot 
boxplot(demo_data_mod$Income)

# Binning the age variable and store it into "binning.age".
demo_data_mod$binning.Income <- as.factor(cut(demo_data_mod$Income, breaks = c(10,20,30,40,50,60)))

# Plotting Income variable
plot_response(demo_data_mod$binning.Income, "binning income")

#-----------------------------------------------------#

# Variables No.of.months.in.current.company

# Let's check the outlier in the variables No.of.months.in.current.company
quantile(demo_data_mod$No.of.months.in.current.company,seq(0,1,0.01))

# Box plot 
boxplot(demo_data_mod$No.of.months.in.current.company)

# Capping the lower values of No.of.months.in.current.company with 74
demo_data_mod[(which(demo_data_mod$No.of.months.in.current.company>74)),]$No.of.months.in.current.company <- 74

# Binning the age variable and store it into "binning.tenure".
demo_data_mod$binning.tenure <- as.factor(cut(demo_data_mod$No.of.months.in.current.company, breaks = c(0,24,48,72,96)))

# Plotting Months in current company variable
plot_response(demo_data_mod$binning.tenure, "binning tenure")

#-----------------------------------------------------#

# Variables No.of.months.in.current.residence

# Let's check the outlier in the variables No.of.months.in.current.company
quantile(demo_data_mod$No.of.months.in.current.residence,seq(0,1,0.01))

# Box plot 
boxplot(demo_data_mod$No.of.months.in.current.residence)

# Binning the age variable and store it into "binning.tenure".
demo_data_mod$binning.resi_tenure <- as.factor(cut(demo_data_mod$No.of.months.in.current.residence, breaks = c(0,24,48,72,96,120,144)))

# Plotting Months in current residence variable
plot_response(demo_data_mod$binning.resi_tenure, "binning resi tenure")

#-----------------------------------------------------#
# Graphs and summary for credit bureau and demographic variables - Segmented Analysis
#-----------------------------------------------------#

#-----------------------------------------------------#
# Segmented analysis will be on looking at the impact of performance Tag = 1
# on all the credit bureau variables
#-----------------------------------------------------#

#Subsetting the data to take only Performance.Tag.y = 1
performance_tag_1 <- filter(cb_data, Performance.Tag.y == "1")

# creating the plots to understand the distribution of people who
# defaulted on payments, where performance tag = 1

plot_grid(ggplot(performance_tag_1, aes(x=No.of.times.90.DPD.or.worse.in.last.6.months))+ geom_bar(fill = "orange"), 
          ggplot(performance_tag_1, aes(x=No.of.times.60.DPD.or.worse.in.last.6.months))+ geom_bar(fill = "orange"),
          ggplot(performance_tag_1, aes(x=No.of.times.30.DPD.or.worse.in.last.6.months))+ geom_bar(fill = "orange"),
          ggplot(performance_tag_1, aes(x=No.of.times.90.DPD.or.worse.in.last.12.months))+ geom_bar(fill = "orange"),
          align = "h")

plot_grid(ggplot(performance_tag_1, aes(x=No.of.times.60.DPD.or.worse.in.last.12.months))+ geom_bar(fill = "orange"), 
          ggplot(performance_tag_1, aes(x=No.of.times.30.DPD.or.worse.in.last.12.months))+ geom_bar(fill = "orange"),
          ggplot(performance_tag_1, aes(x=No.of.trades.opened.in.last.6.months))+ geom_bar(fill = "orange"),
          ggplot(performance_tag_1, aes(x=No.of.trades.opened.in.last.12.months))+ geom_bar(fill = "orange"),
          align = "h")

plot_grid(ggplot(performance_tag_1, aes(x=No.of.PL.trades.opened.in.last.6.months))+ geom_bar(fill = "orange"), 
          ggplot(performance_tag_1, aes(x=No.of.PL.trades.opened.in.last.12.months))+ geom_bar(fill = "orange"),
          ggplot(performance_tag_1, aes(x=No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.))+ geom_bar(fill = "orange"),
          ggplot(performance_tag_1, aes(x=No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.))+ geom_bar(fill = "orange"),
          align = "h")

plot_grid(ggplot(performance_tag_1, aes(x=Presence.of.open.home.loan))+ geom_bar(fill = "orange"), 
          ggplot(performance_tag_1, aes(x=Education))+ geom_bar(fill = "orange"),
          ggplot(performance_tag_1, aes(x=No.of.dependents))+ geom_bar(fill = "orange"),
          ggplot(performance_tag_1, aes(x=Presence.of.open.auto.loan))+ geom_bar(fill = "orange"),
          align = "h")

plot_grid(ggplot(performance_tag_1, aes(x=Gender))+ geom_bar(fill = "orange"), 
          ggplot(performance_tag_1, aes(x=Total.No.of.Trades))+ geom_bar(fill = "orange"),
          ggplot(performance_tag_1, aes(x=Avgas.CC.Utilization.in.last.12.months))+ geom_bar(fill = "orange"),
          ggplot(performance_tag_1, aes(x=Marital.Status..at.the.time.of.application.))+ geom_bar(fill = "orange"),
          align = "h")

plot_grid(ggplot(performance_tag_1, aes(x=Profession))+ geom_bar(fill = "orange"), 
          ggplot(performance_tag_1, aes(x=Age))+ geom_bar(fill = "orange"),
          ggplot(performance_tag_1, aes(x=Income))+ geom_bar(fill = "orange"),
          ggplot(performance_tag_1, aes(x=No.of.months.in.current.company))+ geom_bar(fill = "orange"),
          align = "h")



#-----------------------------------------------------#
#               FEATURE ENGINEERING
#-----------------------------------------------------#

#-----------------------------------------------------#
#   VARIABLE TRANSFORMATION - DEMOGRAPHIC DATA
#-----------------------------------------------------#

# Variable - Age

# Plotting the Age histogram
ggplot(cb_data,aes(Age))+geom_histogram()
# We see that Age has negative and zero in it, we need to see how we can treat them.
# Ideally having zero means data not available, hence we will make the zero and negative as NA's

# MAking 0 and -3 as NA's
cb_data[, 19][cb_data[, 19] == 0] <- NA
cb_data[, 19][cb_data[, 19] == -3] <- NA

# Let's check the outlier in the variables 
quantile(cb_data$Age,seq(0,1,0.01),na.rm = TRUE)
# Data is closely kint, no outliers found.

# Below is the woe values for Age, we also see that there were 19 NA's
# introduced, which we will remove, as it contributes only 0.0002% of the total 

#Age    N      Percent          WOE           IV
#1       NA   19 0.0002759622  0.000000000 0.0000000000
#2  [15,30] 5820 0.0845315904 -0.040002629 0.0001328179
#3  [31,35] 6824 0.0991140160  0.044365722 0.0003319175
#4  [36,38] 6829 0.0991866376  0.067737602 0.0008014053
#5  [39,41] 7033 0.1021496006  0.077067136 0.0014299757
#6  [42,44] 6884 0.0999854757 -0.055590327 0.0017312143
#7  [45,47] 6732 0.0977777778 -0.006102128 0.0017348450
#8  [48,50] 6655 0.0966594045 -0.009005722 0.0017426521
#9  [51,53] 6728 0.0977196805 -0.139220832 0.0035204996
#10 [54,57] 7515 0.1091503268  0.046332976 0.0037598524
#11 [58,65] 7811 0.1134495280 -0.013138495 0.0037793187

# Lets remove the NA values and bin the other values.
cb_data <- na.omit(cb_data, cols="Age")

# Let us create the bucket as is, instead of doing a coarse classing and
# recreating the bins

cb_data$Age <- as.numeric(cb_data$Age)
cb_data$Age_binned <- cut(cb_data$Age, c(14,30,35,38,41,44,47,50,53,57,66),include.lowest = FALSE)

# Now we will rename the levels in the newly created Age_binned variable.

cb_data$Age_binned <- as.character(cb_data$Age_binned)
cb_data$Age_binned[which(cb_data$Age_binned == "(14,30]")] <- "15_30" 
cb_data$Age_binned[which(cb_data$Age_binned == "(30,35]")] <- "31_35"
cb_data$Age_binned[which(cb_data$Age_binned == "(35,38]")] <- "36_38"
cb_data$Age_binned[which(cb_data$Age_binned == "(38,41]")] <- "39_41"
cb_data$Age_binned[which(cb_data$Age_binned == "(41,44]")] <- "42_44"
cb_data$Age_binned[which(cb_data$Age_binned == "(44,47]")] <- "45_47"
cb_data$Age_binned[which(cb_data$Age_binned == "(47,50]")] <- "48_50"
cb_data$Age_binned[which(cb_data$Age_binned == "(50,53]")] <- "51_53"
cb_data$Age_binned[which(cb_data$Age_binned == "(53,57]")] <- "54_57"
cb_data$Age_binned[which(cb_data$Age_binned == "(57,66]")] <- "58_65"
cb_data$Age_binned <- as.factor(cb_data$Age_binned)

#Converting "Age_binned" into dummies#
cb_data$Age_binned <- as.factor(cb_data$Age_binned)
agebin_dummy <- data.frame(model.matrix( ~Age_binned, data = cb_data))

#Removing the x-intercept from the newly created dataframe,#
#using the n-1 principal of level creation and keeping the rest 9 variables#
agebin_dummy <- agebin_dummy[,-1]

#Merging the dummy variables to the main data set, after removing the original column#
cb_data <- cbind(cb_data[,-30], agebin_dummy)

#-----------------------------------------------------#

# UDF for changing blanks to na
blank_as_na <- function(x){
  if("factor" %in% class(x)) x <- as.character(x) ## since ifelse wont work with factors
  ifelse(as.character(x)!="", x, NA)
}

#-----------------------------------------------------#

# Variable - Gender

# Checking the summary
cb_data$Gender <- as.factor(cb_data$Gender)
summary(cb_data$Gender)

#Blank F     M 
#1    16256 52574
# We see that there is one data point which is blank
# We will change that to NA and see the impact

cb_data$Gender <- blank_as_na(cb_data$Gender)

#Gender     N      Percent         WOE           IV
#1   <NA>     1 1.452834e-05  0.00000000 0.0000000000
#2      F 16256 2.361727e-01  0.02837190 0.0001925996
#3      M 52574 7.638128e-01 -0.00890386 0.0002529075

# The weight attcahed to NA is negligible hence we will remove it from our analysis
cb_data <- na.omit(cb_data, cols="Gender")

#Converting gender values to numeric so as to replace the levels- M and F with 1 and 0#
cb_data$Gender <- as.factor(cb_data$Gender)
levels(cb_data$Gender)<-c(0,1)

#Storing the numeric values in the same variable#
cb_data$Gender <- as.numeric(levels(cb_data$Gender))[cb_data$Gender]

# Checking the summary of variable after conversion#
summary(factor(cb_data$Gender))

# 0     1 
# 16256 52574

#-----------------------------------------------------#

# Variable - Marital.Status..at.the.time.of.application.

# we will first change the column header
colnames(cb_data)[colnames(cb_data)=="Marital.Status..at.the.time.of.application."] <- "Marital_Status"

# Checking the summary
cb_data$Marital_Status <- as.factor(cb_data$Marital_Status)
summary(cb_data$Marital_Status)

#Blanks Married  Single 
#5      58643   10182
# we see 5 data points having blanks.
# we will change it to NA and see its impact

cb_data$Marital_Status <- blank_as_na(cb_data$Marital_Status)

#Marital_Status     N      Percent          WOE           IV
#1           <NA>     5 7.264274e-05  0.000000000 0.000000e+00
#2        Married 58643 8.519977e-01 -0.003657802 1.138025e-05
#3         Single 10182 1.479297e-01  0.021334821 7.937569e-05

# The weight attcahed to NA is negligible hence we will remove it from our analysis
cb_data <- na.omit(cb_data, cols="Marital_Status")

#Converting marital status values to numeric so as to replace the levels- Married and single with 1 and 0#
cb_data$Marital_Status <- as.factor(cb_data$Marital_Status)
levels(cb_data$Marital_Status)<-c(0,1)

#Storing the numeric values in the same variable#
cb_data$Marital_Status <- as.numeric(levels(cb_data$Marital_Status))[cb_data$Marital_Status]

# Checking the summary of variable after conversion#
summary(factor(cb_data$Marital_Status))

# 0     1 
# 58643 10182

#-----------------------------------------------------#

# Variable - No.of.dependents

# Checking the summary
cb_data$No.of.dependents <- as.factor(cb_data$No.of.dependents)
summary(cb_data$No.of.dependents)

#1     2     3     4     5 
#14995 14912 15407 11828 11683 

# Lets now check the WOE value for this variable
#No.of.dependents     N   Percent          WOE          IV
#1            [1,1] 14999 0.2178504  0.046911416 0.000489851
#2            [2,2] 14919 0.2166885 -0.091083426 0.002214426
#3            [3,3] 15413 0.2238635  0.052679561 0.002850882
#4            [4,4] 11832 0.1718519 -0.028733569 0.002990914
#5            [5,5] 11687 0.1697458  0.007536975 0.003000590

# We will not do any changes to this variable, we will go with the
# original buckets produced by WOE.

#Converting "No.of.dependents" into dummies#
cb_data$No.of.dependents <- as.factor(cb_data$No.of.dependents)
dependent_dummy <- data.frame(model.matrix( ~No.of.dependents, data = cb_data))

#Removing the x-intercept from the newly created dataframe,#
#using the n-1 principal of level creation and keeping the rest 4 variables#
dependent_dummy <- dependent_dummy[,-1]

#Merging the dummy variables to the main data set, after removing the original column#
cb_data <- cbind(cb_data[,-22], dependent_dummy)

#-----------------------------------------------------#

# Variable - Education

# Checking the summary
cb_data$Education <- as.factor(cb_data$Education)
summary(cb_data$Education)

#           Bachelor    Masters       Others        Phd         Professional 
# 117        17033        23127          115         4399        24034

# We see that there are 117 data points which are blank
# We will change that to NA and see the impact

cb_data$Education <- blank_as_na(cb_data$Education)

#Education     N     Percent           WOE           IV
#1      NA        117 0.001699346  0.0147652520 3.729939e-07
#2     Bachelor 17040 0.247494553  0.0201956609 1.022559e-04
#3      Masters 23134 0.336005810 -0.0008066263 1.024745e-04
#4       Others   115 0.001670298  0.5304389181 7.046308e-04
#5          Phd  4403 0.063950617 -0.0085914436 7.093327e-04
#6 Professional 24041 0.349179375 -0.0155378274 7.930358e-04

# The weight attcahed to NA is negligible hence we will remove it from our analysis
cb_data <- na.omit(cb_data, cols="Education")

#Converting "Education" into dummies#
cb_data$Education <- as.factor(cb_data$Education)
education_dummy <- data.frame(model.matrix( ~Education, data = cb_data))

#Removing the x-intercept from the newly created dataframe,#
#using the n-1 principal of level creation and keeping the rest 4 variables#
education_dummy <- education_dummy[,-1]

#Merging the dummy variables to the main data set, after removing the original column#
cb_data <- cbind(cb_data[,-23], education_dummy)

#-----------------------------------------------------#

# Variable - Profession

# Checking the summary
cb_data$Profession <- as.factor(cb_data$Profession)
summary(cb_data$Profession)

#       SAL      SE   SE_PROF 
# 12   39029   13702   15965

# We see that there are 12 data points which are blank
# We will change that to NA and see the impact

cb_data$Profession <- blank_as_na(cb_data$Profession)

#Profession     N      Percent         WOE           IV
#1        NA     12 0.0001888163  0.00000000 0.0000000000
#2        SAL 39098 0.5678721859 -0.02720428 0.0004150712
#3         SE 13727 0.1993754539  0.08940755 0.0020756951
#4    SE_PROF 16012 0.2325635439 -0.01349425 0.0021177830

# The weight attcahed to NA is negligible hence we will remove it from our analysis
cb_data <- na.omit(cb_data, cols="Profession")

#Converting "Profession" into dummies#
cb_data$Profession <- as.factor(cb_data$Profession)
Profession_dummy <- data.frame(model.matrix( ~Profession, data = cb_data))

#Removing the x-intercept from the newly created dataframe,#
#using the n-1 principal of level creation and keeping the rest 2 variables#
Profession_dummy <- Profession_dummy[,-1]

#Merging the dummy variables to the main data set, after removing the original column#
cb_data <- cbind(cb_data[,-23], Profession_dummy)

#-----------------------------------------------------#

# Variable - Type.of.residence

# Checking the levels for Types of Residence
cb_data$Type.of.residence <- as.factor(cb_data$Type.of.residence)
levels(cb_data$Type.of.residence)

#"" "Company provided" "Living with Parents" "Others" "Owned" "Rented"
# There seems to be few blanks in this variable, lets check the summary.
summary(cb_data$Type.of.residence)

#"" "Company provided" "Living with Parents" "Others" "Owned" "Rented"
#8   1583               1752                  196      13801   51485

# We see that there are 8 data points which are blank
# We will change that to NA and see the impact

cb_data$Type.of.residence <- blank_as_na(cb_data$Type.of.residence)

# Below is the woe values for Type of residence, we also see that there were 8 NA's
# this we will remove, as it contributes only 0.0001% of the total

#Type.of.residence     N      Percent          WOE           IV
#1               NA        8 0.0001161946  0.000000000 0.0000000000
#2    Company provided  1585 0.0230210603  0.078642616 0.0001476167
#3 Living with Parents  1752 0.0254466231  0.070900362 0.0002797680
#4              Others   196 0.0028467683 -0.519009305 0.0008883758
#5               Owned 13809 0.2005664488 -0.003312354 0.0008905731
#6              Rented 51500 0.7480029049 -0.002387196 0.0008948310

# The weight attcahed to NA is negligible hence we will remove it from our analysis
cb_data <- na.omit(cb_data, cols="Type.of.residence")

#Converting "Type.of.residence" into dummies#
cb_data$Type.of.residence <- as.factor(cb_data$Type.of.residence)
residence_dummy <- data.frame(model.matrix( ~Type.of.residence, data = cb_data))

#Removing the x-intercept from the newly created dataframe,#
#using the n-1 principal of level creation and keeping the rest 4 variables#
residence_dummy <- residence_dummy[,-1]

#Merging the dummy variables to the main data set, after removing the original column#
cb_data <- cbind(cb_data[,-23], residence_dummy)

#-----------------------------------------------------#

# Variable - No.of.months.in.current.residence

# we will first change the column header
colnames(cb_data)[colnames(cb_data)=="No.of.months.in.current.residence"] <- "Mth_Curr_Residence"

# There are 122 levels and no NA present for Months in current residence.
# Below is the WOE for months in current residence.
#No.of.months.in.current.residence     N    Percent         WOE         IV
#1                             [6,9] 34130 0.49571532 -0.27546546 0.03321627
#2                           [10,28]  6819 0.09904139  0.50439874 0.06510569
#3                           [29,49]  7136 0.10364561  0.30271455 0.07603484
#4                           [50,72]  6920 0.10050835  0.13911541 0.07810875
#5                           [73,97]  6836 0.09928831  0.13284527 0.07997154
#6                          [98,126]  7009 0.10180102 -0.07811877 0.08057104

# Let us create the bucket as is, instead of doing a coarse classing and
# recreating the bins
cb_data$Mth_in_res_binned <- cut(cb_data$Mth_Curr_Residence, c(5,9,28,49,72,97,127),include.lowest = FALSE)

# Now we will rename the levels in the newly created Mth_in_res_binned variable.
cb_data$Mth_in_res_binned <- as.character(cb_data$Mth_in_res_binned)
cb_data$Mth_in_res_binned[which(cb_data$Mth_in_res_binned == "(5,9]")] <- "6_9" 
cb_data$Mth_in_res_binned[which(cb_data$Mth_in_res_binned == "(9,28]")] <- "10_28"
cb_data$Mth_in_res_binned[which(cb_data$Mth_in_res_binned == "(28,49]")] <- "29_49"
cb_data$Mth_in_res_binned[which(cb_data$Mth_in_res_binned == "(49,72]")] <- "50_72"
cb_data$Mth_in_res_binned[which(cb_data$Mth_in_res_binned == "(72,97]")] <- "73_97"
cb_data$Mth_in_res_binned[which(cb_data$Mth_in_res_binned == "(97,127]")] <- "98_126"
cb_data$Mth_in_res_binned <- as.factor(cb_data$Mth_in_res_binned)

#Converting "Mth_in_res_binned" into dummies#
cb_data$Mth_in_res_binned <- as.factor(cb_data$Mth_in_res_binned)
mthsinresi_dummy <- data.frame(model.matrix( ~Mth_in_res_binned, data = cb_data))

#Removing the x-intercept from the newly created dataframe,#
#using the n-1 principal of level creation and keeping the rest 4 variables#
mthsinresi_dummy <- mthsinresi_dummy[,-1]

#Merging the dummy variables to the main data set, after removing the original column#
cb_data <- cbind(cb_data[,-49], mthsinresi_dummy)

#-----------------------------------------------------#

# Variable - No.of.months.in.current.company

# Checking the summary of No.of.months.in.current.company
summary(factor(cb_data$No.of.months.in.current.company))
# There are people who are there in the same company for more than 11 years
# and there are people who have joined just 3 months back.

# we will first change the column header
colnames(cb_data)[colnames(cb_data)=="No.of.months.in.current.company"] <- "Mth_Curr_Company"

#Below is the WOE for months in current company.
#No.of.months.in.current.company    N    Percent         WOE          IV
#1                            [3,5] 6540 0.09498911  0.10275968 0.001051595
#2                           [6,12] 6687 0.09712418  0.17828478 0.004403473
#3                          [13,19] 6826 0.09914306  0.20288541 0.008885433
#4                          [20,26] 6825 0.09912854  0.03721700 0.009025100
#5                          [27,33] 7017 0.10191721 -0.07930589 0.009643328
#6                          [34,40] 7097 0.10307916  0.02405783 0.009703650
#7                          [41,47] 7117 0.10336964 -0.16056042 0.012181038
#8                          [48,53] 6056 0.08795933 -0.22249456 0.016118060
#9                          [54,61] 7693 0.11173566 -0.22917698 0.021408390
#10                        [62,133] 6992 0.10155410  0.06330585 0.021827386

# Let us create the bucket as is, instead of doing a coarse classing and
# recreating the bins
cb_data$Mth_in_comp_binned <- cut(cb_data$Mth_Curr_Company, c(2,5,12,19,26,33,40,47,53,61,134),include.lowest = FALSE)

# Now we will rename the levels in the newly created Mth_in_res_binned variable.
cb_data$Mth_in_comp_binned <- as.character(cb_data$Mth_in_comp_binned)
cb_data$Mth_in_comp_binned[which(cb_data$Mth_in_comp_binned == "(2,5]")] <- "3_5" 
cb_data$Mth_in_comp_binned[which(cb_data$Mth_in_comp_binned == "(5,12]")] <- "6_12"
cb_data$Mth_in_comp_binned[which(cb_data$Mth_in_comp_binned == "(12,19]")] <- "13_19"
cb_data$Mth_in_comp_binned[which(cb_data$Mth_in_comp_binned == "(19,26]")] <- "20_26"
cb_data$Mth_in_comp_binned[which(cb_data$Mth_in_comp_binned == "(26,33]")] <- "27_33"
cb_data$Mth_in_comp_binned[which(cb_data$Mth_in_comp_binned == "(33,40]")] <- "34_40"
cb_data$Mth_in_comp_binned[which(cb_data$Mth_in_comp_binned == "(40,47]")] <- "41_47"
cb_data$Mth_in_comp_binned[which(cb_data$Mth_in_comp_binned == "(47,53]")] <- "48_53"
cb_data$Mth_in_comp_binned[which(cb_data$Mth_in_comp_binned == "(53,61]")] <- "54_61"
cb_data$Mth_in_comp_binned[which(cb_data$Mth_in_comp_binned == "(61,134]")] <- "62_133"

#Converting "Mth_in_res_binned" into dummies#
cb_data$Mth_in_comp_binned <- as.factor(cb_data$Mth_in_comp_binned)
mthsincomp_dummy <- data.frame(model.matrix( ~Mth_in_comp_binned, data = cb_data))

#Removing the x-intercept from the newly created dataframe,#
#using the n-1 principal of level creation and keeping the rest 9 variables#
mthsincomp_dummy <- mthsincomp_dummy[,-1]

#Merging the dummy variables to the main data set, after removing the original column#
cb_data <- cbind(cb_data[,-54], mthsincomp_dummy)

#-----------------------------------------------------#

# Variable - Income

# Plotting the Age histogram
ggplot(cb_data,aes(Income))+geom_histogram()
# we see that income is already scaled, hence we will not do any scaling activity.

# Let's check the outlier in the variables 
quantile(cb_data$Age,seq(0,1,0.01),na.rm = TRUE)
# Data is closely kint, no outliers found.

# Let us now look at the WOE values for Income
#Income    N    Percent          WOE          IV
#1  [-0.5,5] 6230 0.09048656  0.302447324 0.009523536
#2    [6,10] 6435 0.09346405  0.268177360 0.017134352
#3   [11,16] 7816 0.11352215  0.075544370 0.017805099
#4   [17,21] 6723 0.09764706  0.087531717 0.018583969
#5   [22,26] 6728 0.09771968  0.009223692 0.018592318
#6   [27,31] 6724 0.09766158  0.070229869 0.019089800
#7   [32,36] 6733 0.09779230 -0.135813775 0.020785561
#8   [37,41] 6620 0.09615105 -0.265573456 0.026800337
#9   [42,48] 7654 0.11116921 -0.177171606 0.030020290
#10  [49,60] 7187 0.10438635 -0.364825851 0.041813440

# Let us create the bucket as is, instead of doing a coarse classing and
# recreating the bins

cb_data$Income_binned <- cut(cb_data$Income, c(-0.6,5,10,16,21,26,31,36,41,48,61),include.lowest = FALSE)

# Now we will rename the levels in the newly created Income_binned variable.
cb_data$Income_binned <- as.character(cb_data$Income_binned)
cb_data$Income_binned[which(cb_data$Income_binned == "(-0.6,5]")] <- "-0.5_5" 
cb_data$Income_binned[which(cb_data$Income_binned == "(5,10]")] <- "6_10" 
cb_data$Income_binned[which(cb_data$Income_binned == "(10,16]")] <- "11_16" 
cb_data$Income_binned[which(cb_data$Income_binned == "(16,21]")] <- "17-21" 
cb_data$Income_binned[which(cb_data$Income_binned == "(21,26]")] <- "22_26" 
cb_data$Income_binned[which(cb_data$Income_binned == "(26,31]")] <- "27_31" 
cb_data$Income_binned[which(cb_data$Income_binned == "(31,36]")] <- "32_36" 
cb_data$Income_binned[which(cb_data$Income_binned == "(36,41]")] <- "37_41" 
cb_data$Income_binned[which(cb_data$Income_binned == "(41,48]")] <- "42_48" 
cb_data$Income_binned[which(cb_data$Income_binned == "(48,61]")] <- "49_60"

#Converting "Income_binned" into dummies#
cb_data$Income_binned <- as.factor(cb_data$Income_binned)
income_dummy <- data.frame(model.matrix( ~Income_binned, data = cb_data))

#Removing the x-intercept from the newly created dataframe,#
#using the n-1 principal of level creation and keeping the rest 9 variables#
income_dummy <- income_dummy[,-1]

#Merging the dummy variables to the main data set, after removing the original column#
cb_data <- cbind(cb_data[,-63], income_dummy)

#-----------------------------------------------------#
#     MODEL BUILDING DEMOGRAPHIC DATA (MODEL-1)
#-----------------------------------------------------#

# Taking only the variables that were there in demographic data
demographic <- cb_data[, c(25,20,21,26:71)]
demographic$Performance.Tag.y <- as.factor(demographic$Performance.Tag.y)

#-----------------------------------------------------------------#
# Eye balling the correlation among all the variables#
#-----------------------------------------------------------------#

#Checking if the correlation matrix to check on insights.
correlate <- cor(demographic)
View(correlate)

#-----------------------------------------------------------------#
# Dividing demographic into two datasets, train and test datasets
#-----------------------------------------------------------------#

#Setting the seed to 50# 
set.seed(50)

#Now we randomly generate row indices for train dataset
trainindices <- sample(1:nrow(demographic), 0.7*nrow(demographic))

#Generating the train data set
demographic_train <- demographic[trainindices,]

#Storing the rest of the observations into an object "demographic_test".
demographic_test <- demographic[-trainindices,]

#----------------------------------------------------------------#
#                    Model creation starts
#----------------------------------------------------------------#

# Creating the first classification model from the training data set#
demographicmodel_1 <- glm(Performance.Tag.y ~ ., data = demographic_train, family = "binomial")

# Checking the summary of model 
summary(demographicmodel_1)

# Creating the second model using Stepwise selection
demographicmodel_2 <- stepAIC(demographicmodel_1, direction="both")
summary(demographicmodel_2)
vif(demographicmodel_2)

# removing ProfessionSE as it has high p-value
demographicmodel_3 <- glm(formula = Performance.Tag.y ~ No.of.dependents2 + No.of.dependents4 + 
                            EducationOthers + Mth_in_res_binned29_49 + 
                            Mth_in_res_binned50_72 + Mth_in_res_binned6_9 + Mth_in_res_binned73_97 + 
                            Mth_in_res_binned98_126 + Mth_in_comp_binned20_26 + Mth_in_comp_binned27_33 + 
                            Mth_in_comp_binned34_40 + Mth_in_comp_binned41_47 + Mth_in_comp_binned48_53 + 
                            Mth_in_comp_binned54_61 + Mth_in_comp_binned62_133 + Income_binned11_16 + 
                            Income_binned17.21 + Income_binned22_26 + Income_binned27_31 + 
                            Income_binned32_36 + Income_binned37_41 + Income_binned42_48 + 
                            Income_binned49_60, family = "binomial", data = demographic_train)
summary(demographicmodel_3)
vif(demographicmodel_3)

# removing EducationOthers as it has high p-value
demographicmodel_4 <- glm(formula = Performance.Tag.y ~ No.of.dependents2 + No.of.dependents4 + 
                            Mth_in_res_binned29_49 + Mth_in_res_binned50_72 + 
                            Mth_in_res_binned6_9 + Mth_in_res_binned73_97 + Mth_in_res_binned98_126 + 
                            Mth_in_comp_binned20_26 + Mth_in_comp_binned27_33 + Mth_in_comp_binned34_40 + 
                            Mth_in_comp_binned41_47 + Mth_in_comp_binned48_53 + Mth_in_comp_binned54_61 + 
                            Mth_in_comp_binned62_133 + Income_binned11_16 + Income_binned17.21 + 
                            Income_binned22_26 + Income_binned27_31 + Income_binned32_36 + 
                            Income_binned37_41 + Income_binned42_48 + Income_binned49_60, 
                          family = "binomial", data = demographic_train)
summary(demographicmodel_4)
vif(demographicmodel_4)

# removing Mth_in_comp_binned20_26 as it has high p-value
demographicmodel_5 <- glm(formula = Performance.Tag.y ~ No.of.dependents2 + No.of.dependents4 + 
                            Mth_in_res_binned29_49 + Mth_in_res_binned50_72 + Mth_in_res_binned6_9 + 
                            Mth_in_res_binned73_97 + Mth_in_res_binned98_126 + 
                            Mth_in_comp_binned27_33 + Mth_in_comp_binned34_40 + Mth_in_comp_binned41_47 + 
                            Mth_in_comp_binned48_53 + Mth_in_comp_binned54_61 + Mth_in_comp_binned62_133 + 
                            Income_binned11_16 + Income_binned17.21 + Income_binned22_26 + 
                            Income_binned27_31 + Income_binned32_36 + Income_binned37_41 + 
                            Income_binned42_48 + Income_binned49_60, family = "binomial", 
                          data = demographic_train)
summary(demographicmodel_5)
vif(demographicmodel_5)

# removing Mth_in_comp_binned34_40 as it has high p-value
demographicmodel_6 <- glm(formula = Performance.Tag.y ~ No.of.dependents2 + No.of.dependents4 + 
                            Mth_in_res_binned29_49 + Mth_in_res_binned50_72 + Mth_in_res_binned6_9 + 
                            Mth_in_res_binned73_97 + Mth_in_res_binned98_126 + Mth_in_comp_binned27_33 + 
                            Mth_in_comp_binned41_47 + Mth_in_comp_binned48_53 + 
                            Mth_in_comp_binned54_61 + Mth_in_comp_binned62_133 + Income_binned11_16 + 
                            Income_binned17.21 + Income_binned22_26 + Income_binned27_31 + 
                            Income_binned32_36 + Income_binned37_41 + Income_binned42_48 + 
                            Income_binned49_60, family = "binomial", data = demographic_train)
summary(demographicmodel_6)
vif(demographicmodel_6)

# removing No.of.dependents2 as it has high p-value
demographicmodel_7 <- glm(formula = Performance.Tag.y ~ No.of.dependents4 + 
                            Mth_in_res_binned29_49 + Mth_in_res_binned50_72 + Mth_in_res_binned6_9 + 
                            Mth_in_res_binned73_97 + Mth_in_res_binned98_126 + Mth_in_comp_binned27_33 + 
                            Mth_in_comp_binned41_47 + Mth_in_comp_binned48_53 + Mth_in_comp_binned54_61 + 
                            Mth_in_comp_binned62_133 + Income_binned11_16 + Income_binned17.21 + 
                            Income_binned22_26 + Income_binned27_31 + Income_binned32_36 + 
                            Income_binned37_41 + Income_binned42_48 + Income_binned49_60, 
                          family = "binomial", data = demographic_train)
summary(demographicmodel_7)
vif(demographicmodel_7)

# removing No.of.dependents4 as it has high p-value
demographicmodel_8 <- glm(formula = Performance.Tag.y ~ Mth_in_res_binned29_49 + 
                            Mth_in_res_binned50_72 + Mth_in_res_binned6_9 + Mth_in_res_binned73_97 + 
                            Mth_in_res_binned98_126 + Mth_in_comp_binned27_33 + Mth_in_comp_binned41_47 + 
                            Mth_in_comp_binned48_53 + Mth_in_comp_binned54_61 + Mth_in_comp_binned62_133 + 
                            Income_binned11_16 + Income_binned17.21 + Income_binned22_26 + 
                            Income_binned27_31 + Income_binned32_36 + Income_binned37_41 + 
                            Income_binned42_48 + Income_binned49_60, family = "binomial", 
                          data = demographic_train)
summary(demographicmodel_8)
vif(demographicmodel_8)

# removing Mth_in_res_binned29_49 as it has high p-value
demographicmodel_9 <- glm(formula = Performance.Tag.y ~  Mth_in_res_binned50_72 + 
                            Mth_in_res_binned6_9 + Mth_in_res_binned73_97 + Mth_in_res_binned98_126 + 
                            Mth_in_comp_binned27_33 + Mth_in_comp_binned41_47 + Mth_in_comp_binned48_53 + 
                            Mth_in_comp_binned54_61 + Mth_in_comp_binned62_133 + Income_binned11_16 + 
                            Income_binned17.21 + Income_binned22_26 + Income_binned27_31 + 
                            Income_binned32_36 + Income_binned37_41 + Income_binned42_48 + 
                            Income_binned49_60, family = "binomial", data = demographic_train)
summary(demographicmodel_9)
vif(demographicmodel_9)

# removing Mth_in_res_binned73_97 as it has high p-value
demographicmodel_10 <- glm(formula = Performance.Tag.y ~ Mth_in_res_binned50_72 + 
                             Mth_in_res_binned6_9 + Mth_in_res_binned98_126 + 
                             Mth_in_comp_binned27_33 + Mth_in_comp_binned41_47 + Mth_in_comp_binned48_53 + 
                             Mth_in_comp_binned54_61 + Mth_in_comp_binned62_133 + Income_binned11_16 + 
                             Income_binned17.21 + Income_binned22_26 + Income_binned27_31 + 
                             Income_binned32_36 + Income_binned37_41 + Income_binned42_48 + 
                             Income_binned49_60, family = "binomial", data = demographic_train)
summary(demographicmodel_10)
vif(demographicmodel_10)

# removing Income_binned27_31 as it has high p-value
demographicmodel_11 <- glm(formula = Performance.Tag.y ~ Mth_in_res_binned50_72 + Mth_in_res_binned6_9 + 
                             Mth_in_res_binned98_126 + Mth_in_comp_binned27_33 + Mth_in_comp_binned41_47 + 
                             Mth_in_comp_binned48_53 + Mth_in_comp_binned54_61 + Mth_in_comp_binned62_133 + 
                             Income_binned11_16 + Income_binned17.21 + Income_binned22_26 + 
                             Income_binned32_36 + Income_binned37_41 + 
                             Income_binned42_48 + Income_binned49_60, family = "binomial", 
                           data = demographic_train)
summary(demographicmodel_11)
vif(demographicmodel_11)

# removing Income_binned17.21 as it has high p-value
demographicmodel_12 <- glm(formula = Performance.Tag.y ~ Mth_in_res_binned50_72 + Mth_in_res_binned6_9 + 
                             Mth_in_res_binned98_126 + Mth_in_comp_binned27_33 + Mth_in_comp_binned41_47 + 
                             Mth_in_comp_binned48_53 + Mth_in_comp_binned54_61 + Mth_in_comp_binned62_133 + 
                             Income_binned11_16 + Income_binned22_26 + 
                             Income_binned32_36 + Income_binned37_41 + Income_binned42_48 + 
                             Income_binned49_60, family = "binomial", data = demographic_train)
summary(demographicmodel_12)
vif(demographicmodel_12)

# removing Income_binned11_16 as it has high p-value
demographicmodel_13 <- glm(formula = Performance.Tag.y ~ Mth_in_res_binned50_72 + Mth_in_res_binned6_9 + 
                             Mth_in_res_binned98_126 + Mth_in_comp_binned27_33 + Mth_in_comp_binned41_47 + 
                             Mth_in_comp_binned48_53 + Mth_in_comp_binned54_61 + Mth_in_comp_binned62_133 + 
                             Income_binned22_26 + Income_binned32_36 + 
                             Income_binned37_41 + Income_binned42_48 + Income_binned49_60, 
                           family = "binomial", data = demographic_train)
summary(demographicmodel_13)
vif(demographicmodel_13)

# removing Income_binned22_26 as it has high p-value
demographicmodel_14 <- glm(formula = Performance.Tag.y ~ Mth_in_res_binned50_72 + Mth_in_res_binned6_9 + 
                             Mth_in_res_binned98_126 + Mth_in_comp_binned27_33 + Mth_in_comp_binned41_47 + 
                             Mth_in_comp_binned48_53 + Mth_in_comp_binned54_61 + Mth_in_comp_binned62_133 + 
                             Income_binned32_36 + Income_binned37_41 + 
                             Income_binned42_48 + Income_binned49_60, family = "binomial", 
                           data = demographic_train)
summary(demographicmodel_14)
vif(demographicmodel_14)

# removing Mth_in_comp_binned62_133 as it has high p-value
demographicmodel_15 <- glm(formula = Performance.Tag.y ~ Mth_in_res_binned50_72 + Mth_in_res_binned6_9 + 
                             Mth_in_res_binned98_126 + Mth_in_comp_binned27_33 + Mth_in_comp_binned41_47 + 
                             Mth_in_comp_binned48_53 + Mth_in_comp_binned54_61 + 
                             Income_binned32_36 + Income_binned37_41 + Income_binned42_48 + 
                             Income_binned49_60, family = "binomial", data = demographic_train)
summary(demographicmodel_15)
vif(demographicmodel_15)

# removing Mth_in_res_binned50_72 as it has high p-value and VIF
demographicmodel_16 <- glm(formula = Performance.Tag.y ~ Mth_in_res_binned6_9 + 
                             Mth_in_res_binned98_126 + Mth_in_comp_binned27_33 + Mth_in_comp_binned41_47 + 
                             Mth_in_comp_binned48_53 + Mth_in_comp_binned54_61 + Income_binned32_36 + 
                             Income_binned37_41 + Income_binned42_48 + Income_binned49_60, 
                           family = "binomial", data = demographic_train)
summary(demographicmodel_16)
vif(demographicmodel_16)

# We should consider demographicmodel_16 while taking only demographic
# variables into consideration. The most important variables are
# Mth_in_res_binned6_9,Mth_in_res_binned98_126,Mth_in_comp_binned27_33,
# Mth_in_comp_binned41_47,Mth_in_comp_binned48_53,Mth_in_comp_binned54_61,
# Income_binned32_36,Income_binned37_41,Income_binned42_48,Income_binned49_60

#-----------------------------------------------------#
#        MODEL EVALUATION DEMOGRAPHIC DATA 
#-----------------------------------------------------#

# predicting probabilities of Performance TAG for test data
demo_test_pred <- predict(demographicmodel_16, demographic_test[,-1],type = "response")
summary(demo_test_pred)
demographic_test$prob <- demo_test_pred

# probability greater than .5 is 1 (customer will default)
test_pred_ptag_50 <- factor(ifelse(demo_test_pred >= 0.50, 1,0))

# confusion matrix
demo_test_conf <- confusionMatrix(test_pred_ptag_50, demographic_test$Performance.Tag.y, positive = "1")
demo_test_conf

#Sensitivity : 0.0000         
#Specificity : 1.0000      
#Accuracy : 0.9572

# We see that Sensitivity is zero and Specificity is 1 when cutoff is 0.50,
# hence we need to create a function to understand the optimal probability cutoff

#-----------------------------------------------------#

# compute optimal probalility cutoff for better model reliability
perform_fn_demo <- function(cutoff) 
{
  test_pred_ptag_50 <- factor(ifelse(demo_test_pred >= cutoff, 1,0))
  demo_test_conf <- confusionMatrix(test_pred_ptag_50, demographic_test$Performance.Tag.y, positive = "1")
  acc <- demo_test_conf$overall[1]
  sens <- demo_test_conf$byClass[1]
  spec <- demo_test_conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values from 0.01 to 0.99 for plotting and initiallizing a matrix of 1000 X 4.
s = seq(.01,.99,length=100)
OUT = matrix(0,100,3)

for(i in 1:100)
{
  OUT[i,] = perform_fn_demo(s[i])
}

#-----------------------------------------------------#

# plotting sensitivity, specificity and accuracy with different values of probability
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

# finding cutoff probability for threshold value above which represents that customer will default
cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]

# probability greater than .04 is 1 (customer will default)
test_pred_ptag <- factor(ifelse(demo_test_pred >= 0.0429, 1,0))

# confusion matrix with cutoff value equal 0.0429
demo_conf_final <- confusionMatrix(test_pred_ptag, demographic_test$Performance.Tag.y, positive = "1")
demo_conf_final

#Sensitivity : 0.51814         
#Specificity : 0.63539      
#Accuracy : 0.6304

# These results clearly look better now, but Sensitivity is at 51%.
# We will have to add more variables to our analysis to get better results.

#-----------------------------------------------------#
#-----------------------------------------------------#
#-----------------------------------------------------#

#-----------------------------------------------------#
#   VARIABLE TRANSFORMATION - CREDIT DATA
#-----------------------------------------------------#

# Variable - No of times 90 DPD or worse in last 6 months
levels(factor(cb_data$No.of.times.90.DPD.or.worse.in.last.6.months))

# There are 4 levels (0 to 3) in this variable, now lets plot a histogram and check.

cb_data$No.of.times.90.DPD.or.worse.in.last.6.months <- as.numeric(cb_data$No.of.times.90.DPD.or.worse.in.last.6.months)
ggplot(cb_data, aes(No.of.times.90.DPD.or.worse.in.last.6.months))+geom_histogram()

# The plot shows that most of the applicants have not defaulted when it comes to this
# time frame, lets now see the summary of this variable
summary(factor(cb_data$No.of.times.90.DPD.or.worse.in.last.6.months))

# 0     1     2     3 
# 53673 13205  1765   207

# Almost 78% of times applicant have not defaulted.

#No.of.times.90.DPD.or.worse.in.last.6.months     N   Percent        WOE         IV
#1                                        [0,0] 53673 0.7795692 -0.2665577 0.04910681
#2                                        [1,3] 15177 0.2204308  0.6239303 0.16405085


# For this variable we see that WOE is monotonic (it has a increasing pattern).
# First we will bin the variable according to the above table then
# We will convert the variable to factor, then change the levels with WOE values.

cb_data$No.of.times.90.DPD.or.worse.in.last.6.months <- as.numeric(cb_data$No.of.times.90.DPD.or.worse.in.last.6.months)
cb_data$No.of.times.90.DPD.or.worse.in.last.6.months[which(cb_data$No.of.times.90.DPD.or.worse.in.last.6.months == 0)] <- '0'
cb_data$No.of.times.90.DPD.or.worse.in.last.6.months[which(cb_data$No.of.times.90.DPD.or.worse.in.last.6.months >= 1 & cb_data$No.of.times.90.DPD.or.worse.in.last.6.months <= 3)] <- '1_3'

#Converting values to numeric so as to replace the levels- 0 and 1_3 with 1 and 0#
cb_data$No.of.times.90.DPD.or.worse.in.last.6.months <- as.factor(cb_data$No.of.times.90.DPD.or.worse.in.last.6.months)
levels(cb_data$No.of.times.90.DPD.or.worse.in.last.6.months)<-c(0,1)

#Storing the numeric values in the same variable#
cb_data$No.of.times.90.DPD.or.worse.in.last.6.months <- as.numeric(levels(cb_data$No.of.times.90.DPD.or.worse.in.last.6.months))[cb_data$No.of.times.90.DPD.or.worse.in.last.6.months]

# Checking the summary of variable after conversion#
summary(factor(cb_data$No.of.times.90.DPD.or.worse.in.last.6.months))

# 0     1 
# 53554 15134

#-----------------------------------------------------#

# Variable - No of times 60 DPD or worse in last 6 months
levels(factor(cb_data$No.of.times.60.DPD.or.worse.in.last.6.months))

# There are 6 levels (0 to 5) in this variable, now lets check the summary of this variable.
summary(factor(cb_data$No.of.times.60.DPD.or.worse.in.last.6.months))

# 0     1     2     3     4     5 
# 50883 11127  4905  1461   404    70

# Now lets plot a box plot to check outliers
cb_data$No.of.times.60.DPD.or.worse.in.last.6.months <- as.numeric(cb_data$No.of.times.60.DPD.or.worse.in.last.6.months)
boxplot(cb_data$No.of.times.60.DPD.or.worse.in.last.6.months)

# Anything above 4 should be considered as an outlier.
# Lets do a QQ analysis to check our assumption on outliers

quantile(cb_data$No.of.times.60.DPD.or.worse.in.last.6.months,seq(0,1,.01))

# We see that 99% of the data fall inside 4 times defaulted criteria.

#No.of.times.60.DPD.or.worse.in.last.6.months     N   Percent        WOE         IV
#1                                        [0,0] 50883 0.7390591 -0.3435224 0.07472725
#2                                        [1,5] 17967 0.2609409  0.6227167 0.21018832

# For this variable we see that WOE is monotonic (it has a increasing pattern).
# First we will bin the variable according to the above table then
# We will convert the variable to factor, then change the levels with WOE values.

cb_data$No.of.times.60.DPD.or.worse.in.last.6.months <- as.numeric(cb_data$No.of.times.60.DPD.or.worse.in.last.6.months)
cb_data$No.of.times.60.DPD.or.worse.in.last.6.months[which(cb_data$No.of.times.60.DPD.or.worse.in.last.6.months == 0)] <- '0'
cb_data$No.of.times.60.DPD.or.worse.in.last.6.months[which(cb_data$No.of.times.60.DPD.or.worse.in.last.6.months >= 1 & cb_data$No.of.times.60.DPD.or.worse.in.last.6.months <= 5)] <- '1_5'

#Converting values to numeric so as to replace the levels- 0 and 1_5 with 1 and 0#
cb_data$No.of.times.60.DPD.or.worse.in.last.6.months <- as.factor(cb_data$No.of.times.60.DPD.or.worse.in.last.6.months)
levels(cb_data$No.of.times.60.DPD.or.worse.in.last.6.months)<-c(0,1)

#Storing the numeric values in the same variable#
cb_data$No.of.times.60.DPD.or.worse.in.last.6.months <- as.numeric(levels(cb_data$No.of.times.60.DPD.or.worse.in.last.6.months))[cb_data$No.of.times.60.DPD.or.worse.in.last.6.months]

# Checking the summary of variable after conversion#
summary(factor(cb_data$No.of.times.60.DPD.or.worse.in.last.6.months))

# 0     1 
# 50773 17915

#-----------------------------------------------------#

# Variable - No of times 30 DPD or worse in last 6 months
levels(factor(cb_data$No.of.times.30.DPD.or.worse.in.last.6.months))

# There are 8 levels (0 to 7) in this variable, now lets check the summary of this variable.
summary(factor(cb_data$No.of.times.30.DPD.or.worse.in.last.6.months))

# 0     1     2     3     4     5     6     7 
# 49111  9500  5890  2824  1036   379    95    15

# Now lets plot a box plot to check outliers
cb_data$No.of.times.30.DPD.or.worse.in.last.6.months <- as.numeric(cb_data$No.of.times.30.DPD.or.worse.in.last.6.months)
boxplot(cb_data$No.of.times.30.DPD.or.worse.in.last.6.months)

# Anything above 4 should be considered as an outlier.
# Lets do a QQ analysis to check our assumption on outliers

quantile(cb_data$No.of.times.30.DPD.or.worse.in.last.6.months,seq(0,1,.01))

# We see that 99% of the data fall inside 5 times defaulted criteria.

#No.of.times.30.DPD.or.worse.in.last.6.months     N   Percent        WOE         IV
#1                                        [0,0] 49111 0.7133208 -0.3957417 0.09354591
#2                                        [1,1]  9500 0.1379726  0.4675809 0.13106210
#3                                        [2,7] 10239 0.1487066  0.7412817 0.24681069

# For this variable we see that WOE is monotonic (it has a increasing pattern).
# First we will bin the variable according to the above table then
# We will convert the variable to factor, then change the levels with WOE values.

cb_data$No.of.times.30.DPD.or.worse.in.last.6.months <- as.numeric(cb_data$No.of.times.30.DPD.or.worse.in.last.6.months)
cb_data$No.of.times.30.DPD.or.worse.in.last.6.months[which(cb_data$No.of.times.30.DPD.or.worse.in.last.6.months == 0)] <- '0'
cb_data$No.of.times.30.DPD.or.worse.in.last.6.months[which(cb_data$No.of.times.30.DPD.or.worse.in.last.6.months == 1)] <- '1'
cb_data$No.of.times.30.DPD.or.worse.in.last.6.months[which(cb_data$No.of.times.30.DPD.or.worse.in.last.6.months >= 2 & cb_data$No.of.times.30.DPD.or.worse.in.last.6.months <= 7)] <- '2_7'

#Converting this variable into dummies#
cb_data$No.of.times.30.DPD.or.worse.in.last.6.months <- as.factor(cb_data$No.of.times.30.DPD.or.worse.in.last.6.months)
dpd30_60_mth_dummy <- data.frame(model.matrix( ~No.of.times.30.DPD.or.worse.in.last.6.months, data = cb_data))

#Removing the x-intercept from the newly created dataframe,#
#using the n-1 principal of level creation and keeping the rest 2 variables#
dpd30_60_mth_dummy  <- dpd30_60_mth_dummy [,-1]

#Merging the dummy variables to the main data set, after removing the original column#
cb_data <- cbind(cb_data[,-4], dpd30_60_mth_dummy)

#-----------------------------------------------------#

# Variable - No of times 90 DPD or worse in last 12 months
levels(factor(cb_data$No.of.times.90.DPD.or.worse.in.last.12.months))

# There are 6 levels (0 to 5) in this variable, now lets check the summary of this variable.
summary(factor(cb_data$No.of.times.90.DPD.or.worse.in.last.12.months))

# 0     1     2     3     4     5 
# 49507 11653  6153  1235   267    35

# Now lets plot a box plot to check outliers
cb_data$No.of.times.90.DPD.or.worse.in.last.12.months <- as.numeric(cb_data$No.of.times.90.DPD.or.worse.in.last.12.months)
boxplot(cb_data$No.of.times.90.DPD.or.worse.in.last.12.months)

# Anything above 4 should be considered as an outlier.
# Lets do a QQ analysis to check our assumption on outliers

quantile(cb_data$No.of.times.90.DPD.or.worse.in.last.12.months,seq(0,1,.01))

# We see that 99% of the data fall inside 4 times defaulted criteria.

#No.of.times.90.DPD.or.worse.in.last.12.months     N   Percent        WOE         IV
#1                                         [0,0] 49507 0.7190582 -0.3646322 0.08115669
#2                                         [1,1] 11653 0.1692594  0.5084746 0.13664682
#3                                         [2,5]  7690 0.1116824  0.7232622 0.21869426

# For this variable we see that WOE is monotonic (it has a increasing pattern).
# First we will bin the variable according to the above table then
# We will convert the variable to factor, then change the levels with WOE values.

cb_data$No.of.times.90.DPD.or.worse.in.last.12.months <- as.numeric(cb_data$No.of.times.90.DPD.or.worse.in.last.12.months)
cb_data$No.of.times.90.DPD.or.worse.in.last.12.months[which(cb_data$No.of.times.90.DPD.or.worse.in.last.12.months == 0)] <- '0'
cb_data$No.of.times.90.DPD.or.worse.in.last.12.months[which(cb_data$No.of.times.90.DPD.or.worse.in.last.12.months == 1)] <- '1'
cb_data$No.of.times.90.DPD.or.worse.in.last.12.months[which(cb_data$No.of.times.90.DPD.or.worse.in.last.12.months >= 2 & cb_data$No.of.times.90.DPD.or.worse.in.last.12.months <= 5)] <- '2_5'

#Converting this variable into dummies#
cb_data$No.of.times.90.DPD.or.worse.in.last.12.months <- as.factor(cb_data$No.of.times.90.DPD.or.worse.in.last.12.months)
dpd90_12_mth_dummy <- data.frame(model.matrix( ~cb_data$No.of.times.90.DPD.or.worse.in.last.12.months, data = cb_data))

#Removing the x-intercept from the newly created dataframe,#
#using the n-1 principal of level creation and keeping the rest 2 variables#
dpd90_12_mth_dummy  <- dpd90_12_mth_dummy [,-1]

#Merging the dummy variables to the main data set, after removing the original column#
cb_data <- cbind(cb_data[,-4], dpd90_12_mth_dummy)

#-----------------------------------------------------#

# Variable - No of times 60 DPD or worse in last 12 months
levels(factor(cb_data$No.of.times.60.DPD.or.worse.in.last.12.months))

# There are 8 levels (0 to 7) in this variable, now lets check the summary of this variable.
summary(factor(cb_data$No.of.times.60.DPD.or.worse.in.last.12.months))

# 0     1     2     3     4     5     6     7 
# 44967 12727  6414  3192  1040   393   110     7

# Now lets plot a box plot to check outliers
cb_data$No.of.times.60.DPD.or.worse.in.last.12.months <- as.numeric(cb_data$No.of.times.60.DPD.or.worse.in.last.12.months)
boxplot(cb_data$No.of.times.60.DPD.or.worse.in.last.12.months)

# Anything above 4 should be considered as an outlier.
# Lets do a QQ analysis to check our assumption on outliers

quantile(cb_data$No.of.times.60.DPD.or.worse.in.last.12.months,seq(0,1,.01))

# We see that 99% of the data fall inside 5 times defaulted criteria.

#No.of.times.60.DPD.or.worse.in.last.12.months     N   Percent        WOE         IV
#1                                         [0,0] 44967 0.6531294 -0.3596125 0.07185858
#2                                         [1,1] 12727 0.1848592  0.2149501 0.08129187
#3                                         [2,7] 11156 0.1620114  0.6947327 0.18962592

# For this variable we see that WOE is monotonic (it has a increasing pattern).
# First we will bin the variable according to the above table then
# We will convert the variable to factor, then change the levels with WOE values.

cb_data$No.of.times.60.DPD.or.worse.in.last.12.months <- as.numeric(cb_data$No.of.times.60.DPD.or.worse.in.last.12.months)
cb_data$No.of.times.60.DPD.or.worse.in.last.12.months[which(cb_data$No.of.times.60.DPD.or.worse.in.last.12.months == 0)] <- '0'
cb_data$No.of.times.60.DPD.or.worse.in.last.12.months[which(cb_data$No.of.times.60.DPD.or.worse.in.last.12.months == 1)] <- '1'
cb_data$No.of.times.60.DPD.or.worse.in.last.12.months[which(cb_data$No.of.times.60.DPD.or.worse.in.last.12.months >= 2 & cb_data$No.of.times.60.DPD.or.worse.in.last.12.months <= 7)] <- '2_7'

#Converting this variable into dummies#
cb_data$No.of.times.60.DPD.or.worse.in.last.12.months <- as.factor(cb_data$No.of.times.60.DPD.or.worse.in.last.12.months)
dpd60_12_mth_dummy <- data.frame(model.matrix( ~No.of.times.60.DPD.or.worse.in.last.12.months, data = cb_data))

#Removing the x-intercept from the newly created dataframe,#
#using the n-1 principal of level creation and keeping the rest 2 variables#
dpd60_12_mth_dummy  <- dpd60_12_mth_dummy [,-1]

#Merging the dummy variables to the main data set, after removing the original column#
cb_data <- cbind(cb_data[,-4], dpd60_12_mth_dummy)

#-----------------------------------------------------#

# Variable - No of times 30 DPD or worse in last 12 months
levels(factor(cb_data$No.of.times.30.DPD.or.worse.in.last.12.months))

# There are 10 levels (0 to 9) in this variable, now lets check the summary of this variable.
summary(factor(cb_data$No.of.times.30.DPD.or.worse.in.last.12.months))

# 0     1     2     3     4     5     6     7     8     9 
# 43958 11386  6112  4132  1918   846   368   106    23     1

# Now lets plot a box plot to check outliers
cb_data$No.of.times.30.DPD.or.worse.in.last.12.months <- as.numeric(cb_data$No.of.times.30.DPD.or.worse.in.last.12.months)
boxplot(cb_data$No.of.times.30.DPD.or.worse.in.last.12.months)

# Anything above 4 should be considered as an outlier.
# Lets do a QQ analysis to check our assumption on outliers

quantile(cb_data$No.of.times.30.DPD.or.worse.in.last.12.months,seq(0,1,.01))

# We see that 99% of the data fall inside 6 times defaulted criteria.

#No.of.times.30.DPD.or.worse.in.last.12.months     N   Percent        WOE         IV
#1                                         [0,0] 43958 0.6384737 -0.3858830 0.07995556
#2                                         [1,2] 17498 0.2541432  0.2843118 0.10339208
#3                                         [3,9]  7394 0.1073830  0.7982725 0.20299181

# For this variable we see that WOE is monotonic (it has a increasing pattern).
# First we will bin the variable according to the above table then
# We will convert the variable to factor, then change the levels with WOE values.

cb_data$No.of.times.30.DPD.or.worse.in.last.12.months <- as.numeric(cb_data$No.of.times.30.DPD.or.worse.in.last.12.months)
cb_data$No.of.times.30.DPD.or.worse.in.last.12.months[which(cb_data$No.of.times.30.DPD.or.worse.in.last.12.months == 0)] <- '0'
cb_data$No.of.times.30.DPD.or.worse.in.last.12.months[which(cb_data$No.of.times.30.DPD.or.worse.in.last.12.months >= 1 & cb_data$No.of.times.30.DPD.or.worse.in.last.12.months <= 2)] <- '1_2'
cb_data$No.of.times.30.DPD.or.worse.in.last.12.months[which(cb_data$No.of.times.30.DPD.or.worse.in.last.12.months >= 3 & cb_data$No.of.times.30.DPD.or.worse.in.last.12.months <= 9)] <- '3_9'

#Converting this variable into dummies#
cb_data$No.of.times.30.DPD.or.worse.in.last.12.months <- as.factor(cb_data$No.of.times.30.DPD.or.worse.in.last.12.months)
dpd30_12_mth_dummy <- data.frame(model.matrix( ~No.of.times.30.DPD.or.worse.in.last.12.months, data = cb_data))

#Removing the x-intercept from the newly created dataframe,#
#using the n-1 principal of level creation and keeping the rest 2 variables#
dpd30_12_mth_dummy  <- dpd30_12_mth_dummy [,-1]

#Merging the dummy variables to the main data set, after removing the original column#
cb_data <- cbind(cb_data[,-4], dpd30_12_mth_dummy)

#-----------------------------------------------------#
#-----------------------------------------------------#

# The upcoming variables do not have a monotonic behaviour, so we will have
# to do a coarse classing for each of the variables. Coarse classing is nothing
# but combining similar groups. Before we do a coarse classing we need to write
# a user defined function to combine weight of evidences of similar groups.

# Lets us then create a UDF for combined WOE.

cb_data$Performance.Tag.y <- as.factor(cb_data$Performance.Tag.y)

computeWoE <- function(local_good, local_bad){
  total_good <- length(cb_data$Performance.Tag.y[which(cb_data$Performance.Tag.y == 0)])
  total_bad <- length(cb_data$Performance.Tag.y[which(cb_data$Performance.Tag.y == 1)])
  woe = log(local_good/total_good) - log(local_bad/total_bad)
  return(woe)
}

#-----------------------------------------------------#
#-----------------------------------------------------#

# Variable - Avgas CC Utilization in last 12 months
levels(factor(cb_data$Avgas.CC.Utilization.in.last.12.months))

# There are 114 levels (0 to 113) in this variable, now lets check the summary of this variable.
head(summary(factor(cb_data$Avgas.CC.Utilization.in.last.12.months)))

# First 6 from the summary function are
# 8    9    7   10  113    6 
# 3485 3482 3384 3216 3134 2974

# Lets do a QQ analysis to check the outlier.
cb_data$Avgas.CC.Utilization.in.last.12.months <- as.numeric(cb_data$Avgas.CC.Utilization.in.last.12.months)
quantile(cb_data$Avgas.CC.Utilization.in.last.12.months,seq(0,1,.01))

# We see that 95% of the data fall inside 105 times Avgas CC Utilization.
# We see that the WOE is almost monotonic till it reaches the last two bins.
# We will go ahead with the bins created by information value matrix.

#Avgas.CC.Utilization.in.last.12.months    N    Percent         WOE         IV
#1                                   [0,4] 5524 0.08023589 -0.80032795 0.03621097
#2                                   [5,6] 5471 0.07946606 -0.80007191 0.07205538
#3                                   [7,8] 6869 0.09977196 -0.79309264 0.11640557
#4                                  [9,11] 9598 0.13941058 -0.67108225 0.16310146
#5                                 [12,14] 6595 0.09579212 -0.46657144 0.18003151
#6                                 [15,21] 6854 0.09955408 -0.07757383 0.18060977
#7                                 [22,37] 7122 0.10344677  0.47647625 0.20994169
#8                                 [38,51] 6746 0.09798539  0.58601948 0.25421043
#9                                 [52,71] 7017 0.10192165  0.56516146 0.29661561
#10                               [72,113] 7051 0.10241550  0.38496013 0.31476921

colnames(cb_data)[colnames(cb_data)=="Avgas.CC.Utilization.in.last.12.months"] <- "Avg_Utility_12Mths"

cb_data$Avg_Utility_12Mths <- as.numeric(cb_data$Avg_Utility_12Mths)
cb_data$Avg_Utility_12Mths[which(cb_data$Avg_Utility_12Mths == 0 | cb_data$Avg_Utility_12Mths == 1 | cb_data$Avg_Utility_12Mths == 2 | 
                                   cb_data$Avg_Utility_12Mths == 3 | cb_data$Avg_Utility_12Mths == 4)] <- '0_4'
cb_data$Avg_Utility_12Mths[which(cb_data$Avg_Utility_12Mths == 5 | cb_data$Avg_Utility_12Mths == 6)] <- '5_6'
cb_data$Avg_Utility_12Mths[which(cb_data$Avg_Utility_12Mths == 7 | cb_data$Avg_Utility_12Mths == 8)] <- '7_8'
cb_data$Avg_Utility_12Mths[which(cb_data$Avg_Utility_12Mths == 9 | cb_data$Avg_Utility_12Mths == 10 | cb_data$Avg_Utility_12Mths == 11)] <- '9_11'
cb_data$Avg_Utility_12Mths[which(cb_data$Avg_Utility_12Mths == 12 | cb_data$Avg_Utility_12Mths == 13 | cb_data$Avg_Utility_12Mths == 14)] <- '12_14'
cb_data$Avg_Utility_12Mths[which(cb_data$Avg_Utility_12Mths == 15 | cb_data$Avg_Utility_12Mths == 16 | cb_data$Avg_Utility_12Mths == 17 | 
                                   cb_data$Avg_Utility_12Mths == 18 | cb_data$Avg_Utility_12Mths == 19 |
                                   cb_data$Avg_Utility_12Mths == 20 | cb_data$Avg_Utility_12Mths == 21)] <- '15_21'
cb_data$Avg_Utility_12Mths[which(cb_data$Avg_Utility_12Mths == 22 | cb_data$Avg_Utility_12Mths == 23 | cb_data$Avg_Utility_12Mths == 24 | 
                                   cb_data$Avg_Utility_12Mths == 25 | cb_data$Avg_Utility_12Mths == 26 | cb_data$Avg_Utility_12Mths == 27 |
                                   cb_data$Avg_Utility_12Mths == 28 | cb_data$Avg_Utility_12Mths == 29 | cb_data$Avg_Utility_12Mths == 30 |
                                   cb_data$Avg_Utility_12Mths == 31 | cb_data$Avg_Utility_12Mths == 32 | cb_data$Avg_Utility_12Mths == 33 |
                                   cb_data$Avg_Utility_12Mths == 34 | cb_data$Avg_Utility_12Mths == 35 | cb_data$Avg_Utility_12Mths == 36 |
                                   cb_data$Avg_Utility_12Mths == 37)] <- '22_37'
cb_data$Avg_Utility_12Mths[which(cb_data$Avg_Utility_12Mths == 38 | cb_data$Avg_Utility_12Mths == 39 | cb_data$Avg_Utility_12Mths == 40 | 
                                   cb_data$Avg_Utility_12Mths == 41 | cb_data$Avg_Utility_12Mths == 42 | cb_data$Avg_Utility_12Mths == 43 |
                                   cb_data$Avg_Utility_12Mths == 44 | cb_data$Avg_Utility_12Mths == 45 | cb_data$Avg_Utility_12Mths == 46 |
                                   cb_data$Avg_Utility_12Mths == 47 | cb_data$Avg_Utility_12Mths == 48 | cb_data$Avg_Utility_12Mths == 49 |
                                   cb_data$Avg_Utility_12Mths == 50 | cb_data$Avg_Utility_12Mths == 51)] <- '38_51'
cb_data$Avg_Utility_12Mths[which(cb_data$Avg_Utility_12Mths == 52 | cb_data$Avg_Utility_12Mths == 53 | cb_data$Avg_Utility_12Mths == 54 | 
                                   cb_data$Avg_Utility_12Mths == 55 | cb_data$Avg_Utility_12Mths == 56 | cb_data$Avg_Utility_12Mths == 57 |
                                   cb_data$Avg_Utility_12Mths == 58 | cb_data$Avg_Utility_12Mths == 59 | cb_data$Avg_Utility_12Mths == 60 |
                                   cb_data$Avg_Utility_12Mths == 61 | cb_data$Avg_Utility_12Mths == 62 | cb_data$Avg_Utility_12Mths == 63 |
                                   cb_data$Avg_Utility_12Mths == 64 | cb_data$Avg_Utility_12Mths == 65 | cb_data$Avg_Utility_12Mths == 66 |
                                   cb_data$Avg_Utility_12Mths == 67 | cb_data$Avg_Utility_12Mths == 68 | cb_data$Avg_Utility_12Mths == 69 |
                                   cb_data$Avg_Utility_12Mths == 70 | cb_data$Avg_Utility_12Mths == 71)] <- '52_71'
cb_data$Avg_Utility_12Mths[which(cb_data$Avg_Utility_12Mths == 72 | cb_data$Avg_Utility_12Mths == 73 | cb_data$Avg_Utility_12Mths == 74 | 
                                   cb_data$Avg_Utility_12Mths == 75 | cb_data$Avg_Utility_12Mths == 76 | cb_data$Avg_Utility_12Mths == 77 |
                                   cb_data$Avg_Utility_12Mths == 78 | cb_data$Avg_Utility_12Mths == 79 | cb_data$Avg_Utility_12Mths == 80 |
                                   cb_data$Avg_Utility_12Mths == 81 | cb_data$Avg_Utility_12Mths == 82 | cb_data$Avg_Utility_12Mths == 83 |
                                   cb_data$Avg_Utility_12Mths == 84 | cb_data$Avg_Utility_12Mths == 85 | cb_data$Avg_Utility_12Mths == 86 |
                                   cb_data$Avg_Utility_12Mths == 87 | cb_data$Avg_Utility_12Mths == 88 | cb_data$Avg_Utility_12Mths == 89 |
                                   cb_data$Avg_Utility_12Mths == 90 | cb_data$Avg_Utility_12Mths == 91 | cb_data$Avg_Utility_12Mths == 92 |
                                   cb_data$Avg_Utility_12Mths == 93 | cb_data$Avg_Utility_12Mths == 94 | cb_data$Avg_Utility_12Mths == 95 |
                                   cb_data$Avg_Utility_12Mths == 96 | cb_data$Avg_Utility_12Mths == 97 | cb_data$Avg_Utility_12Mths == 98 |
                                   cb_data$Avg_Utility_12Mths == 99 | cb_data$Avg_Utility_12Mths == 100 | cb_data$Avg_Utility_12Mths == 101 |
                                   cb_data$Avg_Utility_12Mths == 102 | cb_data$Avg_Utility_12Mths == 103 | cb_data$Avg_Utility_12Mths == 104 |
                                   cb_data$Avg_Utility_12Mths == 105 | cb_data$Avg_Utility_12Mths == 106 | cb_data$Avg_Utility_12Mths == 107 |
                                   cb_data$Avg_Utility_12Mths == 108 | cb_data$Avg_Utility_12Mths == 109 | cb_data$Avg_Utility_12Mths == 110 |
                                   cb_data$Avg_Utility_12Mths == 111 | cb_data$Avg_Utility_12Mths == 112 |
                                   cb_data$Avg_Utility_12Mths == 113)] <- '72_113'


#Converting this variable into dummies#
cb_data$Avg_Utility_12Mths <- as.factor(cb_data$Avg_Utility_12Mths)
Avg_Utility_dummy <- data.frame(model.matrix( ~Avg_Utility_12Mths, data = cb_data))

#Removing the x-intercept from the newly created dataframe,#
#using the n-1 principal of level creation and keeping the rest 9 variables#
Avg_Utility_dummy  <- Avg_Utility_dummy [,-1]

#Merging the dummy variables to the main data set, after removing the original column#
cb_data <- cbind(cb_data[,-4], Avg_Utility_dummy)

#-----------------------------------------------------#

# Variable - No of trades opened in last 6 months
levels(factor(cb_data$No.of.trades.opened.in.last.6.months))

# There are 13 levels (0 to 13) in this variable, now lets check the summary of this variable.
summary(factor(cb_data$No.of.trades.opened.in.last.6.months))

# 0     1     2     3     4     5     6     7     8     9    10    11    12 
# 11205 20116 12110  9397  6287  3661  2336  1649  1154   618   238    65    11

# Now lets plot a box plot to check outliers
cb_data$No.of.trades.opened.in.last.6.months <- as.numeric(cb_data$No.of.trades.opened.in.last.6.months)
boxplot(cb_data$No.of.trades.opened.in.last.6.months)

# Anything above 8 should be considered as an outlier.
# Lets do a QQ analysis to check our assumption on outliers

quantile(cb_data$No.of.trades.opened.in.last.6.months,seq(0,1,.01))

# We see that 99% of the data fall inside 10 trades opened in last 6 months.
# Lets do the bucketing now, this will be done according to WOE
# For this variable we see that WOE is not monotonic. 
# First we will do a coarse classing and then check on how to do the binning for this variable

#No.of.trades.opened.in.last.6.months     N    Percent        WOE         IV
#1                                [0,0] 11205 0.16275219 -0.7456037 0.06521883
#2                                [1,1] 20116 0.29218412 -0.4778296 0.11911680
#3                                [2,2] 12110 0.17589728  0.2364677 0.13008940
#4                                [3,3]  9397 0.13649106  0.4353380 0.16177399
#5                                [4,4]  6287 0.09131843  0.5224756 0.19359384
#6                               [5,12]  9732 0.14135692  0.1342192 0.19630277

# Coarse classing 4 to 12

optrade_6mths_4_12 <- cb_data[which(cb_data$No.of.trades.opened.in.last.6.months==4 | cb_data$No.of.trades.opened.in.last.6.months==5 |
                                      cb_data$No.of.trades.opened.in.last.6.months==6 | cb_data$No.of.trades.opened.in.last.6.months==7 |  
                                      cb_data$No.of.trades.opened.in.last.6.months==8 | cb_data$No.of.trades.opened.in.last.6.months==9 |
                                      cb_data$No.of.trades.opened.in.last.6.months==10 | cb_data$No.of.trades.opened.in.last.6.months==11 |
                                      cb_data$No.of.trades.opened.in.last.6.months==12),c(4,20)]
loc_good <- length(optrade_6mths_4_12$Performance.Tag.y[which(optrade_6mths_4_12$Performance.Tag.y==0)])
loc_bad <- length(optrade_6mths_4_12$Performance.Tag.y[which(optrade_6mths_4_12$Performance.Tag.y==1)])
combined_woe <- computeWoE(loc_good,loc_bad)

# We tried with coarse classing for the bins where the monotonic behaviour of WOE is breaking,
# but the trend is not becoming monotonic, hence we will go with the original binning.

cb_data$No.of.trades.opened.in.last.6.months <- as.numeric(cb_data$No.of.trades.opened.in.last.6.months)
cb_data$No.of.trades.opened.in.last.6.months[which(cb_data$No.of.trades.opened.in.last.6.months >= 5 & cb_data$No.of.trades.opened.in.last.6.months <= 12)] <- '5_12'

#Converting this variable into dummies#
cb_data$No.of.trades.opened.in.last.6.months <- as.factor(cb_data$No.of.trades.opened.in.last.6.months)
trades_op6_dummy <- data.frame(model.matrix( ~No.of.trades.opened.in.last.6.months, data = cb_data))

#Removing the x-intercept from the newly created dataframe,#
#using the n-1 principal of level creation and keeping the rest 9 variables#
trades_op6_dummy  <- trades_op6_dummy [,-1]

#Merging the dummy variables to the main data set, after removing the original column#
cb_data <- cbind(cb_data[,-4], trades_op6_dummy)

#-----------------------------------------------------#

# Variable - No of PL trades opened in last 6 months
levels(factor(cb_data$No.of.PL.trades.opened.in.last.6.months))

# There are 7 levels (0 to 6) in this variable, now lets check the summary of this variable.
summary(factor(cb_data$No.of.PL.trades.opened.in.last.6.months))

# 0     1     2     3     4     5     6 
# 30090 13539 12556  7937  3339  1090   296

# Now lets plot a box plot to check outliers
cb_data$No.of.PL.trades.opened.in.last.6.months <- as.numeric(cb_data$No.of.PL.trades.opened.in.last.6.months)
boxplot(cb_data$No.of.PL.trades.opened.in.last.6.months)

# Anything above 6 should be considered as an outlier.
# Lets do a QQ analysis to check our assumption on outliers

quantile(cb_data$No.of.PL.trades.opened.in.last.6.months,seq(0,1,.01))

# We see that 99% of the data fall inside 6 trades opened in last 6 months.
# Lets do the bucketing now, this will be done according to WOE
# For this variable we see that WOE is not monotonic. 
# First we will do a coarse classing and then check on how to do the binning for this variable

#No.of.PL.trades.opened.in.last.6.months     N   Percent        WOE        IV
#1                                   [0,0] 30090 0.4370561 -0.6796043 0.1495953
#2                                   [1,1] 13539 0.1966534  0.2028610 0.1584832
#3                                   [2,2] 12556 0.1823754  0.4379676 0.2013855
#4                                   [3,6] 12662 0.1839151  0.3603287 0.2296176

# Coarse classing 2 to 6

pltrade_6mths_2_6 <- cb_data[which(cb_data$No.of.PL.trades.opened.in.last.6.months==2 | cb_data$No.of.PL.trades.opened.in.last.6.months==3 |
                                      cb_data$No.of.PL.trades.opened.in.last.6.months==4 | cb_data$No.of.PL.trades.opened.in.last.6.months==5 |
                                      cb_data$No.of.PL.trades.opened.in.last.6.months==6),c(5,19)]
loc_good <- length(pltrade_6mths_2_6$Performance.Tag.y[which(pltrade_6mths_2_6$Performance.Tag.y==0)])
loc_bad <- length(pltrade_6mths_2_6$Performance.Tag.y[which(pltrade_6mths_2_6$Performance.Tag.y==1)])
combined_woe <- computeWoE(loc_good,loc_bad) # -0.40

# We tried with coarse classing for the bins where the monotonic behaviour of WOE is breaking,
# but the trend is not becoming monotonic, hence we will go with the original binning.

cb_data$No.of.PL.trades.opened.in.last.6.months <- as.numeric(cb_data$No.of.PL.trades.opened.in.last.6.months)
cb_data$No.of.PL.trades.opened.in.last.6.months[which(cb_data$No.of.PL.trades.opened.in.last.6.months >= 3 & cb_data$No.of.PL.trades.opened.in.last.6.months <= 6)] <- '3_6'

#Converting this variable into dummies#
cb_data$No.of.PL.trades.opened.in.last.6.months <- as.factor(cb_data$No.of.PL.trades.opened.in.last.6.months)
pltrades_op6_dummy <- data.frame(model.matrix( ~No.of.PL.trades.opened.in.last.6.months, data = cb_data))

#Removing the x-intercept from the newly created dataframe,#
#using the n-1 principal of level creation and keeping the rest 3 variables#
pltrades_op6_dummy  <- pltrades_op6_dummy [,-1]

#Merging the dummy variables to the main data set, after removing the original column#
cb_data <- cbind(cb_data[,-5], pltrades_op6_dummy)

#-----------------------------------------------------#

# Variable - No of PL trades opened in last 12 months
levels(factor(cb_data$No.of.PL.trades.opened.in.last.12.months))

# There are 13 levels in this variable, now lets plot a histogram and check. 

cb_data$No.of.PL.trades.opened.in.last.12.months <- as.numeric(cb_data$No.of.PL.trades.opened.in.last.12.months)
ggplot(cb_data, aes(No.of.PL.trades.opened.in.last.12.months))+geom_histogram()
boxplot(cb_data$No.of.PL.trades.opened.in.last.12.months)

# The plot shows that 3 & 4 trades are opened the most in the last 12 months, the box plot shows
# that anything that falls beyond 11 should be considered as outlier.
# Lets do a QQ analysis to check our assumption on outliers

quantile(cb_data$No.of.PL.trades.opened.in.last.12.months,seq(0,1,.01))

# We see that 99% of the data fall in 10 opened trades, the rest can be bucketed as 10+
# Lets do the bucketing now, this will be done according to WOE
# For this variable we see that WOE is not monotonic. 
# First we will do a coarse classing and then check on how to do the binning for this variable

#No.of.PL.trades.opened.in.last.12.months     N    Percent        WOE        IV
#1                                    [0,0] 24836 0.36074194 -0.9507795 0.2160257
#2                                    [1,1]  6640 0.09644574 -0.1294299 0.2175490
#3                                    [2,2]  6826 0.09914738  0.2504986 0.2245353
#4                                    [3,3]  8127 0.11804436  0.4162217 0.2493588
#5                                    [4,4]  7899 0.11473267  0.5020489 0.2859167
#6                                    [5,5]  6179 0.08974973  0.4238324 0.3055572
#7                                   [6,12]  8340 0.12113818  0.2386941 0.3132648

# for coarse classing, compute WOE 1,2

pl_trade_12mths_5_12 <- cb_data[which(cb_data$No.of.PL.trades.opened.in.last.12.months == 5 | cb_data$No.of.PL.trades.opened.in.last.12.months == 6 |
                                      cb_data$No.of.PL.trades.opened.in.last.12.months == 7 | cb_data$No.of.PL.trades.opened.in.last.12.months == 8 |  
                                      cb_data$No.of.PL.trades.opened.in.last.12.months == 9 | cb_data$No.of.PL.trades.opened.in.last.12.months == 10 |
                                      cb_data$No.of.PL.trades.opened.in.last.12.months == 11 | cb_data$No.of.PL.trades.opened.in.last.12.months == 12),c(5,18)]
loc_good <- length(pl_trade_12mths_5_12$Performance.Tag.y[which(pl_trade_12mths_5_12$Performance.Tag.y==0)])
loc_bad <- length(pl_trade_12mths_5_12$Performance.Tag.y[which(pl_trade_12mths_5_12$Performance.Tag.y==1)])
combined_woe <- computeWoE(loc_good,loc_bad) # -0.32

# We tried with coarse classing for the bins where the monotonic behaviour of WOE is breaking,
# but the trend is not becoming monotonic, hence we will go with the original binning.

cb_data$No.of.PL.trades.opened.in.last.12.months <- as.numeric(cb_data$No.of.PL.trades.opened.in.last.12.months)
cb_data$No.of.PL.trades.opened.in.last.12.months[which(cb_data$No.of.PL.trades.opened.in.last.12.months >= 6 & cb_data$No.of.PL.trades.opened.in.last.12.months <= 12)] <- '6_12'

#Converting this variable into dummies#
cb_data$No.of.PL.trades.opened.in.last.12.months <- as.factor(cb_data$No.of.PL.trades.opened.in.last.12.months)
pltrades_op12_dummy <- data.frame(model.matrix( ~No.of.PL.trades.opened.in.last.12.months, data = cb_data))

#Removing the x-intercept from the newly created dataframe,#
#using the n-1 principal of level creation and keeping the rest 3 variables#
pltrades_op12_dummy <- pltrades_op12_dummy [,-1]

#Merging the dummy variables to the main data set, after removing the original column#
cb_data <- cbind(cb_data[,-5], pltrades_op12_dummy)

#-----------------------------------------------------#

# Variable - No of Inquiries in last 6 months (excluding home & auto loans)
levels(factor(cb_data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.))

# There are 11 levels (0,1,2,3,4,5,6,7,8,9,10) in this variable, now lets plot a histogram and check.

cb_data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. <- as.numeric(cb_data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.)
ggplot(cb_data, aes(No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.))+geom_histogram()

# We see that there are more than 24000 customers who never enquired and almost 26000 customers
# that have enquired once and twice. We can bin this variable into different buckets.
# For this variable we see that WOE is not monotonic. 
# First we will do a coarse classing and then check on how to do the binning for this variable


#No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.     N   Percent         WOE        IV
#1                                                          [0,0] 24118 0.3503130 -0.75790991 0.1443077
#2                                                          [1,1] 13145 0.1909161  0.17777798 0.1508575
#3                                                          [2,2] 12809 0.1860212  0.21632514 0.1604781
#4                                                          [3,4] 11492 0.1669209  0.50843356 0.2151917
#5                                                         [5,10]  7286 0.1058289  0.01384596 0.2152121

# for coarse classing, compute WOE 1 to 4

exc_home_auto_6mths_1_4 <- cb_data[which(cb_data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.==1 | cb_data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.==2 |
                                         cb_data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.==3 | cb_data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.==4),c(5,17)]
loc_good <- length(exc_home_auto_6mths_1_4$Performance.Tag.y[which(exc_home_auto_6mths_1_4$Performance.Tag.y==0)])
loc_bad <- length(exc_home_auto_6mths_1_4$Performance.Tag.y[which(exc_home_auto_6mths_1_4$Performance.Tag.y==1)])
combined_woe <- computeWoE(loc_good,loc_bad) # -0.30

# After coarse classing, WOE trend is monotonic.
# First we will bin the variable according to the combined WOE values and then
# We will convert the variable to factor, then change the levels with WOE values.

temp_value_exc_home_auto_6mths <- cb_data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.
cb_data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.[which(temp_value_exc_home_auto_6mths==0)] <- '0'
cb_data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.[which(temp_value_exc_home_auto_6mths>=1 & temp_value_exc_home_auto_6mths<=4)] <- '1_4'
cb_data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.[which(temp_value_exc_home_auto_6mths>=5 & temp_value_exc_home_auto_6mths<=10)] <- '5_10'

#Converting this variable into dummies#
cb_data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. <- as.factor(cb_data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.)
inquirylast_6_dummy <- data.frame(model.matrix( ~No.of.Inquiries.in.last.6.months..excluding.home...auto.loans., data = cb_data))

#Removing the x-intercept from the newly created dataframe,#
#using the n-1 principal of level creation and keeping the rest 3 variables#
inquirylast_6_dummy <- inquirylast_6_dummy[,-1]

#Merging the dummy variables to the main data set, after removing the original column#
cb_data <- cbind(cb_data[,-5], inquirylast_6_dummy)

#-----------------------------------------------------#

# Variable - No of Inquiries in last 12 months (excluding home & auto loans)
levels(factor(cb_data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.))

# There are 21 levels (0 to 20) in this variable, now lets plot a histogram and check.

cb_data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. <- as.numeric(cb_data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.)
ggplot(cb_data, aes(No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.))+geom_histogram()
boxplot(cb_data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.)

# Lets do the bucketing now, this will be done according to WOE
# For this variable we see that WOE is not monotonic. 
# First we will do a coarse classing and then check on how to do the binning for this variable

#No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.     N    Percent         WOE        IV
#1                                                           [0,0] 19961 0.28993275 -1.14675125 0.2335759
#2                                                           [1,1]  3572 0.05188316 -0.02428351 0.2336061
#3                                                           [2,2]  7885 0.11452932  0.14098716 0.2360355
#4                                                           [3,3]  8960 0.13014365  0.16789098 0.2399993
#5                                                           [4,4]  7095 0.10305460  0.24942884 0.2471954
#6                                                           [5,5]  4915 0.07139018  0.58025385 0.2787306
#7                                                           [6,8]  8948 0.12996935  0.48592119 0.3172302
#8                                                          [9,20]  7511 0.10909698  0.01513532 0.3172554

# for coarse classing, compute WOE 6 to 20

exc_home_auto_12mths_6_20 <- cb_data[which(cb_data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.==6 | cb_data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.==7 |
                                             cb_data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.==8 | cb_data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.==9 |  
                                             cb_data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.==10 | cb_data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.==11 |
                                             cb_data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.==12 | cb_data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.==13 |
                                             cb_data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.==14 | cb_data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.==15 |
                                             cb_data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.==16 | cb_data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.==17 |
                                             cb_data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.==18 | cb_data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.==19 | cb_data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.==20),c(5,16)]
loc_good <- length(exc_home_auto_12mths_6_20$Performance.Tag.y[which(exc_home_auto_12mths_6_20$Performance.Tag.y==0)])
loc_bad <- length(exc_home_auto_12mths_6_20$Performance.Tag.y[which(exc_home_auto_12mths_6_20$Performance.Tag.y==1)])
combined_woe <- computeWoE(loc_good,loc_bad) # -0.29

# We tried with coarse classing for the bins where the monotonic behaviour of WOE is breaking,
# but the trend is not becoming monotonic, hence we will go with the original binning.

colnames(cb_data)[colnames(cb_data)=="No.of.Inquiries.in.last.12.months..excluding.home...auto.loans."] <- "Noofenq_12Mths_exc_hmat"
cb_data$Noofenq_12Mths_exc_hmat <- as.numeric(cb_data$Noofenq_12Mths_exc_hmat)

cb_data$Noofenq_12Mths_exc_hmat[which(cb_data$Noofenq_12Mths_exc_hmat == 6 | cb_data$Noofenq_12Mths_exc_hmat == 7 |
                                        cb_data$Noofenq_12Mths_exc_hmat == 8)] <- '6_8'
cb_data$Noofenq_12Mths_exc_hmat[which(cb_data$Noofenq_12Mths_exc_hmat == 9 | cb_data$Noofenq_12Mths_exc_hmat == 10 |
                                        cb_data$Noofenq_12Mths_exc_hmat == 11 | cb_data$Noofenq_12Mths_exc_hmat == 12 |
                                        cb_data$Noofenq_12Mths_exc_hmat == 13 | cb_data$Noofenq_12Mths_exc_hmat == 14 |
                                        cb_data$Noofenq_12Mths_exc_hmat == 15 | cb_data$Noofenq_12Mths_exc_hmat == 16 |
                                        cb_data$Noofenq_12Mths_exc_hmat == 17 | cb_data$Noofenq_12Mths_exc_hmat == 18 |
                                        cb_data$Noofenq_12Mths_exc_hmat == 19 | cb_data$Noofenq_12Mths_exc_hmat == 20)] <- '9_20'

#Converting this variable into dummies#
cb_data$Noofenq_12Mths_exc_hmat <- as.factor(cb_data$Noofenq_12Mths_exc_hmat)
inquirylast_12_dummy <- data.frame(model.matrix( ~Noofenq_12Mths_exc_hmat, data = cb_data))

#Removing the x-intercept from the newly created dataframe,#
#using the n-1 principal of level creation and keeping the rest 3 variables#
inquirylast_12_dummy <- inquirylast_12_dummy[,-1]

#Merging the dummy variables to the main data set, after removing the original column#
cb_data <- cbind(cb_data[,-5], inquirylast_12_dummy)

#-----------------------------------------------------#

# Variable - Presence of open home loan

cb_data$Presence.of.open.home.loan <- as.factor(cb_data$Presence.of.open.home.loan)
levels(cb_data$Presence.of.open.home.loan)

# There are 2 levels in this variable, now lets check the summary of this variable.
summary(cb_data$Presence.of.open.home.loan)

# 0     1      
# 50784 18063  

# Home loan is not present for almost 74% of the customers.

#Presence.of.open.home.loan     N   Percent         WOE          IV
#1                      [0,0] 50784 0.7376356  0.07353653 0.004125917
#2                      [1,1] 18063 0.2623644 -0.23818417 0.017489727

# For this variable we see that WOE is monotonic (it has a decreasing pattern).

#-----------------------------------------------------#

# Variable - Presence of open auto loan

cb_data$Presence.of.open.auto.loan <- as.factor(cb_data$Presence.of.open.auto.loan)
levels(cb_data$Presence.of.open.auto.loan)

# There are 2 levels in this variable, now lets check the summary of this variable.
summary(cb_data$Presence.of.open.auto.loan)

# 0     1 
# 62917  5930

# Presence of open auto loans is not present for 91% of applicants.

#Presence.of.open.auto.loan     N    Percent         WOE           IV
#1                      [0,0] 62917 0.91386698  0.01206033 0.0001336599
#2                      [1,1]  5930 0.08613302 -0.13693704 0.0016512788

# For this variable we see that WOE is monotonic (it has a decreasing pattern).

#-----------------------------------------------------#

# Variable - No of trades opened in last 12 months
levels(factor(cb_data$No.of.trades.opened.in.last.12.months))

# There are 29 levels (0 to 28) in this variable, now lets check the summary of this variable.
head(summary(factor(cb_data$No.of.trades.opened.in.last.12.months)))

# First 6 from the summary function are
# 0     1     2     3     4     5 
# 3968 11377  9322  4677  4848  4547

# Now lets plot a box plot to check outliers
cb_data$No.of.trades.opened.in.last.12.months <- as.numeric(cb_data$No.of.trades.opened.in.last.12.months)
boxplot(cb_data$No.of.trades.opened.in.last.12.months)

# Anything above 20 should be considered as an outlier.
# Lets do a QQ analysis to check our assumption on outliers

quantile(cb_data$No.of.trades.opened.in.last.12.months,seq(0,1,.01))

# We see that 99% of the data fall inside 22 trades opened in last 12 months.
# Lets do the bucketing now, this will be done according to WOE
# For this variable we see that WOE is not monotonic. 
# First we will do a coarse classing and then check on how to do the binning for this variable

#No.of.trades.opened.in.last.12.months     N    Percent          WOE         IV
#1                                 [0,0]  3968 0.05763505 -0.910228232 0.03215848
#2                                 [1,1] 11377 0.16525048 -1.017655565 0.14251191
#3                                 [2,2]  9322 0.13540169 -0.814929003 0.20548758
#4                                 [3,4]  9525 0.13835025  0.009802636 0.20550093
#5                                 [5,5]  4547 0.06604500  0.205558279 0.20856964
#6                                 [6,7]  8291 0.12042645  0.450185383 0.23867496
#7                                 [8,9]  7166 0.10408587  0.572044558 0.28318716
#8                               [10,12]  6689 0.09715747  0.487643324 0.31219519
#9                               [13,28]  7962 0.11564774  0.005035981 0.31219813

# Coarse classing 1 to 2

optrade_12mths_1_2 <- cb_data[which(cb_data$No.of.trades.opened.in.last.12.months == 1 | cb_data$No.of.trades.opened.in.last.12.months == 2),c(4,15)]
loc_good <- length(optrade_12mths_1_2$Performance.Tag.y[which(optrade_12mths_1_2$Performance.Tag.y==0)])
loc_bad <- length(optrade_12mths_1_2$Performance.Tag.y[which(optrade_12mths_1_2$Performance.Tag.y==1)])
combined_woe <- computeWoE(loc_good,loc_bad) # 0.92

# Coarse classing 10 to 28

optrade_12mths_10_28 <- cb_data[which(cb_data$No.of.trades.opened.in.last.12.months==10 | cb_data$No.of.trades.opened.in.last.12.months==11 |
                                        cb_data$No.of.trades.opened.in.last.12.months==12 | cb_data$No.of.trades.opened.in.last.12.months==13 |  
                                        cb_data$No.of.trades.opened.in.last.12.months==14 | cb_data$No.of.trades.opened.in.last.12.months==15 |
                                        cb_data$No.of.trades.opened.in.last.12.months==16 | cb_data$No.of.trades.opened.in.last.12.months==17 |
                                        cb_data$No.of.trades.opened.in.last.12.months==18 | cb_data$No.of.trades.opened.in.last.12.months==19 |  
                                        cb_data$No.of.trades.opened.in.last.12.months==20 | cb_data$No.of.trades.opened.in.last.12.months==21 |
                                        cb_data$No.of.trades.opened.in.last.12.months==22 | cb_data$No.of.trades.opened.in.last.12.months==23 |
                                        cb_data$No.of.trades.opened.in.last.12.months==24 | cb_data$No.of.trades.opened.in.last.12.months==25 |
                                        cb_data$No.of.trades.opened.in.last.12.months==26 | cb_data$No.of.trades.opened.in.last.12.months==27 |
                                        cb_data$No.of.trades.opened.in.last.12.months==28),c(4,15)]
loc_good <- length(optrade_12mths_10_28$Performance.Tag.y[which(optrade_12mths_10_28$Performance.Tag.y==0)])
loc_bad <- length(optrade_12mths_10_28$Performance.Tag.y[which(optrade_12mths_10_28$Performance.Tag.y==1)])
combined_woe <- computeWoE(loc_good,loc_bad) # -0.25

# We tried with coarse classing for the bins where the monotonic behaviour of WOE is breaking,
# but the trend is not becoming monotonic, hence we will go with the original binning.

cb_data$No.of.trades.opened.in.last.12.months <- as.numeric(cb_data$No.of.trades.opened.in.last.12.months)
cb_data$No.of.trades.opened.in.last.12.months[which(cb_data$No.of.trades.opened.in.last.12.months == 0)] <- '0'
cb_data$No.of.trades.opened.in.last.12.months[which(cb_data$No.of.trades.opened.in.last.12.months == 1)] <- '1'
cb_data$No.of.trades.opened.in.last.12.months[which(cb_data$No.of.trades.opened.in.last.12.months == 2)] <- '2'
cb_data$No.of.trades.opened.in.last.12.months[which(cb_data$No.of.trades.opened.in.last.12.months == 3 |
                                                      cb_data$No.of.trades.opened.in.last.12.months == 4)] <- '3_4'
cb_data$No.of.trades.opened.in.last.12.months[which(cb_data$No.of.trades.opened.in.last.12.months == 5)] <- '5'
cb_data$No.of.trades.opened.in.last.12.months[which(cb_data$No.of.trades.opened.in.last.12.months == 6 |
                                                      cb_data$No.of.trades.opened.in.last.12.months == 7)] <- '6_7'
cb_data$No.of.trades.opened.in.last.12.months[which(cb_data$No.of.trades.opened.in.last.12.months == 8 |
                                                      cb_data$No.of.trades.opened.in.last.12.months == 9)] <- '8_9'
cb_data$No.of.trades.opened.in.last.12.months[which(cb_data$No.of.trades.opened.in.last.12.months == 10 |
                                                      cb_data$No.of.trades.opened.in.last.12.months == 11 |
                                                      cb_data$No.of.trades.opened.in.last.12.months == 12)] <- '10_12'
cb_data$No.of.trades.opened.in.last.12.months[which(cb_data$No.of.trades.opened.in.last.12.months == 13 |
                                                      cb_data$No.of.trades.opened.in.last.12.months == 14 |
                                                      cb_data$No.of.trades.opened.in.last.12.months == 15 |
                                                      cb_data$No.of.trades.opened.in.last.12.months == 16 |
                                                      cb_data$No.of.trades.opened.in.last.12.months == 17 |
                                                      cb_data$No.of.trades.opened.in.last.12.months == 18 |
                                                      cb_data$No.of.trades.opened.in.last.12.months == 19 |
                                                      cb_data$No.of.trades.opened.in.last.12.months == 20 |
                                                      cb_data$No.of.trades.opened.in.last.12.months == 21 |
                                                      cb_data$No.of.trades.opened.in.last.12.months == 22 |
                                                      cb_data$No.of.trades.opened.in.last.12.months == 23 |
                                                      cb_data$No.of.trades.opened.in.last.12.months == 24 |
                                                      cb_data$No.of.trades.opened.in.last.12.months == 25 |
                                                      cb_data$No.of.trades.opened.in.last.12.months == 26 |
                                                      cb_data$No.of.trades.opened.in.last.12.months == 27 |
                                                      cb_data$No.of.trades.opened.in.last.12.months == 28)] <- '13_28'

#Converting this variable into dummies#
cb_data$No.of.trades.opened.in.last.12.months <- as.factor(cb_data$No.of.trades.opened.in.last.12.months)
trades_op_12_dummy <- data.frame(model.matrix( ~No.of.trades.opened.in.last.12.months, data = cb_data))

#Removing the x-intercept from the newly created dataframe,#
#using the n-1 principal of level creation and keeping the rest 8 variables#
trades_op_12_dummy <- trades_op_12_dummy[,-1]

#Merging the dummy variables to the main data set, after removing the original column#
cb_data <- cbind(cb_data[,-4], trades_op_12_dummy)

#-----------------------------------------------------#

# Variable - Total No of Trades
levels(factor(cb_data$Total.No.of.Trades))

# The total number of trade has levels from 1 to 44, now lets plot a histogram and check.

cb_data$Total.No.of.Trades <- as.numeric(cb_data$Total.No.of.Trades)
ggplot(cb_data, aes(Total.No.of.Trades))+geom_histogram()
boxplot(cb_data$Total.No.of.Trades)

# The plot shows that 1 to 10 open trades are the most from all the trades opened, the box plot shows
# that anything that falls beyond 20 should be considered as outlier.
# Lets do a QQ analysis to check our assumption on outliers

quantile(cb_data$Total.No.of.Trades,seq(0,1,.01))

# We see that 99% of the data fall in 31 total trades, the rest can be bucketed as 31+
# Lets do the bucketing now, this will be done according to WOE

#Total.No.of.Trades    N    Percent         WOE         IV
#1               [1,1] 2925 0.04248551 -1.05756245 0.03015553
#2               [2,2] 6764 0.09824684 -1.02562360 0.09658325
#3               [3,3] 8612 0.12508897 -0.70023837 0.14164451
#4               [4,4] 7489 0.10877743 -0.44628481 0.15938999
#5               [5,5] 5712 0.08296658 -0.04700532 0.15956941
#6               [6,7] 9791 0.14221389  0.21868205 0.16709379
#7               [8,8] 4524 0.06571092  0.46139517 0.18444082
#8              [9,10] 7128 0.10353392  0.54183926 0.22359891
#9             [11,19] 8470 0.12302642  0.42537184 0.25073715
#10            [20,44] 7432 0.10794951 -0.06546748 0.25118620

# We see that the WOE is almost monotonic till it reaches the last two bins.
# We will go ahead with the bins created by information value matrix.

cb_data$Total.No.of.Trades <- as.numeric(cb_data$Total.No.of.Trades)
cb_data$Total.No.of.Trades[which(cb_data$Total.No.of.Trades == 1)] <- '1'
cb_data$Total.No.of.Trades[which(cb_data$Total.No.of.Trades == 2)] <- '2'
cb_data$Total.No.of.Trades[which(cb_data$Total.No.of.Trades == 3)] <- '3'
cb_data$Total.No.of.Trades[which(cb_data$Total.No.of.Trades == 4)] <- '4'
cb_data$Total.No.of.Trades[which(cb_data$Total.No.of.Trades == 5)] <- '5'
cb_data$Total.No.of.Trades[which(cb_data$Total.No.of.Trades == 6 | cb_data$Total.No.of.Trades == 7)] <- '6_7'
cb_data$Total.No.of.Trades[which(cb_data$Total.No.of.Trades == 8)] <- '8'
cb_data$Total.No.of.Trades[which(cb_data$Total.No.of.Trades == 9 | cb_data$Total.No.of.Trades == 10)] <- '9_10'
cb_data$Total.No.of.Trades[which(cb_data$Total.No.of.Trades == 11 | cb_data$Total.No.of.Trades == 12 |
                                   cb_data$Total.No.of.Trades == 13 | cb_data$Total.No.of.Trades == 14 |
                                   cb_data$Total.No.of.Trades == 15 | cb_data$Total.No.of.Trades == 16 |
                                   cb_data$Total.No.of.Trades == 17 | cb_data$Total.No.of.Trades == 18 |
                                   cb_data$Total.No.of.Trades == 19)] <- '11_19'
cb_data$Total.No.of.Trades[which(cb_data$Total.No.of.Trades == 20 | cb_data$Total.No.of.Trades == 21 |
                                   cb_data$Total.No.of.Trades == 22 | cb_data$Total.No.of.Trades == 23 |
                                   cb_data$Total.No.of.Trades == 24 | cb_data$Total.No.of.Trades == 25 |
                                   cb_data$Total.No.of.Trades == 26 | cb_data$Total.No.of.Trades == 27 |
                                   cb_data$Total.No.of.Trades == 28 | cb_data$Total.No.of.Trades == 29 |
                                   cb_data$Total.No.of.Trades == 30 | cb_data$Total.No.of.Trades == 31 |
                                   cb_data$Total.No.of.Trades == 32 | cb_data$Total.No.of.Trades == 33 |
                                   cb_data$Total.No.of.Trades == 34 | cb_data$Total.No.of.Trades == 35 |
                                   cb_data$Total.No.of.Trades == 36 | cb_data$Total.No.of.Trades == 37 |
                                   cb_data$Total.No.of.Trades == 38 | cb_data$Total.No.of.Trades == 39 |
                                   cb_data$Total.No.of.Trades == 40 | cb_data$Total.No.of.Trades == 41 |
                                   cb_data$Total.No.of.Trades == 42 | cb_data$Total.No.of.Trades == 43 |
                                   cb_data$Total.No.of.Trades == 44)] <- '20_44'

#Converting this variable into dummies#
cb_data$Total.No.of.Trades <- as.factor(cb_data$Total.No.of.Trades)
total_trades_dummy <- data.frame(model.matrix( ~Total.No.of.Trades, data = cb_data))

#Removing the x-intercept from the newly created dataframe,#
#using the n-1 principal of level creation and keeping the rest 3 variables#
total_trades_dummy <- total_trades_dummy[,-1]

#Merging the dummy variables to the main data set, after removing the original column#
cb_data <- cbind(cb_data[,-6], total_trades_dummy)

----------------------------------------------------#

# Variable - Outstanding Balance
cb_data$Outstanding.Balance <- as.numeric(cb_data$Outstanding.Balance)
quantile(cb_data$Outstanding.Balance,seq(0,1,.01))


#Outstanding.Balance    N    Percent         WOE         IV
#1             [0,7790] 6884 0.09998548 -0.97822666 0.06268284
#2         [7791,56368] 6885 0.10000000 -0.85046897 0.11260121
#3       [56516,392876] 6885 0.10000000 -0.07870865 0.11319887
#4      [392909,590337] 6885 0.10000000  0.27247424 0.12162187
#5      [590343,777912] 6885 0.10000000  0.46594184 0.14860148
#6      [777936,976123] 6885 0.10000000  0.41500112 0.16949517
#7     [976147,1362729] 6885 0.10000000  0.38992263 0.18772302
#8    [1362827,2962087] 6885 0.10000000 -0.39064003 0.20052990
#9    [2962089,3289690] 6885 0.10000000 -0.80341870 0.24595188
#10   [3289827,5218801] 6886 0.10001452  0.29460366 0.25590265

# We tried with coarse classing for the bins where the monotonic behaviour of WOE is breaking,
# but the trend is not becoming monotonic, hence we will go with the original binning.

# First we will scale it to thousand level and create a new variable.
cb_data$Outstanding.Balance_scale <- cb_data$Outstanding.Balance/1000

# Now we will bin the newly created variable and again create a new variable
cb_data$Outstanding.Balance_binned <- cut(cb_data$Outstanding.Balance_scale,
                                          c(-0.1,7.790,56.368,392.876,590.337,777.912,976.123,1362,2962,3289,5219),
                                          include.lowest = FALSE)

# Changing to character and renaming the bins.
cb_data$Outstanding.Balance_binned <- as.character(cb_data$Outstanding.Balance_binned)
cb_data$Outstanding.Balance_binned[which(cb_data$Outstanding.Balance_binned == "(-0.1,7.79]")] <- "0_7790"
cb_data$Outstanding.Balance_binned[which(cb_data$Outstanding.Balance_binned == "(7.79,56.4]")] <- "7791_56368"
cb_data$Outstanding.Balance_binned[which(cb_data$Outstanding.Balance_binned == "(56.4,393]")] <- "56516_392876"
cb_data$Outstanding.Balance_binned[which(cb_data$Outstanding.Balance_binned == "(393,590]")] <- "392909_590337"
cb_data$Outstanding.Balance_binned[which(cb_data$Outstanding.Balance_binned == "(590,778]")] <- "590343_777912"
cb_data$Outstanding.Balance_binned[which(cb_data$Outstanding.Balance_binned == "(778,976]")] <- "777936_976123"
cb_data$Outstanding.Balance_binned[which(cb_data$Outstanding.Balance_binned == "(976,1.36e+03]")] <- "976147_1362729"
cb_data$Outstanding.Balance_binned[which(cb_data$Outstanding.Balance_binned == "(1.36e+03,2.96e+03]")] <- "1362827_2962087"
cb_data$Outstanding.Balance_binned[which(cb_data$Outstanding.Balance_binned == "(2.96e+03,3.29e+03]")] <- "2962089_3289690"
cb_data$Outstanding.Balance_binned[which(cb_data$Outstanding.Balance_binned == "(3.29e+03,5.22e+03]")] <- "3289827_5218801"

#Converting this variable into dummies#
cb_data$Outstanding.Balance_binned <- as.factor(cb_data$Outstanding.Balance_binned)
outstanding_bin_dummy <- data.frame(model.matrix( ~Outstanding.Balance_binned, data = cb_data))

#Removing the x-intercept from the newly created dataframe,#
#using the n-1 principal of level creation and keeping the rest 9 variables#
outstanding_bin_dummy <- outstanding_bin_dummy[,-1]

#Merging the dummy variables to the main data set, after removing the original column#
cb_data <- cbind(cb_data[,-118], outstanding_bin_dummy)

#-----------------------------------------------------#
#             FEATURE ENGINEERING ENDS
#-----------------------------------------------------#

#-----------------------------------------------------#
#     MODEL BUILDING CREDIT DATA "ONLY" (MODEL-2)
#-----------------------------------------------------#

# Creating a dataframe taking only credit bureau variables.
credit_only <- cb_data[,c(13,2:4,6,60:116,118:126)]
credit_only$Performance.Tag.y <- as.factor(credit_only$Performance.Tag.y)

#-----------------------------------------------------------------#
# Eye balling the correlation among all the variables#
#-----------------------------------------------------------------#

#Checking if the correlation matrix to check on insights.
correlate <- cor(credit_only)
View(correlate)

#-----------------------------------------------------------------#
# Dividing credit only into two datasets, train and test datasets
#-----------------------------------------------------------------#

#Setting the seed to 50# 
set.seed(50)

#Now we randomly generate row indices for train dataset
trainindices_credit <- sample(1:nrow(credit_only), 0.7*nrow(credit_only))

#Generating the train data set
credit_only_train <- credit_only[trainindices_credit,]

#Storing the rest of the observations into an object "demographic_test".
credit_only_test <- credit_only[-trainindices_credit,]

#----------------------------------------------------------------#
#                    Model creation starts
#----------------------------------------------------------------#

# Creating the first classification model from the credit training data set#
credit_1 <- glm(Performance.Tag.y ~ ., data = credit_only_train, family = "binomial")

# Checking the summary of model 
summary(credit_1)

# Creating the second model using Stepwise selection
credit_2 <- stepAIC(credit_1, direction="both")
summary(credit_2)
vif(credit_2)

# removing No.of.trades.opened.in.last.12.months3_4 as it has high p-value
credit_3 <- glm(formula = Performance.Tag.y ~ No.of.times.90.DPD.or.worse.in.last.6.months + 
                  No.of.times.30.DPD.or.worse.in.last.6.months1 + No.of.times.30.DPD.or.worse.in.last.6.months2_7 + 
                  cb_data.No.of.times.90.DPD.or.worse.in.last.12.months1 + 
                  cb_data.No.of.times.90.DPD.or.worse.in.last.12.months2_5 + 
                  No.of.times.30.DPD.or.worse.in.last.12.months3_9 + Avg_Utility_12Mths22_37 + 
                  Avg_Utility_12Mths38_51 + Avg_Utility_12Mths52_71 + Avg_Utility_12Mths72_113 + 
                  No.of.trades.opened.in.last.6.months1 + No.of.trades.opened.in.last.6.months2 + 
                  No.of.trades.opened.in.last.6.months4 + No.of.trades.opened.in.last.6.months5_12 + 
                  No.of.PL.trades.opened.in.last.12.months2 + No.of.PL.trades.opened.in.last.12.months3 + 
                  No.of.PL.trades.opened.in.last.12.months4 + No.of.PL.trades.opened.in.last.12.months5 + 
                  No.of.PL.trades.opened.in.last.12.months6_12 + No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.5_10 + 
                  Noofenq_12Mths_exc_hmat1 + Noofenq_12Mths_exc_hmat2 + Noofenq_12Mths_exc_hmat3 + 
                  Noofenq_12Mths_exc_hmat4 + Noofenq_12Mths_exc_hmat5 + Noofenq_12Mths_exc_hmat6_8 + 
                  Noofenq_12Mths_exc_hmat9_20 + No.of.trades.opened.in.last.12.months10_12 + 
                  No.of.trades.opened.in.last.12.months13_28 + No.of.trades.opened.in.last.12.months8_9 + 
                  Outstanding.Balance_binned2962089_3289690 + Outstanding.Balance_binned777936_976123,
                  family = "binomial", data = credit_only_train)

summary(credit_3)
vif(credit_3)

# removing No.of.trades.opened.in.last.6.months1 as it has high p-value
credit_4 <- glm(formula = Performance.Tag.y ~ No.of.times.90.DPD.or.worse.in.last.6.months + 
                  No.of.times.30.DPD.or.worse.in.last.6.months1 + No.of.times.30.DPD.or.worse.in.last.6.months2_7 + 
                  cb_data.No.of.times.90.DPD.or.worse.in.last.12.months1 + 
                  cb_data.No.of.times.90.DPD.or.worse.in.last.12.months2_5 + 
                  No.of.times.30.DPD.or.worse.in.last.12.months3_9 + Avg_Utility_12Mths22_37 + 
                  Avg_Utility_12Mths38_51 + Avg_Utility_12Mths52_71 + Avg_Utility_12Mths72_113 + 
                  No.of.trades.opened.in.last.6.months2 + 
                  No.of.trades.opened.in.last.6.months4 + No.of.trades.opened.in.last.6.months5_12 + 
                  No.of.PL.trades.opened.in.last.12.months2 + No.of.PL.trades.opened.in.last.12.months3 + 
                  No.of.PL.trades.opened.in.last.12.months4 + No.of.PL.trades.opened.in.last.12.months5 + 
                  No.of.PL.trades.opened.in.last.12.months6_12 + No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.5_10 + 
                  Noofenq_12Mths_exc_hmat1 + Noofenq_12Mths_exc_hmat2 + Noofenq_12Mths_exc_hmat3 + 
                  Noofenq_12Mths_exc_hmat4 + Noofenq_12Mths_exc_hmat5 + Noofenq_12Mths_exc_hmat6_8 + 
                  Noofenq_12Mths_exc_hmat9_20 + No.of.trades.opened.in.last.12.months10_12 + 
                  No.of.trades.opened.in.last.12.months13_28 + No.of.trades.opened.in.last.12.months8_9 + 
                  Outstanding.Balance_binned2962089_3289690 + Outstanding.Balance_binned777936_976123, 
                  family = "binomial", data = credit_only_train)

summary(credit_4)
vif(credit_4)

# removing No.of.trades.opened.in.last.12.months8_9 as it has high p-value
credit_5 <- glm(formula = Performance.Tag.y ~ No.of.times.90.DPD.or.worse.in.last.6.months + 
                  No.of.times.30.DPD.or.worse.in.last.6.months1 + No.of.times.30.DPD.or.worse.in.last.6.months2_7 + 
                  cb_data.No.of.times.90.DPD.or.worse.in.last.12.months1 + 
                  cb_data.No.of.times.90.DPD.or.worse.in.last.12.months2_5 + 
                  No.of.times.30.DPD.or.worse.in.last.12.months3_9 + Avg_Utility_12Mths22_37 + 
                  Avg_Utility_12Mths38_51 + Avg_Utility_12Mths52_71 + Avg_Utility_12Mths72_113 + 
                  No.of.trades.opened.in.last.6.months2 + No.of.trades.opened.in.last.6.months4 + 
                  No.of.trades.opened.in.last.6.months5_12 + No.of.PL.trades.opened.in.last.12.months2 + 
                  No.of.PL.trades.opened.in.last.12.months3 + No.of.PL.trades.opened.in.last.12.months4 + 
                  No.of.PL.trades.opened.in.last.12.months5 + No.of.PL.trades.opened.in.last.12.months6_12 + 
                  No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.5_10 + 
                  Noofenq_12Mths_exc_hmat1 + Noofenq_12Mths_exc_hmat2 + Noofenq_12Mths_exc_hmat3 + 
                  Noofenq_12Mths_exc_hmat4 + Noofenq_12Mths_exc_hmat5 + Noofenq_12Mths_exc_hmat6_8 + 
                  Noofenq_12Mths_exc_hmat9_20 + No.of.trades.opened.in.last.12.months10_12 + 
                  No.of.trades.opened.in.last.12.months13_28 + 
                  Outstanding.Balance_binned2962089_3289690 + Outstanding.Balance_binned777936_976123, 
                  family = "binomial", data = credit_only_train)

summary(credit_5)
vif(credit_5)

# removing No.of.trades.opened.in.last.6.months2 as it has high p-value
credit_6 <- glm(formula = Performance.Tag.y ~ No.of.times.90.DPD.or.worse.in.last.6.months + 
                  No.of.times.30.DPD.or.worse.in.last.6.months1 + No.of.times.30.DPD.or.worse.in.last.6.months2_7 + 
                  cb_data.No.of.times.90.DPD.or.worse.in.last.12.months1 + 
                  cb_data.No.of.times.90.DPD.or.worse.in.last.12.months2_5 + 
                  No.of.times.30.DPD.or.worse.in.last.12.months3_9 + Avg_Utility_12Mths22_37 + 
                  Avg_Utility_12Mths38_51 + Avg_Utility_12Mths52_71 + Avg_Utility_12Mths72_113 + 
                  No.of.trades.opened.in.last.6.months4 + 
                  No.of.trades.opened.in.last.6.months5_12 + No.of.PL.trades.opened.in.last.12.months2 + 
                  No.of.PL.trades.opened.in.last.12.months3 + No.of.PL.trades.opened.in.last.12.months4 + 
                  No.of.PL.trades.opened.in.last.12.months5 + No.of.PL.trades.opened.in.last.12.months6_12 + 
                  No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.5_10 + 
                  Noofenq_12Mths_exc_hmat1 + Noofenq_12Mths_exc_hmat2 + Noofenq_12Mths_exc_hmat3 + 
                  Noofenq_12Mths_exc_hmat4 + Noofenq_12Mths_exc_hmat5 + Noofenq_12Mths_exc_hmat6_8 + 
                  Noofenq_12Mths_exc_hmat9_20 + No.of.trades.opened.in.last.12.months10_12 + 
                  No.of.trades.opened.in.last.12.months13_28 + Outstanding.Balance_binned2962089_3289690 + 
                  Outstanding.Balance_binned777936_976123, family = "binomial", 
                  data = credit_only_train)

summary(credit_6)
vif(credit_6)

# removing No.of.times.90.DPD.or.worse.in.last.6.months as it has high p-value
credit_7 <- glm(formula = Performance.Tag.y ~ 
                  No.of.times.30.DPD.or.worse.in.last.6.months1 + No.of.times.30.DPD.or.worse.in.last.6.months2_7 + 
                  cb_data.No.of.times.90.DPD.or.worse.in.last.12.months1 + 
                  cb_data.No.of.times.90.DPD.or.worse.in.last.12.months2_5 + 
                  No.of.times.30.DPD.or.worse.in.last.12.months3_9 + Avg_Utility_12Mths22_37 + 
                  Avg_Utility_12Mths38_51 + Avg_Utility_12Mths52_71 + Avg_Utility_12Mths72_113 + 
                  No.of.trades.opened.in.last.6.months4 + No.of.trades.opened.in.last.6.months5_12 + 
                  No.of.PL.trades.opened.in.last.12.months2 + No.of.PL.trades.opened.in.last.12.months3 + 
                  No.of.PL.trades.opened.in.last.12.months4 + No.of.PL.trades.opened.in.last.12.months5 + 
                  No.of.PL.trades.opened.in.last.12.months6_12 + No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.5_10 + 
                  Noofenq_12Mths_exc_hmat1 + Noofenq_12Mths_exc_hmat2 + Noofenq_12Mths_exc_hmat3 + 
                  Noofenq_12Mths_exc_hmat4 + Noofenq_12Mths_exc_hmat5 + Noofenq_12Mths_exc_hmat6_8 + 
                  Noofenq_12Mths_exc_hmat9_20 + No.of.trades.opened.in.last.12.months10_12 + 
                  No.of.trades.opened.in.last.12.months13_28 + Outstanding.Balance_binned2962089_3289690 + 
                  Outstanding.Balance_binned777936_976123, family = "binomial", 
                  data = credit_only_train)

summary(credit_7)
vif(credit_7)

# removing cb_data.No.of.times.90.DPD.or.worse.in.last.12.months1 as it has high p-value
credit_8 <- glm(formula = Performance.Tag.y ~ No.of.times.30.DPD.or.worse.in.last.6.months1 + 
                  No.of.times.30.DPD.or.worse.in.last.6.months2_7 + 
                  cb_data.No.of.times.90.DPD.or.worse.in.last.12.months2_5 + 
                  No.of.times.30.DPD.or.worse.in.last.12.months3_9 + Avg_Utility_12Mths22_37 + 
                  Avg_Utility_12Mths38_51 + Avg_Utility_12Mths52_71 + Avg_Utility_12Mths72_113 + 
                  No.of.trades.opened.in.last.6.months4 + No.of.trades.opened.in.last.6.months5_12 + 
                  No.of.PL.trades.opened.in.last.12.months2 + No.of.PL.trades.opened.in.last.12.months3 + 
                  No.of.PL.trades.opened.in.last.12.months4 + No.of.PL.trades.opened.in.last.12.months5 + 
                  No.of.PL.trades.opened.in.last.12.months6_12 + No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.5_10 + 
                  Noofenq_12Mths_exc_hmat1 + Noofenq_12Mths_exc_hmat2 + Noofenq_12Mths_exc_hmat3 + 
                  Noofenq_12Mths_exc_hmat4 + Noofenq_12Mths_exc_hmat5 + Noofenq_12Mths_exc_hmat6_8 + 
                  Noofenq_12Mths_exc_hmat9_20 + No.of.trades.opened.in.last.12.months10_12 + 
                  No.of.trades.opened.in.last.12.months13_28 + Outstanding.Balance_binned2962089_3289690 + 
                  Outstanding.Balance_binned777936_976123, family = "binomial", 
                  data = credit_only_train)

summary(credit_8)
vif(credit_8)

# removing cb_data.No.of.times.90.DPD.or.worse.in.last.12.months2_5 as it has high p-value
credit_9 <- glm(formula = Performance.Tag.y ~ No.of.times.30.DPD.or.worse.in.last.6.months1 + 
                  No.of.times.30.DPD.or.worse.in.last.6.months2_7 + 
                  No.of.times.30.DPD.or.worse.in.last.12.months3_9 + Avg_Utility_12Mths22_37 + 
                  Avg_Utility_12Mths38_51 + Avg_Utility_12Mths52_71 + Avg_Utility_12Mths72_113 + 
                  No.of.trades.opened.in.last.6.months4 + No.of.trades.opened.in.last.6.months5_12 + 
                  No.of.PL.trades.opened.in.last.12.months2 + No.of.PL.trades.opened.in.last.12.months3 + 
                  No.of.PL.trades.opened.in.last.12.months4 + No.of.PL.trades.opened.in.last.12.months5 + 
                  No.of.PL.trades.opened.in.last.12.months6_12 + No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.5_10 + 
                  Noofenq_12Mths_exc_hmat1 + Noofenq_12Mths_exc_hmat2 + Noofenq_12Mths_exc_hmat3 + 
                  Noofenq_12Mths_exc_hmat4 + Noofenq_12Mths_exc_hmat5 + Noofenq_12Mths_exc_hmat6_8 + 
                  Noofenq_12Mths_exc_hmat9_20 + No.of.trades.opened.in.last.12.months10_12 + 
                  No.of.trades.opened.in.last.12.months13_28 + Outstanding.Balance_binned2962089_3289690 + 
                  Outstanding.Balance_binned777936_976123, family = "binomial", 
                  data = credit_only_train)

summary(credit_9)
vif(credit_9)

# removing No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.5_10 as it has high p-value
credit_10 <- glm(formula = Performance.Tag.y ~ No.of.times.30.DPD.or.worse.in.last.6.months1 + 
                   No.of.times.30.DPD.or.worse.in.last.6.months2_7 + No.of.times.30.DPD.or.worse.in.last.12.months3_9 + 
                   Avg_Utility_12Mths22_37 + Avg_Utility_12Mths38_51 + Avg_Utility_12Mths52_71 + 
                   Avg_Utility_12Mths72_113 + No.of.trades.opened.in.last.6.months4 + 
                   No.of.trades.opened.in.last.6.months5_12 + No.of.PL.trades.opened.in.last.12.months2 + 
                   No.of.PL.trades.opened.in.last.12.months3 + No.of.PL.trades.opened.in.last.12.months4 + 
                   No.of.PL.trades.opened.in.last.12.months5 + No.of.PL.trades.opened.in.last.12.months6_12 + 
                   Noofenq_12Mths_exc_hmat1 + Noofenq_12Mths_exc_hmat2 + Noofenq_12Mths_exc_hmat3 + 
                   Noofenq_12Mths_exc_hmat4 + Noofenq_12Mths_exc_hmat5 + Noofenq_12Mths_exc_hmat6_8 + 
                   Noofenq_12Mths_exc_hmat9_20 + No.of.trades.opened.in.last.12.months10_12 + 
                   No.of.trades.opened.in.last.12.months13_28 + Outstanding.Balance_binned2962089_3289690 + 
                   Outstanding.Balance_binned777936_976123, family = "binomial", 
                   data = credit_only_train)

summary(credit_10)
vif(credit_10)

# removing Outstanding.Balance_binned777936_976123 as it has high p-value
credit_11 <- glm(formula = Performance.Tag.y ~ No.of.times.30.DPD.or.worse.in.last.6.months1 + 
                   No.of.times.30.DPD.or.worse.in.last.6.months2_7 + No.of.times.30.DPD.or.worse.in.last.12.months3_9 + 
                   Avg_Utility_12Mths22_37 + Avg_Utility_12Mths38_51 + Avg_Utility_12Mths52_71 + 
                   Avg_Utility_12Mths72_113 + No.of.trades.opened.in.last.6.months4 + 
                   No.of.trades.opened.in.last.6.months5_12 + No.of.PL.trades.opened.in.last.12.months2 + 
                   No.of.PL.trades.opened.in.last.12.months3 + No.of.PL.trades.opened.in.last.12.months4 + 
                   No.of.PL.trades.opened.in.last.12.months5 + No.of.PL.trades.opened.in.last.12.months6_12 + 
                   Noofenq_12Mths_exc_hmat1 + Noofenq_12Mths_exc_hmat2 + Noofenq_12Mths_exc_hmat3 + 
                   Noofenq_12Mths_exc_hmat4 + Noofenq_12Mths_exc_hmat5 + Noofenq_12Mths_exc_hmat6_8 + 
                   Noofenq_12Mths_exc_hmat9_20 + No.of.trades.opened.in.last.12.months10_12 + 
                   No.of.trades.opened.in.last.12.months13_28 + Outstanding.Balance_binned2962089_3289690, 
                   family = "binomial", data = credit_only_train)

summary(credit_11)
vif(credit_11)

# removing No.of.trades.opened.in.last.6.months4 as it has high p-value
credit_12 <- glm(formula = Performance.Tag.y ~ No.of.times.30.DPD.or.worse.in.last.6.months1 + 
                   No.of.times.30.DPD.or.worse.in.last.6.months2_7 + No.of.times.30.DPD.or.worse.in.last.12.months3_9 + 
                   Avg_Utility_12Mths22_37 + Avg_Utility_12Mths38_51 + Avg_Utility_12Mths52_71 + 
                   Avg_Utility_12Mths72_113 + 
                   No.of.trades.opened.in.last.6.months5_12 + No.of.PL.trades.opened.in.last.12.months2 + 
                   No.of.PL.trades.opened.in.last.12.months3 + No.of.PL.trades.opened.in.last.12.months4 + 
                   No.of.PL.trades.opened.in.last.12.months5 + No.of.PL.trades.opened.in.last.12.months6_12 + 
                   Noofenq_12Mths_exc_hmat1 + Noofenq_12Mths_exc_hmat2 + Noofenq_12Mths_exc_hmat3 + 
                   Noofenq_12Mths_exc_hmat4 + Noofenq_12Mths_exc_hmat5 + Noofenq_12Mths_exc_hmat6_8 + 
                   Noofenq_12Mths_exc_hmat9_20 + No.of.trades.opened.in.last.12.months10_12 + 
                   No.of.trades.opened.in.last.12.months13_28 + Outstanding.Balance_binned2962089_3289690, 
                   family = "binomial", data = credit_only_train)

summary(credit_12)
vif(credit_12)

# removing No.of.trades.opened.in.last.6.months5_12 as it has high p-value
credit_13 <- glm(formula = Performance.Tag.y ~ No.of.times.30.DPD.or.worse.in.last.6.months1 + 
                   No.of.times.30.DPD.or.worse.in.last.6.months2_7 + No.of.times.30.DPD.or.worse.in.last.12.months3_9 + 
                   Avg_Utility_12Mths22_37 + Avg_Utility_12Mths38_51 + Avg_Utility_12Mths52_71 + 
                   Avg_Utility_12Mths72_113 + 
                   No.of.PL.trades.opened.in.last.12.months2 + No.of.PL.trades.opened.in.last.12.months3 + 
                   No.of.PL.trades.opened.in.last.12.months4 + No.of.PL.trades.opened.in.last.12.months5 + 
                   No.of.PL.trades.opened.in.last.12.months6_12 + Noofenq_12Mths_exc_hmat1 + 
                   Noofenq_12Mths_exc_hmat2 + Noofenq_12Mths_exc_hmat3 + Noofenq_12Mths_exc_hmat4 + 
                   Noofenq_12Mths_exc_hmat5 + Noofenq_12Mths_exc_hmat6_8 + Noofenq_12Mths_exc_hmat9_20 + 
                   No.of.trades.opened.in.last.12.months10_12 + No.of.trades.opened.in.last.12.months13_28 + 
                   Outstanding.Balance_binned2962089_3289690, family = "binomial", 
                   data = credit_only_train)

summary(credit_13)
vif(credit_13)

# removing No.of.trades.opened.in.last.12.months13_28 as it has high p-value
credit_14 <- glm(formula = Performance.Tag.y ~ No.of.times.30.DPD.or.worse.in.last.6.months1 + 
                   No.of.times.30.DPD.or.worse.in.last.6.months2_7 + No.of.times.30.DPD.or.worse.in.last.12.months3_9 + 
                   Avg_Utility_12Mths22_37 + Avg_Utility_12Mths38_51 + Avg_Utility_12Mths52_71 + 
                   Avg_Utility_12Mths72_113 + No.of.PL.trades.opened.in.last.12.months2 + 
                   No.of.PL.trades.opened.in.last.12.months3 + No.of.PL.trades.opened.in.last.12.months4 + 
                   No.of.PL.trades.opened.in.last.12.months5 + No.of.PL.trades.opened.in.last.12.months6_12 + 
                   Noofenq_12Mths_exc_hmat1 + Noofenq_12Mths_exc_hmat2 + Noofenq_12Mths_exc_hmat3 + 
                   Noofenq_12Mths_exc_hmat4 + Noofenq_12Mths_exc_hmat5 + Noofenq_12Mths_exc_hmat6_8 + 
                   Noofenq_12Mths_exc_hmat9_20 + No.of.trades.opened.in.last.12.months10_12 + 
                   Outstanding.Balance_binned2962089_3289690, 
                   family = "binomial", data = credit_only_train)

summary(credit_14)
vif(credit_14)

# removing No.of.PL.trades.opened.in.last.12.months2 as it has high p-value
credit_15 <- glm(formula = Performance.Tag.y ~ No.of.times.30.DPD.or.worse.in.last.6.months1 + 
                   No.of.times.30.DPD.or.worse.in.last.6.months2_7 + No.of.times.30.DPD.or.worse.in.last.12.months3_9 + 
                   Avg_Utility_12Mths22_37 + Avg_Utility_12Mths38_51 + Avg_Utility_12Mths52_71 + 
                   Avg_Utility_12Mths72_113 + 
                   No.of.PL.trades.opened.in.last.12.months3 + No.of.PL.trades.opened.in.last.12.months4 + 
                   No.of.PL.trades.opened.in.last.12.months5 + No.of.PL.trades.opened.in.last.12.months6_12 + 
                   Noofenq_12Mths_exc_hmat1 + Noofenq_12Mths_exc_hmat2 + Noofenq_12Mths_exc_hmat3 + 
                   Noofenq_12Mths_exc_hmat4 + Noofenq_12Mths_exc_hmat5 + Noofenq_12Mths_exc_hmat6_8 + 
                   Noofenq_12Mths_exc_hmat9_20 + No.of.trades.opened.in.last.12.months10_12 + 
                   Outstanding.Balance_binned2962089_3289690, family = "binomial", 
                   data = credit_only_train)

summary(credit_15)
vif(credit_15)

# removing No.of.PL.trades.opened.in.last.12.months3 as it has high p-value
credit_16 <- glm(formula = Performance.Tag.y ~ No.of.times.30.DPD.or.worse.in.last.6.months1 + 
                   No.of.times.30.DPD.or.worse.in.last.6.months2_7 + No.of.times.30.DPD.or.worse.in.last.12.months3_9 + 
                   Avg_Utility_12Mths22_37 + Avg_Utility_12Mths38_51 + Avg_Utility_12Mths52_71 + 
                   Avg_Utility_12Mths72_113 + 
                   No.of.PL.trades.opened.in.last.12.months4 + No.of.PL.trades.opened.in.last.12.months5 + 
                   No.of.PL.trades.opened.in.last.12.months6_12 + Noofenq_12Mths_exc_hmat1 + 
                   Noofenq_12Mths_exc_hmat2 + Noofenq_12Mths_exc_hmat3 + Noofenq_12Mths_exc_hmat4 + 
                   Noofenq_12Mths_exc_hmat5 + Noofenq_12Mths_exc_hmat6_8 + Noofenq_12Mths_exc_hmat9_20 + 
                   No.of.trades.opened.in.last.12.months10_12 + Outstanding.Balance_binned2962089_3289690, 
                   family = "binomial", data = credit_only_train)

summary(credit_16)
vif(credit_16)

# removing No.of.PL.trades.opened.in.last.12.months5 as it has high p-value
credit_17 <- glm(formula = Performance.Tag.y ~ No.of.times.30.DPD.or.worse.in.last.6.months1 + 
                   No.of.times.30.DPD.or.worse.in.last.6.months2_7 + No.of.times.30.DPD.or.worse.in.last.12.months3_9 + 
                   Avg_Utility_12Mths22_37 + Avg_Utility_12Mths38_51 + Avg_Utility_12Mths52_71 + 
                   Avg_Utility_12Mths72_113 + No.of.PL.trades.opened.in.last.12.months4 + 
                   No.of.PL.trades.opened.in.last.12.months6_12 + 
                   Noofenq_12Mths_exc_hmat1 + Noofenq_12Mths_exc_hmat2 + Noofenq_12Mths_exc_hmat3 + 
                   Noofenq_12Mths_exc_hmat4 + Noofenq_12Mths_exc_hmat5 + Noofenq_12Mths_exc_hmat6_8 + 
                   Noofenq_12Mths_exc_hmat9_20 + No.of.trades.opened.in.last.12.months10_12 + 
                   Outstanding.Balance_binned2962089_3289690, family = "binomial", 
                   data = credit_only_train)

summary(credit_17)
vif(credit_17)

# removing No.of.PL.trades.opened.in.last.12.months6_12 as it has high p-value
credit_18 <- glm(formula = Performance.Tag.y ~ No.of.times.30.DPD.or.worse.in.last.6.months1 + 
                   No.of.times.30.DPD.or.worse.in.last.6.months2_7 + No.of.times.30.DPD.or.worse.in.last.12.months3_9 + 
                   Avg_Utility_12Mths22_37 + Avg_Utility_12Mths38_51 + Avg_Utility_12Mths52_71 + 
                   Avg_Utility_12Mths72_113 + No.of.PL.trades.opened.in.last.12.months4 + 
                   Noofenq_12Mths_exc_hmat1 + 
                   Noofenq_12Mths_exc_hmat2 + Noofenq_12Mths_exc_hmat3 + Noofenq_12Mths_exc_hmat4 + 
                   Noofenq_12Mths_exc_hmat5 + Noofenq_12Mths_exc_hmat6_8 + Noofenq_12Mths_exc_hmat9_20 + 
                   No.of.trades.opened.in.last.12.months10_12 + Outstanding.Balance_binned2962089_3289690, 
                   family = "binomial", data = credit_only_train)

summary(credit_18)
vif(credit_18)

# removing No.of.trades.opened.in.last.12.months10_12 as it has high p-value
credit_19 <- glm(formula = Performance.Tag.y ~ No.of.times.30.DPD.or.worse.in.last.6.months1 + 
                   No.of.times.30.DPD.or.worse.in.last.6.months2_7 + No.of.times.30.DPD.or.worse.in.last.12.months3_9 + 
                   Avg_Utility_12Mths22_37 + Avg_Utility_12Mths38_51 + Avg_Utility_12Mths52_71 + 
                   Avg_Utility_12Mths72_113 + No.of.PL.trades.opened.in.last.12.months4 + 
                   Noofenq_12Mths_exc_hmat1 + Noofenq_12Mths_exc_hmat2 + Noofenq_12Mths_exc_hmat3 + 
                   Noofenq_12Mths_exc_hmat4 + Noofenq_12Mths_exc_hmat5 + Noofenq_12Mths_exc_hmat6_8 + 
                   Noofenq_12Mths_exc_hmat9_20 + 
                   Outstanding.Balance_binned2962089_3289690, family = "binomial", 
                   data = credit_only_train)

summary(credit_19)
vif(credit_19)

# removing No.of.PL.trades.opened.in.last.12.months4 as it has high p-value
credit_20 <- glm(formula = Performance.Tag.y ~ No.of.times.30.DPD.or.worse.in.last.6.months1 + 
                   No.of.times.30.DPD.or.worse.in.last.6.months2_7 + No.of.times.30.DPD.or.worse.in.last.12.months3_9 + 
                   Avg_Utility_12Mths22_37 + Avg_Utility_12Mths38_51 + Avg_Utility_12Mths52_71 + 
                   Avg_Utility_12Mths72_113 + 
                   Noofenq_12Mths_exc_hmat1 + Noofenq_12Mths_exc_hmat2 + Noofenq_12Mths_exc_hmat3 + 
                   Noofenq_12Mths_exc_hmat4 + Noofenq_12Mths_exc_hmat5 + Noofenq_12Mths_exc_hmat6_8 + 
                   Noofenq_12Mths_exc_hmat9_20 + Outstanding.Balance_binned2962089_3289690, 
                   family = "binomial", data = credit_only_train)   
  
summary(credit_20)
vif(credit_20)  
  
# We should consider credit_20 while taking only credit bureau
# variables into consideration. The most important variables are
# No.of.times.30.DPD.or.worse.in.last.6.months1,No.of.times.30.DPD.or.worse.in.last.6.months2_7,
# No.of.times.30.DPD.or.worse.in.last.12.months3_9,Avg_Utility_12Mths22_37,
# Avg_Utility_12Mths38_51,Avg_Utility_12Mths52_71,Avg_Utility_12Mths72_113,
# Noofenq_12Mths_exc_hmat1,Noofenq_12Mths_exc_hmat2,Noofenq_12Mths_exc_hmat3,
# Noofenq_12Mths_exc_hmat4,Noofenq_12Mths_exc_hmat5,Noofenq_12Mths_exc_hmat6_8,
# Noofenq_12Mths_exc_hmat9_20,Outstanding.Balance_binned2962089_3289690

#-----------------------------------------------------#
#        MODEL EVALUATION CREDIT BUREAU DATA 
#-----------------------------------------------------#

# predicting probabilities of Performance TAG for test data
cred_test_pred <- predict(credit_20, credit_only_test[,-1],type = "response")
summary(cred_test_pred)
credit_only_test$prob <- cred_test_pred

# probability greater than .5 is 1 (customer will default)
cred_test_pred_ptag_50 <- factor(ifelse(cred_test_pred >= 0.50, 1,0))

# confusion matrix
cred_test_conf <- confusionMatrix(cred_test_pred_ptag_50, credit_only_test$Performance.Tag.y, positive = "1")
cred_test_conf

#Sensitivity : 0.0000         
#Specificity : 1.0000      
#Accuracy : 0.9572

# We see that Sensitivity is zero and Specificity is 1 when cutoff is 0.50,
# hence we need to create a function to understand the optimal probability cutoff

#-----------------------------------------------------#

# compute optimal probalility cutoff for better model reliability
perform_fn_cred <- function(cutoff) 
{
  cred_test_pred_ptag_50 <- factor(ifelse(cred_test_pred >= cutoff, 1,0))
  cred_test_conf <- confusionMatrix(cred_test_pred_ptag_50, credit_only_test$Performance.Tag.y, positive = "1")
  acc <- cred_test_conf$overall[1]
  sens <- cred_test_conf$byClass[1]
  spec <- cred_test_conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values from 0.01 to 0.99 for plotting and initiallizing a matrix of 1000 X 4.
cred_s = seq(.01,.99,length=100)
OUT = matrix(0,100,3)

for(i in 1:100)
{
  OUT[i,] = perform_fn_cred(cred_s[i])
}

#-----------------------------------------------------#

# plotting sensitivity, specificity and accuracy with different values of probability
plot(cred_s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(cred_s,OUT[,2],col="darkgreen",lwd=2)
lines(cred_s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

# finding cutoff probability for threshold value above which represents that customer will default
cutoff <- cred_s[which(abs(OUT[,1]-OUT[,2])<0.01)]

# probability greater than .04 is 1 (customer will default)
cred_test_pred_ptag <- factor(ifelse(cred_test_pred >= 0.0429, 1,0))

# confusion matrix with cutoff value equal 0.0429
cred_conf_final <- confusionMatrix(cred_test_pred_ptag, credit_only_test$Performance.Tag.y, positive = "1")
cred_conf_final

#Sensitivity : 0.65574         
#Specificity : 0.58890      
#Accuracy : 0.5926

# These results clearly look better now, Sensitivity is at 65%.
# So using the glm model and taking up only credit variables we see
# the results look more promising.

#-----------------------------------------------------#
#-----------------------------------------------------#
#-----------------------------------------------------#

#-----------------------------------------------------#
#        WOE VALUE CREATION FOR ALL THE VARIABLES
#-----------------------------------------------------#


#-----------------------------------------------------#

# Creating a new data set from the original variables
woe_data <- original_data

#-----------------------------------------------------#

# Checking the WOE and information value
IV_woe <- create_infotables(data = woe_data, valid = NULL, trt = NULL,
                            y = "Performance.Tag.y", bins=10, parallel=FALSE)

#-----------------------------------------------------#

# Variable Age

# MAking 0 and -3 as NA's
woe_data[, 19][woe_data[, 19] == 0] <- NA
woe_data[, 19][woe_data[, 19] == -3] <- NA

# Lets remove the NA values and bin the other values.
woe_data <- na.omit(woe_data, cols="Age")

# Lets remove the NA values and bin the other values.
woe_data <- na.omit(woe_data, cols="Age")

# Let us create the bucket as is, instead of doing a coarse classing and
# recreating the bins
woe_data$Age <- as.numeric(woe_data$Age)
woe_data$Age_binned <- cut(woe_data$Age, c(14,30,35,38,41,44,47,50,53,57,66),include.lowest = FALSE)

# Now we will rename the levels in the newly created Age_binned variable.
woe_data$Age_binned <- as.character(woe_data$Age_binned)
woe_data$Age_binned[which(woe_data$Age_binned == "(14,30]")] <- "15_30" 
woe_data$Age_binned[which(woe_data$Age_binned == "(30,35]")] <- "31_35"
woe_data$Age_binned[which(woe_data$Age_binned == "(35,38]")] <- "36_38"
woe_data$Age_binned[which(woe_data$Age_binned == "(38,41]")] <- "39_41"
woe_data$Age_binned[which(woe_data$Age_binned == "(41,44]")] <- "42_44"
woe_data$Age_binned[which(woe_data$Age_binned == "(44,47]")] <- "45_47"
woe_data$Age_binned[which(woe_data$Age_binned == "(47,50]")] <- "48_50"
woe_data$Age_binned[which(woe_data$Age_binned == "(50,53]")] <- "51_53"
woe_data$Age_binned[which(woe_data$Age_binned == "(53,57]")] <- "54_57"
woe_data$Age_binned[which(woe_data$Age_binned == "(57,66]")] <- "58_65"
woe_data$Age_binned <- as.factor(woe_data$Age_binned)

# Creating the WOE Values
woe_data$Age_binned <- as.factor(woe_data$Age_binned)
levels(woe_data$Age_binned) <-c(-.04,.04,.07,.08,-.06,-.01,-.01,-.14,.05,-.01)

#-----------------------------------------------------#

# Variable - Gender

# Using the UDF to convert blanks to NA
woe_data$Gender <- blank_as_na(woe_data$Gender)

# The weight attcahed to NA is negligible hence we will remove it from our analysis
woe_data <- na.omit(woe_data, cols="Gender")

# Creating the WOE Values
woe_data$Gender <- as.factor(woe_data$Gender)
levels(woe_data$Gender) <-c(.03,-.01)

#-----------------------------------------------------#

# Variable - Marital.Status..at.the.time.of.application.

# we will first change the column header
colnames(woe_data)[colnames(woe_data)=="Marital.Status..at.the.time.of.application."] <- "Marital_Status"

# Using the UDF to convert blanks to NA
woe_data$Marital_Status <- blank_as_na(woe_data$Marital_Status)

# The weight attcahed to NA is negligible hence we will remove it from our analysis
woe_data <- na.omit(woe_data, cols="Marital_Status")

# Creating the WOE Values
woe_data$Marital_Status <- as.factor(woe_data$Marital_Status)
levels(woe_data$Marital_Status) <-c(-.01,.02)

#-----------------------------------------------------#

# Variable - No.of.dependents

# Creating the WOE Values
woe_data$No.of.dependents <- as.factor(woe_data$No.of.dependents)
levels(woe_data$No.of.dependents) <-c(.05,-.10,.05,-.03,.01)

#-----------------------------------------------------#

# Variable - Education

# Using the UDF to convert blanks to NA
woe_data$Education <- blank_as_na(woe_data$Education)

# The weight attcahed to NA is negligible hence we will remove it from our analysis
woe_data <- na.omit(woe_data, cols="Education")

# Creating the WOE Values
woe_data$Education <- as.factor(woe_data$Education)
levels(woe_data$Education) <-c(.020,-.0008,0.530,-.008,-.015)

#-----------------------------------------------------#

# Variable - Profession

# Using the UDF to convert blanks to NA
woe_data$Profession <- blank_as_na(woe_data$Profession)

# The weight attcahed to NA is negligible hence we will remove it from our analysis
woe_data <- na.omit(woe_data, cols="Profession")

# Creating the WOE Values
woe_data$Profession <- as.factor(woe_data$Profession)
levels(woe_data$Profession) <-c(.02,.08,-.01)

#-----------------------------------------------------#

# Variable - Type.of.residence

# Using the UDF to convert blanks to NA
woe_data$Type.of.residence <- blank_as_na(woe_data$Type.of.residence)

# The weight attcahed to NA is negligible hence we will remove it from our analysis
woe_data <- na.omit(woe_data, cols="Type.of.residence")

# Creating the WOE Values
woe_data$Type.of.residence <- as.factor(woe_data$Type.of.residence)
levels(woe_data$Type.of.residence) <-c(.08,.07,-.51,-.01,-.01)

#-----------------------------------------------------#

# Variable - No.of.months.in.current.residence

# we will first change the column header
colnames(woe_data)[colnames(woe_data)=="No.of.months.in.current.residence"] <- "Mth_Curr_Residence"

# Let us create the bucket as is, instead of doing a coarse classing and
# recreating the bins
woe_data$Mth_in_res_binned <- cut(woe_data$Mth_Curr_Residence, c(5,9,28,49,72,97,127),include.lowest = FALSE)

# Now we will rename the levels in the newly created Mth_in_res_binned variable.
woe_data$Mth_in_res_binned <- as.character(woe_data$Mth_in_res_binned)
woe_data$Mth_in_res_binned[which(woe_data$Mth_in_res_binned == "(5,9]")] <- "6_9" 
woe_data$Mth_in_res_binned[which(woe_data$Mth_in_res_binned == "(9,28]")] <- "10_28"
woe_data$Mth_in_res_binned[which(woe_data$Mth_in_res_binned == "(28,49]")] <- "29_49"
woe_data$Mth_in_res_binned[which(woe_data$Mth_in_res_binned == "(49,72]")] <- "50_72"
woe_data$Mth_in_res_binned[which(woe_data$Mth_in_res_binned == "(72,97]")] <- "73_97"
woe_data$Mth_in_res_binned[which(woe_data$Mth_in_res_binned == "(97,127]")] <- "98_126"

# Creating the WOE Values
woe_data$Mth_in_res_binned <- as.factor(woe_data$Mth_in_res_binned)
levels(woe_data$Mth_in_res_binned) <-c(-.27,.50,.30,.13,.13,-.07)

#-----------------------------------------------------#

# Variable - No.of.months.in.current.company

# we will first change the column header
colnames(woe_data)[colnames(woe_data)=="No.of.months.in.current.company"] <- "Mth_Curr_Company"

# Let us create the bucket as is, instead of doing a coarse classing and
# recreating the bins
woe_data$Mth_in_comp_binned <- cut(woe_data$Mth_Curr_Company, c(2,5,12,19,26,33,40,47,53,61,134),include.lowest = FALSE)

# Now we will rename the levels in the newly created Mth_in_res_binned variable.
woe_data$Mth_in_comp_binned <- as.character(woe_data$Mth_in_comp_binned)
woe_data$Mth_in_comp_binned[which(woe_data$Mth_in_comp_binned == "(2,5]")] <- "3_5" 
woe_data$Mth_in_comp_binned[which(woe_data$Mth_in_comp_binned == "(5,12]")] <- "6_12"
woe_data$Mth_in_comp_binned[which(woe_data$Mth_in_comp_binned == "(12,19]")] <- "13_19"
woe_data$Mth_in_comp_binned[which(woe_data$Mth_in_comp_binned == "(19,26]")] <- "20_26"
woe_data$Mth_in_comp_binned[which(woe_data$Mth_in_comp_binned == "(26,33]")] <- "27_33"
woe_data$Mth_in_comp_binned[which(woe_data$Mth_in_comp_binned == "(33,40]")] <- "34_40"
woe_data$Mth_in_comp_binned[which(woe_data$Mth_in_comp_binned == "(40,47]")] <- "41_47"
woe_data$Mth_in_comp_binned[which(woe_data$Mth_in_comp_binned == "(47,53]")] <- "48_53"
woe_data$Mth_in_comp_binned[which(woe_data$Mth_in_comp_binned == "(53,61]")] <- "54_61"
woe_data$Mth_in_comp_binned[which(woe_data$Mth_in_comp_binned == "(61,134]")] <- "62_133"

# Creating the WOE Values
woe_data$Mth_in_comp_binned <- as.factor(woe_data$Mth_in_comp_binned)
levels(woe_data$Mth_in_comp_binned) <-c(.10,.17,.20,.03,-.17,.02,-.16,-.22,-.22,.06)

#-----------------------------------------------------#

# Variable - Income

# Let us create the bucket as is, instead of doing a coarse classing and
# recreating the bins
woe_data$Income_binned <- cut(woe_data$Income, c(-0.6,5,10,16,21,26,31,36,41,48,61),include.lowest = FALSE)

# Now we will rename the levels in the newly created Income_binned variable.
woe_data$Income_binned <- as.character(woe_data$Income_binned)
woe_data$Income_binned[which(woe_data$Income_binned == "(-0.6,5]")] <- "-0.5_5" 
woe_data$Income_binned[which(woe_data$Income_binned == "(5,10]")] <- "6_10" 
woe_data$Income_binned[which(woe_data$Income_binned == "(10,16]")] <- "11_16" 
woe_data$Income_binned[which(woe_data$Income_binned == "(16,21]")] <- "17-21" 
woe_data$Income_binned[which(woe_data$Income_binned == "(21,26]")] <- "22_26" 
woe_data$Income_binned[which(woe_data$Income_binned == "(26,31]")] <- "27_31" 
woe_data$Income_binned[which(woe_data$Income_binned == "(31,36]")] <- "32_36" 
woe_data$Income_binned[which(woe_data$Income_binned == "(36,41]")] <- "37_41" 
woe_data$Income_binned[which(woe_data$Income_binned == "(41,48]")] <- "42_48" 
woe_data$Income_binned[which(woe_data$Income_binned == "(48,61]")] <- "49_60"

# Creating the WOE Values
woe_data$Income_binned <- as.factor(woe_data$Income_binned)
levels(woe_data$Income_binned) <-c(.30,.26,.07,.08,.009,.07,-.13,-.26,-.17,-.36)

#-----------------------------------------------------#

# Variable - No of times 90 DPD or worse in last 6 months

# For this variable WOE is monotonic, we will bucket according to the WOE table
woe_data$No.of.times.90.DPD.or.worse.in.last.6.months <- as.numeric(woe_data$No.of.times.90.DPD.or.worse.in.last.6.months)
woe_data$No.of.times.90.DPD.or.worse.in.last.6.months[which(woe_data$No.of.times.90.DPD.or.worse.in.last.6.months == 0)] <- '0'
woe_data$No.of.times.90.DPD.or.worse.in.last.6.months[which(woe_data$No.of.times.90.DPD.or.worse.in.last.6.months >= 1 & woe_data$No.of.times.90.DPD.or.worse.in.last.6.months <= 3)] <- '1_3'

# Creating the WOE Values
woe_data$No.of.times.90.DPD.or.worse.in.last.6.months <- as.factor(woe_data$No.of.times.90.DPD.or.worse.in.last.6.months)
levels(woe_data$No.of.times.90.DPD.or.worse.in.last.6.months) <-c(-.27,.62)

#-----------------------------------------------------#

# Variable - No of times 60 DPD or worse in last 6 months

# For this variable WOE is monotonic, we will bucket according to the WOE table
woe_data$No.of.times.60.DPD.or.worse.in.last.6.months <- as.numeric(woe_data$No.of.times.60.DPD.or.worse.in.last.6.months)
woe_data$No.of.times.60.DPD.or.worse.in.last.6.months[which(woe_data$No.of.times.60.DPD.or.worse.in.last.6.months == 0)] <- '0'
woe_data$No.of.times.60.DPD.or.worse.in.last.6.months[which(woe_data$No.of.times.60.DPD.or.worse.in.last.6.months >= 1 & woe_data$No.of.times.60.DPD.or.worse.in.last.6.months <= 5)] <- '1_5'

# Creating the WOE Values
woe_data$No.of.times.60.DPD.or.worse.in.last.6.months <- as.factor(woe_data$No.of.times.60.DPD.or.worse.in.last.6.months)
levels(woe_data$No.of.times.60.DPD.or.worse.in.last.6.months) <-c(-.34,.62)

#-----------------------------------------------------#

# Variable - No of times 30 DPD or worse in last 6 months

# For this variable WOE is monotonic, we will bucket according to the WOE table
woe_data$No.of.times.30.DPD.or.worse.in.last.6.months <- as.numeric(woe_data$No.of.times.30.DPD.or.worse.in.last.6.months)
woe_data$No.of.times.30.DPD.or.worse.in.last.6.months[which(woe_data$No.of.times.30.DPD.or.worse.in.last.6.months == 0)] <- '0'
woe_data$No.of.times.30.DPD.or.worse.in.last.6.months[which(woe_data$No.of.times.30.DPD.or.worse.in.last.6.months == 1)] <- '1'
woe_data$No.of.times.30.DPD.or.worse.in.last.6.months[which(woe_data$No.of.times.30.DPD.or.worse.in.last.6.months >= 2 & woe_data$No.of.times.30.DPD.or.worse.in.last.6.months <= 7)] <- '2_7'

# Creating the WOE Values
woe_data$No.of.times.30.DPD.or.worse.in.last.6.months <- as.factor(woe_data$No.of.times.30.DPD.or.worse.in.last.6.months)
levels(woe_data$No.of.times.30.DPD.or.worse.in.last.6.months) <-c(-.39,.47,.74)

#-----------------------------------------------------#

# Variable - No of times 90 DPD or worse in last 12 months

# For this variable WOE is monotonic, we will bucket according to the WOE table
woe_data$No.of.times.90.DPD.or.worse.in.last.12.months <- as.numeric(woe_data$No.of.times.90.DPD.or.worse.in.last.12.months)
woe_data$No.of.times.90.DPD.or.worse.in.last.12.months[which(woe_data$No.of.times.90.DPD.or.worse.in.last.12.months == 0)] <- '0'
woe_data$No.of.times.90.DPD.or.worse.in.last.12.months[which(woe_data$No.of.times.90.DPD.or.worse.in.last.12.months == 1)] <- '1'
woe_data$No.of.times.90.DPD.or.worse.in.last.12.months[which(woe_data$No.of.times.90.DPD.or.worse.in.last.12.months >= 2 & woe_data$No.of.times.90.DPD.or.worse.in.last.12.months <= 5)] <- '2_5'

# Creating the WOE Values
woe_data$No.of.times.90.DPD.or.worse.in.last.12.months <- as.factor(woe_data$No.of.times.90.DPD.or.worse.in.last.12.months)
levels(woe_data$No.of.times.90.DPD.or.worse.in.last.12.months) <-c(-.36,.51,.72)

#-----------------------------------------------------#

# Variable - No of times 60 DPD or worse in last 12 months

# For this variable WOE is monotonic, we will bucket according to the WOE table
woe_data$No.of.times.60.DPD.or.worse.in.last.12.months <- as.numeric(woe_data$No.of.times.60.DPD.or.worse.in.last.12.months)
woe_data$No.of.times.60.DPD.or.worse.in.last.12.months[which(woe_data$No.of.times.60.DPD.or.worse.in.last.12.months == 0)] <- '0'
woe_data$No.of.times.60.DPD.or.worse.in.last.12.months[which(woe_data$No.of.times.60.DPD.or.worse.in.last.12.months == 1)] <- '1'
woe_data$No.of.times.60.DPD.or.worse.in.last.12.months[which(woe_data$No.of.times.60.DPD.or.worse.in.last.12.months >= 2 & woe_data$No.of.times.60.DPD.or.worse.in.last.12.months <= 7)] <- '2_7'

# Creating the WOE Values
woe_data$No.of.times.60.DPD.or.worse.in.last.12.months <- as.factor(woe_data$No.of.times.60.DPD.or.worse.in.last.12.months)
levels(woe_data$No.of.times.60.DPD.or.worse.in.last.12.months) <-c(-.36,.21,.69)

#-----------------------------------------------------#

# Variable - No of times 30 DPD or worse in last 12 months

# For this variable WOE is monotonic, we will bucket according to the WOE table
woe_data$No.of.times.30.DPD.or.worse.in.last.12.months <- as.numeric(woe_data$No.of.times.30.DPD.or.worse.in.last.12.months)
woe_data$No.of.times.30.DPD.or.worse.in.last.12.months[which(woe_data$No.of.times.30.DPD.or.worse.in.last.12.months == 0)] <- '0'
woe_data$No.of.times.30.DPD.or.worse.in.last.12.months[which(woe_data$No.of.times.30.DPD.or.worse.in.last.12.months >= 1 & woe_data$No.of.times.30.DPD.or.worse.in.last.12.months <= 2)] <- '1_2'
woe_data$No.of.times.30.DPD.or.worse.in.last.12.months[which(woe_data$No.of.times.30.DPD.or.worse.in.last.12.months >= 3 & woe_data$No.of.times.30.DPD.or.worse.in.last.12.months <= 9)] <- '3_9'

# Creating the WOE Values
woe_data$No.of.times.30.DPD.or.worse.in.last.12.months <- as.factor(woe_data$No.of.times.30.DPD.or.worse.in.last.12.months)
levels(woe_data$No.of.times.30.DPD.or.worse.in.last.12.months) <-c(-.38,.28,.80)

#-----------------------------------------------------#

# Variable - Avgas CC Utilization in last 12 months

# Changing the column name
colnames(woe_data)[colnames(woe_data)=="Avgas.CC.Utilization.in.last.12.months"] <- "Avg_Utility_12Mths"

# For this variable WOE is monotonic, we will bucket according to the WOE table
woe_data$Avg_Utility_12Mths <- as.numeric(woe_data$Avg_Utility_12Mths)
woe_data$Avg_Utility_12Mths[which(woe_data$Avg_Utility_12Mths == 0 | woe_data$Avg_Utility_12Mths == 1 | woe_data$Avg_Utility_12Mths == 2 | 
                                    woe_data$Avg_Utility_12Mths == 3 | woe_data$Avg_Utility_12Mths == 4)] <- '0_4'
woe_data$Avg_Utility_12Mths[which(woe_data$Avg_Utility_12Mths == 5 | woe_data$Avg_Utility_12Mths == 6)] <- '5_6'
woe_data$Avg_Utility_12Mths[which(woe_data$Avg_Utility_12Mths == 7 | woe_data$Avg_Utility_12Mths == 8)] <- '7_8'
woe_data$Avg_Utility_12Mths[which(woe_data$Avg_Utility_12Mths == 9 | woe_data$Avg_Utility_12Mths == 10 | woe_data$Avg_Utility_12Mths == 11)] <- '9_11'
woe_data$Avg_Utility_12Mths[which(woe_data$Avg_Utility_12Mths == 12 | woe_data$Avg_Utility_12Mths == 13 | woe_data$Avg_Utility_12Mths == 14)] <- '12_14'
woe_data$Avg_Utility_12Mths[which(woe_data$Avg_Utility_12Mths == 15 | woe_data$Avg_Utility_12Mths == 16 | woe_data$Avg_Utility_12Mths == 17 | 
                                    woe_data$Avg_Utility_12Mths == 18 | woe_data$Avg_Utility_12Mths == 19 |
                                    woe_data$Avg_Utility_12Mths == 20 | woe_data$Avg_Utility_12Mths == 21)] <- '15_21'
woe_data$Avg_Utility_12Mths[which(woe_data$Avg_Utility_12Mths == 22 | woe_data$Avg_Utility_12Mths == 23 | woe_data$Avg_Utility_12Mths == 24 | 
                                    woe_data$Avg_Utility_12Mths == 25 | woe_data$Avg_Utility_12Mths == 26 | woe_data$Avg_Utility_12Mths == 27 |
                                    woe_data$Avg_Utility_12Mths == 28 | woe_data$Avg_Utility_12Mths == 29 | woe_data$Avg_Utility_12Mths == 30 |
                                    woe_data$Avg_Utility_12Mths == 31 | woe_data$Avg_Utility_12Mths == 32 | woe_data$Avg_Utility_12Mths == 33 |
                                    woe_data$Avg_Utility_12Mths == 34 | woe_data$Avg_Utility_12Mths == 35 | woe_data$Avg_Utility_12Mths == 36 |
                                    woe_data$Avg_Utility_12Mths == 37)] <- '22_37'
woe_data$Avg_Utility_12Mths[which(woe_data$Avg_Utility_12Mths == 38 | woe_data$Avg_Utility_12Mths == 39 | woe_data$Avg_Utility_12Mths == 40 | 
                                    woe_data$Avg_Utility_12Mths == 41 | woe_data$Avg_Utility_12Mths == 42 | woe_data$Avg_Utility_12Mths == 43 |
                                    woe_data$Avg_Utility_12Mths == 44 | woe_data$Avg_Utility_12Mths == 45 | woe_data$Avg_Utility_12Mths == 46 |
                                    woe_data$Avg_Utility_12Mths == 47 | woe_data$Avg_Utility_12Mths == 48 | woe_data$Avg_Utility_12Mths == 49 |
                                    woe_data$Avg_Utility_12Mths == 50 | woe_data$Avg_Utility_12Mths == 51)] <- '38_51'
woe_data$Avg_Utility_12Mths[which(woe_data$Avg_Utility_12Mths == 52 | woe_data$Avg_Utility_12Mths == 53 | woe_data$Avg_Utility_12Mths == 54 | 
                                    woe_data$Avg_Utility_12Mths == 55 | woe_data$Avg_Utility_12Mths == 56 | woe_data$Avg_Utility_12Mths == 57 |
                                    woe_data$Avg_Utility_12Mths == 58 | woe_data$Avg_Utility_12Mths == 59 | woe_data$Avg_Utility_12Mths == 60 |
                                    woe_data$Avg_Utility_12Mths == 61 | woe_data$Avg_Utility_12Mths == 62 | woe_data$Avg_Utility_12Mths == 63 |
                                    woe_data$Avg_Utility_12Mths == 64 | woe_data$Avg_Utility_12Mths == 65 | woe_data$Avg_Utility_12Mths == 66 |
                                    woe_data$Avg_Utility_12Mths == 67 | woe_data$Avg_Utility_12Mths == 68 | woe_data$Avg_Utility_12Mths == 69 |
                                    woe_data$Avg_Utility_12Mths == 70 | woe_data$Avg_Utility_12Mths == 71)] <- '52_71'
woe_data$Avg_Utility_12Mths[which(woe_data$Avg_Utility_12Mths == 72 | woe_data$Avg_Utility_12Mths == 73 | woe_data$Avg_Utility_12Mths == 74 | 
                                    woe_data$Avg_Utility_12Mths == 75 | woe_data$Avg_Utility_12Mths == 76 | woe_data$Avg_Utility_12Mths == 77 |
                                    woe_data$Avg_Utility_12Mths == 78 | woe_data$Avg_Utility_12Mths == 79 | woe_data$Avg_Utility_12Mths == 80 |
                                    woe_data$Avg_Utility_12Mths == 81 | woe_data$Avg_Utility_12Mths == 82 | woe_data$Avg_Utility_12Mths == 83 |
                                    woe_data$Avg_Utility_12Mths == 84 | woe_data$Avg_Utility_12Mths == 85 | woe_data$Avg_Utility_12Mths == 86 |
                                    woe_data$Avg_Utility_12Mths == 87 | woe_data$Avg_Utility_12Mths == 88 | woe_data$Avg_Utility_12Mths == 89 |
                                    woe_data$Avg_Utility_12Mths == 90 | woe_data$Avg_Utility_12Mths == 91 | woe_data$Avg_Utility_12Mths == 92 |
                                    woe_data$Avg_Utility_12Mths == 93 | woe_data$Avg_Utility_12Mths == 94 | woe_data$Avg_Utility_12Mths == 95 |
                                    woe_data$Avg_Utility_12Mths == 96 | woe_data$Avg_Utility_12Mths == 97 | woe_data$Avg_Utility_12Mths == 98 |
                                    woe_data$Avg_Utility_12Mths == 99 | woe_data$Avg_Utility_12Mths == 100 | woe_data$Avg_Utility_12Mths == 101 |
                                    woe_data$Avg_Utility_12Mths == 102 | woe_data$Avg_Utility_12Mths == 103 | woe_data$Avg_Utility_12Mths == 104 |
                                    woe_data$Avg_Utility_12Mths == 105 | woe_data$Avg_Utility_12Mths == 106 | woe_data$Avg_Utility_12Mths == 107 |
                                    woe_data$Avg_Utility_12Mths == 108 | woe_data$Avg_Utility_12Mths == 109 | woe_data$Avg_Utility_12Mths == 110 |
                                    woe_data$Avg_Utility_12Mths == 111 | woe_data$Avg_Utility_12Mths == 112 |
                                    woe_data$Avg_Utility_12Mths == 113)] <- '72_113'

# Creating the WOE Values
woe_data$Avg_Utility_12Mths <- as.factor(woe_data$Avg_Utility_12Mths)
levels(woe_data$Avg_Utility_12Mths) <-c(-.80,-.80,-.79,-.67,-.47,-.08,.48,.59,.57,.38)

#-----------------------------------------------------#

# Variable - No of trades opened in last 6 months

# We had already tried with coarse classing but the WOE did not became monotonic
# Hence we will go with the original buckets
woe_data$No.of.trades.opened.in.last.6.months <- as.numeric(woe_data$No.of.trades.opened.in.last.6.months)
woe_data$No.of.trades.opened.in.last.6.months[which(woe_data$No.of.trades.opened.in.last.6.months >= 5 & woe_data$No.of.trades.opened.in.last.6.months <= 12)] <- '5_12'

# Creating the WOE Values
woe_data$No.of.trades.opened.in.last.6.months <- as.factor(woe_data$No.of.trades.opened.in.last.6.months)
levels(woe_data$No.of.trades.opened.in.last.6.months) <-c(-.75,-.48,.24,.44,.52,.13)

#-----------------------------------------------------#

# Variable - No of PL trades opened in last 6 months

# We had already tried with coarse classing but the WOE did not became monotonic
# Hence we will go with the original buckets
woe_data$No.of.PL.trades.opened.in.last.6.months <- as.numeric(woe_data$No.of.PL.trades.opened.in.last.6.months)
woe_data$No.of.PL.trades.opened.in.last.6.months[which(woe_data$No.of.PL.trades.opened.in.last.6.months >= 3 & woe_data$No.of.PL.trades.opened.in.last.6.months <= 6)] <- '3_6'

# Creating the WOE Values
woe_data$No.of.PL.trades.opened.in.last.6.months <- as.factor(woe_data$No.of.PL.trades.opened.in.last.6.months)
levels(woe_data$No.of.PL.trades.opened.in.last.6.months) <-c(-.68,.20,.44,.36)

#-----------------------------------------------------#

# Variable - No of PL trades opened in last 12 months

# We had already tried with coarse classing but the WOE did not became monotonic
# Hence we will go with the original buckets
woe_data$No.of.PL.trades.opened.in.last.12.months <- as.numeric(woe_data$No.of.PL.trades.opened.in.last.12.months)
woe_data$No.of.PL.trades.opened.in.last.12.months[which(woe_data$No.of.PL.trades.opened.in.last.12.months >= 6 & woe_data$No.of.PL.trades.opened.in.last.12.months <= 12)] <- '6_12'

# Creating the WOE Values
woe_data$No.of.PL.trades.opened.in.last.12.months <- as.factor(woe_data$No.of.PL.trades.opened.in.last.12.months)
levels(woe_data$No.of.PL.trades.opened.in.last.12.months) <-c(-.95,-.13,.25,.42,.50,.42,.23)

#-----------------------------------------------------#

# Variable - No of Inquiries in last 6 months (excluding home & auto loans)

# coarse classing gave us a WOE that is monotonic for this variable.
temp_value_exc_home_auto_6mths_woe <- woe_data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.
woe_data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.[which(temp_value_exc_home_auto_6mths_woe==0)] <- '0'
woe_data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.[which(temp_value_exc_home_auto_6mths_woe>=1 & temp_value_exc_home_auto_6mths_woe<=4)] <- '1_4'
woe_data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.[which(temp_value_exc_home_auto_6mths_woe>=5 & temp_value_exc_home_auto_6mths_woe<=10)] <- '5_10'

# Creating the WOE Values
woe_data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. <- as.factor(woe_data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.)
levels(woe_data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.) <-c(-.76,-.30,.01)

#-----------------------------------------------------#

# Variable - No of Inquiries in last 12 months (excluding home & auto loans)

# Changing the column name
colnames(woe_data)[colnames(woe_data)=="No.of.Inquiries.in.last.12.months..excluding.home...auto.loans."] <- "Noofenq_12Mths_exc_hmat"

# We had already tried with coarse classing but the WOE did not became monotonic
# Hence we will go with the original buckets
woe_data$Noofenq_12Mths_exc_hmat <- as.numeric(woe_data$Noofenq_12Mths_exc_hmat)
woe_data$Noofenq_12Mths_exc_hmat[which(woe_data$Noofenq_12Mths_exc_hmat == 6 | woe_data$Noofenq_12Mths_exc_hmat == 7 |
                                         woe_data$Noofenq_12Mths_exc_hmat == 8)] <- '6_8'
woe_data$Noofenq_12Mths_exc_hmat[which(woe_data$Noofenq_12Mths_exc_hmat == 9 | woe_data$Noofenq_12Mths_exc_hmat == 10 |
                                         woe_data$Noofenq_12Mths_exc_hmat == 11 | woe_data$Noofenq_12Mths_exc_hmat == 12 |
                                         woe_data$Noofenq_12Mths_exc_hmat == 13 | woe_data$Noofenq_12Mths_exc_hmat == 14 |
                                         woe_data$Noofenq_12Mths_exc_hmat == 15 | woe_data$Noofenq_12Mths_exc_hmat == 16 |
                                         woe_data$Noofenq_12Mths_exc_hmat == 17 | woe_data$Noofenq_12Mths_exc_hmat == 18 |
                                         woe_data$Noofenq_12Mths_exc_hmat == 19 | woe_data$Noofenq_12Mths_exc_hmat == 20)] <- '9_20'


# Creating the WOE Values
woe_data$Noofenq_12Mths_exc_hmat <- as.factor(woe_data$Noofenq_12Mths_exc_hmat)
levels(woe_data$Noofenq_12Mths_exc_hmat) <-c(-1.14,-.02,.14,.17,.25,.58,.48,.01)

#-----------------------------------------------------#

# Variable - Presence of open home loan

# Creating the WOE Values, as observed WOE was monotonic for this variable
woe_data$Presence.of.open.home.loan <- as.factor(woe_data$Presence.of.open.home.loan)
levels(woe_data$Presence.of.open.home.loan) <-c(.07,-.23)

#-----------------------------------------------------#

# Variable - Presence of open auto loan

# Creating the WOE Values, as observed WOE was monotonic for this variable
woe_data$Presence.of.open.auto.loan <- as.factor(woe_data$Presence.of.open.auto.loan)
levels(woe_data$Presence.of.open.auto.loan) <-c(.01,-.14)

#-----------------------------------------------------#

# Variable - No of trades opened in last 12 months

# We had already tried with coarse classing but the WOE did not became monotonic
# Hence we will go with the original buckets
woe_data$No.of.trades.opened.in.last.12.months <- as.numeric(woe_data$No.of.trades.opened.in.last.12.months)
woe_data$No.of.trades.opened.in.last.12.months[which(woe_data$No.of.trades.opened.in.last.12.months == 0)] <- '0'
woe_data$No.of.trades.opened.in.last.12.months[which(woe_data$No.of.trades.opened.in.last.12.months == 1)] <- '1'
woe_data$No.of.trades.opened.in.last.12.months[which(woe_data$No.of.trades.opened.in.last.12.months == 2)] <- '2'
woe_data$No.of.trades.opened.in.last.12.months[which(woe_data$No.of.trades.opened.in.last.12.months == 3 |
                                                       woe_data$No.of.trades.opened.in.last.12.months == 4)] <- '3_4'
woe_data$No.of.trades.opened.in.last.12.months[which(woe_data$No.of.trades.opened.in.last.12.months == 5)] <- '5'
woe_data$No.of.trades.opened.in.last.12.months[which(woe_data$No.of.trades.opened.in.last.12.months == 6 |
                                                       woe_data$No.of.trades.opened.in.last.12.months == 7)] <- '6_7'
woe_data$No.of.trades.opened.in.last.12.months[which(woe_data$No.of.trades.opened.in.last.12.months == 8 |
                                                       woe_data$No.of.trades.opened.in.last.12.months == 9)] <- '8_9'
woe_data$No.of.trades.opened.in.last.12.months[which(woe_data$No.of.trades.opened.in.last.12.months == 10 |
                                                       woe_data$No.of.trades.opened.in.last.12.months == 11 |
                                                       woe_data$No.of.trades.opened.in.last.12.months == 12)] <- '10_12'
woe_data$No.of.trades.opened.in.last.12.months[which(woe_data$No.of.trades.opened.in.last.12.months == 13 |
                                                       woe_data$No.of.trades.opened.in.last.12.months == 14 |
                                                       woe_data$No.of.trades.opened.in.last.12.months == 15 |
                                                       woe_data$No.of.trades.opened.in.last.12.months == 16 |
                                                       woe_data$No.of.trades.opened.in.last.12.months == 17 |
                                                       woe_data$No.of.trades.opened.in.last.12.months == 18 |
                                                       woe_data$No.of.trades.opened.in.last.12.months == 19 |
                                                       woe_data$No.of.trades.opened.in.last.12.months == 20 |
                                                       woe_data$No.of.trades.opened.in.last.12.months == 21 |
                                                       woe_data$No.of.trades.opened.in.last.12.months == 22 |
                                                       woe_data$No.of.trades.opened.in.last.12.months == 23 |
                                                       woe_data$No.of.trades.opened.in.last.12.months == 24 |
                                                       woe_data$No.of.trades.opened.in.last.12.months == 25 |
                                                       woe_data$No.of.trades.opened.in.last.12.months == 26 |
                                                       woe_data$No.of.trades.opened.in.last.12.months == 27 |
                                                       woe_data$No.of.trades.opened.in.last.12.months == 28)] <- '13_28'

# Creating the WOE Values
woe_data$No.of.trades.opened.in.last.12.months <- as.factor(woe_data$No.of.trades.opened.in.last.12.months)
levels(woe_data$No.of.trades.opened.in.last.12.months) <-c(-.91,-1.01,-.81,.01,.21,.45,.57,.48,.01)

#-----------------------------------------------------#

# Variable - Total No of Trades

# We had already tried with coarse classing but the WOE did not became monotonic
# Hence we will go with the original buckets
woe_data$Total.No.of.Trades <- as.numeric(woe_data$Total.No.of.Trades)
woe_data$Total.No.of.Trades[which(woe_data$Total.No.of.Trades == 1)] <- '1'
woe_data$Total.No.of.Trades[which(woe_data$Total.No.of.Trades == 2)] <- '2'
woe_data$Total.No.of.Trades[which(woe_data$Total.No.of.Trades == 3)] <- '3'
woe_data$Total.No.of.Trades[which(woe_data$Total.No.of.Trades == 4)] <- '4'
woe_data$Total.No.of.Trades[which(woe_data$Total.No.of.Trades == 5)] <- '5'
woe_data$Total.No.of.Trades[which(woe_data$Total.No.of.Trades == 6 | woe_data$Total.No.of.Trades == 7)] <- '6_7'
woe_data$Total.No.of.Trades[which(woe_data$Total.No.of.Trades == 8)] <- '8'
woe_data$Total.No.of.Trades[which(woe_data$Total.No.of.Trades == 9 | woe_data$Total.No.of.Trades == 10)] <- '9_10'
woe_data$Total.No.of.Trades[which(woe_data$Total.No.of.Trades == 11 | woe_data$Total.No.of.Trades == 12 |
                                    woe_data$Total.No.of.Trades == 13 | woe_data$Total.No.of.Trades == 14 |
                                    woe_data$Total.No.of.Trades == 15 | woe_data$Total.No.of.Trades == 16 |
                                    woe_data$Total.No.of.Trades == 17 | woe_data$Total.No.of.Trades == 18 |
                                    woe_data$Total.No.of.Trades == 19)] <- '11_19'
woe_data$Total.No.of.Trades[which(woe_data$Total.No.of.Trades == 20 | woe_data$Total.No.of.Trades == 21 |
                                    woe_data$Total.No.of.Trades == 22 | woe_data$Total.No.of.Trades == 23 |
                                    woe_data$Total.No.of.Trades == 24 | woe_data$Total.No.of.Trades == 25 |
                                    woe_data$Total.No.of.Trades == 26 | woe_data$Total.No.of.Trades == 27 |
                                    woe_data$Total.No.of.Trades == 28 | woe_data$Total.No.of.Trades == 29 |
                                    woe_data$Total.No.of.Trades == 30 | woe_data$Total.No.of.Trades == 31 |
                                    woe_data$Total.No.of.Trades == 32 | woe_data$Total.No.of.Trades == 33 |
                                    woe_data$Total.No.of.Trades == 34 | woe_data$Total.No.of.Trades == 35 |
                                    woe_data$Total.No.of.Trades == 36 | woe_data$Total.No.of.Trades == 37 |
                                    woe_data$Total.No.of.Trades == 38 | woe_data$Total.No.of.Trades == 39 |
                                    woe_data$Total.No.of.Trades == 40 | woe_data$Total.No.of.Trades == 41 |
                                    woe_data$Total.No.of.Trades == 42 | woe_data$Total.No.of.Trades == 43 |
                                    woe_data$Total.No.of.Trades == 44)] <- '20_44'


# Creating the WOE Values
woe_data$Total.No.of.Trades <- as.factor(woe_data$Total.No.of.Trades)
levels(woe_data$Total.No.of.Trades) <-c(-1.06,-1.03,-.70,-.44,-.05,.22,.46,.54,.43,-.07)

#-----------------------------------------------------#

# Variable - Outstanding Balance

# We tried with coarse classing for the bins where the monotonic behaviour of WOE is breaking,
# but the trend is not becoming monotonic, hence we will go with the original binning.

# First we will scale it to thousand level and create a new variable.
woe_data$Outstanding.Balance_scale <- woe_data$Outstanding.Balance/1000

# Now we will bin the newly created variable and again create a new variable
woe_data$Outstanding.Balance_binned <- cut(woe_data$Outstanding.Balance_scale,
                                           c(-0.1,7.790,56.368,392.876,590.337,777.912,976.123,1362,2962,3289,5219),
                                           include.lowest = FALSE)

# Changing to character and renaming the bins.
woe_data$Outstanding.Balance_binned <- as.character(woe_data$Outstanding.Balance_binned)
woe_data$Outstanding.Balance_binned[which(woe_data$Outstanding.Balance_binned == "(-0.1,7.79]")] <- "0_7790"
woe_data$Outstanding.Balance_binned[which(woe_data$Outstanding.Balance_binned == "(7.79,56.4]")] <- "7791_56368"
woe_data$Outstanding.Balance_binned[which(woe_data$Outstanding.Balance_binned == "(56.4,393]")] <- "56516_392876"
woe_data$Outstanding.Balance_binned[which(woe_data$Outstanding.Balance_binned == "(393,590]")] <- "392909_590337"
woe_data$Outstanding.Balance_binned[which(woe_data$Outstanding.Balance_binned == "(590,778]")] <- "590343_777912"
woe_data$Outstanding.Balance_binned[which(woe_data$Outstanding.Balance_binned == "(778,976]")] <- "777936_976123"
woe_data$Outstanding.Balance_binned[which(woe_data$Outstanding.Balance_binned == "(976,1.36e+03]")] <- "976147_1362729"
woe_data$Outstanding.Balance_binned[which(woe_data$Outstanding.Balance_binned == "(1.36e+03,2.96e+03]")] <- "1362827_2962087"
woe_data$Outstanding.Balance_binned[which(woe_data$Outstanding.Balance_binned == "(2.96e+03,3.29e+03]")] <- "2962089_3289690"
woe_data$Outstanding.Balance_binned[which(woe_data$Outstanding.Balance_binned == "(3.29e+03,5.22e+03]")] <- "3289827_5218801"

# Creating the WOE Values
woe_data$Outstanding.Balance_binned <- as.factor(woe_data$Outstanding.Balance_binned)
levels(woe_data$Outstanding.Balance_binned) <-c(.97,.85,.07,.27,.46,.41,.38,.39,.80,.29)

#-----------------------------------------------------#

# Taking only the variables that are required
woe_final <- woe_data[, c(29,2:15,17,18,20:22,24:26,30:33,35)]

#-----------------------------------------------------#
# MODEL BUILDING ALL VARIABLES TAKING WOE VALUES (MODEL-3)
#-----------------------------------------------------#

#Setting the seed to 50# 
set.seed(50)

#Now we randomly generate row indices for train dataset
trainindices_woe <- sample(1:nrow(woe_final), 0.7*nrow(woe_final))

#Generating the train data set
woe_final_train <- woe_final[trainindices_woe,]

#Storing the rest of the observations into an object "woe_final_test".
woe_final_test <- woe_final[-trainindices_woe,]

#-----------------------------------------------------#
#              Model creation starts
#-----------------------------------------------------#

# Creating the first classification model from the woe training data set#
model_woe_1 <- glm(Performance.Tag.y ~ ., data = woe_final_train, family = "binomial")

# Checking the summary of model 
summary(model_woe_1)

# Creating the second model using Stepwise selection
model_woe_2 <- stepAIC(model_woe_1, direction="both")
summary(model_woe_2)
vif(model_woe_2)

model_woe_3 <- glm(formula = Performance.Tag.y ~ No.of.times.30.DPD.or.worse.in.last.6.months + 
                      No.of.times.30.DPD.or.worse.in.last.12.months + Avg_Utility_12Mths + 
                      No.of.PL.trades.opened.in.last.12.months + No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
                      Noofenq_12Mths_exc_hmat + Mth_in_res_binned, family = "binomial", 
                      data = woe_final_train)

summary(model_woe_3)

model_woe_4 <- glm(formula = Performance.Tag.y ~ No.of.times.30.DPD.or.worse.in.last.6.months + 
                     No.of.times.30.DPD.or.worse.in.last.12.months + Avg_Utility_12Mths + 
                     No.of.PL.trades.opened.in.last.12.months +  
                     Noofenq_12Mths_exc_hmat + Mth_in_res_binned, family = "binomial", 
                     data = woe_final_train)

summary(model_woe_4)

model_woe_5 <- glm(formula = Performance.Tag.y ~ No.of.times.30.DPD.or.worse.in.last.6.months + 
                     No.of.times.30.DPD.or.worse.in.last.12.months + Avg_Utility_12Mths + 
                     No.of.PL.trades.opened.in.last.12.months +  
                     Noofenq_12Mths_exc_hmat, family = "binomial", 
                     data = woe_final_train)

summary(model_woe_5)

model_woe_6 <- glm(formula = Performance.Tag.y ~ No.of.times.30.DPD.or.worse.in.last.12.months + 
                     Avg_Utility_12Mths + 
                     No.of.PL.trades.opened.in.last.12.months +  
                     Noofenq_12Mths_exc_hmat, family = "binomial", 
                     data = woe_final_train)

summary(model_woe_6)

# We see that "Number of enquiries 12 months excluding home and auto loan" along
# with "number of PL trades opened in 12 last months" and "average utility 12 months"
# seems to be the most significant variables.

# We will not evaluate the model that was build with WOE values
# As this was done just to check the important variable that comes
# out, when we use WOE values. Now we will build the model using all
# the variables via dummy variable creation.v

#-----------------------------------------------------#
#-----------------------------------------------------#
#-----------------------------------------------------#


#-----------------------------------------------------#
# MODEL BUILDING ALL VARIABLES TAKING DUMMY VARIABLES (MODEL-4)
#-----------------------------------------------------#

# Creating a dataframe taking all the variables.
combined_data <- cb_data[,c(13,2:4,6,8,9,14:116,118:126)]
combined_data$Performance.Tag.y <- as.factor(combined_data$Performance.Tag.y)

#-----------------------------------------------------------------#
# Eye balling the correlation among all the variables#
#-----------------------------------------------------------------#

#Checking if the correlation matrix to check on insights.
correlate <- cor(combined_data)
View(correlate)

#-----------------------------------------------------------------#
# Dividing combined data into two datasets, train and test datasets
#-----------------------------------------------------------------#

#Setting the seed to 50# 
set.seed(50)

#Now we randomly generate row indices for train dataset
trainindices_combined <- sample(1:nrow(combined_data), 0.7*nrow(combined_data))

#Generating the train data set
combined_data_train <- combined_data[trainindices_combined,]

#Storing the rest of the observations into an object "combined_data_test".
combined_data_test <- combined_data[-trainindices_combined,]

#----------------------------------------------------------------#
#                    Model creation starts
#----------------------------------------------------------------#

# Creating the first classification model from the combined training data set#
combined_data_1 <- glm(Performance.Tag.y ~ ., data = combined_data_train, family = "binomial")

# Checking the summary of model 
summary(combined_data_1)

# Creating the second model using Stepwise selection
combined_data_2 <- stepAIC(combined_data_1, direction="both")
summary(combined_data_2)
vif(combined_data_2)

# removing No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.5_10 as it has high p-value
combined_data_3 <- glm(formula = Performance.Tag.y ~ No.of.dependents2 + No.of.dependents4 + 
                         EducationOthers + Mth_in_res_binned6_9 + Mth_in_res_binned73_97 + 
                         Mth_in_comp_binned20_26 + Mth_in_comp_binned27_33 + Mth_in_comp_binned41_47 + 
                         Mth_in_comp_binned48_53 + Mth_in_comp_binned54_61 + Mth_in_comp_binned6_12 + 
                         Mth_in_comp_binned62_133 + Income_binned11_16 + Income_binned37_41 + 
                         No.of.times.30.DPD.or.worse.in.last.6.months1 + No.of.times.30.DPD.or.worse.in.last.6.months2_7 + 
                         No.of.times.30.DPD.or.worse.in.last.12.months3_9 + Avg_Utility_12Mths22_37 + 
                         Avg_Utility_12Mths38_51 + Avg_Utility_12Mths52_71 + Avg_Utility_12Mths72_113 + 
                         No.of.PL.trades.opened.in.last.6.months3_6 + No.of.PL.trades.opened.in.last.12.months2 + 
                         No.of.PL.trades.opened.in.last.12.months3 + No.of.PL.trades.opened.in.last.12.months4 + 
                         No.of.PL.trades.opened.in.last.12.months5 + No.of.PL.trades.opened.in.last.12.months6_12 + 
                         Noofenq_12Mths_exc_hmat1 + Noofenq_12Mths_exc_hmat2 + Noofenq_12Mths_exc_hmat3 + 
                         Noofenq_12Mths_exc_hmat4 + Noofenq_12Mths_exc_hmat5 + Noofenq_12Mths_exc_hmat6_8 + 
                         Noofenq_12Mths_exc_hmat9_20 + No.of.trades.opened.in.last.12.months1 + 
                         No.of.trades.opened.in.last.12.months10_12 + No.of.trades.opened.in.last.12.months2 + 
                         No.of.trades.opened.in.last.12.months8_9 + Total.No.of.Trades2 + 
                         Total.No.of.Trades3 + Total.No.of.Trades4 + Total.No.of.Trades5 + 
                         Total.No.of.Trades6_7 + Total.No.of.Trades8 + Outstanding.Balance_binned2962089_3289690 + 
                         Outstanding.Balance_binned777936_976123, family = "binomial", 
                         data = combined_data_train)

summary(combined_data_3)
vif(combined_data_3)

# removing Outstanding.Balance_binned777936_976123 as it has high p-value
combined_data_4 <- glm(formula = Performance.Tag.y ~ No.of.dependents2 + No.of.dependents4 + 
                         EducationOthers + Mth_in_res_binned6_9 + Mth_in_res_binned73_97 + 
                         Mth_in_comp_binned20_26 + Mth_in_comp_binned27_33 + Mth_in_comp_binned41_47 + 
                         Mth_in_comp_binned48_53 + Mth_in_comp_binned54_61 + Mth_in_comp_binned6_12 + 
                         Mth_in_comp_binned62_133 + Income_binned11_16 + Income_binned37_41 + 
                         No.of.times.30.DPD.or.worse.in.last.6.months1 + No.of.times.30.DPD.or.worse.in.last.6.months2_7 + 
                         No.of.times.30.DPD.or.worse.in.last.12.months3_9 + Avg_Utility_12Mths22_37 + 
                         Avg_Utility_12Mths38_51 + Avg_Utility_12Mths52_71 + Avg_Utility_12Mths72_113 + 
                         No.of.PL.trades.opened.in.last.6.months3_6 + No.of.PL.trades.opened.in.last.12.months2 + 
                         No.of.PL.trades.opened.in.last.12.months3 + No.of.PL.trades.opened.in.last.12.months4 + 
                         No.of.PL.trades.opened.in.last.12.months5 + No.of.PL.trades.opened.in.last.12.months6_12 + 
                         Noofenq_12Mths_exc_hmat1 + Noofenq_12Mths_exc_hmat2 + Noofenq_12Mths_exc_hmat3 + 
                         Noofenq_12Mths_exc_hmat4 + Noofenq_12Mths_exc_hmat5 + Noofenq_12Mths_exc_hmat6_8 + 
                         Noofenq_12Mths_exc_hmat9_20 + No.of.trades.opened.in.last.12.months1 + 
                         No.of.trades.opened.in.last.12.months10_12 + No.of.trades.opened.in.last.12.months2 + 
                         No.of.trades.opened.in.last.12.months8_9 + Total.No.of.Trades2 + 
                         Total.No.of.Trades3 + Total.No.of.Trades4 + Total.No.of.Trades5 + 
                         Total.No.of.Trades6_7 + Total.No.of.Trades8 + Outstanding.Balance_binned2962089_3289690, 
                         family = "binomial", data = combined_data_train)

summary(combined_data_4)
vif(combined_data_4)

# removing No.of.trades.opened.in.last.12.months8_9 as it has high p-value
combined_data_5 <- glm(formula = Performance.Tag.y ~ No.of.dependents2 + No.of.dependents4 + 
                         EducationOthers + Mth_in_res_binned6_9 + Mth_in_res_binned73_97 + 
                         Mth_in_comp_binned20_26 + Mth_in_comp_binned27_33 + Mth_in_comp_binned41_47 + 
                         Mth_in_comp_binned48_53 + Mth_in_comp_binned54_61 + Mth_in_comp_binned6_12 + 
                         Mth_in_comp_binned62_133 + Income_binned11_16 + Income_binned37_41 + 
                         No.of.times.30.DPD.or.worse.in.last.6.months1 + No.of.times.30.DPD.or.worse.in.last.6.months2_7 + 
                         No.of.times.30.DPD.or.worse.in.last.12.months3_9 + Avg_Utility_12Mths22_37 + 
                         Avg_Utility_12Mths38_51 + Avg_Utility_12Mths52_71 + Avg_Utility_12Mths72_113 + 
                         No.of.PL.trades.opened.in.last.6.months3_6 + No.of.PL.trades.opened.in.last.12.months2 + 
                         No.of.PL.trades.opened.in.last.12.months3 + No.of.PL.trades.opened.in.last.12.months4 + 
                         No.of.PL.trades.opened.in.last.12.months5 + No.of.PL.trades.opened.in.last.12.months6_12 + 
                         Noofenq_12Mths_exc_hmat1 + Noofenq_12Mths_exc_hmat2 + Noofenq_12Mths_exc_hmat3 + 
                         Noofenq_12Mths_exc_hmat4 + Noofenq_12Mths_exc_hmat5 + Noofenq_12Mths_exc_hmat6_8 + 
                         Noofenq_12Mths_exc_hmat9_20 + No.of.trades.opened.in.last.12.months1 + 
                         No.of.trades.opened.in.last.12.months10_12 + No.of.trades.opened.in.last.12.months2 + 
                         Total.No.of.Trades2 + 
                         Total.No.of.Trades3 + Total.No.of.Trades4 + Total.No.of.Trades5 + 
                         Total.No.of.Trades6_7 + Total.No.of.Trades8 + Outstanding.Balance_binned2962089_3289690, 
                         family = "binomial", data = combined_data_train)

summary(combined_data_5)
vif(combined_data_5)

# removing No.of.trades.opened.in.last.12.months10_12 as it has high p-value
combined_data_6 <- glm(formula = Performance.Tag.y ~ No.of.dependents2 + No.of.dependents4 + 
                         EducationOthers + Mth_in_res_binned6_9 + Mth_in_res_binned73_97 + 
                         Mth_in_comp_binned20_26 + Mth_in_comp_binned27_33 + Mth_in_comp_binned41_47 + 
                         Mth_in_comp_binned48_53 + Mth_in_comp_binned54_61 + Mth_in_comp_binned6_12 + 
                         Mth_in_comp_binned62_133 + Income_binned11_16 + Income_binned37_41 + 
                         No.of.times.30.DPD.or.worse.in.last.6.months1 + No.of.times.30.DPD.or.worse.in.last.6.months2_7 + 
                         No.of.times.30.DPD.or.worse.in.last.12.months3_9 + Avg_Utility_12Mths22_37 + 
                         Avg_Utility_12Mths38_51 + Avg_Utility_12Mths52_71 + Avg_Utility_12Mths72_113 + 
                         No.of.PL.trades.opened.in.last.6.months3_6 + No.of.PL.trades.opened.in.last.12.months2 + 
                         No.of.PL.trades.opened.in.last.12.months3 + No.of.PL.trades.opened.in.last.12.months4 + 
                         No.of.PL.trades.opened.in.last.12.months5 + No.of.PL.trades.opened.in.last.12.months6_12 + 
                         Noofenq_12Mths_exc_hmat1 + Noofenq_12Mths_exc_hmat2 + Noofenq_12Mths_exc_hmat3 + 
                         Noofenq_12Mths_exc_hmat4 + Noofenq_12Mths_exc_hmat5 + Noofenq_12Mths_exc_hmat6_8 + 
                         Noofenq_12Mths_exc_hmat9_20 + No.of.trades.opened.in.last.12.months1 + 
                         No.of.trades.opened.in.last.12.months2 + 
                         Total.No.of.Trades2 + Total.No.of.Trades3 + Total.No.of.Trades4 + 
                         Total.No.of.Trades5 + Total.No.of.Trades6_7 + Total.No.of.Trades8 + 
                         Outstanding.Balance_binned2962089_3289690, family = "binomial", 
                         data = combined_data_train)

summary(combined_data_6)
vif(combined_data_6)

# removing Total.No.of.Trades6_7 as it has high p-value
combined_data_7 <- glm(formula = Performance.Tag.y ~ No.of.dependents2 + No.of.dependents4 + 
                         EducationOthers + Mth_in_res_binned6_9 + Mth_in_res_binned73_97 + 
                         Mth_in_comp_binned20_26 + Mth_in_comp_binned27_33 + Mth_in_comp_binned41_47 + 
                         Mth_in_comp_binned48_53 + Mth_in_comp_binned54_61 + Mth_in_comp_binned6_12 + 
                         Mth_in_comp_binned62_133 + Income_binned11_16 + Income_binned37_41 + 
                         No.of.times.30.DPD.or.worse.in.last.6.months1 + No.of.times.30.DPD.or.worse.in.last.6.months2_7 + 
                         No.of.times.30.DPD.or.worse.in.last.12.months3_9 + Avg_Utility_12Mths22_37 + 
                         Avg_Utility_12Mths38_51 + Avg_Utility_12Mths52_71 + Avg_Utility_12Mths72_113 + 
                         No.of.PL.trades.opened.in.last.6.months3_6 + No.of.PL.trades.opened.in.last.12.months2 + 
                         No.of.PL.trades.opened.in.last.12.months3 + No.of.PL.trades.opened.in.last.12.months4 + 
                         No.of.PL.trades.opened.in.last.12.months5 + No.of.PL.trades.opened.in.last.12.months6_12 + 
                         Noofenq_12Mths_exc_hmat1 + Noofenq_12Mths_exc_hmat2 + Noofenq_12Mths_exc_hmat3 + 
                         Noofenq_12Mths_exc_hmat4 + Noofenq_12Mths_exc_hmat5 + Noofenq_12Mths_exc_hmat6_8 + 
                         Noofenq_12Mths_exc_hmat9_20 + No.of.trades.opened.in.last.12.months1 + 
                         No.of.trades.opened.in.last.12.months2 + Total.No.of.Trades2 + 
                         Total.No.of.Trades3 + Total.No.of.Trades4 + Total.No.of.Trades5 + 
                         Total.No.of.Trades8 + Outstanding.Balance_binned2962089_3289690, 
                         family = "binomial", data = combined_data_train)

summary(combined_data_7)
vif(combined_data_7)

# removing Total.No.of.Trades2 as it has high p-value
combined_data_8 <- glm(formula = Performance.Tag.y ~ No.of.dependents2 + No.of.dependents4 + 
                         EducationOthers + Mth_in_res_binned6_9 + Mth_in_res_binned73_97 + 
                         Mth_in_comp_binned20_26 + Mth_in_comp_binned27_33 + Mth_in_comp_binned41_47 + 
                         Mth_in_comp_binned48_53 + Mth_in_comp_binned54_61 + Mth_in_comp_binned6_12 + 
                         Mth_in_comp_binned62_133 + Income_binned11_16 + Income_binned37_41 + 
                         No.of.times.30.DPD.or.worse.in.last.6.months1 + No.of.times.30.DPD.or.worse.in.last.6.months2_7 + 
                         No.of.times.30.DPD.or.worse.in.last.12.months3_9 + Avg_Utility_12Mths22_37 + 
                         Avg_Utility_12Mths38_51 + Avg_Utility_12Mths52_71 + Avg_Utility_12Mths72_113 + 
                         No.of.PL.trades.opened.in.last.6.months3_6 + No.of.PL.trades.opened.in.last.12.months2 + 
                         No.of.PL.trades.opened.in.last.12.months3 + No.of.PL.trades.opened.in.last.12.months4 + 
                         No.of.PL.trades.opened.in.last.12.months5 + No.of.PL.trades.opened.in.last.12.months6_12 + 
                         Noofenq_12Mths_exc_hmat1 + Noofenq_12Mths_exc_hmat2 + Noofenq_12Mths_exc_hmat3 + 
                         Noofenq_12Mths_exc_hmat4 + Noofenq_12Mths_exc_hmat5 + Noofenq_12Mths_exc_hmat6_8 + 
                         Noofenq_12Mths_exc_hmat9_20 + No.of.trades.opened.in.last.12.months1 + 
                         No.of.trades.opened.in.last.12.months2 + 
                         Total.No.of.Trades3 + Total.No.of.Trades4 + Total.No.of.Trades5 + 
                         Total.No.of.Trades8 + Outstanding.Balance_binned2962089_3289690, 
                         family = "binomial", data = combined_data_train)

summary(combined_data_8)
vif(combined_data_8)

# removing Total.No.of.Trades5 as it has high p-value
combined_data_9 <- glm(formula = Performance.Tag.y ~ No.of.dependents2 + No.of.dependents4 + 
                         EducationOthers + Mth_in_res_binned6_9 + Mth_in_res_binned73_97 + 
                         Mth_in_comp_binned20_26 + Mth_in_comp_binned27_33 + Mth_in_comp_binned41_47 + 
                         Mth_in_comp_binned48_53 + Mth_in_comp_binned54_61 + Mth_in_comp_binned6_12 + 
                         Mth_in_comp_binned62_133 + Income_binned11_16 + Income_binned37_41 + 
                         No.of.times.30.DPD.or.worse.in.last.6.months1 + No.of.times.30.DPD.or.worse.in.last.6.months2_7 + 
                         No.of.times.30.DPD.or.worse.in.last.12.months3_9 + Avg_Utility_12Mths22_37 + 
                         Avg_Utility_12Mths38_51 + Avg_Utility_12Mths52_71 + Avg_Utility_12Mths72_113 + 
                         No.of.PL.trades.opened.in.last.6.months3_6 + No.of.PL.trades.opened.in.last.12.months2 + 
                         No.of.PL.trades.opened.in.last.12.months3 + No.of.PL.trades.opened.in.last.12.months4 + 
                         No.of.PL.trades.opened.in.last.12.months5 + No.of.PL.trades.opened.in.last.12.months6_12 + 
                         Noofenq_12Mths_exc_hmat1 + Noofenq_12Mths_exc_hmat2 + Noofenq_12Mths_exc_hmat3 + 
                         Noofenq_12Mths_exc_hmat4 + Noofenq_12Mths_exc_hmat5 + Noofenq_12Mths_exc_hmat6_8 + 
                         Noofenq_12Mths_exc_hmat9_20 + No.of.trades.opened.in.last.12.months1 + 
                         No.of.trades.opened.in.last.12.months2 + Total.No.of.Trades3 + 
                         Total.No.of.Trades4 + Total.No.of.Trades8 + 
                         Outstanding.Balance_binned2962089_3289690, family = "binomial", 
                         data = combined_data_train)

summary(combined_data_9)
vif(combined_data_9)

# removing Total.No.of.Trades8 as it has high p-value
combined_data_10 <- glm(formula = Performance.Tag.y ~ No.of.dependents2 + No.of.dependents4 + 
                          EducationOthers + Mth_in_res_binned6_9 + Mth_in_res_binned73_97 + 
                          Mth_in_comp_binned20_26 + Mth_in_comp_binned27_33 + Mth_in_comp_binned41_47 + 
                          Mth_in_comp_binned48_53 + Mth_in_comp_binned54_61 + Mth_in_comp_binned6_12 + 
                          Mth_in_comp_binned62_133 + Income_binned11_16 + Income_binned37_41 + 
                          No.of.times.30.DPD.or.worse.in.last.6.months1 + No.of.times.30.DPD.or.worse.in.last.6.months2_7 + 
                          No.of.times.30.DPD.or.worse.in.last.12.months3_9 + Avg_Utility_12Mths22_37 + 
                          Avg_Utility_12Mths38_51 + Avg_Utility_12Mths52_71 + Avg_Utility_12Mths72_113 + 
                          No.of.PL.trades.opened.in.last.6.months3_6 + No.of.PL.trades.opened.in.last.12.months2 + 
                          No.of.PL.trades.opened.in.last.12.months3 + No.of.PL.trades.opened.in.last.12.months4 + 
                          No.of.PL.trades.opened.in.last.12.months5 + No.of.PL.trades.opened.in.last.12.months6_12 + 
                          Noofenq_12Mths_exc_hmat1 + Noofenq_12Mths_exc_hmat2 + Noofenq_12Mths_exc_hmat3 + 
                          Noofenq_12Mths_exc_hmat4 + Noofenq_12Mths_exc_hmat5 + Noofenq_12Mths_exc_hmat6_8 + 
                          Noofenq_12Mths_exc_hmat9_20 + No.of.trades.opened.in.last.12.months1 + 
                          No.of.trades.opened.in.last.12.months2 + Total.No.of.Trades3 + 
                          Total.No.of.Trades4 + Outstanding.Balance_binned2962089_3289690, 
                          family = "binomial", data = combined_data_train)

summary(combined_data_10)
vif(combined_data_10)

# removing No.of.PL.trades.opened.in.last.6.months3_6 as it has high p-value
combined_data_11 <- glm(formula = Performance.Tag.y ~ No.of.dependents2 + No.of.dependents4 + 
                          EducationOthers + Mth_in_res_binned6_9 + Mth_in_res_binned73_97 + 
                          Mth_in_comp_binned20_26 + Mth_in_comp_binned27_33 + Mth_in_comp_binned41_47 + 
                          Mth_in_comp_binned48_53 + Mth_in_comp_binned54_61 + Mth_in_comp_binned6_12 + 
                          Mth_in_comp_binned62_133 + Income_binned11_16 + Income_binned37_41 + 
                          No.of.times.30.DPD.or.worse.in.last.6.months1 + No.of.times.30.DPD.or.worse.in.last.6.months2_7 + 
                          No.of.times.30.DPD.or.worse.in.last.12.months3_9 + Avg_Utility_12Mths22_37 + 
                          Avg_Utility_12Mths38_51 + Avg_Utility_12Mths52_71 + Avg_Utility_12Mths72_113 + 
                          No.of.PL.trades.opened.in.last.12.months2 + 
                          No.of.PL.trades.opened.in.last.12.months3 + No.of.PL.trades.opened.in.last.12.months4 + 
                          No.of.PL.trades.opened.in.last.12.months5 + No.of.PL.trades.opened.in.last.12.months6_12 + 
                          Noofenq_12Mths_exc_hmat1 + Noofenq_12Mths_exc_hmat2 + Noofenq_12Mths_exc_hmat3 + 
                          Noofenq_12Mths_exc_hmat4 + Noofenq_12Mths_exc_hmat5 + Noofenq_12Mths_exc_hmat6_8 + 
                          Noofenq_12Mths_exc_hmat9_20 + No.of.trades.opened.in.last.12.months1 + 
                          No.of.trades.opened.in.last.12.months2 + Total.No.of.Trades3 + 
                          Total.No.of.Trades4 + Outstanding.Balance_binned2962089_3289690, 
                          family = "binomial", data = combined_data_train)

summary(combined_data_11)
vif(combined_data_11)

# removing No.of.trades.opened.in.last.12.months1 as it has high p-value
combined_data_12 <- glm(formula = Performance.Tag.y ~ No.of.dependents2 + No.of.dependents4 + 
                          EducationOthers + Mth_in_res_binned6_9 + Mth_in_res_binned73_97 + 
                          Mth_in_comp_binned20_26 + Mth_in_comp_binned27_33 + Mth_in_comp_binned41_47 + 
                          Mth_in_comp_binned48_53 + Mth_in_comp_binned54_61 + Mth_in_comp_binned6_12 + 
                          Mth_in_comp_binned62_133 + Income_binned11_16 + Income_binned37_41 + 
                          No.of.times.30.DPD.or.worse.in.last.6.months1 + No.of.times.30.DPD.or.worse.in.last.6.months2_7 + 
                          No.of.times.30.DPD.or.worse.in.last.12.months3_9 + Avg_Utility_12Mths22_37 + 
                          Avg_Utility_12Mths38_51 + Avg_Utility_12Mths52_71 + Avg_Utility_12Mths72_113 + 
                          No.of.PL.trades.opened.in.last.12.months2 + No.of.PL.trades.opened.in.last.12.months3 + 
                          No.of.PL.trades.opened.in.last.12.months4 + No.of.PL.trades.opened.in.last.12.months5 + 
                          No.of.PL.trades.opened.in.last.12.months6_12 + Noofenq_12Mths_exc_hmat1 + 
                          Noofenq_12Mths_exc_hmat2 + Noofenq_12Mths_exc_hmat3 + Noofenq_12Mths_exc_hmat4 + 
                          Noofenq_12Mths_exc_hmat5 + Noofenq_12Mths_exc_hmat6_8 + Noofenq_12Mths_exc_hmat9_20 + 
                          No.of.trades.opened.in.last.12.months2 + 
                          Total.No.of.Trades3 + Total.No.of.Trades4 + Outstanding.Balance_binned2962089_3289690, 
                          family = "binomial", data = combined_data_train)

summary(combined_data_12)
vif(combined_data_12)

# removing No.of.trades.opened.in.last.12.months2 as it has high p-value
combined_data_13 <- glm(formula = Performance.Tag.y ~ No.of.dependents2 + No.of.dependents4 + 
                          EducationOthers + Mth_in_res_binned6_9 + Mth_in_res_binned73_97 + 
                          Mth_in_comp_binned20_26 + Mth_in_comp_binned27_33 + Mth_in_comp_binned41_47 + 
                          Mth_in_comp_binned48_53 + Mth_in_comp_binned54_61 + Mth_in_comp_binned6_12 + 
                          Mth_in_comp_binned62_133 + Income_binned11_16 + Income_binned37_41 + 
                          No.of.times.30.DPD.or.worse.in.last.6.months1 + No.of.times.30.DPD.or.worse.in.last.6.months2_7 + 
                          No.of.times.30.DPD.or.worse.in.last.12.months3_9 + Avg_Utility_12Mths22_37 + 
                          Avg_Utility_12Mths38_51 + Avg_Utility_12Mths52_71 + Avg_Utility_12Mths72_113 + 
                          No.of.PL.trades.opened.in.last.12.months2 + No.of.PL.trades.opened.in.last.12.months3 + 
                          No.of.PL.trades.opened.in.last.12.months4 + No.of.PL.trades.opened.in.last.12.months5 + 
                          No.of.PL.trades.opened.in.last.12.months6_12 + Noofenq_12Mths_exc_hmat1 + 
                          Noofenq_12Mths_exc_hmat2 + Noofenq_12Mths_exc_hmat3 + Noofenq_12Mths_exc_hmat4 + 
                          Noofenq_12Mths_exc_hmat5 + Noofenq_12Mths_exc_hmat6_8 + Noofenq_12Mths_exc_hmat9_20 + 
                          Total.No.of.Trades3 + 
                          Total.No.of.Trades4 + Outstanding.Balance_binned2962089_3289690, 
                          family = "binomial", data = combined_data_train)

summary(combined_data_13)
vif(combined_data_13)

# removing Total.No.of.Trades3 as it has high p-value
combined_data_14 <- glm(formula = Performance.Tag.y ~ No.of.dependents2 + No.of.dependents4 + 
                          EducationOthers + Mth_in_res_binned6_9 + Mth_in_res_binned73_97 + 
                          Mth_in_comp_binned20_26 + Mth_in_comp_binned27_33 + Mth_in_comp_binned41_47 + 
                          Mth_in_comp_binned48_53 + Mth_in_comp_binned54_61 + Mth_in_comp_binned6_12 + 
                          Mth_in_comp_binned62_133 + Income_binned11_16 + Income_binned37_41 + 
                          No.of.times.30.DPD.or.worse.in.last.6.months1 + No.of.times.30.DPD.or.worse.in.last.6.months2_7 + 
                          No.of.times.30.DPD.or.worse.in.last.12.months3_9 + Avg_Utility_12Mths22_37 + 
                          Avg_Utility_12Mths38_51 + Avg_Utility_12Mths52_71 + Avg_Utility_12Mths72_113 + 
                          No.of.PL.trades.opened.in.last.12.months2 + No.of.PL.trades.opened.in.last.12.months3 + 
                          No.of.PL.trades.opened.in.last.12.months4 + No.of.PL.trades.opened.in.last.12.months5 + 
                          No.of.PL.trades.opened.in.last.12.months6_12 + Noofenq_12Mths_exc_hmat1 + 
                          Noofenq_12Mths_exc_hmat2 + Noofenq_12Mths_exc_hmat3 + Noofenq_12Mths_exc_hmat4 + 
                          Noofenq_12Mths_exc_hmat5 + Noofenq_12Mths_exc_hmat6_8 + Noofenq_12Mths_exc_hmat9_20 + 
                          Total.No.of.Trades4 + Outstanding.Balance_binned2962089_3289690, 
                          family = "binomial", data = combined_data_train)

summary(combined_data_14)
vif(combined_data_14)

# removing Total.No.of.Trades4 as it has high p-value
combined_data_15 <- glm(formula = Performance.Tag.y ~ No.of.dependents2 + No.of.dependents4 + 
                          EducationOthers + Mth_in_res_binned6_9 + Mth_in_res_binned73_97 + 
                          Mth_in_comp_binned20_26 + Mth_in_comp_binned27_33 + Mth_in_comp_binned41_47 + 
                          Mth_in_comp_binned48_53 + Mth_in_comp_binned54_61 + Mth_in_comp_binned6_12 + 
                          Mth_in_comp_binned62_133 + Income_binned11_16 + Income_binned37_41 + 
                          No.of.times.30.DPD.or.worse.in.last.6.months1 + No.of.times.30.DPD.or.worse.in.last.6.months2_7 + 
                          No.of.times.30.DPD.or.worse.in.last.12.months3_9 + Avg_Utility_12Mths22_37 + 
                          Avg_Utility_12Mths38_51 + Avg_Utility_12Mths52_71 + Avg_Utility_12Mths72_113 + 
                          No.of.PL.trades.opened.in.last.12.months2 + No.of.PL.trades.opened.in.last.12.months3 + 
                          No.of.PL.trades.opened.in.last.12.months4 + No.of.PL.trades.opened.in.last.12.months5 + 
                          No.of.PL.trades.opened.in.last.12.months6_12 + Noofenq_12Mths_exc_hmat1 + 
                          Noofenq_12Mths_exc_hmat2 + Noofenq_12Mths_exc_hmat3 + Noofenq_12Mths_exc_hmat4 + 
                          Noofenq_12Mths_exc_hmat5 + Noofenq_12Mths_exc_hmat6_8 + Noofenq_12Mths_exc_hmat9_20 + 
                          Outstanding.Balance_binned2962089_3289690, 
                          family = "binomial", data = combined_data_train)

summary(combined_data_15)
vif(combined_data_15)

# removing Income_binned37_41 as it has high p-value
combined_data_16 <- glm(formula = Performance.Tag.y ~ No.of.dependents2 + No.of.dependents4 + 
                          EducationOthers + Mth_in_res_binned6_9 + Mth_in_res_binned73_97 + 
                          Mth_in_comp_binned20_26 + Mth_in_comp_binned27_33 + Mth_in_comp_binned41_47 + 
                          Mth_in_comp_binned48_53 + Mth_in_comp_binned54_61 + Mth_in_comp_binned6_12 + 
                          Mth_in_comp_binned62_133 + Income_binned11_16 + 
                          No.of.times.30.DPD.or.worse.in.last.6.months1 + No.of.times.30.DPD.or.worse.in.last.6.months2_7 + 
                          No.of.times.30.DPD.or.worse.in.last.12.months3_9 + Avg_Utility_12Mths22_37 + 
                          Avg_Utility_12Mths38_51 + Avg_Utility_12Mths52_71 + Avg_Utility_12Mths72_113 + 
                          No.of.PL.trades.opened.in.last.12.months2 + No.of.PL.trades.opened.in.last.12.months3 + 
                          No.of.PL.trades.opened.in.last.12.months4 + No.of.PL.trades.opened.in.last.12.months5 + 
                          No.of.PL.trades.opened.in.last.12.months6_12 + Noofenq_12Mths_exc_hmat1 + 
                          Noofenq_12Mths_exc_hmat2 + Noofenq_12Mths_exc_hmat3 + Noofenq_12Mths_exc_hmat4 + 
                          Noofenq_12Mths_exc_hmat5 + Noofenq_12Mths_exc_hmat6_8 + Noofenq_12Mths_exc_hmat9_20 + 
                          Outstanding.Balance_binned2962089_3289690, family = "binomial", 
                          data = combined_data_train)

summary(combined_data_16)
vif(combined_data_16)

# removing Income_binned11_16 as it has high p-value
combined_data_17 <- glm(formula = Performance.Tag.y ~ No.of.dependents2 + No.of.dependents4 + 
                          EducationOthers + Mth_in_res_binned6_9 + Mth_in_res_binned73_97 + 
                          Mth_in_comp_binned20_26 + Mth_in_comp_binned27_33 + Mth_in_comp_binned41_47 + 
                          Mth_in_comp_binned48_53 + Mth_in_comp_binned54_61 + Mth_in_comp_binned6_12 + 
                          Mth_in_comp_binned62_133 + No.of.times.30.DPD.or.worse.in.last.6.months1 + 
                          No.of.times.30.DPD.or.worse.in.last.6.months2_7 + No.of.times.30.DPD.or.worse.in.last.12.months3_9 + 
                          Avg_Utility_12Mths22_37 + Avg_Utility_12Mths38_51 + Avg_Utility_12Mths52_71 + 
                          Avg_Utility_12Mths72_113 + No.of.PL.trades.opened.in.last.12.months2 + 
                          No.of.PL.trades.opened.in.last.12.months3 + No.of.PL.trades.opened.in.last.12.months4 + 
                          No.of.PL.trades.opened.in.last.12.months5 + No.of.PL.trades.opened.in.last.12.months6_12 + 
                          Noofenq_12Mths_exc_hmat1 + Noofenq_12Mths_exc_hmat2 + Noofenq_12Mths_exc_hmat3 + 
                          Noofenq_12Mths_exc_hmat4 + Noofenq_12Mths_exc_hmat5 + Noofenq_12Mths_exc_hmat6_8 + 
                          Noofenq_12Mths_exc_hmat9_20 + Outstanding.Balance_binned2962089_3289690, 
                          family = "binomial", data = combined_data_train)

summary(combined_data_17)
vif(combined_data_17)

# removing Mth_in_comp_binned41_47 as it has high p-value
combined_data_18 <- glm(formula = Performance.Tag.y ~ No.of.dependents2 + No.of.dependents4 + 
                          EducationOthers + Mth_in_res_binned6_9 + Mth_in_res_binned73_97 + 
                          Mth_in_comp_binned20_26 + Mth_in_comp_binned27_33 + 
                          Mth_in_comp_binned48_53 + Mth_in_comp_binned54_61 + Mth_in_comp_binned6_12 + 
                          Mth_in_comp_binned62_133 + No.of.times.30.DPD.or.worse.in.last.6.months1 + 
                          No.of.times.30.DPD.or.worse.in.last.6.months2_7 + No.of.times.30.DPD.or.worse.in.last.12.months3_9 + 
                          Avg_Utility_12Mths22_37 + Avg_Utility_12Mths38_51 + Avg_Utility_12Mths52_71 + 
                          Avg_Utility_12Mths72_113 + No.of.PL.trades.opened.in.last.12.months2 + 
                          No.of.PL.trades.opened.in.last.12.months3 + No.of.PL.trades.opened.in.last.12.months4 + 
                          No.of.PL.trades.opened.in.last.12.months5 + No.of.PL.trades.opened.in.last.12.months6_12 + 
                          Noofenq_12Mths_exc_hmat1 + Noofenq_12Mths_exc_hmat2 + Noofenq_12Mths_exc_hmat3 + 
                          Noofenq_12Mths_exc_hmat4 + Noofenq_12Mths_exc_hmat5 + Noofenq_12Mths_exc_hmat6_8 + 
                          Noofenq_12Mths_exc_hmat9_20 + Outstanding.Balance_binned2962089_3289690, 
                          family = "binomial", data = combined_data_train)

summary(combined_data_18)
vif(combined_data_18)

# removing Mth_in_comp_binned6_12 as it has high p-value
combined_data_19 <- glm(formula = Performance.Tag.y ~ No.of.dependents2 + No.of.dependents4 + 
                          EducationOthers + Mth_in_res_binned6_9 + Mth_in_res_binned73_97 + 
                          Mth_in_comp_binned20_26 + Mth_in_comp_binned27_33 + Mth_in_comp_binned48_53 + 
                          Mth_in_comp_binned54_61 + Mth_in_comp_binned62_133 + 
                          No.of.times.30.DPD.or.worse.in.last.6.months1 + No.of.times.30.DPD.or.worse.in.last.6.months2_7 + 
                          No.of.times.30.DPD.or.worse.in.last.12.months3_9 + Avg_Utility_12Mths22_37 + 
                          Avg_Utility_12Mths38_51 + Avg_Utility_12Mths52_71 + Avg_Utility_12Mths72_113 + 
                          No.of.PL.trades.opened.in.last.12.months2 + No.of.PL.trades.opened.in.last.12.months3 + 
                          No.of.PL.trades.opened.in.last.12.months4 + No.of.PL.trades.opened.in.last.12.months5 + 
                          No.of.PL.trades.opened.in.last.12.months6_12 + Noofenq_12Mths_exc_hmat1 + 
                          Noofenq_12Mths_exc_hmat2 + Noofenq_12Mths_exc_hmat3 + Noofenq_12Mths_exc_hmat4 + 
                          Noofenq_12Mths_exc_hmat5 + Noofenq_12Mths_exc_hmat6_8 + Noofenq_12Mths_exc_hmat9_20 + 
                          Outstanding.Balance_binned2962089_3289690, family = "binomial", 
                          data = combined_data_train)

summary(combined_data_19)
vif(combined_data_19)

# removing Mth_in_comp_binned20_26 as it has high p-value
combined_data_20 <- glm(formula = Performance.Tag.y ~ No.of.dependents2 + No.of.dependents4 + 
                          EducationOthers + Mth_in_res_binned6_9 + Mth_in_res_binned73_97 + 
                          Mth_in_comp_binned27_33 + Mth_in_comp_binned48_53 + 
                          Mth_in_comp_binned54_61 + Mth_in_comp_binned62_133 + No.of.times.30.DPD.or.worse.in.last.6.months1 + 
                          No.of.times.30.DPD.or.worse.in.last.6.months2_7 + No.of.times.30.DPD.or.worse.in.last.12.months3_9 + 
                          Avg_Utility_12Mths22_37 + Avg_Utility_12Mths38_51 + Avg_Utility_12Mths52_71 + 
                          Avg_Utility_12Mths72_113 + No.of.PL.trades.opened.in.last.12.months2 + 
                          No.of.PL.trades.opened.in.last.12.months3 + No.of.PL.trades.opened.in.last.12.months4 + 
                          No.of.PL.trades.opened.in.last.12.months5 + No.of.PL.trades.opened.in.last.12.months6_12 + 
                          Noofenq_12Mths_exc_hmat1 + Noofenq_12Mths_exc_hmat2 + Noofenq_12Mths_exc_hmat3 + 
                          Noofenq_12Mths_exc_hmat4 + Noofenq_12Mths_exc_hmat5 + Noofenq_12Mths_exc_hmat6_8 + 
                          Noofenq_12Mths_exc_hmat9_20 + Outstanding.Balance_binned2962089_3289690, 
                          family = "binomial", data = combined_data_train)

summary(combined_data_20)
vif(combined_data_20)

# removing Mth_in_comp_binned48_53 as it has high p-value
combined_data_21 <- glm(formula = Performance.Tag.y ~ No.of.dependents2 + No.of.dependents4 + 
                          EducationOthers + Mth_in_res_binned6_9 + Mth_in_res_binned73_97 + 
                          Mth_in_comp_binned27_33 + Mth_in_comp_binned54_61 + 
                          Mth_in_comp_binned62_133 + No.of.times.30.DPD.or.worse.in.last.6.months1 + 
                          No.of.times.30.DPD.or.worse.in.last.6.months2_7 + No.of.times.30.DPD.or.worse.in.last.12.months3_9 + 
                          Avg_Utility_12Mths22_37 + Avg_Utility_12Mths38_51 + Avg_Utility_12Mths52_71 + 
                          Avg_Utility_12Mths72_113 + No.of.PL.trades.opened.in.last.12.months2 + 
                          No.of.PL.trades.opened.in.last.12.months3 + No.of.PL.trades.opened.in.last.12.months4 + 
                          No.of.PL.trades.opened.in.last.12.months5 + No.of.PL.trades.opened.in.last.12.months6_12 + 
                          Noofenq_12Mths_exc_hmat1 + Noofenq_12Mths_exc_hmat2 + Noofenq_12Mths_exc_hmat3 + 
                          Noofenq_12Mths_exc_hmat4 + Noofenq_12Mths_exc_hmat5 + Noofenq_12Mths_exc_hmat6_8 + 
                          Noofenq_12Mths_exc_hmat9_20 + Outstanding.Balance_binned2962089_3289690, 
                          family = "binomial", data = combined_data_train)

summary(combined_data_21)
vif(combined_data_21)

# removing Mth_in_comp_binned54_61 as it has high p-value
combined_data_22 <- glm(formula = Performance.Tag.y ~ No.of.dependents2 + No.of.dependents4 + 
                          EducationOthers + Mth_in_res_binned6_9 + Mth_in_res_binned73_97 + 
                          Mth_in_comp_binned27_33 + Mth_in_comp_binned62_133 + 
                          No.of.times.30.DPD.or.worse.in.last.6.months1 + No.of.times.30.DPD.or.worse.in.last.6.months2_7 + 
                          No.of.times.30.DPD.or.worse.in.last.12.months3_9 + Avg_Utility_12Mths22_37 + 
                          Avg_Utility_12Mths38_51 + Avg_Utility_12Mths52_71 + Avg_Utility_12Mths72_113 + 
                          No.of.PL.trades.opened.in.last.12.months2 + No.of.PL.trades.opened.in.last.12.months3 + 
                          No.of.PL.trades.opened.in.last.12.months4 + No.of.PL.trades.opened.in.last.12.months5 + 
                          No.of.PL.trades.opened.in.last.12.months6_12 + Noofenq_12Mths_exc_hmat1 + 
                          Noofenq_12Mths_exc_hmat2 + Noofenq_12Mths_exc_hmat3 + Noofenq_12Mths_exc_hmat4 + 
                          Noofenq_12Mths_exc_hmat5 + Noofenq_12Mths_exc_hmat6_8 + Noofenq_12Mths_exc_hmat9_20 + 
                          Outstanding.Balance_binned2962089_3289690, family = "binomial", 
                          data = combined_data_train)

summary(combined_data_22)
vif(combined_data_22)

# removing Mth_in_comp_binned27_33 as it has high p-value
combined_data_23 <- glm(formula = Performance.Tag.y ~ No.of.dependents2 + No.of.dependents4 + 
                          EducationOthers + Mth_in_res_binned6_9 + Mth_in_res_binned73_97 + 
                          Mth_in_comp_binned62_133 + No.of.times.30.DPD.or.worse.in.last.6.months1 + 
                          No.of.times.30.DPD.or.worse.in.last.6.months2_7 + No.of.times.30.DPD.or.worse.in.last.12.months3_9 + 
                          Avg_Utility_12Mths22_37 + Avg_Utility_12Mths38_51 + Avg_Utility_12Mths52_71 + 
                          Avg_Utility_12Mths72_113 + No.of.PL.trades.opened.in.last.12.months2 + 
                          No.of.PL.trades.opened.in.last.12.months3 + No.of.PL.trades.opened.in.last.12.months4 + 
                          No.of.PL.trades.opened.in.last.12.months5 + No.of.PL.trades.opened.in.last.12.months6_12 + 
                          Noofenq_12Mths_exc_hmat1 + Noofenq_12Mths_exc_hmat2 + Noofenq_12Mths_exc_hmat3 + 
                          Noofenq_12Mths_exc_hmat4 + Noofenq_12Mths_exc_hmat5 + Noofenq_12Mths_exc_hmat6_8 + 
                          Noofenq_12Mths_exc_hmat9_20 + Outstanding.Balance_binned2962089_3289690, 
                          family = "binomial", data = combined_data_train)

summary(combined_data_23)
vif(combined_data_23)

# removing EducationOthers as it has high p-value
combined_data_24 <- glm(formula = Performance.Tag.y ~ No.of.dependents2 + No.of.dependents4 + 
                          Mth_in_res_binned6_9 + Mth_in_res_binned73_97 + 
                          Mth_in_comp_binned62_133 + No.of.times.30.DPD.or.worse.in.last.6.months1 + 
                          No.of.times.30.DPD.or.worse.in.last.6.months2_7 + No.of.times.30.DPD.or.worse.in.last.12.months3_9 + 
                          Avg_Utility_12Mths22_37 + Avg_Utility_12Mths38_51 + Avg_Utility_12Mths52_71 + 
                          Avg_Utility_12Mths72_113 + No.of.PL.trades.opened.in.last.12.months2 + 
                          No.of.PL.trades.opened.in.last.12.months3 + No.of.PL.trades.opened.in.last.12.months4 + 
                          No.of.PL.trades.opened.in.last.12.months5 + No.of.PL.trades.opened.in.last.12.months6_12 + 
                          Noofenq_12Mths_exc_hmat1 + Noofenq_12Mths_exc_hmat2 + Noofenq_12Mths_exc_hmat3 + 
                          Noofenq_12Mths_exc_hmat4 + Noofenq_12Mths_exc_hmat5 + Noofenq_12Mths_exc_hmat6_8 + 
                          Noofenq_12Mths_exc_hmat9_20 + Outstanding.Balance_binned2962089_3289690, 
                          family = "binomial", data = combined_data_train)

summary(combined_data_24)
vif(combined_data_24)

# removing No.of.dependents2 as it has high p-value
combined_data_25 <- glm(formula = Performance.Tag.y ~ No.of.dependents4 + 
                          Mth_in_res_binned6_9 + Mth_in_res_binned73_97 + Mth_in_comp_binned62_133 + 
                          No.of.times.30.DPD.or.worse.in.last.6.months1 + No.of.times.30.DPD.or.worse.in.last.6.months2_7 + 
                          No.of.times.30.DPD.or.worse.in.last.12.months3_9 + Avg_Utility_12Mths22_37 + 
                          Avg_Utility_12Mths38_51 + Avg_Utility_12Mths52_71 + Avg_Utility_12Mths72_113 + 
                          No.of.PL.trades.opened.in.last.12.months2 + No.of.PL.trades.opened.in.last.12.months3 + 
                          No.of.PL.trades.opened.in.last.12.months4 + No.of.PL.trades.opened.in.last.12.months5 + 
                          No.of.PL.trades.opened.in.last.12.months6_12 + Noofenq_12Mths_exc_hmat1 + 
                          Noofenq_12Mths_exc_hmat2 + Noofenq_12Mths_exc_hmat3 + Noofenq_12Mths_exc_hmat4 + 
                          Noofenq_12Mths_exc_hmat5 + Noofenq_12Mths_exc_hmat6_8 + Noofenq_12Mths_exc_hmat9_20 + 
                          Outstanding.Balance_binned2962089_3289690, family = "binomial", 
                          data = combined_data_train)

summary(combined_data_25)
vif(combined_data_25)  
  
# removing No.of.dependents4 as it has high p-value
combined_data_26 <- glm(formula = Performance.Tag.y ~ Mth_in_res_binned6_9 + 
                          Mth_in_res_binned73_97 + Mth_in_comp_binned62_133 + No.of.times.30.DPD.or.worse.in.last.6.months1 + 
                          No.of.times.30.DPD.or.worse.in.last.6.months2_7 + No.of.times.30.DPD.or.worse.in.last.12.months3_9 + 
                          Avg_Utility_12Mths22_37 + Avg_Utility_12Mths38_51 + Avg_Utility_12Mths52_71 + 
                          Avg_Utility_12Mths72_113 + No.of.PL.trades.opened.in.last.12.months2 + 
                          No.of.PL.trades.opened.in.last.12.months3 + No.of.PL.trades.opened.in.last.12.months4 + 
                          No.of.PL.trades.opened.in.last.12.months5 + No.of.PL.trades.opened.in.last.12.months6_12 + 
                          Noofenq_12Mths_exc_hmat1 + Noofenq_12Mths_exc_hmat2 + Noofenq_12Mths_exc_hmat3 + 
                          Noofenq_12Mths_exc_hmat4 + Noofenq_12Mths_exc_hmat5 + Noofenq_12Mths_exc_hmat6_8 + 
                          Noofenq_12Mths_exc_hmat9_20 + Outstanding.Balance_binned2962089_3289690, 
                          family = "binomial", data = combined_data_train)

summary(combined_data_26)
vif(combined_data_26)
  
# removing Mth_in_comp_binned62_133 as it has high p-value
combined_data_27 <- glm(formula = Performance.Tag.y ~ Mth_in_res_binned6_9 + Mth_in_res_binned73_97 + 
                          No.of.times.30.DPD.or.worse.in.last.6.months1 + 
                          No.of.times.30.DPD.or.worse.in.last.6.months2_7 + No.of.times.30.DPD.or.worse.in.last.12.months3_9 + 
                          Avg_Utility_12Mths22_37 + Avg_Utility_12Mths38_51 + Avg_Utility_12Mths52_71 + 
                          Avg_Utility_12Mths72_113 + No.of.PL.trades.opened.in.last.12.months2 + 
                          No.of.PL.trades.opened.in.last.12.months3 + No.of.PL.trades.opened.in.last.12.months4 + 
                          No.of.PL.trades.opened.in.last.12.months5 + No.of.PL.trades.opened.in.last.12.months6_12 + 
                          Noofenq_12Mths_exc_hmat1 + Noofenq_12Mths_exc_hmat2 + Noofenq_12Mths_exc_hmat3 + 
                          Noofenq_12Mths_exc_hmat4 + Noofenq_12Mths_exc_hmat5 + Noofenq_12Mths_exc_hmat6_8 + 
                          Noofenq_12Mths_exc_hmat9_20 + Outstanding.Balance_binned2962089_3289690, 
                          family = "binomial", data = combined_data_train)

summary(combined_data_27)
vif(combined_data_27)
  
# removing No.of.times.30.DPD.or.worse.in.last.12.months3_9 as it has high p-value and high VIF
combined_data_28 <- glm(formula = Performance.Tag.y ~ Mth_in_res_binned6_9 + Mth_in_res_binned73_97 + 
                          No.of.times.30.DPD.or.worse.in.last.6.months1 + No.of.times.30.DPD.or.worse.in.last.6.months2_7 + 
                          Avg_Utility_12Mths22_37 + 
                          Avg_Utility_12Mths38_51 + Avg_Utility_12Mths52_71 + Avg_Utility_12Mths72_113 + 
                          No.of.PL.trades.opened.in.last.12.months2 + No.of.PL.trades.opened.in.last.12.months3 + 
                          No.of.PL.trades.opened.in.last.12.months4 + No.of.PL.trades.opened.in.last.12.months5 + 
                          No.of.PL.trades.opened.in.last.12.months6_12 + Noofenq_12Mths_exc_hmat1 + 
                          Noofenq_12Mths_exc_hmat2 + Noofenq_12Mths_exc_hmat3 + Noofenq_12Mths_exc_hmat4 + 
                          Noofenq_12Mths_exc_hmat5 + Noofenq_12Mths_exc_hmat6_8 + Noofenq_12Mths_exc_hmat9_20 + 
                          Outstanding.Balance_binned2962089_3289690, family = "binomial", 
                          data = combined_data_train)
  
summary(combined_data_28)
vif(combined_data_28)

# removing No.of.PL.trades.opened.in.last.12.months2 as it has high p-value and high VIF
combined_data_29 <- glm(formula = Performance.Tag.y ~ Mth_in_res_binned6_9 + Mth_in_res_binned73_97 + 
                          No.of.times.30.DPD.or.worse.in.last.6.months1 + No.of.times.30.DPD.or.worse.in.last.6.months2_7 + 
                          Avg_Utility_12Mths22_37 + Avg_Utility_12Mths38_51 + Avg_Utility_12Mths52_71 + 
                          Avg_Utility_12Mths72_113 + 
                          No.of.PL.trades.opened.in.last.12.months3 + No.of.PL.trades.opened.in.last.12.months4 + 
                          No.of.PL.trades.opened.in.last.12.months5 + No.of.PL.trades.opened.in.last.12.months6_12 + 
                          Noofenq_12Mths_exc_hmat1 + Noofenq_12Mths_exc_hmat2 + Noofenq_12Mths_exc_hmat3 + 
                          Noofenq_12Mths_exc_hmat4 + Noofenq_12Mths_exc_hmat5 + Noofenq_12Mths_exc_hmat6_8 + 
                          Noofenq_12Mths_exc_hmat9_20 + Outstanding.Balance_binned2962089_3289690, 
                          family = "binomial", data = combined_data_train)

summary(combined_data_29)
vif(combined_data_29)

# removing No.of.PL.trades.opened.in.last.12.months3 as it has high p-value and high VIF
combined_data_30 <- glm(formula = Performance.Tag.y ~ Mth_in_res_binned6_9 + Mth_in_res_binned73_97 + 
                          No.of.times.30.DPD.or.worse.in.last.6.months1 + No.of.times.30.DPD.or.worse.in.last.6.months2_7 + 
                          Avg_Utility_12Mths22_37 + Avg_Utility_12Mths38_51 + Avg_Utility_12Mths52_71 + 
                          Avg_Utility_12Mths72_113 + 
                          No.of.PL.trades.opened.in.last.12.months4 + No.of.PL.trades.opened.in.last.12.months5 + 
                          No.of.PL.trades.opened.in.last.12.months6_12 + Noofenq_12Mths_exc_hmat1 + 
                          Noofenq_12Mths_exc_hmat2 + Noofenq_12Mths_exc_hmat3 + Noofenq_12Mths_exc_hmat4 + 
                          Noofenq_12Mths_exc_hmat5 + Noofenq_12Mths_exc_hmat6_8 + Noofenq_12Mths_exc_hmat9_20 + 
                          Outstanding.Balance_binned2962089_3289690, family = "binomial", 
                          data = combined_data_train)

summary(combined_data_30)
vif(combined_data_30)

# removing No.of.PL.trades.opened.in.last.12.months5 as it has high p-value and high VIF
combined_data_31 <- glm(formula = Performance.Tag.y ~ Mth_in_res_binned6_9 + Mth_in_res_binned73_97 + 
                          No.of.times.30.DPD.or.worse.in.last.6.months1 + No.of.times.30.DPD.or.worse.in.last.6.months2_7 + 
                          Avg_Utility_12Mths22_37 + Avg_Utility_12Mths38_51 + Avg_Utility_12Mths52_71 + 
                          Avg_Utility_12Mths72_113 + No.of.PL.trades.opened.in.last.12.months4 + 
                          No.of.PL.trades.opened.in.last.12.months6_12 + 
                          Noofenq_12Mths_exc_hmat1 + Noofenq_12Mths_exc_hmat2 + Noofenq_12Mths_exc_hmat3 + 
                          Noofenq_12Mths_exc_hmat4 + Noofenq_12Mths_exc_hmat5 + Noofenq_12Mths_exc_hmat6_8 + 
                          Noofenq_12Mths_exc_hmat9_20 + Outstanding.Balance_binned2962089_3289690, 
                          family = "binomial", data = combined_data_train)

summary(combined_data_31)
vif(combined_data_31)

# removing No.of.PL.trades.opened.in.last.12.months6_12 as it has high p-value and high VIF
combined_data_32 <- glm(formula = Performance.Tag.y ~ Mth_in_res_binned6_9 + Mth_in_res_binned73_97 + 
                          No.of.times.30.DPD.or.worse.in.last.6.months1 + No.of.times.30.DPD.or.worse.in.last.6.months2_7 + 
                          Avg_Utility_12Mths22_37 + Avg_Utility_12Mths38_51 + Avg_Utility_12Mths52_71 + 
                          Avg_Utility_12Mths72_113 + No.of.PL.trades.opened.in.last.12.months4 + 
                          Noofenq_12Mths_exc_hmat1 + 
                          Noofenq_12Mths_exc_hmat2 + Noofenq_12Mths_exc_hmat3 + Noofenq_12Mths_exc_hmat4 + 
                          Noofenq_12Mths_exc_hmat5 + Noofenq_12Mths_exc_hmat6_8 + Noofenq_12Mths_exc_hmat9_20 + 
                          Outstanding.Balance_binned2962089_3289690, family = "binomial", 
                          data = combined_data_train)

summary(combined_data_32)
vif(combined_data_32)

# removing No.of.PL.trades.opened.in.last.12.months4 as it has high p-value and high VIF
combined_data_33 <- glm(formula = Performance.Tag.y ~ Mth_in_res_binned6_9 + Mth_in_res_binned73_97 + 
                          No.of.times.30.DPD.or.worse.in.last.6.months1 + No.of.times.30.DPD.or.worse.in.last.6.months2_7 + 
                          Avg_Utility_12Mths22_37 + Avg_Utility_12Mths38_51 + Avg_Utility_12Mths52_71 + 
                          Avg_Utility_12Mths72_113 + 
                          Noofenq_12Mths_exc_hmat1 + Noofenq_12Mths_exc_hmat2 + Noofenq_12Mths_exc_hmat3 + 
                          Noofenq_12Mths_exc_hmat4 + Noofenq_12Mths_exc_hmat5 + Noofenq_12Mths_exc_hmat6_8 + 
                          Noofenq_12Mths_exc_hmat9_20 + Outstanding.Balance_binned2962089_3289690, 
                          family = "binomial", data = combined_data_train)

summary(combined_data_33)
vif(combined_data_33)

# We should consider combined_data_33 while taking all the
# variables into consideration. The most important variables are
# Mth_in_res_binned6_9,  Mth_in_res_binned73_97,No.of.times.30.DPD.or.worse.in.last.6.months1,
# No.of.times.30.DPD.or.worse.in.last.6.months2_7,Avg_Utility_12Mths22_37,Avg_Utility_12Mths38_51,
# Avg_Utility_12Mths52_71,Avg_Utility_12Mths72_113,Noofenq_12Mths_exc_hmat1,Noofenq_12Mths_exc_hmat2,
# Noofenq_12Mths_exc_hmat3,Noofenq_12Mths_exc_hmat4,Noofenq_12Mths_exc_hmat5,Noofenq_12Mths_exc_hmat6_8,
# Noofenq_12Mths_exc_hmat9_20,Outstanding.Balance_binned2962089_3289690

#-----------------------------------------------------#
#        MODEL EVALUATION COMBINED DATA 
#-----------------------------------------------------#

#-----------------------------------------------------#
#               LOGISTIC REGRESSION
#-----------------------------------------------------#

# predicting probabilities of Performance TAG for test data
comb_test_pred <- predict(combined_data_33, combined_data_test[,-1],type = "response")
summary(comb_test_pred)

# Adding the predicted values to the test dataframe
combined_data_test$prob <- comb_test_pred

# sorting the predict column of test dataframe in descending order
combined_data_test <- combined_data_test[rev(order(combined_data_test$prob)),]

# probability greater than .5 is 1 (customer will default)
comb_test_pred_ptag_50 <- factor(ifelse(comb_test_pred >= 0.50, 1,0))

# confusion matrix
comb_test_conf <- confusionMatrix(comb_test_pred_ptag_50, combined_data_test$Performance.Tag.y, positive = "1")
comb_test_conf

#Sensitivity : 0.0000         
#Specificity : 1.0000      
#Accuracy : 0.9572

# We see that Sensitivity is zero and Specificity is 1 when cutoff is 0.50,
# hence we need to create a function to understand the optimal probability cutoff

#-----------------------------------------------------#

# compute optimal probalility cutoff for better model reliability
perform_fn_comb <- function(cutoff) 
{
  comb_test_pred_ptag_50 <- factor(ifelse(comb_test_pred >= cutoff, 1,0))
  comb_test_conf <- confusionMatrix(comb_test_pred_ptag_50, combined_data_test$Performance.Tag.y, positive = "1")
  acc <- comb_test_conf$overall[1]
  sens <- comb_test_conf$byClass[1]
  spec <- comb_test_conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values from 0.01 to 0.99 for plotting and initiallizing a matrix of 1000 X 4.
comb_s = seq(.01,.99,length=100)
OUT = matrix(0,100,3)

for(i in 1:100)
{
  OUT[i,] = perform_fn_comb(comb_s[i])
}

#-----------------------------------------------------#

# plotting sensitivity, specificity and accuracy with different values of probability
plot(comb_s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(comb_s,OUT[,2],col="darkgreen",lwd=2)
lines(comb_s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

# finding cutoff probability for threshold value above which represents that customer will default
cutoff <- comb_s[which(abs(OUT[,1]-OUT[,2])<0.01)]

# probability greater than .04 is 1 (customer will default)
comb_test_pred_ptag <- factor(ifelse(comb_test_pred >= 0.0429, 1,0))

# confusion matrix with cutoff value equal 0.0429
comb_conf_final <- confusionMatrix(comb_test_pred_ptag, combined_data_test$Performance.Tag.y, positive = "1")
comb_conf_final

#Sensitivity : 0.65873         
#Specificity : 0.60061      
#Accuracy : 0.6031

# These results clearly look better now, Sensitivity is at 65%.
# So using the glm model and taking up all the variables we see
# the results look more promising.

#-----------------------------------------------------#
#        MODEL VALIDATION LOGISTIC REGRESSION
#-----------------------------------------------------#

# Storing the actual predicted value from original test data
actual_response <- as.data.frame(combined_data_test$Performance.Tag.y)
summary(actual_response)

# 0:19725
# 1:  882

# Default percentage
(882/(19725+882))*100 # 4.28%

# Storing the predicted value from combined data 33 variables
predicted_response <- as.data.frame(comb_test_pred_ptag)
summary(predicted_response)

# 0:12148
# 1: 8459

# Default percentage
(8459/(12148+8459))*100 # 41.04%

#-----------------------------------------------------#
#                      ROC CURVE
#-----------------------------------------------------#

# Making the predicted value into numeric
test_pred_numeric <- as.numeric(comb_test_pred_ptag)

# Taking both precicted and actual into one 
pred_object_test<- prediction(test_pred_numeric, combined_data_test$Performance.Tag.y)

# Checking the performance here, we are using true positive and false positive rate parameters
performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

#"x.values":
#0.0000000 0.4112548 1.0000000

#"y.values":
#0.000000 0.393424 1.000000

# Checking the difference
ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

# 0.00000000 -0.01783072  0.00000000

max(ks_table_test) #0
min(ks_table_test) #-0.0178

# Plotting the ROC curve
plot(performance_measures_test)
abline(0,1,col="grey")


#-----------------------------------------------------#
#-----------------------------------------------------#
#-----------------------------------------------------#


#-----------------------------------------------------#
#               DECISION TREE (Model-5)
#-----------------------------------------------------#

#-----------------------------------------------------------------#
# Dividing combined dataset into, train and test datasets for decision tree
#-----------------------------------------------------------------#

# Creating a dataframe for decision tree from original values
tree_data <- original_data[,-1]

#Setting the seed to 50# 
set.seed(50)

#Now we randomly generate row indices for train dataset
splitindices_dt <- sample(nrow(tree_data), 0.7*nrow(tree_data))

#Generating the train data set
dt_train <- tree_data[splitindices_dt,]

#Storing the rest of the observations into an object "dt_test".
dt_test <- tree_data[-splitindices_dt,]

#-----------------------------------------------------#

# Building the tree with default hyperparameters
dt_train <- data.frame(lapply(dt_train, as.factor))
tree_model_1 <- rpart(Performance.Tag.y ~ ., data = tree_data, method = "class")

# Making predictions on the test  dataset
tree_predict <- predict(tree_model_1, dt_test, type = "class")

# Evaluating the results using confusion matrix
confusionMatrix(dt_test$Performance.Tag.y, tree_predict, positive = "1")

# We see that the accuray is around 95.78% when we are using the
# default hyperparameters, the default settings are gini index,
# minsplit of 20 and minbucket of 7

#-----------------------------------------------------#

# Now lets change the algorithm to "information gain" instead of default "gini"
tree.model_2 <- rpart(Performance.Tag.y ~ ., data = tree_data, method = "class",
                    parms = list(split = "information"))

# Making predictions on the test  dataset
tree.predict <- predict(tree.model_2, dt_test, type = "class")

# Evaluating the results using confusion matrix
confusionMatrix(dt_test$Performance.Tag.y, tree_predict, positive = "1")

# We see that the accuray is around 95.78% when we are using
# "information gain" instead of default "gini", no change happened to accuracy

#-----------------------------------------------------#

# Lets now add arbitary minsplit and cp and check the output.
tree.model_3 <-  rpart(Performance.Tag.y ~ ., data = tree_data, method= "class", 
                       control=rpart.control(minsplit=1000, minbucket = 1000, cp=0.025))

# Making predictions on the test  dataset
tree.predict <- predict(tree.model_3, dt_test, type = "class")

# Evaluating the results using confusion matrix
confusionMatrix(dt_test$Performance.Tag.y, tree_predict, positive = "1")

# We see that the accuray is around 95.78% when we are defining control

#-----------------------------------------------------#
#                RANDOM FOREST (Model-6)
#-----------------------------------------------------#

# Creating a dataframe for random forest from original values
random_data <- original_data[,-1]

# Creating a vector to store all the numeric values
numericcols <- c('No.of.times.90.DPD.or.worse.in.last.6.months','No.of.times.60.DPD.or.worse.in.last.6.months',
                 'No.of.times.30.DPD.or.worse.in.last.6.months','No.of.times.90.DPD.or.worse.in.last.12.months',
                 'No.of.times.60.DPD.or.worse.in.last.12.months','No.of.times.30.DPD.or.worse.in.last.12.months',
                 'Avgas.CC.Utilization.in.last.12.months','No.of.trades.opened.in.last.6.months',
                 'No.of.trades.opened.in.last.12.months','No.of.PL.trades.opened.in.last.6.months',
                 'No.of.PL.trades.opened.in.last.12.months','No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.',
                 'No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.','Presence.of.open.home.loan',
                 'Outstanding.Balance','Total.No.of.Trades','Presence.of.open.auto.loan','Age','No.of.dependents',
                 'Income','No.of.months.in.current.residence','No.of.months.in.current.company')

# Creating a vector to store all the factor values
factorcols <- c('Gender','Marital.Status..at.the.time.of.application.','Education',
                'Profession','Type.of.residence','Performance.Tag.y')

# Making all the variables with numbers to numeric
random_data[, numericcols] <- lapply(numericcols, function(x) as.numeric(as.character(random_data[, x])))

# Making all the variables with character to factor
random_data[, factorcols] <- lapply(factorcols, function(x) as.factor(as.character(random_data[, x])))

# Shuffling the data to remove bias
shuffledata <- random_data[sample(nrow(random_data)), ]

# Setting the seed to 50
set.seed(50)

# Now we randomly generate row indices for train dataset
ntrain_rf <- as.integer(nrow(shuffledata)*0.8)

# Generating the train data set
traindata_rf <- shuffledata[1:ntrain_rf, ]

# Generating the test data set
testdata_rf <- shuffledata[(ntrain_rf+1):nrow(shuffledata), ]

# Building the random forest model
# Hyperparameters considered are 
# na.action, this is used to omit na's from the dataset while calculating
# do.trace, this is used to see while the algorithm is running and creating trees
# ntree, this is for the number of trees to be produced
# mtry, the number of attributes it will try on before making a split
# proximity, the closeness between datapoints is not considered, hence it is false 
model_rf <- randomForest(Performance.Tag.y ~ ., data=traindata_rf, proximity=FALSE,
                        ntree=1000, mtry=5, do.trace=TRUE, na.action=na.omit)

# Checking the model
model_rf
# OOB error is at 4.23%, it is that error which each tree has committed on the
# data that this tree hasn't seen. This is aggregated across all trees.

# Confusion matrix:
#    0     1          class.error
#0   52749 0           0
#1    2331 0           1

# We do not need a seperate test data to predict the accuracy of a random forest,
# this is because every tree that random forest builds leaves out few datapoints
# that it has not seen. But as we have already created a test set we will use it here
testPred_rf <- predict(model_rf, newdata=testdata_rf)


table(testPred_rf, testdata_rf$Performance.Tag.y)
#testPred_rf     0     1
#0            13200   570
#1               0     0

#-----------------------------------------------------#
#               Score card building 
#-----------------------------------------------------#

library(scorecard)
dt_sel = var_filter(combined_data, "Performance.Tag.y")
bins = woebin(dt_sel, "Performance.Tag.y")
dt_woe = woebin_ply(dt_sel, bins)
card<-scorecard(bins,combined_data_33, points0 = 400, odds0 = 1/9, pdo = 20,basepoints_eq0 = FALSE)
#create a scorecard
score1 <- scorecard_ply(combined_data, card)
#1:   468
#2:   445
#3:   461
#4:   476
#5:   476
#---      
#68684:   476
#68685:   476
#68686:   474
#68687:   476
#68688:   476
#credit score for both total and each variable
combined_data$application_score<-scorecard_ply(combined_data, card, only_total_score = F)
head(combined_data$application_score)

#-----------------------------------------------------#
#                     END OF CODE
#-----------------------------------------------------#
