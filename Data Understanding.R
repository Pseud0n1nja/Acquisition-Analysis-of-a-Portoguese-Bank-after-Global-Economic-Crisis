setwd("/Users/Arpit/Documents/R_Playground/BFSI/Acquisition Analytics/Acquisition_Labs")
##------------Bank Marketing Analysis---------------------##

#----------------------------------------------------------
# The standard process followed in analytics projects is:
# 1. Business Understanding
# 2. Data Understanding  
# 3. Data Preparation
# 4. Modelling
# 5. Model Evaluation
# 6. Model Deployment and Recommendations

#-------------------------------------------------------
## Business Understanding:- Prospect Profiling
#-------------------------------------------------------

# Loading bank marketing data in the working directory. 

bank_data<- read.csv("bank_marketing.csv")

# Checking structure of dataset 

str(bank_data)

# Summary of dataset gives a nominal  Statistical perpective

summary(bank_data)

#-------------------------------------------------------

# Checking response rate of prospect customer
response_rate <- length(which(bank_data$response == "yes"))/(length(which(bank_data$response == "yes")) 
                                                             + length(which(bank_data$response == "no")))
response <- 4640/(36548+4640)
response

# Checking missing values inside the dataset

sum(is.na(bank_data))

#-------------------------------------------------------

# Loading ggplot2 library
library(ggplot2)

# Plotting Age histogram

ggplot(bank_data,aes(age))+geom_histogram()

# Let's check the outlier in the variables

quantile(bank_data$age,seq(0,1,0.01))

# Box plot 

boxplot(bank_data$age)

# Capping the upper values of age with 71.

bank_data[(which(bank_data$age>71)),]$age <- 71

# for lower capping bank_data[(which(bank_data$age<23)),]$age <- 23
 

# Binning the age variable and store it into "binning.age".

bank_data$binning.age <- as.factor(cut(bank_data$age, breaks = c(16, 20, 30, 40, 50, 60, 70, 80)))

# Change the response value to numbers i.e"yes-no" to "1-0"

bank_data$response <- ifelse(bank_data$response == "yes", 1, 0)

# Check the numeric value of response rate in each bucket

agg_age <- merge(aggregate(response ~ binning.age, bank_data, mean),
                 aggregate(response~binning.age, bank_data, sum),by = "binning.age") 

# Adding No.of_prospect
count <- data.frame(table(bank_data$binning.age))
count <- count[,-1]
agg_age <- cbind(agg_age,count)


# changing column name of each variables in agg_age dataframe

colnames(agg_age) <- c("age", "response_rate", "count_prospects","No.of_prospect")

# Round Off the values

agg_age$response_rate <- format(round(agg_age$response_rate, 2))

agg_age

#-------------------------------------------------------

# Let's see the response rate of each age bucket in the plot

ggplot(agg_age, aes(age, No.of_prospect,label = response_rate)) + 
  geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5)

#bargraph corresponding to the age_bin vs response_rate
#ggplot(agg_age, aes(age, response_rate)) + geom_bar(stat = 'identity')

# Let's check the dataset of age less than equal to 20 years. 
Bank_data_age20 <- subset(bank_data,age <=20)

View(Bank_data_age20)
summary(Bank_data_age20)

##--------------------------------------------------------  

# Checking structure of dataset

str(bank_data)

#-----Next Variable is "job"

# Checking the levels of the job

levels(bank_data$job)


# Plotting bar graph for job variable.

# Writing a function "plot_response" to do the same task for each variable

plot_response <- function(cat_var, var_name){
  a <- aggregate(response~cat_var, bank_data, mean)
  count <- data.frame(table(cat_var))
  count <- count[,-1]
  agg_response <- cbind(a, count)
  
  colnames(agg_response) <- c(var_name, "response_rate","No.of_Prospect")
  agg_response[, 2] <- format(round(agg_response[, 2], 2))
  
  ggplot(agg_response, aes(agg_response[, 1], count, label = response_rate)) + 
    geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
    geom_text(size = 3, vjust = -0.5) + xlab(var_name)
  
}

plot_response(bank_data$job, "job")

##--------------------------------------------------------  

# Checking structure of dataset 

str(bank_data)

# Checking Marital status

summary(bank_data$marital)

# Let's replace Unknown level to married

levels(bank_data$marital)[4] <- "married"

# Plotting marital status

plot_response(bank_data$marital,"marital")

# Let's see the education variables

plot_response(bank_data$education,"Education")



# Reducing the levels of education variable

levels(bank_data$education)[c(1:3,5)] <- "Primary_Education"
levels(bank_data$education)[2] <- "Secondary_Education"
levels(bank_data$education)[4]<- "Tertiary_Education"

# Let's again check the education plot

plot_response(bank_data$education,"Education_levels")


#-------------------------------------------------------
# Let's see the default variable

table(bank_data$default)

plot_response(bank_data$default, "Default")
bank_data <- bank_data[,-5]

#-------------------------------------------------------

# Let's understand the housing variables 

summary(bank_data$housing)


plot_response(bank_data$housing, "Housing")

#-------------------------------------------------------

#-- Let's see the next variable which is "loan"

summary(bank_data$loan)

plot_response(bank_data$loan, "Loan Status")
#-------------------------------------------------------

#------------------------Data Understanding-Campaign Information------------------------------------------------------------#
#  Next variable is Contact, Let's see the response rate of each mode 

summary(bank_data$contact)
plot_response(bank_data$contact,"Contact_mode")

#-------------------------------------------------------

# Next variable is "Month" i.e contact month. 

plot_response(bank_data$month,"Contact_month")

#-------------------------------------------------------

# Let's do the same of "day_of_week" variable

plot_response(bank_data$day_of_week,"day_of_week")

#-------------------------------------------------------

# Now, Let's see the "duration" variable: Which is Quantitative variable

# Let's check the histogram 

ggplot(bank_data,aes(duration))+geom_histogram()

# Let's see the summary of this variable once 

summary(bank_data$duration)

# Average duration 
bank_data$response_1 <- as.factor(bank_data$response)
Avg_duration <- aggregate(duration~response_1,bank_data,mean)

#Removing the column#22 from the data frame
bank_data <- bank_data[,-22]

## Definitely the outlier is present in the dataset

# So let's check the percentile distribution of duration 

quantile(bank_data$duration,seq(0,1,0.01))


# So, capping the duration seconds at 99% which is 1271.3sec 

bank_data[(which(bank_data$duration>1271.13)),]$duration <- 1271.13

# Now, again plot the histogram 

ggplot(bank_data,aes(duration))+geom_histogram()

#-------------------------------------------------------

# the next variable is "campaign" variable
#(number of contacts performed during this campaign and for this client 
# numeric, includes last contact)

# So let's check the summay of this variable 

summary(bank_data$campaign)

# Let's see the percentile distribution of this variable

boxplot(bank_data$campaign)


quantile(bank_data$campaign,seq(0,1,0.01))

# Capping this at 99% which the value is 14

bank_data[which(bank_data$campaign>14),]$campaign <- 14

# Visualizing it with plot

ggplot(bank_data,aes(campaign))+geom_histogram()

#-------------------------------------------------------
#-- Next variable is "pdays"
# Let's first convert this variable to factor type

bank_data$pdays<- as.factor(bank_data$pdays)

# Checking summary

summary(bank_data$pdays)

levels(bank_data$pdays)

# Reducing the levels of this variable to 3.

levels(bank_data$pdays)[1:10] <- "Contacted_in_first_10days"
levels(bank_data$pdays)[2:17] <-"Contacted_after_10days"
levels(bank_data$pdays)[3] <- "First_time_contacted"


# Also,lets see the respose rate of each levels. 

plot_response(bank_data$pday,"Pday")

# Number of prospects under each category

table(bank_data$pdays)

#-------------------------------------------------------

# Next variable is "previous" i.e number of contacts performed before 
# this campaign and for this client (numeric)

summary(bank_data$previous)
# Max=7, best is to convert this variable to factor

bank_data$previous <- as.factor(bank_data$previous)

levels(bank_data$previous)[1]<-"Never contacted"
levels(bank_data$previous)[2:4] <- "Less_than_3_times"
levels(bank_data$previous)[3:6] <- "More than_3_times"


summary(bank_data$previous)


plot_response(bank_data$previous,"Previous_contacts")


# Now, the next variable is "Poutcome" i.e  outcome of the previous marketing campaign 
# (categorical: 'failure','nonexistent','success')

summary(bank_data$poutcome)

plot_response(bank_data$poutcome,"Outcome_of_Previous_contacts")

#-------------------------------------------------------
#------------------------Data Understanding-Economic Information------------------------------------------------------------#

#-- social and economic context attributes

# emp.var.rate- :employment variation rate - quarterly indicator (numeric)
summary(bank_data$emp.var.rate)

# Histogram of employment variation rate variable
ggplot(bank_data,aes(emp.var.rate))+geom_histogram()

# cons.price.idx:consumer price index - monthly indicator (numeric) 
summary(bank_data$cons.price.idx)

# Histogram of consumer price index variable
ggplot(bank_data,aes(cons.price.idx))+geom_histogram()

# cons.conf.idx: consumer confidence index - monthly indicator (numeric) 
summary(bank_data$cons.conf.idx)

# euribor3m: euribor 3 month rate - daily indicator (numeric)
summary(bank_data$euribor3m)

# nr.employed: number of employees - quarterly indicator (numeric)
summary(bank_data$nr.employed)

#----------------------------------------------------------------------------

bank_data_DT <- bank_data
bank_data_RF <- bank_data

#---------------------------------------------------------------------------
