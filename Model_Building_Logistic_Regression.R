setwd("/Users/Arpit/Documents/R_Playground/BFSI/Acquisition Analytics/Acquisition_Labs")
## Model Building   

##---------Logistic Regression----------#
#scaling/normailsation(0,1) + dummy variable creation (categorical -> numerical)----- PRE-PROCESSING -------------------

# Required Packages

library(caret)
library(caTools)
library(dummies)

#---------------------------------------------------------    

# Removing binning variables 

bank_data <- bank_data[, -21]


#creating dummy variables

bank_data$response <- as.integer(bank_data$response)

k1 <- bank_data

bank_data <- dummy.data.frame(bank_data)

bank_data$response <- as.factor(ifelse(bank_data$response == 1, "yes", "no"))

#---------------------------------------------------------    

# splitting into train and test data
#for reproducibility
set.seed(1)

split_indices <- sample.split(bank_data$response, SplitRatio = 0.70)

train <- bank_data[split_indices, ]

test <- bank_data[!split_indices, ]

nrow(train)/nrow(bank_data)

nrow(test)/nrow(bank_data)

#---------------------------------------------------------    

### Model 1: Logistic Regression


library(MASS)
#install.packages("MatrixModels")
library(car)

logistic_1 <- glm(response ~ ., family = "binomial", data = train)

summary(logistic_1)

#---------------------------------------------------------    

# Using stepwise algorithm for removing insignificant variables 

# logistic_2 <- stepAIC(logistic_1, direction = "both")

# stepAIC has removed some variables and only the following ones remain

logistic_2 <- glm(formula = response ~ jobadmin. + jobretired + jobstudent + 
                    jobtechnician + jobunemployed + educationprofessional.course + 
                    educationTertiary_Education + contactcellular + monthaug + 
                    monthjun + monthmar + monthmay + monthnov + day_of_weekfri + 
                    day_of_weekmon + day_of_weekthu + duration + campaign + pdaysContacted_in_first_10days + 
                    pdaysContacted_after_10days + previousLess_than_3_times + 
                    poutcomefailure + emp.var.rate + cons.price.idx + cons.conf.idx + 
                    euribor3m, family = "binomial", data = train)



# checking vif for logistic_2 

vif(logistic_2)

summary(logistic_2)

#---------------------------------------------------------    

# removing "previousLess_than_3_times"since vif is high and also the variable is not significant 

logistic_3 <- glm(formula = response ~ jobadmin. + jobretired + jobstudent + 
                    jobtechnician + jobunemployed + educationprofessional.course + 
                    educationTertiary_Education + contactcellular + monthaug + 
                    monthjun + monthmar + monthmay + monthnov + day_of_weekfri + 
                    day_of_weekmon + day_of_weekthu + duration + campaign + pdaysContacted_in_first_10days + 
                    pdaysContacted_after_10days +poutcomefailure + emp.var.rate +
                    cons.price.idx + cons.conf.idx +euribor3m, family = "binomial", data = train)

summary(logistic_3)

vif(logistic_3)


# Removing "emp.var.rate" variable.

logistic_4 <- glm(formula = response ~ jobadmin. + jobretired + jobstudent + 
                    jobtechnician + jobunemployed + educationprofessional.course + 
                    educationTertiary_Education + contactcellular + monthaug + 
                    monthjun + monthmar + monthmay + monthnov + day_of_weekfri + 
                    day_of_weekmon + day_of_weekthu + duration + campaign + pdaysContacted_in_first_10days + 
                    pdaysContacted_after_10days +poutcomefailure +
                    cons.price.idx + cons.conf.idx +euribor3m, family = "binomial", data = train)


summary(logistic_4)

vif(logistic_4)

# Removing "monthaug"

logistic_5 <- glm(formula = response ~ jobadmin. + jobretired + jobstudent + 
                    jobtechnician + jobunemployed + educationprofessional.course + 
                    educationTertiary_Education + contactcellular + 
                    monthjun + monthmar + monthmay + monthnov + day_of_weekfri + 
                    day_of_weekmon + day_of_weekthu + duration + campaign + pdaysContacted_in_first_10days + 
                    pdaysContacted_after_10days +poutcomefailure +
                    cons.price.idx + cons.conf.idx +euribor3m, family = "binomial", data = train)

summary(logistic_5)



# Removing "monthnov"

logistic_6 <- glm(formula = response ~ jobadmin. + jobretired + jobstudent + 
                    jobtechnician + jobunemployed + educationprofessional.course + 
                    educationTertiary_Education + contactcellular + 
                    monthjun + monthmar + monthmay + day_of_weekfri + 
                    day_of_weekmon + day_of_weekthu + duration + campaign + pdaysContacted_in_first_10days + 
                    pdaysContacted_after_10days +poutcomefailure +
                    cons.price.idx + cons.conf.idx +euribor3m, family = "binomial", data = train)



summary(logistic_6)



# Removing "jobunemployed"


# Removing ""
logistic_7 <- glm(formula = response ~ jobadmin. + jobretired + jobstudent + 
                    jobtechnician + educationprofessional.course + 
                    educationTertiary_Education + contactcellular + 
                    monthjun + monthmar + monthmay + day_of_weekfri + 
                    day_of_weekmon + day_of_weekthu + duration + campaign + pdaysContacted_in_first_10days + 
                    pdaysContacted_after_10days +poutcomefailure +
                    cons.price.idx + cons.conf.idx +euribor3m, family = "binomial", data = train)

summary(logistic_7)


# Removing "educationprofessional.course"
logistic_8 <- glm(formula = response ~ jobadmin. + jobretired + jobstudent + 
                    jobtechnician + educationTertiary_Education + contactcellular + 
                    monthjun + monthmar + monthmay + day_of_weekfri + day_of_weekmon +  
                    day_of_weekthu + duration + campaign + pdaysContacted_in_first_10days + 
                    pdaysContacted_after_10days +poutcomefailure + cons.price.idx + 
                    cons.conf.idx +euribor3m, family = "binomial", data = train)

summary(logistic_8)


# Removing "day_of_weekthu"

logistic_9 <- glm(formula = response ~ jobadmin. + jobretired + jobstudent + 
                    jobtechnician + educationTertiary_Education + contactcellular + 
                    monthjun + monthmar + monthmay + day_of_weekfri + day_of_weekmon +  
                    duration + campaign + pdaysContacted_in_first_10days + 
                    pdaysContacted_after_10days +poutcomefailure + cons.price.idx + 
                    cons.conf.idx +euribor3m, family = "binomial", data = train)

summary(logistic_9)



# Removing "day_of_weekfri"

logistic_10 <- glm(formula = response ~ jobadmin. + jobretired + jobstudent + 
                     jobtechnician + educationTertiary_Education + contactcellular + 
                     monthjun + monthmar + monthmay + day_of_weekmon +  
                     duration + campaign + pdaysContacted_in_first_10days + 
                     pdaysContacted_after_10days +poutcomefailure + cons.price.idx + 
                     cons.conf.idx +euribor3m, family = "binomial", data = train)


summary(logistic_10)


# Removing "jobadmin." 

logistic_11 <- glm(formula = response ~ jobretired + jobstudent + 
                     jobtechnician + educationTertiary_Education + contactcellular + 
                     monthjun + monthmar + monthmay + day_of_weekmon +  
                     duration + campaign + pdaysContacted_in_first_10days + 
                     pdaysContacted_after_10days +poutcomefailure + cons.price.idx + 
                     cons.conf.idx +euribor3m, family = "binomial", data = train)

summary(logistic_11)

# Removing "jobtechnician"

logistic_12 <- glm(formula = response ~ jobretired + jobstudent + 
                     educationTertiary_Education + contactcellular + 
                     monthjun + monthmar + monthmay + day_of_weekmon +  
                     duration + campaign + pdaysContacted_in_first_10days + 
                     pdaysContacted_after_10days +poutcomefailure + cons.price.idx + 
                     cons.conf.idx +euribor3m, family = "binomial", data = train)

summary(logistic_12)

# Removing "monthjun"

logistic_13 <- glm(formula = response ~ jobretired + jobstudent + 
                     educationTertiary_Education + contactcellular + 
                     monthmar + monthmay + day_of_weekmon + duration +  
                     campaign + pdaysContacted_in_first_10days + 
                     pdaysContacted_after_10days +poutcomefailure + cons.price.idx + 
                     cons.conf.idx +euribor3m, family = "binomial", data = train)
summary(logistic_13)

logistic_final <- logistic_13
#---------------------------------------------------------    

# Predicting probabilities of responding for the test data

predictions_logit <- predict(logistic_final, newdata = test, type = "response")
summary(predictions_logit)

#--------------------------------------------------------- 


## Model Evaluation: Logistic Regression

# Let's use the probability cutoff of 50%.

predicted_response <- factor(ifelse(predictions_logit >= 0.50, "yes", "no"))

# Creating confusion matrix for identifying the model evaluation.

conf <- confusionMatrix(predicted_response, test$response, positive = "yes")

conf

#---------------------------------------------------------    

# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_response <- factor(ifelse(predictions_logit >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_response, test$response, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

#---------------------------------------------------------    

# Creating cutoff values from 0.01 to 0.99 for plotting and initiallizing a matrix of 1000 X 4.

s = seq(.01,.99,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

#---------------------------------------------------------    

# plotting cutoffs 
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


#---------------------------------------------------------    

cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]


# Let's choose a cutoff value of 12% for final model

predicted_response <- factor(ifelse(predictions_logit >= 0.128, "yes", "no"))

conf_final <- confusionMatrix(predicted_response, test$response, positive = "yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc

sens

spec
