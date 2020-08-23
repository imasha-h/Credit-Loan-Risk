install.packages("tidyverse")
install.packages("dplyr")
install.packages("gmodels")
install.packages("plyr")
install.packages("partykit")
library(plyr)
library(tidyverse)
library(dplyr)
library(gmodels)
#library(gtools)
library(e1071)
#library(MASS)
library(caret)
library(Hmisc)
library(partykit)
names(loans)

dic <- read.csv("data-dictionary.csv")
loans <- read.csv("loans.csv")
CrossTable(loans$loan_status)

#Changing loan status to binary variable
status <- revalue(loans$loan_status, c("Default"="0", "Charged Off"="0", "Fully Paid"= "1", "Late (> 90 days)" ="0", "Ongoing"="1"))
loans <- mutate(loans, loan_status = status)
loans$loan_status<- as.numeric(loans$loan_status)
####################################################################################################################
#change all missing to na
summary(loans)
loans <- mutate_all(loans, list(~na_if(.,"")))


############################################################################################
#credit score grade

sum(is.na(loans$credit_score)) #0 missing

hist(loans$credit_score, main = "Histogram of Credit Score", xlab = "Credit Score")
plot(loans$credit_score, ylab = "Credit Score") # outliers present 

index_hiscore <- which(loans$credit_score>1100)
index_lowscore <- which(loans$credit_score< 0)
# Delete credit score outliers
loans <- loans[-index_lowscore, ]
loans <- loans[-index_hiscore, ]

hist(loans$credit_score, main = "Histogram of Credit Score", xlab = "Credit Score")
plot(loans$credit_score, ylab = "Credit Score")

loans <-loans %>% 
  mutate (grade = case_when (
    credit_score <= 560 ~ "Very Poor",
    credit_score <= 720 ~ "Poor",
    credit_score <= 880 ~ "Fair",
    credit_score <= 960 ~ "Good",
    credit_score <= 999 ~ "Excellent",
    credit_score >999 ~ "Incredibly Good"
  )) 
loans$grade <- as.factor(loans$grade)
counts<-table(loans$grade)
barplot(counts, ylab="Frequency",
        xlab="Credit Score Grade")

#change missing data of last record months, change NA to No Records###############################################
############################################removed where last record month = 0 and where public record = 0

#loans$last_record_months[is.na(loans$last_record_months)] <- "No Records"
#want to delete where public record == 0 and where last record month ==0 - only 3 rows REMOVE 3 rows 
#index_lastrecordmonth <- which(loans$public_records ==0 & loans$last_record_months ==0)
#loans <- loans[-index_lastrecordmonth, ]
#view(unique(loans$last_record_months))

#change public records to binary########################################################################
loans<- loans %>%mutate(public_records_bi = case_when(
  public_records == 0 ~ 0,
  public_records != 0 ~ 1
)) 


############################################################################################################
#change missing data of last_derog_months	Months since most recent 90-day or worse rating########
#change NA to No defaults

plot(loans$last_derog_months, ylab = "Months since most recent 90-day or worse rating.") # outliers present 

####class as last derog year#########################################################
###decided to remove data after 84 months - maybe keep?

loans<- loans %>%
  mutate(last_derog_months_cat = case_when(
    last_derog_months <= 12 ~ "1",
    last_derog_months <= 24 ~ "2",
    last_derog_months <= 36 ~ "3",
    last_derog_months <= 48 ~ "4",
    last_derog_months <= 60 ~ "5",
    last_derog_months <= 72 ~ "6",
    last_derog_months <= 84 ~ "7",
    last_derog_months >85 ~ ">7",
  )) 


#loans$last_derog_months[is.na(loans$last_derog_months)] <- "No defaults"

view(unique(loans$last_derog_months))

loans$last_record_months <- as.numeric(loans$last_record_months)

############################################################################################################

#catoragorise interest rate
loans <-loans %>% 
  mutate (interest_range = case_when (
    interest_rate <= 7 ~ "0-7",
    interest_rate <= 10 ~ "7-10",
    interest_rate <= 13 ~ "10-13",
    interest_rate <= 16 ~ "13-16",
    interest_rate <= 19 ~ "16-20",
    interest_rate <= 25 ~ "20-25",
    interest_rate >25 ~ "25+"
  ))

loans$interest_range <- as.factor(loans$interest_range)
plot(loans$interest_range, ylab = "Freq", xlab = "Interest Range")


##############################################################################################################
#checking missing values in employment length
#Note: In Data Analyses, this is called MNAR ( = Missing Not At Random), 
#which means the value of the variable that is missing may be related to the reason it is missing.

sum(is.na(loans$employment_length)) #4% missing
#############################################################################################
##################################################################################

#We create a new column in our database called “int_bin” (“interest-bin”).
#This column is filled with nothing but NA´s and has the same length like all the other columns.
#this is likely unnecessary
loans$emp_bin <- rep(NA, length(loans$employment_length))

loans$emp_bin <- as.character(loans$emp_bin)

loans$emp_bin[which(loans$employment_length == "< 1 year")] <- "<1"
loans$emp_bin[which(loans$employment_length == "1 year")] <- "1"
loans$emp_bin[which(loans$employment_length == "2 years")] <- "2"
loans$emp_bin[which(loans$employment_length == "3 years")] <- "3"
loans$emp_bin[which(loans$employment_length == "4 years")] <- "4"
loans$emp_bin[which(loans$employment_length == "5 years")] <- "5"
loans$emp_bin[which(loans$employment_length == "6 years")] <- "6"
loans$emp_bin[which(loans$employment_length == "7 years")] <- "7"
loans$emp_bin[which(loans$employment_length == "8 years")] <- "8"
loans$emp_bin[which(loans$employment_length == "9 years")] <- "9"
loans$emp_bin[which(loans$employment_length == "10+ years")] <- "10+"

loans$emp_bin[which(is.na(loans$employment_length))] <- "Missing"
loans$emp_bin <- as.factor(loans$emp_bin)
#loans$employment_length <- as.factor(loans$employment_length)
loans$employment_length <- as.numeric(loans$employment_length)

plot(loans$employment_length, xlab="Employment Length", ylab = "Frequency")
plot(loans$emp_bin, xlab="Employment Length", ylab = "Frequency") #plot with missing



#####################################################################################################
#CREDIT CARD BALANCE
#Removing outliers

sum(is.na(loans1$credit_card_balance)) #0% missing
#credit credit balance outliers##############################################################################
plot(loans$credit_card_balance, ylab = "Credit Card Balance") #one outlier present #maybe linked with annual income?
#chck1<- loans %>% select(annual_income, credit_card_balance, loan_status) %>% filter(credit_card_balance>=1000000) #%>% unique()
# Save the Position of all cc balance above 1mil
index_highbal <- which(loans$credit_card_balance > 500000)
# Delete every Row with Ages above 1mil
loans <- loans[-index_highbal, ]


#ANNUAL INCOME####does not look right##############################################################
loans$annual_income <- as.numeric(loans$annual_income)
sum(is.na(loans$annual_income)) #0% missing
plot(loans$annual_income, ylab = "Annual Income")
#look<-loans %>% select(annual_income) %>% filter(annual_income<=50)

#############################################################################################################
# Creating a variable that contains our CrossTable shown above
data <-CrossTable(loans$grade, loans$loan_status, prop.r = TRUE, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)
# Creating a variable that contains only the default probability
data_relationship <- data$prop.row[,2]  
# Plotting it
position <- data_relationship / 2
text(x = barplot(data_relationship),labels=names(data_relationship), y = position)
title("The worse the grade, the higher the default probability")

difference <- diff(data_relationship)
plot(difference, type = "b", xlab = "Grades", ylab = "Changes in Default Probability",xaxt="n") 
axis(1, at=1:6, labels=names(difference))
title("Probability of Default are changing dramatically from E/F and F/G")


##############################################################################################
#removing insignifcant vars
loans$account_id <- NULL
loans$purpose<- NULL
loans$issue_date <- NULL#
loans$description <- NULL
loans$title<- NULL
loans$job_title<- NULL
#loans$interest_rate <- NULL#
#loans$credit_score<- NULL#
loans$district<-NULL#
loans$postcode_district<-NULL#
loans$purpose <-NULL
#loans$employment_length <- NULL
loans$earliest_credit_line <- NULL #takes too long 
loans$public_records <-NULL
loans$amount_payed <- NULL





###############Logistic Regression########################################
#We’ll create our train and test set by random sampling:

set.seed(300)
in_loans_train <- sample(nrow(loans), nrow(loans)*0.75)
training <- loans[in_loans_train, ]
test <- loans[-in_loans_train, ]



#Building the Model 1##########################################################################
names(loans)



model_interest_grade_debt <- glm(loan_status ~ home_ownership+ interest_range + grade + delinquency_2y 
                                 + employment_length + nr_accounts 
                                 + open_accounts + loan_amount + debt_to_income 
                                 + credit_card_balance + credit_card_usage + installment +
                                   annual_income+ last_record_months+ loan_amount+ term+ last_record_months +
                                   last_delinquency_months + last_derog_months + inquiries_6m +
                                   total_current_balance + year+
                                   public_records_bi+ emp_bin, family = "binomial", data = training)
summary(model_interest_grade_debt)


anova(model_interest_grade_debt, test = "Chisq")


#status vs interest range
model_interestrange <- glm(loan_status ~ interest_range, family = "binomial", data = training)
summary(model_interestrange)
model_interestrange$coefficients
coefficient <- model_interestrange$coefficients   # age coefficient
e <- exp(1)  # e equals the exponantional function from 1, do you remember from school math?
Percentage <- e^coefficient
Percentage


#status vs homeownership#############################################
model_homeownership <- glm(loan_status ~ home_ownership, family = "binomial", data = training)
summary(model_homeownership) #check the results, it says its not significant 
model_interestrange$coefficients
coefficient <- model_homeownership$coefficients   # age coefficient
e <- exp(1)  # e equals the exponantional function from 1, do you remember from school math?
Percentage <- e^coefficient
Percentage

#status vs interest+ loan amount+ debt to income + credit card balance etc .................
model_interest_grade_debt <- glm(loan_status ~ home_ownership+ interest_range + grade + delinquency_2y + public_records + employment_length + nr_accounts + open_accounts + loan_amount + debt_to_income + credit_card_balance + credit_card_usage , family = "binomial", data = training)
summary(model_interest_grade_debt) 

anova(model_interest_grade_debt, test = "Chisq")



##################################################################################################
#Looking at correlation using Pearsons correlation
loans$last_derog_months<-as.numeric(loans$last_derog_months)
loanstest<- loans %>% select(-c(emp_bin, interest_range, grade, home_ownership, term, last_derog_months_cat, last_delinquency_months, delinquency_2y))
loanstest[is.na(loanstest)] <- 0
df2 <- cor(loanstest, method = "pearson") ##pearsons correlation on numeric vars
df2
hc = findCorrelation(df2, cutoff=0.75)
hc = sort(hc)
reduced_Data = loanstest[,-c(hc)]
print (reduced_Data)


corrplot(loanstest, method = "circle") #correlation plot on vars

corrplot(lol, type = "upper")





#final model with 5 more correlated variables############################################################################
model_2 <- glm(loan_status ~  interest_range + debt_to_income + open_accounts+ 
                 delinquency_2y+ home_ownership
               ,family = "binomial", data = training)













####extra exploratory ##############
#if had more time could look at post code and district and title
prediction_1st <- predict(model_final, newdata = test[225,], type = "response")
prediction_1st

prediction_2nd <- predict(model_final, newdata = test[22553,], type = "response")
prediction_2nd
test[22553,]

prediction_3rd <- predict(model_final, newdata = test[22554,], type = "response")
prediction_3rd
test[22554,]

prediction_4th <- predict(model_final, newdata = test[22555,], type = "response")
prediction_4th
test[22555,]


#Using Decision Tree#######################################################################################
loans_model_dt <- ctree(loan_status ~ ., training)
#Plotting our decision tree of our credit risk analysis

plot(loans_model_dt)
plot(loans_model_dt, type = "simple")
loans_model_dt

#Use the model to make prediction
loans_pred_dt <- predict(loans_model_dt, test)
#Accuracy, Precision, and Recall
confusionMatrix(loans_pred_dt, test$loan_status, positive = "yes")
#Let’s try to set mincriterion to 0.7 to increase our Recall rate from the previous model.
loans_model_dt2 <- ctree(loan_status ~ ., training, control = ctree_control(mincriterion = 0.7))
plot(loans_model_dt2)
loans_model_dt2
#Use the model to make prediction
loans_pred_dt2 <- predict(loans_model_dt2, test)
#Accuracy, Precision, and Recall
confusionMatrix(loans_pred_dt2, test$default, positive = "yes")

