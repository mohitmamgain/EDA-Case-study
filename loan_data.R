# set working directory
loan_data <- read.csv("loan.csv", stringsAsFactors = FALSE)
# looks for duplicate value
sum(duplicated(loan_data$id)) # No duplicate value
sum(duplicated(loan_data$member_id)) # No duplicate value
sum(is.na(loan_data$loan_status)) # no NA values
sapply(loan_data$loan_status, function(x) length(which(x == "")))
sum(is.na(loan_data$purpose)) # No NA values

# In this data set there are many columns with mostly NA values, 0 or 1 valaues.
# Remove those columns as apart of data cleaning for ease of anaysis
loan_data <- loan_data[, -c(54:105)]
loan_data <- loan_data[, -c(55:59)]
loan_data <- loan_data[, -c(18:20,30,36,50:53)]

library(dplyr)
library(tidyr)
# convert all to lowercase
loan_data <- mutate_all(loan_data, funs(tolower))

# convert date to proper standard format
loan_data$issue_d <- paste("01-", loan_data$issue_d, sep="")
loan_data$issue_d <- as.Date(loan_data$issue_d, format = "%d-%y-%b")

# seperate year from date
loan_data$funded_year <- format(loan_data$issue_d, "%Y")


# removing current loan status from data frame as it is not relevant in analysis because we dont know
# either the loan borrower will default or not.
loan_data <- loan_data[-which((loan_data$loan_status) == "current"), ]

library(ggplot2)
library(scales)

# plot1 is for showing how many percentage of laon borrower have charged off or fully paid status.
plot1 <- ggplot(loan_data, aes(x = factor(loan_status, levels = names(sort(table(loan_status), decreasing=TRUE))))) + geom_bar(fill = "#FF6666")+ 
  geom_text(stat = "count", aes(label= sprintf("%.02f %%", ..count../sum(..count..)*100)), size=4, vjust= -0.5, colour="black")+
  ggtitle("frequency of people who default or not") +
  labs(x="Status", y="Frequency")
plot1
# PLOT 1 shows 14.59% of applicants are defaulters after removing current from loan status.

#------------------------------------------------------------------------------------------------------------

#plot2 is for showing how many applicants have applied for loan for 36 months term or 60 months term
plot2 <- ggplot(loan_data, aes(x = factor(term))) + geom_bar(fill = "#FF6666")+ 
  geom_text(stat = "count", aes(label= sprintf("%.02f %%", ..count../sum(..count..)*100)), size=4, vjust= -0.5, colour="black")+
  labs(x="term", y="count")
plot2
# plot 2 shows 75% applicants have applied for 36 months term

#------------------------------------------------------------------------------------------------------------

# plot3 shows number of applicants are increasing year by year.
plot3 <- ggplot(loan_data, aes(x = factor(funded_year))) + geom_bar(fill = "#FF6666")+ 
  geom_text(stat = "count", aes(label= sprintf("%.02f %%", ..count../sum(..count..)*100)), size=4, vjust= -0.5, colour="black")+
  labs(x="funded year", y="Frequency") 
plot3

#------------------------------------------------------------------------------------------------------------

#plot 4 shows 47% of loan applicant applied loans for debt consolidation purpose.
plot4 <- ggplot(loan_data, aes(x = factor(purpose, levels = names(sort(table(purpose), decreasing=TRUE))))) + geom_bar(fill = "#FF6666")+ 
  geom_text(stat = "count", aes(label= sprintf("%.02f %%", ..count../sum(..count..)*100)), size=4, vjust= -0.5, colour="black")+
  labs(x="purpose", y="Frequency") + theme(axis.text.x = element_text(angle = 60, hjust = 1))
plot4

#------------------------------------------------------------------------------------------------------------

# plot5 shows 48% of loan applicant applied loans have rented home and 44% are mortaged.
plot5 <- ggplot(loan_data, aes(x = factor(home_ownership, levels = names(sort(table(home_ownership), decreasing=TRUE))))) + geom_bar(fill = "#FF6666")+ 
  geom_text(stat = "count", aes(label= sprintf("%.02f %%", ..count../sum(..count..)*100)), size=4, vjust= -0.5, colour="black")+
  labs(x="Home ownership", y="Frequency")
plot5

#-----------------------------------------------------------------------------------------------------------

# plot6 shows 30% of loan applicant applied loans have comes under grade b.
plot6 <- ggplot(loan_data, aes(x = factor(grade, levels = names(sort(table(grade), decreasing=TRUE))))) + geom_bar(fill = "#FF6666")+ 
  geom_text(stat = "count", aes(label= sprintf("%.02f %%", ..count../sum(..count..)*100)), size=4, vjust= -0.5, colour="black")+
  labs(x="Grade", y="Frequency")
plot6

#---------------------------------------------------------------------------------------------------------

# plot7 shows 48.50% of loan applicant applied loans have comes nill inquiry in past 6 months.
plot7 <- ggplot(loan_data, aes(x = factor(inq_last_6mths, levels = names(sort(table(inq_last_6mths), decreasing=TRUE))))) + geom_bar(fill = "#FF6666")+ 
  geom_text(stat = "count", aes(label= sprintf("%.02f %%", ..count../sum(..count..)*100)), size=4, vjust= -0.5, colour="black")+
  labs(x="inquiry in last 6 months", y="Frequency")
plot7

#-------------------------------------------------------------------------------------------------------------

# plot8 shows nearly 95% of loan applicant have nill derogatory public records.
plot8 <- ggplot(loan_data, aes(x = factor(pub_rec, levels = names(sort(table(pub_rec), decreasing=TRUE))))) + geom_bar(fill = "#FF6666")+ 
  geom_text(stat = "count", aes(label= sprintf("%.02f %%", ..count../sum(..count..)*100)), size=4, vjust= -0.5, colour="black")+
  labs(x="Derogatory public records", y="Frequency")
plot8

#-----------------------------------------------------------------------------------------------------------
# Plot9 shows 22% of loan applicant have 10+ years of employment length
plot9 <- ggplot(loan_data, aes(x = factor(emp_length, levels = names(sort(table(emp_length), decreasing=TRUE))))) + geom_bar(fill = "#FF6666")+ 
  geom_text(stat = "count", aes(label= sprintf("%.02f %%", ..count../sum(..count..)*100)), size=4, vjust= -0.5, colour="black")+
  labs(x="employment length", y="count")
plot9
#-------------------------------------------------------------------------------------------------------

# plot10 shows 43.27% of loan applicant have not verified status.
plot10 <- ggplot(loan_data, aes(x = factor(verification_status, levels = names(sort(table(verification_status), decreasing=TRUE))))) +
  geom_bar(fill = "#FF6666")+ 
  geom_text(stat = "count", aes(label= sprintf("%.02f %%", ..count../sum(..count..)*100)), size=4, vjust= -0.5, colour="black")+
  labs(x="verification status", y="count")
plot10
#---------------------------------------------------------------------------------------------------------

# percentage of loan status for each purpose
plot11 <- ggplot(loan_data, aes(x = loan_status,  group = purpose)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat ="count") +
  geom_text(aes( label = scales::percent(..prop..), y = ..prop.. ), stat = "count", vjust = -.5) +
  labs(y = "Percent", fill="Status") +
  facet_grid(~purpose) +
  scale_y_continuous(labels = scales::percent)+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
plot11
# loan borrowers with small buisness purpose are most likely to default incomaprison to other purposes.

#-------------------------------------------------------------------------------------------------------------

# percentage of loan status for each employment length
plot12 <- ggplot(loan_data, aes(x= loan_status,  group = emp_length)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat ="count") +
  geom_text(aes( label = scales::percent(..prop..), y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="Status") +
  facet_grid(~emp_length) +
  scale_y_continuous(labels = scales::percent)+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
plot12
# loan borrowers with 10+ years of employment lenght are more likely to default

#---------------------------------------------------------------------------------------------------------

# percentage of loan status for each home_ownership
plot13 <- ggplot(loan_data, aes(x= loan_status,  group = home_ownership)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat ="count") +
  geom_text(aes( label = scales::percent(..prop..), y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="Status") +
  facet_grid(~home_ownership) +
  scale_y_continuous(labels = scales::percent)+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
plot13
# loan borrowers with others as home_ownership are more likely to default in comaprison to more home_ownership

#----------------------------------------------------------------------------------------------------------

# percentage of loan_status for each grade.
plot14 <- ggplot(loan_data, aes(x= loan_status,  group = grade)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat ="count") +
  geom_text(aes( label = scales::percent(..prop..), y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="Status") +
  facet_grid(~grade) +
  scale_y_continuous(labels = scales::percent)+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
plot14
# loan borrowers with grade G are more likely to default in comparison to other grades.

#---------------------------------------------------------------------------------------------------------

# percentage of laon_status for each verification_status
plot15 <- ggplot(loan_data, aes(x= loan_status,  group = verification_status)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat ="count") +
  geom_text(aes( label = scales::percent(..prop..), y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="loan Status") +
  facet_grid(~verification_status) +
  scale_y_continuous(labels = scales::percent)+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
plot15
# loan borrowers with verified status are more likely to default

#-----------------------------------------------------------------------------------------------------------

# percentage of loan_status for each term
plot16 <- ggplot(loan_data, aes(x= loan_status,  group = term)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat ="count") +
  geom_text(aes( label = scales::percent(..prop..), y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="loan Status") +
  facet_grid(~term) +
  scale_y_continuous(labels = scales::percent)+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
plot16
# loan borrower with 60months of term likely to default.

#-----------------------------------------------------------------------------------------------------------
#  percentage of loan applicants number of having derogatory public records.
plot17 <- ggplot(loan_data, aes(x= loan_status,  group = pub_rec)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat ="count") +
  geom_text(aes( label = scales::percent(..prop..), y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="loan Status") +
  facet_grid(~pub_rec) +
  scale_y_continuous(labels = scales::percent)+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
plot17
#------------------------------------------------------------------------------------------------------

# percentage of loan applicants with number of inquiry in last 6 months
plot18 <- ggplot(loan_data, aes(x= loan_status,  group = inq_last_6mths)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat ="count") +
  geom_text(aes( label = scales::percent(..prop..), y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="loan Status") +
  facet_grid(~inq_last_6mths) +
  scale_y_continuous(labels = scales::percent)+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
plot18
#---------------------------------------------------------------------------------------------------------------

# removing % sign from int_rate
loan_data$int_rate <- gsub("%", "", loan_data$int_rate)
loan_data$int_rate <- as.numeric(as.character(loan_data$int_rate))
# for ease of analysis we have created bins for interest rate
# 0-10% = low, 10-15% = medium, 15-25% = high
loan_data$int_rate_slot[((loan_data$int_rate >= 00) & 
                              (loan_data$int_rate < 10))] <- c("low")

loan_data$int_rate_slot[((loan_data$int_rate >= 10) & 
                              (loan_data$int_rate < 15))] <- c("medium")

loan_data$int_rate_slot[((loan_data$int_rate >= 15) & 
                              (loan_data$int_rate < 25))] <- c("high")


# convering into numeric
loan_data$annual_inc <- as.numeric(as.character(loan_data$annual_inc))
# creating box plot of annual income
boxplot(loan_data$annual_inc)
summary(loan_data$annual_inc)

# as there are many outliers in annual income, therefore 
boxplot(loan_data$annual_inc[loan_data$annual_inc < 150000 ])
summary(loan_data$annual_inc[loan_data$annual_inc < 150000 ])

loan_data$annual_inc <- as.numeric(as.character(loan_data$annual_inc))
# creating bins for annual income
loan_data$annual_inc_slot[((loan_data$annual_inc >= 00000) & 
                           (loan_data$annual_inc <= 50000))] <- c("low")

loan_data$annual_inc_slot[((loan_data$annual_inc > 50000) & 
                           (loan_data$annual_inc <= 100000))] <- c("medium")

loan_data$annual_inc_slot[((loan_data$annual_inc > 100000))] <- c("high")


# creating new df with filter on annual income less than 150000
loan_data_income <- filter(loan_data, loan_data$annual_inc <= 150000)

# plot20 percent of loan status for each annual income slot
plot20 <- ggplot(loan_data_income, aes(x= loan_status,  group = annual_inc_slot)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat ="count") +
  geom_text(aes( label = scales::percent(..prop..), y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="loan_status") +
  facet_grid(~annual_inc_slot) +
  scale_y_continuous(labels = scales::percent)+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
plot20


loan_data$installment <- as.numeric(as.character(loan_data$installment))
# boxplot for installment
boxplot(loan_data$installment)
summary(loan_data$installment)

# creating bins for installment
# 0-200 = low, 201-400 = medium, 401-800 = high
loan_data$installment_slot[((loan_data$installment >= 000) & 
                             (loan_data$installment <= 200))] <- c("low")

loan_data$installment_slot[((loan_data$installment > 200) & 
                             (loan_data$installment <= 400))] <- c("medium")

loan_data$installment_slot[((loan_data$installment > 400))] <- c("high") 

# creating bins for loan amount
loan_data$loan_amnt <- as.numeric(as.character(loan_data$loan_amnt))

#boxplot of loan amount
boxplot(loan_data$loan_amnt)
summary(loan_data$loan_amnt)

loan_data$loan_amnt_slot[((loan_data$loan_amnt >= 00000) & 
                              (loan_data$loan_amnt <= 5000))] <- c("low")

loan_data$loan_amnt_slot[((loan_data$loan_amnt > 5000) & 
                              (loan_data$loan_amnt <= 15000))] <- c("medium")

loan_data$loan_amnt_slot[((loan_data$loan_amnt > 15000))] <- c("high") 


# creating new df with filter on installment< 800
loan_data_installment <- filter(loan_data, loan_data$installment <= 800)

#plot21 shows percent of loan status for each installment slot
plot21 <- ggplot(loan_data_installment, aes(x= loan_status,  group = installment_slot)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat ="count") +
  geom_text(aes( label = scales::percent(..prop..), y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="loan_status") +
  facet_grid(~installment_slot) +
  scale_y_continuous(labels = scales::percent)+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
plot21

#creating new dataframe in which filter is on annual income, installmetn, loan amount anf loan status as charged off
loan_default <- filter(loan_data, loan_data$annual_inc <= 150000, loan_data$installment <= 800,loan_data$loan_amnt <= 25000, loan_data$loan_status == "charged off")
plot22 <- ggplot(loan_default, aes(x= annual_inc_slot,  group = installment_slot)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat ="count") +
  geom_text(aes( label = scales::percent(..prop..), y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="annual income slot") +
  facet_grid(~installment_slot) +
  scale_y_continuous(labels = scales::percent)+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
plot22

# Now as far we have done univariate and bivariate analyis.
# To identify 5 important drivar variables we have silter the data for top 4 purposes
# i.e. debt_consolidation, credit card, home improvement and major purchase.

loan_default_purpose <- filter(loan_default, ((loan_default$purpose == "debt_consolidation")|(loan_default$purpose == "credit_card")|(loan_default$purpose == "home_improvement")|(loan_default$purpose == "major_purchase")))
# plot23 shows percentage of defaulters on term basis for 4 major purposes
plot23 <- ggplot(loan_default_purpose, aes(x = factor(term))) + 
  geom_bar(stat = "count", fill = "#FF6666") + 
  facet_wrap( ~ loan_default_purpose$purpose)+
  geom_text(stat = "count", aes(label= sprintf("%.02f %%", ..count../sum(..count..)*100)), size=2, vjust= -0.4, colour="black")+
  labs(x="term", y="Counts")
plot23

#plot24 shows percentage of defaulters on grade basis for 4 major purposes
plot24 <- ggplot(loan_default_purpose, aes(x = factor(grade))) + 
  geom_bar(stat = "count", fill = "#FF6666") + 
  facet_wrap( ~ loan_default_purpose$purpose)+
  geom_text(stat = "count", aes(label= sprintf("%.02f %%", ..count../sum(..count..)*100)), size=2, vjust= -0.4, colour="black")+
  labs(x="Grade", y="Counts")
plot24

#plot25 shows percentage of defaulters on interest rate slot basis for 4 major purposes
plot25 <- ggplot(loan_default_purpose, aes(x = factor(int_rate_slot))) + 
  geom_bar(stat = "count", fill = "#FF6666") + 
  facet_wrap( ~ loan_default_purpose$purpose)+
  geom_text(stat = "count", aes(label= sprintf("%.02f %%", ..count../sum(..count..)*100)), size=2, vjust= -0.4, colour="black")+
  labs(x="Interest rate slot", y="Counts")
plot25

#plot26 shows percentage of defaulters on home ownership basis for 4 major purposes
plot26 <- ggplot(loan_default_purpose, aes(x = factor(home_ownership))) + 
  geom_bar(stat = "count", fill = "#FF6666") + 
  facet_wrap( ~ loan_default_purpose$purpose)+
  geom_text(stat = "count", aes(label= sprintf("%.02f %%", ..count../sum(..count..)*100)), size=2, vjust= -0.4, colour="black")+
  labs(x="Home Ownership", y="Counts")
plot26

#plot27 shows percentage of defaulters on annual inncome slot basis for 4 major purposes
plot27 <- ggplot(loan_default_purpose, aes(x = factor(annual_inc_slot))) + 
  geom_bar(stat = "count", fill = "#FF6666") + 
  facet_wrap( ~ loan_default_purpose$purpose)+
  geom_text(stat = "count", aes(label= sprintf("%.02f %%", ..count../sum(..count..)*100)), size=2, vjust= -0.4, colour="black")+
  labs(x="Annual Income Slot", y="Counts")
plot27



#plot28 shows percentage of defaulters on loan amount basis for 4 major purposes
plot28 <- ggplot(loan_default_purpose, aes(x = factor(loan_amnt_slot))) + 
  geom_bar(stat = "count", fill = "#FF6666") + 
  facet_wrap( ~ loan_default_purpose$purpose)+
  geom_text(stat = "count", aes(label= sprintf("%.02f %%", ..count../sum(..count..)*100)), size=2, vjust= -0.4, colour="black")+
  labs(x="loan amount slot", y="Counts")
plot28

#plot29 shows percentage of defaulters on employment length basis for 4 major purposes
plot29 <- ggplot(loan_default_purpose, aes(x = factor(emp_length))) + 
  geom_bar(stat = "count", fill = "#FF6666") + 
  facet_wrap( ~ loan_default_purpose$purpose)+
  geom_text(stat = "count", aes(label= sprintf("%.02f %%", ..count../sum(..count..)*100)), size=2, vjust= -0.4, colour="black")+
  labs(x="Employment length", y="Counts")
plot29

#plot30 shows percentage of defaulters on installment slot basis for 4 major purposes
plot30 <- ggplot(loan_default_purpose, aes(x = factor(installment_slot))) + 
  geom_bar(stat = "count", fill = "#FF6666") + 
  facet_wrap( ~ loan_default_purpose$purpose)+
  geom_text(stat = "count", aes(label= sprintf("%.02f %%", ..count../sum(..count..)*100)), size=2, vjust= -0.4, colour="black")+
  labs(x="Installment Slot", y="Counts")
plot30

#plot31 shows percentage of defaulters on verification status basis for 4 major purposes
plot31 <- ggplot(loan_default_purpose, aes(x = factor(verification_status))) + 
  geom_bar(stat = "count", fill = "#FF6666") + 
  facet_wrap( ~ loan_default_purpose$purpose)+
  geom_text(stat = "count", aes(label= sprintf("%.02f %%", ..count../sum(..count..)*100)), size=2, vjust= -0.4, colour="black")+
  labs(x="Verification Status", y="Counts")
plot31
