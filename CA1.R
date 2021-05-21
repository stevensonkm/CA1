# Installation of packages

install.packages("mice")
install.packages("VIm")
install.packages("psych")
install.packages("dplyr")
install.packages("sqldf")
install.packages("magrittr")
install.packages("hrbrthemes")

library(mice)
library(VIM)
library(psych)
library(dplyr)
library(sqldf)
library(magrittr)
library(hrbrthemes)

#---------------------------------------------------------------------------------------------------------------


# load the dataset "covid.csv"

covid_data <- read.csv("covid.csv", na = "", header = T, stringsAsFactors = T)  
covid_data [covid_data == ""] <- NA 

str(covid_data)

# change the date variable to date

covid_data$date <- as.Date(covid_data$date)
str(covid_data$date)
str(covid_data)

head(covid_data, n = 5)

class(covid_data)

nrow(covid_data)


# see if the dataset has any missing values
complete.cases(covid_data)
incomplete_data <- covid_data[!complete.cases(covid_data),]
incomplete_data


# visualize the missing variables
library("mice")
md.pattern(covid_data)

library(VIM)
missing_values <- aggr(covid_data, prop = FALSE, numbers = TRUE)

# summary of missing values
summary(missing_values)



# Research Question 1
# H0 Vaccination has no impact on new cases in USA
# H1 Vaccination has impact on new cases in USA

usa_covid <- subset(covid_data, iso_code == "USA", select = c("new_cases", "people_fully_vaccinated"))  
usa_covid  

# show the structure of USA covid
str(usa_covid)  

#Identify the missing values

missing_values <- aggr(usa_covid, prop = FALSE, numbers = TRUE)
summary(usa_covid)
any(is.na(usa_covid))

# Replacing NA with 0s in people_fully_vaccinated variable
usa_covid$people_fully_vaccinated[is.na(usa_covid$people_fully_vaccinated)] <- 0
usa_covid
# for correlation of the variables
pairs(usa_covid, labels = colnames(usa_covid), col = "RED", main = "COVID 19 Correlation Plot")
# Removing all the NA values
na.omit(usa_covid)
usa_covid<-na.omit(usa_covid)
usa_covid
# Correlation distribution of all the variables from the data frame
pairs.panels(usa_covid,
             smooth = TRUE,      # If TRUE, draws loess smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = TRUE,    # If TRUE, draws ellipses
             method = "spearman",# Correlation method (also "pearson" or "kendall")
             pch = 21,           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE)     
# to visualize the data frame
md.pattern(usa_covid)
library("lattice")
attach(usa_covid)
# visual analysis 
plot(new_cases, people_fully_vaccinated, pch = 16, col = "BLACK",  
     main = "Correlation between People fully vaccinated and New cases",
     xlab = "New Cases",
     ylab = "People fully Vaccinated")  

# Plotting the Histogram
histogram(~new_cases | people_fully_vaccinated, 
          data = usa_covid,
          main = "Correlation between People fully vaccinated and New cases", 
          xlab = "New Cases", 
          ylab = "People Fully Vaccinated")
# tapply for computing the median of the variable

tapply(new_cases, people_fully_vaccinated, median)  
qqnorm(new_cases)                                                            
qqline(new_cases, col = "RED")                                           
# to check the normality of the variables
normality_test <- shapiro.test(usa_covid$new_cases)                     
normality_test$p.value                                                          
hist(new_cases)
# correlation test to check the hypothesis
cor.test(usa_covid$new_cases, usa_covid$people_fully_vaccinated,  
         method = "spearman") 
detach(usa_covid)


# Research Question 2
# H0 new deaths has no correlation wih the Icu patients in UK
# H1 new deaths has no correlation wih the Icu patients in UK

uk_covid <- subset(covid_data, iso_code == "GBR", select = c("new_deaths", "icu_patients"))  
uk_covid

str(uk_covid)

# identify the missing values
incomplete_data <- uk_covid[!complete.cases(uk_covid),]
nrow(incomplete_data)

# plottiing the missing vales
md.pattern(uk_covid)
missing_values <- aggr(uk_covid, prop = FALSE, numbers = TRUE)
summary(missing_values)

attach(uk_covid)
# visual analysis
plot(new_deaths, icu_patients, pch = 19, col= "RED",main = "Correlation between new deaths and icu patients", xlab = "New Deaths", ylab = "ICU Patients")

# Correlation distribution of two variables
pairs(uk_covid, labels = colnames(uk_covid), col = "RED", main = "COVID 19 Correlation Plot")
# Removing the NA values
na.omit(uk_covid)
uk_covid<-na.omit(uk_covid)
uk_covid
# Correlation distribution of all the variables from the data frame
pairs.panels(uk_covid,
             smooth = TRUE,      # If TRUE, draws loess smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = TRUE,    # If TRUE, draws ellipses
             method = "spearman",# Correlation method (also "pearson" or "kendall")
             pch = 21,           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
                          ci = TRUE)     

# to visualize the data frame
md.pattern(uk_covid)
library("lattice")
attach(uk_covid)
# visual analysis 
plot(new_deaths, icu_patients, pch = 16, col = "BLACK",  
     main = "Correlation between new deaths and ICU patients",
     xlab = "New Deaths",
     ylab = "ICU patients")  

# Plotting the Histogram
histogram(~new_deaths | icu_patients, 
          data = uk_covid,
          main = "Correlation between new deaths and ICU patients", 
          xlab = "New Deaths", 
          ylab = "Icu patients")
# tapply for computing the median of the variable

tapply(new_deaths, icu_patients, median)  
qqnorm(new_deaths)                                                            
qqline(new_deaths, col = "RED")                                           
# to check the normality of the variables
normality_test <- shapiro.test(uk_covid$new_deaths)                     
normality_test$p.value                                                          
hist(new_deaths)
# correlation test to check the hypothesis
cor.test(uk_covid$new_deaths, uk_covid$icu_patients,  
         method = "spearman") 
detach(uk_covid)


# Research Question 3
# H0 Handwashing facilities has no impact with the new cases 
# H1 Handwashing facilities has  impact with the new cases  

hand_wash_data <- subset(covid_data, !is.na(covid_data$handwashing_facilities),
                         select = c("handwashing_facilities", "new_cases"))
str(hand_wash_data)
# identify the missing values
incomplete_data <- hand_wash_data[!complete.cases(hand_wash_data),]
nrow(incomplete_data)

# plottiing the missing vales
md.pattern(hand_wash_data)
missing_values <- aggr(hand_wash_data, prop = FALSE, numbers = TRUE)
summary(missing_values)

# removing the missing values
na.omit(hand_wash_data)
hand_wash_data <- na.omit(hand_wash_data)

hand_wash_data
attach(hand_wash_data)
# Correlation distribution of all the variables from the data frame
pairs.panels(hand_wash_data,
             smooth = FALSE,      # If TRUE, draws loess smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = TRUE,    # If TRUE, draws ellipses
             method = "spearman",# Correlation method (also "pearson" or "kendall")
             pch = 21,           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE) 

# Correlation distribution of two variables
plot(new_cases, handwashing_facilities, pch = 19, col = "RED")
# Plotting the Histogram
histogram(~new_cases | handwashing_facilities, 
          data = hand_wash_data, 
          main = "Hand wash COVID New cases", 
          xlab = "New Cases", 
          ylab = "Hand Washing facilities")
# tapply for computing the median of the variable
tapply(new_cases, handwashing_facilities, median)
qqnorm(new_cases)                                                            
qqline(new_cases, col = "RED")
temp_data <- sample_n(hand_wash_data, 10000)
temp_data<-hand_wash_data[sample(1:nrow(hand_wash_data), 5000, replace = FALSE),]
temp_data
# to check the normality of the variables
normality_test <- shapiro.test(temp_data$new_cases)
normality_test$p.value
hist(new_cases)                                           
# correlation test to check the hypothesis
cor.test(hand_wash_data$handwashing_facilities, hand_wash_data$new_cases, method = "spearman")

detach(hand_wash_data)


# Research Question 4 
# H0 Stringency index has no effect with new cases in Ireland
# H1 stringency index has effect with new cases in Ireland
irl_covid <- subset(covid_data, iso_code == "IRL", select = c("new_cases", "stringency_index"))  
irl_covid  

# show the structure of USA covid
str(irl_covid)  

#Identify the missing values

missing_values <- aggr(irl_covid, prop = FALSE, numbers = TRUE)
summary(irl_covid)
# Removing all the NA values
na.omit(irl_covid)
irl_covid<-na.omit(irl_covid)
irl_covid
# Correlation distribution of two variables
pairs(irl_covid, labels = colnames(irl_covid), col = "RED", main = "COVID 19 Correlation Plot")

# Correlation distribution of all the variables from the data frame
pairs.panels(irl_covid,
             smooth = TRUE,      # If TRUE, draws loess smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = TRUE,    # If TRUE, draws ellipses
             method = "spearman",# Correlation method (also "pearson" or "kendall")
             pch = 21,           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE)     

md.pattern(irl_covid)
library("lattice")
attach(irl_covid)
# visualization of data
plot(new_cases, stringency_index, pch = 16, col = "BLACK",  
     main = "Sringency index and new Cases",
     xlab = "New Cases",
     ylab = "Stringency Index")  

# Plotting the Histogram
histogram(~new_cases | stringency_index, 
          data = irl_covid,
          main = "Sringency index and new Cases", 
          xlab = "New Cases", 
          ylab = "Stringency Index")
# tapply for computing the median of the variable

tapply(new_cases, stringency_index, median)  
qqnorm(new_cases)                                                            
qqline(new_cases, col = "RED")                                           
# check the normality of the variables
normality_test <- shapiro.test(irl_covid$new_cases)                     
normality_test$p.value                                                          
hist(new_cases)
# correlation test to check the hypothesis
cor.test(irl_covid$new_cases, irl_covid$stringency_index,  
         method = "spearman") 
detach(irl_covid)
# Research question 5
# H0 There is no correlation between new cases per million smoothed and new vaccinations smoothed per million in India
# H1 there  no correlation between new cases per million and population density in India
ind_covid <- subset(covid_data, iso_code == "IND", select = c("new_cases_smoothed_per_million", "new_vaccinations_smoothed_per_million"))  
ind_covid  

# show the structure of USA covid
str(ind_covid)  

#Identify the missing values

missing_values <- aggr(ind_covid, prop = FALSE, numbers = TRUE)
summary(ind_covid)
# Removing all the NA values
na.omit(ind_covid)
ind_covid<-na.omit(ind_covid)
ind_covid
# Correlation distribution of variables
pairs(ind_covid, labels = colnames(ind_covid), col = "RED", main = "COVID 19 Correlation Plot")


pairs.panels(ind_covid,
             smooth = TRUE,      # If TRUE, draws loess smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = TRUE,    # If TRUE, draws ellipses
             method = "spearman",# Correlation method (also "pearson" or "kendall")
             pch = 21,           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE)     

md.pattern(ind_covid)
library("lattice")
attach(ind_covid)
plot(new_cases_smoothed_per_million, new_cases_smoothed_per_million, pch = 16, col = "BLACK",  
     main = "New cases and New Vaccinations per million",
     xlab = "New Cases per million",
     ylab = "New Vaccinations per million")  

# Plotting the Histogram
histogram(~new_cases_smoothed_per_million | new_vaccinations_smoothed_per_million, 
          data = ind_covid,
          main = "New cases and New Vaccinations per million", 
          xlab = "New Cases per million", 
          ylab = "New Vaccinations per million")
# tapply for computing the median of the variable

tapply(new_cases_smoothed_per_million, new_vaccinations_smoothed_per_million, median)  
qqnorm(new_cases_smoothed_per_million)                                                            
qqline(new_cases_smoothed_per_million, col = "RED")                                           
# check the normality of the variables
normality_test <- shapiro.test(ind_covid$new_cases_smoothed_per_million)                     
normality_test$p.value                                                          
hist(new_cases_smoothed_per_million)
# correlation test to check the hypothesis
cor.test(ind_covid$new_cases_smoothed_per_million, ind_covid$new_vaccinations_smoothed_per_million,  
         method = "spearman")
detach(ind_covid)



