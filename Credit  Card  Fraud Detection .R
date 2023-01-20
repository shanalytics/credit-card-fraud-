#Step one install all needed packages 
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("lubridate")
install.packages("dplyr")
install.packages("here")
install.packages("skimr")
install.packages("janitor")
install.packages("ggpubr")
install.packages("ggrepel")
install.packages("tidymodels")

library("tidyverse") #helps wrangle the data 
library("ggplot2") #helps visualize the data
library("lubridate") #helps wrangle date attributes
library("dplyr")
library("here")#enables easy file referencing by using the top-level directory of a file project to easily build file path
library("skimr")#is designed to provide summary statistics about variables in data frames, tibbles, data tables and vectors
library("janitor")#perfectly format data.frame column names; provide quick counts of variable combinations (
library("ggpubr")#facilitates the creation of beautiful ggplot2-based graphs for researcher with non-advanced programming backgrounds
library("ggrepel")#Provides text and label geoms for 'ggplot2' that help to avoid overlapping text labels. 
library("tidymodels")#used for for modeling and statistical analysis that share the underlying design philosophy, grammar

#Set file location 
getwd()
setwd("/Users/violetmakena/Credit Card Fraud Detection")

#import data 
library(readr)
creditcard <- read_csv("~/Dropbox/Mac/Desktop/creditcard.csv")
View(creditcard)

#data discovery
dim(creditcard)
colnames(creditcard)
is.null(creditcard)
dim(creditcard)
#View unique values in the data frame 

#Descriptive Analysis
creditcard$Class <- as.character(creditcard$Class)
is.character(creditcard$Class)

#create filtered data sets and analyse individual data 
library(dplyr)
install.packages("funModeling")
install.packages("Hmisc")

library(funModeling) 
library(Hmisc)

#Step 1 - First approach to data
glimpse(Stud_Per)#Number of observations (rows) and variables, and a head of the first cases.
#Getting the metrics about data types, zeros, infinite numbers, and missing values

df_status(creditcard)
describe(creditcard)
#df_status returns a table, so it is easy to keep with variables that match certain conditions like:
#+ Having at least 80% of non-NA values (p_na < 20)
#+ Having less than 50 unique values (unique <= 50)
#View unique values in the data frame 
unique(creditcard$Class)

#get the difference between normal and fraudulent transactions

table(creditcard$Class)
creditcard$Class <- as.character(creditcard$Class)
is.character(creditcard$Class)
#Step 2 - Analyzing categorical variables
freq(creditcard$Class)
#fraudulent transactions add up to 492 which is 17% of the total transactions 

#Step 3 - Analyzing numerical variables
plot_num(creditcard)
#Try to identify high-unbalanced variables

#Visually check any variable with outliers
data_prof=profiling_num(creditcard)

data_prof %>% select(variable, variation_coef, range_98)
#A high value in variation_coef may indictate outliers. range_98 indicates where most of the values are.

# Since it is in seconds, change to minute and hour, then do feature selection
# Change into time delta object and creates a time data class and out of that, convert into minutes and hours.

# convert seconds to hours and add new column

creditcard <- creditcard %>%
  mutate(hours = creditcard$Time/3600)
head(creditcard)

#Legitimate vs Fraudulent transactions 
ggplot(data = creditcard, 
       aes(x = creditcard$hours,
           y = creditcard$Amount)) +
  geom_point(aes(color = creditcard$Class, 
                 shape = creditcard$Class),
             size = 3,
             alpha = 0.8) +
  scale_color_manual(values = c("darkorange","purple")) +
  labs(title = "Creditcard transactions",
       subtitle = "credit card transactions Legitimate vs Fraudulent",
       x = "Time(hrs)",
       y = "Amount",
       color = "Class",
       shape = "Class") +
  theme(legend.position = c(0.2, 0.7),
        plot.title.position = "plot",
        plot.caption = element_text(hjust = 0, face= "italic"),
        plot.caption.position = "plot")

Legit_trans <- filter(creditcard, creditcard$Class == 0)
print(Legit_trans)
# create a histogram with 20 bars
hist(Legit_trans, breaks = 20)

Fraud_trans <- filter(creditcard, creditcard$Class == 1)
print(Fraud_trans)
# create a histogram with 20 bars
hist(Fraud_trans, breaks = 20)

creditcard$Class <- as.numeric(creditcard$Class)
is.numeric(creditcard$Class)

creditcard.cor = cor(creditcard, method = c("spearman"))
corrplot(creditcard.cor)


