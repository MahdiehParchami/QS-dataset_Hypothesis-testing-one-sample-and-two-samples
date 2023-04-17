
# set working directory

setwd("E:/Milleson_1/")

#  read in data
# install.packages("readr")
# install.packages("dplyr")
# install.packages("ggstatsplot")
# install.packages("corrplot")
# install.packages("jmv")
# install.packages('ggside')
# install.packages("rstatix")
# install.packages("writexl")
# install.packages("gplots")
# install.packages("webr")
# install.packages("BSDA")
# install.packages("ggpubr")
# install.packages("gginference")

library(gginference)
library(ggpubr)
library(BSDA)
library(webr)
library(rstatix)
library(ggside)
library(jmv)
library(corrplot)
library(readr)
library(tidyverse)
library(dplyr)
library(plotrix)
library(ggplot2)
library(gplots)
library(ggstatsplot)


df <- read_csv("qs.csv")
df

summary(df)
str(df)
count(df)

# data cleaning

# find non-UTF8 characters in variables 
df[duplicated(df),]
df$university <- gsub("[^[:alnum:][:blank:]?&/\\-]", "", df$university)
df$country <- gsub("[^[:alnum:][:blank:]?&/\\-]", "", df$country)
df$city <- gsub("[^[:alnum:][:blank:]?&/\\-]", "", df$city)
df$region <- gsub("[^[:alnum:][:blank:]?&/\\-]", "", df$region)


#  Convert string from lowercase to uppercase in
df$research_output <- str_to_title(df$research_output)


#drop rows that have more than 4 missing values 
sum(is.na(df))
df1 <- df[complete.cases(df), ]


# drop 'link' and 'logo' column as they are hyperlinks.
# Although 'score' column can be very useful for analysis,
# its missing nearly 50% values

df1$link <- NULL
df1$logo <- NULL
df1$score <- NULL
df1

# Arrange variables / create variable

df1$international_students <- as.numeric(df1$international_students)
df1$rank_display <- as.numeric(df1$rank_display)
df1$faculty_count <- as.numeric(df1$faculty_count)
summary(df1)

df2 <- head(df1,20)


# check whether the data follow a normal distribution and apply t-test.

# t test- one sample
# H0 = mean = 5000
# H1 = mean > 5000
shapiro.test(df2$international_students) 

ggdensity(df2$international_students, main = "Check Normality", xlab = "international_students", fill = ("#00FF00"))


t.test(df2$international_students, mu = 5000 , alternative = "greater")
ggttest(t.test(df2$international_students, mu = 5000 , paired = FALSE,
               var.equal = TRUE,alternative = "greater"))

qt(p=.05, df=19, lower.tail= FALSE)

# t test one sample
# H0 = mean = 6
# H1 = mean < 6

#  p-value is greater than the significance level 0.05 we can assume the normality.

shapiro.test(df2$student_faculty_ratio) 

ggdensity(df2$student_faculty_ratio, main = "Check Normality", xlab = "student_faculty_ratio", fill = ("#FFC0CB"))

t.test(df2$student_faculty_ratio, mu = 6 ,  alternative = "less")

ggttest(t.test(df2$student_faculty_ratio, mu = 6 , paired = FALSE,
               var.equal = TRUE,alternative = "less"))

qt(p=.05, df=18, lower.tail= TRUE)

# t test two sample
# H0 = m1 = m2
# H1 = m1 != m2

#  p-value is greater than the significance level 0.05 we can assume the normality.

shapiro.test(df2$international_students) 

ggdensity(df2$international_students, main = "Check Normality", xlab = "international_students", fill = ("#FFD700"))

var.test(df2$international_students ~ df2$type)

t.test(df2$international_students ~ df2$type,alternative = "two.sided", var.equal = TRUE )

ggttest(t.test(df2$international_students ~ df2$type , paired = FALSE,
               var.equal = TRUE,alternative = "two.sided"))



