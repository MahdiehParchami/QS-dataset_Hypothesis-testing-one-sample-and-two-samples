setwd("E:/Milleson_1/")

# read in data
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
# install.packages("tidyverse")
# install.packages("ggthemes")
# install.packages("gmodels")
# install.packages("qqplotr")


# # library(gmodels)
# library(ggthemes)
# # library(reticulate)
# # library(webr)
# # library(rstatix)
# # library(ggside)
# # library(jmv)
# library(corrplot)
# library(readr)
# library(tidyverse)
# library(dplyr)
# # library(plotrix)
# library(ggplot2)
# library(gplots)
# library(ggstatsplot)

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


# select complete rows 
sum(is.na(df))
df1 <- df[complete.cases(df), ]
df1

# drop 'link' and 'logo' column as they are hyperlinks.
# Although 'score' column can be very useful for analysis,
# its missing nearly 50% values

df1$link <- NULL
df1$logo <- NULL
df1$score <- NULL
df1

# Arrange variables


df1$international_students <- as.numeric(df1$international_students)
df1$rank_display <- as.numeric(df1$rank_display)
df1$faculty_count <- as.numeric(df1$faculty_count)
summary(df1)



df2 <- head(df1,30)

#   descriptive statistic after data cleaning


# rank_display

shapiro.test(df2$rank_display) 

df2$rank_display <- as.numeric(df2$rank_display)


ggqqplot(df2$rank_display, ylab = " rank_display distribution",
         ggtheme = theme_minimal(), color = "#00BFFF")

mean(df2$rank_display, na.rm = TRUE)
median(df2$rank_display , na.rm = TRUE)
sd(df2$rank_display , na.rm = TRUE)

find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}
find_mode(df2$rank_display)
min(df2$rank_display ,na.rm = TRUE)
max(df2$rank_display, na.rm = TRUE)

# international_students

shapiro.test(df2$international_students)

ggqqplot(df2$international_students, ylab = " international_students distribution",
         ggtheme = theme_minimal(), color = "#00BFFF")

mean(df2$international_students, na.rm = TRUE)
median(df2$international_students, na.rm = TRUE)
sd(df2$international_students , na.rm = TRUE)

find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}
find_mode(df2$international_students)
min(df2$international_students ,na.rm = TRUE)
max(df2$international_students, na.rm = TRUE)

# faculty_count

shapiro.test(df2$faculty_count)

ggqqplot(df2$faculty_count, ylab = " faculty_count distribution",
         ggtheme = theme_minimal(), color = "#00BFFF")

mean(df2$faculty_count, na.rm = TRUE)
median(df2$faculty_count)
sd(df2$faculty_count , na.rm = TRUE)

find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}
find_mode(df1$faculty_count)
min(df2$faculty_count ,na.rm = TRUE)
max(df2$faculty_count, na.rm = TRUE)

# student_faculty_ratio

shapiro.test(df2$student_faculty_ratio)

ggqqplot(df2$student_faculty_ratio, ylab = " student_faculty_ratio distribution",
         ggtheme = theme_minimal(), color = "#00BFFF")

mean(df2$student_faculty_ratio, na.rm = TRUE)
median(df2$student_faculty_ratio, na.rm = TRUE)
sd(df2$student_faculty_ratio , na.rm = TRUE)

find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}
find_mode(df$student_faculty_ratio)
min(df2$student_faculty_ratio ,na.rm = TRUE)
max(df2$student_faculty_ratio, na.rm = TRUE)



# ----------------------------------------------------------------------------
# Matrix Correlation

df2$type[df2$type == "Public"] = 1
df2$type[df2$type == "Private"] = 0
df2$type <- as.numeric(df2$type)

sub_df2 <- df2[, c(3,9:10,12)]

corr <- round(cor(sub_df2), 1)
corr# Plot

#install.packages("corrplot")
library(corrplot)

corrplot(corr,type = "upper",order = "hclust" ,
         col = COL2('PiYG'),addCoef.col = 'black')
--------------------------------------------------------------------------
  # question 1
  
  # implementing cor.test()
  cor.test(df2$student_faculty_ratio,df2$rank_display,method = "pearson")

# implementing scatter Plot

options(scipen=999)  # turn-off scientific notation like 1e+48

theme_set(theme_bw())  # pre-set the bw theme.

gg <- ggplot(df2, aes(x = student_faculty_ratio, y = rank_display )) + 
  geom_point(aes(col= rank_display, size = student_faculty_ratio)) + 
  geom_smooth(method="loess", se=F) + 
  xlim(c(1, 15)) + 
  ylim(c(1,30)) + 
  labs(subtitle="Correlation rank_display Vs student_faculty_ratio", 
       y="rank_display", 
       x="student_faculty_ratio", 
       title="Scatterplot", 
       caption = "Source: QS")
plot(gg)

# Implementing Regression

fit <- lm(rank_display ~ student_faculty_ratio, data = df2)
summary(fit)


theme_set(theme_bw())  # pre-set the bw theme.
g <- ggplot(df2, aes(student_faculty_ratio, rank_display)) + 
  labs(subtitle="rank_display vs student_faculty_ratio ",
       title="ScatterPlot")
g + geom_jitter(aes(col=rank_display, size=student_faculty_ratio)) + 
  geom_smooth(aes(col=rank_display), method="lm", se=F)

# Hypothesis test

# install.packages("gginference")
library(gginference)

t.test(df2$student_faculty_ratio, df2$rank_display,alternative = "two.sided", var.equal = TRUE )

ggttest(t.test(df2$student_faculty_ratio , df2$rank_display , paired = FALSE,
               var.equal = TRUE,alternative = "two.sided"))
qt(p=.05, df=28, lower.tail= TRUE)

# -------------------------------------------------------------------------------
# question 2

# implementing cor.test()
cor.test(df2$faculty_count,df2$international_students,method = "pearson")

cor.test(df2$type,df2$international_students,method = "pearson")
# implementing scatter Plot

options(scipen=999)  # turn-off scientific notation like 1e+48

theme_set(theme_bw())  # pre-set the bw theme.

gg <- ggplot(df2, aes(x = faculty_count, y = international_students )) + 
  geom_point(aes(col= international_students, size = faculty_count)) + 
  geom_smooth(method="loess", se=F) + 
  xlim(c(1, 10000)) + 
  ylim(c(1,18000)) + 
  labs(subtitle="Correlation international_students Vs faculty_count", 
       y="international_students", 
       x="faculty_count", 
       title="Scatterplot", 
       caption = "Source: QS")
plot(gg)

options(scipen=999)  # turn-off scientific notation like 1e+48

theme_set(theme_bw())  # pre-set the bw theme.

gg <- ggplot(df2, aes(x = type, y = international_students )) + 
  geom_point(aes(col= type, size = international_students)) + 
  
  xlim(c(0, 1)) + 
  ylim(c(1,18000)) + 
  labs(subtitle="Correlation international_students Vs Type Of universities", 
       y="international_students", 
       x="Type OF universities", 
       title="Scatterplot", 
       caption = "Source: QS")
plot(gg)


boxplot(international_students~type, data = df2, col = c("#0000FF", "#FF00FF"),
        xlab = "Type Of universities", ylab = "international_students",
        main = "international_students by Type Of universities")
# Implementing Regression

fixed.dum <- lm(international_students ~ faculty_count + type, data = df2)
summary(fixed.dum)


ggplot(df2, aes(faculty_count, international_students, col = type)) + 
  geom_point(size = 3) + # change size and colour
  labs(y = "internationalo students", x = "faculty_count") + # rename axes
  geom_smooth(method = 'lm', se = F) # fit linear regression line

# Hypothesis test

# install.packages("gginference")
library(gginference)

t.test(df2$international_students , df2$faculty_count,alternative = "two.sided", var.equal = TRUE )

ggttest(t.test(df2$international_students , df2$faculty_count , paired = FALSE,
               var.equal = TRUE,alternative = "two.sided"))

qt(p=.05, df=58, lower.tail= FALSE)


