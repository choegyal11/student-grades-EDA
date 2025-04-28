# Load necessary libraries
library(tidyverse)
library(ggplot2)
library(corrplot)
library(gridExtra)
library(psych)

# Read the dataset
students <- read.csv("Students_Grading_Dataset.csv")

# Initial exploration
str(students)
summary(students)

# Check for missing values
colSums(is.na(students))

## 1. Quantitative Variable Analysis

# Summary statistics for numerical variables
describe(students %>% select(Age, Attendance...., Midterm_Score, Final_Score, 
                             Assignments_Avg, Quizzes_Avg, Participation_Score, 
                             Projects_Score, Total_Score, Study_Hours_per_Week, 
                             Stress_Level..1.10., Sleep_Hours_per_Night))

# Distribution of Total Score
ggplot(students, aes(x = Total_Score)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  ggtitle("Distribution of Total Scores") +
  xlab("Total Score") + ylab("Frequency")

# Boxplot of Total Score by Department
ggplot(students, aes(x = Department, y = Total_Score, fill = Department)) +
  geom_boxplot() +
  ggtitle("Total Score Distribution by Department") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Scatterplot: Study Hours vs Total Score
ggplot(students, aes(x = Study_Hours_per_Week, y = Total_Score)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  ggtitle("Study Hours vs Total Score") +
  xlab("Study Hours per Week") + ylab("Total Score")

# Scatterplot: Attendance vs Total Score
ggplot(students, aes(x = Attendance...., y = Total_Score)) +
  geom_point(alpha = 0.5, color = "green") +
  geom_smooth(method = "lm", color = "red") +
  ggtitle("Attendance vs Total Score") +
  xlab("Attendance (%)") + ylab("Total Score")

# Correlation matrix for numerical variables
numeric_vars <- students %>% select(Attendance...., Midterm_Score, Final_Score,
                                    Assignments_Avg, Quizzes_Avg, Participation_Score,
                                    Projects_Score, Total_Score, Study_Hours_per_Week,
                                    Stress_Level..1.10., Sleep_Hours_per_Night)

cor_matrix <- cor(numeric_vars)
corrplot(cor_matrix, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45,
         title = "Correlation Matrix of Numerical Variables")

## 2. Qualitative Variable Analysis

# Total Score by Gender
ggplot(students, aes(x = Gender, y = Total_Score, fill = Gender)) +
  geom_boxplot() +
  ggtitle("Total Score by Gender")

# Total Score by Extracurricular Activities
ggplot(students, aes(x = Extracurricular_Activities, y = Total_Score, 
                     fill = Extracurricular_Activities)) +
  geom_boxplot() +
  ggtitle("Total Score by Extracurricular Activities")

# Total Score by Internet Access at Home
ggplot(students, aes(x = Internet_Access_at_Home, y = Total_Score, 
                     fill = Internet_Access_at_Home)) +
  geom_boxplot() +
  ggtitle("Total Score by Internet Access at Home")

# Total Score by Parent Education Level
ggplot(students, aes(x = Parent_Education_Level, y = Total_Score, 
                     fill = Parent_Education_Level)) +
  geom_boxplot() +
  ggtitle("Total Score by Parent Education Level") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Total Score by Family Income Level
ggplot(students, aes(x = Family_Income_Level, y = Total_Score, 
                     fill = Family_Income_Level)) +
  geom_boxplot() +
  ggtitle("Total Score by Family Income Level")

## 3. Statistical Tests for Relationships

# ANOVA: Does department affect total score?
anova_department <- aov(Total_Score ~ Department, data = students)
summary(anova_department)

# t-test: Does gender affect total score?
t.test(Total_Score ~ Gender, data = students)

# t-test: Do extracurricular activities affect total score?
t.test(Total_Score ~ Extracurricular_Activities, data = students)

# ANOVA: Does parent education level affect total score?
anova_education <- aov(Total_Score ~ Parent_Education_Level, data = students)
summary(anova_education)

# Correlation tests
cor.test(students$Total_Score, students$Attendance....)
cor.test(students$Total_Score, students$Study_Hours_per_Week)
cor.test(students$Total_Score, students$Stress_Level..1.10.)
cor.test(students$Total_Score, students$Sleep_Hours_per_Night)

## 4. Multiple Linear Regression to predict Total Score

# Create a regression model
model <- lm(Total_Score ~ Attendance.... + Midterm_Score + Final_Score + 
              Assignments_Avg + Quizzes_Avg + Participation_Score + 
              Projects_Score + Study_Hours_per_Week + Stress_Level..1.10. + 
              Sleep_Hours_per_Night + Gender + Extracurricular_Activities + 
              Internet_Access_at_Home + Parent_Education_Level + 
              Family_Income_Level + Department, 
            data = students)

summary(model)

# Check for significant predictors
significant_predictors <- summary(model)$coefficients[,4] < 0.05
names(significant_predictors)[significant_predictors]

## 5. Stress and Sleep Analysis

# Stress level distribution
ggplot(students, aes(x = Stress_Level..1.10.)) +
  geom_histogram(binwidth = 1, fill = "purple", color = "black") +
  ggtitle("Distribution of Stress Levels")

# Sleep hours distribution
ggplot(students, aes(x = Sleep_Hours_per_Night)) +
  geom_histogram(binwidth = 0.5, fill = "orange", color = "black") +
  ggtitle("Distribution of Sleep Hours per Night")

# Stress vs Total Score
ggplot(students, aes(x = Stress_Level..1.10., y = Total_Score)) +
  geom_point(alpha = 0.5, color = "purple") +
  geom_smooth(method = "lm", color = "red") +
  ggtitle("Stress Level vs Total Score") +
  xlab("Stress Level (1-10)") + ylab("Total Score")

# Sleep vs Total Score
ggplot(students, aes(x = Sleep_Hours_per_Night, y = Total_Score)) +
  geom_point(alpha = 0.5, color = "orange") +
  geom_smooth(method = "lm", color = "red") +
  ggtitle("Sleep Hours vs Total Score") +
  xlab("Sleep Hours per Night") + ylab("Total Score")

## 6. Grade Analysis

# Grade distribution
ggplot(students, aes(x = Grade, fill = Grade)) +
  geom_bar() +
  ggtitle("Grade Distribution")

# Average total score by grade
students %>% 
  group_by(Grade) %>% 
  summarize(mean_score = mean(Total_Score)) %>% 
  ggplot(aes(x = Grade, y = mean_score, fill = Grade)) +
  geom_col() +
  ggtitle("Average Total Score by Grade") +
  ylab("Average Total Score")