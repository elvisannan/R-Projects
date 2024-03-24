#Importing Library 

library(tidyverse)
library(plotly)
library(IRdisplay)
library(dplyr)
library(ggplot2)

# Reading Data 
healthcare <- read.csv("healthcare.csv")

View(healthcare)

# Correlation of healthcare
nums <- unlist(lapply(healthcare, is.numeric), use.names = FALSE)

cor(healthcare[,nums])

#Adding Age Group to Healthcare DataFrame
healthcare <- healthcare %>%
  mutate(age_group = ifelse(age <= 17, "0-17",
                     ifelse(age <= 34, "18-34",
                     ifelse(age <= 51, "35-51",
                     ifelse(age <= 68, "52-68", "69-85")))))
# Bar Chart of Age Group 
ggplot(healthcare, aes(x = age_group)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribution of Age Groups",
       x = "Age Group",
       y = "Count") +
  theme_minimal()

# Create Box Plot for Charges and Body Mass Index
boxplot(healthcare$charges ~ healthcare$sex, main="Boxplot - Billing Amount by Sex",ylab = "Billing Amounts",xlab = "Sex", las = 1 )
boxplot(healthcare$charges ~ healthcare$Test.Results, main="Boxplot - Billing Amount by Test Results",ylab = "Billing Amounts",xlab = "Test Results", las = 1 )
boxplot(healthcare$bmi ~ healthcare$sex, main="Boxplot - Body Mass Index by Sex",ylab = "Body Mass Index",xlab = "Sex", las = 1 )

# Plot Pie Chart using Blood Type
redcolours <- c("#FFCCCC", "#FF6666", "#FF2400", "#8B0000","#DC143C", "#800000", "#FF6347", "#CE2029")

# Working out Blood Type percentage 
Healthcareblood <- healthcare %>% 
  group_by(Blood.Type) %>%
  summarise(counts = n(),
            percentage = n()/nrow(healthcare))

# Create Pie Chart 
pie <- plot_ly(data = Healthcareblood, labels = ~Blood.Type, values = ~percentage, type = 'pie', 
               marker = list(colors = redcolours, line = list(color = "black", width = 1))) %>%
  layout(title = "Pie Chart of Blood Type")

pie

# Predict linear regression model 
predict.charges <- lm(charges ~. , data = healthcare)
summary(predict.charges)


