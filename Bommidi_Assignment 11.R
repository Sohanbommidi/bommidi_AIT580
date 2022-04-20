###------------------
###Visualization
###------------------

###Students Name:SOHAN BOMMIDI
###GNumber: G01358263

library(tidyverse)
library(ggplot2)

data <- read.csv("/Users/sohanbommidi/Downloads/EmployeeAttrition(1).csv")
View(data)

ggplot(data)+
  geom_histogram(aes(x=Age),binwidth = 3,color="black")+
  labs(title = "Histogram for Age",
       x= "Age",
       y="Count")

ggplot(data,aes(x=Age,y=MonthlyIncome))+
  geom_point()+
  labs()
