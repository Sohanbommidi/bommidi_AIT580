###------------------
###Regression and Clustering
###------------------

###Students Name: Sohan Bommidi
###GNumber: G01358263

library(tidyverse)
library(ggplot2)
library(cluster)
library(ClusterR)
emp <- read.csv("/Users/sohanbommidi/Downloads/EmployeeAttrition(1).csv")

# Regression 
#1a.
with(emp, scatter.smooth(TotalWorkingYears, MonthlyIncome,col='black',lpars =
                           list(col = "red", lwd = 3, lty = 3)))
# Monthly income climbed as total working years increased, and the two observations are positively associated.

#1b.
with(emp, scatter.smooth(Age, DistanceFromHome,col='black',lpars =
                           list(col = "red", lwd = 3, lty = 3)))
# It's tough to follow the narrative since it doesn't show any link between age and distance from home.

#1c.
a= cor(x=emp$TotalWorkingYears, y=emp$MonthlyIncome)
print(a)
# Total Working Years and Monthly Income have a 0.7728 association and are connected.
b= cor(x=emp$Age, y=emp$DistanceFromHome)
print(b)
# The age and distance from home have a -0.00168 association.

#1d.
twy = lm(MonthlyIncome~TotalWorkingYears, data=emp)
print(twy)
summary(twy)
ggplot(emp, aes(x = TotalWorkingYears, y= MonthlyIncome), na.rm= TRUE) +geom_point() +labs(x = "TotalWorkingYears", y = "MonthlyIncome", title = "Relationship between TotalWorkingYears and MonthlyIncome using scatterplot")
# A statistically significant p-value is less than 0.05. It implies that there is substantial evidence against the null hypothesis, with a P-Value of 2.2e-16 and a significant connection.


# Clustering
#2a.
t = as.data.frame(cbind(emp$HourlyRate,emp$TotalWorkingYears))
K_means<-kmeans(t,3)
K_means1 <- as.factor(K_means$cluster)
K_means
ggplot(emp,aes(HourlyRate,TotalWorkingYears, color = K_means$cluster )) + geom_point()+labs(x = "HourlyRate", y = "TotalWorkingYears", title = "HourlyRate vs TotalWorkingYears")
# The Hourly Rate is divided into three clusters, with Cluster 1 ranging from 1 to 55, Cluster 2 from 55 to 80, and Cluster 3 from 80 to 100.

#2b. 
Km<-kmeans(t,5)
ggplot(emp,aes(HourlyRate,TotalWorkingYears, color = Km$cluster )) + geom_point()+ labs(x = "HourlyRate", y = "TotalWorkingYears", title = "HourlyRate vs TotalWorkingYears")
# It has been divided into separate hourlyrate groupings utilizing the 5 clusters. The hourly rate varies from 10 to 50, 50-68, 68-82, 75-100, and 83-100.
