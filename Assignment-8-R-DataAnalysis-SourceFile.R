###--------------------------------------
#Student Name: Sohan Bommidi
#GNumber: G01358263
###--------------------------------------

rm(list=ls())

data <- read.csv("/Users/sohanbommidi/Downloads/EmployeeAttrition.csv")

# this is just for testing to use "print" statement.
print(data[1,])



# a. Find the number of rows and columns in the dataset (5 points)
nrow(data)
ncol(data)


# b. Find the maximum Age in the dataset (5 points)
max(data$Age)



# c. Find the minimum DailyRate in the dataset (5 points)
min(data$DailyRate)


# d. Find the average/mean MontlyIncome in the dataset (5 points)
mean(data$MonthlyIncome)


# e. How many employees rated WorkLifeBalance as 1 (5 points)
length(which( data$WorkLifeBalance == 1))


# f. What percent of total employees have TotalWorkingYears less than equal to 5? Also calculate the percentage for TotalWorkingYears greater than 5 (5 points)
length(which(data$TotalWorkingYear<=5))/sum(data$TotalWorkingYear)
length(which(data$TotalWorkingYear>5))/sum(data$TotalWorkingYear)

# g. Print EmployeeNumber, Department and MaritalStatus for those employees whose Attrition is Yes and RelationshipSatisfaction is 1 and YearsSinceLastPromotion is greater than 3 (10 points)
data1<-subset(data,RelationshipSatisfaction==1 & Attrition=='Yes'& YearsSinceLastPromotion >3,select = c(EmployeeNumber, Department, MaritalStatus))
print(data1)
# h. Find the mean, median, mode, standard deviation and frequency distribution of EnvironmentSatisfaction for males and females separately. (Hint: For frequency distribution use table() function (10 points)
male_data<- data %>% filter(Gender == "Male")
female_data<- data %>% filter(Gender == "Female")
Environmental_Male<- maledata$EnvironmentSatisfaction
Environmental_Female<- femaledata$EnvironmentSatisfaction
# mean of the environment satisfaction data for males
mean(Environmental_Male)
# mean of the environment satisfaction data for females
mean(Environmental_Female)
# median of the environment satisfaction data for males
median(Environmental_Male)
# median of the environment satisfaction data for females
median(Environmental_Female)
# standard deviation of the environment satisfaction data for males
sqrt(var(Environmental_Male))
# standard deviation of the environment satisfaction data for females
sqrt(var(Environmental_Female))
# frequency distribution of the environment satisfaction data for males
table(Environmental_Male)
# frequency distribution of the environment satisfaction data for females
table(Environmental_Female)
# mode of the environment satisfaction data for males
mode <- function(Environmental_Male) {
  a <- unique(EnvironmentalMale)
  table <- tabulate(match(Environmental_Male, a))
  a[table == max(table)]
}
mode(Environmental_Male)
# mode of the environment satisfaction data for females
mode <- function(Environmental_Female) {
  a <- unique(Environmental_Female)
  table <- tabulate(match(Environmental_Female, a))
  a[table == max(table)]
}
mode(Environmental_Female)



PART 2

#1. Identify data types for each attribute in the dataset
data<- read.csv("/Users/sohanbommidi/Downloads/Acme.csv")
print(data)

#to find the attributes of the dataset
attributes(data)

#to find the datatypes of attibutes from the dataset
typeof(data$Years)
typeof(data$StSalary)
typeof(data$Gender)
typeof(data$Degree)


#2.Produce a summary statistic for each attribute in the dataset
summary(data)

#3. Produce visualizations for each attribute
hist(data$Years)
hist(data$StSalary)
hist(data$Gender)
hist(data$Degree)
#As we observe that both the Gender and Degree are not numeric we cannot draw the histograms for these attributes.

#4. Display the relationship between
#a. Years of Experience and Starting Salary for all employees 
plot(data$Years,data$StSalary)
lines(lowess(data$Years,data$StSalary),col= "red")
#b. Years of Experience and Starting Salary for each gender
library(dplyr)
male_data<- data %>% filter(Gender == "M")
female_data<- data %>% filter(Gender == "F")
male_Years<-male_data$Years
male_Salary<-male_data$StSalary
plot(male_Years,male_Salary)
lines(lowess(male_Years,male_Salary),col= "red")


female_Years<- female_data$Years
female_Salary<- female_data$StSalary
plot(female_Years,female_Salary)
lines(lowess(female_Years,female_Salary),col= "red")


#c. Years of Experience and Starting Salary for each degree 
deg1<- data %>% filter(Degree == "BS")
deg2<- data %>% filter(Degree == "MS")
deg3<- data %>% filter(Degree == "PhD")


deg1_Years<-deg1$Years
deg1_Salary<-deg1$StSalary
plot(deg1_Years,deg1_Salary)
lines(lowess(deg1_Years,deg1_Salary),col= "red")



deg2_Years<-deg2$Years
deg2_Salary<-deg2$StSalary
plot(deg2_Years,deg2_Salary)
lines(lowess(deg2_Years,deg2_Salary),col= "red")


deg3_Years<-deg3$Years
deg3_Salary<-deg3$StSalary
plot(deg3_Years,deg3_Salary)
lines(lowess(deg3_Years,deg3_Salary),col= "red")

#5. Find the correlation between Starting Salary and Years of Experience? 
corr1 <- cor.test(data$StSalary, data$Years, 
                method = "pearson")
corr1

#corelation for genders
#for male
corr2<-cor.test(male_Salary,male_Years,method = "pearson")
corr2
#for female
corr3<-cor.test(female_Salary,female_Years,method = "pearson")
corr3
 # it is different for males and females
#for deg1 BS
corr4<-cor.test(deg1_Salary,deg1_Years,method = "pearson")
corr4
#for deg2 MS
corr5<-cor.test(deg2_Salary,deg2_Years,method = "pearson")
corr5
#for deg3PhD
corr6<-cor.test(deg3_Salary,deg3_Years,method = "pearson")
corr6

#6. What can you conclude about Acme with respect to gender bias after your overall analysis?
Males and females with MS and BS degrees and 1-6 years of experience earn comparable starting salaries; however, there is just one female PhD who earns similarly to male PhDs with equivalent years of experience. There are only minimal disparities in starting pay between male and female employees, according to me, but I cannot make any conclusions in the case of female PhDs because there is just one such case.
