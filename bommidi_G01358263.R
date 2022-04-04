{\rtf1\ansi\ansicpg1252\cocoartf2636
\cocoatextscaling0\cocoaplatform0{\fonttbl\f0\fswiss\fcharset0 Helvetica;\f1\froman\fcharset0 TimesNewRomanPSMT;}
{\colortbl;\red255\green255\blue255;\red0\green0\blue0;}
{\*\expandedcolortbl;;\cssrgb\c0\c0\c0;}
\margl1440\margr1440\vieww28600\viewh17440\viewkind0
\pard\tx720\tx1440\tx2160\tx2880\tx3600\tx4320\tx5040\tx5760\tx6480\tx7200\tx7920\tx8640\pardirnatural\partightenfactor0

\f0\fs24 \cf0 #Assignment - 9\
#Sohan Bommidi\
#G01358263\
\
setwd("/Users/sohanbommidi/Desktop/AIT580")\
\
rm(list=ls())\
\
data <- read.csv("/Users/sohanbommidi/Downloads/EmployeeAttrition.csv")\
\
\
# Hypothesis Testing \
#1. If the MonthlyIncome of Males is greater than Females\
p<-subset(data,Gender=="Male")\
q<-subset(data,Gender=="Female")\
\
p1<-x$MonthlyIncome  #monthly income of Males\
q1<-y$MonthlyIncome  #monthly income of females\
\
t.test(p1,q1,alternative = "greater",mu=0)\
# Because the P-value (0.889) is bigger than the significance level, we fail to reject the null hypothesis. \
#As a result, we are unable to back up the assertion.\
\
\
#2. If the WorkLifeBalance of Males is less than Females\
p2<-x$WorkLifeBalance\
q2<-y$WorkLifeBalance\
\
t.test(p2,q2,alternative = "less",mu=0)\
# Because the P-value (0.457) is bigger than the significance level, we fail to reject the null hypothesis. \
#As a result, we are unable to back up the assertion.\
\
\
\
#3. If the YearsAtCompany of Single is less than Married \
p3<-subset(data,MaritalStatus=="Single")\
q3<-subset(data,MaritalStatus=="Married")\
\
p4<-p3$YearsAtCompany\
q4<-q3$YearsAtCompany\
\
t.test(p4,q4,alternative = "less",mu=0)\
#Because the P-value (0.00497) is smaller than the significance level, we can reject the null hypothesis and support the claim.\
\
\
#4. If the EnvironmentalSatisfaction of Attrition=Yes is less than Attrition=No \
p5<-subset(data,Attrition=="Yes")\
q5<-subset(data,Attrition=="No")\
\
p6<-p5$EnvironmentSatisfaction\
q6<-q5$EnvironmentSatisfaction\
\
t.test(p6,q6,alternative = "less",mu=0)\
#Because the P-value (0.000104) is smaller than the significance level, we can reject the null hypothesis and support the claim.\
\
#5. If the MonthlyIncome of Manager is greater than Laboratory Technician (Hint: Use\
#JobRole to find Manager and Laboratory Technician)\
p7<-subset(data,JobRole=="Manager")\
q7<-subset(data,JobRole=="Laboratory Technician")\
\
p8<-p7$MonthlyIncome\
q8<-q7$MonthlyIncome\
\
t.test(p8,q8,alternative = "greater",mu=0)\
#Because the P-value (2.2e-16) is smaller than the significance level, we can reject the null hypothesis and support the claim.#6. If YearsAtCompany and DailyRate are correlated with each other
\f1\fs32 \expnd0\expndtw0\kerning0
\outl0\strokewidth0 \strokec2 \

\f0\fs24 \kerning1\expnd0\expndtw0 \outl0\strokewidth0 \
#6 If YearsAtCompany and DailyRate are correlated with each other\
\
p9<-data$YearsAtCompany\
q9<-data$DailyRate\
cor.test(p9,q9)\
# We fail to reject the null hypothesis since the P-value (0.1919) exceeds the significance level. \
#As a result, we can't prove the link between YearsAtCompany and DailyRate.\
\
#7.If YearsAtCompany and MonthlyIncome are correlated with each other \
\
cor.test(data$YearsAtCompany,data$MonthlyIncome)\
#We may reject the null hypothesis and support the claim because the P-value (2.2e-16) is less than the significance level. \
# YearsAtCompany and MonthlyIncome have an inverse relationship.\
\
#8. If YearsAtCompany varies depending on individual\'92s MaritalStatus \
\
rs_a<-aov(data$YearsAtCompany~factor(data$MaritalStatus))\
summary(rs_a)\
\pard\tx720\tx1440\tx2160\tx2880\tx3600\tx4320\tx5040\tx5760\tx6480\tx7200\tx7920\tx8640\pardirnatural\partightenfactor0
\cf0 # Because the P-value is 0.0247 < 0.5 , we can reject the null hypothesis. \
\pard\tx720\tx1440\tx2160\tx2880\tx3600\tx4320\tx5040\tx5760\tx6480\tx7200\tx7920\tx8640\pardirnatural\partightenfactor0
\cf0 #As a result, YearsAtCompany fluctuates according to each MaritalStatus.\
\
#9. If MonthlyIncome varies depending on individual\'92s PerformanceRating\
\
rs_b<-aov(data$MonthlyIncome~factor(data$PerformanceRating))\
summary(rs_b)\
\pard\tx720\tx1440\tx2160\tx2880\tx3600\tx4320\tx5040\tx5760\tx6480\tx7200\tx7920\tx8640\pardirnatural\partightenfactor0
\cf0 # Because the P-value is 0.512 > 0.5, we cannot reject the null hypothesis. \
\pard\tx720\tx1440\tx2160\tx2880\tx3600\tx4320\tx5040\tx5760\tx6480\tx7200\tx7920\tx8640\pardirnatural\partightenfactor0
\cf0 #As a result, we cannot substantiate the assertion that MonthlyIncome fluctuates based on individual PerformanceRating.\
\
\
 #10. If MonthlyIncome varies depending on individual\'92s WorkLifeBalance \
\
rs_c<-aov(data$MonthlyIncome~factor(data$WorkLifeBalance))\
summary(rs_c)\
# Because the P-value is 0.607 > 0.5, we cannot reject the null hypothesis. \
#As a result, we cannot substantiate the claim that MonthlyIncome fluctuates according to individual WorkLifeBalance.\
\
\
\
}