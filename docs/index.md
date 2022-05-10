---
title: "DATA ANALYSIS OF COLLEGE SCORECARD"
permalink: /
layout: default
---

## Breif Introduction 

This project aims to conduct research on the US Education System from the 2018 - 2019 academic year. Our analysis will be edited. In that case, we will follow the question and answer format. First we will ask a question First, then we will use Python and its Data Processing Libraries to create an Intuitive Visualizations and statistical Summary that attempts to answer our question. Initially we will stick to answering questions about one variable at a time, followed by increasing the variables finally expanded our perspective to consider the interaction between three or more variables.

## Nature of the Data Curation

* The Data used here was collected under the College score Card Project by the U.S. Department of Education.The College score Card is designed to raise awareness, place power in the hands of students, families and college selectors for those who improve college quality to see how well the different schools help their students. This study provides data to assist students and families in the costs and outcomes of college as they measure the transactions of different colleges, calculating their needs and academic policies.
* Major advantage of college scorecard is they are better than the state data in that they provide earnings after a longer time period: six to 10 years, far more representative of lifetime earnings than one quarter. They also capture earnings from students who leave the state, providing more complete On the plus side, the College Scorecard data has a sample size that is about 100 times larger and covers an additional 4000 colleges, including many community colleges with no other data. Thus, the scorecard is likely to be more accurate than any other data sources, while filling major gaps in coverage.
* On the other hand, the College Scorecard earnings data has a major weakness. It only applies to students who received federal aid. Those students are more economically disadvantaged than their peers. As per the U.S. Department of Education, fine percent of all undergraduate students received student aid, a very impressive sample size superior to anything else available. Yet, students from the lowest family income are more than twice as likely to receive federal aid as students in the highest of earnings. Since family earnings strongly predict student earnings after graduation on the biases the data.

* Another serious problem with the scorecard data is inaccuracy with respect to schools with branch campuses, because earnings data are only reported at the system level, not for each distinct campus. Of all colleges with earnings data, 8 percent of students across 31 percent of campuses attend what appears to be either a branch campus or at least an institution that shares the same six-digit Office of Post-Secondary Education identification number (OPEID), analogous to a company identifier in that a business may have multiple physical establishments in different locations. This problem also affects federal default rate data and has unclear implications for Payscale, which may be subject to various errors related to the assignment of earnings to the wrong campuses.

* There are minor privacy issues like the data of the different universities like emailed and contact numbers of students are published online and can be used for spam by anyone. Secondly the quality of data is very poor and most of the data is inaccurate  the data set consist many useless variables which are repeated without any point , so this data set should be preprocessed before analysis.


## Questions
1. Number of colleges controlled by different types?
2. What are the costs of attendance for different types of control colleges?
3. What are the top 20 colleges with high cost of attendance?
4. What is the distribution of Net Tuition fees of colleges in US?
5. What is the tuition fee distribution for different type of control colleges?
6. What are the top 20 colleges with high tuition fee?
7. What are the top 20 states with most number of colleges?
8. What are the colleges with most number of undergraduate students?
9. What are the Admission rates of all campuses?
10.	What are the most Competitive colleges?
11.	What is the distribution of number of branches per college?
12. What are the top ten colleges with high number of branches?
13. What are the Distrubution of Degrees?
14. What is good score of SAT and ACT?
15. what is Corelation bewteen 75% score of SAT and ACT?
16. Does Colleges with low Admission rate accepts students with high SAT scores?
17. what is Distribution of percentage of students who receive federal loans?
18. Students from which type of control colleges most likely to recieve loan?
19. what are earnings of faculty in diatance only colleges vs on campus and distance colleges?
20. What is the distrubution of admission rate less than 50% vs tution fee less than average ?

## Requirements and Resources needed
* Softwares : Google collabrator, Jupyter Notebook
* The raw data comprises of 1977 columns. The structure of the data violates the definition of Tidy Data according to Hadely Wickham's Paper on Tidy Data [here](https://www.jstatsoft.org/article/view/v059i10)
* Due to this reason, many columns are not variable names; instead, they are possible values. This drastically increases the number of columns in the dataset. In short, the number of variables is far fewer than the number of columns in the dataset.

## Descriptive Analysis
* Dataset has 6806 entries and 1986 columns which gives us the information about the variuos variable. Being it has many columns we have taken limited variable for the analysis.

  ![image](https://user-images.githubusercontent.com/67438038/166414805-ac53b7bc-be34-438f-9370-95c2c6ec0499.png)
* Columns UNITID, INSTNM, CITY, STABBR, NUMBRANCH, HIGHDEG, PREDDEG, CONTROL, DISTANCEONLY, TUITFTE, AVGFACSAL, ADM_RATE_ALL, SATVR25, SATVR75, SATMT25, SATMT75, ACTCM25, ACTCM75, UGDS,UG25ABV, PCTFLOAN, COSTT4_A, ADM_RATE are selected for the analysis. Refer the define Termss section to know more about the columns and their metadata.
* Descriptive statistics
![image](https://user-images.githubusercontent.com/67438038/166419934-8e93da5b-9a80-4ca0-8e7e-72ad36956ed1.png)

## Result and Findings

### 1. Number of colleges controlled by different types?
![image](https://user-images.githubusercontent.com/67438038/166413839-5777cd52-d8de-42e4-8f51-3a2e84168731.png)
* From ablove grpah we can analyse that the colleges run by the private instutions for profit are very high.

### 2. What are the costs of attendance for different types of control colleges?
![image](https://user-images.githubusercontent.com/67438038/166223635-c3b4403d-5433-45cb-9ddb-c3961552de17.png)
* From the above scatter plot we can notice that attending the public college way less than attending the prive colleges.

### 3. What are the top 20 colleges with high cost of attendance?
![image](https://user-images.githubusercontent.com/67438038/166223679-697e8d37-279e-49f0-9563-44b9db8c7e20.png)

### 4.What is the distribution of Net Tuition fees of colleges in US?
![image](https://user-images.githubusercontent.com/67438038/166223720-3c4accd1-4224-453d-b921-c2ec0025ca53.png)
* Most number of colleges are having below 20000 for a full time student.

### 5.What is the tuition fee distribution for different type of control colleges?
![image](https://user-images.githubusercontent.com/67438038/166413956-37fbb43e-15ae-4124-bfe1-67bc355d9d34.png)
* Net tution fee for a full student for different type of control of colleges doesn't vary much and all colleges fall bewlow 20000.

### 6.What are the top 20 colleges with high tuition fee?
![image](https://user-images.githubusercontent.com/67438038/166223826-73738376-76ad-450e-a812-ffde70333312.png)

### 7.What are the top 20 states with most number of colleges?
![image](https://user-images.githubusercontent.com/67438038/166223874-a18483af-d529-4690-977d-08f6ca0917cb.png)

### 8.What are the top 20 states with most number of colleges?
![image](https://user-images.githubusercontent.com/67438038/166223925-91016f47-8ba5-484b-ab3b-cc373bc47117.png)

### 9. What are the Admission rates of all campuses?
![image](https://user-images.githubusercontent.com/67438038/166223948-a244854a-c3d6-4db4-8999-d048e9a33313.png)
* From above grph all colleges are having the high acceptance rate

### 10.What are the most Competitive colleges?
![image](https://user-images.githubusercontent.com/67438038/166224000-67475628-1d43-4577-a869-bf2828ea5f79.png)
* we can analyse from the graph standford is most competitive college with only 4 % acceptance rate.

## 11.What is the distribution of number of branches per college?
![image](https://user-images.githubusercontent.com/67438038/166224045-d79b315f-ebb0-4a36-a3c9-3850c95e5dac.png)
* Most of the colleges are having only 1 branch. only few are having more than 1 brannch

### 12.What are the top ten colleges with high number of branches?
![image](https://user-images.githubusercontent.com/67438038/166224133-c3d1cc7f-3688-42d7-b5a8-a9bd4374421f.png)
* From above graphs we can see that all Strayer universities are having the more than 70 branches.
```
x = df.query('NUMBRANCH > 70').groupby("STABBR").count().NUMBRANCH.sort_values(ascending=False)
x.head(10)
```
![image](https://user-images.githubusercontent.com/67438038/166224822-45bc837a-b499-4c42-b2f9-de9cfbb2cb55.png)
* Above is the list of states with number of colleges where the number of brnaches are more than 70.

### 13. What are the Distrubution of Degrees?
![image](https://user-images.githubusercontent.com/67438038/166414112-9d1b876b-5d48-41e1-9690-b6b4b30895c0.png)
* A Certificate is the most predominantly conferred award. It is clear that most institutes that have their predominant award as a Certificate, also have their Highest award as the same

### 14. What is good score of SAT and ACT?
![image](https://user-images.githubusercontent.com/67438038/166414150-548a24d2-c136-4237-85d4-df7f651d4f33.png)
![image](https://user-images.githubusercontent.com/67438038/166414160-e9bc345f-ee20-4317-ab34-eb8689c50fd4.png)
* Most universities have their 75th Percentile for both Reading and Math fall around 600.
* Similarly most universities have their 25th Percentile fall around 500.
* From the observations it can be said that a good ACT Composite score is anywhere between 20 and 26.
 
### 15. what is Corelation bewteen 75% score of SAT and ACT?
![image](https://user-images.githubusercontent.com/67438038/166414193-bbb0b725-4b2b-48bc-b17b-0e1cf3e2e54a.png)
* We can say that if a certain institute has a high 75th Percentile of SAT, it is very likely that the 75th percentile for ACT also will be high as well, viceversa.

### 16. Does Colleges with low Admission rate accepts students with high SAT scores?
![image](https://user-images.githubusercontent.com/67438038/166414246-708b9cf5-201a-493a-a48d-be50d3e90630.png)
* The light datapoints on the upper right of the scatter plot represent the most selective institutes in the US. These institutes require higher SAT scores but even then have a very low admission rate

### 17. what is Distribution of percentage of students who receive federal loans?
![image](https://user-images.githubusercontent.com/67438038/166414303-9c7448d5-9f05-44e6-9ef9-26a9a73076c9.png)
* From above graph we can notice that almost 700 institues students doesn't recieve any federal Loan.

### 18. Students from which type of control colleges most likely to recieve loan?
![image](https://user-images.githubusercontent.com/67438038/166414355-730a0784-76ec-4d10-83dd-8d2eb671484c.png)
* Students from Public Colleges are less likely to receive loans. A large fraction of students from these public collges do not receive a federal loan.

### 19.what are earnings of faculty in diatance only colleges vs on campus and distance colleges?
![image](https://user-images.githubusercontent.com/67438038/166414391-6a16bc0e-28a9-42be-ae19-72b3284abb82.png)
* we can notice that average median salary of disatnceonly college is 2000 dollar less than on cmapus and Distance.


### 20. What is the distrubution of admission rate less than 50% vs tution fee less than average ?
![image](https://user-images.githubusercontent.com/67438038/166429721-19b087f8-a128-4a03-8a16-24be0f9f117b.png)

### Conclusion of findings
* State of california has most number of Institues.
* Even though the large number of enrollments happened in Public Colleges, Private For-Profit colleges are the common in the US. There are about 24% more Private For-Profits than there are Public or Private Nonprofit Schools.
* Certificate is the most common degreee in US followed by bachelor's.`
* Average salary of distance only is less compared to oncampus and disatnce.
* Most of colleges in US doesn;t have many branches.
* From the data and above graphs we can say that  good SAT score falls in the range 500 - 600 for both Reading and Math while a good ACT Composite score can be anywhere between 20 and 26.
* Colleges that have a high admission rate have lower requirements on test scores.

## Limitations
*  Analysis we have chosen to study the data for the academic year 2018 - 2019. Some of the patterns that we saw here may not exist for other years.
* As per the dataset only 14% of colleges have reported their data on test score, this puts us in limitation where answering the question admission rate on basis of test score.
* Cost of attendance, Tution fee and salary eraning these analysis are done on average.
* For the entire data for academic year 2018 - 2019, we have chosen a limited number of data fields to analyze. This analysis is in not the complete analysis of the U. S. Educational system. 

## References
* [https://www.unigo.com/get-to-college/college-search/is-college-scorecard-missing-the-point](https://www.unigo.com/get-to-college/college-search/is-college-scorecard-missing-the-point)
* [https://collegescorecard.ed.gov/assets/InstitutionDataDocumentation.pdf](https://collegescorecard.ed.gov/assets/InstitutionDataDocumentation.pdf)
* [https://anson.ucdavis.edu/~jsharpna/DSBook/unit1/cost_of_uni.html](https://anson.ucdavis.edu/~jsharpna/DSBook/unit1/cost_of_uni.html)
* [https://catalog.data.gov/dataset/college-scorecard](https://catalog.data.gov/dataset/college-scorecard)
* [https://collegescorecard.ed.gov/data/documentation/ ](https://collegescorecard.ed.gov/data/documentation/ )
* [https://seaborn.pydata.org/](https://seaborn.pydata.org/)
* [https://matplotlib.org/](https://matplotlib.org/)

## Define Terms
* UNITID:
Data files are provided at the UNITID level, which is the unique identification number assigned to postsecondary institutions as surveyed through IPEDS - Integrated Postsecondary Education Data System.
* INSTNM:
The institution’s name, as reported in IPEDS.
* CITY, STABBR:
City and state as reported in IPEDS.
* NUMBRANCH:
This variable identifies the number of branch campuses at that institution.
* HIGHDEG:
Highest award identifies the highest award level conferred at the institution.
* PREDDEG:
Predominant undergraduate award identifies the type of award that the institution primarily confers; for instance, an institution that awards 40 percent bachelor’s degrees, 30 percent associate degrees, and 30 percent certificate programs would be classified as predominantly bachelor’s degree awarding.
* CONTROL:
This element is reported directly to IPEDS, and identifies whether the institution’s governance structure is public, private nonprofit, or private for-profit.
* DISTANCEONLY:
Institutions are identified as distance education-only if all their programs are available only via distance education.
* TUITFTE:
The net tuition revenue per full-time equivalent (FTE) student uses tuition revenue minus discounts and allowances, and divides that by the number of FTE undergraduate and graduate students.
* AVGFACSAL:
The average faculty salary produces the average faculty salary per month, by dividing the total salary outlays by the number of months worked for all full-time, nonmedical instructional staff.
* ADM_RATE_ALL:
Colleges report to IPEDS their Fall admissions rate, defined as the number of admitted undergraduates divided by the number of undergraduates who applied. ADM_RATE_ALL represents the admissions rate across all campuses, defined as the total number of admitted undergraduates across all branches divided by the total number of undergraduates who applied across all branches.
* SATVR25, SATVR75, SATMT25, SATMT75, ACTCM25, ACTCM75:
The files include the 25th and 75th percentiles of SAT reading (SATVR for _25 and _75), writing (SATWR for _25 and _75), math (SATMT* for _25 and _75)
* COSTT4_A
Cost of attendance for college


## Team Members
### *1. Lahari Pampati - G01350730*
### *2. Bommidi Sohan  - G01358263*
