# Homework 5
Yifan Zhong  
October 14, 2015  
#Load packages


```r
library(doBy)
```

```
## Loading required package: survival
```

Homework 5 Description

Load the Gross Domestic Product data for the 190 ranked countries in this data set: 
https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv 

Load the educational data from this data set: 
https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv 

Original data sources (if the links above don’t work): 
http://data.worldbank.org/data-catalog/GDP-ranking-table 
http://data.worldbank.org/data-catalog/ed-stats 

#download dataset

```r
gross_url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv "
education_url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
gross <- read.csv(gross_url,skip=4,na.strings=c("..",""),nrows=215)
gross <- gross[,c(1,2,4,5)]
colnames(gross) <- c("CountryCode","Ranking","Economy", "GDP")

education <- read.csv(education_url)
```

#Questions
#1	Match the data based on the country shortcode. How many of the IDs match?
189 data matched

#2	Sort the data frame in ascending order by GDP rank (so United States is last). What is the 13th country in the resulting data frame?
KNA : St. Kitts and Nevis   


```r
# Merge data
mergedata <- merge(gross, education, by="CountryCode",all=T)

#checking how many missing values
sapply(mergedata,function(var){sum(is.na(var))})
```

```
##                                       CountryCode 
##                                                 1 
##                                           Ranking 
##                                                49 
##                                           Economy 
##                                                25 
##                                               GDP 
##                                                49 
##                                         Long.Name 
##                                                 5 
##                                      Income.Group 
##                                                 5 
##                                            Region 
##                                                 5 
##                                  Lending.category 
##                                                 5 
##                                      Other.groups 
##                                                 5 
##                                     Currency.Unit 
##                                                 5 
##                          Latest.population.census 
##                                                 5 
##                           Latest.household.survey 
##                                                 5 
##                                     Special.Notes 
##                                                 5 
##                       National.accounts.base.year 
##                                                 5 
##                  National.accounts.reference.year 
##                                               202 
##                       System.of.National.Accounts 
##                                               154 
##                               SNA.price.valuation 
##                                                 5 
##                     Alternative.conversion.factor 
##                                                 5 
##                                   PPP.survey.year 
##                                                94 
##                 Balance.of.Payments.Manual.in.use 
##                                                 5 
##                    External.debt.Reporting.status 
##                                                 5 
##                                   System.of.trade 
##                                                 5 
##                     Government.Accounting.concept 
##                                                 5 
##                   IMF.data.dissemination.standard 
##                                                 5 
## Source.of.most.recent.Income.and.expenditure.data 
##                                                 5 
##                       Vital.registration.complete 
##                                                 5 
##                        Latest.agricultural.census 
##                                                 5 
##                            Latest.industrial.data 
##                                               144 
##                                 Latest.trade.data 
##                                                51 
##                      Latest.water.withdrawal.data 
##                                                87 
##                                     X2.alpha.code 
##                                                 6 
##                                         WB.2.code 
##                                                 6 
##                                        Table.Name 
##                                                 5 
##                                        Short.Name 
##                                                 5
```

```r
#delete observations missing GDP
data2 <- mergedata[!is.na(mergedata$GDP),]

#delete observations missing Income.Group
data2 <- data2[!is.na(data2$Income.Group),]

#checking missing values
sapply(data2,function(var){sum(is.na(var))})
```

```
##                                       CountryCode 
##                                                 0 
##                                           Ranking 
##                                                 0 
##                                           Economy 
##                                                 0 
##                                               GDP 
##                                                 0 
##                                         Long.Name 
##                                                 0 
##                                      Income.Group 
##                                                 0 
##                                            Region 
##                                                 0 
##                                  Lending.category 
##                                                 0 
##                                      Other.groups 
##                                                 0 
##                                     Currency.Unit 
##                                                 0 
##                          Latest.population.census 
##                                                 0 
##                           Latest.household.survey 
##                                                 0 
##                                     Special.Notes 
##                                                 0 
##                       National.accounts.base.year 
##                                                 0 
##                  National.accounts.reference.year 
##                                               155 
##                       System.of.National.Accounts 
##                                               106 
##                               SNA.price.valuation 
##                                                 0 
##                     Alternative.conversion.factor 
##                                                 0 
##                                   PPP.survey.year 
##                                                45 
##                 Balance.of.Payments.Manual.in.use 
##                                                 0 
##                    External.debt.Reporting.status 
##                                                 0 
##                                   System.of.trade 
##                                                 0 
##                     Government.Accounting.concept 
##                                                 0 
##                   IMF.data.dissemination.standard 
##                                                 0 
## Source.of.most.recent.Income.and.expenditure.data 
##                                                 0 
##                       Vital.registration.complete 
##                                                 0 
##                        Latest.agricultural.census 
##                                                 0 
##                            Latest.industrial.data 
##                                                94 
##                                 Latest.trade.data 
##                                                10 
##                      Latest.water.withdrawal.data 
##                                                42 
##                                     X2.alpha.code 
##                                                 1 
##                                         WB.2.code 
##                                                 1 
##                                        Table.Name 
##                                                 0 
##                                        Short.Name 
##                                                 0
```

```r
#checking how many obs matched
dim(data2)
```

```
## [1] 189  34
```

```r
#sort data by GDP
data2$GDP <- as.numeric(gsub(",","",data2$GDP))
data2 <- data2[order(data2$GDP,na.last=F),]

# The 13th country in the resulting data frame
data2[13,c("CountryCode","Long.Name","Ranking", "GDP")]
```

```
##     CountryCode           Long.Name Ranking GDP
## 103         KNA St. Kitts and Nevis     178 767
```

#3	What are the average GDP rankings for the "High income: OECD" and "High income: nonOECD" groups? 
       

1 High income: nonOECD  -----  91.91304

2 High income: OECD ----- 32.96667


```r
q3 <- data2[which(data2$Income.Group %in% c("High income: OECD","High income: nonOECD" )),]
summaryBy(Ranking~Income.Group, data=q3)
```

```
##           Income.Group Ranking.mean
## 1 High income: nonOECD     91.91304
## 2    High income: OECD     32.96667
```

#4	Cut the GDP ranking into 5 separate quantile groups. Make a table versus Income.Group. How many countries are Lower middle income but among the 38 nations with highest GDP?
5 countries are Lower middle income but among the 38 nations with highest GDP


```r
breakpoint <- quantile(data2$Ranking,probs=seq(0,1,length.out = 6))
data2$RankingGroup <- cut(data2$Ranking,breaks=breakpoint)
levels(data2$RankingGroup) <- c("first(top)", "second", "third", "fourth", "fifth")
table(data2$RankingGroup, data2$Income.Group)[,2:6]
```

```
##             
##              High income: nonOECD High income: OECD Low income
##   first(top)                    4                17          0
##   second                        5                10          1
##   third                         8                 1          9
##   fourth                        4                 1         16
##   fifth                         2                 0         11
##             
##              Lower middle income Upper middle income
##   first(top)                   5                  11
##   second                      13                   9
##   third                       11                   8
##   fourth                       9                   8
##   fifth                       16                   9
```

```r
#How many countries are Lower middle income but among the 38 nations with highest GDP?
sum(data2$Ranking<=38 & data2$Income.Group == "Lower middle income")
```

```
## [1] 5
```

5	If you encounter NAs in your analysis, please delete those values and continue the analysis with the non-missing values. However, please include code to count the number of missing values for each variable used in the analysis.


Deliverable: Markdown file uploaded to GitHub containing the following 
•	Code for downloading, tidying, and merging data in a R Markdown file
•	Code to answer the four questions above (plus the answers) in the same R Markdown file
•	The file must be readable in GitHub – 5 points off if I have to download the file to read it!

In order to submit your assignment, send me the link to the file in GitHub via e-mail.

Rubric (25 points total):
Assignment is submitted via a link to GitHub that results in a readable file (0 or 1)
I can run the code on my computer with no modifications, except setting the working directory (0 – doesn’t run, 1 – partially runs, 2 – completely runs).
Correct answers for each of the first four questions (5 points each, 20 points total).
Correct estimate of NAs (2 points total).

