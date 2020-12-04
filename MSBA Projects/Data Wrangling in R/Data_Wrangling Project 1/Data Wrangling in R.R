 ## 1. Create a dataframe containing two columns:
 # Method A and Method B. Populate the column Method A with 
 # values 1 through 9 and column Method B with values 10 through 18.

# create the dataframe
q1 <- data.frame(MethodA=c(1:9), MethodB=c(10:18))
 
# a.Using apply(), find the row means.
apply(q1, 1, mean)

# b.Using apply(), find the column means.
apply(q1, 2, mean)

# c.Using apply(), find the column sums.
apply(q1, 2, sum)

# d.Using apply(), sort the columns.
apply(q1, 2, sort)

# e.Using apply(), find the product in each row.
apply(q1, 1, prod)

# f.Print the columns of the dataset using the apply() function.
apply(q1, 2, print)

# g.Find the length of the dataset columns using the apply() function.
apply(q1, 2, length)

# h.Use apply() to find the range of numbers in each column of the dataset.
apply(q1, 2, range)
 

## 2.use the mtcars dataset available with R.
head(mtcars)

# a. Use the appropriate apply function to find the mean mileage per number of cylinders.
tapply(mtcars$mpg, mtcars$cyl, mean)

# b. Use appropriate apply function to find the mean weight per number of cylinders.
tapply(mtcars$wt, mtcars$cyl, mean)

# c. Use appropriate apply function to find the mean mileage per type of transmission 
# (auto or manual).
tapply(mtcars$disp, mtcars$am, mean)

# d. Use appropriate apply function to find the mean mileage per number of gears.
tapply(mtcars$disp, mtcars$gear, mean)


## 3.Using the dplyr package in R, perform the following actions:
library(dplyr)
library(tidyverse)

# a. Create a tibble object named tbl_cars from the cars dataset.
tbl_cars <- as_tibble(cars)
head(tbl_cars)

# b. Create a variable time that divides distance by speed
tbl2 <- mutate(tbl_cars, time = dist/speed)
head(tbl2)

# c. Create two new variables (speed_km and dist_km) that correspond to km instead of miles.
tbl3 <- mutate(tbl2, speed_km = speed*1.60934, dist_km = dist*1.60934)
head(tbl3)

# d. Create a time2 variable with the same formula using the variables from step c)
tbl4 <- mutate(tbl3, time2 = dist_km/speed_km)
head(tbl4)

# e. Create a car_build variable that is either (fast, average, slow) depending on whether 
# its speed (in miles per hour) is (0-10, 11-20, 21+). 
tbl5 <- mutate(tbl4, car_build=ifelse(speed<=10,"slow", ifelse(speed<=20,"average", 'fast')))
head(tbl5)

# f.Remove any distance that is less than or equal to 4 miles
tbl6 <- filter(tbl5, dist>4)
head(tbl6)

# g. Order the data in descending order by distance, then by increasing order by speed
tbl7<-arrange(tbl6, desc(dist), speed)
head(tbl7)

# h. Keep all columns and rename speed and dist to contain the _miles suffix, i.e. 
# speed_miles and dist_miles.
tbl8<-tbl7 %>%
  rename("speed_mile"="speed","dist_miles"="dist")
head(tbl8)

# i. Find the mean of the speed and 
# the distance in both km and miles grouped by car_build.
tbl9<-tbl8%>%
  group_by(car_build)%>%
  summarise(mean_km=mean(dist_km), mean_miles=mean(dist_miles))
head(tbl9)
  
# j. Re-program parts b through i above using 
# the chaining method (the pipe operator) in as a single program.
tbl_cars%>%
  # create new variables based on the existing
  mutate(time = dist/speed, 
         speed_km = speed*1.60934, dist_km = dist*1.60934,
         time2 = dist_km/speed_km,
         car_build=ifelse(speed<=10,"slow", ifelse(speed<=20,"average", 'fast')))%>%
  # remove distance <= 4 miles
  filter(dist>4)%>%
  # reorder the data
  arrange(desc(dist), speed)%>%
  # renames two columns
  rename("speed_mile"="speed","dist_miles"="dist")%>%
  # find the mean of the speed and 
  # the distance in both km and miles grouped by car_build.
  group_by(car_build)%>%
  summarise(mean_km=mean(dist_km), mean_miles=mean(dist_miles))


## 4. Refer to the dataset SupermarketTransactions available 
# in the form of csv file. Use the dataset and answer the 
# following questions. If you decide to use R, then use a 
# combination of apply and dplyr functions. If you decide to use Python, 
# then use whatever functions/packages you see fit. Regardless of whether 
# you use R or Python, please paste the results (not the code but 
# the results) for each part below.

# read the file
smt <- read.csv('SupermarketTransactions.csv')

# a. Show the distribution (frequency and percentage) of gender. 
smt1 <- smt %>%
  # grouping the F and M
  group_by(Gender) %>%
  # count the number of F and M
  summarise(n=n()) %>%
  # calculate the percentage
  mutate(freq = n/sum(n))
smt1
# b. Show the distribution (frequency and percentage) of shoppers by 
# marital status. 
smt2 <- smt %>%
  # grouping the married and the single
  group_by(MaritalStatus)%>%
  # count the number of each group
  summarise(n=n()) %>%
  # calculate the frequency
  mutate(freq = n/sum(n))

# c. Show the distribution of shoppers (frequency and percentage) for each income group. 
smt3 <- smt %>%
  # grouping the income brackets
  group_by(AnnualIncome)%>%
  # count the number of each group
  summarise(n=n()) %>%
  # calculate the frequency
  mutate(freq = n/sum(n))

# d. Show total revenue by state.
smt4 <- smt %>%
  # grouping by the state
  group_by(StateOrProvince)%>%
  # calculate the sum of revenue by state
  summarise(sum(Revenue))

# e. Show total revenue by product category
smt5 <- smt %>%
  # grouping by the product category
  group_by(ProductCategory)%>%
  # calculate the total revenue by each
  summarise(sum(Revenue))

# f. Show the number of transactions per state.
smt6 <- smt %>%
  # grouping by state
  group_by(StateOrProvince)%>%
  #calculate the numbre of transaction by state
  summarise(tranction=n())

# g. Proportion of shoppers who are single and own a home.
smt7 <- smt%>%
  # grouping by the marital and homeowning status
  group_by(MaritalStatus, Homeowner)%>%
  # filter out the single and homeowner
  filter(MaritalStatus  == "S" & Homeowner == "Y")%>%
  # calculate the number of this group
  summarise(n_sum=n())%>%
  # calculate the proportion of this group to all
  mutate(pro=n_sum/nrow(smt))


# h. Proportion of shoppers who have more than one child
smt8 <- smt%>%
  # grouping by numbre of children
  group_by(Children)%>%
  # filter out the shoppers with more than 1 child
  filter(Children>1)%>%
  # count the number of each group
  summarise(n_sum=n())%>%
  # calculate the proportion of each group to all
  mutate(pro=n_sum/nrow(smt))%>%
  # sum the proportion of shoppers with more than 1 child
  summarise(sum(pro))

# i. Compare total revenue from single versus married shoppers. 
# Do married shoppers spend more than those who are single?
smt9 <- smt %>%
  # grouping by the marital status
  group_by(MaritalStatus)%>%
  # find the total revenue from each group
  summarize_each(funs(sum), Revenue)
# CONCLUSION: No, married shoppers spend less than those who are single


# j. Total revenue for January and February 2016
install.packages("tibbletime")
library(tibbletime)
library(lubridate)

smt10 <- smt%>%
  # separating purchase date into month date year
  separate(PurchaseDate, into = c("Month", "Day","Year"), sep="/")%>%
  # grouping by Month
  group_by(Month)%>%
  # filtering out jan feb of 2016
  filter(Year=="2016" & (Month %in% 1:2)) %>%
  # Finding the total revenue for the two mnonths
  summarize(sum(Revenue))

smt10  
# k. Create a crosstab table showing the relationship 
# between gender and product family. Hint: A crosstabulation 
# table is one which has one variable in the form of rows and 
# another in the form of columns.
crosstab <- table(smt$ProductFamily, smt$Gender)
crosstab


