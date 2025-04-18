################################################################################
##
## [ PROJ ] < Assessing gender wage gaps using the Current Population Survey >
## [ FILE ] < Lecture2-inclass-FULL.R >
## [ INIT ] < Sep 14, 2022 >
##
################################################################################

## ---------------------------
## libraries
## ---------------------------

library(tidyverse)

## ---------------------------
## directory paths
## ---------------------------
getwd()

## -----------------------------------------------------------------------------
## 1. load and inspect CPS data: 
##    
##    a. inspect the data frame and data types for each column
##          - make sure to inspect the age, sex, race, college columns
##
##    b. use the mutate function to create new column for sex:
##        sex.fac = as.factor(sex),
##        check if it worked by calling the str() function
##
##    c. include sex.fac in a new data frame called cps.temp1, 
##       also create factors for race and college education,
##        use a pipe to exclude the columns for serial, ind
##        after creating cps.temp1, print the first 5 observations
##
##    d. inspect race.fac, sex.fac, and college.fac using the levels() function,
##        what package is the levels() function located in?
##
##    e. use filter() to only include rows only for June 2022,
##        store as a new object cps_2022,
##        print the first 5 observations,
##        confirm your data only includes observations for 2022
##
##    f. remove the cps.temp1 object from memory using the rm() function
## -----------------------------------------------------------------------------

# load the CPS data frame and store as an object 'cps'
  cps <- read.csv("cps_june_22-23.csv")
# should we explore why there are so many NA values?
  cps <- na.omit(cps) #remove all observations with NA values
  
  
# 1a. 
  str(cps)
  View(cps)
  #we can also inspect the data frame by double-clicking in the Environment tab
  
  summary(cps$age)
  summary(cps$sex) #summary is not very useful with character variables
  summary(cps$race) 
  summary(cps$college)  
  
  
# 1b.
  mutate(cps, sex.fac = as.factor(sex)) # note: here we're not storing this result in memory
  str(mutate(cps, sex.fac = as.factor(sex))) # you can put the entire operation within str()
  
  
# 1c.
  cps.temp1 <- cps %>% 
    mutate(sex.fac = as.factor(sex),
           race.fac = as.factor(race),
           college.fac = as.factor(college)) %>% 
    select(-serial, -ind) 
  
  #alternatively:
  cps.temp1 <- mutate(cps, 
                      sex.fac = as.factor(sex), 
                      race.fac = as.factor(race),
                      college.fac = as.factor(college)) %>%
    select(-serial, -ind)
  
  head(cps.temp1, n = 5) 
  
  #some helpful syntax for later: 
  #subset the first row of cps.temp1
  cps.temp1[1,]
  #subset the cell in the first row, 4th column (i.e. first obs for age)
  cps.temp1[1,4]
  
  
# 1d.
  
  levels(cps.temp1$sex.fac)
  levels(cps.temp1$race.fac)
  levels(cps.temp1$college.fac)
  
  ?levels   
  #note that levels is a base R function
  #so levels cannot be used with columns using the tidyverse language
  #make sure you understand why this won't work: cps.temp1 %>% levels(sex.fac)
  #Documentation: https://cps.ipums.org/cps-action/variables/sex
  
  
# 1e.
  
  cps_2022 <- cps.temp1 %>% 
    filter(year==2022)
  
  head(cps_2022, n = 5)
  
  summary(cps_2022$year)
  summary(cps_2022$month)
  
  
# 1f.
  
  rm(cps.temp1)

  
## -----------------------------------------------------------------------------
## 2. Describe the cps_2022 data frame
##
##    a. what is the unit of observation? 
##
##    b. how many individuals are observed? from how many households?
##
##    c. what is the average age of individuals in the sample? youngest and oldest person?
## -----------------------------------------------------------------------------
  
# 2a.
  str(cps_2022)
  view(cps_2022)
  #NOTE: don't include all of your inspection commands in your R Markdown submission
  #      I've included view() as a reminder, but shouldn't be in your submission
  #      neither should clunky str() output, be selective about the output you show!
  
  
# 2b. HINT: we need to calculate a statistic for a given column of data
  ?summarise
  summarise(cps_2022, n_distinct(personid))
  summarise(cps_2022, n_distinct(hhid))
  
  #summarise is in the dplyr package which is part of the tidyverse
  cps_2022 %>% 
    summarise(num_persons = n_distinct(personid),
              num_households = n_distinct(hhid))
  
  #short way: use in your .rmd file to reference code to answer questions
  n_distinct(cps_2022$personid)
  n_distinct(cps_2022$hhid)
  
  
# 2c. 
  #use summarise with multiple arguments, one for each statistic
  #try ?summarise to find the syntax for different summary statistics
  
  cps_2022 %>% 
    summarise(avg_age = mean(age),
              min_age = min(age),
              max_age = max(age) )

  
## -----------------------------------------------------------------------------
## 3. Let's now look at earnings per week for different groups in June 2022
##
##    a. find the observation for the top weekly earnings using the summarise() function, 
##        assign this to a new object called max_earnings
##
##    b. find max weekly earnings using the arrange function instead of summarise
##
##    c. use the filter function to subset for the observation with max weekly earnings
##        (don't hardcode the max earnings to filter on, refer to the max_earnings object from a),
##        store in new data frame cps_max_earn,
##        confirm it worked
##
##    d. what is the age, sex, and race of the top weekly earner in the sample?
##
##    e. list the age, sex, and race of the top 10 weekly earners in the sample.
##
##    f. how many individuals earned more than $2000 in weekly earnings?
## -----------------------------------------------------------------------------
  
# 3a. 
  cps_2022 %>% summarise(max_earnings = max(earnweek))
  #Documentation: https://cps.ipums.org/cps-action/variables/EARNWEEK#codes_section
  
  #assign to object
  max_earnings <- cps_2022 %>% 
    summarise(max_earnings = max(earnweek))
  max_earnings
    
# 3b. 
  cps_2022 %>% 
    arrange(desc(earnweek)) %>% 
    select(earnweek) %>% 
    head(n = 1)
  
  
# 3c. 
  
  #HINT: your condition needs to refer to the max weekly earnings.
  # you created max_earnings as a data frame in part a,
  # so in your filter() call refer to the value from max_earnings that you want to filter on
  # this requires subsetting the appropriate element from that data frame
  # (in this case just the 1st row of a 1-row data frame).
  # See Lecture2.1/Section 4.2 for examples of how to subset matrix/df elements
  
  cps_max_earn <- cps_2022 %>% filter(earnweek == max_earnings[1,])
  
  
# 3d. 
  cps_max_earn %>% 
    select(age,sex,race) %>% 
    head(n = 1)
  
  #obviously better to refer to column names when you know them as we do here
  #alternatively, you could refer to the elements you want within the cps_max_earn object
  #by calling the 4th, 5th, and 6th columns from the first row
  cps_max_earn[1,4:6]
  
  
# 3e.
   
df1 <-  cps_2022 %>% 
    arrange(desc(earnweek)) %>% 
    select(age,sex,race, earnweek)

  
# 3f.
  
  #use the nrow() function to return the number of observations
  cps_2022 %>% 
    filter(earnweek > 2000) %>% 
    nrow()

  
## -----------------------------------------------------------------------------
## 4. Let's look at wage gaps between males and females:
##
##    a. use the filter function to subset observations for males, 
##        assign to new data frame, cps_2022_male,
##        sort in descending order of weekly earnings
##        check if it worked
##
##    b. repeat part a for females and create a new data frame, cps_2022_female
##
##    c. use summarise to find mean, min & max for males and females, separately
##        name each statistic appropriately (i.e. name each column in the 1-row table of stats)
##        what is the gender gap in mean weekly earnings?
##
##    d. what is the wage gap in weekly earnings between white males and Black females?
##
##    e. what is the wage gap between college educated white males and college educated
##       Black females?
## -----------------------------------------------------------------------------
  
# 4a. 
  
  #HINT: try levels(cps_2022$sex.fac) function to get the factor labels to filter on
  
  cps_2022_male <- cps_2022 %>% 
    filter(sex.fac == "Male") %>% 
    arrange(desc(earnweek)) 

  
# 4b.
  cps_2022_female <- cps_2022 %>% 
    filter(sex.fac == "Female") %>% 
    arrange(desc(earnweek)) 

  
# 4c.

  #HINT: use the newly created data frames for males and females to generate stats
  
  cps_2022_male %>% 
    summarise(mean_earnings_male = mean(earnweek),
              min_earnings_male = min(earnweek),
              max_earnings_male = max(earnweek) )
    
  cps_2022_female %>% 
    summarise(mean_earnings_female = mean(earnweek),
              min_earnings_female = min(earnweek),
              max_earnings_female = max(earnweek) ) 
  
  #calculate gender gap in weekly earnings:
  #HINT: use base R to calculate the wage gap directly in your .rmd file
  mean(cps_2022_male$earnweek) - mean(cps_2022_female$earnweek)
  
  
# 4d. 
  
  #HINT: use levels() again to get the factor labels
  
  #create object w/observations for white males
  cps_2022_wh_male <-  cps_2022_male %>% 
    filter(race.fac == "White") 
  
  #create object w/observations for Black females
  cps_2022_bl_female <-  cps_2022_female %>% 
    filter(race.fac == "Black") 
  
  #calculate wage gap between white males and Black females
  mean(cps_2022_wh_male$earnweek) - mean(cps_2022_bl_female$earnweek)
  
  #Q: do you see any potential issues with the demographic groups we just used?
  

# 4e. 
  
  #create object w/observations for white college educated males
  cps_2022_wh_male_college <- cps_2022_male %>% 
    filter(college.fac == "College degree" & race.fac == "White")

  #create object w/observations for Black college educated females
  cps_2022_bl_female_college <- cps_2022_female %>% 
    filter(college.fac == "College degree" & race.fac == "Black")
  
  #calculate wage gap
  mean(cps_2022_wh_male_college$earnweek) - mean(cps_2022_bl_female_college$earnweek)

#NOTE: above exercises are done w/weekly earnings, but can easily be converted to hourly wages
  
  