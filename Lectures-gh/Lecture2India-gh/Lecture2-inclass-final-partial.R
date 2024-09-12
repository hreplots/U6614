################################################################################
##
## [ PROJ ] < Caste-based expenditure gaps and the ACCESS 2018 Survey >
## [ FILE ] < Lecture2-inclass.R >
## [ INIT ] < Sept 05, 2024 >
##
################################################################################

## ---------------------------
## LIBRARIES
## ---------------------------

library(tidyverse)
library(lmtest)
library(sandwich)


## ---------------------------
## DIRECTORY PATHS
## ---------------------------

getwd()


## -----------------------------------------------------------------------------
## 1. load and inspect access2018 data: 
##    
##    a. inspect the data frame and data types for each column
##        - make sure to inspect the age, gender, caste, income source, state & educ columns
##
##    b. use the mutate function to create new column for gender:
##        - gender.fac = as.factor(gender),
##        - check if it worked by calling the str() function
##
##    c. in a single pipe, include gender.fac in a new data frame called access2018.temp1, 
##       also create factors for caste, income source, state and education,
##       and exclude the columns for HHID and date
##       after creating access2018.temp1, print the first 5 observations
##
##    d. inspect caste.fac, gender.fac, incsource.fac, education.fac and state.fac
##.      using the levels() function,
##       what package is the levels() function located in?
##
##    e. use filter() to only include rows only for the state "Uttar Pradesh",
##       store as a new object access_UP,
##       print the first 5 observations,
##       confirm your data only includes observations for Uttar Pradesh
##
##.   f. what is the female share of respondents in Uttar Pradesh? 
##       compare it to the female share of respondents in Bihar. 
##
##    g. remove the access2018.temp1 object from memory using the rm() function
## -----------------------------------------------------------------------------

# 1a. 

  load("access2018.RData")
  str(access2018)
  
  #View(access2018)
  #we can also inspect the data frame by double-clicking in the Environment tab
  #NOTE: DON'T INCLUDE View() IN YOUR R MARKDOWN SUBMISSION!

  summary(access2018$age)
  summary(access2018$gender) #summary is not very useful with character variables
  summary(access2018$caste) #summary is not very useful with character variables
  summary(access2018$education) #summary is not very useful with character variables
  summary(access2018$state) #summary is not very useful with character variables

  
# 1b.
  mutate(access2018, gender.fac = as.factor(gender)) 
  
  #note: the output is an object, we're just not assigning it to the environment
  #but we can put the entire operation within str() to inspect the output
  str(mutate(access2018, gender.fac = as.factor(gender))) 
  summary(mutate(access2018, gender.fac = as.factor(gender))$gender.fac) 
  
  
# 1c. best way (using a pipe)
  access.temp1 <- access2018 %>% 
    mutate(gender.fac = as.factor(gender),
          caste.fac = as.factor(caste),
          education.fac = as.factor(education), 
          state.fac = as.factor(state),
          incsource.fac = as.factor(incsource)) %>% 
    select(-HHID, -date) 

  #alternatively, you can initialize without a pipe
  access.temp1 <- mutate(access2018,
                    gender.fac = as.factor(gender), 
                    caste.fac = as.factor(caste),
                    education.fac = as.factor(education), 
                    state.fac = as.factor(state),
                    incsource.fac = as.factor(incsource)) %>%
    select(-HHID, -date)
  
  head(access.temp1, n = 5) 

  #some helpful syntax for later: 
  #subset the first row of access.temp1
  access.temp1[1,]
  
  #subset the cell in the first row, 4th column (i.e. first obs for age)
  access.temp1[1,5]


# 1d.
  levels(access.temp1$gender.fac)
  levels(access.temp1$caste.fac)
  levels(access.temp1$education.fac)
  levels(access.temp1$state.fac)
  levels(access.temp1$incsource.fac)

  ?levels   
  #note that levels is a base R function
  #so levels can't be used with columns using tidyverse syntax (i.e. not within a pipe)
  #make sure you understand why this won't work: cps.temp1 %>% levels(sex.fac)


# 1e.
  access_UP <- access.temp1 %>% 
    filter(state == "UTTAR PRADESH")
  
  head(access_UP, n = 5)
  
  #validate
    summary(access_UP$state.fac)

  
# 1f.

  #Share of female respondents in UP
  summary(access_UP$gender.fac)
  prop.table(table(access_UP$gender.fac))


  #Share of female respondents in Bihar
  access_bihar <- access.temp1 %>% 
    filter(state == "BIHAR")
  summary(access_bihar$gender.fac)
  prop.table(table(access_bihar$gender.fac))
  

# 1g.
  rm(access.temp1)
  rm(access_bihar)


## -----------------------------------------------------------------------------
## 2. Describe the access_UP data frame
##
##    a. what is the unit of observation (or unit of analysis)?
##
##    b. how many individuals are observed? 
##       how many of these are from 'Reserved' castes (i.e: Scheduled Caste, Scheduled Tribe, Other Backward Class")? 
##       how many are "General" caste members?
##       [Note: for this question, use the 'table' and 'prop.table' functions. 
##             'table' will give the number of observations belonging to each caste category.
##             'prop.table will give the proportion]
##
##    c. do you think the sample is representative of all households in India?
##       how would you describe the population represented by this sample?
##       is there more information you would like to see to assess the representativeness of the sample?
##
##    d. what is the average age of individuals in the sample? youngest and oldest person?
## -----------------------------------------------------------------------------

# 2b. Note: For this question, first run the following code to add a dummy 
# variable 'reserved' to the 'access_UP' dataframe. This variable has two levels— 
# 'Other Backward Class', 'Scheduled Caste' and 'Scheduled Tribe' belong to the 
# 'Reserved' level, while the rest are assigned to 'General'.
  access_UP <- access_UP %>% 
    mutate(reserved = factor(if_else(caste.fac %in% c("Other Backward Class", 
                                                      "Scheduled Caste",
                                                      "Scheduled Tribe"),
                                     "Reserved",
                                     "General")))

  #Number of total observations in access_UP
    nrow(access_UP)
    #Ans: 3002
  
  #Number of 'Reserved' and 'General' individuals
    table(access_UP$reserved)
    #Ans: 708 (G), 22294 (R)
  
  #Proportion of 'Reserved' and 'General' individuals
    prop.table(table(access_UP$reserved))
    #Ans: 23.5% (G), 76.4% (R)

  
# 2d. 
  summary(access_UP$age)
  #Avg age: 43.41 years, youngest: 18, oldest: 98.  
