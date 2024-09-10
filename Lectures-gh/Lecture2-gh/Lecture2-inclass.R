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
##        - make sure to inspect the age, gender, caste, religion, state & educ columns
##
##    b. use the mutate function to create new column for gender:
##        - gender.fac = as.factor(gender),
##        - check if it worked by calling the str() function
##
##    c. in a single pipe, include gender.fac in a new data frame called access2018.temp1, 
##       also create factors for caste, religion, state and education,
##       and exclude the columns for HHID and date
##       after creating access2018.temp1, print the first 5 observations
##
##    d. inspect caste.fac, gender.fac, religion.fac, education.fac and state.fac
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


## -----------------------------------------------------------------------------
## 3. Let's now look at expenditures per month for different groups in Uttar Pradesh
##
##    a. find the observation for the top monthly expenditure using the summarize() function, 
##        assign this to a new object called max_exp_obs1
##
##    b. find max monthly expenditures using the arrange function instead of summarize
##
##    c. use the filter function to subset for the observation with max monthly expenditure
##        (don't hardcode the max expenditure to filter on, refer to the max_exp_obs1 object from a),
##        store in new data frame max_exp_obs2,
##        confirm it worked
##
##    d. what is the age, gender, caste and religion of the top monthly spender in the sample?
##
##    e. list the age, gender and caste of the top 10 monthly spenders in the sample.
##
##    f. how many individuals spend more than the mean monthly expenditure amount of the sample?
## -----------------------------------------------------------------------------

# 3a. 
  max_exp_obs1 <- access_UP %>% 
    summarize(max_exp = max(month_exp)) 
  max_exp_obs1

  
# 3b. 
  access_UP %>%
    arrange(desc(month_exp)) %>%
    select(month_exp) %>%
    head(n = 1)

  
# 3c. 
# HINT: your condition needs to refer to the max monthly expenditures (month_exp)
# you created max_exp_obs1 as a data frame in part a,
# so in your filter() call refer to the value from max_exp_obs1 that you want to filter on
# this requires subsetting the appropriate element from that data frame
# (in this case just the max_exp column of the max_exp_obs1 data frame)
  
  max_exp_obs2 <- access_UP %>% 
                filter(month_exp == max_exp_obs1$max_exp) 
  
  #validate
    summary(max_exp_obs2$month_exp) 

    
# 3d.
  max_exp_obs2 %>%
    select(age, gender, caste) %>%
    head(n = 1)

  
# 3e.
  access_UP %>%
    arrange(desc(month_exp)) %>%
    select(age, gender, caste, month_exp) %>%
    head(n = 10)

  
# 3f.
  access_UP %>%
    filter(month_exp > mean(month_exp)) %>% 
    nrow()

  
## -----------------------------------------------------------------------------
## 4. Now, let's look at caste-based monthly expenditure gaps in Uttar Pradesh. 
##    [Note: Since this data has no earnings data, we are using monthly expenditure 
##    (month_exp) as a proxy for earnings.
##
##    a. use the filter function to subset observations belonging to the 'General' caste, 
##       assign to new data frame, access_UPgen,
##       sort in descending order of monthly expenditure
##       check if it worked
##
##    b. repeat part a, but this time, use the filter function to subset observations 
##       belonging to 'Reserved" castes: 'Other Backward Class', 'Scheduled Caste' or 'Scheduled Tribe', and
##       assign them to a new data frame called 'access_UPres'. 
##
##    c. use summarise to find mean, min & max monthly expenditure for the General 
##       category and Reserved category of castes, separately.
##       name each statistic appropriately (i.e. name each column in the 1-row table of stats)
##       what is the gap in mean monthly expenditure between the two groups? 
##
##    d. research suggests that people belonging to the 'General' caste category
##       own 65% of agricultural land in India (India Human Development Survey, 2020). 
##       they are also more likely to own land in a proportion that is much higher
##       than their share of the population. 
##       we will now use this data to compare 'General' versus 'Reserved' households
##       using the 'reserved' dummy variable to understand whether this is true. 
##       - use the 'table' and 'prop.table' functions to explore the total number 
##         of landowners by their caste. 
##       - here, 'landowners' are people whose primary source of income is 
##         agriculture on their own land. So, you can use the 'incsource.fac' 
##         variable to filter the 'access_UP' data frame for 'Agriculture (own land)'
##       - does the number of General and Reserved caste landowners seem 
##         proportional to the overall number of 'General' and 'Reserved' caste 
##         individuals in Uttar Pradesh's population (as calculated in Q2 b)? 
##
##    e. do differences in landowning explain the expenditure gap between General and Reserved castes?
##       i.e., does the General-Reserved expenditure gap persist among landowning families?
##       what is the gap in mean monthly expenditures of General vs Reserved category landowners? 
##
##    f. is there a gap between the mean monthly expenditures of General caste male 
##       and Reserved caste male landowners? what about the same, but for the female landowners? 
##
##    g. does educational attainment explain part of the expenditure gap 
##       between General and Reserved caste landowners?
##       what is gap between landowning General vs. Reserved household with a HS degree or more? 
## -----------------------------------------------------------------------------

# 4a. 
  access_UPgen <- access_UP %>% 
    filter(caste.fac == "General") %>%
    arrange(desc(month_exp))

  #validate
    summary(access_UPgen$caste.fac)
  

# 4b. 
  access_UPres <- access_UP %>% 
    filter(caste.fac %in% c("Other Backward Class", 
                            "Scheduled Caste",
                            "Scheduled Tribe"))%>%
    arrange(desc(month_exp))
  
  #validate
    summary(access_UPres$caste.fac)


# 4c.
  access_UPgen %>%
    summarise(avg_monthexp_gen = mean(month_exp), 
              min_monthexp_gen = min(month_exp), 
              max_monthexp_gen = max(month_exp),
              n_monthexp_gen = n())
  
  access_UPres %>%
    summarise(avg_monthexp_res = mean(month_exp), 
              min_monthexp_res = min(month_exp), 
              max_monthexp_res = max(month_exp),
              n_monthexp_res = n())
  
  mean(access_UPgen$month_exp) - mean(access_UPres$month_exp)
  #Ans: 1385.48

# 4d.
  table(access_UP$incsource.fac, access_UP$reserved)
  prop.table(table(access_UP$incsource.fac, access_UP$reserved)) %>% round(2)

# 4e. 
  access_UPgen_land <- access_UPgen %>% 
    filter(incsource.fac == "Agriculture (own land)") 
  
  access_UPres_land <- access_UPres %>% 
    filter(incsource.fac == "Agriculture (own land)") 
  
  gap_land <- mean(access_UPgen_land$month_exp) - mean(access_UPres_land$month_exp)
  gap_land 
  #Ans: 864.89

#-----
# BONUS: is this difference statistically significant? do a difference means t-test.
#         - write out the null and alternative hypotheses
#         - state the p-value of this test
  access_UP_land <- access_UP %>%
    group_by(reserved) %>%
    filter(incsource.fac == "Agriculture (own land)")
  
  ols1 <- lm(month_exp ~ reserved, data = access_UP_land)
  coeftest(ols1, vcov = vcovHC(ols, type="HC1", na.omit = TRUE)) #Diff is statistically significant at 1%
  rm(access_UP_land)
  rm(ols1)
#-----


# 4f. 
  access_UPgen_male <- access_UPgen_land %>%
    filter(gender == "Male")
  
  access_UPgen_fem <- access_UPgen_land %>%
    filter(gender == "Female")
  
  access_UPres_male <- access_UPres_land %>%
    filter(gender == "Male")
  
  access_UPres_fem <- access_UPres_land %>%
    filter(gender == "Female")
  
  gap_male <- mean(access_UPgen_male$month_exp) - mean(access_UPres_male$month_exp)
  gap_male #Ans: 838.74
  
  gap_fem <- mean(access_UPgen_fem$month_exp) - mean(access_UPres_fem$month_exp)
  gap_fem #Ans: 915.32

  
# 4g. 
#Note: First run the following code to create a dummy variable 'degree' with two levels: 
#"HS degree or above" and "Less than HS".

  access_UP <- access_UP %>% 
    mutate(degree = factor(if_else(education.fac %in% c("Grade 12", 
                                                         "Graduate or above"),
                                    "HS degree or above",
                                    "Less than HS")))
  
  #General v Reserved landowners who have degrees
  access_UPgen_hs <- access_UP %>%
    filter(reserved == "General", 
           incsource.fac == "Agriculture (own land)", 
           degree == "HS degree or above")
  
  access_UPres_hs <- access_UP %>%
    filter(reserved == "Reserved", 
           incsource.fac == "Agriculture (own land)", 
           degree == "HS degree or above")
  
  gap_hs_land <- mean(access_UPgen_hs$month_exp) - mean(access_UPres_hs$month_exp)
  gap_hs_land #Ans: 588.62
  