################################################################################
##
## [ PROJ ] < Caste-based expenditure gaps and the ACCESS 2018 Survey >
## [ FILE ] < Lecture2-startclass.R >
## [ INIT ] < Sept 05, 2024 >
##
################################################################################


## ---------------------------
## LIBRARIES
## ---------------------------



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



# 1b.



# 1c. 



# 1d.


?levels   
  #note that levels is a base R function
  #so levels can't be used with columns using tidyverse syntax (i.e. not within a pipe)
  #make sure you understand why this won't work: cps.temp1 %>% levels(sex.fac)


# 1e.



# 1f.

  #Share of female respondents in UP


  #Share of female respondents in Bihar



# 1g.



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


#Number of 'Reserved' and 'General' individuals


#Proportion of 'Reserved' and 'General' individuals



# 2d. 


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
##    d. what is the age, gender and caste of the top monthly spender in the sample?
##
##    e. list the age, gender and caste of the top 10 monthly spenders in the sample.
##
##    f. how many individuals spend more than the mean monthly expenditure amount of the sample?
## -----------------------------------------------------------------------------

# 3a. 



# 3b. 



# 3c. 
# HINT: your condition needs to refer to the max monthly expenditures (month_exp)
# you created max_exp_obs1 as a data frame in part a,
# so in your filter() call refer to the value from max_exp_obs1 that you want to filter on
# this requires subsetting the appropriate element from that data frame
# (in this case just the max_exp column of the max_exp_obs1 data frame)



# 3d.



# 3e.



# 3f.



## -----------------------------------------------------------------------------
## 4. Now, let's look at caste-based monthly expenditure gaps in Uttar Pradesh. 
##    [Note: Since this data has no earnings data, we are using monthly expenditure 
##    (month_exp) as a proxy for earnings.]
##
##    a. use the filter function and 'reserved' dummy variable to subset observations belonging to the 'General' caste, 
##       assign to new data frame, access_UPgen,
##       sort in descending order of monthly expenditure
##       check if it worked
##
##    b. repeat part a, but this time, use the filter function to subset observations 
##       belonging to 'Reserved" castes.
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



# 4b. 



# 4c.



# 4d.



# 4e. 



# 4f. 



# 4g. 
#Note: First run the following code to create a dummy variable 'degree' with two levels: 
#"HS degree or above" and "Less than HS".

access_UP <- access_UP %>% 
  mutate(degree = factor(if_else(education.fac %in% c("Grade 12", 
                                                      "Graduate or above"),
                                 "HS degree or above",
                                 "Less than HS")))

#General v Reserved landowners who have degrees


