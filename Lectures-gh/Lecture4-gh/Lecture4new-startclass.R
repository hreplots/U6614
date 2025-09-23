################################################################################
##
## [ PROJ ] DSP IA7514 Assignment 4: Subway Fare Evasion Arrests: Exploring Racial Disparities
## [ FILE ] Lecture4new-FULL.r 
## [ AUTH ] < YOUR NAME >
## [ INIT ] < Sept. 23, 2025 >
##
################################################################################

##  RESEARCH QUESTION

##  Police can stop and ticket or arrest people for subway fare evasion.

##  Are there racial disparities in how the NYPD enforces subway fare evasion?

##  What is the relationship between station area poverty rates and arrest 
##  intensity, and how is this relationship moderated by station area racial
##  composition?


## -----------------------------------------------------------------------------
## 0. load libraries and confirm working directory
## -----------------------------------------------------------------------------

#install.packages("weights", dependencies = TRUE)
#install.packages("estimatr")
#install.packages("knitr")
#install.packages("ggpmisc")

library(tidyverse)
library(knitr)
library(estimatr)
library(weights)
library(ggpmisc)

getwd()


## -----------------------------------------------------------------------------
## 1. Aggregating to subway station-level arrest totals
##
##  a. load full set of cleaned arrest microdata (arrests.clean.rdata)
##
##  b. Using tidyverse functions, create a new data frame (`st_arrests`) 
##      that aggregates the microdata to station-level observations. 
##      The unit of analysis should be the station, with columns for:
##        - `st_id`, `loc2` and `total arrests`.
##
##  c. plot histogram of arrests 
##     briefly describe distribution of arrests across stations
## -----------------------------------------------------------------------------

# 1a.


# 1b.        

  
# 1c.



## -----------------------------------------------------------------------------
## 2. joining ridership and neighborhood demographics to arrest data
##
##  a. read in other station-level csv files (w/strings as factors) and inspect
##      - don't include inspection code in your rmd submission
##  
##  b. join both data frames to st_arrests & inspect results (store new df as st_joined)
##      - inspect results of join and describe any issues
##      - drop unnecessary columns from the ridership data
##      - group st_joined by both st_id and mta_name
##      - inspect but don't include inspection code in your rmd submission
##        - confirm you have 157 station-level observations
##
##  c. Print the top 10 stations by total arrest counts. 
##     - Only display st_id, mta_name, arrests_all, shareblack, povrt_all_2016
##     - Round percentages to 2 decimal points (do this going forward)
##
##  d) Compute arrest intensity and other explanatory variables for analysis.{-}
##      - Drop  observation for the Coney Island station, very briefly explain logic
##      - Create new column of data for the following:
##        + fare evasion arrest intensity: 
##            - arrperswipe_2016 = arrests per 100,000 ridership ('swipes')
##        + a dummy indicating if a station is high poverty: 
##            - highpov=1 if povrate > median povrate across all BK station areas
##        + a dummy for majority Black station areas: 
##            - nblack = 1 if shareblack > 0.5
##      - Coerce new dummy variables into factors with category labels
##      - Assign results to new data frame called `stations`
##      - Show top 10 stations by arrest intensity using `kable()` in `knitr` package
##
##  e) How do top 10 stations by arrest intensity compare to top 10 by arrest count?
## -----------------------------------------------------------------------------

# 2a. 
st_poverty <- read.csv("station_povdataclean_2016.csv", 
                       stringsAsFactors = TRUE) %>% 
  select(-nblack, -mta_name)
  
  # background on poverty data (from 2016 American Community Survey): 
  #  Source: https://usa.ipums.org/usa/acs.shtml
  #  each observation represents a "subway station area", defined as follows:
  #   - all census tracts w/geometric center within .5km of a station (see map)
  #  st_id is the unique identifier for each subway station area
  #  povrt_all_2016: % of adults in subway station area living below federal poverty level
  #  shareblack: share of adults in subway station area who identify as Black
  #  nblack: dummy variable = 1 if shareblack >= 50%, 0 otherwise
  
  
st_ridership <- read.csv("Subway Ridership by Station - BK.csv", 
                         stringsAsFactors = TRUE)
  # background on MTA ridership data:
  #  Source: http://web.mta.info/nyct/facts/ridership/ridership_sub_annual.htm
  #  each observation is a subway station w/a unique identifier (st_id)
  #  includes annual ridership (# of MetroCard swipes) at each station for 2011-16

  # make sure to inspect these new df's before joining them in 3b!


# 2b.

  # a vector of columns we don't need to keep, will use in 3b
  drop_vars <- c("swipes2011", "swipes2012", "swipes2013", "swipes2014", "swipes2015")

  
  
  
  
  # QUESTION: could we have used full_join or left_join instead of inner_join?

  
# 2c.
  st_joined %>% 
    arrange(desc(arrests_all)) %>% 
    select(st_id, mta_name, arrests_all, shareblack, povrt_all_2016) %>% 
    head(n = 10) 


# 2d.
  stations <- st_joined %>%
    mutate(arrperswipe = round(arrests_all / (swipes2016 / 100000), 2),
           highpov = as.numeric(povrt_all_2016 > median(st_joined$povrt_all_2016)),
           nblack = as.numeric(shareblack > .5),
           shareblack = round(shareblack, 2),
           povrt_all_2016 = round(povrt_all_2016, 2)) %>% 
    mutate(highpov = factor(highpov, 
                            levels = c(0, 1), 
                            labels = c("Not high poverty", "High poverty")),
           nblack  = factor(nblack, 
                            levels = c(0, 1), 
                            labels = c("Majority non-Black", "Majority Black"))) %>% 
    filter(st_id != 66)
  
  # some validation and inspection:
  
    # check if nblack recoding worked as intended

    
    # examine joint distribution of highpov and black

  
  
  # display top 10 stations by arrest intensity 
  stations %>% 
    arrange(desc(arrperswipe)) %>% 
    select(st_id, mta_name, arrperswipe, arrests_all, shareblack, povrt_all_2016, highpov, nblack) %>% 
    head(n = 10) %>% 
    kable() # kable offers better table formatting
 

# 2e.
  
  

## -----------------------------------------------------------------------------
## 3. Examine the relationship between arrest intensity and poverty rates
##
##  a) Show a scatterplot of arrest intensity vs. poverty rates,
##      - overlay the regression line you think best fits this relationship. 
##      - weight observations by ridership, and label your axes appropriately. 
##      - only show one plot with your preferred specification!**
##      - which regression specification do you prefer: linear or quadratic? 
##      - be clear about your logic and cite supporting statistical evidence
##      - interpret your preferred regression specification (carefully!). 
##      - remember to test for statistical significance where appropriate
##
##  b) Estimate and test difference in mean arrest intensity btw high/low poverty areas
##      - report difference and assess statistical significance
##      - weight observations by ridership
## ---------------------------------------------------------------------------

# 3a.
  


  # fit linear model w/station observations (can also add optional weights argument)


  # how to refer to specific regression results

  

  # add quadratic prediction line to scatter plot
  

  
  
  # fit quadratic OLS model 

    
  # Q: what's the appropriate hypothesis test when there is a quadratic term?

  
  summary(m_3a_q)  # ok we see the global F test results, how do we access?
  tidy(m_3a_q)     # clean df of regression results can be nice to work with
  
  m_3a_q$fstatistic # we have the appropriate F-stat and df, but not the p-value!
  
  # here's how to get the p-value for a global F-test
  pf(m_3a_q$fstatistic[1], 
     m_3a_q$fstatistic[2], 
     m_3a_q$fstatistic[3], 
     lower.tail = FALSE)
    
  
# 3b. calculate and test difference in means between high/low poverty stations
  
  # get difference in ridership-weighted means using dplyr


  # inference with t.test command and unequal variance (doesn't accept weights!)


  # regress arrest intensity on highpov dummy to implement diff in means test 
  # weighted by ridership with robust SEs

       

  # wtd.t.test function in the weights package accepts weights... but not robust SEs
  ?wtd.t.test
  stations_highpov <- stations %>% filter(highpov == "High poverty")
  stations_lowpov  <- stations %>% filter(highpov == "Not high poverty")
  wtd.t.test(stations_highpov$arrperswipe, 
             stations_lowpov$arrperswipe, 
             weight = stations_highpov$swipes2016, 
             weighty = stations_lowpov$swipes2016)
    # compare difference and SE of difference to previous approach


## -----------------------------------------------------------------------------
## 4. How does neighborhood racial composition moderate the relationship between 
##    poverty and arrest intensity? 
##    - examine the relationship btw arrest intensity & poverty, 
##        by Black/non-Black station area (nblack)
##
##  a.  show a table w/the mean arrest intensity for highpov x nblack groups
##      - make sure to calculate statistics weighted by ridership
##      HINT: use group_by() and summarise() 
##      BONUS: can you report this information in a 2x2 table?
##
##  b.  does the difference in mean arrest intensity btw high-pov Majority Black
##        and high-pov Majority non-Black stations appear to be explained by
##        differences in the mean pov rate?
##      
##      Step 1:
##      - calculate the difference in mean arrest intensity between high poverty
##        Majority Black and Majority Non-Black station areas
##      - make sure to calculate statistics weighted by ridership
##      - test whether this difference is statistically significant
##
##      Step 2:
##      - repeat above steps for the poverty rate instead of arrest intensity
##
##      Step 3:
##      - don't forget to answer the question above in your own words!
##    
##  c. Show a scatterplot of arrest intensity vs. poverty rates by majority
##      Black/non-Black (nblack) and interpret 
##      - use separate aesthetics for Black and non-Black station areas
##      - overlay the regression lines that you think best capture this relationship:
##        - show linear or quadratic (not both!)
##      - weight observations by ridership, and label your axes appropriately
##      - only show one plot with your preferred specification!
##      - interpret your preferred regression specification (carefully!)
##
##  d. Next let's let's think about how measurement error might impact results
##      from 4b. Do you think measurement error could bias your estimates
##      of neighborhood racial gaps in the effect of poverty on enforcement
##      intensity from 4b? Explain, carefully. Do you have any creative ideas
##      to address any concerns you have about bias due to measurement error? 
## -----------------------------------------------------------------------------
  
# 4a.


  
# 4b.
  
  # Approach #1: OLS

  
  
  # Approach #2: datasummary_balance()
  
  # show a diff in means table by highpov for Majority non-Black stations
  # only show the means of arrperswipe and povrt_all_2016
  # NOTE: if there's a column called weights, weighted stats will be calculated
  #   - we want to weight observations by ridership
  #   - so first let's create a new column called weights equal to swipes2016
  stations <- stations %>% 
    mutate(weights = swipes2016)
  
  datasummary_balance(~ nblack, 
                      fmt = 2, 
                      data = subset(stations, 
                                    highpov == "High poverty",
                                    select = c("nblack",
                                               "arrperswipe",
                                               "povrt_all_2016",
                                               "weights")))
  
  # how can we extract statistics from this object?
  # we know how to subset data frames, but this output is not a data frame!
  #   HINT: try passing the output from above into str()
  # luckily we can modify an argument to output this table as a data frame
  # then we can subset using the base R square bracket [] syntax
  
  t1 <- datasummary_balance(~ nblack,
                            fmt = 2,
                            output = 'data.frame',
                            data = subset(stations,
                                          highpov == "High poverty",
                                          select = c("nblack",
                                                     "arrperswipe",
                                                     "povrt_all_2016",
                                                     "weights")))
  
  # how to refer to elements of this data frame
  t1[1,6] # difference in mean arrest intensity
  t1[1,7] # SE of the difference in mean arrest intensity
  t1[2,6] # difference in mean poverty rate
  t1[2,7] # SE of the difference in mean poverty rate  
  

# 4c.
    
  # scatterplot by nblack - linear fit
 
  
  # quadratic fit
  
  

  # the above plots show the Sample Regression Function (predicted relationship)
  # how do we refer to regression coefficients and stats to interpret?

  # we can estimate separate models for Majority Black and non-Black stations
    
    
    
  # or we can estimate an interaction model, interacting nblack and the pov rate

    
  
# 4d.
    


## -----------------------------------------------------------------------------
## 5. Is the differential effect of poverty in majority Black station areas
##    explained by differences in crime?
##
##    One determinant of fare evasion enforcement is police presence:
##    when more police are present, the greater the chances they will encounter
##    fare evasion.
##
##    In the absence of data on police deployment across the subway system,
##    we can use the number of crimes as a proxy for police presence.
##
##  a. load nypd_criminalcomplaints_2016.csv as st_crime and join to stations
##      by st_id and mta_name
##
##  b. are there more crimes reported in high-poverty Majority Black station areas
##      than in high-poverty Majority non-Black station areas?
##    - report the difference in crimes and assess statistically significance
##
##  c. does the difference in crimes that you found in 5b explain the finding
##      from 4c that poverty has a stronger positive effect on arrest intensity 
##      in majority Black station areas than in majority non-Black station areas?
##      - start with your preferred specification from 4c, then control for the
##        the number of crimes and see if the conclusions change
##      - do your conclusions change if you consider different functional forms
##        for the relationship between crime and arrest intensity?
## -----------------------------------------------------------------------------
  
# 5a.
  
  
  
# 5b.


  
# 5c.
  

  
    
## -----------------------------------------------------------------------------
## 6. Summarize and interpret your findings with respect to racial disparities in 
##    subway fare evasion arrest intensity. Be very careful about how you frame
##    and justify any claims of racial bias; any such claims should be supported
##    by the analysis you present.
##    - Is there any additional analysis you'd like to do with the data at hand?
##    - Are there any key limitations to the data and/or analysis affecting your ability 
##       to examine racial disparities in enforcement?
##    - Is there any additional data you'd like to see that would help 
##       strengthen your analysis and interpretation?
##   For this question, try to be very specific and avoid vaguely worded concerns.
## -----------------------------------------------------------------------------
  
  