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
#install.packages('modelsummary')

library(tidyverse)
library(knitr)
library(estimatr)
library(weights)
library(ggpmisc)
library(modelsummary)

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
load("arrests.clean.RData")


# 1b.        
st_arrests <- arrests.clean %>% 
  group_by(st_id, loc2) %>% 
  summarise(arrests_all = n() ) %>% 
  arrange(desc(arrests_all)) %>% 
  ungroup()

  # could use count() instead of group_by()+summarise() - shorter but less flexible
  arrests.clean %>% 
    count(st_id, loc2) %>% 
    arrange(desc(n))
  
  # inspect - DO NOT INCLUDE IN RMD SUBMISSION
  str(st_arrests) 
  
  
# 1c.
  ggplot(data = st_arrests, 
         aes(x = arrests_all)) + 
    geom_histogram()


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
  
  # example: join st_arrests to st_poverty
  st_joinedtemp <- st_arrests %>%
    inner_join(st_poverty,
               by = c("st_id" = "st_id"))
  # uh-oh, doesn't work!
  
  # what's the problem? st_id is a factor in st_arrests but an integer in st_poverty!
  # need to join on columns of the same data type
  st_arrests <- st_arrests %>% 
    mutate(st_id = as.integer(st_id))
  
  st_joinedtemp <- st_arrests %>%
    inner_join(st_poverty,
               by = c("st_id" = "st_id"))
  
  rm(st_joinedtemp)
  

  # in-class exercise: join all 3 data frames (in a single pipe if you can):
    # 3 data frames to join: st_arrests, st_poverty, st_ridership
  st_joined <- st_arrests %>%
    inner_join(st_poverty, 
               by = c("st_id")) %>%
    inner_join(st_ridership, 
               by = c("st_id" = "st_id")) %>% 
    select(-all_of(drop_vars)) %>% 
    group_by(st_id, mta_name) 
  
  # why use all_of()? 
  # https://stackoverflow.com/questions/62397267/why-should-i-use-all-of-to-select-columns

  # inspect - DO NOT INCLUDE IN RMD SUBMISSION
  str(st_joined, give.attr = FALSE) # suppress attributes to avoid lengthy output
  summary(st_joined)
    # Note: 157 obs in joined df w/no NAs 
    # inner join worked as intended!

  # QUESTION: could we have used full_join or left_join instead of inner_join?
    # yes! in this case all 3 datasets have been pre-cleaned to have 157 obs.
    
  
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
    stations %>% 
      group_by(nblack) %>% 
      summarise(min(shareblack), 
                mean(shareblack), 
                max(shareblack))
    
    # examine joint distribution of highpov and black
    table(stations$highpov, stations$nblack)
  
  
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
  
  ggplot(stations, 
         aes(x = povrt_all_2016, 
             y = arrperswipe,
             weight = swipes2016)) + 
    geom_point() + 
    geom_smooth(method = 'lm', 
                formula = y ~ x) + #add linear SRF
    ggtitle('Scatterplot of arrest intensity vs. poverty rate') + 
    labs(x = 'poverty rate', 
         y = 'arrests per 100,000 ridership')

  # fit linear model w/station observations (can also add optional weights argument)
  m_3a_l <- lm_robust(arrperswipe ~ povrt_all_2016, 
                     data = stations,
                     weight = swipes2016)
  summary(m_3a_l) #get summary of the model

  # how to refer to specific regression results
  ?summary.lm
  summary(m_3a_l)$adj.r.squared  #adj R-square
  summary(m_3a_l)$coefficients   #coefficients
  round(summary(m_3a_l)$coefficients[2,1],2) #beta1_hat
  round(summary(m_3a_l)$coefficients[2,4],5) #p-value for beta1_hat
  format(round(summary(m_3a_l)$coefficients[2,4], 5),
         scientific = FALSE) #non-exponential notation
  

  # add quadratic prediction line to scatter plot
  
  prf <-  y ~ x + I(x^2)
  
  ggplot(stations, 
         aes(x = povrt_all_2016, 
             y = arrperswipe,
             weight = swipes2016)) + 
    geom_point() + 
    ggtitle('Fare evasion arrest intensity vs. poverty rate') + #add title
    labs(x = 'poverty rate', 
         y = 'arrests per 100,000 ridership') + #change axis labels
    geom_smooth(method = 'lm', 
                formula = prf) +  #add regression line for prf defined above
    stat_poly_eq(mapping = use_label("eq"), 
                 formula = prf) #add regression equation
  
  
  # fit quadratic OLS model
  # HINT: see quadratic syntax from Lecture4.2 (section 4.1)
  m_3a_q <- lm_robust(arrperswipe ~ povrt_all_2016 + I(povrt_all_2016^2),
                     data = stations,
                     weight = swipes2016) #include quadratic term
  summary(m_3a_q) 
  
  
  # Q: what's the appropriate hypothesis test when there is a quadratic term?
  # A: an F test for joint significance:
  #     H0: beta2 = beta3 = 0
  #     HA: at least one coefficient, beta2 or beta3, does not equal 0
  # let's see how to conduct the test using the stored results in ols1q
  
  summary(m_3a_q)  # ok we see the global F test results, how do we access?
  tidy(m_3a_q)     # clean df of regression results can be nice to work with
  
  m_3a_q$fstatistic # we have the appropriate F-stat and df, but not the p-value!
  
  # here's how to get the p-value for a global F-test
  pf(m_3a_q$fstatistic[1], 
     m_3a_q$fstatistic[2], 
     m_3a_q$fstatistic[3], 
     lower.tail = FALSE)
    
  
# 3b. calculate and test difference in means between high/low poverty stations
  
  # regress arrest intensity on highpov dummy to implement diff in means test 
  # weighted by ridership with robust SEs
  m_3b <- lm_robust(formula = arrperswipe ~ highpov, 
                        data = stations,
                        weights = swipes2016)
  summary(m_3b) 
       
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
##  d. BONUS: Next let's let's think about how measurement error might impact results
##      from 4b. Do you think measurement error could bias your estimates
##      of neighborhood racial gaps in the effect of poverty on enforcement
##      intensity from 4b? Explain, carefully. Do you have any creative ideas
##      to address any concerns you have about bias due to measurement error? 
## -----------------------------------------------------------------------------
  
# 4a.
  
  # show the mean arrest intensity for highpov x nblack groups
  # HINT: use group_by() and summarise(), along with weighted.mean()
  stations %>% 
    group_by(nblack, highpov) %>% 
    summarise(mean = round(weighted.mean(povrt_all_2016, swipes2016), 2)) 

    
# 4b.
  
  # Approach #1: OLS
  m_4b_arr <- lm_robust(formula = arrperswipe ~ nblack, 
                         data = subset(stations,
                                       highpov == "High poverty"),
                         weights = swipes2016)
  summary(m_4b_arr) 
  
  m_4b_pov <- lm_robust(formula = povrt_all_2016 ~ nblack, 
                      data = subset(stations,
                                    highpov == "High poverty"),
                      weights = swipes2016)
  summary(m_4b_pov) 
  
  
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
  ggplot(stations, aes(x = povrt_all_2016,
                    y= arrperswipe,
                    color = nblack,
                    weight = swipes2016)) +
    geom_point()  +
    geom_smooth(method = 'lm', 
                formula = y ~ x) + 
    ylab("Arrests per 100,000 ridership") + 
    xlab("Station area poverty rate") +
    ggtitle("Fare Evasion Arrest Intensity vs Poverty by Race", 
            subtitle = "Subway stations in Brooklyn (2016)") +
    scale_color_discrete(name = "Predominantly Black Station",
                         labels=c("No", "Yes"),
                         guide = guide_legend(reverse=TRUE)) +
    theme(legend.position = "bottom", 
          legend.background = element_rect(color = "black", 
                                           fill = "grey90", 
                                           size = .2, 
                                           linetype = "solid"), 
          legend.direction = "horizontal",
          legend.text = element_text(size = 8), 
          legend.title = element_text(size = 8) )
  
  # quadratic fit
  ggplot(stations, aes(x = povrt_all_2016,
                    y= arrperswipe,
                    color = nblack,
                    weight = swipes2016)) +
    geom_point()  +
    geom_smooth(method = 'lm', 
                formula = y ~ x + I(x^2)) + 
    ylab("Arrests per 100,000 ridership") + 
    xlab("Station area poverty rate") +
    ggtitle("Fare Evasion Arrest Intensity vs Poverty by Race", 
            subtitle = "Subway stations in Brooklyn (2016)") +
    scale_color_discrete(name = "Predominantly Black Station",
                         labels=c("No", "Yes"),
                         guide = guide_legend(reverse=TRUE)) +
    theme(legend.position = "bottom", 
          legend.background = element_rect(color = "black", 
                                           fill = "grey90", 
                                           size = .2, 
                                           linetype = "solid"), 
          legend.direction = "horizontal",
          legend.text = element_text(size = 8), 
          legend.title = element_text(size = 8) )

  # the above plots show the Sample Regression Function (predicted relationship)
  # how do we refer to regression coefficients and stats to interpret?

  # we can estimate separate models for Majority Black and non-Black stations
    
    # get separate data frames by predominantly Black stations 
    stations_black <- stations %>% 
      filter(nblack == "Majority Black")
    
    stations_nonblack <- stations %>% 
      filter(nblack == "Majority non-Black")
      
    # nblack == 1: linear model with station observations
    m_4c_black_l <- lm_robust(arrperswipe ~ povrt_all_2016,
                         data = stations_black,
                         weights = swipes2016)
    summary(m_4c_black_l)
      
    # nblack == 1: quadratic model with station observations
    m_4c_black_q <- lm_robust(arrperswipe ~ povrt_all_2016 + I(povrt_all_2016^2),
                         data = stations_black,
                         weights = swipes2016)
    summary(m_4c_black_q)
      
    #nblack == 0: linear model with station observations
    m_4c_nonblack_l <- lm_robust(arrperswipe ~ povrt_all_2016,
                          data = stations_nonblack,
                          weights = swipes2016)
    summary(m_4c_nonblack_l)
      
    # nblack == 0: quadratic model with station observations
    m_4c_nonblack_q <- lm_robust(arrperswipe ~ povrt_all_2016 + I(povrt_all_2016^2), 
                          data = stations_nonblack,
                          weights = swipes2016)
    summary(m_4c_nonblack_q)
    
    
  # or we can estimate an interaction model, interacting nblack and the pov rate
  m_4c_interact <- lm_robust(arrperswipe ~ povrt_all_2016*nblack, 
                               data = stations,
                               weights = swipes2016)
  summary(m_4c_interact)
    
  
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
  
  