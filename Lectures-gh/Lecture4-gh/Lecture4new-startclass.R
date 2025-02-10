################################################################################
##
## [ PROJ ] Lecture 4: Subway Fare Evasion Arrests and Racial Bias (part 1)
## [ FILE ] Lecture4-inclass=FULL.r
## [ AUTH ] < YOUR NAME >
## [ INIT ] < Sept. 27, 2022 >
##
################################################################################

## POLICY QUESTION FOR THIS WEEK & NEXT:
## Police can stop and ticket or arrest people for subway fare evasion. 
## Is NYPD enforcement of subway fare evasion enforcement in Brooklyn racist?

## -----------------------------------------------------------------------------
## 1. load libraries and check directory
## -----------------------------------------------------------------------------

#install.packages("weights", dependencies = TRUE)
#install.packages("lmtest")
#install.packages("sandwich")
#install.packages("knitr")

library(tidyverse)
library(weights)
library(lmtest)
library(sandwich)
library(knitr)
library(estimatr)

getwd()


## -----------------------------------------------------------------------------
## 2. Aggregating to subway station-level arrest totals
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

#2a.
load("arrests.clean.RData")


#2b.        

  
  #inspect - DO NOT INCLUDE IN RMD SUBMISSION

  
#2c.



## -----------------------------------------------------------------------------
## 3. joining ridership and neighborhood demographics to arrest data
##
##  a. read in other station-level csv files (w/strings as factors) and inspect
##      - don't include inspection code in your rmd submission
##  
##  b. join both data frames to st_arrests & inspect results (store new df as st_joined)
##      - inspect results of join and describe any issues
##      - drop unnecessary columns from the ridership data
##      - group st_joined by st_id and mta_name
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

#3a. 
  st_poverty <- read.csv("station_povdataclean_2016.csv", 
                         stringsAsFactors = TRUE) %>% select(-nblack)
  
  #background on poverty data (from 2016 American Community Survey): 
  #  Source: https://usa.ipums.org/usa/acs.shtml
  #  each observation represents a "subway station area", defined as follows:
  #   - all census tracts w/geometric center within .5km of a station (see map)
  #  st_id is the unique identifier for each subway station area
  #  povrt_all_2016: % of adults in subway station area living below federal poverty level
  #  shareblack: share of adults in subway station area who identify as Black
  #  nblack: dummy variable = 1 if shareblack >= 50%, 0 otherwise
  
  
  st_ridership <- read.csv("Subway Ridership by Station - BK.csv", 
                           stringsAsFactors = TRUE)
  #background on MTA ridership data:
  #  Source: http://web.mta.info/nyct/facts/ridership/ridership_sub_annual.htm
  #  each observation is a subway station w/a unique identifier (st_id)
  #  includes annual ridership (# of MetroCard swipes) at each station for 2011-16

  #make sure to inspect these new df's before we join them in 3b!


#3b.

  #a vector of columns we don't need to keep, will use in 3b
  drop_vars <- c("swipes2011", "swipes2012", "swipes2013", "swipes2014", "swipes2015")
  
  #practice: join st_arrests to st_poverty
  st_joinedtemp <- FILL IN CODE
  

  #in-class exercise: join all 3 data frames (in a single pipe if you can):
    #3 data frames to join: st_arrests, st_poverty, st_ridership
  st_joined <- FILL IN CODE
  
  
  #why use all_of()? 
  #https://stackoverflow.com/questions/62397267/why-should-i-use-all-of-to-select-columns

  #inspect - DO NOT INCLUDE IN RMD SUBMISSION


  #QUESTION: could we have used full_join or left_join here instead of inner_join?
    #yes! in this case all 3 datasets have been pre-cleaned to have 157 obs.
    
  
#3c.


#3d.
  stations <- st_joined %>%
    mutate(arrperswipe = round(arrests_all / (swipes2016 / 100000), 2),
           highpov = as.numeric(povrt_all_2016 > median(st_joined$povrt_all_2016)),
           nblack = as.numeric(shareblack > .5),
           shareblack = round(shareblack, 2),
           povrt_all_2016 = round(povrt_all_2016, 2)) %>% 
    mutate(highpov = factor(highpov, levels = c(0,1), 
                            labels = c("Not high poverty", "High poverty")),
           nblack  = factor(nblack, levels = c(0,1), 
                            labels = c("Majority non-Black", "Majority Black"))) 
  FILL IN CODE TO EXCLUDE CONEY ISLAND OBSERVATION
  
  #validate and inspect
  
    #check if nblack recoding worked as intended
    stations %>% 
      group_by(nblack) %>% 
      summarise(min(shareblack), 
                mean(shareblack), 
                max(shareblack))
    
    #examine joint distribution of highpov and black
    table(stations$highpov, stations$nblack)
  
  
  #print top 10 stations by arrest intensity 

 

#3e.  
  
    

## -----------------------------------------------------------------------------
## 4. Examine the relationship between arrest intensity and poverty rates
##
##  a) Show scatterplot of arrest intensity vs. poverty rates,
##      - include regression line you think best fits this relationship. 
##      - weight observations by ridership, and label your axes appropriately. 
##      - Only show one plot with your preferred specification!**
##     Which regression specification do you prefer: linear or quadratic? 
##      Be clear about your logic and cite statistical evidence to support your decision.
##     Interpret your preferred regression specification (carefully!). 
##      Remember to test for statistical significance.
##
##  b) Estimate and test difference in mean arrest intensity btw high/low poverty areas
##      - Report difference and assess statistical significance
##      - Weight observations by ridership
## ---------------------------------------------------------------------------

#4a.
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

  #fit linear model with station observations (can also add optional weights argument)
    ols1l <- lm_robust(arrperswipe ~ povrt_all_2016, 
                       data = stations,
                       weight = swipes2016)
    summary(ols1l) #get summary of the model

  #how to refer to specific regression results
    ?summary.lm
    summary(ols1l)$adj.r.squared  #adj R-square
    summary(ols1l)$coefficients   #coefficients
    round(summary(ols1l)$coefficients[2,1],2) #beta1_hat
    round(summary(ols1l)$coefficients[2,4],5) #p-value for beta1_hat
  

  #add quadratic prediction line to scatter plot

    
  #fit quadratic OLS model (arrest intensity vs. poverty rate)
  #HINT: see quadratic syntax from Lecture4.2 (section 4.1)


  # what's the appropriate hypothesis test when there are two quadratic terms?
    
  
    
#4c. calculate and test difference in means between high/low poverty stations
    
    stations %>% 
      ungroup() %>% #stations was already grouped by st_id, need to ungroup first
      group_by(highpov) %>% 
      summarise(n = n(),
                mean_pov = weighted.mean(povrt_all_2016, swipes2016),
                mean_arrper = weighted.mean(arrperswipe, swipes2016))

   #inference with t.test command and unequal variance (doesn't accept weights!)
     t.test(arrperswipe ~ highpov, 
            data = stations, 
            var.equal = FALSE)

  #regress arrest intensity on highpov dummy to implement diff in means test 
  #weighted by ridership, robust SEs
     ols_diff1 <- lm_robust(formula = arrperswipe ~ highpov, 
                            data = stations,
                            weights = swipes2016)
     summary(ols_diff1) 
       

  #wtd.t.test function in the weights package accepts weights... but not robust SEs
    ?wtd.t.test
    stations_highpov <- stations %>% filter(highpov == "High poverty")
    stations_lowpov  <- stations %>% filter(highpov == "Not high poverty")
    wtd.t.test(stations_highpov$arrperswipe, stations_lowpov$arrperswipe, 
               weight = stations_highpov$swipes2016, 
               weighty = stations_lowpov$swipes2016)
      #compare difference and SE of difference to previous approach
    

## -----------------------------------------------------------------------------
## 5. How does neighborhood racial composition mediate the relationship between 
##    poverty and arrest intensity? 
##    - examine relationship btw arrest intensity & poverty by Black/non-Black station area (nblack)
##
##    a. Present a difference in mean table for arrests intensity for each group
##        in a 2x2 table of `highpov` vs `nblack`. 
##        - Remember to weight by ridership. 
##       Present a similar table for the mean poverty rate 
##       Does it appear that differences in arrest intensity are explained by differences in poverty rate?
##
##    b. Show a scatterplot of arrest intensity vs. poverty rates
##        - use separate aesthetics for Black and non-Black station areas
##        - include the regression lines that you think best capture this relationship
##        - Weight observations by ridership, and label your axes appropriately. 
##        - Only show one plot with your preferred specification!
##        - Interpret your preferred regression specification (carefully!).
##
##    c. Next let's let's think about how measurement error might impact results
##        from 5b. Do you think measurement error could bias your estimates
##        of neighborhood racial gaps in the effect of poverty on enforcement
##        intensity from 5b? Explain, carefully. Do you have any creative ideas
##        to address any concerns you have about bias due to measurement error? 
## -----------------------------------------------------------------------------
  
#5a. 
  #the tapply() function from base R can help here, let's see how it works
    
  #let's apply the median function to the arrperswipe column by highpov groups
    tapply(stations$arrperswipe, stations$highpov, median) 
    
  #to answer 5a we want to use the mean function
    tapply(stations$arrperswipe, stations$highpov, mean)
    
  #the with() function can help streamline our code a bit
    with(stations, 
         tapply(arrperswipe, highpov, mean))  
    
  #we can also report mean arrest intensity for more than 1 grouping variable,
  #let's group by highpov x nblack and show unweighted mean arrest intensity by group
  #we can pass the whole table into the round() function to make it easier to read
  #and store the results so we can refer back to them in our write-up
    t1_arrper <- with(stations, 
                      tapply(arrperswipe, 
                             list(highpov, nblack), 
                             mean) ) %>% round(2)
    t1_arrper
    
  #tapply doesn't allow for weights to be passed to the function it uses
  #how do we replicate the table above w/the weighted difference in mean arrests?
  #one alternative approach is to use tapply but incorporate weights 'manually'
  #here's the formula for the weighted mean = Σ(x*w) / Σw
    t1_arrper_wtd <-
      with(stations,
           tapply(arrperswipe * swipes2016,
                  list(highpov, nblack),
                  sum))  /
      with(stations,
           tapply(swipes2016,
                  list(highpov, nblack),
                  sum) )
    t1_arrper_wtd <- t1_arrper_wtd %>% round(2)
    t1_arrper_wtd 
    
  #note how group means can change with weighting 
    t1_arrper 
    t1_arrper_wtd
    
    
  #ok so arrest intensity is higher in high-pov stations that are majority black
  #do differences in poverty rates in part explain this association?
  #let's do a quick check
    t1_povrt_wtd <- 
      with(stations,
           tapply(povrt_all_2016 * swipes2016,
                  list(highpov, nblack),
                  sum)) / 
      with(stations,
           tapply(swipes2016,
                  list(highpov, nblack),
                  sum)) 
    t1_povrt_wtd <- t1_povrt_wtd %>% round(2)
    
    t1_povrt_wtd
    t1_arrper_wtd

    
#5b.
    
  #scatterplot by nblack w/linear plots
    ggplot(stations, aes(x = povrt_all_2016, 
                         y = arrperswipe, 
                         color = nblack)) +
      geom_point()  +
      geom_smooth(method = 'lm', formula = y ~ x) + 
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
  
  #w/quadratic plots


#5b.  
  
  # get separate data frames by predominantly Black stations 
    stations_black <- stations %>% 
      filter(nblack == "Majority Black")
    
    stations_nonblack <- stations %>% 
      filter(nblack == "Majority non-Black")
    
  # nblack == 1: linear model with station observations
    ols_b_l <- lm_robust(arrperswipe ~ povrt_all_2016,
                         data = stations_black,
                         weights = swipes2016)
    summary(ols_b_l)
    
  # nblack == 1: quadratic model with station observations
    ols_b_q <- lm_robust(arrperswipe ~ povrt_all_2016 + I(povrt_all_2016^2),
                         data = stations_black,
                         weights = swipes2016)
    summary(ols_b_q)
    
    
  #nblack == 0: linear model with station observations
    ols_nb_l <- lm_robust(arrperswipe ~ povrt_all_2016,
                          data = stations_nonblack,
                          weights = swipes2016)
    summary(ols_nb_l)
    
  # nblack == 0: quadratic model with station observations
    ols_nb_q <- lm_robust(arrperswipe ~ povrt_all_2016 + I(povrt_all_2016^2), 
                          se_type = "HC1",
                          data = stations_nonblack,
                          weights = swipes2016)
    summary(ols_nb_q)
    
    
  # what's the appropriate hypothesis test when there are two quadratic terms?
  
    
    
#5c.
    
  
    
## -----------------------------------------------------------------------------
## 6. Examine the relationship between arrest intensity and crime 
##  
##  a. Load the crime data (`nypd_criminalcomplaints_2016.csv`)
##     Join to the existing `stations` data frame. 
##     Drop the stations with the 4 highest crime counts: 
##      - they are in close proximity to the criminal courthouse and thus may
##        experience higher arrest intensity for reasons unrelated to crime & poverty.
##
##  b. First examine the overall relationship between arrest intensity and crime 
##      (without taking neighborhood rrace or poverty into account, similar to 4a)
##     Carefully interpret the results you choose to present
##      - Show a scatterplot of your preferred crime measure vs. poverty rates 
##      - Include the regression line you think best fits this relationship. 
##      - Weight observations by ridership, and label your axes appropriately. 
##      - Only show one plot with your preferred specification!
##      - Interpret your preferred regression specification (carefully!).
##      - Remember to test for statistical significance.
##
##  c. Examine how neighborhood racial composition mediates the relationship 
##      between arrest intensity and crime (comparable to Section 5b).
##      - Show a scatterplot of your preferred crime measure vs. poverty rates
##      - Include the regression line you think best fits this relationship. 
##      - Weight observations by ridership, and label your axes appropriately.
##      - Only show one plot with your preferred specification!
##      - Interpret your preferred regression specification (carefully!). 
##      - Remember to test for statistical significance.
## -----------------------------------------------------------------------------

#6a.  

  
#6b.

    
#6c.   
    

  
## -----------------------------------------------------------------------------
## 7. Summarize and interpret your findings with respect to subway fare evasion 
##    enforcement bias based on race. Be very careful about any claims of racial
##    basis, any such claims should be supported by the analysis you present.
##    - Is there any additional analysis you'd like to do with the data at hand?
##    - Are there any key limitations to the data and/or analysis affecting 
##       your ability to assess enforcement bias based on race? 
##    - Is there any additional data you'd like to see that would help 
##       strengthen your analysis and interpretation?
##   For this question, try to be specific and avoid vaguely worded concerns.
## -----------------------------------------------------------------------------
  
  