################################################################################
##
## [ PROJ ] Lecture 4: Subway Fare Evasion Arrests and Racial Bias (part 1)
## [ FILE ] Lecture4-inclass.R
## [ AUTH ] < YOUR NAME >
## [ INIT ] < Feb 6, 2024 >
##
################################################################################

## POLICY QUESTION FOR THIS WEEK & NEXT:
## Police can stop and ticket or arrest people for subway fare evasion. 
## Is there racial bias in NYPD enforcement of subway fare evasion?

## Lecture3/A3 examined this question using variation among arrested individuals.

## Lecture4/A4 examines this question using variation between subway stations


## --------------------------------------
## 1. load libraries and check directory
## --------------------------------------

#install.packages("weights")
#install.packages("estimatr")
#install.packages("lmtest")
#install.packages("sandwich")
#install.packages("knitr")

library(tidyverse)
library(weights)
library(estimatr)
library(lmtest)
library(sandwich)
library(knitr)

getwd()


## -----------------------------------------------------------------------------
## 2. Aggregating to subway station-level arrest totals
##
##  a. load full set of cleaned arrest microdata (arrests.clean.rdata)
##
##  b. aggregate microdata to station-level observations w/the following info:
##      - st_id, loc2, arrests_all (=arrest count)
##      - store results as new data frame st_arrests
##      - inspect new data frame (but don't include inspection code in submission)
##
##  c. plot histogram of arrests and briefly describe distribution of arrests across stations
## -----------------------------------------------------------------------------

#2a.
load("arrests.clean.RData")


#2b.
  st_arrests <- arrests.clean %>% 
    group_by(st_id, loc2) %>% 
    summarise(arrests_all = n() ) %>% 
    arrange(desc(arrests_all))

  #could use count() instead of group_by()+summarise() - shorter but less flexible
  arrests.clean %>% 
    count(st_id, loc2) %>% 
    arrange(desc(n))
  
  #inspect - DO NOT INCLUDE IN RMD SUBMISSION
  str(st_arrests)  #why is this so long?
  str(st_arrests, give.attr = FALSE)
    #can also consider ungrouping the st_arrests dataframe 

#2c.
  ggplot(data = st_arrests, aes(x = arrests_all)) + geom_histogram()
  
  #remember ggplot is in the tidyverse, so we can also start by passing the data into a pipe
    st_arrests %>% ggplot(aes(x = arrests_all)) + geom_histogram()
  

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
##  c. print top 10 stations by arrest counts
##      - only display st_id, mta_name, arrests_all, shareblack, povrt_all_2016
## -----------------------------------------------------------------------------

#3a. 
  st_poverty <- read.csv("station_povdataclean_2016.csv", 
                         stringsAsFactors = TRUE)
  
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
  #  includes annual ridership (# of MetroCard 'swipes') at each station for 2011-16

  #make sure to inspect these new df's before we join them in 3b!


#3b.
  
  #a vector of columns we don't need to keep, will use in 3b
  drop_vars <- c("swipes2011", "swipes2012", "swipes2013", "swipes2014", "swipes2015")
  
  #example: join st_arrests to st_poverty
  #st_joinedtemp <- inner_join(st_arrests, st_poverty, by = c("st_id" = "st_id"))
    #uh-oh, doesn't work!
  
  #what's the problem? st_id is a factor in st_arrests but an integer in st_poverty!
  #need to join on columns of the same data type
  st_arrests <- st_arrests %>% mutate(st_id = as.integer(st_id))
  st_joinedtemp <- inner_join(st_arrests, st_poverty, by = c("st_id" = "st_id"))
  
  rm(st_joinedtemp)
  
    
  #in-class exercise: join all 3 data frames (in a single pipe if you can):
    #3 data frames to join: st_arrests, st_poverty, st_ridership
  st_joined <- st_arrests %>%
    inner_join(st_poverty, by = c("st_id")) %>%
    inner_join(st_ridership, by = c("st_id" = "st_id",
                                    "mta_name" = "mta_name")) %>% 
    select(-all_of(drop_vars)) %>% 
    group_by(st_id, mta_name) 

    #why use all_of()? 
    #https://stackoverflow.com/questions/62397267/why-should-i-use-all-of-to-select-columns
  
  #inspect - DO NOT INCLUDE IN RMD SUBMISSION
    #display structure of ungrouped data frame to avoid lengthy output listing every group
    st_joined %>% ungroup() %>% str(give.attr = FALSE)
    summary(st_joined)
      #Note: 157 obs in joined df w/no NAs (except some missing demographics) 
      #inner join worked as intended!
    
  #QUESTION: could we have used full_join or left_join here instead of inner_join?
    #yes! in this case all 3 datasets have been pre-cleaned to have 157 obs.

    
#3c.
  st_joined %>% 
    arrange(desc(arrests_all)) %>% 
    select(st_id, mta_name, arrests_all, shareblack, povrt_all_2016) %>% 
    head(n = 10) 


## -----------------------------------------------------------------------------
## 4. Explore relationship between arrest intensity & poverty rates across stations
##
##  a. compute variables for arrest intensity and other explanatory variables
##     - exclude coney island station from the analysis sample
##     - create new variable measuring fare evasion arrest intensity:
##       - arrperswipe_2016 = arrests per 100,000 ridership (swipes)
##     - create new dummy variable indicating high poverty station area:
##       - highpov = 1 if pov rate is > median pov rate across stations
##     - create new dummy for majority Black station areas (shareblack > .5)
##     - coerce new dummies into factors w/category labels
##     - assign results to new df called stations
##     - validate results 
##     - display top 10 stations by arrest intensity using kable() in knitr package
##       - only show st_id, mta_name, arrests_all and new columns
##
##  b. investigate arrests intensity vs poverty rates 
##     - plot arrperswipe vs povrt_all_2016
##     - should we weight stations by ridership?
##     - investigate linear and quadratic model fit 
##     - interpret your preferred regression specification (carefully!)
##  
##  c. report diff in mean arrest intensity between high/low pov areas
##     - weight observations by ridership (swipes) for difference in group means
##     - is this difference statistically significant?
## ---------------------------------------------------------------------------

#4a.
  stations <- st_joined %>%
    mutate(arrperswipe = round(arrests_all / (swipes2016/100000), 2), #arrest intensity
           highpov = as.numeric(povrt_all_2016 > median(st_joined$povrt_all_2016)),
           nblack = as.numeric(shareblack > .5),
           shareblack = round(shareblack, 2),
           povrt_all_2016 = round(povrt_all_2016, 2)) %>% 
    mutate(highpov = factor(highpov, levels = c(0,1), 
                            labels = c("Not high poverty", "High poverty")),
           nblack  = factor(nblack, levels = c(0,1), 
                            labels = c("Majority non-Black", "Majority Black"))) %>% 
    filter(st_id != 66)
  
      #in the above code we directly test conditions as a logical comparison
      #then we convert logical results into numeric (i.e. a dummy variable)
      #lastly we continued on to convert to factors
  
  #some inspection and validation
  
    #check if nblack recoding worked as intended
      stations %>% 
        group_by(nblack) %>% 
        summarise(min(shareblack), mean(shareblack), max(shareblack))

    #examine joint distribution of highpov and black
      table(stations$highpov, stations$nblack)
      
  
  #display top 10 stations by arrest intensity (show st_id, mta_name, arrests_all and new variables)
   stations %>% 
     arrange(desc(arrperswipe)) %>% 
     select(st_id, mta_name, arrperswipe, arrests_all, shareblack, povrt_all_2016, highpov, nblack) %>% 
     head(n = 10)  %>% 
     kable() #kable offers better table formatting

  
#4b. 
  ggplot(data = stations, #specify dataframe to use
         aes(x = povrt_all_2016, y = arrperswipe)) + #specify columns to use
    geom_point(aes(size=pop_all_2016)) #specify plot geometry
    #('Scatterplot of arrest intensity vs. poverty rate') + #add title
    #labs(x = 'poverty rate', y = 'arrests per 100,000 ridership') #change axis labels

  #fit linear model with station observations (can also add optional weights argument)
    ols1l <- lm(arrperswipe ~ povrt_all_2016, data = stations)
    summary(ols1l) #get summary of the model
    coeftest(ols1l, vcov = vcovHC(ols1l, type="HC1")) #get robust SEs
  
  #how to refer to specific regression results
    ?summary.lm
    summary(ols1l)$adj.r.squared  #adj R-square
    summary(ols1l)$coefficients   #coefficients
    round(summary(ols1l)$coefficients[2,1],2) #beta1_hat
    coeftest(ols1l, vcov = vcovHC(ols1l, type="HC1"))[2,4] #p-value on beta1_hat
  
  #same results using lm_robust() in the estimatr package
    ols1l_lmr <- lm_robust(arrperswipe ~ povrt_all_2016,
                       se_type="HC1",  #type="HC1" specifies robust SE formula
                       data = stations)
    summary(ols1l_lmr)
    summary(ols1l_lmr)$coefficients[2,1]
    summary(ols1l_lmr)$adj.r.squared
    
  #add linear prediction line to scatter plot
    ggplot(stations, 
           aes(x = povrt_all_2016, y = arrperswipe)) + 
      geom_point() + 
      ggtitle('Scatterplot of arrest intensity vs. poverty rate') + 
      labs(x = 'poverty rate', y = 'arrests per 100,000 ridership') + 
      geom_smooth(method = "lm", formula = y~x)  #add linear SRF

  #fit quadratic OLS model (arrest intensity vs. poverty rate)
    #HINT: see quadratic syntax from Lecture4.2 (section 4.1)
    ols1q <- lm_robust(arrperswipe ~ povrt_all_2016 + I(povrt_all_2016^2),
                           se_type="HC1", #get robust SEs
                           data = stations) 
    summary(ols1q) 
    
  #add quadratic prediction line to scatter plot
    ggplot(stations,
           aes(x = povrt_all_2016, y = arrperswipe)) + 
      geom_point() + 
      ggtitle('Linear regression fit') + 
      labs(x = 'poverty rate', y = 'arrests per 100,000 ridership') + 
      geom_smooth(method = 'lm', formula = y ~ x + I(x^2)) 
    
  
#4c. calculate and test difference in means between high/low poverty stations
    
  #summarise w/group_by is ok... but doesn't accept weights!
    stations %>% 
      ungroup() %>%  #note we're ungrouping by station first
      group_by(highpov) %>% 
      summarise(n = n(),
                mean_pov = mean(povrt_all_2016),
                mean_arrper = mean(arrperswipe))
    
  #inference with t.test command and unequal variance (doesn't accept weights!)
    t.test(arrperswipe ~ highpov, data = stations, var.equal = FALSE)
    
    
  #instead let's use bivariate regression (accepts weights!) w/robust SEs
  #QUANT II REVIEW: 
  # equivalence of diff-in-means test & bivariate regression w/dummmy regressor
  # consult Video Lecture 2.2a on the class website  
    diff1 <- lm_robust(arrperswipe ~ highpov, 
                       se_type="HC1", #get robust SEs
                       data = stations, 
                       weight = swipes2016)
    summary(diff1) #get summary of the model

    
  #wtd.t.test function in the weights package accepts weights... but not robust SEs
    ?wtd.t.test
    stations_highpov <- stations %>% filter(highpov == "High poverty")
    stations_lowpov  <- stations %>% filter(highpov == "Not high poverty")
    wtd.t.test(stations_highpov$arrperswipe, stations_lowpov$arrperswipe, 
               weight = stations_highpov$swipes2016, 
               weighty = stations_lowpov$swipes2016)
      #compare difference and SE of difference to previous approach

 
## -----------------------------------------------------------------------------
## 5. How does neighborhood racial composition mediate the relationship between poverty and arrest intensity
##    - examine relationship between arrest intensity & poverty by Black vs non-Black station area (nblack)
##
##    a. show a difference in means table: arrests per ridership (swipes) by highpov vs nblack
##        - weighted by station ridership
##        - could difference in arrest intensity be explained by differences in povrt?
##
##    b. show and interpret a scatterplot of arrest intensity vs. pov rates by nblack w/your preferred regression lines
##        - use separate aesthetics to plot Black and non-Black station areas
##        - your plot should include your preferred regression estimates: linear or quadratic (not both)
##        - carefully explain which regression specification you prefer and why (linear or quadratic)
##        - if applicable, cite statistical evidence to support your decision.
##        - Interpret your preferred regression specification (carefully)!
##
##    c. Next let's let's think about how measurement error might impact results from 5b.
##       Do you think measurement error could bias your estimates of neighborhood racial gaps
##       in the effect of poverty on enforcement intensity from 5b? 
##       Explain, carefully. 
##       Do you have any creative ideas to address concerns about potential bias from measurement error?
##    
##       (This is tricky!)  
##
##    d. interpret the results from your preferred regression specification (carefully!)
##
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
  #scatterplot by nblack
    ggplot(stations, aes(x = povrt_all_2016, y = arrperswipe, color = nblack)) +
      geom_point() +
      #Modify legend title and text
      scale_color_discrete(name = "Predominantly Black Station",
                           labels=c("No", "Yes"),
                           # Reverse Label Order
                           guide = guide_legend(reverse=TRUE)) +
      #Modify legend aesthetics (optional)
      theme(legend.position = "bottom", 
            legend.background = element_rect(color = "black", fill = "grey90", size = .2, linetype = "solid"), 
            legend.direction = "horizontal",
            legend.text = element_text(size = 8), 
            legend.title = element_text(size = 8) )
  
  #add linear plot

  
  #add quadratic plot


#5c.  
    
  #by nblack: get separate data frames
  stations_black <- stations %>% filter(nblack == "Majority Black")
  stations_nonblack <- stations %>% filter(nblack == "Majority non-Black")
  
  #nblack == 1: linear model with station observations 
  ols1lb <- lm_robust(arrperswipe ~ povrt_all_2016, 
                      data = stations_black,
                      se_type="HC1")
  summary(ols1lb) #get summary of the model
  
  #nblack == 1: quadratic model with station observations

  
  #nblack == 0: linear model with station observations (can also add optional weights argument)

  
  #nblack == 0: quadratic model with station observations

    
  
#5d.
  
    
## -----------------------------------------------------------------------------
## 6. Similar to analysis above, examine relationship between arrest intensity & criminal complaints
##  
##  a. read in crime data (nypd_criminalcomplaints_2016.csv) and join to existing stations df
##      - nypd_criminalcomplaints_2016.csv: 
##        - shows # of criminal complaints for each subway station area
##        - https://data.cityofnewyork.us/Public-Safety/NYPD-Complaint-Data-Historic/qgea-i56i
##      - join on st_id
##      - exclude the stations with the 4 highest counts of criminal complaints
##        - why? bc they are in very close proximity to Brooklyn transit policing HQ,
##          and to Brooklyn criminal courts, so they don't face comparable
##          neighborhood policing conditions
##
##
## 6b. Examine the overall relationship between arrest intensity and crime 
##     without taking neighborhood racial composition or poverty into account 
##     (comparable to Section 4b)
##
##
## 6c. Examine how neighborhood racial composition mediates the relationship 
##     between arrest intensity and crime (comparable to Section 5b)
##  
##
##  NOTE: for 6b and 6c, you don't need to follow *all* the same steps in questions 3-5.
##  - focus on showing your preferred plots to inform the relationship, along with
##    any additional data manipulation & evidence to support your decisions/interpretation/conclusions
##  - explore the data before arriving at your preferred plots, don't show us everything
##    you tried, just the analysis you settled on as most relevant to this question
##
## -----------------------------------------------------------------------------
    
#6a.  
  st_crime <- read.csv("nypd_criminalcomplaints_2016.csv")
    
  
  
#6b.

  
#6c. 
  

  
## -----------------------------------------------------------------------------
## 7. interpret your findings with respect to subway fare evasion enforcement bias based on race
##    - any additional analysis you'd like to explore with the data at hand?
##    - are there any key limitations to the data/analysis affecting your
##       ability to assess enforcement bias based on race?
##    - any additional data you'd like to see that would strengthen your analysis/interpretation 
##    - for this question, try to be specific and avoid vaguely worded concerns
## -----------------------------------------------------------------------------
  
  