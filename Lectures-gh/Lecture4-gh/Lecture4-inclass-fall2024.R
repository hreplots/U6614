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
