################################################################################
##
## [ PROJ ] Lecture 7: Water shutoffs, race, and health in Detroit (Part 1)
## [ FILE ] detroit-exploratory.r
## [ AUTH ] < YOUR NAME >
## [ INIT ] < Oct 21, 2024 >
##
################################################################################

## POLICY QUESTION FOR THIS WEEK & NEXT:
##
## 177k Detroit homes have had their water shut off for non-payment since 2010
##
## Today: who is most affected by this problem?
##
## Next week: what are the associated public health impacts?


## -----------------------------------------------------------------------------
## libraries
## -----------------------------------------------------------------------------

#install.packages("readstata13")
#install.packages("lubridate")
#install.packages("weights")

#library(foreign)     #note only imports .dta files up thru Stata 12
library(readstata13)  #imports .dta files from Stata 13 thru 15
library(tidyverse)
library(lubridate)
library(weights)


## -----------------------------------------------------------------------------
## directory paths
## -----------------------------------------------------------------------------

#this time we have a parent folder for the project (DetroitWaterShutoffs),
#and two subfolders for Data and Code 
getwd()


## -----------------------------------------------------------------------------
## Get demographic data from the American Community Survey (via tidycensus)
## -----------------------------------------------------------------------------

# we can download data from the US Census Bureau using the tidycensus package

# tidycensus is a package that allows users to interface w/ Census Bureau data
# https://walker-data.com/tidycensus/

# data requests are made through the Census API
# an API is just a way of accessing data from within an app (e.g. RStudio)
# need to sign up for a free Census API Key:
#   http://api.census.gov/data/key_signup.html


### THE FOLLOWING CODE IS COMMENTED OUT AS SAMPLE CODE FOR REFERENCE
#
# # census_api_key("ENTER YOUR API KEY HERE")
# 
# # initialize new data frame to store ACS data from API requests
# MI_acs_tract_10_17 <- data_frame()
# 
# # set up a for loop for each year of data to access
# for (i in 2010:2017) {
#   
#   # query ACS data from the census API for each year 2010-2017
#   acs <- get_acs(geography = "tract",
#                  state = "MI",
#                  variables = c(pop = "B01001_001",
#                                white_pop = "B01001H_001",
#                                hisp_pop = "B01001I_001",
#                                asian_pop = "B02001_005",
#                                black_pop = "B02001_003",
#                                male_pop = "B01001_002",
#                                medianinc = "B19013_001",
#                                med_age = "B01002_001",
#                                pov = "B17025_002"),
#                  year = i)
#   
#   # transform data for later analysis and prep for join
#   acs <- acs %>% 
#     select(-moe, -NAME) %>% 
#     pivot_wider(names_from = variable, values_from = estimate) %>% 
#     mutate(year = i,
#            whiteshare = 100 * (white_pop/pop),
#            hispshare = 100 * (hisp_pop/pop),
#            asianshare = 100 * (asian_pop/pop),
#            blackshare = 100 * (black_pop/pop),
#            maleshare = 100 * (male_pop/pop),
#            poverty_rate = 100 * (pov/pop),
#            black75 = as.numeric(blackshare >= 75),
#            inc_above_median = as.numeric(medianinc > 26884.59),
#            tractid = as.numeric(GEOID)) %>% 
#     select(-GEOID)
#   print(i)
#   
#   # append each year of data to a combined dataset
#   MI_acs_tract_10_17 <- MI_acs_tract_10_17 %>% 
#     bind_rows(acs)
# }
# 
# # save data frame 
# saveRDS(MI_acs_tract_10_17, file = "Data/MI_acs_tract_10_17.rds")


### START HERE IN CLASS

#load ACS data
MI_acs_tract_10_17 <- readRDS("Data/MI_acs_tract_10_17.rds")

#inspect
str(MI_acs_tract_10_17)
summary(MI_acs_tract_10_17)


#what is the unit of observation?
table(MI_acs_tract_10_17$tractid, MI_acs_tract_10_17$year) %>%
  head(n = 10)
#we have a balanced year-tract panel!

#population represented by the sample? 
#all tracts in Michigan (MI)
#tract-level data from ACS is representative of total pop in each tract

#why the NA values? let's explore 
MI_acs_tract_10_17 %>% 
  filter(is.na(medianinc) == TRUE) %>% 
  select(year, tractid, pop, medianinc, whiteshare) %>% 
  arrange(desc(pop))

MI_acs_tract_10_17 %>% 
  filter(is.na(whiteshare) == TRUE) %>% 
  select(year, tractid, pop, medianinc, whiteshare) %>% 
  arrange(desc(pop))



## -----------------------------------------------------------------------------
## Get service interruption (SI) data - i.e. shutoff records (microdata)
## -----------------------------------------------------------------------------

#get service interruption data
input_si <- read.dta13("Data/si_1017_cleaned.dta")
input_si <- si_1017_cleaned
#what is the unit of observation? each observation is a shutoff record.


#focus on key variables to identify period/location of every shutoff
#we'll want to join to demographic data based on tractid and get tract-level obs
si.clean <- input_si %>% 
  select(si_order_number, census_tract_long, year, month) %>% 
  rename(tractid = census_tract_long) %>% 
  arrange(tractid, year, month)

#aggregate to tract-year/month totals
si_tract_ym <- si.clean %>% 
  group_by(tractid, year, month) %>% 
  summarise(si_count = n_distinct(si_order_number)) %>% 
  arrange(tractid, year, month)

#inspect
summary(si_tract_ym)
table(si_tract_ym$month, si_tract_ym$year) #what does this tell us?


## -----------------------------------------------------------------------------
## Join shutoff & demographic data: construct tract-year/month panel 
## -----------------------------------------------------------------------------

#join tract-yr demographic data (MI_acs_tract_10_17) to 
# tract-month shutoff data (si_tract_ym)
#only keep tracts that are in the shutoff data (si_tract_ym)
#MI_acs_tract_10_17 includes Detroit tracts (Wayne County), 
#but also tracts outside of Detroit across the state of Michigan
#want to end up with a tract-year-month panel
#new df should include: all columns from two dfs and a new date column
#also filter out observation for 2017-11-01 due to incomplete data

#HINT: what column(s) do you want to join on?
#HINT: what kind of join would work here?

tract_ym <- left_join(si_tract_ym, MI_acs_tract_10_17, #combines rows from two datasets based on a common variable, keeping all rows from the left dataset
                      by = c("tractid", "year")) %>% 
  mutate(date = make_date(year, month, 1)) %>% 
  arrange(tractid, year, month) %>% 
  filter(date != "2017-11-01")

#inspect
summary(tract_ym)

#do we have a balanced panel? 
table(tract_ym$month, tract_ym$year)

tract_ym %>% 
  group_by(tractid) %>%
  count(tractid) %>% 
  arrange(n)
#nope, if a tract had 0 shutoffs one month -> no row for that tract-month


#collapse to tract-level totals (summed over all year-months)
#i.e. a single obs per tract with shutoffs summed over all years
#include time-invariant measures of other variables
#NOTE: this allows us to focus on variation between tracts 
# (not within-tracts over time)
# will no longer be a panel dataframe
tract <- tract_ym %>% 
  group_by(tractid) %>% #kills time dimension
  summarise(si_count = sum(si_count),
            pop = mean(pop, na.rm = TRUE),
            blackshare = mean(blackshare, na.rm = TRUE),
            black75 = round(mean(black75, na.rm = TRUE), 0),
            medianinc = mean(medianinc, na.rm = TRUE),
            inc_above_median = round(mean(inc_above_median, 
                                          na.rm = TRUE), 0) ) %>% 
  mutate(si_1000 = si_count / (pop / 1000) ) %>% #shutoffs per 1000 people
  arrange(tractid)

#inspect
summary(tract)

#what is the unit of observation? 1 observation for each census tract.


## -----------------------------------------------------------------------------
## 1. Cross-sectional analysis: tract-level income, race & shutoffs
## -----------------------------------------------------------------------------

#NOTE: here "cross-sectional" means 1 obs per tract 
# (w/shutoffs summed over all years)

#scatterplot: % black vs shutoffs
ggplot(data = tract, 
       aes(x = blackshare, y = si_1000)) + 
  geom_point() 

#compute correlation to assess model fit (in addition to visual inspection)
cor(tract$blackshare, tract$si_1000, use = "pairwise.complete.obs")
wtd.cor(tract$blackshare, tract$si_1000, weight = tract$pop)    

#QUESTION: how can we improve the above plot?
#do you want to consider weighting observations?
#how can we use aesthetic mappings to incorporate weights in our plots?  

#scatterplot: % black vs shutoffs
ggplot(data = tract, 
       aes(x = blackshare,
           y = si_1000,
           size = pop)) + 
  geom_point(alpha = 0.1) + #alpha adjusts the transparency of points
  scale_size(range = c(0.1, 6), guide = "none") +
  ggtitle("Shutoffs per capita on Black share of population")

#for A5: 
#play around w/aesthetics including color, labels etc. to improve this plot


#scatterplot: median income vs shutoffs
ggplot(data = tract, 
       aes(x = medianinc,
           y = si_1000,
           size = pop)) + 
  geom_point(alpha = 0.1) + #alpha adjusts the transparency of points
  scale_size(range = c(0.1, 6), guide = "none") +
  ggtitle("Shutoffs per capita on median income")

#correlations for model fit
cor(tract$medianinc, tract$si_1000, use = "pairwise.complete.obs")
wtd.cor(tract$medianinc, tract$si_1000, weight = tract$pop)


#scatterplot: median income & race vs shutoffs
#HINT: try plotting Black share on the X-axis and median income on the Y-axis
#      experiment w/different aesthetics to indicate greater shutoff rates
#      e.g. focus on marker size, color, transparency, etc.
ggplot(data = tract, 
       aes(x = blackshare, 
           y = medianinc, 
           size = si_1000,
           color = si_1000)) + 
  geom_point(alpha = 0.1) + #alpha adjusts the transparency of points 
  scale_size(range = c(0.1,6), guide = "none")+
  scale_color_gradient(low ="blue", high="green")+
  ggtitle("Shutoffs per capita by median income and Black share")+
  xlab("Share of Black Residents")+
  ylab("Median Income")+
  labs(color = "Shutoff Intensity")
#HINT: try ?scale_size, this is a function to adjust the size aesthetic
#HINT: try ?scale_color_gradient for help creating a diverging color gradient

#for A5: 
#play around w/aesthetics including color, labels etc. to improve this plot


## -----------------------------------------------------------------------------
## 2.0 Time series plots of citywide shutoffs (aggregate monthly over all tracts)
## -----------------------------------------------------------------------------

#get total population of Detroit (for simplicity assume pop is time invariant)
#we estimate it by summing the `pop` column in the dataframe `tract`)
detroit_pop <- sum(tract$pop)

#first let's get citywide time series of shutoffs per capita
#aggregate tract-level observations in tract_ym into 1 obs for every month
ym <- tract_ym %>% 
  group_by(date) %>% 
  summarise(si_count = sum(si_count)) %>% 
  mutate(si_1000 = si_count / (detroit_pop / 1000) )

#use the transformed (aggregated) data 
ggplot(ym, aes(x = date, y = si_1000)) + 
  geom_line() 

#plot raw shutoff counts by manually specifying ggplot transformation
ggplot(tract_ym, aes(x = date, y = si_count)) +
  stat_summary(fun = sum, geom = "line")   


## -----------------------------------------------------------------------------
## 2.1 Time series plots of shutoffs by income level of Census tracts 
## -----------------------------------------------------------------------------

#get total population of Detroit tracts which are above/below median income
detroit_pop_hi_inc <- tract %>%
  filter(inc_above_median == 1) %>%
  summarise(sum(pop)) %>%
  as.numeric()

detroit_pop_lo_inc <- tract %>%
  filter(inc_above_median == 0) %>%
  summarise(sum(pop)) %>%
  as.numeric()


#time series of shutoffs per capita for tracts above/below citywide median inc
#what should the unit of observation be for this new data frame?
ym_inc <- tract_ym %>% 
  group_by(date,inc_above_median) %>% 
  summarise(si_count = sum(si_count)) %>%
  mutate(pop = if_else(inc_above_median == 1, 
                       detroit_pop_hi_inc, 
                       detroit_pop_lo_inc),
         si_1000 = si_count / (pop / 1000)) %>%
  na.omit()

#validation
ym_inc %>% 
  group_by(date) %>% 
  summarise(n = n()) %>% 
  filter(n != 2)

#note that we are missing 1 row (we only have 187 instead of 2 * 94 = 188 rows)
#the reason is that in Feb 2016 there is only 1 tract w/shutoffs (low-income)
#that means we are missing a row corresponding to Feb 2016, high income
#in order to solve this, here is one possible solution:

ym_inc <- tract_ym %>% 
  group_by(date,inc_above_median) %>% 
  summarise(si_count = sum(si_count)) %>%
  na.omit() %>% 
  ungroup() %>%
  complete(date, 
           inc_above_median, 
           fill = list(si_count = 0)) %>% #this fills in a new obs for Feb 2016
  mutate(pop = if_else(inc_above_median == 1, 
                       detroit_pop_hi_inc, 
                       detroit_pop_lo_inc),
         si_1000 = si_count / (pop / 1000)) 

#plot time series: separate lines for tracts above/below median income
ggplot(ym_inc, 
       aes(x = date, y = si_1000)) + 
  geom_line(aes(group = inc_above_median, 
                color = inc_above_median)) 
#QUESTION: why did we specify both group and color aes() arguments?


#QUESTION:
#should inc_above_median be treated as a numeric variable?
#what is a better way to store this information?

#ANSWER: let's convert to a factor that works better with plots
#turn inc_above_median into a factor with clear category labels
ym_inc$inc_above_median <- factor(ym_inc$inc_above_median,
                                  levels = c(0,1),
                                  labels = c("Below median income",
                                             "Above median income"))


#now plot time series by income group again using a factor for the color arg
ggplot(ym_inc, 
       aes(x = date, y = si_1000, color = inc_above_median)) + 
  geom_line()

#note that here the color aesthetic can be set in ggplot() or geom_line()
ggplot(ym_inc, 
       aes(x = date, y = si_1000)) + 
  geom_line(aes(color = inc_above_median)) +
  ggtitle("Time series plot of shutoffs by tracts above/median below median income")+
  xlab("Year")+
  ylab("Shutoff intensity")+
  labs(color="Tract Income")+
  scale_color_brewer(palette = "Set2")