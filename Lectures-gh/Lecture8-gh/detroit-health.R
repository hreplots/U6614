################################################################################
##
## [ PROJ ] Week 9: Water shutoffs, race, and health in Detroit (Part 2)
## [ FILE ] detroit-health.r
## [ AUTH ] < YOUR NAME >
## [ INIT ] < TBD >
##
################################################################################

## POLICY QUESTION FOR THIS WEEK & NEXT:
##
## 177,000 Detroit homes have had their water shut off for non-payment since 2010
##
## Last week: who is most affected by this problem?
##
## This week: what are the associated public health impacts?
##            - last week we created a tract-month panel
##            - the public health data is at the zipcode-month level


## ---------------------------
## libraries
## ---------------------------
 
#install.packages("plm")
#install.packages("multiwayvcov")

library(readstata13)  #imports .dta files from Stata 13 thru 15
library(tidyverse)
library(lubridate)
library(fastDummies)
library(plm)
library(weights)
library(lmtest)
library(multiwayvcov)


## ---------------------------
## directory paths
## ---------------------------

getwd()

#syntax for changing working directory 
setwd("Data") #go down to the data folder
getwd()

setwd("..")   #go back up one level to where we started
getwd()



## -----------------------------------------------------------------------------
## 1. Zip-code analysis: gather all input data 
## -----------------------------------------------------------------------------

#A. Get 1 observation for every zipcode for population, income, race variables

  #Read in zipcode-level demographic data from 2015 American Community Survey
    input_acs_zip <- read.dta13("data/ACS_13_17_5YR_Zip.dta")
  
  #get/rename variables
    acs_zip.clean <- input_acs_zip %>% 
      filter(year == 2015) %>% 
      select(geoid2, num_pop_total, num_income_median, per_race_black_alone_or_combo) %>% 
      rename(zip5 = geoid2, 
             pop2015 = num_pop_total, 
             share_black = per_race_black_alone_or_combo,
             med_inc = num_income_median) 
  
    #double-check the unit of observation
      dim(acs_zip.clean)
      head(acs_zip.clean)
  
    

#B. Read in and join public health data to demographic data (by zip code)
  
  #read in public health data
    input_hcup_zip <- read.dta13("data/hcup_total_&_viral.dta")
    
    #data notes: public health outcome variables
      #total_obs: total hospitalizations
      #viral_infect: hospitalizations related to viral infections
 
   #what is unit of observation?
    table(input_hcup_zip$year, input_hcup_zip$month)
  
    
  #join public health data (input_hcup_zip) to demographic data (acs_zip.clean)
  #Q: FILL IN THE CODE TO THE RIGHT OF THE ASSIGNMENT OPERATOR
    #join to end up with zipcode-month observations - how many should there be? 
    #lesson w/join examples: https://hreplots.github.io/U6614/Lectures/Lecture4/Lecture4.1.html
    #also use mutate along with make_date to create a date variable(month_year)
    #sort by zip5, then year, then month
    joined_temp1 <- left_join(input_hcup_zip, acs_zip.clean, by = "zip5") %>% 
      mutate(month_year = make_date(year, month)) %>% 
      arrange(zip5, year, month)
  
    #inspect -- double-check unit of observation
      dim(joined_temp1)
      head(joined_temp1)
      table(joined_temp1$year, joined_temp1$month)
  
    

#C. Read in SI (service interruption) data and aggregate to zip code level

  #get service interruption data
  input_si <- read.dta13("data/si_1017_cleaned.dta")
  
  #focus on variables needed to identify month/tract of every shutoff
  #we'll eventually want to join to demographic data to get zip5-month_year obs
  si.clean <- input_si %>% 
    select(si_order_number, zip5, year, month) %>% 
    arrange(zip5, year, month)
  
  #aggregate to zip5-month_year totals
  si_zip_ym <- si.clean %>% 
    group_by(zip5, year, month) %>% 
    summarise(si_count = n_distinct(si_order_number)) %>% 
    mutate(month_year = make_date(year, month)) %>% 
    arrange(zip5, year, month) 
  
  #Q: CHECK IF EVERY MONTH IS REPRESENTED IN THE SI DATA? 
      #IF NOT, HOW SHOULD WE TREAT MONTHS W/NO SHUTOFF DATA?
    table(si_zip_ym$year, si_zip_ym$month)
  
  
    
#D. join zipcode-date level SI data (si_zip_ym) to joined ACS/health data (joined_temp1)
  
  #Q: WHAT KIND OF JOIN? DIFFERENT WAYS TO APPROACH THIS...
      #HINT: start by thinking about the unit of obs you want to end up with
  
  #the catch here is that missing SI data before 2018 should be 0's not missing
  #one approach: a full_join w/joined_temp1 on the "left" bc it has every zip-date pair
    #that way we'll have dates w/no SI data, just have to fill in si_count as 0s
    joined_temp2 <- full_join(joined_temp1, si_zip_ym, by = c("zip5", "month_year")) %>% 
      filter(zip5 != 48225, zip5 != 48127)  %>%  
        #drop Harper Woods & Dearborn Heights (53 obs) bc they extend past Detroit
      mutate(si_count = replace_na(si_count, 0)) %>%
        #fill in SI count for months w/health data but no SI present
      rename(year = year.x, 
            month = month.x ) %>% 
      select(-month.y, -year.y) %>% 
      filter(year < 2018) #health data not available starting 2018
  
    #inspect
      summary(joined_temp2)
    

  #generate quarter column for joining home vacancy data in next step
    joined_temp3 <- joined_temp2 %>% 
      mutate(quarter = 3 * as.numeric(cut(joined_temp2$month, 
                                      breaks = c(0,3,6,9,12), 
                                      labels = FALSE ) ) )

  
#E. Read in vacancy data and join to previously joined data (joined_temp3)

  input_vacancy_qtr <- read.dta13("data/usps_hud_2010-2019_zip_quarter.dta")
    #data for zip codes in Detroit, or more than that?
    #no data for 48226 (downtown)
  
  #Q: WHAT KIND OF JOIN DO WE WANT
    #HINT: check out the vacancy data first
    summary(input_vacancy_qtr)
    table(input_vacancy_qtr$zip5, input_vacancy_qtr$year)
    joined_temp4 <- left_join(joined_temp3, input_vacancy_qtr, 
                              by = c("zip5", "year", "quarter"))
    #inspect
      dim(joined_temp4)
      head(joined_temp4)
  
    

#F. Panel data: transform input vars to rates and get IDs for panel data
  
  zip_panel <- joined_temp4 %>% 
    mutate(total_obs_1000 = (total_obs/ pop2015) *1000,
           si_1000 = (si_count / pop2015) * 1000,
           viral_infect_1000 = (viral_infect / pop2015) * 1000,
           vac_res_p100 = (vac_res / total_res) * 100 ) %>% 
    mutate(ym = group_indices(joined_temp4, year, month),
           zip5_fac = as.factor(zip5),
           ym_fac = as.factor(ym)) 
  
  
  #get FE dummy for each year-month combination, and for each zipcode
    zip_panel <- dummy_cols(zip_panel, select_columns = c("ym", "zip5")) 

  
  
#G. Also collapse panel data to one observation for each zip code
    #eliminates variation within zip codes over time
    #left with between-zip code variation only ("cross-sectional" data)
  
  zip_cross <- joined_temp4 %>% 
    group_by(zip5) %>% 
    summarise(total_obs = sum(total_obs, na.rm = TRUE),
              si_count  = sum(si_count, na.rm = TRUE),
              viral_infect = sum(viral_infect, na.rm = TRUE),
              vac_res = mean(vac_res, na.rm = TRUE),
              total_res = mean(total_res, na.rm = TRUE),
              pop2015 = mean(pop2015, na.rm = TRUE)) %>% 
    mutate(total_obs_1000 = (total_obs / pop2015) *1000,
           si_1000 = (si_count / pop2015) * 1000,
           viral_infect_1000 = (viral_infect / pop2015) * 1000,
           vac_res_p100 = (vac_res / total_res) * 100 )
  
  #sample code if you want to save this as a dataframe or csv
    save(zip_cross, file="data/zip_cross.rda")
    write.csv(zip_cross, "data/zip_cross.csv")
            
  #remove temporary data frames from environment (check the upper right pane)
    rm(joined_temp1, joined_temp2, joined_temp3, joined_temp4)
  

  
## -----------------------------------------------------------------------------
## 2. Cross-sectional: analyze health outcomes vs service interruptions
## -----------------------------------------------------------------------------

#total hospital admissions as dependent variable (with + without vacancy control)
  cross_total_1 <- lm(total_obs_1000 ~ si_1000, 
                      data = zip_cross, weight = pop2015)
  summary(cross_total_1)
  coeftest(cross_total_1, vcov = vcovHC(cross_total_1, type="HC1")) #robust SEs
  
  #QUESTION: Write out the PRF and interpret coefficient
  
  
  
  cross_total_2 <- lm(total_obs_1000 ~ si_1000 + vac_res_p100, 
                      data = zip_cross, weight = pop2015)
  summary(cross_total_2)
  coeftest(cross_total_2, vcov = vcovHC(cross_total_2, type="HC1")) #robust SEs
  

#plot cross-sectional data: shutoff rate vs. hospital admissions (weight by pop2015)
  ggplot(data = zip_cross, 
         aes(x = si_1000, y = total_obs_1000, size = pop2015, weight = pop2015)) + 
    geom_point(alpha = 0.3) +
    scale_size(range = c(0.1, 6), guide = "none") +
    geom_smooth(method = 'lm', formula = y ~ x)

  #check correlation
  cor(zip_cross$si_1000, zip_cross$vac_res_p100, use = "pairwise.complete.obs")
  wtd.cor(zip_cross$si_1000, zip_cross$vac_res_p100, weight = zip_cross$pop2015)
  
  #QUESTION: what should make of this association? think about internal validity.
  
  

## -----------------------------------------------------------------------------
## 3. Panel: analyze health outcomes vs service interruptions w/FEs
## -----------------------------------------------------------------------------

#estimation with FEs for zip5 (entity) and ym (time) [NO VACANCY RATE CONTROL]

  #Approach A: LSDV model
    
    #QUESTION: FILL IN MODEL FORMULA
    panel_total_1 <- lm(total_obs_1000 ~ si_1000 + as.factor(zip5) + as.factor(ym), 
                        data = zip_panel, 
                        weight = pop2015)
    summary(panel_total_1)
    panel_total_1$coefficients[1:2]
    summary(panel_total_1)$adj.r.squared
    
    #QUESTION: what would happen if we didn't coerce zip5 into a factor?
    
    
    #robust SEs
      coeftest(panel_total_1, vcov = vcovHC(panel_total_1, type = "HC1"))[2,]
    
    #clustered SEs by zip code (equivalent to areg in Stata)
      #stats for coefficient of interest is the 2nd element in this object. 
      panel_total_1_vcov <- cluster.vcov(panel_total_1, 
                                 cbind(zip_panel$zip5),
                                 df_correction = T) #a small sample size adjustment
      
      #just report stats for coefficient of interest (the second row)
      coeftest(panel_total_1, panel_total_1_vcov)[2,] 
    
    
  #Approach B: FE estimation (w/plm package)
      
    panel_total_1b <- plm(total_obs_1000 ~ si_1000, 
                        data = zip_panel,
                        index = c("ym", "zip5"), 
                        effect = "twoways",
                        model = "within",
                        weight = pop2015 )
    summary(panel_total_1b)
      #note 1: here the R-squared stat is not for the full model!
      #note 2: can't estimate robust SEs using weighted plm models
    
    #QUESTION: Write out the PRF and interpret coefficient
  
  

#estimation with FEs for zip5 (entity) and ym (time) + control for vacancy rate
    
  #Approach A: LSDV model
    #try replicating this with Approach B (FE estimation w/plm package)
    
    panel_total_2 <- lm(total_obs_1000 ~ si_1000 + vac_res_p100 + as.factor(zip5) + as.factor(ym),
                      data = zip_panel, 
                      weight = pop2015)
    summary(panel_total_2)
    summary(panel_total_2)$adj.r.squared
    coeftest(panel_total_2, vcov = vcovHC(panel_total_2, type = "HC1"))[2,]
  
    #clustered SEs by zip code (equivalent to areg w cluster option in Stata)
      #stats for coefficient of interest is the 2nd element in this object. 
      panel_total_2_vcov <- cluster.vcov(panel_total_2, 
                                         cbind(zip_panel$zip5),
                                         df_correction = T) #a small sample size adjustment
      
      #just report stats for coefficient of interest (the second row)
      coeftest(panel_total_2, panel_total_2_vcov)[2,] 

    
    
#plot model results 
   
  #plot relationship using pooled cross-sectional data
    ggplot(zip_panel, aes(x = si_1000, y = total_obs_1000, weight = pop2015)) +
      geom_point() +
      geom_smooth(method = 'lm', formula = y ~ x, se = FALSE)
    
    #QUESTION: why doesn't this plot correspond to our FEs estimates?
    
    
    #can also plot binned data
    ggplot(zip_panel, aes(x = si_1000, y = total_obs_1000, weight = pop2015)) +
      geom_point(alpha = 0.4) +
      stat_summary_bin(fun.y = 'mean', binwidth = 1,
                       color = 'orange', size=3, geom = 'point') +
      geom_smooth(method = 'lm', formula = y ~ x, se = FALSE)
    
      #QUESTION: why do you think this approach could be considered misleading?
    
    
  #plot FE results by plotting residual shutoff rate vs residual hospital admission rate 
    #(residuals after accounting for FEs)
    #if you're confused about why, review pre-class Lesson 7 & Quant II Video Lecture 5.2.a on FEs
    
    panel_total_y_feonly <- lm(total_obs_1000 ~ as.factor(zip5) + as.factor(ym), 
                            data = zip_panel, 
                            weight = pop2015)
    panel_total_x_feonly <- lm(si_1000 ~ as.factor(zip5) + as.factor(ym), 
                               data = zip_panel, 
                               weight = pop2015)

    ggplot(data = zip_panel, aes(x= panel_total_x_feonly$residuals, 
                                 y = panel_total_y_feonly$residuals, 
                                 weight = pop2015)) +
      geom_point(alpha = 0.2) +
      geom_smooth(method = 'lm', formula = y ~ x, se = FALSE)

