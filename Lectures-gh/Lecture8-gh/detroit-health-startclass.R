################################################################################
##
## [ PROJ ] Week 8: Water shutoffs, race, and health in Detroit (Part 2)
## [ FILE ] detroit-health.r
## [ AUTH ] < YOUR NAME >
## [ INIT ] < March 5, 2024 >
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
 
#install.packages("fixest")
#install.packages("multiwayvcov")
#install.packages("estimatr")
#install.packages("modelsummary")
#install.packages("tidycensus")

library(readstata13)  #imports .dta files from Stata 13 and up
library(tidyverse)
library(lubridate)
library(fastDummies)
library(weights)
library(lmtest)
library(sandwich)
library(multiwayvcov)
library(fixest)
library(estimatr)
library(modelsummary)
library(tidycensus)
library(ggrepel)
library(ggpmisc)

## ---------------------------
## directory paths
## ---------------------------

getwd()

#syntax for changing working directory 
setwd("Data") #go down to the data folder
getwd()

#go back up one level to where we started
setwd("..")   
getwd()


## -----------------------------------------------------------------------------
## 1. Zip-code analysis: gather all input data 
## -----------------------------------------------------------------------------

#A. Obtain zipcode-year demographic data from 5-year ACS via Tidycensus
      
  # ### THE FOLLOWING CODE IS COMMENTED OUT AS SAMPLE CODE FOR REFERENCE
  # 
  # # census_api_key("ENTER YOUR API KEY HERE")
  # 
  # # initialize new data frame to store ACS data from API requests
  # MI_acs_zip_11_17 <- data_frame()
  # 
  # #load variable descriptions to search for variable names
  # v15 <- load_variables(2015, "acs5", cache = TRUE)
  # #View(v15)
  # 
  # # set up a for loop for each year of data to access
  # for (i in 2011:2017) {
  # 
  #   # query ACS data from the census API for each year 2011-2017
  #   acs <- get_acs(geography = "zip code tabulation area",
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
  #            zip5 = as.character(GEOID)) %>%
  #     select(-GEOID)
  #   print(i)
  # 
  #   # append each year of data to a combined dataset
  #   MI_acs_zip_11_17 <- MI_acs_zip_11_17 %>%
  #     bind_rows(acs)
  # }
  # 
  # #clean input data
  # MI_acs_zip.clean <- MI_acs_zip_11_17 %>% 
  #   select(zip5, year, pop, medianinc, blackshare) %>% 
  #   mutate(zip5 = as.numeric(str_sub(zip5, -5, - 1)) )
  # 
  # #save data frame
  # saveRDS(MI_acs_zip.clean, file = "Data/MI_acs_zip.clean.rds")
  
  
  #load ACS data
  MI_acs_zip.clean <- readRDS("Data/MI_acs_zip.clean.rds") %>% 
    arrange(zip5, year)

  #double-check the unit of observation
  dim(MI_acs_zip.clean)
  head(MI_acs_zip.clean)
  


#B. Read in and join public health data to demographic data (by zip code)
  
  #read in public health data
    input_hcup_zip <- read.dta13("Data/hcup_total_&_viral.dta") 
    
    #data notes: public health outcome variables
      #total_obs: total hospitalizations
      #viral_infect: hospitalizations related to viral infections
 
   #what is unit of observation?
    table(input_hcup_zip$year, input_hcup_zip$month)
  
    
  #join public health data (input_hcup_zip) to demographic data (MI_acs_zip.clean)
    #join to end up with zipcode-month observations - how many should there be? 
    #also use mutate along with make_date to create a date variable(month_year)
    #sort by zip5, then year, then month
    joined_temp1 <- left_join(input_hcup_zip, 
                              MI_acs_zip.clean, 
                              by = c("zip5", "year")) %>% 
      mutate(month_year = make_date(year, month)) %>% 
      arrange(zip5, year, month)
  
    #inspect -- double-check unit of observation
      dim(joined_temp1)
      head(joined_temp1)
      table(joined_temp1$year, joined_temp1$month)
  
    

#C. Read in SI (service interruption) data and aggregate to zip code level

  #get service interruption data
  input_si <- read.dta13("Data/si_1017_cleaned.dta")
  
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
  
  
    
#D. join zipcode-month_year level SI data (si_zip_ym) to joined ACS/health data (joined_temp1)
  
  #Q: WHAT KIND OF JOIN? DIFFERENT WAYS TO APPROACH THIS...
    #HINT: start by thinking about the unit of obs you want to end up with
    #lesson w/join examples: https://hreplots.github.io/U6614/Lectures/Lecture4/Lecture4.1.html
    
    #one catch here: missing SI data (before 2018) should be 0's not missing
    
    joined_temp2 <- full_join(joined_temp1, 
                              si_zip_ym, 
                              by = c("zip5", "month_year")) %>% 
      filter(zip5 != 48225, zip5 != 48127)  %>%  
      #drop Harper Woods & Dearborn Heights (53 obs) bc they extend past Detroit
      mutate(si_count = replace_na(si_count, 0)) %>%
      #fill in SI count for months w/health data but no SI present
      rename(year = year.x, 
             month = month.x ) %>% 
      select(-month.y, -year.y) 

    #inspect
      summary(joined_temp2)
      joined_temp2 %>% filter(is.na(viral_infect)) %>% View()
    

  #generate quarter column for joining home vacancy data in next step
    joined_temp3 <- joined_temp2 %>% 
      mutate(quarter = 3 * as.numeric(cut(joined_temp2$month, 
                                      breaks = c(0,3,6,9,12), 
                                      labels = FALSE ) ) )

  
#E. Read in vacancy data and join to previously joined data (joined_temp3)

  input_vacancy_qtr <- read.dta13("Data/usps_hud_2010-2019_zip_quarter.dta")
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
  
    

#F. prep panel data: transform input vars to "rates" and get IDs for panel data
  
  zip_panel <- joined_temp4 %>% 
    mutate(total_obs_1000 = (total_obs/ pop) *1000,
           si_1000 = (si_count / pop) * 1000,
           viral_infect_1000 = (viral_infect / pop) * 1000,
           vac_res_p100 = (vac_res / total_res) * 100 ) %>% 
    mutate(ym = group_indices(joined_temp4, year, month),
           zip5_fac = as.factor(zip5),
           ym_fac = as.factor(ym)) 
  
  
  #get FE dummy for each year-month combination, and for each zipcode
    zip_panel <- dummy_cols(zip_panel, select_columns = c("ym", "zip5")) 

  
  
#G. Prepare cross-sectional data for comparative analysis
    #Collapse panel data to one observation for each zip code
    #eliminates variation within zip codes over time
    #left with between-zip code variation only ("cross-sectional" data)
  
  zip_cross <- joined_temp4 %>% 
    group_by(zip5) %>% 
    summarise(total_obs = sum(total_obs, na.rm = TRUE),
              si_count  = sum(si_count, na.rm = TRUE),
              viral_infect = sum(viral_infect, na.rm = TRUE),
              vac_res = mean(vac_res, na.rm = TRUE),
              total_res = mean(total_res, na.rm = TRUE),
              medianinc = mean(medianinc, na.rm = TRUE),
              pop = mean(pop, na.rm = TRUE)) %>% 
    mutate(total_obs_1000 = (total_obs / pop) *1000,
           si_1000 = (si_count / pop) * 1000,
           viral_infect_1000 = (viral_infect / pop) * 1000,
           vac_res_p100 = (vac_res / total_res) * 100 )
  
  #sample code if you want to save this as a dataframe or csv
    save(zip_cross, file="Data/zip_cross.rdata")
            
  #remove temporary data frames from environment (check the upper right pane)
    rm(joined_temp1, joined_temp2, joined_temp3, joined_temp4)
  

  
## -----------------------------------------------------------------------------
## 2. Cross-sectional: analyze health outcomes vs service interruptions
## -----------------------------------------------------------------------------

### for all regression analysis to follow, weight observations by pop
    
    
#total hospital admissions as dependent variable (with + without vacancy control)
  cross_total_1 <- lm(total_obs_1000 ~ si_1000, 
                      data = zip_cross, 
                      weight = pop)
  summary(cross_total_1)
  coeftest(cross_total_1, 
           vcov = vcovHC(cross_total_1, type="HC1")) #robust SEs
  
  #can also use lm_robust to get robust SEs
    cross_total_1alt <- lm_robust(total_obs_1000 ~ si_1000, 
                                  data = zip_cross, 
                                  weight = pop, 
                                  se_type = "stata")
    summary(cross_total_1alt)
  
  #QUESTION: Write out the PRF and interpret coefficient
  
    
  
  #add vacancy rate control variable
    cross_total_2 <- lm_robust(total_obs_1000 ~ si_1000 + vac_res_p100, 
                        data = zip_cross, 
                        weight = pop, 
                        se_type = "stata")
    summary(cross_total_2)


#plot cross-sectional data: shutoff rate vs. hospital admissions
  ggplot(data = zip_cross, 
         aes(x = si_1000, 
             y = total_obs_1000)) + 
    geom_point(aes(size = pop), alpha = 0.3) +
    scale_size(range = c(0.1, 6), 
               guide = "none") +
    geom_smooth(aes(weight = pop),
                method = 'lm_robust', #plots robust SEs for confidence band!
                formula = y ~ x) +    
    xlab("Shutoffs per 1,000 residents") +
    ylab("Hospitalizations per 1,000 residents")
  
  
#alternative syntax for estimating and plotting robust SEs
  ggplot(data = zip_cross, 
         aes(x = si_1000, 
             y = total_obs_1000)) + 
    geom_point(aes(size = pop), alpha = 0.3) +
    scale_size(range = c(0.1, 6), 
               guide = "none") +
    geom_smooth(aes(weight = pop),
                method = 'lm_robust', 
                formula = y ~ x,
                method.args = list(se_type = "stata")) + #robust SEs for confidence band 
    xlab("Shutoffs per 1,000 residents") +
    ylab("Hospitalizations per 1,000 residents")

#check correlation
  cor(zip_cross$si_1000, zip_cross$vac_res_p100, use = "pairwise.complete.obs")
  wtd.cor(zip_cross$si_1000, zip_cross$vac_res_p100, weight = zip_cross$pop)
  
  #QUESTION: what should we make of this association? think about internal validity.
  
  

## -----------------------------------------------------------------------------
## 3. Panel: analyze health outcomes vs service interruptions w/FEs
## -----------------------------------------------------------------------------

#estimation with FEs for zip5 (entity) and ym (time) [NO VACANCY RATE CONTROL]

  #QUESTION: write out the PRF we want to estimate
  
  
#Approach A: LSDV model
    
  #QUESTION: FILL IN MODEL FORMULA
  panel_total_lsdv_1 <- lm(FILL IN CODE)
  
    #view full set of results w/no SE adjustment
    summary(panel_total_lsdv_1) 
    
    #view results for coefficient of interest w/robust SEs 
      coeftest(panel_total_lsdv_1, 
               vcov = vcovHC(panel_total_lsdv_1, type = "HC1"))[2,] 
    
    #refer to adj r-squared
      summary(panel_total_lsdv_1)$adj.r.squared
    
  #QUESTION: what would happen if we didn't coerce zip5 into a factor?
  
    
  #clustered SEs by zip code (equivalent to areg in Stata)
  #if you're unfamiliar with clustered SEs, watch Quant II video lecture 5.2.b.
    panel_total_lsdv_1_clust <- coeftest(panel_total_lsdv_1,
                                         vcov = vcovCL,
                                         type = "HC1",
                                         cluster = ~zip5)
      
    #display results for coefficient of interest
      tidy(panel_total_lsdv_1_clust) %>% filter(term == "si_1000")
      
      panel_total_lsdv_1_clust[2,4] #refer directly to p-value for coefficient of interest
      
    


#Approach B: FE estimation (w/feols in the fixest package)
  
  #specify model with FEs for country and wave, w/ robust SEs
    panel_total_fe_1 <- feols(FILL IN CODE)
    
    summary(panel_total_fe_1)
    
    #alternative tidyverse way to access results for coefficient of interest
      tidy(panel_total_fe_1) %>% filter(term == "si_1000")
    
          
  #QUESTION: interpret coefficient on si_1000
  
      
  
  #cluster SEs by zipcode
    panel_total_fe_1_clust <- feols(total_obs_1000 ~ si_1000 |
                                      factor(zip5) + factor(ym),
                                    data = zip_panel,
                                    weights = zip_panel$pop,
                                    vcov = ~zip5)
    summary(panel_total_fe_1_clust) 
      #note the clustered SE differs ever so slightly from above method
      #why? it seems complicated, but here is one person's attempt to understand:
      #https://www.r-bloggers.com/2021/02/reghdfe-and-r-the-joys-of-standard-error-correction/
    
    #alternative tidyverse way to access results for coefficient of interest
      tidy(panel_total_fe_1_clust) %>% filter(term == "si_1000")
    
    

    #QUESTION: which approach to SE estimation seems most appropriate?
    
    
    
  #specify model with zipcode and monthly FEs + vacancy control, cluster SEs by zipcode
    panel_total_fe_2_clust <- feols(total_obs_1000 ~ si_1000 + vac_res_p100 |
                                      factor(zip5) + factor(ym), 
                                    data = zip_panel,
                                    weights = zip_panel$pop,
                                    vcov = ~zip5)
    summary(panel_total_fe_2_clust)
    

  #packages for displaying formatted regression output:
    #modelsummary: https://modelsummary.com/articles/modelsummary.html
    #stargazer: https://cran.r-project.org/web/packages/stargazer/vignettes/stargazer.pdf
    
    #packages and functions have the same name here:
      #modelsummary::modelsummary()
      #stargazer::stargazer()
    
    
  #compare model results
    models <- list(
      "1" = feols(total_obs_1000 ~ si_1000 | factor(zip5), 
                  data = zip_panel,
                  weights = zip_panel$pop, 
                  vcov = ~zip5),
      "2" = feols(total_obs_1000 ~ si_1000 | factor(zip5) + factor(ym), 
                      data = zip_panel,
                      weights = zip_panel$pop, 
                      vcov = ~zip5),
      "3" = feols(total_obs_1000 ~ si_1000 + vac_res_p100 | 
                    factor(zip5) + factor(ym), 
                      data = zip_panel,
                      weights = zip_panel$pop,
                      vcov = ~zip5),
      "4" = feols(total_obs_1000 ~ si_1000 + vac_res_p100 + medianinc | 
                    factor(zip5) + factor(ym), 
                  data = zip_panel,
                  weights = zip_panel$pop,
                  vcov = ~zip5)
      )
   modelsummary(models,
                output = "markdown", #use "latex" for knitting to pdf
                coef_omit = "Intercept",
                gof_map = c("nobs", "adj.r.squared", "vcov.type",
                            "FE: factor(zip5)", "FE: factor(ym)"),
                stars = c('*' = .1, '**' = .05, '***' = .01))
    
    save(models, file="m.Rdata")
  
    
#Plot model results 
   
  #plot relationship using pooled cross-sectional data
    ggplot(zip_panel, 
           aes(x = si_1000, y = total_obs_1000, weight = pop)) +
      geom_point(alpha = 0.4) +
      geom_smooth(method = 'lm_robust', 
                  formula = y ~ x,
                  method.args = list(se_type = "stata")) +
      xlab("Shutoffs per 1,000 residents") +
      ylab("Hospitalizations per 1,000 residents")
    

    #QUESTION: why doesn't this plot correspond to our FEs estimates?
      #HINT: in the above ggplot object, try mapping zip5.fac to the color aesthetic
    
    
  #can also plot binned data (though generally not desirable to do so)
  ggplot(zip_panel, 
         aes(x = si_1000, y = total_obs_1000, weight = pop)) +
    geom_point(alpha = 0.4) +
    stat_summary_bin(fun.y = 'mean', 
                     binwidth = 1,
                     color = 'orange', 
                     size=3, 
                     geom = 'point') +
    geom_smooth(method = 'lm_robust', 
                formula = y ~ x,
                method.args = list(se_type = "stata")) +
    xlab("Shutoffs per 1,000 residents") +
    ylab("Hospitalizations per 1,000 residents")
    
    #QUESTION: is there anything about this approach that seems misleading?
    
    
    
  #plot FE results by plotting residual shutoff rate vs residual hospital admission rate 
    # (residuals after accounting for FEs)
    # if you're confused about why, review pre-class Lesson 7 & Quant II Video Lecture 5.2.a on FEs
    
    zip_panel_feplot <- zip_panel %>% 
      filter(year > 2010) %>% 
      na.omit() 
    
    panel_total_y_feonly <- lm(total_obs_1000 ~ as.factor(zip5) + as.factor(ym),
                               data = zip_panel_feplot, 
                               weight = pop)
    panel_total_x_feonly <- lm(si_1000 ~ as.factor(zip5) + as.factor(ym),
                               data = zip_panel_feplot, 
                               weight = pop)
    
    zip_panel_feplot %>% 
      ggplot(aes(x = panel_total_x_feonly$residuals,
                 y = panel_total_y_feonly$residuals,
                 weight = pop,
                 label = zip5)) +
      geom_point(alpha = 0.2) +
      stat_dens2d_filter(geom = "text_repel", keep.fraction = 0.005) +
      geom_smooth(method = 'lm_robust', 
                  formula = y ~ x,
                  method.args = list(cluster = zip_panel_feplot$zip5_fac)) +
      xlab("Shutoff rate (residualized)") +
      ylab("Hospitalizations rate (residualized")
      #scale_x_continuous(limits = c(-10, 20)) 
      #can add this ggplot option to zoom in and investigate main cluster of data
      
    

