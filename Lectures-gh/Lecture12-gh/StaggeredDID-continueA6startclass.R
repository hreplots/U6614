################################################################################
##
## [ PROJ ] Difference-in-differences with Staggered Treatments 
## [ FILE ] StaggeredDID.R
## [ AUTH ] < YOUR NAME >
## [ INIT ] < Nov. 2025 >
##
################################################################################


## -----------------------------------------------------------------------------
## 1. load libraries
## -----------------------------------------------------------------------------

# Part 1 packages
  library(tidyverse)
  library(haven)
  library(knitr)
  library(kableExtra)
  library(Manu)

# Part 2 packages
  library(fixest)
  library(ggfixest)
  library(bacondecomp)
  library(did)


## -----------------------------------------------------------------------------
## 2. Descriptive analysis
## -----------------------------------------------------------------------------

# 2.1

  load('castle_expanded2.rdata')
  
  data <- data %>% 
    mutate(hom_rate = homicide_c / (population / 100000),
           l_hom_rate = log(hom_rate),
           year = as.integer(year))
  
  save(data, file = "castle_modified.rdata")

  weighted.mean(data$hom_rate, data$popwt, na.rm = FALSE)
  weighted.mean(data$l_hom_rate, data$popwt, na.rm = FALSE)

  
# 2.2
  # load('castle_modified.rdata')
  
  table(data$year)
  n_distinct(data$year)

  
# 2.3
  data %>% 
    group_by(evertreated) %>% 
    summarise(n_distinct(state)) %>% 
    kbl() %>%
    kable_styling(full_width = F)
  
# 2.4
  data %>% 
    filter(year == 2000) %>% 
    group_by(evertreated) %>% 
    summarise(mean = mean(hom_rate),
              weighted_mean = weighted.mean(hom_rate, population))

# 2.5
  data %>% 
    filter(year == 2000) %>%
    ggplot(aes(x = hom_rate)) + 
    geom_histogram(binwidth = 1)
  
  data %>% 
    filter(year == 2000) %>% 
    ggplot(aes(x = l_hom_rate)) + 
    geom_histogram(binwidth = 0.25)
  
  
# 2.6

  # reshape from long to wide to get change over time
  data_wide <- data %>% 
    select(year, sid, state, hom_rate, evertreated) %>% 
    pivot_wider(names_from = year, 
                values_from = hom_rate) %>% 
    mutate(delta_hom_rate = `2010`-`2000`) %>% 
    mutate(evertreated = as.factor(evertreated))
  
  # get factor level labels for ever treated
  data_wide$evertreated <- fct_recode(data_wide$evertreated,
                                      "treated" = "1",
                                      "never treated" = "0")
  
  # barplot  
  data_wide %>% 
    mutate(state = fct_reorder(state, delta_hom_rate)) %>%
    ggplot(aes(x = state, y = delta_hom_rate, fill = as.factor(evertreated))) + 
    geom_bar(stat = "identity") +
    scale_fill_manual(values=c('orange', 'red')) +
    coord_flip() +
    ylab("change in homicide rate (2000 to 2010)") +
    guides(fill = guide_legend(title = "")) # remove legend title
  

      
## -----------------------------------------------------------------------------
## 3. The difference-in-differences (DID) design
## -----------------------------------------------------------------------------  
  
# 3.1
  dataFLtoNever <- data %>% 
    filter(state == "Florida" | evertreated == 0)
  
  table(dataFLtoNever$state, dataFLtoNever$evertreated)
  
# 3.2 
  dataFLtoNever.bytreat <- dataFLtoNever %>% 
    group_by(year, evertreated) %>% 
    summarise(hom_rate = weighted.mean(hom_rate, population),
              l_hom_rate = weighted.mean(l_hom_rate, population))
  head(dataFLtoNever.bytreat)
  
# 3.3
  # convert ever treated into a factor and year into a numeric variable for ggplot
  dataFLtoNever.bytreat <- dataFLtoNever.bytreat %>% 
    mutate(evertreated = factor(evertreated))
  
  # get factor level labels for ever treated
  dataFLtoNever.bytreat$evertreated <- fct_recode(dataFLtoNever.bytreat$evertreated,
                                                  "never treated" = "0",
                                                  "Florida" = "1")
  # plot
  dataFLtoNever.bytreat %>% 
    ggplot(aes(x = year, y = l_hom_rate,
               group = evertreated, 
               color = evertreated)) + 
    geom_line() + 
    geom_vline(xintercept = 2004, linetype = 'dashed', color = 'grey20', size = .8) +
    theme_minimal() +
    scale_color_manual(values = c("#8795E8", "#FF6AD5")) +
    ylab("ln(homicides per 100,000 population)") +
    guides(color = guide_legend(title = "")) +
    theme(legend.position="bottom") +
    scale_x_continuous(breaks = c(2001, 2003, 2005, 2007, 2009, 2011))
  
# 3.4
  data %>% 
    filter(treatment_date == 2006) %>% 
    group_by(state) %>% 
    summarise(first(treatment_date))
  
# 3.5
  
  # filter data from
  data2006toNever <- data %>% 
    filter(treatment_date == 2006 | evertreated == 0)
  table(data2006toNever$state, data2006toNever$evertreated)
  
  # collapse data to obtain weighted means for T and C groups
  data2006toNever.bytreat <- data2006toNever %>% 
    group_by(year, evertreated) %>% 
    summarise(hom_rate = weighted.mean(hom_rate, population),
              l_hom_rate = weighted.mean(l_hom_rate, population))
  
  # convert ever treated into a factor 
  data2006toNever.bytreat <- data2006toNever.bytreat %>% 
    mutate(evertreated = factor(evertreated))
  
  # get factor level labels for ever treated
  data2006toNever.bytreat$evertreated <- fct_recode(data2006toNever.bytreat$evertreated,
                                                  "never treated" = "0",
                                                  "2006 adopters" = "1")
  # plot
  data2006toNever.bytreat %>% 
    ggplot(aes(x = year, y = l_hom_rate,
               group = evertreated, 
               color = evertreated)) + 
    geom_line() + 
    geom_vline(xintercept = 2005, linetype = 'dashed', color = 'grey20', size = .8) +
    theme_minimal() +
    scale_color_manual(values = c("#8795E8", "#FF6AD5")) +
    ylab("ln(homicides per 100,000 population)") +
    guides(color = guide_legend(title = "")) +
    theme(legend.position="bottom") +
    scale_x_continuous(breaks = c(2001, 2003, 2005, 2007, 2009, 2011))
    

## -----------------------------------------------------------------------------
## 4. The Two-Way Fixed Effects (TWFE) model
## ----------------------------------------------------------------------------- 

twfe_did <- feols(l_hom_rate ~ D | factor(state) + factor(year), 
              data = data,
              vcov = "cluster") # weights = data$population
summary(twfe_did) 


# compare SE and conclusion with robust SEs:
twfe_did_rob <- feols(l_hom_rate ~ D | factor(state) + factor(year), 
                  data = data,
                  se = 'hetero') # weights = data$population
summary(twfe_did_rob) 
  
  

## -----------------------------------------------------------------------------
## 5. The TWFE event study model
## -----------------------------------------------------------------------------  

# mutate column for relative time and inspect
  
  
# TWFE event study estimation



## -----------------------------------------------------------------------------
## 5. The Goodman-Bacon Decomposition
## -----------------------------------------------------------------------------   

# get data frame with decomposition results


# show the weighted average of the decomp estimates = TWFE estimate


# plot decomposition
 


  
## -----------------------------------------------------------------------------
## 6. Callaway and Santa'Anna (2021)
## -----------------------------------------------------------------------------   

# recode treatment_date into new variable with 0s for nevertreated instead of NA
# (bc the att_gt() function identifies nevertreated units as 0s)
 

  
# estimate group-time average treatment effects
 

  # Aggregate ATT
 

  # Group-time ATTs
 
  
  # Plot group-time ATTs

  
  # Event-study
 
  
  # Plot event-study coefficients

  
#-------------------------------------------------------------------------------  
# text for additional control variables: region x year dummies 
  # r20001 + r20002 + r20003 + r20011 + r20012 + r20013 + 
  # r20032 + r20033 + r20041 + r20042 + r20043 + 
  # r20051 + r20052 + r20053 + r20061 + r20062 + 
  # r20063 + r20071 + r20072 + r20073 + r20081 + 
  # r20082 + r20083 + r20091 + r20092 + r20093
  