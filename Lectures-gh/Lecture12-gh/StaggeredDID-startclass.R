################################################################################
##
## [ PROJ ] Difference-in-differences with Staggered Treatments 
## [ FILE ] StaggeredDID.R
## [ AUTH ] < YOUR NAME >
## [ INIT ] < Nov. 2024 >
##
################################################################################


## -----------------------------------------------------------------------------
## 1. load libraries
## -----------------------------------------------------------------------------

# Part 1 packages
  library(tidyverse)
  library(haven)
  library(hrbrthemes)
  library(ggsci)
  library(knitr)
  library(kableExtra)

# Part 2 packages
  library(fixest)
  library(ggfixest)
  library(bacondecomp)


## -----------------------------------------------------------------------------
## 2. Descriptive analysis
## -----------------------------------------------------------------------------

# prep data
  data <- haven::read_dta('https://github.com/scunning1975/mixtape/raw/master/castle.dta')
  data <- data %>% 
    rename(D = post) %>%
    group_by(state) %>% 
    mutate(evertreated = max(cdl)) %>%
    ungroup() %>% 
    select(state, year, sid, evertreated, D, treatment_date,
           homicide_c, population, popwt,
           blackm_15_24, whitem_15_24, blackm_25_44, whitem_25_44,
           l_exp_subsidy, l_exp_pubwelfare, l_police, unemployrt,
           poverty, l_income, l_prisoner, l_lagprisoner,
           r20001:r20104, lead1:lag5) %>% 
    mutate(lead6 = if_else((lead6 == 1 | lead7 == 1 | lead8 == 1 | lead9 == 1),
                           1, 0)) %>% 
    select(!c(r20004, r20014, r20024, r20034, r20044, r20054,
              r20064, r20074, r20084, r20094,
              r20101, r20102, r20103, r20104, 
              lead7, lead8, lead9))
  
  save(data, file = "castle_expanded2.rdata")
  

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
  data_wide <- data %>% 
    select(year, sid, state, hom_rate) %>% 
    pivot_wider(names_from = year, values_from = hom_rate) %>% 
    mutate(delta_hom_rate = `2010`-`2000`)
  
  LtoM <-colorRampPalette(c('red', 'yellow' ))          # The spectrum of colors for the lowest returns
  Mid <- "snow3"                                        # Snow3 is the color for the (approximately) median value
  MtoH <-colorRampPalette(c('lightgreen', 'darkgreen')) # The spectrum of colors for the highest values
  
  data_wide %>% 
    mutate(state = fct_reorder(state, delta_hom_rate)) %>%
    ggplot(aes(x = state, y = delta_hom_rate, fill = delta_hom_rate)) + 
    geom_bar(stat = "identity") +
    coord_flip() +
    scale_fill_gradient2(high = LtoM(100), 
                         mid = 'snow3',
                         low = MtoH(100), 
                         space ='Lab') +
    ylab("change in homicide rate (2000 to 2010)") +
    guides(fill="none")

      
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

twfe_did <- feols(FILL IN CODE) # weights = data$population
  
summary(twfe_did) 
  
  

## -----------------------------------------------------------------------------
## 5. The Two-Way Fixed Effects (TWFE) model
## -----------------------------------------------------------------------------  

# get relative time and inspect
  data_es <- data %>% 
    FILL IN CODE

  table(data_es$rel_year)
  data_es %>% 
    select(sid, state, year, treatment_date, rel_year, homicide_c, l_hom_rate)
  
  
# TWFE event study estimation
  twfe_es <- feols(FILL IN CODE)
  summary(twfe_es)               
  ggiplot(twfe_es,   # from the ggfixest package
          geom = 'errorbar',
          xlab = "Time to treatment",
          main = "Effect on ln(homicide rate)",
          theme = theme_minimal() )


## -----------------------------------------------------------------------------
## 5. The Goodman-Bacon Decomposition
## -----------------------------------------------------------------------------   

df_bacon <- bacon(FILL IN CODE)
  
coef_bacon <- FILL IN CODE
print(paste("Weighted sum of decomposition =", round(coef_bacon, 4)))

df_bacon %>% 
  ggplot(FILL IN CODE)
  
  
  
  
#-------------------------------------------------------------------------------  
# text for additional control variables: region x year dummies 
  # r20001 + r20002 + r20003 + r20011 + r20012 + r20013 + 
  # r20032 + r20033 + r20041 + r20042 + r20043 + 
  # r20051 + r20052 + r20053 + r20061 + r20062 + 
  # r20063 + r20071 + r20072 + r20073 + r20081 + 
  # r20082 + r20083 + r20091 + r20092 + r20093
  