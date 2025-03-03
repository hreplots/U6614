################################################################################
##
## [ PROJ ] Supplemental Example: Women in STEM
## [ FILE ] womeninstem.r
## [ AUTH ] < YOUR NAME >
## [ INIT ] < Feb 13th, 2024 >
##
################################################################################

## Research question:
##  - Do countries with more mandated maternal leave have a greater share of 
##    women graduating in STEM fields?
##  - A more informative causal framing of that question:
##     Does maternal leave increase women's representation in STEM fields?

## Data source: World Bank's DataBank Gender Statistics
##  (https://databank.worldbank.org/source/gender-statistics)


##-------------------------
## load libraries
##-------------------------


#install.packages("ggrepel")
#install.packages("ggpmisc")

library(tidyverse)
library(ggrepel)
library(ggpmisc)

getwd()


##---------------------------
## 1. load & prep input data
##---------------------------

wbgender <- read_csv("worldbank-genderstats.csv", na = "..")

#wait this data isn't "tidy"! 
#each row is not its own observation! there are diff rows for diff vars
  
#here are the 3 variables we want to work with
  #SE.TER.GRAD.FE.SI.ZS : 
    #Y: Female share of graduates from STEM programmes, tertiary (%)
  #SH.MMR.LEVE :
    #X1: Length of paid maternity leave (calendar days)
  #SP.UWT.TFRT :
    #X2: Unmet need for contraception (% of married women ages 15-49) 


#first, let's keep *rows* with information for 3 variables:
  keepvars <- c("SE.TER.GRAD.FE.SI.ZS", "SH.MMR.LEVE", "SP.UWT.TFRT")
  stem <- wbgender %>% filter(`Series Code` %in% keepvars)
    #NOTE: use backticks to refer to `Series Code` bc of space in col name 
    #NOTE: the %in% operator checks if two vectors contain overlapping values 

  
##-------------------------------
## 2. prepare analysis data frame
##-------------------------------

## A. create 3 separate df's for each variable
##    - stem.fmshstemgrads, stem.dayspaidmatleave, stem.unmetcontr
##    - each df should be a subset of rows for each variable 
##    - look at the distribution of each variable in each year - any concerns?
  
  stem.fmshstemgrads <- wbgender %>% filter(`Series Code` == "SE.TER.GRAD.FE.SI.ZS")
  stem.dayspaidmatleave <- wbgender %>% filter(`Series Code` == "SH.MMR.LEVE")
  stem.unmetcontr <- wbgender %>% filter(`Series Code` == "SP.UWT.TFRT")
  
  summary(stem.fmshstemgrads)
  summary(stem.dayspaidmatleave)
  summary(stem.unmetcontr)
    #uh oh, way too many missing values in any given year!
    #obs come from diff surveys in diff years. values of vars may change slowly.
    #for now let's try using the mean over all years to reduce NAs
      #i.e. get mean fem share of STEM grads for every year (2011-2020)
  

## B. for each variable, create 'analysis variable' = mean value across all years for each country
##  - 1. start by reshaping data from wide to long form in each df using pivot_longer
##    - new long form df should have 1 obs for every country-year combination
##    - i.e. reshape stem.fmshstemgrads into new df stem.fmshstemgrads_long, etc.
##  - 2. next use group_by + summarise to generate aggregated stats for each group
##    - new df should have 3 columns: `Country Name`, `Country Code`, fmshstemgrads
##    - repeat for each input variable to end up with 3 data frames
##    - round to 2 decimal points if you need to

  #here's a basic pivot_longer example: 
    #https://statisticsglobe.com/pivot_longer-and-pivot_wider-functions-in-r
   
  stem.fmshstemgrads_cross <- stem.fmshstemgrads %>% 
    pivot_longer(cols = `2011 [YR2011]`:`2020 [YR2020]`,
                 names_to = "Year",       #ARG FOR TIME VARIABLE IN THIS EXAMPLE
                 values_to = "value") %>% #ARG FOR NEW COL NAME W/VARIABLE VALUES
    group_by(`Country Name`,`Country Code`) %>% 
    summarise(fmshstemgrads = round(mean(value, na.rm = TRUE),2) )
  
  stem.dayspaidmatleave_cross  <- stem.dayspaidmatleave  %>% 
    pivot_longer(cols = `2011 [YR2011]`:`2020 [YR2020]`,
                 names_to = "Year",
                 values_to = "value") %>% 
    group_by(`Country Name`,`Country Code`) %>% 
    summarise(dayspaidmatleave = round(mean(value, na.rm = TRUE), 2) )
  
  stem.unmetcontr_cross <- stem.unmetcontr %>% 
    pivot_longer(cols = `2011 [YR2011]`:`2020 [YR2020]`,
                 names_to = "Year",
                 values_to = "value") %>% 
    group_by(`Country Name`,`Country Code`) %>% 
    summarise(unmetcontr = round(mean(value, na.rm = TRUE), 2) ) 
  

  
  
## C. join 3 new df's together to get a single "tidy" dataframe
##    - include 3 analysis variables and `Country Name` and `Country Code`
##    - how many countries have non-missing values for all 3 vars?

  stem.cross <- inner_join(stem.fmshstemgrads_cross, stem.unmetcontr_cross) %>% 
    inner_join(stem.dayspaidmatleave_cross) %>% 
    na.omit()

  #think about sample selection issues! 
  #are missing observations for original variables randomly distributed?

    
  
##---------------------------
## 3. exploratory analysis
##---------------------------

## stem.cross has issues w/measurement error and non-random sample selection
## these issues limit the usefulness of this data for credible project work
## but we'll proceed a bit further for this in-class exercise 

  
## A. explore relationship between days paid maternity leave & fem share of STEM grads
    ggplot(stem.cross, 
           aes(x = dayspaidmatleave, 
               y = fmshstemgrads, 
               label = `Country Code`, 
               color = unmetcontr)) + 
      geom_point() + 
      # stat_dens2d_filter(geom = "text_repel", keep.fraction = 0.2) +
      scale_colour_gradient(low = "blue", high = "red") +
      ylab("Female share of graduates from STEM programs") + xlab("Days of paid maternity leave") +
      labs(color = "Unmet need \nfor contraception \n(% of married women \nages 15-49)") + 
      theme(legend.position = "right")
  
    
  #how would you describe this relationship?
    #how would you describe the variation that we're using? 
    
    cor(stem.cross$dayspaidmatleave, stem.cross$fmshstemgrads)
      #it's is positive (though doesn't appear to be linear)
      #we're using cross-sectional variation across countries

    
## B. could other country-level differences in part explain this relationship?
##    - what can you check in the data? 
    
    cor(stem.cross$dayspaidmatleave, stem.cross$unmetcontr)
    
    #surely there are other confounding factors! this is the usual problem w/cross-sectional variation
    #this cross-sectional variation in X (dayspaidmatleave) is endogenous!
    #i.e. it isn't random w/respect to other determinants of Y, such as unmet contraception need (X2)
    

## C. what can we do to improve internal validity?
    #i.e. we at least want to identify a more informative correlation, if not an arguably causal effect
    #trying to explicitly control for all of these other factors is usually an uphill battle
    #a stronger research design might focus on...
      #time variation "within-countries" (using panel data & fixed effects)
      #even better: exploit sharper policy changes giving variation in X    

  

  