################################################################################
##
## [ PROJ ] Lecture 3: Subway Fare Evasion Arrests in Brooklyn
## [ FILE ] Lecture3-recitation.r
## [ AUTH ] < YOUR NAME >
## [ INIT ] < Sept 19, 2024 >
##
################################################################################

## POLICY QUESTION FOR THE NEXT 3 CLASSES:
## Police can stop and ticket or arrest people for subway fare evasion. 
## Is there racial bias in their subway fare evasion enforcement in Brooklyn? 

## Week 3: What can we learn from microdata?

## Please consult the 2-page data primer before continuing
## https://hreplots.github.io/U6614/Lectures-gh/Lecture3-gh/lecture_3-6_data_primer.pdf


## ---------------------------
## 1. load libraries
## ---------------------------

#install.packages("fastDummies")

library(tidyverse)
library(fastDummies)
#library(ggplot2) #included with Tidyverse
#library(forcats) #included with Tidyverse


#also confirm correct working directory
getwd()

## -----------------------------------------------------------------------------
## 2. load datasets using read_csv() and inspect
##
##    two datasets of administrative records from public defenders in Brooklyn
##      1. microdata_BDS_inclass.csv: Brooklyn Defender Services
##      2. microdata_LAS_inclass.csv: Legal Aid Society
##
##    each row contains client data for an individual arrested for subway fare evasion
##
##    note these files were provided by the respective orgs with no documentation
##
##  a. give a brief overview of the data 
##     (start to think about this now, but revisit after you know what
##      information proved most relevant for your analysis)
##
##  b. what is the unit of observation? what population does this `sample` represent?
##     do you think this sample does a good job representing the population of interest?
##
##  c. inspect and describe the coding of race/ethnicity information in each dataset
##
##  d. from the outset, are there any data limitations you think are important to note? 
##     - note that messy data isn't a limitation, it just means we have work to do.
##     - limitations might include information that is missing in this data/sample

## -----------------------------------------------------------------------------


#reading in blanks as NA
  arrests_bds <- read_csv("microdata_BDS_inclass.csv", na = "")
  arrests_las <- read_csv("microdata_LAS_inclass.csv", na = "")
  
  str(arrests_bds)
  str(arrests_las)


#note that string variables are imported as characters by default (not factors)
#let's convert race and ethnicity to factors to inspect before cleaning
  arrests_bds <- arrests_bds %>% 
    mutate(race = as.factor(race), 
           ethnicity = as.factor(ethnicity) )
  
  arrests_las <- arrests_las %>% 
    mutate(race = as.factor(las_race_key), 
           ethnicity = as.factor(hispanic_flag) )
  
  
#2a.


#2b.
  

#2c.
  
  #compare race coding
    summary(arrests_bds$race)
    summary(arrests_las$race)
    
  #compare Hispanic/ethnicity coding
    summary(arrests_bds$ethnicity)
    summary(arrests_las$ethnicity)


#2d.
  


## -----------------------------------------------------------------------------
## 3. clean BDS race and ethnicity data: 
##
##  recode race and ethnicity vars and assign to new df called arrests_bds.clean
##  generate 3 new columns for cleaned data:
##    a. race_clean
##    b. ethnicity_clean
##    c. create a single factor variable w/mutually exclusive groups (race_eth)
##       - Non-Hispanic Black, Non-Hispanic White, Hispanic, Asian/Pacific Islander, Other, NA
##    
##  before we do this...
##    
##  in-class discussion questions: 
##  - what cat do you want to use for Black Hispanic identity?
##  - what is missing by using mutually exclusive, single identity categories?
## -----------------------------------------------------------------------------

#3a. BDS race

  #inspect
  #NOTE: don't show all of this in your R Markdown submission, only what is needed
    levels(arrests_bds$race)
    typeof(arrests_bds$race) #remember factors are stored as integers corresponding to levels
    summary(arrests_bds$race)
    arrests_bds %>% count(race, sort = TRUE)
    arrests_bds %>% count(race, ethnicity, sort = FALSE)
    
    #a quick and easy way to show crosstabs using base R (just show this!)
      table(arrests_bds$race, arrests_bds$ethnicity, useNA = "always")
    
      
  #ok now let's recode in an internally consistent manner
    #recode 0 and Unknown into NA
    #recode Am Indian as Other because only 1 observation
    #assign to race_clean as a factor w/correct race groups
    arrests_bds.clean <- arrests_bds %>% 
      mutate(race_clean = recode(race, "0" = "NA", 
                                       "Unknown" = "NA", 
                                       "Am Indian" = "Other" ) )  %>% 
      mutate(race_clean = factor(race_clean, 
                                 levels = c("Black", "White", "Asian/Pacific Islander", "Other")))
  
      #validation: confirm the recode worked as intended
        arrests_bds.clean %>% count(race_clean, sort = TRUE)
        table(arrests_bds.clean$race_clean, arrests_bds.clean$race, useNA = "always")
      
    #NOTE: recode doesn't always work well with pipes like other dplyr functions,
    #      instead we use the function as a part of the argument to mutate()


#3b. BDS ethnicity
      
  #inspect
    levels(arrests_bds.clean$ethnicity)
    table(arrests_bds.clean$race_clean, arrests_bds.clean$ethnicity, useNA = "always")
  
  #now let's recode by creating a hispanic column where:
  #1. hispanic takes the values Hispanic, Non-Hispanic, or NA
  #2. then use factor() to set levels based on the above values
    
    arrests_bds.clean <- arrests_bds.clean %>% 
      mutate(hispanic = recode(ethnicity, "0" = "NA",
                               "Other" = "Non-Hispanic")) %>% 
      mutate(hispanic = factor(hispanic, 
                               levels = c("Hispanic", "Non-Hispanic"))) 
    
    #validation: confirm the recode worked as intended
    table(arrests_bds.clean$hispanic, arrests_bds.clean$ethnicity, useNA = "always")
    summary(arrests_bds.clean$hispanic) #ok but less useful for validation!
      
  #NOTE: we used separate pipes to clean ethnicity & race, could do in a single pipe
      

#3c. race_eth
      
  #let's investigate a bit
    table(arrests_bds.clean$race_clean, arrests_bds.clean$hispanic, useNA = "always")
    prop.table(table(arrests_bds.clean$race_clean, arrests_bds.clean$hispanic), 2)  %>% round(2)
  
  #recoding with different data types and/or NA values can be tricky 
  #let's start by converting factors to characters using the as.character()
    arrests_bds.clean <- arrests_bds.clean %>% 
      mutate(race_clean_char = as.character(race_clean)) %>% #work with characters
      mutate(hispanic_char = as.character(hispanic))     %>% #work with characters
      mutate(race_eth = ifelse(hispanic_char %in% "Hispanic", #use %in% so that NAs are evaluated
                               hispanic_char, 
                               race_clean_char) ) %>%  
      mutate(race_eth = as.factor(recode(race_eth, 
                                         "White" = "Non-Hispanic White",
                                         "Black" = "Non-Hispanic Black"))) %>%
      select(-race_clean_char, -hispanic_char)        
    
  #validate results
    
    #joint distribution of race_eth and hispanic
      table(arrests_bds.clean$race_eth, arrests_bds.clean$hispanic, useNA = "always")
    
    #distribution of race_eth
      arrests_bds.clean %>% count(race_eth, sort = TRUE)
    
  
## -----------------------------------------------------------------------------
## 4a. Repeat step 2 for LAS:
##        - create race_eth in arrests_las with the same coding as for BDS
##      NOTE: Hispanic identity is included in both las_race_key & hispanic_flag
##    
##  b. Make sure you end up with a data frame with the following variable names,
##      and identical coding as in arrests_bds_clean:
##     - race_eth, age, male, dismissal (not in the BDS data), st_id, loc2
## -----------------------------------------------------------------------------

#4a.
      
  #preliminary inspection
  table(arrests_las$race, arrests_las$ethnicity, useNA="always")
      
  #recoding: 2-step process we used for the BDS data is combined into 1 step here
  arrests_las.clean <- arrests_las %>% 
    mutate(race_eth = recode(las_race_key, 
                             "Asian or Pacific Islander" = "Asian/Pacific Islander",
                             "Unknown" = "NA",
                             "Latino" = "Hispanic",
                             "White" = "Non-Hispanic White",
                             "Black" = "Non-Hispanic Black")) %>% 
    mutate(race_eth = ifelse(hispanic_flag %in% "Y",
                             "Hispanic", 
                             race_eth)) %>% 
    mutate(race_eth = factor(race_eth, levels=c("Non-Hispanic Black", 
                                                "Hispanic",
                                                "Non-Hispanic White",
                                                "Asian/Pacific Islander",
                                                "Other")))

#4b. validate with relevant crosstabs
  arrests_las.clean %>% count(race_eth, sort = TRUE)
  table(arrests_las.clean$race_eth, arrests_las.clean$hispanic_flag, useNA = "always")


## -----------------------------------------------------------------------------
## 5. Append BDS and LAS microdata -- stack rows with bind_rows()
##
##    a. create a column (pd) to identify PD data source (= "las" or "bds")
##
##    b. Append arrests_bds.clean and arrests_las.clean
##        - use bind_rows from the dplyr package
##        - store combined data as new data frame called arrests.clean
##        - only keep columns for pd, race_eth, age, male, dismissal, st_id, loc2,
##          converting to factors for columns w/categorical data as needed
##        - inspect race_eth for accuracy/consistency
##        - store as new data frame arrests.clean
##
##    c. use the nrow function to display the total number of arrests
##
##    d. Save arrests.clean df as an .RData file in a new Lecture4 folder for next week
##
## -----------------------------------------------------------------------------

#5a.
  arrests_bds.clean <- arrests_bds.clean %>% mutate(pd = "bds")
  arrests_las.clean <- arrests_las.clean %>% mutate(pd = "las")

#5b.
  arrests.clean <- bind_rows(arrests_las.clean, arrests_bds.clean) %>%
    mutate(pd = as.factor(pd),
           st_id = as.factor(st_id), #station/location info is not continuous
           loc2 = as.factor(loc2)) %>% 
    select(pd, race_eth, age, male, dismissal, st_id, loc2)
  summary(arrests.clean)

#5c. 
  
  
#5d.
  #save(LIST DATA OBJECTS TO SAVE HERE SEPARATED BY COMMAS,
  #file = "arrests.clean.RData")  
  
  #for future reference, can also write to a csv file:
  #write_csv(arrests_all, "arrests_all.csv") 

  
## -----------------------------------------------------------------------------
## 6. Descriptive statistics by race_eth (grouping)
##
##    a. group arrests.clean by race_eth,
##       show arrest counts for each race_eth category using tidyverse functions
##        Note: we already obtained this information using the summary command,
##        but sometimes it's useful to store this information to work with
##
##    b. show a table with the proportion of total arrests in each race_eth category
##
##    c. compute avg age, share male, and dismissal rate for each race_eth group, 
##       along with the total sample size. 
##       also try to include the sample size for the dismissal variabe as well
##        (just the number of non-NA observations for dimissal)
##        - store as race_eth_stats)
##        - HINT: similar to (a) above, but specify different stats in summarise()
##        - HINT: for most of the requested stats, you'll need to tell R to ignore NA values
##
##    d. what, if anything, do you think is interesting to note about the
##       distribution of:
##        - age by race (conditional distribution of age)
##        - male share by race
##        - dismissal by race
## -----------------------------------------------------------------------------

#6a. here are a few different approaches
  
  #tidyverse approach with summarise()
    arrests.clean %>%
      group_by(race_eth) %>%
      summarise(n = n()) %>%
      arrange(desc(n))
    
  #tidyverse with count()
    arrests.clean %>% count(race_eth, sort = TRUE)
 
  #base R with table()
    table(arrests.clean$race_eth, useNA = "always")
    
  #another way, if we wanted to exclude NAs (which we don't right now!)
    arrests.clean %>% 
      filter(!is.na(race_eth)) %>% 
      count(race_eth, sort = TRUE)

#6b.
 

  #a nicer looking version
  

  #if we want to exclude NAs
    

  
#6c. let's store results as a new data frame in case you want to refer to them in your write-up
  

#6d. 


  
## -----------------------------------------------------------------------------
## 7. grouping by subway station, with subway-station level statistics.
##      our data includes the following columns
##        - st_id: unique subway station identifier
##        - loc2: subway station name (recognized by google maps)
##      for the remainder of this Assignment, group by loc2
##   
##    a. use dummy_cols() in the fastDummies package to create dummies for each race_eth cat
##       and show the mean for each category
##
##    b. using group_by(), create a new data frame w/station-level observations,
##       call it arrests_stations and including the following information:
##      - station name (given by loc2)
##      - st_id
##      - total number of arrests at each station
##      - total number of arrests for each race_eth category at each station
##      - sort in descending order of total number of arrests
##      - only show the top 10 stations 
##
##    c. create a new data frame called arrests_stations_top with the following information
##      - the combined total number of Black and Hispanic arrests (call it n_bh)
##      - the number of arrests with race/ethnicity coded as NA
##      - sh_bh = share of arrests that are Black and Hispanic (excluding race_eth = NA from denominator)
##      - sort in ascending order of Black and Hispanic arrest share
##      - only show for stations with at least 50 total arrests
##      - use kable() in the knitr package for better formatting
##        - HINT: save as a data frame and pass as an argument to knitr::kable()
##
##    d. briefly summarize any interesting findings about the distribution of race across stations
##      - hint: are there any high arrest stations w/a high share of NHW arrests?
## -----------------------------------------------------------------------------

    arrests.clean <- dummy_cols(arrests.clean, select_columns = "race_eth")
    str(arrests.clean)
    summary(arrests.clean[,8:13]) #too clunky, don't show this in your submission!
    
    #what's a better way to show just the means using summarise()?
    arrests.clean %>% FILL IN CODE
    
    #7b. use group_by to get counts by station
    arrests.clean %>% FILL IN CODE
    
    #let's generate station-level counts for each race_eth group
    #general approach: sum dummy variables
    arrests_stations <- arrests.clean %>%  
      FILL IN CODE
    
    
    #7c. 
    arrests_stations_top <-  FILL IN CODE
    
    
    #7d.
    
  

## -----------------------------------------------------------------------------
## 8. OPTIONAL: barplots 
##
##    a. barplot of all arrests by race_eth category (proportions of all arrests, sorted)
##
##    b. same as part a but exclude all observations with race_eth == NA
##
##    c. stacked barplot showing race_eth breakdown for top 10 stations by total arrest count
## -----------------------------------------------------------------------------

#8a. 
  ggplot(data = arrests.clean, aes(x = race_eth)) + geom_bar()
  ggplot(data = arrests.clean, aes(x = fct_infreq(race_eth), y = ..prop.., group = 1)) + geom_bar() + 
    scale_y_continuous(labels = scales::percent_format()) 

  
#8b.
  arrests.clean.nomiss <- arrests.clean %>% filter(is.na(race_eth) == FALSE)
  summary(arrests.clean.nomiss$race_eth)
  
  ggplot(data = arrests.clean.nomiss, aes(x = race_eth)) + geom_bar()
  ggplot(data = arrests.clean.nomiss, aes(x = fct_infreq(race_eth), y = ..prop.., group = 1)) + geom_bar() + 
    scale_y_continuous(labels = scales::percent_format())


#8c.
  arrests_stations_race_top <- arrests.clean %>%  
    group_by(loc2) %>% 
    mutate(st_arrests = n()) %>% 
    ungroup() %>% 
    group_by(loc2, race_eth)  %>%
    summarise(arrests = n(), st_arrests = first(st_arrests)) %>% 
    arrange(desc(st_arrests)) %>% 
    filter(st_arrests > 100)
  arrests_stations_race_top
  save(list = "arrests_stations_race_top", "../Lecture3/arrests_stations_race_top.RData")
  
  ggplot(arrests_stations_race_top, aes(x = reorder(loc2, -st_arrests), y = arrests, fill = race_eth)) + 
    geom_bar(stat = "identity") + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
