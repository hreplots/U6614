################################################################################
##
## [ PROJ ] Lecture 2: Subway Fare Evasion Arrests in Brooklyn Microdata Analysis
## [ FILE ] Lecture2-inclass.r
## [ AUTH ] < YOUR NAME >
## [ INIT ] < Jan 27, 2026 >
##
################################################################################

## POLICY QUESTION FOR THE NEXT 3 CLASSES:
## Police can stop and ticket or arrest people for subway fare evasion. 
## Is there racial bias in their subway fare evasion enforcement in Brooklyn? 

## Week 3: What can we learn from microdata?
##  - Which demographic groups experience the most enforcement?
##  - Where is the NYPD making the most subway fare evasion arrests?

## Please consult the data primer for an overview of the data and policy context


## -----------------------------------------------------------------------------
## 1. load libraries and check working directory
## -----------------------------------------------------------------------------

#install.packages("fastDummies")
library(fastDummies)
library(tidyverse)
#library(forcats) #included with Tidyverse


#also confirm correct working directory
getwd()


## -----------------------------------------------------------------------------
## 2. Load, inspect and describe the two public defender client datasets 
##
##  a. load datasets using read_csv() and inspect
##
##    two datasets of administrative records from public defenders in Brooklyn
##      1. microdata_BDS_inclass.csv: Brooklyn Defender Services
##      2. microdata_LAS_inclass.csv: Legal Aid Society
##
##    each row contains client data for an individual arrested for subway fare evasion
##
##    note these files were provided by the respective orgs with no documentation
##
##  b. give a brief overview of the data 
##     (start to think about this now, but revisit after you know what
##      information proved most relevant for your analysis)
##
##  c. what is the unit of observation? what population does this `sample` represent?
##     do you think this sample does a good job representing the population of interest?
##
##  d. inspect and describe the coding of race/ethnicity information in each dataset
##
##  e. are there any data limitations you think are important to note from the outset? 
##     - note that messy data isn't a limitation, it just means we have work to do.
##     - limitations might include information that is missing in this data/sample
##
## -----------------------------------------------------------------------------

# 2a. 

  # reading in blanks as NA
    arrests_bds <- read_csv("microdata_BDS_inclass.csv", na = "")
    arrests_las <- read_csv("microdata_LAS_inclass.csv", na = "")
    
    str(arrests_bds, give.attr = FALSE) 
    str(arrests_las, give.attr = FALSE)
    # note the give.attr argument to make long str() output more readable
  
  # recall string variables are imported as characters by default (not factors)
  # this isn't useful for summary statistics
    summary(arrests_bds)
    summary(arrests_las)
  
  # let's convert race and ethnicity to factors to inspect before cleaning
    arrests_bds <- arrests_bds %>% 
      mutate(race = as.factor(race), 
             ethnicity = as.factor(ethnicity))
    
    arrests_las <- arrests_las %>% 
      mutate(race = as.factor(las_race_key), 
             ethnicity = as.factor(hispanic_flag))

# 2b.
  

# 2c.
  
  # compare race coding
    summary(arrests_bds$race)
    summary(arrests_las$race)
    
  # compare Hispanic/ethnicity coding
    summary(arrests_bds$ethnicity)
    summary(arrests_las$ethnicity)


# 2d.
  


## -----------------------------------------------------------------------------
## 3. clean BDS race and ethnicity data: 
##
##  recode race and ethnicity vars and assign to new df called arrests_bds.clean
##  generate 3 new columns for cleaned data:
##    a. race_clean
##    b. ethnicity_clean
##    c. create a single factor variable w/mutually exclusive groups (race_eth)
##       - Black, Non-Hispanic White, Hispanic, Asian/Pacific Islander, Other, NA
##    
##  before we do this...
##    
##  in-class discussion questions: 
##  - what cat do you want to use for Black Hispanic identity?
##  - what is missing by using mutually exclusive, single identity categories?
## -----------------------------------------------------------------------------

# 3a. BDS race

  # inspect
  # NOTE: don't show all of this in your R Markdown submission, it's just for yoU!
    levels(arrests_bds$race)
    typeof(arrests_bds$race) #remember factors are stored as integers corresponding to levels
    summary(arrests_bds$race)
    arrests_bds %>% count(race, sort = TRUE)
    arrests_bds %>% count(race, ethnicity, sort = FALSE)
    
    # a quick and easy way to show a crosstab using base R (just show this!)
      table(arrests_bds$race, 
            arrests_bds$ethnicity, 
            useNA = "always")
      # NOTE: why set useNA = "always" here?
      
      
  # ok now let's recode in an internally consistent manner...
    
  # Approach #1: use recode() in the dplyr package
    
    # recode 0 and Unknown into NA (use use NA_character)
    # recode Am Indian as Other because only 1 observation
    # assign to race_clean as a factor w/correct race groups
    
    # note the original ordering of the levels
    # let's assign Other to be last in the ordering of levels
    
    arrests_bds.clean <- arrests_bds %>% 
      mutate(race_clean = recode(race,
                                 "0" = NA_character_, # use NA_character 
                                 "Unknown" = NA_character_, 
                                 "Am Indian" = "Other")) %>% 
      mutate(race_clean = fct_relevel(race_clean,
                                      "Asian/Pacific Islander",
                                      "Black",
                                      "White",
                                      "Other"))
      
  # Approach #2: use case_when() in the dplyr package
    
    arrests_bds.clean <- arrests_bds %>% 
      mutate(race_clean = case_when(race == "0" ~ NA_character_,
                                    race == "Unknown" ~ NA_character_,
                                    race == "Am Indian" ~ "Other",
                                    TRUE ~ race))  %>% 
      mutate(race_clean = fct_relevel(race_clean,
                                      "Asian/Pacific Islander",
                                      "Black",
                                      "White",
                                      "Other"))
    
  # also note how we could easily go back and forth between character and factor 
  # (no need to do this, just showing you what is possible)
    arrests_bds.clean <- arrests_bds.clean %>% 
      mutate(race_clean = as.character(race_clean)) %>%  
      mutate(race_clean = as.factor(race_clean))

  # validation: confirm the recode worked as intended
    arrests_bds.clean %>% 
      count(race_clean, sort = TRUE)
    
    table(arrests_bds.clean$race, 
          arrests_bds.clean$race_clean, 
          useNA = "always")
    
    
# 3b. BDS ethnicity (Hispanic identity)
      
  # inspect
  # NOTE: don't show all of this in your R Markdown submission, it's just for yoU!
    levels(arrests_bds.clean$ethnicity)
    table(arrests_bds.clean$race_clean, 
          arrests_bds.clean$ethnicity, 
          useNA = "always")
  
  # now let's recode by creating a Hispanic column where:
  # hispanic takes the values Hispanic, Non-Hispanic, or NA
    arrests_bds.clean <- arrests_bds.clean %>% 
      mutate(hispanic = recode(ethnicity, 
                               "0" = NA_character_,
                               "Other" = "Non-Hispanic")) 
    
  # validation: confirm the recode worked as intended
    table(arrests_bds.clean$hispanic, 
          arrests_bds.clean$ethnicity, 
          useNA = "always")
    summary(arrests_bds.clean$hispanic) #less useful for validation!
      


# 3c. race_eth
      
  # let's investigate a bit...
  # examine every possible combination of hispanic identity and race

    table(arrests_bds.clean$race_clean,
          arrests_bds.clean$hispanic, 
          useNA = "always")
    
  # when recoding vars, 1st determine the recoding logic you want to implement:
  # let's generate a new variable race_eth by...
    # first, assigning Hispanic identity if indicated
    # second, filling in the new variable with race if they are not Hispanic
    # third, rename White and Black levels as Non-Hispanic White & Non-Hispanic Black
 arrests_bds.clean <- arrests_bds.clean %>% 
   mutate(race_eth = if_else(hispanic %in% "Hispanic",
                             hispanic,
                             race_clean)) %>% 
   mutate(race_eth = recode(race_eth, 
                            "White" = "Non-Hispanic White",
                            "Black" = "Non-Hispanic Black"
                            ))

  
  # validate results
    
    # joint distribution of race_eth and hispanic
 
    table(arrests_bds.clean$race_eth,
          arrests_bds.clean$hispanic,
          useNA = "always")

    # wait a minute... Non-Hispanic, White, and Black still appear as levels!
    # even though they are longer categories we want to use and has no observations
    # so let's remove them from the vector of levels by specifying levels to keep

    arrests_bds.clean <- arrests_bds.clean %>% 
      mutate(race_eth = factor(race_eth,
                               levels = c("Asian/Pacific Islander",
                                          "Hispanic",
                                          "Non-Hispanic Black",
                                          "Non-Hispanic White",
                                          "Other")))
    # let's validate again
    table(arrests_bds.clean$race_eth,
          arrests_bds.clean$hispanic,
          useNA = "always")
    
    # what's a tidyverse way of showing the distribution of race_eth

    
  
## -----------------------------------------------------------------------------
## 4a. Repeat steps from q3 for Legal Aid Society (LAS) data:
##      - create race_eth in arrests_las with the same coding as for BDS
##      - note that Hispanic identity is included in two columns, not one:
##          - las_race_key and hispanic_flag
##      - Make sure you end up with a data frame with the following var names
##        and identical coding as in arrests_bds_clean:
##        - race_eth, age, male, dismissal (not in the BDS data), st_id, loc2
## -----------------------------------------------------------------------------

# 4.

    
  table(arrests_las$race,
        arrests_las$ethnicity,
        useNA = "always")

  arrests_las.clean <-  arrests_las %>% 
    mutate(race_eth = recode(race,
                             "Asian or Pacific Islander" = "Asian/Pacific Islander",
                             "Unknown" = NA_character_,
                            "Latino" = "Hispanic",
                            "White" = "Non-Hispanic White",
                            "Black" = "Non-Hispanic Black")) %>% 
    mutate(race_eth = if_else(ethnicity %in% "Y",
                             "Hispanic",
                             race_eth)) %>% 
    mutate(race_eth = factor(race_eth,
                             levels = c("Asian/Pacific Islander",
                                        "Hispanic",
                                        "Non-Hispanic Black",
                                        "Non-Hispanic White",
                                        "Other")))
    
  
    table(arrests_las.clean$race_eth,
          arrests_las.clean$ethnicity,
          useNA = "always")
    
    table(arrests_las.clean$race_eth, useNA = "always")
  
  # validate with relevant cross-tabs
  FILL IN CODE
 


## -----------------------------------------------------------------------------
## 5. Append BDS and LAS microdata -- stack rows with bind_rows()
##
##    a. create a column (pd) to identify PD data source ("las" or "bds")
##
##    b. Append arrests_bds.clean and arrests_las.clean
##        - use bind_rows from the dplyr package
##        - store combined data as new data frame called arrests.clean
##        - only keep columns for pd, race_eth, age, male, dismissal, st_id, loc2,
##          converting to factors for columns w/categorical data as needed
##        - note that dismissal column is in LAS data but not BDS
##        - inspect race_eth for accuracy/consistency
##        - store as new data frame arrests.clean
##
##    c. use the nrow function to display the total number of arrests
##
##    d. Save arrests.clean df as an .RData file in a new Lecture4 folder for next week
##
## -----------------------------------------------------------------------------

# 5a.
  arrests_bds.clean <- arrests_bds.clean %>% 
    mutate( pd = "bds")
  
  
# 5b. since we don't have arrests_las.clean yet, for now let's append arrests_bds.clean to itself

  arrests.clean <- bind_rows(arrests_bds.clean,
                             arrests_bds.clean) %>% 
    mutate(pd = as.factor(pd),
           st_id = as.factor(st_id),
           loc2 = as.factor(loc2)) %>% 
    select(pd, race_eth, age, male, st_id, loc2)

  MAKE SURE TO UPDATE ABOVE CODE TO APPEND arrests_las.clean TO arrests_bds.clean
  
  
# 5c. 
  
  
# 5d.
  save(LIST DATA OBJECTS TO SAVE HERE SEPARATED BY COMMAS,
       file = "arrests.clean.RData")  

  # for future reference, can also write to a csv file:
  # write_csv(arrests_all, "arrests_all.csv") 

  
  
## -----------------------------------------------------------------------------
## 6. Descriptive statistics by race_eth (grouping)
##
##    a. group arrests.clean by race_eth,
##       show arrest counts for each race_eth category using tidyverse functions
##        Note: we already obtained this information using the summary command,
##        but sometimes it's useful to store this information to work with
##
##    b. show a table with the proportion of total arrests in each race_eth category
##        - how does excluding NAs change the results? 
##
##    c. compute avg age, share male, and dismissal rate for each race_eth group, 
##       along with the total sample size. 
##       also compute the sample size for the dismissal variabe as well
##        (just the number of non-NA observations for dimissal)
##        - assign results to a new object, race_eth_stats
##        - HINT: similar to (a) above, but specify different stats in summarise()
##        - HINT: for most of these stats you need to tell R to ignore NA values
##
##    d. what, if anything, do you think is interesting to note about the
##       distribution of:
##        - age by race (conditional distribution of age)
##        - male share by race
##        - dismissal by race
## -----------------------------------------------------------------------------

# 6a. 


    
# 6b.



# 6c. 
    
  # assign results to new data frame so you can refer to them in write-up
    race_eth_stats <- FILL IN CODE
    
    
# 6d. 


  
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

# 7a.
  arrests.clean <- dummy_cols(arrests.clean, 
                              select_columns = "race_eth")
  str(arrests.clean)
  summary(arrests.clean[,8:13]) #too clunky, don't show this in your submission!
  
  # what's a better way to show just the means using summarise()?

  

# 7b.
  
  # let's generate station-level counts for each race_eth group
  # general approach: sum dummy variables
  arrests_stations <- arrests.clean %>%  FILL IN CODE
  

# 7c. 
  arrests_stations_top <-  FILL IN CODE
  
  
#  7d.
  

## -----------------------------------------------------------------------------
## 8. OPTIONAL: barplots 
##
##    a. barplot of all arrests by race_eth category (proportions of all arrests, sorted)
##
##    b. same as part a but exclude all observations with race_eth == NA
##
##    c. stacked barplot showing race_eth breakdown for top 10 stations by total arrest count
## -----------------------------------------------------------------------------

# 8a. 
  arrests.clean %>% 
    ggplot(aes(x = race_eth)) + 
    geom_bar()
  
  arrests.clean %>% 
    ggplot(aes(x = fct_infreq(race_eth),
               y = ..prop.., group = 1)) + 
    geom_bar() + 
    scale_y_continuous(labels = scales::percent_format()) 

  
# 8b.
  arrests.clean.nomiss <- arrests.clean %>% 
    filter(is.na(race_eth) == FALSE)
  summary(arrests.clean.nomiss$race_eth)
  
  arrests.clean.nomiss %>% 
    ggplot(aes(x = race_eth)) + 
    geom_bar()
  
  arrests.clean.nomiss %>% 
    ggplot(aes(x = fct_infreq(race_eth),
               y = ..prop.., group = 1)) + 
    geom_bar() + 
    scale_y_continuous(labels = scales::percent_format())


# 8c.
  arrests_stations_race_top <- arrests.clean %>%  
    group_by(loc2) %>% 
    mutate(st_arrests = n()) %>% 
    ungroup() %>% 
    group_by(loc2, race_eth)  %>%
    summarise(arrests = n(), 
              st_arrests = first(st_arrests)) %>% 
    arrange(desc(st_arrests)) %>% 
    filter(st_arrests > 100)
  
  arrests_stations_race_top
  
  
  arrests_stations_race_top %>% 
    ggplot(aes(x = reorder(loc2, -st_arrests),
               y = arrests,
               fill = race_eth)) + 
    geom_bar(stat = "identity") + 
    theme(axis.text.x = element_text(angle = 90, 
                                     vjust = 0.5, 
                                     hjust=1)) 
