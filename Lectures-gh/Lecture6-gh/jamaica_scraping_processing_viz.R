## -----------------------------------------------------------------------------
## 1. load libraries and check directory
## -----------------------------------------------------------------------------

# 1.1 - Libraries 

library(rvest)
library(tidyverse)
library(tidyr)
library(lubridate)
library(sf)
library(gganimate)

## -----------------------------------------------------------------------------
## 2. Extracting all the tables  
## -----------------------------------------------------------------------------

## My starting point was checking whether there was an AI tool to easily get the tables. 
## After not finding one I tried this prompt: 
## CHAT GPT Prompt: "explain me how to extract the table from this URL in R https://www.indecom.gov.jm/report/2017-security-force-related-fatalities"

## 2.1 - URL List. 

## -----------------------------------------------------------------------------
## There are different types of URLs found in the website:
## Type of URLS (#### is year): 
## 2021 - 2024
## https://www.indecom.gov.jm/report/202(####)-security-forces-fatal-shootings
## 2018 - 2020
## https://www.indecom.gov.jm/report/202(####)-security-forces-related-fatalities
## 2017 - 2017
## https://www.indecom.gov.jm/report/2017-security-force-related-fatalities

## NOTE: All fatalities are shootings. This can be read on the PDF's on the website. 
## -----------------------------------------------------------------------------

### 2.1a - Checking scraping one URL using a CHAT GPT prompt 

  url <- "https://www.indecom.gov.jm/report/2017-security-force-related-fatalities"
  webpage <- read_html(url)
  tables <- webpage %>% html_table()
  length(tables)  # Check how many tables were extracted
  tables[[1]]  # View the first table
  
  # Works perfectly! But will require some cleaning. 
  # Can I check two URLS?   
  url1 <- c("https://www.indecom.gov.jm/report/2017-security-force-related-fatalities","https://www.indecom.gov.jm/report/2017-security-force-related-fatalities")
  webpage <- read_html(url1[1])

  
### 2.1b - Building a dataframe with the complete URL list   

  # Using the same objects from above for 2017
  urldf <- data.frame(ulist = url)
  
  for (year in 2018:2024){
    if (year >= 2018 & year < 2021){
      auxi_url <- paste0("https://www.indecom.gov.jm/report/",year,"-security-forces-related-fatalities") 
    }
    if (year >= 2021){
      auxi_url <- paste0("https://www.indecom.gov.jm/report/",year,"-security-forces-fatal-shootings") 
    }
    urldf <- urldf %>% bind_rows(data.frame(ulist = auxi_url))
  }
  
  urldf # inspect 
  dim(urldf) # number of years 

  
### 2.1c - Extracting all the tables  

  # Generates a function to obtain the data: 
  extract_fun  <- function(num){
    
    # read HTML
    webpage <- read_html(urldf$ulist[num])
    
    # extract table and convert to data frame
    extract_df <- webpage %>% 
      html_table() %>% 
      as.data.frame()
    
    # note that the var names in each df appear in the first row
    # extract var names, use to set column names, and then remove 1st row
    new_names <- as.character(extract_df[1, ])
    colnames(extract_df) <- new_names
    extr_table <- extract_df[-1, ]  # remove the first row
    
    # we observe that the 2023 table (element 7) has an extra column to start
    # in all other years, the first column is a counter for the num of deaths
    # but in 2023, the death counter column is the second column
    # using an if statement, rename the death counter column as 'input_id'    
    if (num != 7){
      names(extract_df)[1] <- "input_id" #
    }
    if (num == 7){
      names(extract_df)[1] <- "empty_name" # first column in 2023 is empty
      names(extract_df)[2] <- "input_id"
    }

    # create a column indicating the year
    extract_df <- extract_df %>% mutate(year = num + 2016) 
      # The first table is 2017=1+2016, the second is 2018=2+2016, etc.
    
    # we can also remove empty rows by filtering on the year column
    #extr_table <- extr_table %>% filter(year != "") 
    
    return(extract_df) # Output
  }
  
  # create an empty list to store the results
  df_list <- vector("list", 8)

  # loop through 1 to 8 and apply extract_fun
  for (i in 1:8) {
    df_list[[i]] <- extract_fun(i)
  }
  
  # assign a name to each table (each element of the tables list) for each year
    # define the vector of years to use for table names
    year_names <- as.character(2017:2024)
    # assign table names
    names(df_list) <- year_names


## -----------------------------------------------------------------------------
## 3. Cleaning
## -----------------------------------------------------------------------------

### Columns

  # Let's check all the column names 
  for (num in 1:8) {
    print(df_list[[num]]$year[1])
    print(names(df_list[[num]]))
  }
  # Most of the problems are in the last two years...
  names(df_list[["2023"]])[3] <- "Date of Incident"    # was Date
  names(df_list[["2023"]])[4] <- "Name of Deceased"    # was Deceased
  names(df_list[["2023"]])[5] <- "Location of Incident"# was "Location"
  names(df_list[["2023"]])[6] <- "Related State Agent" # was Security Force Unit
  names(df_list[["2024"]])[2] <- "Date of Incident"    # was Date
  # ... and some Sneaky problems 
  names(df_list[["2017"]])[2] <- "Date of Incident"    # was Date of Indicent, not Date of Incident
  names(df_list[["2017"]])[5] <- "Related State Agent" # was Related State Agency
  names(df_list[["2021"]])[5] <- "Related State Agent" # was Related State Agency
  names(df_list[["2022"]])[5] <- "Related State Agent" # was Related State Agency
  names(df_list[["2024"]])[5] <- "Related State Agent" # was Related State Agency
  
  # Binding all the data 
  killings <- bind_rows(df_list)
  
  # Renaming columns
  killings <- killings %>%
    rename(
      name_deceased = `Name of Deceased`,
      date_of_incident = `Date of Incident`,
      location    = `Location of Incident`,
      state_agent = `Related State Agent`
    )

### Rows

  # Eliminate colons to eliminate rows that are Sub-headers.  
  killings <- killings %>% filter(grepl(":",state_agent) == 0)

  # Filter out remaining empty rows by searching for empty name_deceased 
  # (some rows lack an input_id but are still a record of a killing, so we do not want to discard these)
  killings <- killings %>% filter(name_deceased != "")
  
  # Let's keep only the columns of interest
  killings <- killings %>% dplyr::select (input_id, date_of_incident, name_deceased, location, state_agent, year)


### 3.1.1 Generate a variable with the number of deceased for every record

  # First let's check those rows that have a blank input_id 
  filtered_killings <- killings %>%
    filter(is.na(input_id) == TRUE | input_id == "") %>%
    select(input_id, name_deceased)
  filtered_killings
  # we see that they are all records of one killing (as only one name appears for each)
  
  # handle records with multiple killings
  # Calculate the deceased variable as the difference between the first input_id number and the last
  # Set cases where there is no input_id equal to 1. 
  killings <- killings %>%
    mutate(
      
      # Extract the first number (before the first comma, space, or dash)
      first_num = as.numeric(str_extract(input_id, "^\\d+")),
        # Regex overview:
          # ^  Match the start of a string
          # \  Escape a special character
          # \d Matches any digit
          # +  Match the preceding item 1 or more times
          # $  Match the end of a string
      
      # Extract the last number (after the last comma, space, or dash)
      last_num = as.numeric(str_extract(input_id, "\\d+$")),
      
      # Compute deceased count
      deceased = case_when(
        is.na(input_id) == TRUE | input_id == "" ~ 1,  # if input_id is empty, assign deceased = 1
        !is.na(first_num) & !is.na(last_num) ~ last_num - first_num + 1,  # normal case with the difference
      )
    ) %>%
    select(-first_num, -last_num)

  
### 3.1.2 Generate a date variable

  killings <- killings %>%
    mutate(date = str_c(date_of_incident, "-", as.character(year))) %>% 
    mutate(date = dmy(date)) %>%  # use dmy from Lubridate which recognizes day-month-year format
    select(-date_of_incident) # drop date_of_incident as redundant


### 3.1.3  Building a Parish variable.

  ## Here are the 14 Parishes in Jamaica that we want to identify from the location variable:
  # Kingston, St. Andrew, St. Thomas, Portland, St. Mary,  St. Ann, Trelawny, St. James,Hanover, 
  # Westmoreland, St. Elizabeth, Manchester, Clarendon, St. Catherine
  
  # Standardize format of location of incident by turning everything to lower case
  killings <- killings %>% mutate(location = tolower(location)) 
  # Fix wrong addresses: 
  killings$location <- gsub("cathrine", "catherine", killings$location)
  killings$location <- gsub("catheirne", "catherine", killings$location)
  killings$location <- gsub("clerendon", "clarendon", killings$location)
  killings$location <- gsub("claredndon", "clarendon", killings$location)
  killings$location <- gsub("westmotreland", "westmoreland", killings$location)
  killings$location <- gsub("knigston", "kingston", killings$location)
  killings$location <- gsub("kingaston", "kingston", killings$location)
  killings$location <- gsub(".", " ", killings$location, fixed = TRUE) #replace . with space, e.g. 'st. ' to 'st "
  killings$location <- gsub("\\s+"," ", killings$location) #remove duplicate spaces
  
  
  # paste(..., collapse = "|") creates a pattern that matches any of the parish names (i.e., "kingston|andrew|thomas|...")
  parishes <- paste(c("kingston", "st andrew", "st thomas", "portland", 
                      "st mary", "st ann", "trelawny", "st james", "hanover",
                      "westmoreland", "st elizabeth", "manchester", 
                      "clarendon", "st catherine"), collapse = "|")
  
  # extract the parish from the location column and assign to parish column
  killings <- killings %>%
    mutate(parish = str_extract(location, parishes))

# run some summary stats
summary(killings)

# show distribution by parish (pooled over all years)
summary(as.factor(killings$parish))

# and save the file
save(killings,  file = "jamaica_clean.RData") 


### 3.2. Convert to panel, join in population data

  # cross-tab between parish and year including NAs
  table(killings$parish, killings$year, useNA = "always")

  # By parish-year 
  panel <- data.frame(table(killings$parish, 
                            killings$year, 
                            useNA = "no")) %>% 
    rename(parish = Var1,
           year = Var2,
           killings = Freq) %>% 
    mutate(year = as.integer(as.character(year)))
  
  # is the panel balanced?
  table(panel$parish, panel$year)
    # yes!
  
  
  # load and prepare data with population by parish
  # data is from the Statistical Institute of Jamaica
  pop_raw <- read.csv("DZ-2025-04-08-17-50-38.csv") 
  pop <- pop_raw %>% filter(Series.title != "Total") %>% 
    mutate(Series.title = tolower(Series.title)) %>% 
    dplyr::select(Series.title, X1.1.2019) %>% 
    rename("pop" = "X1.1.2019",
           "parish" = "Series.title")
  
  # generate killings per 10,000 pop  
  panel_pop <- panel %>% 
    inner_join(pop, by = c("parish")) %>% 
    mutate(killings_pc = round(killings / (pop/10000), 2)) %>% 
    dplyr::select(-pop)
  
  
## -----------------------------------------------------------------------------
## 4. Descriptive panel analysis
## -----------------------------------------------------------------------------
  
  # Let’s plot the trend in killings per 10,000 pop for each parish
  panel_pop %>% 
    ggplot(aes(x = year, y = killings_pc, color = parish)) +
    geom_line()  +
    labs(title = '',
         x = '', 
         y = 'killings per 10,000 population')
  
  # convert to wide form panel 
  panel_wide <- panel_pop %>% 
    select(-killings) %>% 
    pivot_wider(names_from = year,
                names_prefix = "kpc",
                values_from = killings_pc) 
  
  # obtain the indexed change relative to 2017, convert back to long form
  panel_index <- panel_wide %>% 
    mutate(killings_pc_index_2017 = 100,
           killings_pc_index_2018 = round(100*kpc2018/kpc2017, 2),
           killings_pc_index_2019 = round(100*kpc2019/kpc2017, 2),
           killings_pc_index_2020 = round(100*kpc2020/kpc2017, 2),
           killings_pc_index_2021 = round(100*kpc2021/kpc2017, 2),
           killings_pc_index_2022 = round(100*kpc2022/kpc2017, 2),
           killings_pc_index_2023 = round(100*kpc2023/kpc2017, 2),
           killings_pc_index_2024 = round(100*kpc2024/kpc2017, 2)) %>% 
    select(-(kpc2017:kpc2024)) %>% # drop original cols with killings per 10k
    pivot_longer(cols = c(killings_pc_index_2017:killings_pc_index_2024),
                 names_to = "year",
                 values_to = "kpci") %>% # reshape back to long form
    mutate(year = as.integer(str_sub(year, 19, 22))) # obtain the year from last 4 chars of each char string
  
  # join panel_index to panel_full so all vars in one data frame
  panel_full <- panel_pop %>% 
    inner_join(panel_index, by = c("year", "parish"))
  
  # plot new time series by parish
  panel_full %>% 
    ggplot(aes(x = year, y = kpci, color = parish)) +
    geom_line() +
    labs(title = '',
         x = '', 
         y = 'killings as a percent of 2017 levels')

  # time trends in killings per capita by parish (faceted plots)
  panel_pop %>% 
    ggplot(aes(x = year, y = killings_pc, color = parish)) +
    geom_line(show.legend = FALSE) +
    facet_grid(~parish) +
    labs(title = 'Killings per 10,000 population by parish',
         x = '', 
         y = 'killings per 10,000 population') +
    scale_x_continuous(breaks = seq(2018,2024,4),
                       labels = seq(2018,2024,4),
                       minor_breaks = seq(2017,2024,1) ) +
    theme(axis.text.x = element_text(angle = 45, 
                                     hjust = 1, 
                                     vjust = 1,
                                     size = 8))
  
## -----------------------------------------------------------------------------
## 5. Mapping
## -----------------------------------------------------------------------------

# Load the shape file as an simple features (sf) data frame.
  jm_parish <- rnaturalearth::ne_states("Jamaica", returnclass = "sf")
  class(jm_parish)

  jm_parish <- jm_parish %>% 
    mutate(parish = tolower(name))  %>% 
    mutate(parish = gsub("saint", "st", parish)) %>% 
    dplyr::select(geometry, parish) 
  
  head(jm_parish)
  str(jm_parish)

# plot simple base map
  jm_parish %>% ggplot() + geom_sf()
  
# join geospatial data to panel_full by parish
  panel_long_map <- jm_parish %>% 
    right_join(panel_full, by = "parish") %>% 
    mutate(parish = str_to_title(parish))

# static map of killings per capita in 2024
panel_long_map %>% 
  filter(year == 2024) %>% 
  ggplot() + 
  geom_sf(aes(fill = killings_pc)) +
  theme(plot.title=element_text(size = 12),
        axis.title = element_blank(),
        axis.text  = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  scale_fill_gradient(limits = c(0, 6), 
                      breaks = c(0, 2, 4, 6),
                      low = "white", high = "purple", na.value="grey80") +
  labs(title = 'Killings per 10,000 population',
       fill='') +
  geom_sf_text(aes(label = parish), size = 2.5, family = "sans")

# static map of indexed killings per capita in 2024
panel_long_map %>% 
  filter(year == 2024) %>% 
  ggplot() + 
  geom_sf(aes(fill = kpci)) +
  theme(plot.title=element_text(size = 12),
        axis.title = element_blank(),
        axis.text  = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  scale_fill_gradient(limits = c(0, 1100), 
                      breaks = c(0, 250, 500, 750, 1000),
                      low = "white", high = "purple", na.value="grey80") +
  labs(title = 'Police killings indexed to 2017 levels',
       fill='') +
  geom_sf_text(aes(label = parish), size = 2.5, family = "sans")

### 5.1 Adding Animation

# animated plot of killings indexed to 2017 levels
basemap <- panel_long_map %>% 
  filter(year > 2017) %>% # omit 2017 as the indexed base year
  ggplot() + 
  geom_sf(aes(fill = kpci)) +
  theme(plot.title=element_text(size = 12),
        axis.title = element_blank(),
        axis.text  = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  scale_fill_gradient(limits = c(0, 1100), 
                      breaks = c(0, 250, 500, 750, 1000),
                      low = "white", high = "purple", na.value="grey80") +
  labs(title = 'Police killings indexed to 2017 levels',
       fill='') 

basemap

basemap +
  transition_states(states = year) +
  labs(subtitle = 'Year: {closest_state}')


# animated plot of killings per capita
basemap <- panel_long_map %>% 
  ggplot() + 
  geom_sf(aes(fill = killings_pc)) +
  theme(plot.title=element_text(size = 12),
        axis.title = element_blank(),
        axis.text  = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  scale_fill_gradient(low = "white", high = "purple", na.value="grey80") +
  labs(title = 'Police killings per 10,000 population',
       fill='') +
  geom_sf_text(aes(label = parish), size = 2.5, family = "sans", 
               nudge_y = -.01)

basemap +
  transition_states(states = year) +
  geom_text(aes(y = 17.8, x = -78.2, label = as.character(year)), 
            check_overlap = TRUE, 
            size = 6) 
