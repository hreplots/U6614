################################################################################
##
## [ PROJ ] Working with strings and dates 
## [ FILE ] indonesian_village_fund.r
## [ AUTH ] < YOUR NAME >
## [ INIT ] < Oct 23, 2025 >
##
################################################################################

# This lesson works w/messy panel data on Village Fund Allocations in Indonesia

# Organization of this R script:

  #1. Cleaning up Village Fund data using string functions
  #2. Using the Village Fund data to work with dates
  #3. Preparing the Village Fund data for a subsequent mapping exercise

#To work with character variables and regular expressions, we'll focus on the the stringr package
#See https://stringr.tidyverse.org/ for a cheatsheet listing key stringr functions

#Regular expressions are an efficient way to match different patterns in strings,
#similar to the [command + f] function you use to find text in documents

#To work with dates in R, we'll use functions from the lubridate package
#See https://lubridate.tidyverse.org/ for a cheatsheet listing key lubridate functions


## -----------------------------------------------------------------------------
## libraries
## -----------------------------------------------------------------------------

#install.packages("readxl")

library(tidyverse) 
library(readxl)
library(lubridate)
#stringr is a handy package for working w/strings that is part of the tidyverse

options(warn = -1) #this supresses certain warnings


## ---------------------------
## directory paths
## ---------------------------

getwd()


## -----------------------------------------------------------------------------
## 1. Clean VF data using string functions (from stringr package)
## -----------------------------------------------------------------------------

#Loading the Village Fund data
  vf_data <- read_excel('rpt_dd_kabupaten.xls')

  View(vf_data)
  #Observation:
  #This dataframe contains information about the amount of Village Fund allocated 
  #for each kabupaten (regency) for each year from 2015 to 2018
  #The amounts are in thousand Rupiah (1,000 Rupiah = 0.068 USD as of Aug 21st 2020)
  #Notice how there are missing entries, which we should address first
  
  #This is messy panel data where the unit of observation is regency-year

  
### FILLING IN MISSING VALUES (using fill() function)

  #A simple Google search leads us to the following powerful function from the tidyr package (which is a part of the tidyverse)
  #By looking at the documentation, this function is able to "fill in missing values with previous or next value"
  vf_data <- vf_data %>%
    fill(c('Provinsi', 'Kabupaten'))
  
  #It does not work. Why? A closer look suggests that our data does not really contain missing values
  table(vf_data$Provinsi, useNA = "no") #the apparent blanks aren't coded as NA
  
  #It seems that what appear to be blank spaces we have in both Provinsi and Kabupaten columns 
  #are actually non-breaking spaces (nbsp). Let's explore this a bit more.
  x <- vf_data$Provinsi[2] #take 2nd element of Provinsi column and assign to x
  x == ' '  #evaluates whether this express is TRUE/FALSE
  x == '\u00A0'
  
  #Having realized that, here is one way to solve the problem
  vf_data <- vf_data %>%
    mutate(Provinsi = replace(Provinsi, Provinsi == '\u00A0', NA),
           Kabupaten = replace(Kabupaten, Kabupaten == '\u00A0', NA)) %>%
    fill(c('Provinsi', 'Kabupaten'))

  #To make our lives easier, let us rename the variables
  colnames(vf_data) <- c('province', 'regency', 'year', 'vfund_allocation')
  
  #From the data, it seems like we have 4 rows of information for each regency (each row represents the allocation for a year)
  #We can verify this with a simple check: both of these commands return 4
  max(table(vf_data$regency))
  min(table(vf_data$regency))

  
### INSPECTING NUMERICAL DATA AND ASSIGNING NAs AS NEEDEED
  
  #Also, we need to ensure that our numerical data columns contain no missing values
  #One possible way is to make sure the following command returns no observations
  missing <- vf_data[rowSums(is.na(vf_data)) == TRUE,]
  
  #Alternatively, we can do the following, which checks the number of NA entries in the dataframe
  sum(is.na(vf_data))
  
  #However, we are not done yet. 
  #A simple check on vf_data reveals that some of these values do not make sense
  #For example, the fund allocated for KABUPATEN BUTON TENGAH in 2017 is 54,000 Rupiah, which is <4 USD!
  vf_data[(vf_data$regency == "KABUPATEN BUTON TENGAH"),]
  
  #without further information, let's treat this as a date entry error and recode as NA
  vf_data[(vf_data$regency == "KABUPATEN BUTON TENGAH") & (vf_data$year == 2017), 
          "vfund_allocation"] <- NA

  
### CHANGING CAPITALIZATION OF CHARACTER VARIABLES (using str_to_title)
  
  #Now, let's fix the capitalization of the provinces and regencies' names
  #e.g. show the province name as Kabupaten Musi Rawas instead of KABUPATEN MUSI RAWAS
  #The function str_to_title from stringr comes in handy
  vf_data <- vf_data %>%
    mutate(province = str_to_title(province),
           regency = str_to_title(regency))

  
### RECODING CHARACTER VARIABLES USING STRING FUNCTIONS
  
  #In order to familiarize ourselves with strings in R, let's do some ad hoc tasks

  #First, all entries in the regency column begin with either Kabupaten or Kota. 
  #A city (kota) is defined as a 2nd-level administrative subdivision of the country, 
  #equivalent to regency (kabupaten). The difference between a regency and a city is
  #that a city has non-agricultural economic activities and a dense urban population,
  #while a regency comprises a rural area larger than a city.
  
  #Let us now create a column called type to indicate if a 'regency' is a Kabupaten or Kota
  #In order to do that, we want to take the first word of the regency column
  #the word() function from stringr helps us do that
  vf_data <- vf_data %>%
    mutate(type = word(regency, 1))

  
  #Let's define another column called name including regency name without Kabupaten or Kota
  #This will be useful for subsequent coding tasks and joining other data sources
  vf_data <- vf_data %>%
    mutate(name = word(regency, 2, -1)) #extracts 2nd word onwards from regency string

  
  #Next let's create a column as a dummy variable for all regencies on Kalimantan Island
  #Note that all provinces in Kalimantan begin with the word Kalimantan
  vf_data <- vf_data %>% 
    mutate(kalimantan = (word(province, 1) == "Kalimantan"))
  summary(vf_data$kalimantan)

  #let's encode kalimantan as a factor with clear labels
  vf_data$kalimantan <- factor(vf_data$kalimantan,
                               levels = c(FALSE,TRUE),
                               labels = c("Not in Kalimantan", "Kalimantan"))
  summary(vf_data$kalimantan)


  #Lastly, let's obtain the full name of each regency as follows: 
  #if regency name is Kota Langsa and the province name is Aceh, 
  #the full name becomes Kota Langsa, Provinsi Aceh
  vf_data <- vf_data %>%
    mutate(full_name = str_c(regency, ', Provinsi ', province, sep = ''))

  
  #Let's explore village fund allocation over time for provinces in Kalimantan across years
    
    #first let's subset vf_data to get a dataframe for regencies in Kalimantan
    #and let's group by province-year
      kalimantan_vf <- vf_data %>%
        filter(word(province, 1) == 'Kalimantan') %>%
        group_by(province, year) %>%
        summarize(vfund_allocation = sum(vfund_allocation)) %>%
        mutate(year = as.factor(year))
  
    ggplot(kalimantan_vf, 
           aes(fill = year, y = vfund_allocation, x = province)) + 
      geom_bar(position = 'dodge', stat = 'identity')

  #We can see that the province w the highest overall funding is Kalimantan Barat, 
  #followed by Kalimantan Selatan, Kalimantan Tengah, Kalimantan Timur and Kalimantan Utara
  #Also, we see a noticeable increase in the allocated fund in 2015-2016 for all provinces
  #The increase is not as large in absolute terms from 2016-17 and 2017-18. 
  
  
### ADDITIONAL EXERCISES
  
  #How many Kabupaten and Kota are there in the dataset?
   table(vf_data$type)/4

  #Which regency(s) has the longest name?
  #excluding Kabupaten/Kota, by number of characters including spaces
    vf_data %>%
      select(province, name) %>%
      distinct() %>%
      mutate(length = str_length(name)) %>%
      filter(length == max(length))
  
  #How many regencies contain the substring 'Selatan' (means 'South')?
    vf_data %>%
      select(regency) %>%
      distinct() %>%
      filter(str_detect(regency, 'Selatan')) %>%
      summarize(count = n())

  #Note how the str_detect() function works
    str_detect('Kabupaten Aceh Selatan' ,'Selatan')
    str_detect('SelatanColumbia University', 'Selatan')

  #Let's create a function to check whether a string contains the exact WORD 'Selatan'
  #In other word, the first command should return TRUE and the second FALSE
  #One alternative would be to split the string by space and do the check
    containSelatan <- function(sentence){
      split <- str_split(sentence, pattern = ' ')[[1]]
      return('Selatan' %in% split)
    }
    containSelatan('Kabupaten Aceh Selatan')
    containSelatan('SelatanColumbia University')

  #To learn more about regex, please consult https://r4ds.had.co.nz/strings.html

    
  #Transform this dataframe into the a wide form panel dat
  #province, regency, vfund_2015, vfund_2016, vfund_2017, vfund_2018
    vf_data_wide <- vf_data %>%
      select(province, regency, year, vfund_allocation) %>%
      spread(year, vfund_allocation) 
        #spread is an alternative to widen_panel() and pivot_wider()
    
    colnames(vf_data_wide)[3:6] = c('vfund_2015', 'vfund_2016', 
                                    'vfund_2017', 'vfund_2018')


## -----------------------------------------------------------------------------
## 2. Working with date functions (from the lubridate package)
## -----------------------------------------------------------------------------

### MAKING DATES IN R w/LUBRIDATE
    
  #Let's assume every regency-year observation is as of January 1 of each year
  #Let's create a date variable on vf_data that specifies the exact date
  #One possible way is to create a string of the form 'YYYY-MM-DD', which can
  #be easily transformed into a Date object by the ymd function in lubridate
  vf_data_date <- vf_data %>%
    mutate(date = ymd(str_c(year, '-01-01')))
  
  #Alternatively, the following command should also work:
  vf_data_date <- vf_data %>%
    mutate(date = make_date(year, 1, 1))
  
  #With lubridate, we can do a lot of useful things with Date in a simple manner
  #Consider the following examples all return the same date:
  ymd(20101204)
  ymd(101204)
  ymd('2010-12-04')
  ymd('10-12-04')
  mdy('12-4-17')
  mdy('12-04-17')
  dmy('4/12/2017')
  
  today <- ymd('2020/11/10')
  month(today)
  day(today)
  wday(today, label = TRUE)
  

### A SIMPLE EXERCISE TO SUMMARIZE DAILY OBSERVATION COUNTS
  
  #Create random month (from 1 to 12) and date (from 1 to 28) columns in vf_data_date
  #Obtain the distribution of the day of week of the observations
  #For reproducibility, set the seed to 432
  #Note: sample.int() draws a random sample
    set.seed(432)
    vf_data_date2 <- vf_data %>%
      mutate(month = sample.int(12, nrow(vf_data), replace=TRUE),
             date = sample.int(28, nrow(vf_data), replace=TRUE),
             date2 = make_date(year, month, date),
             day = wday(date2, label = TRUE))
    
    vf_data_date2 %>%
      group_by(day) %>%
      summarize(count = n())

    
### USEFUL FUNCTIONS FOR INCREMENTING TIME
  
  make_date(2018, 1, 1) + days(1)
  ymd(20100803) + weeks(1)
  mdy('4/10/2015') + months(1)


## -----------------------------------------------------------------------------
## 3. Preparing VF data for a subsequent mapping exercise
## -----------------------------------------------------------------------------
  
  #So far we've only looked at total VF allocations
  #Let's calculate the amount of funding per resident living in poverty in 2018
  #Need data on the number of residents in poverty per regency in 2018
  
  #Start with VF data for 2018
    vf_data_2018 <- vf_data %>%
      filter(year == 2018) %>%
      select(province, regency, vfund_allocation)
  
  #Read in poverty data and inspect -- we're looking for 2018 poverty information
    poor_2018 <- read_excel('bps_poverty.xls') 
    str(poor_2018)
    head(poor_2018)
    
    #Some key observations:
    #First, Nama Wilayah written in all caps represents rows w/province-level totals
    #Nama Wilayah is written in the form Kota X (if X is a Kota) or Y (if Y is a Kabupaten)
    #Number of of poor residents in 2018 is in the poverty 2018 column (measured in 1000s)
      poor_2018 <- poor_2018 %>%
        select('Nama Wilayah', 'poverty 2018')
      
      colnames(poor_2018) <- c('region_name', 'poverty')

  #let's clean the poverty data and prep to join with vf_data
  #1. remove rows for province totals (only want to keep regencies)
  #2. get region_name to match regency name coding in vf_data (e.g. Kabutapen X, Kota Y)
  poor_2018 <- poor_2018 %>%
    filter(str_to_upper(region_name) != region_name) %>% #remove provinces by isolating CAPITALIZED names
    mutate(region_name = ifelse(word(region_name, 1) == 'Kota', 
                                region_name, 
                                str_c('Kabupaten ', region_name)))

  #Join poor_2018 to vf_data, pay attention to regencies in vf_data_2018 w/no match
  joined <- vf_data_2018 %>%
    left_join(poor_2018, by = c('regency' = 'region_name'))

  #Regencies have no match if and only if the poverty column is NA
  no_match <- joined[is.na(joined$poverty),]
  no_match

  #It turns out 20 of the 434 regencies have no match. Let's check them one by one...
  #Most are due to spelling variants that prevented a match!
    #Karangasem: it is written as Karang Asem in poor_2018
    #Muko Muko: it is written as Mukomuko in poor_2018
    #Pahuwato: it is written as Pohuwato in poor_2018 (just a writing variant)
    #Batanghari: it is written as Batang Hari in poor_2018
    #Mempawah: doesn't exist in poor_2018
    #Kotabaru: it is written as Kota Baru in poor_2018
    #Mahakam Ulu: it is written as Mahakam Hulu in poor_2018 (just a writing variant)
    #Tulang Bawang: it is written as Tulangbawang in poor_2018
    #Nduga: it is written as Nduga * in poor_2018
    #Fak Fak: it is written as Fakfak in poor_2018
    #Pangkajene Kepulauan: it is written as Pangkajene Dan Kepulauan in poor_2018 ('dan' means 'and')
    #Tojo Una Una: it is written as Tojo Una-Una in poor_2018 (just a writing variant)
    #Toli Toli: it is written as Toli-Toli in poor_2018 (just a writing variant)
    #Kep. Siau Tagulandang Biaro: it is written as Siau Tagulandang Biaro in poor_2018 (Kep. means islands)
    #Sawahlunto: it is written as Sawah Lunto in poor_2018
    #Banyuasin: it is written as Banyu Asin in poor_2018
    #Penukal Abab Lematan: it is written as Penukal Abab Lematang Ilir in poor_2018 (not sure why)
    #Labuhanbatu: it is written as Labuhan Batu in poor_2018
    #Labuhanbatu Selatan: it is written as Labuhan Batu Selatan in poor_2018
    #Labuhanbatu Utara: it is written as Labuhan Batu Utara in poor_2018

  #Actually, out of those 20 no-matches, there is actually only 1 no-match!
    #However, a Wikipedia search tells us that based on Government Regulation No. 58 in 2014
    #(Peraturan Pemerintah Nomor 58 Tahun 2014), Pontianak Regency formally became Mempawah Regency
    #and Kabupaten Pontianak exists in poor_2018!

  #Let us now manually rename the region_name in poor_2018
  #One approach:
    #1. create a vector of values to be replaced (call it the 'to' vector)
    #2. create a vector of replacement values (call it the ,from' vector)
    #3. use plyr's mapvalues() function to replace elements in 'to' with corresponding element in 'from' 
    
    to <- no_match$regency
    from <- c('Kabupaten Karang Asem', 'Kabupaten Mukomuko', 'Kabupaten Pohuwato',
              'Kabupaten Batang Hari', 'Kabupaten Pontianak', 'Kota Baru', 
              'Kabupaten Mahakam Hulu', 'Kabupaten Tulangbawang', 'Kabupaten Nduga *',
              'Kabupaten Fakfak', 'Kabupaten Pangkajene Dan Kepulauan', 
              'Kabupaten Tojo Una-Una', 'Kabupaten Toli-Toli', 
              'Kabupaten Siau Tagulandang Biaro', 'Kota Sawah Lunto', 
              'Kabupaten Banyu Asin', 'Kabupaten Penukal Abab Lematang Ilir', 
              'Kabupaten Labuhan Batu', 'Kabupaten Labuhan Batu Selatan', 
              'Kabupaten Labuhan Batu Utara')
  
    poor_2018$region_name <- plyr::mapvalues(poor_2018$region_name, from = from, to = to)

  #Let's redo the join and ensure there is no regencies with no match
  joined <- vf_data_2018 %>%
    left_join(poor_2018, by =c('regency' = 'region_name'))

  no_match <- joined[is.na(joined$poverty),]
  no_match

  #Now no_match contains 0 observations and we've successfully joined poverty data to every regency

  #One last step is to create a new column specifying the amount of funding for every poor resident in each regency
  #Note that vfund_allocation is in 1000s of Rupiah, and poverty is in 1000s of residents
  #Let's divide vfund_allocation by poverty to get Rupiah amount per poor resident
  joined <- joined %>%
    mutate(vfund_per_poor = round(vfund_allocation/poverty))


### WE'RE DONE!
  
  #let's export this dataframe as a csv file for a subsequent mapping exercise
    write.csv(joined, 'joined.csv', row.names = FALSE)

  #let's also save the joined df as an .rdata file for next week
    vf_joined <- joined
    save(vf_joined, file = "vf_joined.rda")
    
  #and here's how you save the whole workspace with all data object
    save.image("vf_joined.rdata")
