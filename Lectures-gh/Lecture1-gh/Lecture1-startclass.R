################################################################################
##
## [ PROJ ] Lecture1-startclass: Getting familiar with RStudio
## [ FILE ] Lecture1-startclass.r
## [ AUTH ] INSTRUCTOR FILE 
## [ INIT ] Jan 21, 2025 
##
################################################################################

## -----------------------------------------------------------------------------
## 0. create an R project that includes this R script (Lecture1-startclass.r)
## -----------------------------------------------------------------------------


## -----------------------------------------------------------------------------
## 1. look around and get our bearings.
## -----------------------------------------------------------------------------

# get working directory



## -----------------------------------------------------------------------------
## 2. install and load the gapminder package 
## -----------------------------------------------------------------------------

# first we have to install the package
  

# load this package


## -----------------------------------------------------------------------------
## 3. Inspect gapminder data frame (in the gapminder package) w base R functions
##    (this exercise is based on STAT545 by Jenny Bryan)
## -----------------------------------------------------------------------------

# let's assign the gapminder data frame to a new data frame called gap


# let's use some functions to inspect gapminder dataframe (an object in the gapminder package)

  # access built in help function for str()
   

  # the function class() tells us what class(es) an object is assigned to
    
    
  # some other functions
    

## -----------------------------------------------------------------------------
## 4. Use some base R functions to perform some very basic exploratory analysis
## -----------------------------------------------------------------------------

# let's use some more base R functions to understand the data structure


# assign the number of columns in gap to a new object 
  
  
# get summary statistics


# let's plot the relationship of year (x) vs lifeExp (y) using base R


# what is a factor? let's look at some base R functions to figure it out

  
  