################################################################################
##
## [ PROJ ] Lecture1-startclass: Getting familiar with RStudio
## [ FILE ] Lecture1-startclass.r
## [ AUTH ] INSTRUCTOR FILE 
## [ INIT ] Sep 2, 2025 
##
################################################################################

## -----------------------------------------------------------------------------
## 0. create an R project that includes this R script (Lecture1-startclass.r)
## -----------------------------------------------------------------------------

# this is an R script!
# an R script is just a bunch of text saved in a file with the .R extension
#   that RStudio knows to be R code (along with code comments like this)

# first create a project by selecting File -> New Project
# - when you create or open a project, RStudio will reload the workspace
# - revisit your R script by opening the .rproj file in the same directory


# let's check the working directory by executing the getwd() function
getwd()


## -----------------------------------------------------------------------------
## 1. install and load the gapminder package 
## -----------------------------------------------------------------------------

# first we have to install the package (once and only once!)
  

# load this package


## -----------------------------------------------------------------------------
## 2. Inspect gapminder data frame (in the gapminder package) w base R functions
##    (this exercise is based on STAT545 by Jenny Bryan)
## -----------------------------------------------------------------------------

# let's assign the gapminder data frame to a new data frame called gap


# let's use some functions to inspect gapminder dataframe (an object in the gapminder package)

  # access built in help function for str()
   
  # here are some other inspection functions
    

## -----------------------------------------------------------------------------
## 3. Use some base R functions to perform some very basic exploratory analysis
## -----------------------------------------------------------------------------

# let's use some more base R functions to understand the data structure


# assign the number of columns in gap to a new object 
  
  
# get summary statistics


# let's plot the relationship of year (x) vs lifeExp (y) using base R


# what is a factor? let's look at some base R functions to figure it out

  
  