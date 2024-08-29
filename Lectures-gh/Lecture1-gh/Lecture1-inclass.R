################################################################################
##
## [ PROJ ] Lecture1-inclass: Getting familiar with RStudio
## [ FILE ] Lecture1-inclass.r
## [ AUTH ] INSTRUCTOR FILE 
## [ INIT ] Sep 3, 2024
##
################################################################################

## -----------------------------------------------------------------------------
## 0. create an R project that is includes this R script (Lecture1-inclass.r)
## -----------------------------------------------------------------------------


## -----------------------------------------------------------------------------
## 1. look around and get our bearings.
## -----------------------------------------------------------------------------

# get working directory
  getwd()


## -----------------------------------------------------------------------------
## 2. install and load the gapminder package 
## -----------------------------------------------------------------------------

# first we have to install the package
  install.packages("gapminder")

# we can see the list of packages under in the bottom right panel of RStudio

# load this package
  library(gapminder)
    # can also load by going to pane in bottom right, under Packages tab, and
    # clicking the checkbox beside package name under "Packages"


## -----------------------------------------------------------------------------
## 3. Inspect gapminder data frame (in the gapminder package) w base R functions
##    (this exercise is based on STAT545 by Jenny Bryan)
## -----------------------------------------------------------------------------

# let's assign the gapminder data frame to a new data frame called gap
  gap <- gapminder

# let's use some functions to inspect the structure of the gap object
  str(gap) #str (structure) is the function, gap is the argument
    #notice that each variable has its own data type (Factor, int, num)

# access built-in help files/documentation (also use Google)
  ?str

# the function class() tells us what class(es) an object is assigned to... more next week
  class(gap)

# head is another function that shows us the first parts of an object
  head(gap)
  ?head
    # note the optional argument n, which is the # of rows you want to display

# let's try to see the first 10 rows
  head(gap, n = 10)

# to view full data frame use View(), or click on object in Environment pane
  View(gap)
  

## -----------------------------------------------------------------------------
## 4. Use some base R functions to perform some very basic exploratory analysis
## -----------------------------------------------------------------------------

# let's use some more base R functions to understand the data structure
  dim(gap) #get the number of rows and columns
  ncol(gap) #get the number of columns (alternatively, dim(gap)[2])
  nrow(gap) #get the number of rows (alternatively, dim(gap)[1])
  names(gap) #get the column names
  names(gap)[3] #get the name of the third column
    # note that indexing in R starts from 1, NOT 0

# we can assign the result of a function if we want to refer back to it:
  num_of_vars <- ncol(gap) #assign the results of nrow to new object num_of_vars
  num_of_vars #note that we need to type the object name again to view it

# get summary statistics for all variables
  summary(gap)
  #notice that the stats shown depend on the type of each column/variable
  #for Factor, it lists how many times each level appears
  #for int or num, it lists the 1st, 2nd, 3rd quartiles and min, max, mean

# let's plot the relationship of year (x) vs lifeExp (y) using base R
  plot(lifeExp ~ year, gap) #the tilde (~) operator sets up a "formula"
  plot(lifeExp ~ year, gap, xlab = "year", ylab = "life expectancy")
  
  # alternative 1
    plot((y = lifeExp) ~ (x = year), data = gap)

  # alternative 2
    plot(gap$year, gap$lifeExp)

  # let's look at the documentation
    ?plot

# let's apply some functions to specific variables (i.e. columns of data) 
# start with a continuous numeric variable (object type 'int')
# let's see how R uses $ to refer to a variable in a dataframe (subsetting)
  head(gap$lifeExp)
  summary(gap$lifeExp)
  hist(gap$lifeExp)

# next let's look at a factor variable

# what is a factor? let's look at some base R functions to figure it out
  str(gap)
  class(gap$continent)
  summary(gap$continent)
  levels(gap$continent) #returns the levels of a factor vector
  nlevels(gap$continent) #returns the number of levels of a factor vector
  # in this case, it's 5 since we have Africa, Americas, Asia, Europe and Oceania

  # Note: a factor is R's way of recognizing categorical variables... more next week

# here's one way to run a frequency table to check the number of obs per year
  table(gap$year)

# let's assign this frequency table to a new object called freq_byyear
  freq_byyear <- table(gap$year)

# then pass this object (a column of data!) as an argument to the barplot function
  barplot(freq_byyear)
