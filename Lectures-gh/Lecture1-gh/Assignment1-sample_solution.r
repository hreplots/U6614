################################################################################
##
## [ PROJ ] Assignment1
## [ FILE ] Assignment1-sample_solution.R
## [ AUTH ] < INSTRUCTOR >
## [ DATE ] < SUBMISSION DATE >
##
################################################################################

## Assignment 1: Getting familiar with R & RStudio

## Include the code to answer the following
## Use comments to organize and provide your responses to questions
## Submit only this R script (Assignment1.r)


## -----------------------------------------------------------------------------
## 0. create a new R project called Assignment1
## -----------------------------------------------------------------------------

# This is for your internal project management, do not submit your .rproj file



## -----------------------------------------------------------------------------
## 1. load the gapminder package (to access the gapminder data)
## -----------------------------------------------------------------------------

library(gapminder)



## -----------------------------------------------------------------------------
## 2. a) use the str() function to give an overview of the gapminder data frame
##        (found in the gapminder package)
##    b) how many observations and variables are there?
## -----------------------------------------------------------------------------

# a
str(gapminder)

#b. 1,704 observations of 6 variables



## -----------------------------------------------------------------------------
## 3. a) what is the average gdpPercap across all observations in the dataset
##    b) use ?gapminder to view the documentation & find the units for gdpPercap
##    c) how would you intepret this mean, what is it the mean of?
##       HINT: try executing View(gapminder)
## -----------------------------------------------------------------------------

#a
mean(gapminder$gdpPercap) #7,215.3

#b 
?gapminder #GDP per capita is measured in US$, inflation-adjusted

#c
View(gapminder)
#7,215.3 is the mean GDP/cap across all 12 observations for 142 countries from 
#1952 to 2007 in increments of 5 years
#probably better to first look at the distribution of gdpPercap across 
#countries for a single year or for a single country over time



## -----------------------------------------------------------------------------
## 4. a) plot year vs. gdpPercap
##    b) what does the plot say about economic growth over time (1 sentence)
##    c) describe what a better visualization might look like 
##       (don't worry about how to do this in R... we'll get there!)
##       TIP: try typing your answer using ENTER to fit in the margins,
##            then select the text & use CTRL+SHIFT+C to comment multiple lines
## -----------------------------------------------------------------------------

#a
plot(gdpPercap ~ year, gapminder)

#b. GDP per capita appears to be rising over time

#c
# instead of plotting each country, we might want to plot the yearly mean
# across countries, maybe even weighting by population. we're probably more
# interested in characterizing the distribution within a year (e.g. mean, CI)
# than plotting every observation



## -----------------------------------------------------------------------------
## 5. create a barplot showing the number of observations in each continent
##    a) start by using the table function with continent as its argument,
##       assign the results of this function to a new object
##    b) next pass the object you created as an argument to the barplot function
## -----------------------------------------------------------------------------

#a
freq_bycon <- table(gapminder$continent)

#b
barplot(freq_bycon)

