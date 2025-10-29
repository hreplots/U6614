################################################################################
##
## [ PROJ ] Mapping Lesson: Basic maps using tmap 
## [ FILE ] mapping_indonesiavf.r
## [ AUTH ] < YOUR NAME >
## [ INIT ] < Oct 28, 2025 >
##
################################################################################

# Mapping Indonesian Village Fund allocations and natural disasters

# Village Fund Allocation for Disaster Mitigation: Solution or Another Gimmick? 

# "Since 2014, the Indonesian govt has implemented a Village Fund program
# for all villages in Indonesia, with the intention of stimulating village
# development while also ensuring rural communities' welfare. This stimulus
# is augmented by the need that each village allocate a maximum of 35% of 
# the Village Fund to disaster management and its impact on the community...


# First, let's Village Funding allocations per resident in poverty
# The spatial unit of analysis will be "second level" administrative units: 
# regency (kabupaten) and kota (city)

# According to the World Population Review (https://worldpopulationreview.com/country-rankings/natural-disaster-risk-by-country),
# Indonesia is the second-most natural disaster-prone country in the world
# It is located on the Ring of Fire and three tectonic plates, 
# which increases its susceptibility to earthquakes. 
# Indonesia is also highly prone to tsunamis.

# We'll overlay the location of natural disasters on Village Fund allocations,
# to see how funding aligns with disaster locations for mitigation purposes.

# https://search.earthdata.nasa.gov/search/granules/collection-details?p=C3540930147-ESDIS

# today we'll focus on ggplot for mapping

# tmap is another useful package for mapping: https://github.com/r-tmap/tmap


## -----------------------------------------------------------------------------
## check working directory
## -----------------------------------------------------------------------------

getwd()


## -----------------------------------------------------------------------------
## libraries
## -----------------------------------------------------------------------------

#install.packages("sf")
#Do this in advance of class!

library(sf)
library(tidyverse)


## -----------------------------------------------------------------------------
## get shapefile (geometry for Indonesia)
## -----------------------------------------------------------------------------

# a shapefile is format for storing geospatial information for GIS applications
# shapefiles contain geometry for maps 
# (points and polygons that represent geographic features)

indonesia <- st_read(dsn = 'indonesia.shp')
# shape file can also be downloaded from https://gadm.org/download_country_v3.html


# inspect indonesia data frame - 
#   what classes does this object have? 
#   what is the unit of analysis?
#   what coordinate system is used?
class(indonesia)
View(indonesia)
st_crs(indonesia)
crs_info <- st_crs(indonesia) # we'll refer back to this later


# create a basic ggplot object that maps regencies in Indonesia
ggplot(data = indonesia) +
  geom_sf() + #geometry layer for spatial objects (simple feature or sf objects)
  theme_bw() + 
  ggtitle("Indonesian Regencies")

# create a basic ggplot object that shows Indonesia's regencies shaded by FID
ggplot(data = indonesia) +
  geom_sf(aes(fill = FID)) + #geometry layer for spatial objects (simple feature or sf objects)
  theme_bw() + 
  ggtitle("Indonesian Regencies")




## -----------------------------------------------------------------------------
## load VF data, prepare to join to shapefile, create joined data frame
## -----------------------------------------------------------------------------

# load data on VF funding allocations, poverty counts, and VF funding/poverty
load("vf_joined.RData")
View(vf_joined) # take a look at coding of regency


# before joining, let's inspect and perform some data cleaning

# first let's fix a spelling inconsistency 
indonesia <- indonesia %>%
  mutate(name = ifelse(name == 'Kota Baru', 'Kotabaru', name))

# now to make indonesia joinable with vf_joined,
# let's add Kabupaten to regency name which does not start with Kota
# or Kota Baru (since Kota Baru is a Kabupaten, an exception!)
indonesia <- indonesia %>%
  mutate(name = ifelse(word(name, 1) != 'Kota',
                       str_c('Kabupaten ', name),
                       name))

# also, Kabupaten Banjar where FID = 83 should be "Kota Banjar"
indonesia <- indonesia %>% 
  mutate(name = ifelse(FID == 83,'Kota Banjar', name))

# second step: let's join and see which regencies in vf_joined have no match
vf_joined2 <- indonesia %>%
  right_join(vf_joined, by = c('name' = 'regency'))
View(vf_joined2)

# which regencies have no match?
no_match <- vf_joined2 %>% 
  filter(is.na(FID) == TRUE)
View(no_match)


# a previous TA, Kevin Wibosono, manually investigated non-matching regencies
# created a crosswalk of un-matched regency names from the shape file to VF data
# we'll use this crosswalk to recode the regency in the indonesia shapefile
# (using same code from strings and dates lesson, indonesian_village_fund.R)
to <- c('Kota Banda Aceh', 'Kota Langsa', 'Kota Lhokseumawe', 'Kota Sabang',
        'Kota Subulussalam', 'Kota Denpasar', 'Kabupaten Muko Muko', 'Kabupaten Pahuwato',
        'Kabupaten Batanghari', 'Kabupaten Tanjung Jabung Barat', 'Kabupaten Tanjung Jabung Timur',
        'Kota Sungai Penuh', 'Kota Batu', 'Kabupaten Mempawah',
        'Kabupaten Kotabaru', 'Kabupaten Tulang Bawang', 'Kota Ambon', 
        'Kota Tual', 'Kota Tidore Kepulauan', 'Kabupaten Fak Fak', 
        'Kabupaten Pangkajene Kepulauan', 'Kabupaten Tojo Una Una', 'Kabupaten Toli Toli', 
        'Kabupaten Kep. Siau Tagulandang Biaro', 'Kota Kotamobagu', 'Kota Pariaman', 
        'Kota Sawahlunto', 'Kabupaten Banyuasin', 'Kota Prabumulih',
        'Kabupaten Pakpak Bharat', 'Kota Gunungsitoli', 'Kota Padangsidimpuan')

from <- c('Kabupaten Banda Aceh', 'Kabupaten Langsa', 'Kabupaten Lhokseumawe', 'Kabupaten Sabang',
          'Kabupaten Subulussalam', 'Kabupaten Denpasar', 'Kabupaten Mukomuko', 'Kabupaten Pohuwato',
          'Kabupaten Batang Hari', 'Kabupaten Tanjung Jabung B', 'Kabupaten Tanjung Jabung T',
          'Kabupaten Sungai Penuh', 'Kabupaten Batu', 'Kabupaten Pontianak',
          'Kota Baru', 'Kabupaten Tulangbawang', 'Kabupaten Ambon', 
          'Kabupaten Tual', 'Kabupaten Tidore Kepulauan', 'Kabupaten Fakfak', 
          'Kabupaten Pangkajene Dan Kepulauan', 'Kabupaten Tojo Una-Una', 'Kabupaten Toli-Toli', 
          'Kabupaten Siau Tagulandang Biaro', 'Kabupaten Kotamobagu', 'Kabupaten Pariaman', 
          'Kabupaten Sawahlunto', 'Kabupaten Banyu Asin', 'Kabupaten Prabumulih',
          'Kabupaten Pakpak Barat', 'Kabupaten Gunungsitoli', 'Kabupaten Padangsidimpuan')

indonesia$name <- plyr::mapvalues(indonesia$name, from = from, to = to)

# let's do the join once again
vf_joined2 <- indonesia %>%
  right_join(vf_joined, by = c('name' = 'regency'))

# which regencies have no match?
no_match <- vf_joined2 %>% 
  filter(is.na(FID) == TRUE)
nrow(no_match)

# we managed to reduce 48 non-matches to 17 non-matches 
# remaining non-matches mostly due to new regencies not listed in shapefile

# remove remaining non-matches for now
vf_joined_final <- vf_joined2 %>%
  filter(is.na(FID) == FALSE)


# now we are ready to map!


## -----------------------------------------------------------------------------
## map VF allocations for each regency (vfund_per_poor)
## -----------------------------------------------------------------------------

# use the fill aesthetic to map vfund_per_poor
# what breaks do you want to use for the color scale?
ggplot(vf_joined_final) + 
  geom_sf(aes(fill = vfund_per_poor)) +
  scale_fill_fermenter(breaks = c(0, 10000000, 20000000, 30000000, 40000000),
                       palette = "BuPu",
                       direction = +1)

# QUESTION: is the fill color scale informative? do you have ideas to improve it?
# ANSWER: virtually all regencies are in the bottom category,
#         which makes it hard to visualize variation across space
#         let's use quantiles (more specifically, quartiles)

# what is a quantile?

# a quantile is a value that divides a variable into equal-sized subgroups, 
# indicating the point below which a certain percentage of the data falls.
# for example, the 0.25 quantile (or first quartile) is the value below which
# 25% of the data lies, while the 0.5 quantile (the median) is the value below
# which 50% of the data falls

# let's create a vector of the quartiles of the VF funding per poor resident
qt <- quantile(vf_joined_final$vfund_per_poor,
               probs = c(0, 0.25, 0.50, 0.75,1 ))

# now let's use this vector to set the breaks for the fill aesthetic
ggplot(vf_joined_final) + 
  aes(fill = vfund_per_poor) +
  geom_sf() +
  scale_fill_fermenter(breaks = qt,
                       palette = "BuPu", direction = +1)


## -----------------------------------------------------------------------------
## next let's load the natural disaster data and overlay on existing map
## -----------------------------------------------------------------------------

loc <- read.csv('pend-gdis-1960-2018-disasterlocations.csv') 

# filter for disasters that occur in Indonesia
loc <- loc %>% filter(country == "Indonesia")

# inspect the df and the geospatial information
class(loc) # not a sf object
str(loc) # note that lat and long columns are numbers 

# we can set the df as a sf object using the st_as_sf() function in the sf package
loc_sf <- st_as_sf(loc, 
                   coords = c("longitude","latitude"), # select cols w/coordinates
                   crs = crs_info)

# now let's add another geospatial layer to map natural disaster locations
# now we have to move the data & aes parameters down to each geospatial layer!
ggplot() +
  geom_sf(data = vf_joined_final, 
          aes(fill = vfund_per_poor)) +
  # notice how we moved this data & aes down to the geom_sf layer
  scale_fill_fermenter(breaks = qt,
                       palette = "BuPu", direction = +1) +
  geom_point(data = loc, # new geospatial layer for mapping points
             aes(x = longitude, y = latitude),
             color = 'red',
             alpha = 0.2,
             size = 1.5) +
  theme_bw() + 
  labs(fill = "VF funding per poor resident") +
  ggtitle("Village Funding allocations and natural disaster locations")


## -----------------------------------------------------------------------------
## finally, let's perform a basic geospatial calculations
## -----------------------------------------------------------------------------

# let's calculate how many disasters occurred in each regency
# logically, how do we do this?
# we calculate how many disaster points are included within each regency polygon
lengths(st_intersects(indonesia, loc_sf))

# let's add this as a new column in the indonesia df
indonesia <- indonesia %>% 
  mutate(disasters = lengths(st_intersects(indonesia, loc_sf)))

# some other spatial operations
# here is a good reference for more details
# http://www.geo.hunter.cuny.edu/~ssun/R-Spatial/spatialops.html#spatial-operations

  # calculate the area of a polygon
  indonesia <- indonesia %>% 
    mutate(area = st_area(indonesia))
  
  # find all of the disaster points that intersect with Kabupaten Jakarta Pusat
  indonesia_jp <- indonesia %>% 
    filter(name == "Kabupaten Jakarta Pusat")
  
  jp_intersects <- st_intersects(indonesia_jp, loc_sf)
  class(jp_intersects)
  # this is a sgbp object: a “Sparse Geometry Binary Predicate" or sparse matrix
  # it's just a list w/integer vectors holding the indices for each polygon that intersects
  # in this we only have one vector, because we only intersect with one polygon
  # so we can extract this first vector with jp_intersects[[1]] and use it for subsetting:
    
  # subset the disaster df using results from the st_intersects() operation
  loc_sf %>% slice(jp_intersects[[1]]) %>% View()


