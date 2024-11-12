################################################################################
##
## [ PROJ ] Mapping Lesson: Basic maps using tmap 
## [ FILE ] mapping_indonesiavf.r
## [ AUTH ] < YOUR NAME >
## [ INIT ] < Nov 11, 2024 >
##
################################################################################

#Mapping Indonesian village fund allocations (2018)

#In this exercise we'll map Village Funding allocations per resident in poverty
#The spatial unit of analysis will be "second level" administrative units: 
# regency (kabupaten) and kota (city)

#we'll focus on two packages for mapping:
#   1. the tmap package: https://github.com/r-tmap/tmap
#   2. ggplot


## -----------------------------------------------------------------------------
## libraries
## -----------------------------------------------------------------------------

#install.packages("sf")
#install.packages("tmap")
#Do this in advance of class!

library(sf)
library(tidyverse)
library(tmap)


## -----------------------------------------------------------------------------
## directory paths
## -----------------------------------------------------------------------------

getwd()


## -----------------------------------------------------------------------------
## get shapefile (geometry for Indonesia)
## -----------------------------------------------------------------------------

#a shapefile is format for storing geospatial information for GIS applications
#shapefiles contain geometry for maps (points and polygons that represent geographic features)

  indonesia <- st_read(dsn = 'indonesia.shp')
  #shape file can also be downloaded from https://gadm.org/download_country_v3.html


#inspect indonesia data frame - 
#  what classes does this object have? 
#  what is the unit of analysis?


#create a basic ggplot object g that shows Indonesia's regencies shaded by FID
  g1 <- ggplot(data = indonesia) +
    geom_sf(aes(fill = FID)) + #geometry layer for spatial objects (simple feature or sf objects)
    theme_bw() + 
    ggtitle("Indonesian Regencies")
  
  g1 


#tmap works a bit more seamlessly for thematic mapping than ggplot
# tmap uses a different "layered grammar of graphics" than ggplot
# the syntax is quite similar, but tailored for maps
# other mapping packages include leaflet, mapview

  ?tmap

#create a basic tmap object t that shows Indonesia's regencies shaded by FID
  t1 <- tm_shape(indonesia) + 
    tm_polygons()
  
  t1


## -----------------------------------------------------------------------------
## Load VF data, prepare to join to shapefile, create joined data frame
## -----------------------------------------------------------------------------

#load data on VF funding allocations, poverty counts, and VF funding/poverty
  load("vf_joined.RData")
  View(vf_joined) # take a look at coding of regency


#before joining, let's inspect and perform some data cleaning

#first step: to make indonesia joinable with vf_joined, 
#let's add Kabupaten to regency name which does not start with Kota
#or Kota Baru (since Kota Baru is a Kabupaten, an exception!)
  indonesia <- indonesia %>%
    mutate(name = ifelse(word(name, 1) != 'Kota' | name == 'Kota Baru',
                         str_c('Kabupaten ', name), name))

#also, Kabupaten Banjar where FID = 83 should be "Kota Banjar"
  
  #base R approach
    indonesia[indonesia$FID == 83, "name"] <- 'Kota Banjar'
  #or the tidyverse approach
    indonesia <- indonesia %>% 
      mutate(name = ifelse(FID == 83,'Kota Banjar', 
                           name))
  
#second step: let's join and see which regencies in vf_joined have no match
  
   vf_joined2 <- indonesia %>%
    right_join(vf_joined, by = c('name' = 'regency'))

  #equivalent joining code
  vf_joined2 <- vf_joined %>%
    left_join(indonesia, by = c('regency' = 'name'))
    
  
#which regencies have no match?
  
  #base R approach
    no_match <- vf_joined2[is.na(vf_joined2$FID),]
    View(no_match)

  #tidyverse approach
    vf_joined2 %>% 
      filter(is.na(FID) == TRUE) %>% 
      View
  
    
#do a manual check and perform a mapping correspondingly 
  #same code from strings and dates lesson (indonesian_village_fund.R)
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
            'Kabupaten Kota Baru', 'Kabupaten Tulangbawang', 'Kabupaten Ambon', 
            'Kabupaten Tual', 'Kabupaten Tidore Kepulauan', 'Kabupaten Fakfak', 
            'Kabupaten Pangkajene Dan Kepulauan', 'Kabupaten Tojo Una-Una', 'Kabupaten Toli-Toli', 
            'Kabupaten Siau Tagulandang Biaro', 'Kabupaten Kotamobagu', 'Kabupaten Pariaman', 
            'Kabupaten Sawahlunto', 'Kabupaten Banyu Asin', 'Kabupaten Prabumulih',
            'Kabupaten Pakpak Barat', 'Kabupaten Gunungsitoli', 'Kabupaten Padangsidimpuan')

  indonesia$name <- plyr::mapvalues(indonesia$name, from = from, to = to)
  
  
#let's do the join once again
  vf_joined2 <- indonesia %>%
    right_join(vf_joined, by = c('name' = 'regency'))

#which regencies have no match?
  no_match <- vf_joined2 %>% 
    filter(is.na(FID) == TRUE)
  nrow(no_match)
  
  
#we managed to reduce 49 non-matches to 17 non-matches 
#remaining non-matches mostly due to new regencies not contained in shapefile

  
#get rid of remaining non-matches for now
  vf_joined_final <- vf_joined2 %>%
    filter(is.na(FID) == FALSE)
  

#now we are ready to map!


## -----------------------------------------------------------------------------
## Map VF allocations for each regency (vfund_per_poor)
## -----------------------------------------------------------------------------

# ggplot version first. remember to spsecify the fill aesthetic.
  g2 <- ggplot(vf_joined_final) + 
    geom_sf(aes(fill = vfund_per_poor)) +
    scale_fill_fermenter(breaks = c(0, 10000000, 20000000, 30000000, 40000000)
                         , palette = "BuPu", direction = +1)
  g2
  
  
  qt <- quantile(vf_joined_final$vfund_per_poor,
           probs = c(0, 0.25, 0.50, 0.75,1 ))
  
  ggplot(vf_joined_final) + 
    geom_sf(aes(fill = vfund_per_poor)) +
    scale_fill_fermenter(breaks = qt
                         , palette = "BuPu", direction = +1)
  
  
# tmap version - a bit better. specify the tm_fill layer..
  # ?tmap
  t2 <- tm_shape(vf_joined_final) + 
    tm_fill('vfund_per_poor') +  #tm_polygon is tm_fill and tm_borders together
    tm_borders() + 
    tm_legend(outside = TRUE)
  
  t2


  
# modifying the color schemes

  # suppose we want to have one color for every multiple of 10,000,000
  # let's explore what each component does!
    t3 <- tm_shape(vf_joined_final) + 
      tm_fill('vfund_per_poor', 
              palette = 'Reds',
              style = 'fixed', 
              breaks = c(0, 10000000, 20000000, 30000000, 40000000),
              labels = c('Under 10M', '10M to 20M', '20M to 30M', 'Above 30M'),
              title = 'Fund per poor individual (Rp)') + 
      tm_borders() +
      tm_legend(outside = TRUE) 
    
    t3


# modifying the vfund/poor breaks for mapping values to colors
    
  #suppose we want to split vfund/poor into quartiles (up to 25th pctile, 26-50, etc.)
  #then use these quartiles as the breaks for the tm_fill() layer
    qt <- quantile(vf_joined_final$vfund_per_poor, probs = c(0, 0.25, 0.5, 0.75, 1))
  
    t4 <- tm_shape(vf_joined_final) + 
      tm_fill('vfund_per_poor', 
              palette = 'Reds',
              style = 'fixed', 
              breaks = qt,
              title = 'Fund per poor individual (Rp)') + 
      tm_borders() +
      tm_legend(outside = TRUE)
    
    t4
  
