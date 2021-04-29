Gather Census Data
================

``` r
library(tidyverse)
```

    ## ── Attaching packages ───────────────────────────────────────────────────────────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.2     ✓ purrr   0.3.4
    ## ✓ tibble  3.0.1     ✓ dplyr   1.0.0
    ## ✓ tidyr   1.0.2     ✓ stringr 1.4.0
    ## ✓ readr   1.3.1     ✓ forcats 0.4.0

    ## ── Conflicts ──────────────────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(sf)
```

    ## Linking to GEOS 3.7.2, GDAL 2.4.2, PROJ 5.2.0

``` r
library(tidycensus)
library(tmap)
```

Explore Variables
=================

I captured information on variables available in the 2014-2018 5-year ACS and saved it as a .csv file. I manually scanned the info and selected some relevant attributes.

``` r
# v18_acs5 <- load_variables(2018, "acs5", cache = TRUE)
# # write.csv(v18_acs5, file="Data/vars2018_ACS5.csv", row.names=FALSE)


# v18_acs5_concepts <- as.data.frame(unique(v18_acs5$concept))
# colnames(v18_acs5_concepts) = c("concept")
# 
# v18_acs5_concepts %>% 
#   filter(str_detect(concept, "TRANSPORTATION"))
# 
# v18_acs5 %>% 
#   filter(concept == "MEANS OF TRANSPORTATION TO WORK")
```

Ideally, gather data by Census Block Group to match the KC MVA study, but consider Census Tract (larger, several Block Groups) if relevant data is not available.

Note: Kansas City stretches across portion of Jackson, Clay, Platte counties. I will need to gather data for multiple counties and toss the tracts/block groups outside the boundary of the KC MVA study.

Gather Data at Census Block Group Level
=======================================

``` r
# TOTAL POPULATION
# B01003_001    Estimate!!Total     - matches B01001_001

# MEDIAN HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 2018 INFLATION-ADJUSTED DOLLARS)
# B19013_001    Estimate!!Median household income in the past 12 months (in 2018 inflation-adjusted dollars)

# MEDIAN AGE BY SEX
# B01002_001    Estimate!!Median age --!!Total

# MEANS OF TRANSPORTATION TO WORK
# B08301_001    Estimate!!Total
# B08301_002    Estimate!!Total!!Car, truck, or van
# B08301_010    Estimate!!Total!!Public transportation (excluding taxicab)
# B08301_016    Estimate!!Total!!Taxicab
# B08301_017    Estimate!!Total!!Motorcycle
  # B08301_018  Estimate!!Total!!Bicycle
  # B08301_019  Estimate!!Total!!Walked
  # B08301_020  Estimate!!Total!!Other means
  # B08301_021  Estimate!!Total!!Worked at home

co_blkgrp_jack <- 
  get_acs(geography = "block group",
          variables = c(population    = "B01003_001",
                        median_income = "B19013_001",
                        median_age    = "B01002_001",
                        towrk_total   = "B08301_001",
                        towrk_car     = "B08301_002",
                        towrk_public  = "B08301_010",
                        towrk_taxi    = "B08301_016",
                        towrk_mcycle  = "B08301_017"),
          output    = "wide",
          state     = "MO", county = "Jackson County", 
          year      = 2018, survey = "acs5",
          geometry  = TRUE)

co_blkgrp_clay <- 
  get_acs(geography = "block group",
          variables = c(population    = "B01003_001",
                        median_income = "B19013_001",
                        median_age    = "B01002_001",
                        towrk_total   = "B08301_001",
                        towrk_car     = "B08301_002",
                        towrk_public  = "B08301_010",
                        towrk_taxi    = "B08301_016",
                        towrk_mcycle  = "B08301_017"),
          output    = "wide",
          state     = "MO", county = "Clay County", 
          year      = 2018, survey = "acs5",
          geometry  = TRUE)

co_blkgrp_plat <- 
  get_acs(geography = "block group",
          variables = c(population    = "B01003_001",
                        median_income = "B19013_001",
                        median_age    = "B01002_001",
                        towrk_total   = "B08301_001",
                        towrk_car     = "B08301_002",
                        towrk_public  = "B08301_010",
                        towrk_taxi    = "B08301_016",
                        towrk_mcycle  = "B08301_017"),
          output    = "wide",
          state     = "MO", county = "Platte County", 
          year      = 2018, survey = "acs5",
          geometry  = TRUE)

co_blkgrp_vars <- rbind(co_blkgrp_jack, co_blkgrp_clay, co_blkgrp_plat)
```

``` r
# save data to a file with st_write()
st_write(co_blkgrp_vars, "Data/census_blkgrp.shp", 
         layer ="census_blkgrp.shp", driver = "ESRI Shapefile",
         delete_layer = TRUE)
```

    ## Warning in abbreviate_shapefile_names(obj): Field names abbreviated for ESRI
    ## Shapefile driver

    ## Deleting layer `census_blkgrp' using driver `ESRI Shapefile'
    ## Writing layer `census_blkgrp' to data source `Data/census_blkgrp.shp' using driver `ESRI Shapefile'
    ## Writing 763 features with 18 fields and geometry type Multi Polygon.

``` r
# example code to read file back in
# co_blkgrp_vars.in <- st_read("Data/census_blkgrp.shp")
# colnames(co_blkgrp_vars.in)
# rm(co_blkgrp_vars.in)

# columns renamed by ESRI format ...
# population    "popltnE"  "popltnM"  
# median_income "mdn_ncE"  "mdn_ncM"  
# median_age    "medn_gE"  "medn_gM"  
# towrk_total   "twrk_ttE" "twrk_ttM"
# towrk_car     "twrk_cE"  "twrk_cM"  
# towrk_public  "twrk_pE"  "twrk_pM"  
# towrk_taxi    "twrk_txE" "twrk_txM" 
# towrk_mcycle  "twrk_mE"  "twrk_mM"  
```

### Couple of Quick Maps

**Median Income (Est & Measure of Est. (MOE)) by Block Groups across the 3 counties**

``` r
tm_shape(co_blkgrp_vars) +
  tm_polygons(col  =c("median_incomeE", "median_incomeM"), 
              title=c("Est",            "MOE"),
              palette="Greens") +
  tm_layout(main.title = "Median Income", legend.position = c("left", "bottom"))
```

![](md-figures/gather-census-unnamed-chunk-6-1.png)

**Percent of Fuel Vehicles as the Means of Transportation to Work** Fueled Vehicles include Car/Truck/Van, Motorcycle and Taxi (TBD), but exclude Public Transportation, Walking or Bicycle.

``` r
co_blkgrp_vars %>% 
  mutate(towork_fuel = (towrk_carE + towrk_taxiE + towrk_mcycleE)/towrk_totalE) %>% 
  tm_shape() +
    tm_polygons(col   = "towork_fuel",
                title = "Percent") +
    tm_layout(main.title = "Fueled Vehicle to Work", legend.position = c("left", "bottom"))
```

![](md-figures/gather-census-unnamed-chunk-7-1.png)

Gather Data at Census Tract Level
---------------------------------

``` r
# AGGREGATE NUMBER OF VEHICLES (CAR, TRUCK, OR VAN) USED IN COMMUTING BY WORKERS 16 YEARS 
#   ... AND OVER BY SEX
# B08015_001    Estimate!!Aggregate number of vehicles (car, truck, or van) used in commuting

co_tract_jack <- 
  get_acs(geography = "tract",
          variables = c(agg_vehicles = "B08015_001"),
          output    = "wide",
          state     = "MO", county = "Jackson County", 
          year      = 2018, survey = "acs5",
          geometry  = TRUE)

# Getting data from the 2014-2018 5-year ACS
# Using FIPS code '29' for state 'MO'
# Using FIPS code '095' for 'Jackson County'

co_tract_clay <- 
  get_acs(geography = "tract",
          variables = c(agg_vehicles = "B08015_001"),
          output    = "wide",
          state     = "MO", county = "Clay County", 
          year      = 2018, survey = "acs5",
          geometry  = TRUE)

# Getting data from the 2014-2018 5-year ACS
# Using FIPS code '29' for state 'MO'
# Using FIPS code '047' for 'Clay County'

co_tract_plat <- 
  get_acs(geography = "tract",
          variables = c(agg_vehicles = "B08015_001"),
          output    = "wide",
          state     = "MO", county = "Platte County", 
          year      = 2018, survey = "acs5",
          geometry  = TRUE)

# Getting data from the 2014-2018 5-year ACS
# Using FIPS code '29' for state 'MO'
# Using FIPS code '165' for 'Platte County'

co_tract_vars <- rbind(co_tract_jack, co_tract_clay, co_tract_plat)
```

``` r
# save data to a file with st_write()
st_write(co_tract_vars, "Data/census_tract.shp", 
         layer ="census_tract.shp", driver = "ESRI Shapefile",
         delete_layer = TRUE)
```

    ## Warning in abbreviate_shapefile_names(obj): Field names abbreviated for ESRI
    ## Shapefile driver

    ## Deleting layer `census_tract' using driver `ESRI Shapefile'
    ## Writing layer `census_tract' to data source `Data/census_tract.shp' using driver `ESRI Shapefile'
    ## Writing 263 features with 4 fields and geometry type Multi Polygon.

``` r
# example of read file back in
# co_tract_vars.in <- st_read("Data/census_tract.shp")

# columns renamed ...
# agg_vehiclesE   agg_vhE
# agg_vehiclesM   agg_vhM
```

### Quick Map

**Vehicles used in Commuting to Work** (Car, Truck, or Van)

``` r
#tmap_mode("view")

tm_shape(co_tract_vars) +
  tm_polygons(col  =c("agg_vehiclesE", "agg_vehiclesM"), 
              title=c("Est",           "MOE"),
              palette="Blues") +
  tm_layout(main.title = "Vehicles used in Commuting", legend.position = c("left", "bottom"))
```

![](md-figures/gather-census-unnamed-chunk-10-1.png)

``` r
#tmap_mode("plot")
```

``` r
# ----------------------------------------------------------------------------
# ONLY Available at County level (that's NOT USEFUL!)
# ----------------------------------------------------------------------------
# 
# # MEANS OF TRANSPORTATION TO WORK BY VEHICLES AVAILABLE FOR WORKPLACE GEOGRAPHY
# # Totals ... B08541_001 to 005 - available at a county level

# ----------------------------------------------------------------------------
# Also available at Census Track level, if useful
# ----------------------------------------------------------------------------
# 
# # ... MEANS OF TRANSPORTATION TO WORK
# #   XXXX SEX OF WORKERS BY 
# # B08006_001  Estimate!!Total                        
# # B08006_002  Estimate!!Total!!Car, truck, or van
# # B08006_016  Estimate!!Total!!Taxicab, motorcycle, or other means
# # B08006_008  Estimate!!Total!!Public transportation (excluding taxicab)
# # other ... 
# # B08006_014  Estimate!!Total!!Bicycle
# # B08006_015  Estimate!!Total!!Walked
# # B08006_017  Estimate!!Total!!Worked at home
```
