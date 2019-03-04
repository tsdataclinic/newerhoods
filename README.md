# NewerHoods

<p align="center">
  <img src="newerhoods/images/NewerHoods.png" width="500"/>
</p>

New York City a city of neighborhoods. These neighborhoods have gone through a lot of changes over the years, but our definitions of neighborhoods have remained mostly the same. 

NewerHoods is a RShiny Application that allows you to reimagine New York City neighborhoods using open data and machine learning. This web app lets you select specific characteristics and visualize clusters of these qualities on a map of New York City, find similar neighborhoods, and compare them against existing geographic divisions.

NewerHoods uses multiple open datasets to generate characteristics related to housing, crime, and noise levels to begin with. These characteristics/features are evaluated at the census tract-level, and are then clustered to find pockets of tracts with homogenous characteristics. These are what we call NewerHoods.


### Directory Structure

`/data` contains just the cleaned/transformed data sets used directly by the Shiny App. 

`/src` contains all the code to merge and clean the data sets, extract features from it, and cluster the features. 

`/newerhoods` contains the code for the RShiny WebApp.


### Running the App

First, the R environment needs to be set up with all the necessary packages.

```r
source("newerhoods/setup.R")
```

The Shiny App uses Mapbox for the underlying map. Getting a token is free by signing up [here](https://www.mapbox.com/). Paste the token and run the code below.

```r
Sys.setenv('MAPBOX_ACCESS_TOKEN'="YOUR TOKEN HERE")
```

Run the App
```r
library(shiny)
runApp("newerhoods")
```

### Data Sources

1. [NYC Annualized Property Sales Data (2012-2017)](https://www1.nyc.gov/site/finance/taxes/property-annualized-sales-update.page)
2. [MapPLUTO (18v1)](https://www1.nyc.gov/site/planning/data-maps/open-data/dwn-pluto-mappluto.page)
3. [Geoclient API v1.1](https://developer.cityofnewyork.us/api/geoclient-api)
4. [Property Assessment Roll Archives](https://www1.nyc.gov/site/finance/taxes/property-assessment-roll-archives.page)
5. [NYPD Complaint Data Historic](https://data.cityofnewyork.us/Public-Safety/NYPD-Complaint-Data-Historic/qgea-i56i)
6. [311 Service Requests from 2010 to Present](https://data.cityofnewyork.us/Social-Services/311-Service-Requests-from-2010-to-Present/erm2-nwe9)

### References

1. [ClustGeo: an R package for hierarchical clustering with spatial constraints](https://arxiv.org/abs/1707.03897) 
2. [Making Neighborhoods - Understanding New York City Transitions 2000-2010](http://chpcny.org/assets/MakingNeighborhoodsPaper.pdf)

### License
This project is licensed under the Apache 2.0 License - see the LICENSE.md file for details

