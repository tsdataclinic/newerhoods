##### *Redefining NYC neighborhood boundaries using open data and machine learning*
By [Data Clinic](https://www.twosigma.com/about/data-clinic/)
___

#### Purpose

New York City’s (NYC’s) neighborhoods are a driving force in the lives of New Yorkers—their identities are closely intertwined and a source of pride. However, the history and evolution of NYC’s neighborhoods don’t follow the rigid, cold lines of statistical and administrative boundaries. Instead, the neighborhoods we live and work in are the result of a more organic confluence of factors.

Data Clinic developed NewerHoods with the goal of helping individuals and organizations better advocate for their communities by enabling them to tailor insights to meet their specific needs. NewerHoods is an interactive web-app that uses open data to generate localized features at the census tract-level and machine learning to create homogeneous clusters. Users are able to select characteristics of interest (currently open data on housing, crime, and 311 complaints), visualize NewerHood clusters on an interactive map, find similar neighborhoods, and compare them against existing administrative boundaries.  The tool is designed to enable users without in-depth data expertise to compare and incorporate these redefined neighborhoods into their work and life. 

#### Data Sources

1. [NYC Annualized Property Sales Data (2012-2017)](https://www1.nyc.gov/site/finance/taxes/property-annualized-sales-update.page)
2. [MapPLUTO (18v1)](https://www1.nyc.gov/site/planning/data-maps/open-data/dwn-pluto-mappluto.page)
3. [Geoclient API v1.1](https://developer.cityofnewyork.us/api/geoclient-api)
4. [Property Assessment Roll Archives](https://www1.nyc.gov/site/finance/taxes/property-assessment-roll-archives.page)
5. [NYPD Complaint Data Historic](https://data.cityofnewyork.us/Public-Safety/NYPD-Complaint-Data-Historic/qgea-i56i)
6. [311 Service Requests from 2010 to Present](https://data.cityofnewyork.us/Social-Services/311-Service-Requests-from-2010-to-Present/erm2-nwe9)

#### References

1. [ClustGeo: an R package for hierarchical clustering with spatial constraints](https://arxiv.org/abs/1707.03897) 
2. [Making Neighborhoods - Understanding New York City Transitions 2000-2010](http://chpcny.org/assets/MakingNeighborhoodsPaper.pdf)

