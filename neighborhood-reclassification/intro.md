### New York City Neighborhood Reclassification

#### What is a neighborhood?


#### Need for reclassifying
- Administrative boundaries are arbitrarily drawn, static and do not entirely reflect the realities on the ground. 
- Social science research heavily depends on local/neighborhood effects to account for quality of life.  

#### How we are redefining neighborhoods
Making use of NYC Open Data to gather wide variety of information at the local level that contribute to the quality of life. 
- Cost of living from real-estate sale prices (highly correlated with rental prices)
- Access to resources from facilities database
- etc. 

##### Open Data
In this current version, we are focusing on capturing cost of living:

1. NYC Annualized Property Sales Data (2012-2017)
2. MapPLUTO (18v1)
3. Geoclient API v1.1
4. Property Assessment Roll Archives

##### Features
From these datasets, we extract features such as the average price per square footage in the last 1 year, 3 year, and 5 year period, the variance in the prices during each of these windows, the number of residential units available, and the average age of buildings.

##### Methodology
Spatial hierarchical clustering to find groups of tracts similar to each other which is the new boundary of a neighborhood.