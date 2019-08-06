Follow these steps to begin creating NewerHoods of your own.

#### Step 1 Upload Data
If you have a dataset you wish to analyse using this tool, you can start by clicking "Upload your data". 
This will prompt you to select the file and fill out the required details to ingest and transform the data for analysis. 
Once you click "Done", you'll notice a list of generated features based on your inputs appearing in the sidebar.  

While we continue to develop this feature, here are some of the current limitations you'll have to keep in mind:

- Uploaded datasets are currently functional only with comma-separated text or csv files (i.e, tab, semicolon or space separated files might not work). Although, .xls/.xlsx files should work. 
- The maximum file size currently allowed is 50MB. 
- If you're having trouble when uploading your dataset, have a look at the [Sample Datasets](https://github.com/tsdataclinic/newerhoods/tree/master/newerhoods/sample_data) to ensure it's in the right format. 


Alternatively, you can simply use the tool with its in-built datasets and features detailed below.  

#### Step 2 Select features of interest
Select one or more chracteristics under Housing, Crime, and 311 Complaints from the sidebar and click Apply. The tool will return clusters based on your selection(s) and produce an output map. You can use the same button to update your selection at any stage. Details on each of the features are as below.

##### Housing

- Age of Building: The average age of residential real-estate from the time constructed.
- Median Sale Price: The median price per square footage of real-estate sold in 2017, 3-year average for 2015-2017, and 5-year average for 2013-2017.


##### Crime
Crimes are categorized into three levels of offense: felony, misdemeanor, and violation. The count of each of these types is calculated for each region and scaled by its respective population to obtain a rate.

- Violations: Also known as infractions, these are the most minor of offenses. For example: a speeding ticket, public intoxication, or jaywalking.
- Misdemeanors: More serious than violations but less severe than felonies, misdemeanors can carry up to a year in jail.
- Felonies: These are the most serious type of criminal offense. Felonies often involve serious physical harm to victims, but they also include offenses like white collar crimes and fraud schemes.


##### 311 Calls
One of the largest categories of complaints received on 311 in NYC has to do with noise. In this case, we look at just a few sub-categories of noise complaints (ice cream trucks, barking dogs, and loud music/parties). As with crime statistics, these are aggregated for each region and scaled by the population.

#### Step 2 Choose number of new neighborhoods
Next, you can use the slider to choose the number of NewerHoods into which you want to divide New York City. The map automatically updates every time you change the number to neighborhoods to view. As a starting point, consider settings that correspond to the city’s existing administrative divisions.

#### Step 3 Dig into the numbers
Having generated a map of NewerHoods, you can pan and zoom the map to your area of interest. Hovering over a cluster, you'll find the NewerHood highlight and a pop-up window showing the average statistics of the characteristics chosen.

#### Step 4 Check out different views
When you select the Heatmap plot, the NewerHoods will be colored according to the magnitude of the values. When multiple features are chosen, the magnitude represents a combination of all the chosen features. You can use them to find broadly similar neighborhoods by simply looking at similarly colored areas.

#### Step 5 Compare
Lastly, you can compare the NewerHoods to existing geographical boundaries. The chosen division appears on the map as a white dashed line. The available administrative divisions and their counts are as below:

- Community Districts: 59
- Public Use Microdata Areas (PUMAs): 55
- Neighborhood Tabulation Areas (NTAs): 195
- Police Precincts: 77
- School Districts: 33

