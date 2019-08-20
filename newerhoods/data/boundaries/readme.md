Steps to add a new baseline map to NewerHoods.

1. Create a folder for the boundary map inside the `raw` folder and drop the shapefile inside it. This is where the shapefiles sit. Generally, boundary maps are available in zip files that have multiple files which account for different geographical layers. If that's the case, simply unzip the file inside `raw`.

2. Edit the `info.json` file to include details regarding the new map you've added. The information here is used internally to look for the file you just added above, and externally in how this is diplayed in the UI. 

Add a json element such as below to the array and fill out the details.

```{json}
    { "folder"  : "City Council Districts",
      "name"    : "City Council Districts",
      "source"  : "https://data.cityofnewyork.us/City-Government/City-Council-Districts/yusd-j4xi"
    }
```
*Note: Make sure you add a comma between elements :)*

`folder` is the folder name you created in step 1 within which you placed the shapefile
`name` is the name you want to give to this. This will be displayed in the "Compare with" dropdown in the UI
`source` is the URL from which you got the shapefile. This helps us keep track of all our data sources

3. Lastly, run the following code which processes the newly uploaded shapefile.

```{r}
source("newerhoods/process_data.R")
process_baseline_maps()
```