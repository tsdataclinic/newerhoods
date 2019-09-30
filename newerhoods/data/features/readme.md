Steps to add a new dataset to NewerHoods.

1. Add a csv of the dataset to be added into the `transformed` folder. Make sure the csv has a column called `boro_ct201` which is a 7-digit combined borough and tract ID.

2. Edit the `info.json` file to include details regarding the new dataset you've added. The information here is used internally to look for the file you just added above, and externally in how this is diplayed in the UI. 

Add a json element such as below to the array and fill out the details.

```{json}
    { "file_name"       : "crime_rates",
      "create_ui"       : true,
      "category_name"   : "Crime",
      "feature_columns" : [
                      { "column_name"   :"violation_rate",
                        "checkbox_name" :"Violations",
                        "legend_name"   :"Violations",
                        "legend_units"  :"/1000 people",
                        "is_rate"       :true
                        },
                        
                      { "column_name"   :"felony_rate",
                        "checkbox_name" :"Felonies",
                        "legend_name"   :"Felonies",
                        "legend_units"  :"/1000 people",
                        "is_rate"       :true
                        },
                        
                      { "column_name"   :"misdemeanor_rate",
                        "checkbox_name" :"Misdemeanors",
                        "legend_name"   :"Misdemeanors",
                        "legend_units"  :"/1000 people",
                        "is_rate"       :true
                        }
                    ]
      }
```
*Note: Make sure you add a comma between elements :)*

`file_name` is the name of the csv file you added in step 1
`create_ui` is a boolean to indicate if you want the UI elements to select features from this should be created automatically
`category_name` is the name you want to give this section of features in the UI
`feature_columns` is an array that provides details for each column. An element of this array looks like below

```{json}
{ "column_name"   :"violation_rate",
  "checkbox_name" :"Violations",
  "legend_name"   :"Violations",
  "legend_units"  :"/1000 people",
  "is_rate"       :true
}
```
`column_name` is the name of the column in the csv
`checkbox_name` is the name you want to give to this feature in the checkbox in the UI 
`legend_name` is the text that should appear in the legend/label when interacting with the map
`legend_units` is unit for the feature in the legend/label. If no units, pass empty string ""
`is_rate` is a boolean to indicate if the measure is a population based rate. The cluster averages are then weighted by population of each tract. 


3. Lastly, run the following code which processes the newly uploaded shapefile.

```{r}
source("newerhoods/process_data.R")
generate_data()
```