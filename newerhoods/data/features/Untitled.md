read csvs  *DONE*
merge csvs *DONE*

**TODO: Requires file to have a column called "boro_ct201". 
If users select which columns correspond to boro and tract, 
we can create this internally**

read json
    - category                      *DONE*
    - {colname: pretty col name}    *DONE*
    - {colname: legend name}        *DONE*
    - {colname: legend units}       *DONE*
    - {scriptoverride}              *DONE*
    - {colname: excludeTracts}      *DONE*

parse json    *DONE*
  df with cols {category, column_name, pretty_name, legend_name, 
                legend_text, excludeTracts, override script}

create UI code *DONE*
 - add override *DONE*

In ui.R
----------------
eval UI code text in components *DONE*

In support_functions.R
----------------
Legend text function
  use legend names etc. from parsed json *DONE*

In server.R
----------------
Include created variables in user selection logic *Almost DONE* _missing from snackbar_
Subset tracts based on the columns selected *DONE*
estimate rates based on is_rate flag in feature info 

  