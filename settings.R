#### This File is a template storing settings needed for the project
#### Make a copy of this file and save as settings_local.R
#### Fill in the values for the tokens below in the local file 
#### Source the file at the start of the session to ensure
#### the tokens are assigned to variables in the environment.

## keys to access Geoclient API from a NYC Developer Portal Project
geoclient_api_app_id <- 'ENTER_TOKEN_HERE'
geoclient_api_app_key <- 'ENTER_TOKEN_HERE'

## general app token for Open Data APIs
nyc_opendata_api_token <- "ENTER_TOKEN_HERE"
mapbox_api_token <- "ENTER_TOKEN_HERE"

Sys.setenv('GEOCLIENT_API_APP_ID'=geoclient_api_app_id)
Sys.setenv('GEOCLIENT_API_APP_KEY'=geoclient_api_app_key)
Sys.setenv('NYC_OPENDATA_API_TOKEN'=nyc_opendata_api_token)
Sys.setenv('MAPBOX_ACCESS_TOKEN'=mapbox_api_token)
