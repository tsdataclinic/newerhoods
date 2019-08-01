header_nav <- withTags(
  header(class ="header",
         div(class="content col-xs-11", 
             div(class="navbar-wrapper", 
                 a(href="https://www.twosigma.com/about/data-clinic/", target="_blank",
                   div(class="navbar-title", "NewerHoods"),
                   div(class="navbar-subtitle", "FROM TWO SIGMA DATA CLINIC")
                 )
             )
         )
  )
)
### Modals
modal_features <- 
  bsModal(
    id = "modal_features",
    title="Getting started with NewerHoods",
    body= includeMarkdown("markdowns/tutorial.md"),
    size="medium",
    trigger = "Help"
  )

modal_plots <- 
  bs_modal(
    id = "modal_plots",
    title="Interpreting Plots",
    body= includeMarkdown("markdowns/plots.md"),
    size="medium"
  )

modal_credits <- 
  bsModal(
    id = "modal_credits",
    title="NewerHoods",
    body= includeMarkdown("markdowns/intro.md"),
    size="medium",
    trigger = "Credits"
  )

modal_feedback <- 
  bsModal(
    id = "modal_feedback",
    title="Feedback",
    body= includeMarkdown("markdowns/appendix.md"),
    size="medium",
    trigger = "Feedback"
  )

modal_example <- 
  bsModal(
    id = "modal_example",
    title="Example Insights",
    body= includeMarkdown("markdowns/insights.md"),
    size="medium",
    trigger = "Example"
  )

### Info
info <- 
  div(class="text",
      div("Choose characteristics to draw neighborhoods.")
  ) 

## Upload inputs

upload_link <- actionLink(inputId = "upload",label="Upload Data")

upload_switch<- materialSwitch(
  inputId = "upload",
  label = "Upload data?",
  value = FALSE,
  status = "primary"
)

upload_file <- fileInput("file","Choose a File",
            multiple = FALSE,
            accept = c("text/csv","text/comma-separated-values",
                       "text/plain",".csv")
)

# Select Geographic id
upload_geo_id <- radioButtons(
    inputId = "geo",
    label = "Select Geographic Identifier", 
    choices = c("Latitude & Longitude" = "lat_lon",
                "Borough & Census Tract" = "boro_tract",
                "Combined Tract ID" = "boro_ct"),
    selected = NULL,
    inline = FALSE)

# Select lat/lon columns
upload_lat_lon <- conditionalPanel(
  condition = "input.geo == 'lat_lon'",
  selectInput("lat","Select Latitude column",
              choices = NULL, multiple = FALSE),
  selectInput("lon","Select Longitude column",
              choices = NULL, multiple = FALSE))

# Select boto/ct columns
upload_boro_ct <- conditionalPanel(
  condition = "input.geo == 'boro_tract'",
  selectInput("boro","Select Borough column",
              choices = NULL, multiple = FALSE),
  selectInput("ct","Select Tract column",
              choices = NULL, multiple = FALSE))

# Select boro_ct columns
upload_tract_id <- conditionalPanel(
  condition = "input.geo == 'boro_ct'",
  selectInput("boro_ct","Select combined tract identifier",
              choices = NULL, multiple = FALSE))

# Select feature columns
upload_feature <- selectInput("user_columns","Select columns to aggregate",
              choices = NULL, multiple = TRUE)

upload_done <- actionButton("upload_done","Done",class="btn-primary")


# upload_file <- conditionalPanel(
#   condition = "input.upload",
#   fileInput("file","Choose a File",
#             multiple = FALSE,
#             accept = c("text/csv","text/comma-separated-values",
#                        "text/plain",".csv"))
# )
# 
# # Select Geographic id
# upload_geo_id <- conditionalPanel(
#   condition = "input.upload",
#   radioButtons(
#     inputId = "geo",
#     label = "Select Geographic Identifier", 
#     choices = c("Latitude & Longitude" = "lat_lon",
#                 "Borough & Census Tract" = "boro_tract",
#                 "Combined Tract ID" = "boro_ct"),
#     selected = NULL,
#     inline = FALSE))
# 
# # Select lat/lon columns
# upload_lat_lon <- conditionalPanel(
#   condition = "input.upload && input.geo == 'lat_lon'",
#   selectInput("lat","Select Latitude column",
#               choices = NULL, multiple = FALSE),
#   selectInput("lon","Select Longitude column",
#               choices = NULL, multiple = FALSE))
# 
# # Select boto/ct columns
# upload_boro_ct <- conditionalPanel(
#   condition = "input.upload && input.geo == 'boro_tract'",
#   selectInput("boro","Select Borough column",
#               choices = NULL, multiple = FALSE),
#   selectInput("ct","Select Tract column",
#               choices = NULL, multiple = FALSE))
# 
# # Select boro_ct columns
# upload_tract_id <- conditionalPanel(
#   condition = "input.upload && input.geo == 'boro_ct'",
#   selectInput("boro_ct","Select combined tract identifier",
#               choices = NULL, multiple = FALSE))
# 
# # Select feature columns
# upload_feature <- conditionalPanel(
#   condition = "input.upload",
#   selectInput("user_columns","Select columns to aggregate",
#               choices = NULL, multiple = TRUE))
# 
# upload_done <- conditionalPanel(
#   condition = "input.upload",
#   actionButton("upload_done","Next",class="btn-primary"))

modal_upload <- 
  bsModal(
    id = "modal_upload",
    title="Upload Data",
    body= list(upload_file,
               upload_geo_id,
               upload_lat_lon,
               upload_boro_ct,
               upload_tract_id,
               upload_feature,
               upload_done),
    size="medium",
    trigger = "upload"
  )

### Inputs
input_housing <- checkboxGroupInput(
  inputId = 'housing',label="HOUSING",
  choices=c("Age of buildings"="bldg_age","Median sale price"="sale_price"),
  selected = "bldg_age"
)

input_housing_sales <- conditionalPanel(condition="input.housing.includes('sale_price')",
                                        radioButtons(
                                          inputId = 'sales_features',label="",
                                          choices=c("1y average"="med_price_1y|sd_price_1y",
                                                    "3y average"="med_price_3y|sd_price_3y",
                                                    "5y average"="med_price_5y|sd_price_5y"
                                          ),selected = NULL))


input_crime <- 
  checkboxGroupInput(
    inputId = 'crime_features', label="CRIME",
    c("Violations"="violation_rate",
      "Felonies"="felony_rate",
      "Misdemeanors"="misdemeanor_rate"
    )
  )

input_noise <- 
  checkboxGroupInput(
    inputId = 'call_features',label="311 COMPLAINTS",
    c("Ice cream truck"="icecream_rate",
      "Barking dog"="animal_rate",
      "Loud music/party"="party_rate"
    )
  )

input_clusters <-
  sliderInput("num_clusters",
              label="Number of neighborhoods",
              ticks = FALSE,
              min = 5,
              max = 200,
              value = 100)

input_enable_heatmap <- 
  materialSwitch(inputId = "enable_heatmap", label = "Cluster map", status = "info")

# info_plot_type <- shiny_iconlink() %>%
#   bs_attach_modal(id_modal = "modal_plots")

info_plot_type <- shiny_iconlink() %>%
  bs_embed_tooltip(title="The cluster map shows the city divided into the selected neighborhoods.
                          The colors are only to differentiate clusters from one another.
                          The heatmap shows the relative value for clusters averaged over the 
                          chosen characteristics.",
                   placement = "top")

input_baseline <- 
  selectInput('baseline',label='Compare against',
              choices=list("None"="none",
                           "Community Districts (59)"="cds",
                           "Public use Microdata Areas (55)"="pumas",
                           "Neighborhood Tabulation Areas (195)"="ntas",
                           "Police Precincts (77)"="precincts",
                           "School Districts (33)"="school_dists"),
              selected = "none")

input_user_features <- 
  pickerInput(inputId = 'user_features',#label=h6("311 Complaints"),
              choices=NULL,
              options=list(`actions-box`=TRUE,title="User Features"),
              multiple=TRUE)

## Download dropdown
# myDownloadButton <- function (outputId, label = "Download", class = NULL,icon=icon("download"),...) {
#   aTag <- tags$a(id = outputId, class = paste("btn btn-default shiny-download-link", class),
#                  href = "", target = "_blank", download = NA, icon, label, ...)
# }

download_dropdown <- dropdownButton(

  downloadButton("downloadGEOJson","GeoJSON",icon=icon("file-download")),
  br(),
  downloadButton("downloadPNG","png"),
  br(),
  bookmarkButton(label="Share",icon=icon("share-alt",lib="font-awesome"),id="share"),
  
  circle = TRUE, status = "danger",
  icon = icon("download",lib="font-awesome"), width = "300px",
  
  tooltip = tooltipOptions(title = "Click to see download options")
)



map_control_panel <- div(
  class="flex flex-between map-control", 
  div(class="xsflex", 
      input_clusters,
      input_baseline
  ),
  div(
    class="flex flex-end auto heatmap-group", 
    input_enable_heatmap,
    div(class="heat-map-label", "Heat map"),
    info_plot_type
  )
)

help_link <- actionLink(inputId = "Help",label="Help")
feedback_link <- actionLink(inputId = 'Feedback',label="Feedback")
credits_link <- actionLink(inputId = 'Credits',label="About")
example_link <- actionLink(inputId = 'Example',label="Examples")

intro_links <- 
  div(class="links flex",
      div(class="mainlink",credits_link),
      div(class="mainslink",help_link),
      div(class="mainslink",example_link),
      div(class="mainslink",feedback_link)
  )

footer <-
  div(class="footer", 
      div(class="content flex-between col-xs-11 xsflex",
          div(class="links", a(href="https://www.twosigma.com/about/data-clinic/", "© 2019 Data Clinic. All rights reserved.")),
          div(class="flex",
              div(class="links", a(href="https://www.twosigma.com/legal-disclosure/", "Legal Disclosure", target="_blank")),
              div(class="slink links", a(href="https://www.twosigma.com/legal-disclosure/privacy-policy/", "Privacy Policy", target="_blank"))
          )
      )
  )