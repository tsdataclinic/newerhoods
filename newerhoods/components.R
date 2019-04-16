header_nav <- withTags(
  header(class ="header",
    div(class="container-fluid", 
      a(href="https://www.twosigma.com/about/data-clinic/", target="_blank",
        div(class="navbar", 
          div(class="navbar-brand", ""),
          div(class="navbar-group", 
            div(class="navbar-name", 
              span(class="navbar-lg", "Newer"),
              span(class="navbar-sm", "hoods")
            ),
            div(class="navbar-subtitle", "FROM TWO SIGMA DATA CLINIC")
          )
        )
      )
    )
  )
)
### Modals
modal_features <- 
  bs_modal(
    id = "modal_features",
    title="Choose data sets to draw neighborhoods.",
      body= includeMarkdown("markdowns/features.md"),
    size="medium"
  )

modal_plots <- 
  bs_modal(
    id = "modal_plots",
    title="Interpreting Plots",
    body= includeMarkdown("markdowns/plots.md"),
    size="medium"
  )

### Info
info <- 
  div(class="text",
    div("Choose data sets to draw neighborhoods.",
    shiny_iconlink() %>%
      bs_attach_modal(id_modal = "modal_features")
    )
  ) 
  

### Inputs
input_housing <- checkboxGroupInput(
  inputId = 'housing',label="HOUSING",
  choices=c("Age of buildings"="bldg_age","Median Sale Price"="sale_price"),
  selected = "bldg_age"
)
  
input_housing_sales <- conditionalPanel(condition="input.housing.includes('sale_price')",
                                        radioButtons(
                                          inputId = 'sales_features',label="",
                                          choices=c("1y Average"="med_price_1y|sd_price_1y",
                                                    "3y Average"="med_price_3y|sd_price_3y",
                                                    "5y Average"="med_price_5y|sd_price_5y"
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
    c("Ice Cream truck"="icecream_rate",
      "Barking Dog"="animal_rate",
      "Loud Music/party"="party_rate"
    )
  )

input_clusters <- 
  pickerInput(inputId = "num_clusters",
              label="Map view",
              choices=c("5 neighborhoods"=5,"25 neighborhoods"=25, "100 neighborhoods"=100,  "200 neighborhoods"=200),
              selected = 100)

input_enable_heatmap <- 
  materialSwitch(inputId = "enable_heatmap", label = "Cluster map", status = "info")

info_plot_type <- shiny_iconlink() %>%
    bs_attach_modal(id_modal = "modal_plots")


input_baseline <- 
  selectInput('baseline',label='Compare against',
              choices=list("Community Districts (59)"="cds",
                           "Public use Microdata Areas (55)"="pumas",
                           "Neighborhood Tabulation Areas (195)"="ntas",
                           "Police Precincts (77)"="precincts",
                           "School Districts (33)"="school_dists"))

map_control_panel <- div(
  class="flex flex-between", 
  input_clusters,
  div(
    class="flex flex-end auto", 
    input_enable_heatmap,
    div(class="text", "Heat map"),
    info_plot_type
  )
)