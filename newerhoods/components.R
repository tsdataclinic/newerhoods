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
    div("Choose data sets to draw neighborhoods.")
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
  sliderInput("num_clusters",
              label="Map view",
              ticks = FALSE,
              min = 5,
              max = 200,
              value = 100)

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
  div(class="xsflex", 
    input_clusters,
    input_baseline
  ),
  div(
    class="flex flex-end auto", 
    input_enable_heatmap,
    div(class="heat-map-label", "Heat map"),
    info_plot_type
  )
)

intro_links <- 
  withTags({
    div(
      class="xsflex",
      span(class='link', 'Getting started') %>% bs_attach_modal(id_modal = "modal_features"),
      ul(
        class="links flex", 
        li(class="slink", a(href="/", "Intro video")),
        li(class="slink", a(href="/", "Credits"))
      )
    )
  })

footer <-
  div(class="footer", 
    div(class="content flex flex-between col-xs-11",
      div(class="hint", "© 2019 Data Clinic. All rights reserved."),
      div(class="flex",
        div(class="links", a(href="https://www.twosigma.com/legal-disclosure/", "Legal Disclosure", target="_blank")),
        div(class="slink links", a(href="https://www.twosigma.com/legal-disclosure/privacy-policy/", "Privacy Policy", target="_blank"))
      )
    )
  )