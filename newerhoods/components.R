source("support_functions.R")
source("process_data.R")

medium_link <- tags$a(icon("medium", lib = "font-awesome"),href="https://medium.com/@dataclinic", target="_blank")
github_link <- tags$a(icon("github-square", lib = "font-awesome"),href="https://github.com/tsdataclinic/newerhoods", target="_blank")
twitter_link <- tags$a(icon("twitter-square", lib = "font-awesome"),href="https://twitter.com/tsdataclinic?lang=en", target="_blank")

social_links <- div(class="links flex",
                    div(class="imglink",medium_link),
                    div(class="imglink",github_link),
                    div(class="imglink",twitter_link)
)

header_nav <- withTags(
  header(class ="header",
         div(class="content col-xs-11", 
             div(class="navbar-wrapper", 
                 a(href="https://www.twosigma.com/about/data-clinic/", target="_blank",
                   div(class="navbar-title", "NewerHoods"),
                   div(class="navbar-subtitle", "FROM TWO SIGMA DATA CLINIC")
                 ),
                 div(class="social-header",social_links)
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

upload_link <- actionButton("upload","Upload your data",icon=icon("upload",lib="font-awesome"),class="btn-custom")
#actionLink(inputId = "upload",label="Upload Data")

# upload_switch<- materialSwitch(
#   inputId = "upload",
#   label = "Upload data?",
#   value = FALSE,
#   status = "primary"
# )

upload_file <- fileInput("file","Choose a File",
                         multiple = FALSE,
                         accept = c(
                           "text/csv","text/comma-separated-values,text/plain",
                           "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
                           ".csv",".xlsx",".xls")
) %>% shinyInput_label_embed(shiny_iconlink() %>%
                               bs_embed_tooltip(title="Choose a data file to upload. Currently supported formats: comma-separated txt and csv files, and xls/xlsx files.",
                                                placement = "left"))

# Select Geographic id
upload_geo_id <- radioButtons(
  inputId = "geo",
  label = "Select Geographic Identifier", 
  choices = c("Latitude & Longitude" = "lat_lon",
              "Borough & Census Tract" = "boro_tract",
              "Combined Tract ID" = "boro_ct"),
  selected = NULL,
  inline = FALSE) %>% shinyInput_label_embed(shiny_iconlink() %>%
                                               bs_embed_tooltip(title="Lat/Lon follows standard definition (EPSG:4326 WGS 84).
                                                                Borogh code and tract ID uses Census 2010 definitions. Borough code ranges from  1 to 5, Tract id can be 1 to 6 digits long.
                                                                Combined Tract ID is a 7 digit number, fomred by concatenating the Borough code and Tract ID.",
                                                                placement = "left",container="body"))


# Select lat/lon columns
upload_lat_lon <- conditionalPanel(
  condition = "input.geo == 'lat_lon'",
  selectInput("lat","Select latitude column",
              choices = NULL, multiple = FALSE),
  selectInput("lon","Select longitude column",
              choices = NULL, multiple = FALSE))

# Select boto/ct columns
upload_boro_ct <- conditionalPanel(
  condition = "input.geo == 'boro_tract'",
  selectInput("boro","Select borough column",
              choices = NULL, multiple = FALSE),
  selectInput("ct","Select tract column",
              choices = NULL, multiple = FALSE))

# Select boro_ct columns
upload_tract_id <- conditionalPanel(
  condition = "input.geo == 'boro_ct'",
  selectInput("boro_ct","Select combined tract identifier",
              choices = NULL, multiple = FALSE))

# Select feature columns
upload_feature <- selectInput("user_columns","Select columns to aggregate",
                              choices = NULL, multiple = TRUE) %>% 
  shinyInput_label_embed(shiny_iconlink() %>%
                           bs_embed_tooltip(title ="When lat & lon identifier is used, count and density based features
are generated in addition to aggregations of the selected (if any) columns. In the other cases, please select at least one column from the dataset to use as a feature.",
                                            placement = "left",container="body"))

upload_done <- actionButton("upload_done","Done",class="btn-custom", "data-dismiss" = "modal")

modified_bsModal <- function(id, title, trigger, ..., size) {
  if(!missing(size)) {
    if(size == "large") {
      size = "modal-lg"
    } else if(size == "small") {
      size = "modal-sm"
    }
    size <- paste("modal-dialog", size)
  } else {
    size <- "modal-dialog"
  }
  bsTag <- shiny::tags$div(class = "modal sbs-modal fade", id = id, tabindex = "-1", "data-sbs-trigger" = trigger,
                           shiny::tags$div(class = size,
                                           shiny::tags$div(class = "modal-content",
                                                           shiny::tags$div(class = "modal-header",
                                                                           shiny::tags$button(type = "button", class = "close", "data-dismiss" = "modal", shiny::tags$span(shiny::HTML("&times;"))),
                                                                           shiny::tags$h4(class = "modal-title", title)
                                                           ),
                                                           shiny::tags$div(class = "modal-body", list(...))#,
                                                           # shiny::tags$div(class = "modal-footer",
                                                           #                 shiny::tags$button(type = "button", class = "btn btn-default", "data-dismiss" = "modal", "Close")
                                                           # )
                                           )
                           )
  )
  shinyBSDep <- htmltools::htmlDependency("shinyBS", packageVersion("shinyBS"), src = c("href" = "sbs"), script = "shinyBS.js", stylesheet = "shinyBS.css")
  htmltools::attachDependencies(bsTag, shinyBSDep)
  
}

modal_upload <- 
  modified_bsModal(
    id = "modal_upload",
    title="Upload Data",
    body= list(upload_file,
               # info_upload_file,
               upload_geo_id,
               # info_geo_id,
               upload_lat_lon,
               upload_boro_ct,
               upload_tract_id,
               upload_feature,
               tags$hr(),
               div(align="right",upload_done)),
    size="medium",
    trigger = "upload"
  )

### Inputs
input_housing <- function(){
  checkboxGroupInput(
    inputId = 'housing',label='HOUSING',
    choices=c('Age of buildings'='bldg_age','Median sale price'='sale_price'),
    selected = 'sale_price'
  )
}


input_housing_sales <- function(){
  conditionalPanel(condition="input.housing.includes('sale_price')",
                   radioButtons(
                     inputId = 'sales_features',label='',
                     choices=c('1y average'='med_price_1y|sd_price_1y',
                               '3y average'='med_price_3y|sd_price_3y',
                               '5y average'='med_price_5y|sd_price_5y'
                     ),selected = 'med_price_3y|sd_price_3y'))
}

feature_inputs <- get_features_ui(process_features_json())
eval(parse(text=feature_inputs$exprs))

# input_crime <- function(){
#   checkboxGroupInput(
#     inputId = 'crime_features', label="CRIME",
#     c("Violations"="violation_rate",
#       "Felonies"="felony_rate",
#       "Misdemeanors"="misdemeanor_rate"
#     )
#   )
# }

# input_noise <- function(){
#   checkboxGroupInput(
#     inputId = 'call_features',label="311 COMPLAINTS",
#     c("Ice cream truck"="icecream_rate",
#       "Barking dog"="animal_rate",
#       "Loud music/party"="party_rate"
#     )
#   )
# }

input_clusters <- function(){
  sliderInput("num_clusters",
              label="Number of neighborhoods",
              ticks = FALSE,
              min = 5,
              max = 200,
              value = 100)
}

input_enable_heatmap <- function(){
  materialSwitch(inputId = "enable_heatmap", label = "Cluster map", status = "info")
}


# info_plot_type <- shiny_iconlink() %>%
#   bs_attach_modal(id_modal = "modal_plots")

info_plot_type <- shiny_iconlink() %>%
  bs_embed_tooltip(title="The cluster map shows the city divided into the selected neighborhoods.
                          The colors are only to differentiate clusters from one another.
                          The heatmap shows the relative value for clusters averaged over the 
                          chosen characteristics.",
                   placement = "top")

baseline_choices <- list("None"="none",
                         "Community Districts (59)"="cds",
                         "Public use Microdata Areas (55)"="pumas",
                         "Neighborhood Tabulation Areas (195)"="ntas",
                         "Police Precincts (77)"="precincts",
                         "School Districts (33)"="school_dists")

baseline_choices <- get_baseline_choices()

input_baseline <- function(){
  selectInput('baseline',label='Compare against',
              choices= baseline_choices,
              selected = "none")
}

# input_user_features <- conditionalPanel(condition = "output.nrows",
#   pickerInput(inputId = 'user_features',label="USER Features",
#               choices=NULL,
#               options=list(`actions-box`=TRUE,title="User Features"),
#               multiple=TRUE))

input_user_features <- conditionalPanel(condition = "output.nrows",
                                        checkboxGroupInput(inputId = 'user_features',label="GENERATED FEATURES",
                                                           choices=NULL))


myDownloadButton <- function(outputId,label,class=NULL,icon=icon("download"), ...){
  # actionButton(id, label, icon, title = title, ...)
  aTag <- tags$a(outputId = outputId, class = paste("btn btn-default shiny-download-link", class),
                 href = "", target = "_blank", download = NA, icon("download"), label, ...)
}

myactionButton <- function (inputId, label, icon = NULL, width = NULL,class, ...){
  value <- restoreInput(id = inputId, default = NULL)
  tags$button(id = inputId, style = if (!is.null(width)) 
    paste0("width: ", validateCssUnit(width), ";"), type = "button", 
    class = class, `data-val` = value, 
    list(icon, label), ...)
}

myDownloadBttn <- function(outputId, label = "Download",icon = icon("download"),action_class){
  bttn <- myactionButton(inputId = paste0(outputId, "_bttn"), 
                         label = tags$a(id = outputId, class = "shiny-download-link", 
                                        href = "", target = "_blank", download = NA, label),
                         icon = icon,class=action_class)
  bttn
}

download_dropdown <- dropdownButton(
  myDownloadBttn(outputId = "downloadCSV",label="CSV",
                 icon=icon("file-download",lib="font-awesome"),action_class="btn-download"),
  br(),
  myDownloadBttn(outputId = "downloadGEOJson",label="GeoJSON",
                 icon=icon("file-archive",lib="font-awesome"),action_class="btn-download"),
  br(),
  myDownloadBttn(outputId ="downloadPNG",label="Image",
                 icon=icon("file-image",lib="font-awesome"),action_class="btn-download"),
  br(),
  bookmarkButton(label="Share",icon=icon("link",lib="font-awesome"),class="btn-download"),
  
  circle = TRUE, status = "danger",size="sm",
  icon = icon("download",lib="font-awesome"), width = "130px",
  
  tooltip = tooltipOptions(title = "Click to see options to download and share results.",
                           placement="top")
)

clus_rec_1 <- actionLink("clus_rec_1",50)
clus_rec_2 <- actionLink("clus_rec_2",100)
clus_rec_3 <- actionLink("clus_rec_3",150)
clus_rec_4 <- actionLink("clus_rec_4",200)

cluster_reco_links <- div(class="reco-form-group shiny-input-container",
                          tags$label("Recommended",class="subtitle-label"),
                          # div(class="subtitle-label","Recommended"),
                          span(class="links flex",
                               div(class="recolink",clus_rec_1),
                               div(class="recolink",clus_rec_2),
                               div(class="recolink",clus_rec_3),
                               div(class="recolink",clus_rec_4)
                          ))


map_control_panel <- function(){div(
  class="flex flex-between map-control", 
  div(class="xsflex", 
      input_clusters(),
      cluster_reco_links
  ),
  div(class="xsflex", 
      div(
        class="flex flex-end auto heatmap-group",
        input_enable_heatmap(),
        div(class="heat-map-label", "Heat map"),
        div(class="heatmap-tooltip",info_plot_type)
      ),
      input_baseline()
      # h5(textOutput("overlap"))
  )
)
}

help_link <- actionLink(inputId = "Help",label="Help")
feedback_link <- actionLink(inputId = 'Feedback',label="Feedback")
credits_link <- actionLink(inputId = 'Credits',label="About")
# example_link <- actionLink(inputId = 'Example',label="Examples")

intro_links <- 
  div(class="links flex",
      div(class="mainlink",credits_link),
      div(class="mainslink",help_link),
      # div(class="mainslink",example_link),
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




custom_withSpinner <- function (ui_element, type = getOption("spinner.type", default = 1), 
          color = getOption("spinner.color", default = "#0275D8"), 
          size = getOption("spinner.size", default = 1), color.background = getOption("spinner.color.background"), 
          custom.css = FALSE, proxy.height = if (grepl("height:\\s*\\d", 
                                                       ui_element)) NULL else "400px") 
{
  stopifnot(type %in% 1:8)
  if (grepl("rgb", color, fixed = TRUE)) {
    stop("Color should be given in hex format")
  }
  id <- paste0("spinner-", digest::digest(ui_element))
  add_style <- function(x) {
    shiny::tags$head(shiny::tags$style(shiny::HTML(x)))
  }
  css_size <- css_color <- shiny::tagList()
  if (!custom.css) {
    color.alpha <- sprintf("rgba(%s,0)", paste(grDevices::col2rgb(color), 
                                               collapse = ","))
    if (type == 1) {
      css_color <- add_style(glue::glue("#{id}, #{id}:before, #{id}:after {{background: {color}}} #{id} {{color: {color}}}"))
    }
    if (type %in% c(2, 3) && is.null(color.background)) {
      stop("For spinner types 2 & 3 you need to specify manually a background color. This should match the background color of the container.")
    }
    if (type == 2) {
      css_color <- add_style(glue::glue("#{id} {{color: {color}}} #{id}:before, #{id}:after {{background: {color.background};}}"))
    }
    if (type == 3) {
      css_color <- add_style(glue::glue("#{id} {{\n  background: -moz-linear-gradient(left, {color} 10%, {color.alpha} 42%);\n  background: -webkit-linear-gradient(left, {color} 10%, {color.alpha} 42%);\n  background: -o-linear-gradient(left, {color} 10%, {color.alpha} 42%);\n  background: -ms-linear-gradient(left, {color} 10%, {color.alpha} 42%);\n  background: linear-gradient(to right, {color} 10%, {color.alpha} 42%);\n}} \n#{id}:before {{\n   background: {color}\n}}  \n#{id}:after {{\n  background: {color.background};\n}}\n"))
    }
    if (type %in% c(4, 6, 7)) {
      css_color <- add_style(glue::glue("#{id} {{color: {color}}}"))
    }
    if (type == 5) {
      base_css <- paste(readLines(system.file("css-loaders/css/load5.css", 
                                              package = "shinycssloaders")), collapse = " ")
      base_css <- gsub(".load5 .loader", paste0("#", id), 
                       base_css)
      base_css <- gsub("load5", paste0("load5-", id), base_css, 
                       fixed = TRUE)
      base_css <- gsub("255, 255, 255", paste(grDevices::col2rgb(color), 
                                              collapse = ","), base_css, fixed = TRUE)
      base_css <- gsub("#ffffff", color, base_css, fixed = TRUE)
      css_color <- add_style(base_css)
    }
    if (type == 8) {
      css_color <- add_style(glue::glue("\n#{id} {{\n      border-top: 1.1em solid rgba({paste(grDevices::col2rgb(color),collapse=',')}, 0.2);\n      border-right: 1.1em solid rgba({paste(grDevices::col2rgb(color),collapse=',')}, 0.2);\n      border-bottom: 1.1em solid rgba({paste(grDevices::col2rgb(color),collapse=',')}, 0.2);\n      border-left: 1.1em solid {color};\n}}\n      "))
    }
    size <- round(c(11, 11, 10, 20, 25, 90, 10, 10)[type] * 
                    size * 0.75)
    css_size <- add_style(glue::glue("#{id} {{font-size: {size}px}}", 
                                     id, size))
  }
  proxy_element <- shiny::tagList()
  if (!is.null(proxy.height)) {
    proxy_element <- shiny::div(style = glue::glue("height:{ifelse(is.null(proxy.height),'100%',proxy.height)}"), 
                                class = "shiny-spinner-placeholder")
  }
  shiny::tagList(shiny::singleton(shiny::tags$head(shiny::tags$link(rel = "stylesheet", 
                                                                    href = "assets/spinner.css"))), shiny::singleton(shiny::tags$script(src = "custom_spinner.js")), 
                 shiny::singleton(shiny::tags$head(shiny::tags$link(rel = "stylesheet", 
                                                                    href = sprintf("css-loaders/css/fallback.css", type)))), 
                 shiny::singleton(shiny::tags$head(shiny::tags$link(rel = "stylesheet", 
                                                                    href = sprintf("css-loaders/css/load%s.css", type)))), 
                 css_color, css_size, shiny::div(class = "shiny-spinner-output-container", 
                                                 shiny::div(class = sprintf("load-container load%s", 
                                                                            type), shiny::div(id = id, class = "loader", 
                                                                                              "Loading...")), proxy_element, ui_element))
}


custom_withLoader <- function (ui_element, type = "html", loader = "dnaspin", 
                               proxy.height = if (grepl("height:\\s*\\d", ui_element)) NULL else "400px") 
{
  stopifnot(type %in% c("html", "image", "text"))
  proxy_element <- shiny::tagList()
  if (!is.null(proxy.height)) {
    proxy_element <- shiny::div(style = glue::glue("height:{ifelse(is.null(proxy.height),'100%',proxy.height)}"), 
                                class = "shiny-loader-placeholder")
  }
  if (type == "image") {
    shiny::tagList(shiny::singleton(shiny::tags$head(shiny::tags$link(rel = "stylesheet", 
                                                                      href = "custom-loader.css"))), shiny::singleton(shiny::tags$script(src = "custom_spinner.js")), 
                   shiny::singleton(shiny::tags$head(shiny::tags$link(rel = "stylesheet", 
                                                                      href = "css-loaders/css/imgcustom-fallback.css"))), 
                   shiny::div(class = "shiny-loader-output-container", 
                              shiny::div(class = "load-container", shiny::img(class = "loader-img", 
                                                                              src = loader)), proxy_element, ui_element))
  }
  else if (type == "html") {
    if (loader %in% c("dnaspin", "pacman", "loader1", "loader2", 
                      "loader3", "loader4", "loader5", "loader6", "loader7", 
                      "loader8", "loader9", "loader10")) {
      htmlfile <- system.file(package = "shinycustomloader", 
                              paste0("css-loaders/html/", loader, ".html"))
      shiny::tagList(shiny::singleton(shiny::tags$head(shiny::tags$link(rel = "stylesheet", 
                                                                        href = "custom-loader.css"))), shiny::singleton(shiny::tags$script(src = "custom_spinner.js")), 
                     shiny::singleton(shiny::tags$head(shiny::tags$link(rel = "stylesheet", 
                                                                        href = "css-loaders/css/imgcustom-fallback.css"))), 
                     shiny::singleton(shiny::tags$head(shiny::tags$link(rel = "stylesheet", 
                                                                        href = paste0("css-loaders/css/", loader, ".css")))), 
                     shiny::div(class = "shiny-loader-output-container", 
                                shiny::div(class = "load-container", shiny::includeHTML(htmlfile)), 
                                proxy_element, ui_element))
    }
    else {
      htmlfile <- paste0("", loader, ".html")
      shiny::tagList(shiny::singleton(shiny::tags$head(shiny::tags$link(rel = "stylesheet", 
                                                                        href = "custom-loader.css"))), shiny::singleton(shiny::tags$script(src = "custom_spinner.js")), 
                     shiny::singleton(shiny::tags$head(shiny::tags$link(rel = "stylesheet", 
                                                                        href = "css-loaders/css/imgcustom-fallback.css"))), 
                     shiny::singleton(shiny::tags$head(shiny::tags$link(rel = "stylesheet", 
                                                                        href = paste0(loader, ".css")))), shiny::div(class = "shiny-loader-output-container", 
                                                                                                                     shiny::div(class = "load-container", shiny::includeHTML(htmlfile)), 
                                                                                                                     proxy_element, ui_element))
    }
  }
  else if (type == "text") {
    if (!is.list(loader)) {
      textout <- "<marquee behavior=\"scroll\" direction=\"left\" scrollamount=\"1\" width=\"100%\">Loading...</marquee>\n    <marquee behavior=\"scroll\" direction=\"down\" scrollamount=\"20\" height=\"50px\" width=\"100%\">Loading</marquee>\n      <marquee behavior=\"scroll\" direction=\"right\" scrollamount=\"20\" width=\"100%\">Loading!</marquee>\n      <marquee behavior=\"scroll\" direction=\"left\" scrollamount=\"100\" width=\"100%\">Loading</marquee>"
    }
    else {
      textout <- ""
      for (amarquee in loader) {
        textout <- paste0(textout, "<marquee ")
        textout <- paste0(textout, paste0(paste0(names(amarquee)[names(amarquee) != 
                                                                   "content"], "=\"", unlist(amarquee)[names(amarquee) != 
                                                                                                         "content"], "\""), collapse = " "))
        textout <- paste0(textout, ">", amarquee[names(amarquee) == 
                                                   "content"], "</marquee>\n ")
      }
    }
    shiny::tagList(shiny::singleton(shiny::tags$head(shiny::tags$link(rel = "stylesheet", 
                                                                      href = "custom-loader.css"))), shiny::singleton(shiny::tags$script(src = "custom_spinner.js")), 
                   shiny::singleton(shiny::tags$head(shiny::tags$link(rel = "stylesheet", 
                                                                      href = "css-loaders/css/imgcustom-fallback.css"))), 
                   shiny::div(class = "shiny-loader-output-container", 
                              shiny::div(class = "load-container", style = "text-align:center; width:100%", 
                                         shiny::HTML(textout)), proxy_element, ui_element))
  }
}
