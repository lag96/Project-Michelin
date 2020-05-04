require(shiny)
require(leaflet)
require(dplyr)
require(ggmap)
require(rio)
require(shinyWidgets)
require(DT)

rsconnect::setAccountInfo(name='luciaaliciagomez', token='9D9E4A1E79CF535B98250000D89EAAF5', secret='pI+wTHExKBeVFtjU2tby9/cvt69PnfKBMlLfu+ms')


#############################################################################################################
ui <- fluidPage(theme="styles.css",
  tags$head(
    tags$meta(name="viewport", content="width=device-width, initial-scale=1.0"),
    tags$style(type= "text/css", "html, body {width: 100%; height:100%; margin:0; padding:0;}")
  ),
  
  #tags$h1("Michelin Starred Restaurants Globally" ),
  #tags$hr(),
  #tags$a(href = "https://guide.michelin.com/en/restaurants/1-star-michelin/2-stars-michelin/3-stars-michelin", "Resources"),

tags$h1("Your Michelin Restaurant Guide"),

tags$div(class="map",
        leafletOutput("m")
),
tags$div(class="slider",
        sliderInput('price', 'Price Range', 
                    min=min(coord$Mid_Point, na.rm=TRUE), max=max(coord$Mid_Point, na.rm=TRUE), value=c(min(coord$Mid_Point, na.rm=TRUE),max(coord$Mid_Point, na.rm=TRUE)), 
                    dragRange = TRUE)
),
tags$div(class="controls",
        pickerGroupUI(
          id = "my-filters",
          params = list(
            Stars = list(inputId = "Stars", label = "Stars", choices = c("One", "Two", "Three")),
            Country =list(inputId = "Country", label = "Country", choices = unique(sort(coord$Country))),
            Region = list(inputId = "Region", label = "Region", choices = unique(sort(coord$Region))),
            Cuisine = list(inputId = "Cuisine", label = "Cuisine", choices = unique(sort(coord$Cuisine)))
          )
        )
),
tags$div(class="tabla",
        DT::dataTableOutput(outputId = "table")
)
)

#############################################################################################################
server <- function(input, output)
{
  coord <- rio::import("www/finaltable.csv")
  michIcon<-makeIcon(iconUrl ="https://seeklogo.com/images/M/michelin-logo-D85CACF9DE-seeklogo.com.png",
                     iconWidth = 40, iconHeight = 40,
                     shadowWidth = 10, shadowHeight = 10)

  res_mod <- callModule(
    module = pickerGroupServer,
    id = "my-filters",
    data = coord,
    vars = c("Stars","Country","Region","Cuisine")
  )
  try(
  filtered_data <- reactive({
    res_mod() %>%
      filter(Mid_Point >= input$price[1] & Mid_Point <= input$price[2])
  }))

  
  output$m<-renderLeaflet({
    leaflet(filtered_data()) %>%
      addProviderTiles(providers$Esri.WorldStreetMap) %>%
      addMarkers(~Longitude,
                 ~Latitude,
                 icon=michIcon, 
                 label=~Name, 
                 clusterOptions=markerClusterOptions()
      )})
  
  output$table <- DT::renderDataTable({
                      DT::datatable(data = filtered_data() %>% select(Name), options = list(pageLength = 10),
                                    rownames = FALSE, class = 'display', escape = FALSE)
  })
  
  
}

shinyApp(ui, server)
