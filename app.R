library(shiny)
library(bslib)


station_choices <- azmetr::station_info |>
  select(choice = meta_station_name, lat = latitude, lon = longitude)

# Main App UI
ui <- page_sidebar(
  title = "Location Selectize Module Demo",
  sidebar = sidebar(
    location_selectize_ui("loc_module", station_choices),
    hr(),
    p(
      "Click the location button to automatically select the nearest city based on your GPS location."
    )
  ),
  card(
    card_header("Selected Location"),
    verbatimTextOutput("selected_info")
  ),
  card(
    card_header("User Coordinates"),
    verbatimTextOutput("coords_info")
  )
)

# Main App Server
server <- function(input, output, session) {
  selected_location <- location_selectize_server("loc_module", station_choices)

  output$selected_info <- renderPrint({
    if (is.null(selected_location())) {
      "No location selected yet"
    } else {
      coords <- strsplit(selected_location(), ",")[[1]]
      list(
        value = selected_location(),
        latitude = as.numeric(coords[1]),
        longitude = as.numeric(coords[2])
      )
    }
  })

  output$coords_info <- renderPrint({
    list(
      user_latitude = input$`loc_module-get_location_lat`,
      user_longitude = input$`loc_module-get_location_lon`
    )
  })
}

shinyApp(ui, server)
