library(shiny)
library(bslib)
library(dplyr)
library(cookies)

station_choices <- azmetr::station_info |>
  select(
    choice = meta_station_name,
    value = meta_station_id,
    lat = latitude,
    lon = longitude
  ) |>
  filter(choice != "Test") |>
  arrange(choice)

# Main App UI
ui <- page_sidebar(
  title = "Location Selectize Module Demo",
  sidebar = sidebar(
    location_selectize_ui(
      "loc_module",
      "Select a station:",
      station_choices,
      selected = "Roll"
    ),
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
      selected_location()
    }
  })

  output$coords_info <- renderPrint({
    list(
      user_latitude = input$`loc_module-user_location_lat`,
      user_longitude = input$`loc_module-user_location_lon`
    )
  })
}

shinyApp(ui, server)
