library(shiny)
library(shinyWidgets)
library(leaflet)
library(dplyr)

gruvoApp <- function() {
  ui <- fluidPage(
    titlePanel(
      "Gruvo - Grundwasservorhersage für Deutschland"
    ),
    sidebarLayout(
      sidebarPanel(
        radioButtons(
          "theme",
          "Thema für Anzeige in Karte",
          choices = mapthemes
        ),
        sliderTextInput(
          "horizon",
          "Vorhersagezeitraum",
          choices = prediction_horizons_labels,
          grid = TRUE
        ),
        checkboxInput(
          "performance",
          "Modellgüte",
          value = FALSE
        ),
        checkboxGroupInput(
          "themes_add",
          "Zusätzliche Themen als Hintergrundkarten",
          choices = mapthemes_add
        )
      ),
      mainPanel(
        leafletOutput("map"),
        textOutput("click")
      )
    )
  )
  server <- function(input, output, session) {
    selected_cluster <- reactive({
      p <- input$map_marker_click
      well_meta |> filter(rm == p$rm)
    })

    output$map <- leaflet::renderLeaflet({
      factpal <- colorFactor(topo.colors(5), well_meta$rm)

      leaflet() |>
        # addTiles() |>
        # addProviderTiles("OpenStreetMap.DE") |>
        addProviderTiles("Stamen.TonerBackground") |>
        addMarkers(data = well_meta)
        # addCircles(data = well_meta, color = ~ factpal(well_meta$rm))
    })

    output$click <- renderText({
      p <- input$map_marker_click
      p$rm
    })

    # observe({
    #   p <- input$map_marker_click
    #   if (is.null(p)) {
    #     return()
    #   }
    #
    #   leafletProxy("map") %>%
    #     clearMarkers() %>%
    #     addMarkers(
    #       data = selected_cluster()
    #     )
    # })
  }
  shinyApp(ui, server)
}
