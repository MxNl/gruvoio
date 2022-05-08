library(shiny)
library(shinythemes)
library(shinyWidgets)
library(leaflet)
library(dplyr)

click_count <- 0

# gruvoApp <- function() {
  ui <- fluidPage(
    theme = shinytheme("darkly"),
    titlePanel(
      "Gruvo - Grundwasservorhersage für Deutschland"
    ),
    sidebarLayout(
      sidebarPanel(
        width = 2,
        radioButtons(
          "theme",
          "Thema für Anzeige in Karte",
          choices = mapthemes,
          selected = mapthemes[2]
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
        ),
        actionButton("reset", "Reset")
      ),
      mainPanel(
        width = 6,
        # height = "95vh",
        leafletOutput("map"),
        textOutput("click")
      )
    )
  )
  server <- function(input, output, session) {
    selected_cluster <- reactive({
      p <- input$map_click
      mouse_point <- sf::st_point(c(p$lng, p$lat)) |>
        sf::st_geometry() |>
        sf::st_sf(crs = sf::st_crs(well_meta))

      ref_well_id <- well_meta |>
        slice(sf::st_nearest_feature(mouse_point, well_meta)) |>
        pull(rm)

      well_meta |>
        filter(rm == ref_well_id)
    })

    # pal <- colorNumeric("viridis", well_meta$pred_gwlnn)
    selected_theme <- reactive({
      # input$theme
      if (input$theme == mapthemes[2]) {
        pal <- colorNumeric("viridis", well_meta$pred_gwlnn)
        pal <- pal(well_meta$pred_gwlnn)
      } else if (input$theme == mapthemes[3]) {
        pal <- colorFactor("RdBu", well_meta$pred_trend)
        pal <- pal(well_meta$pred_trend)
        # to be implemented for other themes
      } else {
        pal <- colorNumeric("viridis", well_meta$pred_gwlnn)
        pal <- pal(well_meta$pred_gwlnn)
      }
      pal
    })

    reset_click <- reactive({
      input$reset
    })

    init_map <- function() {
      leaflet::renderLeaflet({
      # factpal <- colorFactor(topo.colors(5), well_meta$rm)

      leaflet() |>
        # addTiles() |>
        # addProviderTiles("OpenStreetMap.DE") |>
        addProviderTiles("Stamen.TonerBackground") |>
        # addMarkers(data = well_meta)
        addCircles(
          data = well_meta,
          # color = ~ pred_gwlnn_pal(well_meta$pred_gwlnn)
          color = ~ selected_theme()
          )
    })
    }

    output$map <- init_map()

    observeEvent(input$reset, {
      updateRadioButtons(inputId = "theme", selected = mapthemes[2])
      updateSliderTextInput(inputId = "horizon", session = session,
                            choices = prediction_horizons_labels, selected = prediction_horizons_labels[1])
      updateCheckboxInput(inputId = "performance", value = FALSE)
      updateCheckboxGroupInput(inputId = "themes_add", choices = mapthemes_add)
    })

    observe({

      p <- input$map_click
      if (is.null(p)) {
        return()
      }

      selected_cluster <- selected_cluster()
      unselected_clusters <- well_meta |>
        filter(!(well_id %in% pull(selected_cluster, well_id)))

      type <<- click_count%%2
      if (type ==0){
        leafletProxy("map") %>%
          clearShapes() %>%
          addCircles(
            data = selected_cluster,
            color = ~ selected_theme()
          ) |>
          addCircles(
            data = unselected_clusters,
            color = "grey",radius = 10
          )
      }
      if (type == 1){
        leafletProxy("map") %>%
          clearShapes() |>
          addCircles(
            data = well_meta,
            color = ~ selected_theme()
          )
      }

        click_count <<- click_count+1
    })
  }
  shinyApp(ui, server)
# }
