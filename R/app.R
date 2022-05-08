library(leaflet)
library(sf)
library(ggdark)
library(ggplot2)
library(shiny)
library(shinythemes)
library(shinyWidgets)

data("mapthemes")
data("prediction_horizons_labels")
data("mapthemes_add")
data("mapthemes")
data("well_meta")
data("well_data")

click_count <- 0
theme_before <- mapthemes[2]

# gruvoApp <- function() {
  ui <- fluidPage(
    theme = shinytheme("darkly"),
    # titlePanel(
    #   h1("Gruvo - Grundwasservorhersage für Deutschland", align = "center")
    # ),
    fluidRow(
      column(
        width = 12,
        offset = 4,
        titlePanel("Gruvo - Grundwasservorhersage für Deutschland")
        )
      ),
    br(),
    fluidRow(
      column(
        2,
        # offset = 1,
        # "sidebar",
        br(),
        br(),
        radioButtons(
          "theme",
          "Thema für Anzeige in Karte",
          choices = mapthemes,
          selected = mapthemes[2]
        ),
        br(),
        sliderTextInput(
          "horizon",
          "Vorhersagezeitraum",
          choices = prediction_horizons_labels,
          grid = TRUE
        ),
        br(),
        checkboxInput(
          "performance",
          "Modellgüte",
          value = FALSE
        ),
        br(),
        checkboxGroupInput(
          "themes_add",
          "Zusätzliche Themen als Hintergrundkarten",
          choices = mapthemes_add
        ),
        br(),
        actionButton("reset", "Reset")
      ),
      column(
        5,
        # "main",
        leafletOutput("map", width = "100%", height = "80vh"),
      ),
      column(
        5,
        # "main",
        textOutput("info"),
        br(),
        plotOutput("predplot", height = "20vh")
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

    factpal <- colorFactor(topo.colors(5), well_meta$rm)

    init_map <- function() {
      leaflet::renderLeaflet({

      leaflet() |>
        # addTiles() |>
        # addProviderTiles("OpenStreetMap.DE") |>
        addProviderTiles("Stamen.TonerBackground") |>
        # addMarkers(data = well_meta)
        addCircles(
          data = well_meta,
          # color = ~ pred_gwlnn_pal(well_meta$pred_gwlnn)
          opacity = 1,
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

    cluster_select_message <- renderText({"Please select a cluster or observation well!"})

    output$info <- cluster_select_message

    observe({
      p <- input$map_click
      if (is.null(p)) {
        return()
      }
      selected_cluster <- selected_cluster()
      unselected_clusters <- well_meta |>
        filter(!(well_id %in% pull(selected_cluster, well_id)))
      selected_cluster_ref_well_id <- selected_cluster |>
        as_tibble() |>
        pull(rm) |>
        unique()

      type <<- click_count%%2
      if (type == 0){
        output$info <- renderText({
          selected_cluster_ref_well_id
          })

        output$predplot <- renderPlot({
          well_data |>
            filter(well_id == selected_cluster_ref_well_id) |>
            ggplot(aes(date, gwl)) +
            geom_line(colour = "deepskyblue") +
            dark_theme_light()
        })

        leafletProxy("map") %>%
          clearShapes() %>%
          addCircles(
            data = selected_cluster,
            opacity = 1,
            color = ~ selected_theme()
          ) |>
          addCircles(
            data = unselected_clusters,
            opacity = 0.25,
            color = ~ selected_theme(),radius = 10
          )
      }
      if (type == 1){
        output$info <- cluster_select_message

        output$predplot <- NULL

        leafletProxy("map") %>%
          clearShapes() |>
          addCircles(
            data = well_meta,
            opacity = 1,
            color = ~ selected_theme()
          )
      }
        click_count <<- click_count + 1
        # if (theme_before != input$theme) {
        # click_count <<- click_count - 1
        # theme_before <- input$theme
        # }
    })
  }
  shinyApp(ui, server)
# }
