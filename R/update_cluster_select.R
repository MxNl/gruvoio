update_cluster_select <- function() {
  output$info <- renderText({
    selected_cluster() |>
      as_tibble() |>
      pull(well_id) |>
      unique()})

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
