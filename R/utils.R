
plot_estratos <- function(df, estratos) {
  plot <-
    df |>
    filter(year(Data_Medicao) == 2024) |>
    filter(Estrato == estratos) |>
    mutate(Amostra = as.character(Amostra)) |> 
    ggplot(aes(Data_Medicao, `DAP (cm)`)) +
    geom_line(aes(group = Amostra, color = Amostra), show.legend = F) +
    geom_point(
      aes(
        group = Amostra,
        color = Amostra,
        text = sprintf(
          "Amostra: %s<br>MatGen: %d <br>Data: %s<br>MAC: %s<br>DAP (cm): %f<br>Altura (m): %f",
          Amostra,
          Material_Genetico,
          Data_Medicao,
          MAC,
          `DAP (cm)`,
          `Altura (m)`
        )
      ),
      fill = "black",
      stroke = 0.25,
      shape = 21,
      size = 1
    ) +
    theme_minimal() +
    labs(title = estratos, x = "Data da Medição")
  
  return(plot)
}

tab_pretty <- function(data) {
  data |>
    tab_style(
      style = cell_text(align = "center"),
      locations = cells_stubhead()) |>
    tab_style(
      style = cell_text(align = "center"),
      locations = cells_column_labels()
    ) |>
    tab_options(
      column_labels.background.color = "#3D7A43",
      column_labels.font.weight = "bold",
      # row_group.background.color = "#F6F6F6"
    ) |> 
    fmt_number(decimals = 2, dec_mark = ",", sep_mark = ".") |> 
    sub_missing(missing_text = "-")
}
