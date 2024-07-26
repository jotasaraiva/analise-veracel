
plot_estratos <- function(df, estratos) {
  plot <-
    df |>
    filter(year(Data_Medicao) == 2024) |>
    filter(Estrato == estratos) |>
    ggplot(aes(Data_Medicao, `DAP (cm)`)) +
    geom_line(aes(group = Id_Amostra, color = Id_Amostra), show.legend = F) +
    geom_point(
      aes(
        group = Id_Amostra,
        text = sprintf(
          "Amostra: %s<br>MatGen: %d <br>Data: %s<br>MAC: %s<br>DAP (cm): %f<br>Altura (m): %f",
          Id_Amostra,
          Material_Genetico,
          Data_Medicao,
          MAC,
          `DAP (cm)`,
          `Altura (m)`
        )
      ),
      color = "grey20",
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
