library(tidyverse)
library(sf)
library(leaflet)
library(forecast)


data <- read_csv2("data/Inventario-SingleTrees -- Exportado - 24-07-2024 13-41.csv")

df <-
  data |> 
  mutate(across(c(Data_Plantio, Data_Medicao), dmy))

points <- 
  data |> 
  count(Id_Amostra, Coordenada_X, Coordenada_Y) |> 
  st_as_sf(coords = c("Coordenada_X", "Coordenada_Y"))

leaflet(points) |> 
  addTiles() |> 
  addMarkers()

# dados com datas de medição anômalas
df |> 
  filter(year(Data_Medicao) != 2024) |> 
  view()

df |> 
  filter(year(Data_Medicao) == 2024) |> 
  filter(!Dap_Estimado) |> 
  filter(Estrato == unique(Estrato)[10]) |> 
  ggplot(aes(Data_Medicao, `DAP (cm)`, color = Id_Amostra)) +
  geom_point(aes(group = Id_Amostra))

