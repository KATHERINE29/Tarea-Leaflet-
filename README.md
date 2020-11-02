# Tarea-Leaflet-

## Carga packs
library(leaflet)
library(sf)
library(tidyr)
library(dplyr)

## Capa de cantones

sf_cantones <-
  st_read(
    "https://raw.githubusercontent.com/taller-r-jornadas-sigtd-2020/datos/master/cantones.geojson", 
    quiet = T
  )

## Data frames iniciales
df_activos_cantones_ancho <- 
  read.csv("https://raw.githubusercontent.com/pf0953-programaciongeoespacialr-2020/datos/master/covid19/ms/covid19-activos-cantones-20201027.csv")

df_fallecidos_cantones_ancho <- 
  read.csv("https://raw.githubusercontent.com/pf0953-programaciongeoespacialr-2020/datos/master/covid19/ms/covid19-fallecidos-cantones-20201027.csv")

df_positivos_cantones_ancho <- 
  read.csv("https://raw.githubusercontent.com/pf0953-programaciongeoespacialr-2020/datos/master/covid19/ms/covid19-positivos-cantones-20201027.csv")

df_recuperados_cantones_ancho <- 
  read.csv("https://raw.githubusercontent.com/pf0953-programaciongeoespacialr-2020/datos/master/covid19/ms/covid19-recuperados-cantones-20201027.csv")

## Dataframes acomodados por fecha
df_activos_cantones <-
  df_activos_cantones_ancho %>%
  pivot_longer(
    cols = c(-cod_provin, -provincia, -cod_canton, -canton), 
    names_to = "fecha", 
    values_to = "activos"
  )

df_fallecidos_cantones <-
  df_fallecidos_cantones_ancho %>%
  pivot_longer(
    cols = c(-cod_provin, -provincia, -cod_canton, -canton), 
    names_to = "fecha", 
    values_to = "fallecidos"
  )

df_positivos_cantones <-
  df_positivos_cantones_ancho %>%
  pivot_longer(
    cols = c(-cod_provin, -provincia, -cod_canton, -canton), 
    names_to = "fecha", 
    values_to = "positivos"
  )

df_recuperados_cantones <-
  df_recuperados_cantones_ancho %>%
  pivot_longer(
    cols = c(-cod_provin, -provincia, -cod_canton, -canton), 
    names_to = "fecha", 
    values_to = "recuperados"
  )

## Cambio de tipo de la columna "fecha"
df_activos_cantones$fecha <- as.Date(df_activos_cantones$fecha, "X%d.%m.%Y")
df_fallecidos_cantones$fecha <- as.Date(df_fallecidos_cantones$fecha, "X%d.%m.%Y")
df_positivos_cantones$fecha <- as.Date(df_positivos_cantones$fecha, "X%d.%m.%Y")
df_recuperados_cantones$fecha <- as.Date(df_recuperados_cantones$fecha, "X%d.%m.%Y")

## Data frame de casos activos por cantón en la última fecha
  df_activos_cantones_ultima_fecha <- 
    df_activos_cantones %>%
    filter(fecha == max(fecha, na.rm = TRUE)) %>%
    select(cod_canton, activos)

  df_fallecidos_cantones_ultima_fecha <- 
    df_fallecidos_cantones %>%
    filter(fecha == max(fecha, na.rm = TRUE)) %>%
    select(cod_canton, fallecidos)  
  
  df_positivos_cantones_ultima_fecha <- 
    df_positivos_cantones %>%
    filter(fecha == max(fecha, na.rm = TRUE)) %>%
    select(cod_canton, positivos)

  df_recuperados_cantones_ultima_fecha <- 
    df_recuperados_cantones %>%
    filter(fecha == max(fecha, na.rm = TRUE)) %>%
    select(cod_canton, recuperados)
  
  ## Objeto sf de casos activos en cantones en la última fecha
  sf_activos_cantones_ultima_fecha <-
    left_join(sf_cantones, df_activos_cantones_ultima_fecha, by = c('cod_canton')) %>%
    arrange(desc(activos))
  
  sf_fallecidos_cantones_ultima_fecha <-
    left_join(sf_cantones, df_fallecidos_cantones_ultima_fecha, by = c('cod_canton')) %>%
    arrange(desc(fallecidos))
  
  sf_positivos_cantones_ultima_fecha <-
    left_join(sf_cantones, df_positivos_cantones_ultima_fecha, by = c('cod_canton')) %>%
    arrange(desc(positivos))
  
  sf_recuperados_cantones_ultima_fecha <-
    left_join(sf_cantones, df_recuperados_cantones_ultima_fecha, by = c('cod_canton')) %>%
    arrange(desc(recuperados))
  
  sf_COVID19_cantones_total <-
    left_join(sf_cantones, df_positivos_cantones_ultima_fecha, by = c('cod_canton')) %>%
    left_join (df_fallecidos_cantones_ultima_fecha) %>%
    left_join (df_recuperados_cantones_ultima_fecha) %>%
    left_join (df_activos_cantones_ultima_fecha)
  
    sf_COVID19_cantones_ultima_fecha <-st_join(sf_activos_cantones_ultima_fecha, sf_fallecidos_cantones_ultima_fecha)
  
  ## Escala/rangos
  bins <- c(0, 100, 500, 1000, 2000, Inf)
  bins2 <- c(0, 5, 10, 50, 150, Inf)
  
  ## Colores
  paleta_rojo <- colorBin("Reds", domain = sf_COVID19_cantones_total$activos, bins = bins)
  paleta_azul <- colorBin("Blues", domain = sf_COVID19_cantones_total$fallecidos, bins = bins2)
  paleta_morado <- colorBin("RdPu", domain = sf_COVID19_cantones_total$positivos, bins = bins)  
  paleta_verde <- colorBin("Greens", domain = sf_COVID19_cantones_total$recuperados, bins = bins)
  
  ## Mapas 
  leaflet (sf_COVID19_cantones_total) %>% 
    fitBounds(lng1 = -86, lng2 = -82, lat1 = 8, lat2 = 11) %>%
    addProviderTiles(providers$OpenStreetMap.Mapnik, group = "OpenStreetMap") %>%
    addProviderTiles(providers$Esri.WorldTopoMap, group = "EsriTopoMap") %>%
    addProviderTiles(providers$NASAGIBS.ModisTerraBands367CR, group = "NASAModis") %>%
    addPolygons(fillColor = ~paleta_azul(fallecidos), stroke=T, fillOpacity = 1,
                color="black", weight=0.2, opacity= 0.5,
                group = "Fallecidos",
                popup = paste(
                  "Provincia: ", sf_COVID19_cantones_total$provincia, "<br>",
                  "Cantón: ", sf_COVID19_cantones_total$canton, "<br>",
                  "Fallecidos: ", sf_COVID19_cantones_total$fallecidos
                )) %>%
    addPolygons(fillColor = ~paleta_rojo(activos), stroke=T, fillOpacity = 1,
                color="black", weight=0.2, opacity= 0.5,
                group = "Activos",
                popup = paste(
                  "Provincia: ", sf_COVID19_cantones_total$provincia, "<br>",
                  "Cantón: ", sf_COVID19_cantones_total$canton, "<br>",
                  "Activos: ", sf_COVID19_cantones_total$activos
                ) )%>%
    addPolygons(fillColor = ~paleta_verde(recuperados), stroke=T, fillOpacity = 1,
                color="black", weight=0.2, opacity= 0.5,
                group = "Recuperados",
                popup = paste(
                  "Provincia: ", sf_COVID19_cantones_total$provincia, "<br>",
                  "Cantón: ", sf_COVID19_cantones_total$canton, "<br>",
                  "Recuperados: ", sf_COVID19_cantones_total$recuperados
                ) )%>% 
    addPolygons(fillColor = ~paleta_morado(positivos), stroke=T, fillOpacity = 1,
                color="black", weight=0.2, opacity= 0.5,
                group = "Positivos",
                popup = paste(
                  "Provincia: ", sf_COVID19_cantones_total$provincia, "<br>",
                  "Cantón: ", sf_COVID19_cantones_total$canton, "<br>",
                  "Positivos: ", sf_COVID19_cantones_total$positivos
                ) )%>%
    addLegend("bottomleft", pal = paleta_azul, values = ~fallecidos,
              title = "Casos fallecidos",
              opacity = 1)%>%
    addLegend("bottomright", pal = paleta_rojo, values = ~activos,
              title = "Casos activos",
              opacity = 1
    ) %>% 
    addLegend("topright", pal = paleta_verde, values = ~recuperados,
              title = "Casos recuperados",
              opacity = 1
    ) %>%  addLegend("topleft", pal = paleta_morado, values = ~positivos,
                     title = "Casos positivos",
                     opacity = 1
    ) %>%  
    addLayersControl(
      baseGroups = c("OpenStreetMap", "EsriTopoMap", "NASAModis"),
      overlayGroups = c("Fallecidos", "Activos", "Positivos", "Recuperados"),
      options = layersControlOptions(collapsed = TRUE)    
    ) %>%  
    addMiniMap(
      toggleDisplay = TRUE,
      position = "bottomleft",
      tiles = providers$OpenStreetMap.Mapnik)
