# Cargar librerías. sf, leaflet, plotly, raster, tmap y ggplot2 sirven para crear distintos tipos de mapas
library('sf')
library('dplyr')
library('ggplot2')
library('raster')
library('scales')
library('tmap')
library('leaflet')
library('leaflet.extras')
library('gganimate')
library('plotly')
library('gifski') #package to export GIFs

#Leer shapefiles ----
shp_wd <- paste0(getwd(), "/ds-codigos-postales-master/data/codigos_postales/")
shp <- read_sf(dsn = shp_wd, layer = "codigos_postales")

#leer datos de VUTs ----
vft_xlsx <- "REGISTRO VFT JUNTA DE ANDALUCIA.xlsx"
vft_path <- paste0(getwd(), "/drive_cr/", vft_xlsx)
vft <- readxl::read_xlsx(vft_path)

# Crear límites de coordenadas
x_limits <- c(-6.31, -6.25) # longitude of Cádiz
y_limits <- c(36.49, 36.55)  # latitude of Cádiz

# Data Cleaning ----
#fix 11103 postcode does not exist-> 11003
vft <- vft %>% mutate(CODIGO_POSTAL = ifelse(CODIGO_POSTAL==11103, 11003, CODIGO_POSTAL)) %>% 
  mutate(CODIGO_POSTAL = as.character(CODIGO_POSTAL))

#fecha en formato date
vft <- vft %>% 
  mutate(FECHA_INICIO_ACTIV = as.Date(FECHA_INICIO_ACTIV, format = "%Y/%m/%d"))

## Arreglar algunos nombres incorrectos en los datos ----
vft$is_calle <- stringr::str_extract(vft$DOMICILIO_ESTAB, "CALLE")
vft$is_calle <- ifelse(vft$is_calle == "CALLE", "SI", "NO")
vft$NUMERO <- sub("\\..*", "", vft$NUMERO)
vft$direccion_completa <- paste(vft$CALLE, vft$NUMERO, ", Cadiz,", vft$CODIGO_POSTAL, sep = " ")
vft$direccion_completa <- gsub("Apodaca", "Clara Campoamor", vft$direccion_completa)
vft$direccion_completa <- gsub("Plaza San Antonio", "Plaza de San Antonio, Mentidero", vft$direccion_completa)
vft$direccion_completa <- gsub("Plaza Bécquer", "Plaza de Becquer", vft$direccion_completa)
vft$direccion_completa <- gsub("Plaza Helios", "Glorieta Helios", vft$direccion_completa)
vft$direccion_completa <- ifelse(vft$is_calle == "SI",paste("Calle", vft$direccion_completa, sep = " ") , vft$direccion_completa)

## Limpiar columna de coordenadas ----

vft$X_2 <- as.numeric(vft$X)
vft$Y_2 <- as.numeric(vft$Y)

# Las coordenadas UTM zona 30 deben tener 6 dígitos de longitud y 7 de latitud. 
# Las coordenadas no traen decimales. La siguiente función convierte todas las coordenadas
# para tener 6 y 7 dígitos respectivamente
  sixDig <- function(num) {
    factor <- 10^(nchar(floor(num)) - 6)
    num / factor
  }
  
  sevenDig <- function(num) {
    factor <- 10^(nchar(floor(num)) - 7)
    num / factor
  }

# Aplicar la función
vft$X_2 <- sixDig(vft$X_2)
vft$Y_2 <- sevenDig(vft$Y_2)

# Extraer los puntos, convertirlos a WGS84 usando terra::project y extraer dos vectores con las coordenadas x e y
points <- cbind(vft$X_2, vft$Y_2)
v <- terra::vect(points, crs="+proj=utm +zone=30 +datum=WGS84  +units=m")
y <- terra::project(v, "+proj=longlat +datum=WGS84")
lon <- geom(y)[, c("x")]
lat <- geom(y)[, c("y")]

#incrustar las nuevas coordenadas en las columnas X_3 e Y_3
vft$X_3 <- lon
vft$Y_3 <- lat


## SHP: filter Cádiz postcodes -> 11001 - 11012 ----
cd_posts <- c(as.character(seq(11001, 11012, 1)))
shp_cd <- shp %>% 
  filter(COD_POSTAL %in% cd_posts)

#Contar VFTs por código postal
vft_pc <- vft %>% 
  group_by(CODIGO_POSTAL) %>% 
  summarise(VFTs = n())

## Merge data and shapefiles ----
shp_df <- vft_pc %>% 
  rename(COD_POSTAL = CODIGO_POSTAL) %>% 
  right_join(shp_cd, by ="COD_POSTAL")

# Convertir en objeto geométrico
shp_df <- st_as_sf(shp_df)

#  ----------------------- Data cleaning done!
# input para mapas: shp_df <- polígonos de códigos postales de Cádiz con datos de VUTs por código postal
#                   shp_cd <- polígonos de códigos postales de Cádiz sin datos 
#                   vft <- data frame con los datos y coordenadas de VUTs

# Plot maps ----

## Map 1: VFTs por Código Postal ----
pal <- RColorBrewer::brewer.pal(5,"YlOrBr")

tmap_mode("plot") #tmap en modo plot 
map1 <- tm_shape(shp_df) +
  tm_polygons(fill ="VFTs", 
              fill.legend = tm_legend("Número de VUTs" ,labels = c("1 - 100", "101 - 200", "201 - 300", "301 - 400", "401 - 500")),
              fill.scale = tm_scale_intervals(values = pal),
              ) +
  tm_layout(
    main.title = "VUTs en Cádiz por código postal", 
    main.title.position = "center"
    )

## guardar en png y HTML
tmap_save(map1, filename = "./output/cadiz_vut.png")
tmap_save(map1, filename = "./output/cadiz_vut.html")

## Map 2: VFTs heatmap ----
heatmap <- ggplot() +
  geom_sf(data = shp_cd, fill = 'beige', col = 'gray50')+
  coord_sf(xlim = x_limits, ylim = y_limits) + # Set the coordinate limits
  geom_point(data = vft %>% filter(X_3 < -6.2, Y_3<36.55), aes(x = X_3, y = Y_3), col = "burlywood1", alpha = 0.3) + #añadir puntos desde el otro dataframe
  geom_point(data = vft %>% filter(X_3 < -6.2, Y_3<36.55), aes(x = X_3, y = Y_3), col = 'black', alpha = 0.1, shape = 1) +
  stat_density2d(data = vft %>% filter(X_3 < -6.2, Y_3<36.55), aes(fill = ..level.., x = X_3, y = Y_3), alpha=0.8, geom="polygon") +
  scale_fill_gradientn(colours=rev(RColorBrewer::brewer.pal(7,"Spectral"))) +
  theme_void() + 
  guides(fill = FALSE) +
  labs(
    title = "Mapa de calor de VFTs en el municipio de Cádiz"
  )

heatmap_ly <- ggplotly(heatmap) #convertir ggplot a plotly para convertirlo en interactivo
htmlwidgets::saveWidget(heatmap_ly, "./output/VUTs_plotly.html") 
ggsave(heatmap, filename="./output/VUTs_heatmap.png") #guardar el ggplot estático

## Map 3: OpenStreetMap of VUTs ----
cadiz_lat <- 36.5298
cadiz_lon <- -6.2926 #lon y lat de cádiz para centrar el mapa de leaflet

vut_map <- leaflet(data = vft) %>%
  addTiles() %>%
  addCircleMarkers(lng = ~X_3, lat = ~Y_3,
                   radius = 5,
                   weight = 2,
                   fillOpacity = 0.7,
                   fillColor = ifelse(vft$TIPO_VIVIENDA == "COMPLETA", "#ffd39b", "#f57a7a"),
                   color = "black",
                   stroke = TRUE,
                   popup = ~paste(
                     "Código de Registro: ", COD_REGISTRO,
                     "<br>Nombre: ", NOMBRE,
                     "<br>Dirección: ", direccion_completa,
                     "<br>Tipo de Vivienda: ", TIPO_VIVIENDA,
                     "<br>Total Plazas: ", TOTAL_PLAZAS,
                     "<br>Fecha Inicio Actividad: ", format(FECHA_INICIO_ACTIV, "%Y-%m-%d") # Format date
                   )) %>% 
                     setView(lng = cadiz_lon, lat = cadiz_lat, zoom = 12) 
# Exportar mapa en HTML
htmlwidgets::saveWidget(vut_map, "./output/VUTs_map_cdz.html")

## Map 4: Heatmap of VUTs ----
heatmap_leaflet <- leaflet(data = vft) %>% 
  addTiles() %>% 
  addProviderTiles(providers$Esri.WorldGrayCanvas) %>% # Use Esri World Gray Canvas
  addHeatmap(
    lng = ~X_3, lat = ~Y_3,
    blur = 30, max = 0.05, radius = 15
  ) %>% 
  setView(lng = cadiz_lon, lat = cadiz_lat, zoom = 12) 
# Exportar mapa en HTML
htmlwidgets::saveWidget(heatmap_leaflet, "./output/heatmap_leaflet.html")


## Map 5: Heatmap y puntos superpuestos
heatmap_overlayed <-  vut_map %>% 
  addHeatmap(
  lng = ~X_3, lat = ~Y_3,
  blur = 30, max = 0.05, radius = 15
)
# Exportar mapa en HTML
htmlwidgets::saveWidget(heatmap_overlayed, "./output/heatmap_VUTs_overlayed.html")

## Map 6: Mapa animado. -----

### Map 6a. año a año ----

vft$year <- format(as.Date(vft$FECHA_INICIO_ACTIV, format="%Y/%m/%d"),"%Y")

vft_anim <- vft %>% filter(year>2013) 
map6a <- ggplot() +
  geom_sf(data = shp_cd, fill = 'beige', col = 'gray50')+
  coord_sf(xlim = x_limits, ylim = y_limits) + # Set the coordinate limits
  geom_point(data = vft_anim %>% filter(X_3 < -6.2, Y_3<36.55), aes(x = X_3, y = Y_3, group = as.integer(year)), col = "salmon", alpha = 0.7, size = 4) +
  geom_point(data = vft_anim %>% filter(X_3 < -6.2, Y_3<36.55), aes(x = X_3, y = Y_3, group = as.integer(year)), col = 'black', alpha = 0.5, shape = 1, size = 4) +
  transition_time(as.integer(year)) +
  shadow_mark() +
  labs(title = 'Proliferación de VUTs a lo largo del tiempo: {frame_time}') +
  theme_void() +
  theme(legend.position = "none")

# Exportar GIF
anim_save("./output/touristic_apartments_yearly.gif", map6a, fps = 10, renderer = gifski_renderer(), width=800, height=1000)


### Map 6b. Día a día ----
map6b <- ggplot() +
  geom_sf(data = shp_cd, fill = 'beige', col = 'gray50')+
  coord_sf(xlim = x_limits, ylim = y_limits) + # Set the coordinate limits
  geom_point(data = vft_anim, aes(x = X_3, y = Y_3, group = as.Date(FECHA_INICIO_ACTIV, format="%d/%m/%Y")), col = "salmon", alpha = 0.7, size = 4) +
  geom_point(data = vft_anim, aes(x = X_3, y = Y_3, group = as.Date(FECHA_INICIO_ACTIV, format="%d/%m/%Y")), col = 'black', alpha = 0.5, shape = 1, size = 4) +
  transition_time(as.Date(FECHA_INICIO_ACTIV, format="%d/%m/%Y")) +
  shadow_mark() +
  labs(title = 'Proliferación de VUTs a lo largo del tiempo: {frame_time}') +
  theme_void() +
  theme(legend.position = "none")

# Exportar GIF
anim_save("./output/touristic_apartments_dayByDay.gif", map6b, nframes = 2400, fps = 50, renderer = gifski_renderer(), width=800, height=1000)
