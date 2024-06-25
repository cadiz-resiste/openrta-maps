# openrta-maps
Código para generar mapas con los datos de openRTA sobre VUTs.

# Instalación
Se necesita R y una IDE apropiada (por ejemplo, RStudio).

# Uso
- Descarga el archivo "REGISTRO VFT JUNTA DE ANDALUCIA.xlsx"
- Abre el proyecto "cadizVUTMapas.Rproj".
- Ejecuta el script "maps_1.R"

Para ejecutar algunos mapas es necesario descargar el [siguiente shapefile](https://drive.google.com/file/d/1cN2Dy67oGCOybHoM038LieZ1pWjcPDtL/view?usp=sharing) y guardarlo en el directorio ds-codigos-postales-master/data.

# Output
El código genera 7 mapas.

- cadiz_vut.html (y .png) Muestra el número de VUTs por código postal.
- heatmap_leaflet.html muestra un mapa de calor con las viviendas turísticas en Cádiz
- VUTs_map_cdz.html muestra un mapa con todas las VUTs registradas. Este mapa es interactivo y al pinchar en cada vivienda se muestra información básica.
- heatmap_VUTs_overlayed.html muestra un mapa de calor con cada VUT superpuesta. Este mapa es interactivo y al pinchar en cada vivienda se muestra información básica.
- VUTs_heatmap.png similar al anterior mapa, pero en versión estática.
- touristic_apartments_dayByDay.gif Mapa animado donde se muestra la proliferación de VUTs a lo largo del tiempo, día a día, en el período 2015-2024
- touristic_apartments_yearly.gif Mapa animado similar al anterior, pero con animación anual en lugar de diaria.
