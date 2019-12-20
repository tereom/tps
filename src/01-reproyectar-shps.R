# Proyección para leaflet o geoportal
# Coordinate Reference System:
#   EPSG: 4326 
#   proj4string: "+proj=longlat +datum=WGS84 +no_defs"

tps <- read_sf(here("datos", "TPS_entrega", "Sitios_Intervencion_TPS.shp")) %>% 
    select(-area)
# eliminar coordenada z (polígono en 3D)
tps_r2 <- st_zm(tps)
st_coordinates(tps_r2) %>% head()

# escribir longlat para geoportal
# tps_longlat <- st_transform(tps_r2, crs = 4326)
# st_write(obj = tps_longlat, "datos/salidas/tps_longlat.shp")

# calcular buffer
tps_buffer <- st_buffer(select(tps_r2, -SUP_HA), dist = 12000) %>%
    mutate(area = st_area(.)) %>%
    st_difference(st_geometry(tps_r2)) %>%
    mutate(area_anillo = st_area(.)) %>%
    filter(area != area_anillo) %>%
    select(-area) %>%
    mutate(SUP_HA = area_anillo / 10000) %>% 
    select(-area_anillo)

plot(st_geometry(tps_buffer))

# tps_buffer_longlat <- st_transform(tps_buffer, crs = 4326)
# st_write(obj = tps_buffer_longlat, "datos/salidas/tps_buffer_longlat.shp")
