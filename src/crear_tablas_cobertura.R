library(raster)
library(dplyr)
library(stringr)
library(purrr)
library(lubridate)
library(sf)
library(readr)

hansen_loss <- raster::raster("~/Documents/GitHub/anp_reporte_bk2/datos_procesados/rasters/hansen_forest_loss_v1_6_lcc_cropped.tif")
madmex_class <- raster::raster("~/Documents/GitHub/anp_reporte_bk2/datos_procesados/rasters/mapabase_8clases_re_2015_30m_lcc_cropped.tif")
leyenda_madmex <- read_csv("datos/leyenda_madmex_8.csv")



madmex_loss <- raster::stack(list(hansen_loss, madmex_class)) 
crs_loss <- as.character(crs(madmex_loss))
compute_madmex_loss <- function(tps_sf){
    print(tps_sf$REG_TPS)
    tps_madmex_loss <- madmex_loss  %>% 
        raster::crop(tps_sf) %>% 
        raster::mask(mask = tps_sf)
    tps_madmex_loss_df <- data.frame(raster::rasterToPoints(tps_madmex_loss), 
        stringsAsFactors = FALSE) %>% 
        dplyr::rename(year_loss = hansen_forest_loss_v1_6_lcc_cropped, 
            clase_madmex = mapabase_8clases_re_2015_30m_lcc_cropped) %>% 
        dplyr::count(year_loss, clase_madmex) %>% 
        ungroup() %>% 
        dplyr::mutate(
            TPS = tps_sf$TPS,
            REG_TPS = tps_sf$REG_TPS,
            SITINT_TPS = tps_sf$SITINT_TPS,
            year_loss = year_loss + 2000
        )
}

# TPS
tps_path <- "datos/salidas/tps_longlat"
tps  <- sf::st_read(tps_path, stringsAsFactors = FALSE) %>% 
    sf::st_transform(crs_loss)
tps_madmex_loss <- tps %>% 
    group_split(SITINT_TPS) %>% 
    map(~compute_madmex_loss(.))
write_rds(tps_madmex_loss, "datos/salidas/tps_madmex_loss.rds")
tps_madmex_loss <- read_rds("datos/salidas/tps_madmex_loss.rds")
tps_madmex_loss <- bind_rows(!!!tps_madmex_loss)

write_csv(tps_madmex_loss, path = "datos/salidas/tps_madmex_loss.csv")

# Buffers
tps_buffer_path <- "datos/salidas/tps_buffer_longlat/"
tps_buffer  <- sf::st_read(tps_buffer_path, stringsAsFactors = FALSE) %>% 
    sf::st_transform(crs_loss)
tps_buffer_madmex_loss <- tps_buffer %>% 
    group_split(SITINT_TPS) %>% 
    map(~compute_madmex_loss(.))
write_rds(tps_buffer_madmex_loss, "datos/salidas/tps_buffer_madmex_loss.rds")
tps_buffer_madmex_loss <- read_rds("datos/salidas/tps_buffer_madmex_loss.rds")
tps_buffer_madmex_loss <- bind_rows(!!!tps_buffer_madmex_loss)
write_csv(tps_buffer_madmex_loss, 
    path = "datos/salidas/tps_buffer_madmex_loss.csv")

# tablas para entrega con ha de MADMEX-Hansen
# TPS
# sobra
# tps_madmex_loss %>% 
#     mutate(area_ha = n * 9 / 100) %>% 
#     filter(year_loss != 2000) %>% 
#     left_join(leyenda_madmex, by = "clase_madmex") %>% 
#     dplyr::select(year_loss, cod_madmex = clase_madmex, clase_madmex = etiqueta, TPS, 
#         REG_TPS, SITINT_TPS, area_ha) %>% 
#     write_csv(path = "entregas/tps_perdida_madmex_2001_2018.csv")
# # buffer
# tps_buffer_madmex_loss %>% 
#     mutate(area_ha = n * 9 / 100) %>% 
#     filter(year_loss != 2000) %>% 
#     left_join(leyenda_madmex, by = "clase_madmex") %>% 
#     dplyr::select(year_loss, cod_madmex = clase_madmex, clase_madmex = etiqueta, TPS, 
#         REG_TPS, SITINT_TPS, area_ha) %>% 
#     write_csv(path = "entregas/tps_buffer_perdida_madmex_2001_2018.csv")

tps_madmex_loss <- mutate(tps_madmex_loss, tipo = "TPS") %>% 
    bind_rows(mutate(tps_buffer_madmex_loss, tipo = "periferia")) %>% 
    mutate(area_ha = n * 9 / 100)

# cálculo de pérdida como porcentaje del área total y en hectáreas
tps_loss <- tps_madmex_loss  %>% 
    group_by(TPS, REG_TPS, SITINT_TPS, tipo, year_loss) %>% 
    summarise(n = sum(n), area_ha = sum(area_ha, na.rm = TRUE)) %>% 
    mutate(
        area_total = sum(area_ha, na.rm = TRUE),
        perdida_prop = round(area_ha / area_total * 100, 3)
    ) %>% 
    ungroup() %>% 
    filter(!is.na(area_total)) %>% 
    complete(year_loss = 2000:2018, nesting(TPS, REG_TPS, SITINT_TPS, tipo), 
        fill = list(perdida_ha = 0, perdida_prop = 0))

write_csv(filter(tps_loss, year_loss > 2000),  
    "entregas/perdida_tps_anual.csv")

# cálculo de pérdida por clase de vegetación (bosque y selva solamente)
tps_loss_class <- tps_madmex_loss %>% 
    mutate(area_ha = n * 9 / 100) %>% 
    group_by(TPS, REG_TPS, SITINT_TPS, tipo, clase_madmex) %>% 
    mutate(
        area_total = sum(area_ha),
        perdida_prop = round(area_ha / area_total * 100, 3)
    ) %>%  
    ungroup() %>% 
    filter(!is.na(area_total)) %>% 
    complete(year_loss = 2000:2018, nesting(clase_madmex), 
        nesting(TPS, REG_TPS, SITINT_TPS, tipo), 
        fill = list(perdida_ha = 0, perdida_prop = 0)) %>% 
    filter(year_loss > 2000, clase_madmex %in% c(1:2)) %>% 
    mutate(clase_madmex = case_when(
        clase_madmex == 1 ~ "bosque", 
        clase_madmex == 2 ~ "selva"
    ))

write_csv(filter(tps_loss_class, year_loss > 2000), 
    "entregas/perdida_clase_tps_anual.csv")


##################################################
##################################################
# Sólo pérdida, usando solo Hansen, mismos resultados pero más rápido si no
# interesa MAD-Mex
library(raster)
library(dplyr)
library(stringr)
library(purrr)
library(lubridate)
library(sf)
library(readr)

hansen_loss <- raster::raster("~/Documents/GitHub/anp_reporte_bk2/datos_procesados/rasters/hansen_forest_loss_v1_6_lcc_cropped.tif")

crs_loss <- as.character(crs(hansen_loss))
compute_loss <- function(tps_sf){
    print(tps_sf$REG_TPS)
    tps_loss <- hansen_loss  %>% 
        raster::crop(tps_sf) %>% 
        raster::mask(mask = tps_sf)
    anp_madmex_loss_df <- data.frame(raster::rasterToPoints(tps_loss), 
        stringsAsFactors = FALSE) %>% 
        dplyr::rename(year_loss = hansen_forest_loss_v1_6_lcc_cropped) %>% 
        dplyr::count(year_loss) %>% 
        ungroup() %>% 
        dplyr::mutate(
            TPS = tps_sf$TPS,
            REG_TPS = tps_sf$REG_TPS,
            SITINT_TPS = tps_sf$SITINT_TPS,
            year_loss = year_loss + 2000
        )
}
# TPS
tps_path <- "datos/salidas/tps_longlat"
tps  <- sf::st_read(tps_path, stringsAsFactors = FALSE) %>% 
    sf::st_transform(crs_loss)
tps_loss <- tps %>% 
    group_split(SITINT_TPS) %>% 
    map(~compute_loss(.))
tps_loss <- bind_rows(!!!tps_loss)

write_csv(tps_loss, path = "datos/salidas/tps_loss.csv")


# cálculo de pérdida como porcentaje del área total y en hectáreas
tps_loss <- tps_loss  %>% 
    mutate(area_ha = n * 9 / 100) %>% 
    group_by(TPS, REG_TPS, SITINT_TPS) %>% 
    mutate(
        area_total = sum(area_ha, na.rm = TRUE),
        perdida_prop = round(area_ha / area_total * 100, 3)
    ) %>% 
    ungroup() %>% 
    filter(!is.na(area_total)) %>% 
    complete(year_loss = 2000:2018, nesting(TPS, REG_TPS, SITINT_TPS), 
        fill = list(perdida_ha = 0, perdida_prop = 0))

write_csv(filter(tps_loss, year_loss > 2000),  
    "entregas/perdida_tps_anual.csv")







####################################################
####################################################
# Usando recortes Pedro
# Sentinel 2018 17 clases
sentinel_2018_17_path <- "datos/TPS_entrega/sentinel2_2018_17.shp"
sent_18_17  <- sf::st_read(sentinel_2018_17_path, stringsAsFactors = FALSE) %>% 
    st_zm()
a <- sent_18_17 %>% 
    st_intersects(tps_r2)
ind_intersection <- map_int(a, ~ifelse(length(.) == 0, NA, .))
sent_18_17$SITINT_TPS <- tps_r2$SITINT_TPS[ind_intersection]

area_madmex_17_2018 <- sent_18_17 %>% 
    st_drop_geometry() %>% 
    filter(!is.na(SITINT_TPS)) %>% 
    group_by(SITINT_TPS, GRIDCODE) %>% 
    summarise(area = sum(area)) %>% 
    group_by(SITINT_TPS) %>% 
    mutate(prop_area = 100 * area / sum(area))
write_csv(area_madmex, "datos/area_madmex_sentinel_17_2018.csv")

sentinel_2017_17_path <- "datos/TPS_entrega/sentinel2_2017_17.shp"
sent_17_17  <- sf::st_read(sentinel_2017_17_path, stringsAsFactors = FALSE) %>% 
    st_zm()
a <- sent_17_17 %>% 
    st_intersects(tps_r2)
ind_intersection <- map_int(a, ~ifelse(length(.) == 0, NA, .))
sent_17_17$SITINT_TPS <- tps_r2$SITINT_TPS[ind_intersection]

area_madmex_17_2017 <- sent_17_17 %>% 
    st_drop_geometry() %>% 
    filter(!is.na(SITINT_TPS)) %>% 
    group_by(SITINT_TPS, GRIDCODE) %>%  
    summarise(area = sum(area)) %>% 
    group_by(SITINT_TPS) %>% 
    mutate(prop_area = 100 * area / sum(area))
write_csv(area_madmex_17_2017, "datos/area_madmex_sentinel_17_2017.csv")

##### IE 
ie_path <- "datos/TPS_entrega/ie_si_clip.shp"
ie  <- sf::st_read(ie_path, stringsAsFactors = FALSE) %>% 
    st_zm()
a <- ie %>% 
    st_intersects(tps_r2)
ind_intersection <- map_int(a, ~ifelse(length(.) == 0, NA, .))
ie$SITINT_TPS <- tps_r2$SITINT_TPS[ind_intersection]

ie_stats <- ie %>% 
    st_drop_geometry() %>% 
    filter(GRIDCODE > 0) %>% 
    group_by(SITINT_TPS, GRIDCODE) %>%
    summarise(
        area = sum(area),
    ) %>% 
    arrange(SITINT_TPS, GRIDCODE) %>% 
    mutate(
        cumsum_area = cumsum(area), 
        total_area = sum(area), 
        percent_area = cumsum_area / total_area
        ) %>% 
    summarise(
        mean_ie = sum(area * GRIDCODE) / sum(area),
        median_ie = first(GRIDCODE, order_by = percent_area < 0.5), 
        iq_025 = first(GRIDCODE, order_by = percent_area < 0.25), 
        iq_075 = first(GRIDCODE, order_by = percent_area < 0.75)
    )
write_csv(ie_stats, "datos/ie_stats.csv")

