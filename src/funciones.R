library(leaflet)

theme_set(theme_minimal())

tab_clases_17 <- tibble(clase = 1:17, descrip = c(
    "Bosques de aciculifolias y escuamifolias", 
    "Bosques latifoliados",
    "Bosque Húmedo de Montaña", 
    "Manglar y petén", 
    "Selvas húmedas", 
    "Selvas secas", 
    "Matorral alto denso", 
    "Matorral mésico",
    "Matorral bajo abierto", 
    "Vegetación acuática menor",
    "Vegetación de suelos arenosos", 
    "Vegetación halófila",
    "Pastizales y otra vegetación herbácea", 
    "Tierras agrícolas",
    "Urbano y Construido", 
    "Suelo desnudo", 
    "Agua"))

estilos <- rjson::fromJSON(file = "datos/spatial_layers_styles.json")
estilos_df <- estilos %>% 
    flatten() %>% 
    map_df(~tibble(layer = .$layer, labels = list(.$data_labels), 
        colors = list(.$colors)))

mapa <- function(capa, estilos_df, mapa_base = FALSE){
    # (-117.12776, 14.5388286402, -86.811982388, 32.72083))
    xmin <- -117.12776
    xmax <- -86.811982388
    ymin <- 14.5388286402
    ymax <- 32.72083
    
    extent <- raster::extent(c(xmin, xmax, ymin, ymax))
    
    estilo <- dplyr::filter(estilos_df, layer == capa)
    
    mapa <- leaflet() %>% 
        fitBounds(lng1 = xmin, lat1 = ymin, lng2 = xmax, lat2 = ymax) %>% 
        addProviderTiles(providers$Esri.WorldImagery) %>% 
        addProviderTiles(providers$Esri.WorldImagery, group = "Terreno") %>% 
        addWMSTiles("https://monitoreo.conabio.gob.mx/geoserver/geoportal/wms",
            layers = capa,
            options = WMSTileOptions(format = "image/png", transparent = TRUE), 
            group = "Capa"
        ) %>%
        addLegend("bottomright", colors = estilo$colors[[1]],
            labels = estilo$labels[[1]], 
            opacity = 1, group = "leyenda") %>%
        addWMSTiles("https://monitoreo.conabio.gob.mx/geoserver/tps/wms",
            layers = "tps_longlat",
            options = WMSTileOptions(format = "image/png", transparent = TRUE, 
                opacity = 0.5), group = "TPS"
        )
    if (!mapa_base) {
        mapa <- mapa %>% 
            # addWMSTiles("https://monitoreo.conabio.gob.mx/geoserver/tps/wms",
            #     layers = "tps_buffer_longlat",
            #     options = WMSTileOptions(format = "image/png", transparent = TRUE, 
            #         opacity = 0.6), group = "Periferia"
            # ) %>% 
            addWMSTiles("https://monitoreo.conabio.gob.mx/geoserver/reportes/wms",
                layers = "GapPriorExtrema", group = "GAP",
                options = WMSTileOptions(format = "image/png", transparent = TRUE)) %>%
            addWMSTiles("https://monitoreo.conabio.gob.mx/geoserver/reportes/wms",
                layers = "LocPob2500", group = "Loc 2500",
                options = WMSTileOptions(format = "image/png", transparent = TRUE)
            ) %>%
            addLayersControl(
                baseGroups = c("Capa", "Terreno"),
                overlayGroups = c("leyenda", "TPS", "GAP", "Loc 2500", 
                    "Periferia"),
                options = layersControlOptions(collapsed = FALSE)
            )  %>%
            hideGroup("GAP") %>% 
            hideGroup("Loc 2500") %>% 
            # hideGroup("Periferia") %>% 
            leaflet.extras::addFullscreenControl(position = "topleft", 
                pseudoFullscreen = FALSE) %>% 
            leafem::addHomeButton(ext = extent, layer.name = "Centrar país", 
                position = "topleft")
    }
    return(mapa)
}


# boxplot integridad y datos para texto
tabs_grafica_ie <- function(mis_anps_eco, mi_anp, mi_anp_corto, path_stats, 
    path_samples, escala_color, anp_nombres) {
    ie_stats <- read_csv(path_stats)
    ie_samples <- read_csv(path_samples)
    mis_ie_samples <- ie_samples %>% 
        dplyr::filter(anp %in% mis_anps_eco, tipo_id == 1)  %>% 
        left_join(anp_nombres, by = c("anp" = "anp_sin_acentos")) %>% 
        mutate(clase = ifelse(anp == mi_anp, mi_anp_corto, "otras")) 
    mis_ie_stats <- ie_stats %>% 
        dplyr::filter(anp %in% mis_anps_eco, tipo_id != 1)  %>% 
        left_join(anp_nombres, by = c("anp" = "anp_sin_acentos")) %>% 
        mutate(
            clase = case_when(
                tipo_id == 2 ~ "periferia",
                tipo_id == 3 ~ "z.núcleo"
            ) 
        )
    mi_anp_ie_stats <- ie_stats %>% 
        dplyr::filter(anp == mi_anp, tipo_id == 1)
    boxplot <- ggplot() +
        coord_flip() +
        geom_boxplot(data = mis_ie_samples, aes(x = reorder(anp_corto, valores, 
            median), y = valores, color = clase), alpha = 0.6, 
            show.legend = FALSE, outlier.color = "gray90", coef = 0) +
        scale_color_manual("", values = escala_color) +
        geom_point(data = mis_ie_stats, aes(x = anp_corto, y = mediana, 
            color = clase), alpha = 0.8) +
        labs(x = ",", title = "Integridad Ecosistémica", y = "")
    
    return(list(boxplot = boxplot, ie_stats = mi_anp_ie_stats))
}
