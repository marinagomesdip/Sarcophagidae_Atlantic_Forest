#Chapter three - thesis 
#Atlantic Forest - Modelling
#Marina Morim Gomes 2024
#gomes.mari.95@gmail.com

#loading packages -------------------------------------
library(tidyverse)
library(raster)
library(sf)
library(tmap)
library(geobr)

#loading data ----------------------------------------
ocor <- raster("./cap3/rasters/mapa_ocorrencia_raster_10arcmin.tif")
conser <- read_conservation_units(showProgress = FALSE)
mapageralbin <- raster("./cap3/mapas_empilhados/MA_geral_binary.tif")
mapageralcont <- raster("./cap3/mapas_empilhados/MA_geral_continuous.tif")
mma <- read_biomes(year = 2019, showProgress = FALSE)
BR <- geobr::read_country(year = 2020)
mapbiomas <- raster("./cap3/rasters/teste_novo.tif")

#Selecionar apenas o bioma-alvo (MA) ------------------
MA <- mma %>%
  filter(code_biome == 4)

#cortando os rasters para ficarem todos do tamanho da MA  --------------

#Cortando os rasters para a extensão do shapefile 'MA'
ocor <- raster::crop(ocor, extent(MA))

#Aplicando a mascara da MA para todos
ocor <- raster::mask(ocor, MA)

#CORTAR AS OCORRENCIAS SO PRA MA
# Converter o raster em polígonos
polygons <- rasterToPolygons(ocor, fun = function(x) x > 0, dissolve = TRUE)

# Converter para um objeto sf (opcional, mas recomendado para trabalhar com shapefiles no R)
polygons_sf <- st_as_sf(polygons)

# Visualizar para confirmar
plot(polygons_sf)

#CORTAR AS UCS SO PRA MA
#ajustando as geometrias para não dar erro
conser <- st_make_valid(conser)

# Cortar shapefile1 pela extensão do shapefile2
conser <- st_intersection(conser, MA)

st_write(conser, "./cap3/shp_ucs/ucsMA.shp", delete_dsn = TRUE)

#MAPA 1 - diversidade x coleta ----------------------------------
#Áreas muito coletadas recebem menos prioridade

#gerando o mapa
breaks <- seq(0, 1, by = 0.2)

maprank <- tm_shape(mapageralcont) +
  tm_raster(palette = "YlOrBr",
            style = "cont",
            alpha = 1,
            breaks = breaks,
            title = "Species Richness") +
  tm_layout(frame.lwd = 3,
            legend.position = c("left", "top")) +
  tm_scale_bar(position = c("right", "top"),
               width = 0.15,
               color.dark = "gray44")

maprank <- maprank +
  tm_shape(polygons_sf) +
  tm_borders(lwd = 2,
             col = "darkred")

maprank <- maprank +
  tm_shape(MA) +
  tm_borders(lwd = 1.2,
             col = "gray60")

maprank <- maprank +
  tm_shape(BR) +
  tm_borders(lwd = 0.5,
             col = "gray44")

tmap_save(
  tm = maprank, 
  filename = "./cap3/mapas_surveys/mapadiversidadeecoleta.png", 
  width = 3000, 
  height = 2800
)

#MAPA 2 - mapa 1 + UC ----------------------------------
#Áreas pouco coletadas dentro de UC maior diversidade

mapuc <- maprank +
  tm_shape(conser) +
  tm_borders(col = "darkgreen",
             lwd = 1)

tmap_save(
  tm = mapuc, 
  filename = "./cap3/mapas_surveys/mapadiversidadecoletaeuc.png", 
  width = 3000, 
  height = 2800
)

#MAPAS 3 A 6 - ZOOM EM ÁREAS DE INTERESSE ---------------

breaks2 <- seq(0, 80, by = 15)

#MAPA 3 - ÁREA DE INTERESSE NO PR -----------------------

# Extensão do recorte
extensao <- extent(-55, -48, -26, -20)

# Cortar na região alvo
ucilhagr <- crop(mapageralbin, extensao)

# Plotando para ver como ficou 
plot(ucilhagr)

#Refazendo o mapa 
mapdiv <- tm_shape(ucilhagr) +
  tm_raster(palette = "YlOrBr",
            style = "cont",
            alpha = 1,
            breaks = breaks2,
            title = "Species Richness") +
  tm_layout(frame.lwd = 3,
            legend.position = c("left", "top")) +
  tm_scale_bar(position = c("right", "top"),
               width = 0.15,
               color.dark = "gray44") +
  tm_graticules(labels.inside.frame = FALSE,
                lines = FALSE)

mapdiv <- mapdiv +
  tm_shape(BR) +
  tm_borders(lwd = 2,
             col = "gray44")

mapdiv <- mapdiv +
  tm_shape(polygons_sf) +
  tm_borders(lwd = 6,
             col = "darkred")

mapdiv <- mapdiv +
  tm_shape(conser) +
  tm_borders(col = "darkgreen",
             lwd = 5)

tmap_save(
  tm = mapdiv, 
  filename = "./cap3/mapas_surveys/zoompr.png", 
  width = 3000, 
  height = 2800
)

#MAPA 4 - ÁREA DE INTERESSE NO LITORAL RJ X SP -----------------------

# Extensão do recorte
extensao <- extent(-47, -41.7, -24.7, -20)

# Cortar na região alvo
uc <- crop(mapageralbin, extensao)

# Plotando para ver como ficou 
plot(uc)

#Refazendo o mapa 
mapdiv <- tm_shape(uc) +
  tm_raster(palette = "YlOrBr",
            style = "cont",
            alpha = 1,
            breaks = breaks2,
            title = "Species Richness") +
  tm_layout(frame.lwd = 3,
            legend.position = c("left", "top")) +
  tm_scale_bar(position = c("right", "top"),
               width = 0.15,
               color.dark = "gray44") +
  tm_graticules(labels.inside.frame = FALSE,
                lines = FALSE)

mapdiv <- mapdiv +
  tm_shape(BR) +
  tm_borders(lwd = 2,
             col = "gray44")

mapdiv <- mapdiv +
  tm_shape(polygons_sf) +
  tm_borders(lwd = 6,
             col = "darkred")

mapdiv <- mapdiv +
  tm_shape(conser) +
  tm_borders(col = "darkgreen",
             lwd = 5)

tmap_save(
  tm = mapdiv, 
  filename = "./cap3/mapas_surveys/zoomrj.png", 
  width = 3000, 
  height = 2800
)

#MAPA 5 - ÁREA DE INTERESSE NO LITORAL MG -----------------------

# Extensão do recorte
extensao <- extent(-43, -37, -20, -15)

# Cortar na região alvo
uc <- crop(mapageralbin, extensao)

# Plotando para ver como ficou 
plot(uc)

#Refazendo o mapa 
mapdiv <- tm_shape(uc) +
  tm_raster(palette = "YlOrBr",
            style = "cont",
            alpha = 1,
            breaks = breaks2,
            title = "Species Richness") +
  tm_layout(frame.lwd = 3,
            legend.position = c("left", "top")) +
  tm_scale_bar(position = c("right", "top"),
               width = 0.15,
               color.dark = "gray44") +
  tm_graticules(labels.inside.frame = FALSE,
                lines = FALSE)

mapdiv <- mapdiv +
  tm_shape(BR) +
  tm_borders(lwd = 2,
             col = "gray44")

mapdiv <- mapdiv +
  tm_shape(polygons_sf) +
  tm_borders(lwd = 6,
             col = "darkred")

mapdiv <- mapdiv +
  tm_shape(conser) +
  tm_borders(col = "darkgreen",
             lwd = 5)

tmap_save(
  tm = mapdiv, 
  filename = "./cap3/mapas_surveys/zoommg.png", 
  width = 3000, 
  height = 2800
)

  #MAPA 6 - ÁREA DE INTERESSE NORDESTE -----------------------

# Extensão do recorte
extensao <- extent(-38, -33, -10, -5)

# Cortar na região alvo
uc <- crop(mapageralbin, extensao)

# Plotando para ver como ficou 
plot(uc)

#Refazendo o mapa 
mapdiv <- tm_shape(uc) +
  tm_raster(palette = "YlOrBr",
            style = "cont",
            alpha = 1,
            breaks = breaks2,
            title = "Species Richness") +
  tm_layout(frame.lwd = 3,
            legend.position = c("left", "top")) +
  tm_scale_bar(position = c("right", "top"),
               width = 0.15,
               color.dark = "gray44") +
  tm_graticules(labels.inside.frame = FALSE,
                lines = FALSE)

mapdiv <- mapdiv +
  tm_shape(BR) +
  tm_borders(lwd = 2,
             col = "gray44")

mapdiv <- mapdiv +
  tm_shape(polygons_sf) +
  tm_borders(lwd = 6,
             col = "darkred")

mapdiv <- mapdiv +
  tm_shape(conser) +
  tm_borders(col = "darkgreen",
             lwd = 5)

tmap_save(
  tm = mapdiv, 
  filename = "./cap3/mapas_surveys/zoomnord.png", 
  width = 3000, 
  height = 2800
)



#testes

# Função para alterar os valores
reclassificar_valores <- function(x) {
  ifelse(x %in% c(31, 214), 10, 0)
}

# Aplicar a função ao raster
natural <- calc(mapbiomas, fun = reclassificar_valores)

# Visualizando o raster modificado
plot(natural)

natural <- raster::crop(natural, extent(MA))
natural <- raster::mask(natural, MA)

plot(natural)

teste <- mapageralbin + natural

plot(teste)

maprank <- tm_shape(teste) +
  tm_raster(palette = "Greys",
            style = "cont",
            alpha = 1,
            breaks = breaks2,
            title = "Priority Rank") +
  tm_layout(frame.lwd = 3,
            legend.position = c("left", "top")) +
  tm_scale_bar(position = c("right", "top"),
               width = 0.15,
               color.dark = "gray44")

maprank <- maprank +
  tm_shape(polygons_sf) +
  tm_borders(lwd = 2,
             col = "darkred")

maprank <- maprank +
  tm_shape(MA) +
  tm_borders(lwd = 1.2,
             col = "gray60")

maprank <- maprank +
  tm_shape(BR) +
  tm_borders(lwd = 0.5,
             col = "gray44")

maprank <- maprank +
  tm_shape(conser) +
  tm_borders(col = "darkgreen",
             lwd = 1)

tmap_save(
  tm = maprank, 
  filename = "./cap3/mapas_surveys/divertityplusnatural.png", 
  width = 3000, 
  height = 2800
)