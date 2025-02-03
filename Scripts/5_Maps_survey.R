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
mma <- read_biomes(year = 2019, showProgress = FALSE)
BR <- geobr::read_country(year = 2020)

#Selecionar apenas o bioma-alvo (MA) ------------------
MA <- mma %>%
  filter(code_biome == 4)

#cortando os rasters para ficarem todos do tamanho da MA  --------------

#Cortando os rasters para a extensão do shapefile 'MA'
ocor <- raster::crop(ocor, extent(MA))

#Aplicando a mascara da MA para todos
ocor <- raster::mask(ocor, MA)

#ajustando as geometrias para não dar erro
conser <- st_make_valid(conser)
#cortando o shp só para MA
conser <- st_intersection(conser, MA)

#Padronizando o CRS para todos os mapas ----------------------------------
#raster
crs(ocor) <- CRS(SRS_string = "EPSG:4674")
crs(mapageralbin) <- CRS(SRS_string = "EPSG:4674")

#Extraindo os valores das UCs para transformar em um raster --------------------

# Definir as dimensões e a resolução do raster de saída
r <- raster(extent(mapageralbin), 
            res = c(0.167, 0.167))  # resolução configurada para 10 arcmin

# Criando o raster diretamente a partir do shapefile das UCs
raster_conser <- rasterize(conser, r, field = 10, background = 0)

# Aplicando a máscara para restringir à área da Mata Atlântica
rasterizado_final <- raster::mask(raster_conser, MA)

# Reclassificando os valores do raster (mantendo 10 onde houver UC e 0 onde não houver)
UCs <- calc(rasterizado_final, fun = function(x) ifelse(x == 10, 10, 0))

#Ajustando o CRS
crs(UCs) <- CRS(SRS_string = "EPSG:4674")

#Mudandos os valores do raster OCOR -------------------------------------------
# Definir as regras de reclassificação
# [lower_bound, upper_bound, new_value]
reclass_matrix <- matrix(c(-Inf, 0, 10,   # Se valor for 0, muda para 10
                           0,  Inf, 0),  # Se valor for maior que 0, muda para 0
                         ncol = 3, byrow = TRUE)

# Aplicar a reclassificação ao raster 'ocor'
ocor_prio <- reclassify(ocor, reclass_matrix)


#Somando os rasters para criar prioridades ------------------------------------

#ajustando os rasters lidos na função 'raster' para a extensão de 'mapageralbin'

ocor_prio_resampled <- resample(ocor_prio, r, method = "bilinear")

final1 <- mapageralbin + UCs + ocor_prio_resampled

#MAPA 1 - diversidade x coleta ----------------------------------
#Áreas muito coletadas recebem menos prioridade e UCS recebem mais prioridade

#Mapa bruto
breaks <- seq(0, 80, by = 15)

maprank <- tm_shape(final1) +
  tm_raster(palette = "YlGn",
            style = "cont",
            alpha = 1,
            breaks = breaks,
            title = "Priority Rank") +
  tm_layout(frame.lwd = 3,
            legend.position = c("left", "top"),
            legend.text.size = 0.9) +
  tm_scale_bar(position = c("right", "top"),
               width = 0.15,
               color.dark = "gray44")

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
  filename = "./cap3/mapas_surveys/mapariqueza_coleta_UCs.png", 
  width = 3000, 
  height = 2800
)

#Histograma com os valores do raster ------------------------------
# Extrair os valores como um vetor
valores <- getValues(final1)

# Criar o DataFrame
df <- data.frame(valores_raster = valores)

#removendo os valores de NA
df <- df %>%
  filter(!is.na(valores_raster))

mapa1 <- ggplot(df, aes(x = valores_raster)) +
  geom_histogram(binwidth = 1.5,
                 fill = "gray28",
                 color = "white") +
  theme_classic() +
  labs(x = "rank value", y = "Count")

ggsave(filename = "./cap3/graphs/mapa1valores.png",
       plot = mapa1, device = "png", width = 7, height = 4, dpi = 300)

#Mapa mostrando apenas o quartil superior ----------------------------------

#calculando o quartil superior
quartis <- quantile(df$valores_raster)
print(quartis)

#definir as regras de reclassificação
reclass_matrix <- matrix(c(-Inf, 49, 0,  # Valores ≤ 49 viram 0
                           49, Inf, 1),  # Valores > 49 viram 1
                         ncol = 3, byrow = TRUE)

#aplicar a reclassificação ao raster
prioridades <- reclassify(final1, reclass_matrix)

#mostrar no mapa
breaks2 <- seq(0, 1, by = 1)

mapareclass <- tm_shape(prioridades) +
  tm_raster(palette = "Purples",
            style = "cont",
            alpha = 0.7,
            breaks = breaks2,
            title = "Priority Rank") +
  tm_layout(frame.lwd = 3,
            legend.position = c("left", "top"),
            legend.text.size = 0.9) +
  tm_scale_bar(position = c("right", "top"),
               width = 0.15,
               color.dark = "gray44")

mapareclass <- mapareclass +
  tm_shape(MA) +
  tm_borders(lwd = 0.5,
             col = "gray60")

mapareclass <- mapareclass +
  tm_shape(BR) +
  tm_borders(lwd = 0.5,
             col = "gray44")

tmap_save(
  tm = mapareclass, 
  filename = "./cap3/mapas_surveys/apenas_quartil_75.png", 
  width = 3000, 
  height = 2800
)

#Tabelas com UCs de interesse
#Desisti e fiz no QGIS
prioridades_poly <- rasterToPolygons(prioridades, fun = function(x) x == 1)

prioridades_poly <- st_as_sf(prioridades_poly)

st_write(prioridades_poly, "./cap3/priori/priori75.shp")

st_write(conser, "./cap3/ucs/ucss.shp")

# -- QGIS para geoprocessamento --

#loading UC's shapefile
shape <- "C:/Users/Museu Nacional/Desktop/ucs_recortado75"

ucs_recortado <- sf::st_read(dsn = file.path(shape, 
                                       layer = "ucs_recortado75.shp")) 

tabela_ucs <- ucs_recortado %>%
  dplyr::select(nm_cns_, categry, gvrnmn_) %>%
  distinct()

tabela_ucs <- st_drop_geometry(tabela_ucs)

write_csv(tabela_ucs,"C:/Users/Museu Nacional/Desktop/tabela_ucs_75.csv")

#Mapa mostrando apenas o quartil 90% superior ----------------------------------

#calculando o quartil superior
quartis <- quantile(df$valores_raster)
print(quartis)

#definir as regras de reclassificação
reclass_matrix <- matrix(c(-Inf, 58.2, 0,  # Valores ≤ 58.2 viram 0
                           58.2, Inf, 1),  # Valores > 58.2 viram 1
                         ncol = 3, byrow = TRUE)

#aplicar a reclassificação ao raster
prioridades <- reclassify(final1, reclass_matrix)

#mostrar no mapa
breaks2 <- seq(0, 1, by = 1)

mapareclass <- tm_shape(prioridades) +
  tm_raster(palette = "Purples",
            style = "cont",
            alpha = 0.7,
            breaks = breaks2,
            title = "Priority Rank") +
  tm_layout(frame.lwd = 3,
            legend.position = c("left", "top"),
            legend.text.size = 0.9) +
  tm_scale_bar(position = c("right", "top"),
               width = 0.15,
               color.dark = "gray44")

mapareclass <- mapareclass +
  tm_shape(MA) +
  tm_borders(lwd = 0.5,
             col = "gray60")

mapareclass <- mapareclass +
  tm_shape(BR) +
  tm_borders(lwd = 0.5,
             col = "gray44")

tmap_save(
  tm = mapareclass, 
  filename = "./cap3/mapas_surveys/apenas_quartil_90.png", 
  width = 3000, 
  height = 2800
)

#Tabelas com UCs de interesse
#Desisti e fiz no QGIS
prioridades_poly <- rasterToPolygons(prioridades, fun = function(x) x == 1)

prioridades_poly <- st_as_sf(prioridades_poly)

st_write(prioridades_poly, "./cap3/priori/priori90.shp")

# -- QGIS para geoprocessamento --

#loading UC's shapefile
shape <- "C:/Users/Dell/OneDrive/Área de Trabalho/ucs_recortado90"

ucs_recortado <- sf::st_read(dsn = file.path(shape, 
                                             layer = "ucs_recortado90.shp")) 

tabela_ucs <- ucs_recortado %>%
  dplyr::select(nm_cns_, categry, gvrnmn_) %>%
  distinct()

tabela_ucs <- st_drop_geometry(tabela_ucs)

write_csv(tabela_ucs,"C:/Users/Museu Nacional/Desktop/tabela_ucs_90.csv")


#MAPAS - ZOOM EM ÁREAS DE INTERESSE ------------------------

#renomeando a coluna de tabela_ucs para filtrar o objeto conser
tabela_ucs <- tabela_ucs %>%
  rename(name_conservation_unit = nm_cns_)

#filtrando o shapefile das ucs 90%
ucs_90_shp <- semi_join(conser, tabela_ucs)


#MAPA - ÁREA DE INTERESSE UCS FEDERAIS -----------------------

#Fazendo um mapa só com a Mata Atlântica
federalgeral <- tm_shape(MA) +
  tm_fill(col = "lightgoldenrod")

federalgeral <- federalgeral +
  tm_shape(BR) +
  tm_borders(lwd = 0.5,
             col = "gray44")

federalgeral <- federalgeral +
  tm_shape(federal) +
  tm_borders(col = "darkgreen",
             lwd = 1)+
  tm_scale_bar(position = c("right", "top"),
               width = 0.15,
               color.dark = "gray44")

tmap_save(
  tm = federalgeral, 
  filename = "./cap3/mapas_surveys/MA.png", 
  width = 3000, 
  height = 2800
)


#filtrando apenas UCs federais e Parques ---------------------------
federal <- ucs_90_shp %>%
  dplyr::filter(government_level == "federal") %>%
  dplyr::filter(category == "Parque") 

#Zoom Parques
extent <- st_bbox(c(xmin = -47, ymin = -25, xmax = -30, ymax = -19), 
                  crs = st_crs(BR))

# Cortar o shapefile para mostrar apenas o estado da Paraíba
cropped <- st_crop(MA, extent)

federalgeral <- tm_shape(cropped) +
  tm_fill(col = "lightgoldenrod")

federalgeral <- federalgeral +
  tm_shape(federal) +
  tm_borders(col = "darkgreen",
             lwd = 2)+
  tm_scale_bar(position = c("right", "bottom"),
               width = 0.15,
               color.dark = "gray44")

federalgeral

tmap_save(
  tm = federalgeral, 
  filename = "./cap3/mapas_surveys/federalgeralzoom.png", 
  width = 3000, 
  height = 2800
)

#filtrando apenas UCs federais
federal <- ucs_90_shp %>%
  dplyr::filter(government_level == "federal") %>%
  dplyr::filter(category == "Reserva Biológica" | 
                  category == "Monumento Natural") 

#Zoom Reservas Biológicas + Monas 
extent <- st_bbox(c(xmin = -47, ymin = -22.7, xmax = -30, ymax = -15), 
                  crs = st_crs(BR))

# Cortar o shapefile para mostrar apenas o estado da Paraíba
cropped <- st_crop(MA, extent)

federalgeral <- tm_shape(cropped) +
  tm_fill(col = "lightgoldenrod")

federalgeral <- federalgeral +
  tm_shape(federal) +
  tm_borders(col = "darkgreen",
             lwd = 2)+
  tm_scale_bar(position = c("right", "bottom"),
               width = 0.15,
               color.dark = "gray44")

federalgeral

tmap_save(
  tm = federalgeral, 
  filename = "./cap3/mapas_surveys/federalgeralzoom2.png", 
  width = 3000, 
  height = 2800
)
