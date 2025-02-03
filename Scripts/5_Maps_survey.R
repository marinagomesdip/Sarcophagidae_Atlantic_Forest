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

#Selecting only the target biome (MA) ------------------
MA <- mma %>%
  filter(code_biome == 4)

#cropping the rasters to match the size of MA  --------------

#Cropping the rasters to the extent of the 'MA' shapefile
ocor <- raster::crop(ocor, extent(MA))

#Applying the MA mask to all
ocor <- raster::mask(ocor, MA)

#adjusting geometries to avoid errors
conser <- st_make_valid(conser)
#cropping the shapefile to MA only
conser <- st_intersection(conser, MA)

#Standardizing the CRS for all maps ----------------------------------
#raster
crs(ocor) <- CRS(SRS_string = "EPSG:4674")
crs(mapageralbin) <- CRS(SRS_string = "EPSG:4674")

#Extracting the values of the conservation units to transform into a raster --------------------

# Defining the dimensions and resolution of the output raster
r <- raster(extent(mapageralbin), 
            res = c(0.167, 0.167))  # resolution set to 10 arcmin

# Creating the raster directly from the conservation units shapefile
raster_conser <- rasterize(conser, r, field = 10, background = 0)

# Applying the mask to restrict to the Atlantic Forest area
rasterizado_final <- raster::mask(raster_conser, MA)

# Reclassifying the raster values (keeping 10 where there is a conservation unit and 0 where there is none)
UCs <- calc(rasterizado_final, fun = function(x) ifelse(x == 10, 10, 0))

#Adjusting the CRS
crs(UCs) <- CRS(SRS_string = "EPSG:4674")

#Changing the values of the OCOR raster -------------------------------------------
# Defining the reclassification rules
# [lower_bound, upper_bound, new_value]
reclass_matrix <- matrix(c(-Inf, 0, 10,   # If value is 0, change to 10
                           0,  Inf, 0),  # If value is greater than 0, change to 0
                         ncol = 3, byrow = TRUE)

# Applying the reclassification to the 'ocor' raster
ocor_prio <- reclassify(ocor, reclass_matrix)


#Summing the rasters to create priorities ------------------------------------

#adjusting the rasters read in the 'raster' function to the extent of 'mapageralbin'

ocor_prio_resampled <- resample(ocor_prio, r, method = "bilinear")

final1 <- mapageralbin + UCs + ocor_prio_resampled

#MAP 1 - diversity x collection ----------------------------------
#Areas heavily collected receive less priority and conservation units receive more priority

#Raw map
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

#Histogram with the raster values ------------------------------
# Extracting the values as a vector
valores <- getValues(final1)

# Creating the DataFrame
df <- data.frame(valores_raster = valores)

#removing NA values
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

#Map showing only the upper quartile ----------------------------------

#calculating the upper quartile
quartis <- quantile(df$valores_raster)
print(quartis)

#defining the reclassification rules
reclass_matrix <- matrix(c(-Inf, 49, 0,  # Values ≤ 49 become 0
                           49, Inf, 1),  # Values > 49 become 1
                         ncol = 3, byrow = TRUE)

#applying the reclassification to the raster
prioridades <- reclassify(final1, reclass_matrix)

#showing on the map
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

#Tables with conservation units of interest
#Gave up and did it in QGIS
prioridades_poly <- rasterToPolygons(prioridades, fun = function(x) x == 1)

prioridades_poly <- st_as_sf(prioridades_poly)

st_write(prioridades_poly, "./cap3/priori/priori75.shp")

st_write(conser, "./cap3/ucs/ucss.shp")

# -- QGIS for geoprocessing --

#loading UC's shapefile
shape <- "C:/Users/Museu Nacional/Desktop/ucs_recortado75"

ucs_recortado <- sf::st_read(dsn = file.path(shape, 
                                             layer = "ucs_recortado75.shp")) 

tabela_ucs <- ucs_recortado %>%
  dplyr::select(nm_cns_, categry, gvrnmn_) %>%
  distinct()

tabela_ucs <- st_drop_geometry(tabela_ucs)

write_csv(tabela_ucs,"C:/Users/Museu Nacional/Desktop/tabela_ucs_75.csv")

#Map showing only the upper 90% quartile ----------------------------------

#calculating the upper quartile
quartis <- quantile(df$valores_raster)
print(quartis)

#defining the reclassification rules
reclass_matrix <- matrix(c(-Inf, 58.2, 0,  # Values ≤ 58.2 become 0
                           58.2, Inf, 1),  # Values > 58.2 become 1
                         ncol = 3, byrow = TRUE)

#applying the reclassification to the raster
prioridades <- reclassify(final1, reclass_matrix)

#showing on the map
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

#Tables with conservation units of interest
#Gave up and did it in QGIS
prioridades_poly <- rasterToPolygons(prioridades, fun = function(x) x == 1)

prioridades_poly <- st_as_sf(prioridades_poly)

st_write(prioridades_poly, "./cap3/priori/priori90.shp")

# -- QGIS for geoprocessing --

#loading UC's shapefile
shape <- "C:/Users/Dell/OneDrive/Área de Trabalho/ucs_recortado90"

ucs_recortado <- sf::st_read(dsn = file.path(shape, 
                                             layer = "ucs_recortado90.shp")) 

tabela_ucs <- ucs_recortado %>%
  dplyr::select(nm_cns_, categry, gvrnmn_) %>%
  distinct()

tabela_ucs <- st_drop_geometry(tabela_ucs)

write_csv(tabela_ucs,"C:/Users/Museu Nacional/Desktop/tabela_ucs_90.csv")


#MAPS - ZOOM IN AREAS OF INTEREST ------------------------

#renaming the column of tabela_ucs to filter the conser object
tabela_ucs <- tabela_ucs %>%
  rename(name_conservation_unit = nm_cns_)

#filtering the shapefile of the 90% conservation units
ucs_90_shp <- semi_join(conser, tabela_ucs)


#MAP - AREA OF INTEREST FEDERAL CONSERVATION UNITS -----------------------

#Creating a map with only the Atlantic Forest
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


#filtering only federal conservation units and Parks ---------------------------
federal <- ucs_90_shp %>%
  dplyr::filter(government_level == "federal") %>%
  dplyr::filter(category == "Parque") 

#Zoom Parks
extent <- st_bbox(c(xmin = -47, ymin = -25, xmax = -30, ymax = -19), 
                  crs = st_crs(BR))

# Cropping the shapefile to show only the state of Paraíba
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

tmap_save(
  tm = federalgeral, 
  filename = "./cap3/mapas_surveys/federalgeralzoom.png", 
  width = 3000, 
  height = 2800
)

#filtering only federal conservation units
federal <- ucs_90_shp %>%
  dplyr::filter(government_level == "federal") %>%
  dplyr::filter(category == "Reserva Biológica" | 
                  category == "Monumento Natural") 

#Zoom Biological Reserves + Natural Monuments 
extent <- st_bbox(c(xmin = -47, ymin = -22.7, xmax = -30, ymax = -15), 
                  crs = st_crs(BR))

# Cropping the shapefile to show only the state of Paraíba
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

tmap_save(
  tm = federalgeral, 
  filename = "./cap3/mapas_surveys/federalgeralzoom2.png", 
  width = 3000, 
  height = 2800
)