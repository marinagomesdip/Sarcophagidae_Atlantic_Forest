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
mma <- read_biomes(year = 2019, showProgress = FALSE)
BR <- geobr::read_country(year = 2020)
dadosmodelos <- read.csv("./Data/Processados/7_Suplementar_Material5.csv")

#Select target biome (AR) only ------------------
MA <- mma %>%
  filter(code_biome == 4)

#Creating the separate lists that will generate the maps (genus) -------
gene <- c("Tricharaea (Sarcophagula) canuta",
          "Tricharaea (Sarcophagula) occidua",
          "Tricharaea (Sarothromyia) femoralis",
          "Tricharaea (Tricharaea) brevicornis")

#Creating standardized geographic scales -----------------------------
breaks <- seq(0.00, 1, by = 0.1)   #continuous maps
breaks2 <- seq(0, 4, by = 1)       #needs to be adjusted by genus

#GENUS MAPS (REPLACE GENRE IN FOR NAME) ----------------------------------------------------

#Folders Paths
input_folder <- "./cap3/cropped_maps/"
output_folder <- "./cap3/mapas_empilhados/"

#Initialize binary and continuous raster with first file to set extension and resolution
first_raster_binario <- raster(paste0(input_folder, gene[1], "_ensemble_binary.tif"))
first_raster_continuo <- raster(paste0(input_folder, gene[1], "_ensemble_continuous.tif"))

#Create empty rasters with the same extent and resolution to accumulate sums and averages
summed_raster_binario <- raster(first_raster_binario)
summed_raster_continuo <- raster(first_raster_continuo)
summed_raster_binario[] <- 0  # Inicializa todos os valores para zero
summed_raster_continuo[] <- 0  # Inicializa todos os valores para zero

#Counter for continuous rasters
count_continuo <- 0

#Default name to change with genus
nome_padrao <- "Tricharaea_geral_"

#Iterate over species names and process rasters
for (sp in gene) {
  # Caminhos dos arquivos binário e contínuo
  file_path_binario <- paste0(input_folder, sp, "_ensemble_binary.tif")
  file_path_continuo <- paste0(input_folder, sp, "_ensemble_continuous.tif")
  
  # Ler os rasters binário e contínuo
  current_raster_binario <- raster(file_path_binario)
  current_raster_continuo <- raster(file_path_continuo)
  
  # Somar o raster binário ao acumulado
  summed_raster_binario <- summed_raster_binario + current_raster_binario
  
  # Somar o raster contínuo ao acumulado e incrementar o contador
  summed_raster_continuo <- summed_raster_continuo + current_raster_continuo
  count_continuo <- count_continuo + 1
}

# Salvar o raster binário somado
writeRaster(summed_raster_binario, filename = paste0(output_folder, nome_padrao, "_binary.tif"), format = "GTiff")

# Calcular a média do raster contínuo
mean_raster_continuo <- summed_raster_continuo / count_continuo

# Salvar o raster contínuo (média)
writeRaster(mean_raster_continuo, filename = paste0(output_folder, nome_padrao, "_continuous.tif"), format = "GTiff")

# Gerar o mapa binário somado
mapsomado_binario <- tm_shape(summed_raster_binario) +
  tm_raster(palette = "PuBu",
            style = "cont",
            alpha = 0.8,
            breaks = breaks2,
            title = "Species Richness") +
  tm_layout(frame.lwd = 3,
            legend.position = c("left", "top")) +
  tm_scale_bar(position = c("right", "top"),
               width = 0.15,
               color.dark = "gray44")

# Adicionar shapefiles ao mapa binário
mapsomado_binario <- mapsomado_binario +
  tm_shape(MA) +
  tm_borders(lwd = 1.2, col = "gray60") +
  tm_shape(BR) +
  tm_borders(lwd = 0.5, col = "gray44")

# Salvar o mapa binário
tmap_save(
  tm = mapsomado_binario, 
  filename = paste0(output_folder, nome_padrao, "binary_mapa.png"), 
  width = 3000, 
  height = 2800
)

# Gerar o mapa contínuo (média)
mapsomado_continuo <- tm_shape(mean_raster_continuo) +
  tm_raster(palette = "PuBu",
            style = "cont",
            alpha = 0.8,
            breaks = breaks,
            title = "Species Richness") +
  tm_layout(frame.lwd = 3,
            legend.position = c("left", "top")) +
  tm_scale_bar(position = c("right", "top"),
               width = 0.15,
               color.dark = "gray44")

# Adicionar shapefiles ao mapa contínuo
mapsomado_continuo <- mapsomado_continuo +
  tm_shape(MA) +
  tm_borders(lwd = 1.2, col = "gray60") +
  tm_shape(BR) +
  tm_borders(lwd = 0.5, col = "gray44")

# Salvar o mapa contínuo
tmap_save(
  tm = mapsomado_continuo, 
  filename = paste0(output_folder, nome_padrao, "continuous_mapa.png"), 
  width = 3000, 
  height = 2800
)

#MAPAS POUCO DIST -----------------------
#selecionar quais espécies usará em qual mapa
poucodis <- dadosmodelos %>%
  filter(TPR > 0.5) %>%
  filter(proporcao_area <= 30) %>%
  dplyr::select(nome_cientifico)

#criando a lista de espécies necessárias para cada mapa
listpouc <- c("Argoravinia aurea",
              "Argoravinia rufiventris",
              "Dexosarcophaga (Farrimyia) globulosa",
              "Lepidodexia (Notochaeta) cognata",
              "Microcerella analis",
              "Microcerella halli",
              "Oxysarcodexia bakeri",
              "Oxysarcodexia fringidea",
              "Oxysarcodexia intona",
              "Oxysarcodexia major",
              "Oxysarcodexia riograndensis",
              "Oxysarcodexia timida",
              "Peckia (Peckia) uncinata",
              "Peckia (Squamatodes) trivittata",
              "Titanogrypa (Cucullomyia) luculenta",
              "Tricharaea (Sarothromyia) femoralis",
              "Tricharaea (Tricharaea) brevicornis",
              "Udamopyga percita",
              "Villegasia almeidai")


#Diretórios
input_folder <- "./cap3/cropped_maps/"
output_folder <- "./cap3/mapas_empilhados/"

#Inicializar o raster binário e contínuo com o primeiro arquivo para definir a extensão e a resolução
first_raster_binario <- raster(paste0(input_folder, listpouc[1], "_ensemble_binary.tif"))
first_raster_continuo <- raster(paste0(input_folder, listpouc[1], "_ensemble_continuous.tif"))

#Crie rasters vazios com a mesma extensão e resolução para acumular as somas e médias
summed_raster_binario <- raster(first_raster_binario)
summed_raster_continuo <- raster(first_raster_continuo)
summed_raster_binario[] <- 0  # Inicializa todos os valores para zero
summed_raster_continuo[] <- 0  # Inicializa todos os valores para zero

# Contador para rasters contínuos
count_continuo <- 0

#ajustar os breaks de acordo com o número de espécies
breaks2 <- seq(0, 20, by = 3)

#nome padrão para alterar com os gêneros
nome_padrao <- "Sp_pouco_dist_"

# Iterar sobre os nomes das espécies e processar os rasters
for (sp in listpouc) {
  # Caminhos dos arquivos binário e contínuo
  file_path_binario <- paste0(input_folder, sp, "_ensemble_binary.tif")
  file_path_continuo <- paste0(input_folder, sp, "_ensemble_continuous.tif")
  
  # Ler os rasters binário e contínuo
  current_raster_binario <- raster(file_path_binario)
  current_raster_continuo <- raster(file_path_continuo)
  
  # Somar o raster binário ao acumulado
  summed_raster_binario <- summed_raster_binario + current_raster_binario
  
  # Somar o raster contínuo ao acumulado e incrementar o contador
  summed_raster_continuo <- summed_raster_continuo + current_raster_continuo
  count_continuo <- count_continuo + 1
}

# Salvar o raster binário somado
writeRaster(summed_raster_binario, filename = paste0(output_folder, nome_padrao, "_binary.tif"), format = "GTiff")

# Calcular a média do raster contínuo
mean_raster_continuo <- summed_raster_continuo / count_continuo

# Salvar o raster contínuo (média)
writeRaster(mean_raster_continuo, filename = paste0(output_folder, nome_padrao, "_continuous.tif"), format = "GTiff")

# Gerar o mapa binário somado
mapsomado_binario <- tm_shape(summed_raster_binario) +
  tm_raster(palette = "PuBu",
            style = "cont",
            alpha = 0.8,
            breaks = breaks2,
            title = "Species Richness") +
  tm_layout(frame.lwd = 3,
            legend.position = c("left", "top")) +
  tm_scale_bar(position = c("right", "top"),
               width = 0.15,
               color.dark = "gray44")

# Adicionar shapefiles ao mapa binário
mapsomado_binario <- mapsomado_binario +
  tm_shape(MA) +
  tm_borders(lwd = 1.2, col = "gray60") +
  tm_shape(BR) +
  tm_borders(lwd = 0.5, col = "gray44")

# Salvar o mapa binário
tmap_save(
  tm = mapsomado_binario, 
  filename = paste0(output_folder, nome_padrao, "binary_mapa.png"), 
  width = 3000, 
  height = 2800
)

# Gerar o mapa contínuo (média)
mapsomado_continuo <- tm_shape(mean_raster_continuo) +
  tm_raster(palette = "PuBu",
            style = "cont",
            alpha = 0.8,
            breaks = breaks,
            title = "Species Richness") +
  tm_layout(frame.lwd = 3,
            legend.position = c("left", "top")) +
  tm_scale_bar(position = c("right", "top"),
               width = 0.15,
               color.dark = "gray44")

# Adicionar shapefiles ao mapa contínuo
mapsomado_continuo <- mapsomado_continuo +
  tm_shape(MA) +
  tm_borders(lwd = 1.2, col = "gray60") +
  tm_shape(BR) +
  tm_borders(lwd = 0.5, col = "gray44")

# Salvar o mapa contínuo
tmap_save(
  tm = mapsomado_continuo, 
  filename = paste0(output_folder, nome_padrao, "continuous_mapa.png"), 
  width = 3000, 
  height = 2800
)

#MAPAS MUITO DIST -----------------------
#selecionar quais espécies usará em qual mapa
muitodis <- dadosmodelos %>%
  filter(TPR > 0.5) %>%
  filter(proporcao_area >= 70) %>%
  dplyr::select(nome_cientifico)


#criando a lista de espécies necessárias para cada mapa
listmuito <- c("Helicobia aurescens",
              "Oxysarcodexia avuncula",
              "Oxysarcodexia culmiforceps",
              "Oxysarcodexia thornax",
              "Peckia (Sarcodexia) florencioi",
              "Ravinia advena")


#Diretórios
input_folder <- "./cap3/cropped_maps/"
output_folder <- "./cap3/mapas_empilhados/"

#Inicializar o raster binário e contínuo com o primeiro arquivo para definir a extensão e a resolução
first_raster_binario <- raster(paste0(input_folder, listmuito[1], "_ensemble_binary.tif"))
first_raster_continuo <- raster(paste0(input_folder, listmuito[1], "_ensemble_continuous.tif"))

#Crie rasters vazios com a mesma extensão e resolução para acumular as somas e médias
summed_raster_binario <- raster(first_raster_binario)
summed_raster_continuo <- raster(first_raster_continuo)
summed_raster_binario[] <- 0  # Inicializa todos os valores para zero
summed_raster_continuo[] <- 0  # Inicializa todos os valores para zero

# Contador para rasters contínuos
count_continuo <- 0

#ajustar os breaks de acordo com o número de espécies
breaks2 <- seq(0, 7, by = 2)

#nome padrão para alterar com os gêneros
nome_padrao <- "Sp_muito_dist_"

# Iterar sobre os nomes das espécies e processar os rasters
for (sp in listmuito) {
  # Caminhos dos arquivos binário e contínuo
  file_path_binario <- paste0(input_folder, sp, "_ensemble_binary.tif")
  file_path_continuo <- paste0(input_folder, sp, "_ensemble_continuous.tif")
  
  # Ler os rasters binário e contínuo
  current_raster_binario <- raster(file_path_binario)
  current_raster_continuo <- raster(file_path_continuo)
  
  # Somar o raster binário ao acumulado
  summed_raster_binario <- summed_raster_binario + current_raster_binario
  
  # Somar o raster contínuo ao acumulado e incrementar o contador
  summed_raster_continuo <- summed_raster_continuo + current_raster_continuo
  count_continuo <- count_continuo + 1
}

# Salvar o raster binário somado
writeRaster(summed_raster_binario, filename = paste0(output_folder, nome_padrao, "_binary.tif"), format = "GTiff")

# Calcular a média do raster contínuo
mean_raster_continuo <- summed_raster_continuo / count_continuo

# Salvar o raster contínuo (média)
writeRaster(mean_raster_continuo, filename = paste0(output_folder, nome_padrao, "_continuous.tif"), format = "GTiff")

# Gerar o mapa binário somado
mapsomado_binario <- tm_shape(summed_raster_binario) +
  tm_raster(palette = "PuBu",
            style = "cont",
            alpha = 0.8,
            breaks = breaks2,
            title = "Species Richness") +
  tm_layout(frame.lwd = 3,
            legend.position = c("left", "top")) +
  tm_scale_bar(position = c("right", "top"),
               width = 0.15,
               color.dark = "gray44")

# Adicionar shapefiles ao mapa binário
mapsomado_binario <- mapsomado_binario +
  tm_shape(MA) +
  tm_borders(lwd = 1.2, col = "gray60") +
  tm_shape(BR) +
  tm_borders(lwd = 0.5, col = "gray44")

# Salvar o mapa binário
tmap_save(
  tm = mapsomado_binario, 
  filename = paste0(output_folder, nome_padrao, "binary_mapa.png"), 
  width = 3000, 
  height = 2800
)

# Gerar o mapa contínuo (média)
mapsomado_continuo <- tm_shape(mean_raster_continuo) +
  tm_raster(palette = "PuBu",
            style = "cont",
            alpha = 0.8,
            breaks = breaks,
            title = "Species Richness") +
  tm_layout(frame.lwd = 3,
            legend.position = c("left", "top")) +
  tm_scale_bar(position = c("right", "top"),
               width = 0.15,
               color.dark = "gray44")

# Adicionar shapefiles ao mapa contínuo
mapsomado_continuo <- mapsomado_continuo +
  tm_shape(MA) +
  tm_borders(lwd = 1.2, col = "gray60") +
  tm_shape(BR) +
  tm_borders(lwd = 0.5, col = "gray44")

# Salvar o mapa contínuo
tmap_save(
  tm = mapsomado_continuo, 
  filename = paste0(output_folder, nome_padrao, "continuous_mapa.png"), 
  width = 3000, 
  height = 2800
)