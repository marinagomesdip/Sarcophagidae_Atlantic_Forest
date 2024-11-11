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

#Selecionar apenas o bioma-alvo (MA) ------------------
MA <- mma %>%
  filter(code_biome == 4)

#Criando as listas que vou gerar os mapas -------

study_sp <- c("Argoravinia aurea",
              "Argoravinia rufiventris",
              "Blaesoxipha (Gigantotheca) plinthopyga",
              "Chrysagria duodecimpunctata",
              "Dexosarcophaga (Bezzisca) ampullula",
              "Dexosarcophaga (Dexosarcophaga) transita",
              "Dexosarcophaga (Farrimyia) carvalhoi",
              "Dexosarcophaga (Farrimyia) globulosa",
              "Engelimyia inops",
              "Helicobia aurescens",
              "Helicobia morionella",
              "Helicobia pilifera",
              "Helicobia rapax",
              "Lepidodexia (Notochaeta) cognata",
              "Lepidodexia (Orodexia) opima",
              "Lipoptilocnema crispina",
              "Lipoptilocnema crispula",
              "Lipoptilocnema lanei",
              "Microcerella analis",
              "Microcerella halli",
              "Oxysarcodexia admixta",
              "Oxysarcodexia amorosa",
              "Oxysarcodexia angrensis",
              "Oxysarcodexia aura",
              "Oxysarcodexia avuncula",
              "Oxysarcodexia bakeri",
              "Oxysarcodexia carvalhoi",
              "Oxysarcodexia confusa",
              "Oxysarcodexia culmiforceps",
              "Oxysarcodexia culminata",
              "Oxysarcodexia diana",
              "Oxysarcodexia fluminensis",
              "Oxysarcodexia fringidea",
              "Oxysarcodexia injuncta",
              "Oxysarcodexia intona",
              "Oxysarcodexia major",
              "Oxysarcodexia modesta",
              "Oxysarcodexia occulta",
              "Oxysarcodexia parva",
              "Oxysarcodexia riograndensis",
              "Oxysarcodexia paulistanensis",
              "Oxysarcodexia terminalis",
              "Oxysarcodexia thornax",
              "Oxysarcodexia timida",
              "Oxysarcodexia varia",
              "Oxysarcodexia xanthosoma",
              "Peckia (Euboettcheria) anguilla",
              "Peckia (Euboettcheria) australis",
              "Peckia (Euboettcheria) collusor",
              "Peckia (Euboettcheria) subducta",
              "Peckia (Pattonella) intermutans",
              "Peckia (Pattonella) resona",
              "Peckia (Pattonella) smarti",
              "Peckia (Peckia) chrysostoma",
              "Peckia (Peckia) pexata",
              "Peckia (Peckia) uncinata",
              "Peckia (Sarcodexia) florencioi",
              "Peckia (Sarcodexia) lambens",
              "Peckia (Squamatodes) ingens",
              "Peckia (Squamatodes) trivittata",
              "Peckiamyia expuncta",
              "Ravinia advena",
              "Ravinia belforti",
              "Retrocitomyia retrocita",
              "Sarcofahrtiopsis cuneata",
              "Titanogrypa (Cucullomyia) larvicida",
              "Titanogrypa (Cucullomyia) luculenta",
              "Tricharaea (Sarcophagula) canuta",
              "Tricharaea (Sarcophagula) occidua",
              "Tricharaea (Sarothromyia) femoralis",
              "Tricharaea (Tricharaea) brevicornis",
              "Udamopyga percita",
              "Villegasia almeidai")


#Criando as escalas gráficas padronizadas -----------------------------
# Limites para mapas de 0 a 1
breaks <- seq(0.00, 1, by = 0.1)
#Limites para mapas de 0 a 55
breaks2 <- seq(0, 60, by = 10)

#Cortar mapas com a extensão da MA -------------------

# name a folder to save outputs of this run
base_path <- "./cap3/cropped_maps/"

for(i in 1:length(study_sp)){
  
  #read the ensemble continuos model of target species
  cont <- raster(paste0("C:/Users/Dell/OneDrive/Área de Trabalho/cap3/", study_sp[i], "/present/ensemble/", study_sp[i], "_ensemble_average.tif"))
  
  #read the ensemble continuos model of target species
  contun <- raster(paste0("C:/Users/Dell/OneDrive/Área de Trabalho/cap3/", study_sp[i], "/present/ensemble/", study_sp[i], "_uncertainty.tif"))
  
  #read the ensemble continuos model of target species
  bin <- raster(paste0("C:/Users/Dell/OneDrive/Área de Trabalho/cap3/", study_sp[i], "/present/ensemble_2/", study_sp[i], "_ensemble_0.7_consensus.tif"))

  #read the ensemble continuos model of target species
  binun <- raster(paste0("C:/Users/Dell/OneDrive/Área de Trabalho/cap3/", study_sp[i], "/present/ensemble_2/", study_sp[i], "_uncertainty.tif"))
  
  #Convertendo o CRS dos mapas 
  cont <- projectRaster(cont, crs = 4674, method = "ngb")
  contun <- projectRaster(contun, crs = 4674, method = "ngb")
  bin <- projectRaster(bin, crs = 4674, method = "ngb")
  binun <- projectRaster(binun, crs = 4674, method = "ngb")
  
  #Cortando os rasters para a extensão do shapefile 'MA'
  cont <- raster::crop(cont, extent(MA))
  contun <- raster::crop(contun, extent(MA))
  bin <- raster::crop(bin, extent(MA))
  binun <- raster::crop(binun, extent(MA))
  
  #Aplicando a mascara da MA para todos
  cont <- raster::mask(cont, MA)
  contun <- raster::mask(contun, MA)
  bin <- raster::mask(bin, MA)
  binun <- raster::mask(binun, MA)
  
  #exportar em formato raster
  #saving raster
  writeRaster(cont, filename = paste0(base_path, study_sp[i], "_ensemble_continuous.tif"), format = "GTiff")
  writeRaster(contun, filename = paste0(base_path, study_sp[i], "_ensemble_continuous_uncertanty.tif"), format = "GTiff")
  writeRaster(bin, filename = paste0(base_path, study_sp[i], "_ensemble_binary.tif"), format = "GTiff")
  writeRaster(binun, filename = paste0(base_path, study_sp[i], "_ensemble_binary_uncerntanty.tif"), format = "GTiff")

  #gerar mapas .png com esses rasters
  
  # Crie os mapas com a mesma paleta e limites
  mapcont <- tm_shape(cont) +
    tm_raster(palette = "PuBu",
              style = "cont",
              alpha = 0.8,
              breaks = breaks,
              title = "Suitability") +
    tm_layout(frame.lwd = 3,
              legend.position = c("left", "top")) +
    tm_scale_bar(position = c("right", "top"),
                 width = 0.15,
                 color.dark = "gray44")
  mapcont <- mapcont +
    tm_shape(MA) +
    tm_borders(lwd = 1.2,
               col = "gray60")
  
  mapcont <- mapcont +
    tm_shape(BR) +
    tm_borders(lwd = 0.5,
               col = "gray44")
  
  mapcontun <- tm_shape(contun) +
    tm_raster(palette = "Oranges",
              style = "cont",
              alpha = 0.8,
              breaks = breaks,
              title = "Uncertainty") +
    tm_layout(frame.lwd = 3,
              legend.position = c("left", "top")) +
    tm_scale_bar(position = c("right", "top"),
                 width = 0.15,
                 color.dark = "gray44")
  
  mapcontun <- mapcontun +
    tm_shape(MA) +
    tm_borders(lwd = 1.2,
               col = "gray60")
  
  mapcontun <- mapcontun +
    tm_shape(BR) +
    tm_borders(lwd = 0.5,
               col = "gray44")
  
  mapbin <- tm_shape(bin) +
    tm_raster(palette = "PuBu",
              style = "cat",
              alpha = 0.8,
              breaks = breaks,
              title = "Suitability") +
    tm_layout(frame.lwd = 3,
              legend.position = c("left", "top")) +
    tm_scale_bar(position = c("right", "top"),
                 width = 0.15,
                 color.dark = "gray44")
  
  mapbin <- mapbin +
    tm_shape(MA) +
    tm_borders(lwd = 1.2,
               col = "gray60")
  
  mapbin <- mapbin +
    tm_shape(BR) +
    tm_borders(lwd = 0.5,
               col = "gray44")
  
  mapbinun <- tm_shape(binun) +
    tm_raster(palette = "Oranges",
              style = "cont",
              alpha = 0.8,
              breaks = breaks,
              title = "Uncertainty") +
    tm_layout(frame.lwd = 3,
              legend.position = c("left", "top")) +
    tm_scale_bar(position = c("right", "top"),
                 width = 0.15,
                 color.dark = "gray44")
  
  mapbinun <- mapbinun +
    tm_shape(MA) +
    tm_borders(lwd = 1.2,
               col = "gray60")
  
  mapbinun <- mapbinun +
    tm_shape(BR) +
    tm_borders(lwd = 0.5,
               col = "gray44")
  
  #Salvar os mapas
  tmap_save(
    tm = mapcont, 
    filename = paste0(base_path, study_sp[i], "_ensemble_continuous_map.png"), 
    width = 3000, 
    height = 2800
  )
  
  tmap_save(
    tm = mapcontun, 
    filename = paste0(base_path, study_sp[i], "_ensemble_continuous_uncertanty_map.png"), 
    width = 3000, 
    height = 2800
  )
  
  tmap_save(
    tm = mapbin, 
    filename = paste0(base_path, study_sp[i], "_ensemble_binary_map.png"), 
    width = 3000, 
    height = 2800
  )
  
  tmap_save(
    tm = mapbinun, 
    filename = paste0(base_path, study_sp[i], "_ensemble_binary_uncertanty_map.png"), 
    width = 3000, 
    height = 2800
  )
  
}

#Empilhamento simples dos mapas binários (soma) -------------------------

# Inicialize um objeto raster com o primeiro arquivo para definir a extensão e a resolução
first_raster <- raster(paste0("./cap3/cropped_maps/", study_sp[1], "_ensemble_binary.tif"))

# Crie um raster vazio com a mesma extensão e resolução do primeiro raster para acumular as somas
summed_raster <- raster(first_raster)
summed_raster[] <- 0  # Inicializa todos os valores para zero

# Itere sobre os nomes das espécies e some os rasters
for (sp in study_sp) {
  # Construa o caminho do arquivo para o raster atual
  file_path <- paste0("./cap3/cropped_maps/", sp, "_ensemble_binary.tif")
  
  # Leia o raster atual
  current_raster <- raster(file_path)
  
  # Some o raster atual ao raster acumulado
  summed_raster <- summed_raster + current_raster
}

#salvando o raster gerado
writeRaster(summed_raster, filename = "./cap3/mapas_empilhados/MA_geral_binary.tif", format = "GTiff")

#gerando o mapa
mapsomado <- tm_shape(summed_raster) +
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
mapsomado <- mapsomado +
  tm_shape(MA) +
  tm_borders(lwd = 1.2,
             col = "gray60")

mapsomado <- mapsomado +
  tm_shape(BR) +
  tm_borders(lwd = 0.5,
             col = "gray44")

tmap_save(
  tm = mapsomado, 
  filename = "./cap3/mapas_empilhados/MA_geral_binary_mapa.png", 
  width = 3000, 
  height = 2800
)

#Empilhamento simples dos mapas contínos (média) --------------------

# Inicialize um objeto raster com o primeiro arquivo para definir a extensão e a resolução
first_raster <- raster(paste0("./cap3/cropped_maps/", study_sp[1], "_ensemble_continuous.tif"))

# Crie um raster vazio com a mesma extensão e resolução do primeiro raster para acumular os valores
summed_raster <- raster(first_raster)
summed_raster[] <- 0  # Inicializa todos os valores para zero

# Contador para manter o controle do número de rasters somados
count <- 0

# Itere sobre os nomes das espécies e some os rasters
for (sp in study_sp) {
  # Construa o caminho do arquivo para o raster atual
  file_path <- paste0("./cap3/cropped_maps/", sp, "_ensemble_continuous.tif")
  
  # Leia o raster atual
  current_raster <- raster(file_path)
  
  # Some o raster atual ao raster acumulado
  summed_raster <- summed_raster + current_raster
  
  # Incremente o contador
  count <- count + 1
}

# Calcule a média dividindo o raster somado pelo número de rasters
mean_raster <- summed_raster / count

#exportando o raster gerado
writeRaster(mean_raster, filename = "./cap3/mapas_empilhados/MA_geral_continuous.tif", format = "GTiff")

#gerando o mapa
mapmedia <- tm_shape(mean_raster) +
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
mapmedia <- mapmedia +
  tm_shape(MA) +
  tm_borders(lwd = 1.2,
             col = "gray60")

mapmedia <- mapmedia +
  tm_shape(BR) +
  tm_borders(lwd = 0.5,
             col = "gray44")

tmap_save(
  tm = mapmedia, 
  filename = "./cap3/mapas_empilhados/MA_geral_continuous_mapa.png", 
  width = 3000, 
  height = 2800
)
