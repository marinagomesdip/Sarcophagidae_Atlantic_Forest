#Chapter three - thesis 
#Atlantic Forest - Geral Maps
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

#Selecting only Atlantic Forest in IBGE shapefile ------------------
MA <- mma %>%
  filter(code_biome == 4)

#Species list to generate maps -------
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


# Standardize scales of legend between continous and binary-----------------------------

#Scale for binary maps (0 - 1)
breaks <- seq(0.00, 1, by = 0.1)

#Scale for continous maps (0 - 60)
breaks2 <- seq(0, 60, by = 10)

#Scale for histogram maps (0 - 8)
breaks3 <- seq(0, 8, by = 2)

#Cropping rasters with AF extention -------------------

#Name a folder to save outputs of this run
base_path <- "./cap3/cropped_maps/"

for(i in 1:length(study_sp)){
  
  #Read files
  #read the ensemble continuos model of target species
  cont <- raster(paste0("C:/Users/Dell/OneDrive/Área de Trabalho/cap3/", study_sp[i], "/present/ensemble/", study_sp[i], "_ensemble_average.tif"))
  #read the uncertainty of ensemble continuous model of target species
  contun <- raster(paste0("C:/Users/Dell/OneDrive/Área de Trabalho/cap3/", study_sp[i], "/present/ensemble/calculated_uncertainty.tif"))
  #read the ensemble binary model of target species
  bin <- raster(paste0("C:/Users/Dell/OneDrive/Área de Trabalho/cap3/", study_sp[i], "/present/ensemble_2/", study_sp[i], "_ensemble_0.7_consensus.tif"))
  #read the 'histogram' model 
  his <- raster(paste0("C:/Users/Dell/OneDrive/Área de Trabalho/cap3/", study_sp[i], "/present/ensemble/raster_histogram.tif"))
  
  #Transforming the CRS of maps to SIRGAS 2000 (South America)
  cont <- projectRaster(cont, crs = 4674, method = "ngb")
  contun <- projectRaster(contun, crs = 4674, method = "ngb")
  bin <- projectRaster(bin, crs = 4674, method = "ngb")
  his <- projectRaster(his, crs = 4674, method = "ngb")
  
  #Croping rasters for shapefile of 'AF' 
  cont <- raster::crop(cont, extent(MA))
  contun <- raster::crop(contun, extent(MA))
  bin <- raster::crop(bin, extent(MA))
  his <- raster::crop(his, extent(MA))
  
  #Using a mask in rasters with shapefile of 'AF' 
  cont <- raster::mask(cont, MA)
  contun <- raster::mask(contun, MA)
  bin <- raster::mask(bin, MA)
  his <- raster::mask(his, MA)
  
  #Exporting in raster file
  writeRaster(cont, filename = paste0(base_path, study_sp[i], "_ensemble_continuous.tif"), format = "GTiff")
  writeRaster(contun, filename = paste0(base_path, study_sp[i], "_uncertanty.tif"), format = "GTiff")
  writeRaster(bin, filename = paste0(base_path, study_sp[i], "_ensemble_binary.tif"), format = "GTiff")
  writeRaster(his, filename = paste0(base_path, study_sp[i], "_histogram.tif"), format = "GTiff")

  #Generating maps with tmap to use in Suplementar Material Appendix S4 

  #continuous map
  mapcont <- tm_shape(cont) +                             #selecting the raster object
    tm_raster(palette = "PuBu",                           #Using a blue continuous palette
              style = "cont",                             #using the style for continuous scales
              alpha = 0.8,                                #transparency, to make soft colors
              breaks = breaks,                            #using the scale that we set
              title = "Suitability") +                    #title of legend
    tm_layout(frame.lwd = 3,                              #size of legend
              legend.position = c("left", "top")) +       #position of legend
    tm_scale_bar(position = c("right", "top"),            #position of scale bar of map
                 width = 0.15,
                 color.dark = "gray44")                   #color of scale bar of map
  
  mapcont <- mapcont +                                    #Using the previous map to add AF 
    tm_shape(MA) +                                        #selecting the shp of AF
    tm_borders(lwd = 1.2,                                 #adding borders with 1.2 width 
               col = "gray60")                            #selecting the color of borders
  
  mapcont <- mapcont +                                    #using the previous map to add BR limits
    tm_shape(BR) +                                        #selecting the shp of BR
    tm_borders(lwd = 0.5,                                 #adding borders with 0.5 width
               col = "gray44")                            #selecting the color of borders

  #Repeat the process, with little changes of all maps            
  mapcontun <- tm_shape(contun) +
    tm_raster(palette = "Oranges",                        #Using a orange palette for uncertainty maps
              style = "cont",
              alpha = 0.8,
              breaks = breaks,
              title = "Uncertainty") +                    #changing the name of legend
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
              style = "cat",                            #using the style 'cat' for binary maps, to categorize the legend                            
              alpha = 0.8,
              breaks = breaks2,                         #using the other scale that we set 
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
  
  maphis <- tm_shape(his) +
    tm_raster(palette = "PuBu",
              style = "cont",
              alpha = 0.8,
              breaks = breaks3,
              title = "Algorithm Frequency") +
    tm_layout(frame.lwd = 3,
              legend.position = c("left", "top")) +
    tm_scale_bar(position = c("right", "top"),
                 width = 0.15,
                 color.dark = "gray44")
  
  maphis <- maphis +
    tm_shape(MA) +
    tm_borders(lwd = 1.2,
               col = "gray60")
  
  maphis <- maphis +
    tm_shape(BR) +
    tm_borders(lwd = 0.5,
               col = "gray44")
  
  #Export all maps 
  tmap_save(
    tm = mapcont,                                                                 #object that we want to save
    filename = paste0(base_path, study_sp[i], "_ensemble_continuous_map.png"),    #path to save and name of file
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
    tm = maphis, 
    filename = paste0(base_path, study_sp[i], "_histogram_map.png"), 
    width = 3000, 
    height = 2800
  )
  
}

#New species list (TPR > 0.5)

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
              "Lepidodexia (Notochaeta) cognata",
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
              "Oxysarcodexia parva",
              "Oxysarcodexia riograndensis",
              "Oxysarcodexia paulistanensis",
              "Oxysarcodexia terminalis",
              "Oxysarcodexia thornax",
              "Oxysarcodexia timida",
              "Oxysarcodexia varia",
              "Peckia (Euboettcheria) anguilla",
              "Peckia (Euboettcheria) collusor",
              "Peckia (Euboettcheria) subducta",
              "Peckia (Pattonella) intermutans",
              "Peckia (Pattonella) resona",
              "Peckia (Peckia) chrysostoma",
              "Peckia (Peckia) pexata",
              "Peckia (Peckia) uncinata",
              "Peckia (Sarcodexia) florencioi",
              "Peckia (Sarcodexia) lambens",
              "Peckia (Squamatodes) ingens",
              "Peckia (Squamatodes) trivittata",
              "Ravinia advena",
              "Ravinia belforti",
              "Retrocitomyia retrocita",
              "Titanogrypa (Cucullomyia) larvicida",
              "Titanogrypa (Cucullomyia) luculenta",
              "Tricharaea (Sarcophagula) canuta",
              "Tricharaea (Sarcophagula) occidua",
              "Tricharaea (Sarothromyia) femoralis",
              "Tricharaea (Tricharaea) brevicornis",
              "Udamopyga percita",
              "Villegasia almeidai")



#Stacking binary maps to look for macroecological patterns in AF (sum) -------------------------

# Initialize a raster object with the first file to set the extension and resolution
first_raster <- raster(paste0("./cap3/cropped_maps/", study_sp[1], "_ensemble_binary.tif"))

# Create an empty raster with the same extent and resolution as the first raster to accumulate the sums
summed_raster <- raster(first_raster)
summed_raster[] <- 0  # Inicializa todos os valores para zero

# Iterate over the species names and sum the rasters
for (sp in study_sp) {
  # Path to actual raster
  file_path <- paste0("./cap3/cropped_maps/", sp, "_ensemble_binary.tif")
  
  # Import actual raster 
  current_raster <- raster(file_path)
  
  # Add the current raster to the accumulated raster
  summed_raster <- summed_raster + current_raster
}

#Saving the final raster 
writeRaster(summed_raster, filename = "./cap3/mapas_empilhados/MA_geral_binary.tif", format = "GTiff")

#Generating a map with the same arguments of species maps
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

#saving map
tmap_save(
  tm = mapsomado, 
  filename = "./cap3/mapas_empilhados/MA_geral_binary_mapa.png", 
  width = 3000, 
  height = 2800
)