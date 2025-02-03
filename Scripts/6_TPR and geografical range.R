#Chapter three - thesis 
#Atlantic Forest - Modelling
#Marina Morim Gomes 2024
#gomes.mari.95@gmail.com

#loading packages -------------------------------------
library(tidyverse)
library(raster)
library(sf)
library(geobr)

#loading data ----------------------------------------
occ <- read.csv("./Data/Processados/7_para_rodar_modelagem.csv")

#Species list ----------------------------------------
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


#to calculate TPR --------------------------------------------------------------
# Create an empty data frame to store the results
df_tpr <- data.frame(nome_cientifico = character(), TPR = numeric(), stringsAsFactors = FALSE)

for(i in 1:length(study_sp)){
  
  # IMPORTING TARGET SPECIES RASTER
  # path to file
  caminho_arquivo <- paste0("C:/Users/Dell/OneDrive/Área de Trabalho/cap3/", study_sp[i], 
                            "/present/ensemble_2/", study_sp[i], "_ensemble_0.7_consensus.tif")
  
  # read the raster file
  ras <- raster(caminho_arquivo)
  
  # FILTER OCCURRENCE DATA FOR THE TARGET SPECIES
  occal <- occ %>%
    filter(nome_cientifico == study_sp[i])
  
  # TRANSFORM COORDINATES INTO A SPATIAL OBJECT
  occal_sf <- occal %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = crs(ras), remove = FALSE)
  
  # EXTRACT RASTER VALUES FOR THE OCCURRENCES
  valores_raster <- raster::extract(ras, st_coordinates(occal_sf))
  
  # ADD THE EXTRACTED VALUES TO THE DATA FRAME
  occal$valor_raster <- valores_raster
  
  # CALCULATE TPR
  # Count the number of occurrences with raster value = 1
  ocorrencias_valor_1 <- sum(occal$valor_raster == 1, na.rm = TRUE)
  
  # Total number of occurrences
  total_ocorrencias <- nrow(occal)
  
  # Calculate TPR (occurrences with value 1 / total occurrences)
  tpr <- ocorrencias_valor_1 / total_ocorrencias
  
  # ADD THE RESULT TO THE FINAL DATA FRAME
  df_tpr <- rbind(df_tpr, data.frame(nome_cientifico = study_sp[i], TPR = tpr))
}


#to calculate % of predicted area -----------------------------------------------------

# Create an empty data frame to store the results
df_georan <- data.frame(nome_cientifico = character(), proporcao_area = numeric(), stringsAsFactors = FALSE)

# To get the Atlantic Forest shapefile
mata_atlantica <- geobr::read_biomes(year = 2019, showProgress = FALSE) %>%
  filter(code_biome == 4)

# Iterate over the species
for(i in 1:length(study_sp)){
  
  # IMPORTING TARGET SPECIES RASTER
  caminho_arquivo <- paste0("C:/Users/Dell/OneDrive/Área de Trabalho/cap3/", study_sp[i], 
                            "/present/ensemble_2/", study_sp[i], "_ensemble_0.7_consensus.tif")
  
  ras <- raster(caminho_arquivo)
  
  # Reproject the Atlantic Forest mask to match the raster CRS
  mata_atlantica_proj <- st_transform(mata_atlantica, crs = crs(ras))
  
  # Convert to Spatial object to apply the mask
  mata_atlantica_sp <- as(mata_atlantica_proj, "Spatial")
  
  # Apply the mask to the raster
  ras_masked <- mask(ras, mata_atlantica_sp)
  
  # Count the total number of cells within the masked area (excluding NA cells)
  total_cells_within_mask <- sum(!is.na(values(ras_masked)))
  
  # Count the number of cells with value 1 within the masked area
  cells_with_1 <- sum(values(ras_masked) == 1, na.rm = TRUE)
  
  # Calculate the proportion of cells with value 1 relative to the total within the mask
  proporcao <- (cells_with_1 / total_cells_within_mask) * 100
  
  # ADD THE RESULT TO THE FINAL DATA FRAME
  df_georan <- rbind(df_georan, data.frame(nome_cientifico = study_sp[i], proporcao_area = proporcao))
}

#Extracting values for the Supplementary Material for all species -------------------------

# Create an empty data frame to store the results
df_metrics <- data.frame(nome_cientifico = character(), N = numeric(), meanAUC = numeric(), meanTSS = numeric(), stringsAsFactors = FALSE)

for(i in 1:length(study_sp)){
  # IMPORTING METADATA TO EXTRACT THE N OF THE TARGET SPECIES
  caminho_arquivo <- paste0("C:/Users/Dell/OneDrive/Área de Trabalho/cap3/", study_sp[i], 
                            "/present/data_setup/metadata.csv")
  
  metada <- read.csv(caminho_arquivo)
  
  # Select only the N column
  metada <- metada %>%
    dplyr::select(original.n)
  
  # IMPORTING METRICS TO EXTRACT THE MEAN AUC AND MEAN TSS
  caminho_arquivo <- paste0("C:/Users/Dell/OneDrive/Área de Trabalho/cap3/", study_sp[i], 
                            "/present/final_models/", study_sp[i], "_mean_statistics_original.csv")
  
  metric <- read.csv(caminho_arquivo)
  
  # Calculate the mean AUC
  auc <- mean(metric$AUC, na.rm = TRUE)
  
  # Calculate the mean TSS
  tss <- mean(metric$TSSmax, na.rm = TRUE)
  
  # ADD THE RESULT TO THE FINAL DATA FRAME
  df_metrics <- rbind(df_metrics, data.frame(nome_cientifico = study_sp[i], N = metada, meanAUC = auc, meanTSS = tss))
}


#Combining everything into a single DF -------------------------------

united <- left_join(df_tpr, df_georan, by = "nome_cientifico")

united <- left_join(united, df_metrics, by = "nome_cientifico")

#exporting data--------------------------------------------------
#Saving metadata
write_csv(united,"./Data/Processados/7_Suplementar_Material5.csv")

#histograms ------------------------------------------------------

tprgra <- ggplot(united, aes(x = TPR)) +
  geom_histogram(binwidth = 0.1,
                 fill = "gray28",
                 color = "white") +
  theme_classic() +
  labs(x = "TPR", y = "Count")

ggsave(filename = "./cap3/graphs/tpr.png",
       plot = tprgra, device = "png", width = 6, height = 4, dpi = 300)

propor <- ggplot(united, aes(x = proporcao_area)) +
  geom_histogram(binwidth = 10,
                 fill = "gray28",
                 color = "white") +
  theme_classic() +
  labs(x = "Area predicted proportion", y = "Count")

ggsave(filename = "./cap3/graphs/proportion.png",
       plot = propor, device = "png", width = 6, height = 4, dpi = 300)