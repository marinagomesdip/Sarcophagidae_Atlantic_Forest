#Chapter three - thesis 
#Atlantic Forest - Uncertainty in Ensemble models
#Marina Morim Gomes 2024
#gomes.mari.95@gmail.com

#loading packages -------------------------------------
library(tidyverse)
library(raster)

#loading data ----------------------------------------

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

#Creating uncertainty rasters with only the algorithms used -------------

#Looping to calculate uncertainty of continuous models with max - min (ranges 0-1)
#where 0 perfect agree and 1 totaly desagramment

for(i in 1:length(study_sp)){
  print(paste("Processando:", study_sp[i]))
  
  #Path to species folder
  caminho <- paste0("./cap3/", study_sp[i], "/present/")
  
  #Path to the file with algorithm names 
  caminho_alg <- paste0(caminho, "ensemble/")
  
  #Path to folder with models per algorithm
  caminho_mod <- paste0(caminho, "final_models/")
  
  # Read the metadata file as a CSV
  metadata <- read.csv(paste0(caminho_alg, "metadata.csv"))
  
  # Extract the 'algorithm' column and split by dashes
  algorithms <- strsplit(as.character(metadata$algorithm), "-")[[1]]
  
  # Initialize an empty list to store the raster files
  rasters_list <- list()
  
  # Loop through each algorithm to find and read the corresponding raster
  for(alg in algorithms) {
    # Construct the path to the raster file for each algorithm
    raster_file <- paste0(caminho_mod, study_sp[i], "_", alg, "_raw_mean.tif")
    
    # Check if the raster file exists before reading it
    if (file.exists(raster_file)) {
      # Read the raster file
      rasters_list[[alg]] <- raster(raster_file)
      
      # Print a message indicating the raster was read successfully
      print(paste("Lido o arquivo raster para o algoritmo:", alg))
    } else {
      print(paste("Arquivo raster não encontrado para o algoritmo:", alg))
    }
  }
  
  # Check if we have at least two rasters to compare
  if (length(rasters_list) > 1) {
    # Stack the rasters for calculation
    raster_stack <- stack(rasters_list)
    
    # Calculate the range (max - min) for each cell
    uncertainty_raster <- calc(raster_stack, fun = function(x) max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
    
    # Save the uncertainty raster to file
    output_file <- paste0(caminho_alg, "/calculated_uncertainty.tif")
    writeRaster(uncertainty_raster, filename = output_file, format = "GTiff", overwrite = TRUE)
    
    # Print message indicating the uncertainty raster was created
    print(paste("Raster de incerteza criado e salvo como:", output_file))
  } else {
    print("Não há rasters suficientes para calcular a incerteza.")
    
  }
}

# Loop to create the "histogram" raster that counts the number of presences (1) per cell
for(i in 1:length(study_sp)){
  print(paste("Processando:", study_sp[i]))
  
  # Path to species folder
  caminho <- paste0("./cap3/", study_sp[i], "/present/")
  
  #Path to the file with algorithm names 
  caminho_alg <- paste0(caminho, "ensemble/")
  
  # Read the metadata file as a CSV
  metadata <- read.csv(paste0(caminho_alg, "metadata.csv"))
  
  # Extract the 'algorithm' column and split by dashes
  algorithms <- strsplit(as.character(metadata$algorithm), "-")[[1]]
  
  # Print algorithms to check
  print(algorithms)
  
  # Path to folder with models per algorithm
  caminho_mod <- paste0(caminho, "final_models/")
  
  # Initialize an empty list to store the raster files
  rasters_list <- list()
  
  # Loop through each algorithm to find and read the corresponding raster
  for(alg in algorithms) {
    # Construct the path to the raster file for each algorithm
    raster_file <- paste0(caminho_mod, study_sp[i], "_", alg, "_raw_mean_th.tif")
    
    # Check if the raster file exists before reading it
    if (file.exists(raster_file)) {
      # Read the raster file
      rasters_list[[alg]] <- raster(raster_file)
      
      # Print a message indicating the raster was read successfully
      print(paste("Lido o arquivo raster para o algoritmo:", alg))
    } else {
      print(paste("Arquivo raster não encontrado para o algoritmo:", alg))
    }
  }
  
  # Check if we have at least two rasters to compare
  if (length(rasters_list) > 1) {
    # Stack the rasters for calculation
    raster_stack <- stack(rasters_list)
    
    # Calculate the number of rasters with presence (value == 1) per cell
    presence_count_raster <- calc(raster_stack, fun = function(x) {
      sum(x == 1, na.rm = TRUE)  # Count how many rasters have value == 1
    })
    
    # Save the uncertainty raster to file
    output_file <- paste0(caminho_alg, "/raster_histogram.tif")
    writeRaster(presence_count_raster, filename = output_file, format = "GTiff", overwrite = TRUE)
    
    # Print message indicating the presence count raster was created
    print(paste("Raster de contagem de presenças criado e salvo como:", output_file))
  } else {
    print("Não há rasters suficientes para calcular a contagem de presenças.")
  }
}