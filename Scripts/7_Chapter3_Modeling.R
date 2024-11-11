#Chapter three - thesis 
#Atlantic Forest - Modelling
#Marina Morim Gomes 2024
#gomes.mari.95@gmail.com

#loading packages -------------------------------------
library(dplyr)
library(readr)
library(modleR)
library(raster)
library(rJava)
library(dismo)
library(sp)
library(sf)
library(rgdal)
library(fs)

#loading data ----------------------------------------
#occurrences
occ <- read.csv("./Data/Processados/7_para_rodar_modelagem.csv")
#pseudoabsences
#abs <- read.csv("./Data/Processados/7_pseudoausencias.csv")
#environment layers
bio1 <- raster("./Enviromental_data/wc2.1_10m_bio_1.tif")
bio2 <- raster("./Enviromental_data/wc2.1_10m_bio_2.tif")
bio3 <- raster("./Enviromental_data/wc2.1_10m_bio_3.tif")
bio4 <- raster("./Enviromental_data/wc2.1_10m_bio_4.tif")
bio5 <- raster("./Enviromental_data/wc2.1_10m_bio_5.tif")
bio6 <- raster("./Enviromental_data/wc2.1_10m_bio_6.tif")
bio7 <- raster("./Enviromental_data/wc2.1_10m_bio_7.tif")
bio10 <- raster("./Enviromental_data/wc2.1_10m_bio_10.tif")
bio11 <- raster("./Enviromental_data/wc2.1_10m_bio_11.tif")
bio12 <- raster("./Enviromental_data/wc2.1_10m_bio_12.tif")
bio13 <- raster("./Enviromental_data/wc2.1_10m_bio_13.tif")
bio14 <- raster("./Enviromental_data/wc2.1_10m_bio_14.tif")
bio15 <- raster("./Enviromental_data/wc2.1_10m_bio_15.tif")
bio16 <- raster("./Enviromental_data/wc2.1_10m_bio_16.tif")
bio17 <- raster("./Enviromental_data/wc2.1_10m_bio_17.tif")
elev  <- raster("./Enviromental_data/wc2.1_10m_elev.tif")

#preparing enviromental data to model --------------

#uniting maps in a rasterStack 
variables <- stack(bio1, bio2, bio3, bio4, bio5, bio6, bio7,bio10,
                   bio11, bio12, bio13, bio14, bio15, bio16, bio17,
                   elev)

#cropping in americas extent
americas_extent <- extent(-115, -30, -60, 30)

variables <- crop(variables, americas_extent) %>%
  stack()

plot(variables$wc2.1_10m_bio_1)

#projections
# Define the target CRS
targetCRS <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")

#Project the raster stack
variables <- projectRaster(variables, crs = targetCRS)

#preparing occurence data to model --------------------------------------
occ <- occ %>%
  rename(lat = latitude) %>%
  rename(lon = longitude) %>%
  rename(species_name = nome_cientifico)

#preparing occurs to loop-------------------------------
# Lista de espécies

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

# name a folder to save outputs of this run
model_folder <- "./cap3"

# modleR 1/5: Setup data --------------------------------------------------

for(i in 1:length(study_sp)){
  # getting only occurrences for this species
  species_df <- occ[occ$species_name == study_sp[i], ]
  
  # getting only pseudoabsences for this species
  #absences_df <- abs[abs$species_name == study_sp[i], ]
  
  # Calculando o número de pseudoausências a serem selecionadas
  #num_absences <- nrow(species_df) * 10
  
  # Selecionando aleatoriamente as pseudoausências
  #if(nrow(absences_df) > num_absences) {
  #  absences_df <- absences_df %>% sample_n(num_absences)
  #}
  
  #para saber qual espécie é 
  print(paste("Processando:", study_sp[i]))
  
  #running setup_sdmdata
  setup_sdmdata(species_name = study_sp[i],
                occurrences = as.data.frame(species_df),
                predictors = variables,
                models_dir = model_folder, # folder to save partitions
                partition_type = "crossvalidation",
                cv_partitions = 5,
                cv_n = 2,
                seed = 512,
                buffer_type = "mean",
                env_filter = TRUE,
                env_distance = "centroid",
                min_env_dist = 0.3,
                n_back = nrow(species_df)*10,
                png_sdmdata = TRUE,
                clean_dupl = TRUE,
                clean_uni = TRUE,
                clean_nas = TRUE,
                geo_filt = FALSE,
                select_variables = TRUE,
                sample_proportion = 0.5,
                cutoff = 0.7)
}

# modleR 2/5: model calibration -------------------------------------------

for(i in 1:length(study_sp)){
  
  # run selected algorithms for each partition
  do_many(species_name = study_sp[i],
          predictors = variables,
          models_dir = model_folder, # folder to save partitions
          png_partitions = TRUE,
          write_bin_cut = FALSE,
          write_rda = TRUE,
          bioclim = TRUE,
          domain = TRUE,
          glm = TRUE,
          svmk = TRUE,
          svme = TRUE,
          maxent = TRUE,
          maxnet = TRUE,
          rf = TRUE,
          mahal = FALSE,
          brt = FALSE,
          equalize = TRUE)
  
}

# modleR 3/5: final models ----------------------------------------

for(i in 1:length(study_sp)){
  
  final_model(species_name = study_sp[i],
              algorithms = NULL,
              models_dir = model_folder,
              scale_models = TRUE, # convert model outputs to 0-1
              which_models = c("raw_mean", "raw_mean_th"), 
              mean_th_par = "spec_sens",
              png_final = TRUE,
              overwrite = TRUE)
  
}

# modleR 4/5: consensus model continuous ----------------------------------------

for(i in 1:length(study_sp)){
  print(paste("Processando:", study_sp[i]))
  
  # Caminho para o arquivo que tem o AUC médio
  caminho <- paste0("./cap3/", study_sp[i], "/present/final_models/", study_sp[i], "_mean_statistics.csv")
  
  # Verificar se o arquivo existe
  if (file.exists(caminho)) {
    # Ler o arquivo
    algorithms_data <- read_csv(caminho)
    
    # Filtrar os algoritmos que obedecem a regra
    species_algorithms <- algorithms_data %>%
      filter(AUC > 0.7) %>%
      dplyr::select(algorithm) %>%
      distinct()
    
    # Converter a coluna de algoritmos em um vetor
    algorithms_vector <- as.character(species_algorithms$algorithm)
    
    # Verificar se há mais de um algoritmo
    if (length(algorithms_vector) > 1) {
      # Renomear o arquivo original
      caminho_original <- paste0("./cap3/", study_sp[i], "/present/final_models/", study_sp[i], "_mean_statistics_original.csv")
      file_move(caminho, caminho_original)
      print(paste("Arquivo original renomeado para", caminho_original))
      
      # Filtrar o dataframe original para incluir apenas os algoritmos no vetor
      filtered_algorithms_data <- algorithms_data %>%
        filter(algorithm %in% algorithms_vector)
      
      # Salvar o novo arquivo filtrado
      write_csv(filtered_algorithms_data, caminho)
      print(paste("Novo arquivo filtrado salvo como", caminho))
      
      # Chamar a função ensemble_model
      ensemble_model(species_name = study_sp[i],
                     occurrences = as.data.frame(species_df),
                     algorithms = algorithms_vector,
                     which_final = "raw_mean",
                     which_ensemble = c("average"),
                     png_ensemble = TRUE,
                     uncertainty = TRUE,
                     performance_metric = c("TSSmax"),
                     models_dir = model_folder,
                     overwrite = TRUE)
    } else if (length(algorithms_vector) == 1) {
      # Criar a pasta ensemble
      ensemble_dir <- paste0("./cap3/", study_sp[i], "/present/ensemble")
      dir_create(ensemble_dir)
      
      # Nome do arquivo do algoritmo
      algorithm_name <- algorithms_vector[1]
      source_file <- paste0("./cap3/", study_sp[i], "/present/final_models/", study_sp[i], "_", algorithm_name, "_raw_mean.tif")
      target_file <- paste0(ensemble_dir, "/", study_sp[i], "_", algorithm_name, "_raw_mean.tif")
      
      # Verificar se o arquivo fonte existe antes de copiar
      if (file.exists(source_file)) {
        # Copiar o arquivo
        file_copy(source_file, target_file)
        print(paste("Arquivo copiado para", target_file))
      } else {
        print(paste("Arquivo fonte não encontrado para", study_sp[i], "e algoritmo", algorithm_name))
      }
    } else {
      print(paste("Nenhum algoritmo com AUC > 0.7 para a espécie:", study_sp[i]))
    }
  } else {
    print(paste("Arquivo não encontrado para a espécie:", study_sp[i]))
  }
}

# modleR 5/5: consensus model continuous ----------------------------------------

for(i in 1:length(study_sp)){
  print(paste("Processando:", study_sp[i]))
  
  # Caminho para o arquivo que tem o AUC médio
  caminho <- paste0("./cap3/", study_sp[i], "/present/final_models/", study_sp[i], "_mean_statistics.csv")
  
  # Verificar se o arquivo existe
  if (file.exists(caminho)) {
    # Ler o arquivo
    algorithms_data <- read_csv(caminho)
    
    # Filtrar os algoritmos que obedecem a regra
    species_algorithms <- algorithms_data %>%
      filter(AUC > 0.7) %>%
      dplyr::select(algorithm) %>%
      distinct()
    
    # Converter a coluna de algoritmos em um vetor
    algorithms_vector <- as.character(species_algorithms$algorithm)
    
    # Verificar se há mais de um algoritmo
    if (length(algorithms_vector) > 1) {

    # Chamar a função ensemble_model
      ensemble_model(species_name = study_sp[i],
                     occurrences = as.data.frame(species_df),
                     algorithms = algorithms_vector,
                     ensemble_dir = "ensemble_2", 
                     which_final = "raw_mean_th",
                     which_ensemble = c("consensus"),
                     png_ensemble = TRUE,
                     uncertainty = TRUE,
                     consensus_level = 0.7,
                     models_dir = model_folder,
                     overwrite = TRUE)
    } else if (length(algorithms_vector) == 1) {
      # Criar a pasta ensemble
      ensemble_dir <- paste0("./cap3/", study_sp[i], "/present/ensemble_2")
      dir_create(ensemble_dir)
      
      # Nome do arquivo do algoritmo
      algorithm_name <- algorithms_vector[1]
      source_file <- paste0("./cap3/", study_sp[i], "/present/final_models/", study_sp[i], "_", algorithm_name, "_raw_mean_th.tif")
      target_file <- paste0(ensemble_dir, "/", study_sp[i], "_", algorithm_name, "_raw_mean_th.tif")
      
      # Verificar se o arquivo fonte existe antes de copiar
      if (file.exists(source_file)) {
        # Copiar o arquivo
        file_copy(source_file, target_file)
        print(paste("Arquivo copiado para", target_file))
      } else {
        print(paste("Arquivo fonte não encontrado para", study_sp[i], "e algoritmo", algorithm_name))
      }
    } else {
      print(paste("Nenhum algoritmo com AUC > 0.7 para a espécie:", study_sp[i]))
    }
  } else {
    print(paste("Arquivo não encontrado para a espécie:", study_sp[i]))
  }
}
