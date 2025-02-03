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

#Species list
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
  
  # print the species name to known what is going on in the code 
  print(paste("Processando:", study_sp[i]))
  
  #running setup_sdmdata
  setup_sdmdata(species_name = study_sp[i],                   #filtering the species name for this iteration
                occurrences = as.data.frame(species_df),      #transforming the tible of occurrences in df
                predictors = variables,                       #name of raster stack with the variables
                models_dir = model_folder,                    #folder to save partitions
                partition_type = "crossvalidation",           #crossvalidation is a way to partition data without replacement
                cv_partitions = 5,                            #number of parts that the occurrences will be divided into
                cv_n = 2,                                     #how many times this partitions will be separed
                seed = 512,
                buffer_type = "mean",                         #Shows whether the buffer should be calculated, in this case, using the "mean" distance between occurrence points
                env_filter = TRUE,                            #filter used to exclude pseudoabsence from very close to the occurrences point
                env_distance = "centroid",                    #type of enviromental distance, the distance of each raster pixel to the environmental centroid of the distribution
                min_env_dist = 0.3,                           #Sets a minimum value to exclude the areas closest (in the environmental space) to the occurrences or their centroid, expressed in quantiles, from 0 (the closest) to 1
                n_back = nrow(species_df)*10,                 #number of pseudoabsences (in this case, 10x number of occurrences)
                png_sdmdata = TRUE,                           #Save the data in a png file
                clean_dupl = TRUE,                            #removes points with the same longitude and latitude
                clean_uni = TRUE,                             #selects only one point per pixel
                clean_nas = TRUE,                             #removes points that are outside the bounds of the raster
                geo_filt = FALSE,                             #delete occurrences that are too close to each other
                select_variables = TRUE,                      #Whether a variable selection should be performed. It excludes highly correlated environmental variables
                sample_proportion = 0.5,                      #Proportion of the raster values to be sampled to calculate the correlation. The value should be set as a decimal, between 0 and 1
                cutoff = 0.7)                                 #Cutoff value of correlation between variables to exclude environmental layers
}

# modleR 2/5: model calibration -------------------------------------------

for(i in 1:length(study_sp)){
  
  # run selected algorithms for each partition
  do_many(species_name = study_sp[i],          #filtering the species name for this iteration        
          predictors = variables,              #name of raster stack with variables
          models_dir = model_folder,           #folder to save partitions
          png_partitions = TRUE,               #png map of every partitions of algortihms
          write_bin_cut = FALSE,
          write_rda = TRUE,
          bioclim = TRUE,                      #algorithms = TRUE means that this algorithm will run
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
  
  final_model(species_name = study_sp[i],                  #filtering the species name for this iteration
              algorithms = NULL,                           #use all algorthms in the folder
              models_dir = model_folder,                   #folder to save 
              scale_models = TRUE,                         #convert model outputs to 0-1
              which_models = c("raw_mean", "raw_mean_th"), #for generating binary and continuous models
              mean_th_par = "spec_sens",                   #selecting threshold maximizing the metrics
              png_final = TRUE,
              overwrite = TRUE)
  
}

# modleR 4/5: consensus model continuous ----------------------------------------

for(i in 1:length(study_sp)){
  print(paste("Processando:", study_sp[i]))
  
  #Path to file that had median AUC
  caminho <- paste0("./cap3/", study_sp[i], "/present/final_models/", study_sp[i], "_mean_statistics.csv")
  
  #Check if the file exists
  if (file.exists(caminho)) {
    #Read the file
    algorithms_data <- read_csv(caminho)
  
    #Filter the algorithms that had AUC > 0.7
    species_algorithms <- algorithms_data %>%
      filter(AUC > 0.7) %>%
      dplyr::select(algorithm) %>%
      distinct()
    
    #Convert the algorithms column to a vector
    algorithms_vector <- as.character(species_algorithms$algorithm)
    
    #Verificar se há mais de um algoritmo
    if (length(algorithms_vector) > 1) {                                   #if the species in this round has more than one algorithm obeying the rule
     
      #Rename the original file
      caminho_original <- paste0("./cap3/", study_sp[i], "/present/final_models/", study_sp[i], "_mean_statistics_original.csv")
      file_move(caminho, caminho_original)
      print(paste("Arquivo original renomeado para", caminho_original))
      
      #Filter the original dataframe to only include the algorithms in the vector
      filtered_algorithms_data <- algorithms_data %>%
        filter(algorithm %in% algorithms_vector)
      
      #Save the new filtered file
      write_csv(filtered_algorithms_data, caminho)
      print(paste("Novo arquivo filtrado salvo como", caminho))
      
      #Call the ensemble_model function
      ensemble_model(species_name = study_sp[i],                        #filtering the species name for this iteration
                     occurrences = as.data.frame(species_df),           #transforming the tible of occurrences in df
                     algorithms = algorithms_vector,                    #inserting only the filtered algorithms (AUC > 0.7)
                     which_final = "raw_mean",                          #using the continous model
                     which_ensemble = c("average"),                     #calculating the average between the models
                     png_ensemble = TRUE,                               #salving a png with the results
                     uncertainty = TRUE,                                #calculating uncertainty (Attention: the uncertainty calculated by the package use all algorithms in the folder!!)
                     performance_metric = c("TSSmax"),                  #metric used to calculate the average
                     models_dir = model_folder,                         #the folder of the analysis
                     overwrite = TRUE)                                  #allows overwriting if you run the code again
  
      } else if (length(algorithms_vector) == 1) {                      #if the species in this round has only one algorithm obeying the rule
      #Create the ensemble folder
      ensemble_dir <- paste0("./cap3/", study_sp[i], "/present/ensemble")
      dir_create(ensemble_dir)
      
      #Algorithm file name
      algorithm_name <- algorithms_vector[1]
      source_file <- paste0("./cap3/", study_sp[i], "/present/final_models/", study_sp[i], "_", algorithm_name, "_raw_mean.tif")
      target_file <- paste0(ensemble_dir, "/", study_sp[i], "_", algorithm_name, "_raw_mean.tif")
      
      #Check if the source file exists before copying
      if (file.exists(source_file)) {
        #Copy the file
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

# modleR 5/5: consensus model binary ----------------------------------------

for(i in 1:length(study_sp)){
  print(paste("Processando:", study_sp[i]))
  
  #Path to file that had median AUC
  caminho <- paste0("./cap3/", study_sp[i], "/present/final_models/", study_sp[i], "_mean_statistics.csv")
  
  #Check if the file exists
  if (file.exists(caminho)) {
    #Read the file
    algorithms_data <- read_csv(caminho)
    
    #Filter the algorithms that had AUC > 0.7
    species_algorithms <- algorithms_data %>%
      filter(AUC > 0.7) %>%
      dplyr::select(algorithm) %>%
      distinct()
    
    #Convert the algorithms column to a vector
    algorithms_vector <- as.character(species_algorithms$algorithm)
    
    #Verificar se há mais de um algoritmo
    if (length(algorithms_vector) > 1) {

    #Call the ensemble_model function
      ensemble_model(species_name = study_sp[i],                           #filtering the species name for this iteration
                     occurrences = as.data.frame(species_df),              #transforming the tible of occurrences in df
                     algorithms = algorithms_vector,                       #inserting only the filtered algorithms (AUC > 0.7)
                     ensemble_dir = "ensemble_2",                          #changing the name of the folder to not overwrite the other ensemble
                     which_final = "raw_mean_th",                          #using the binary model
                     which_ensemble = c("consensus"),                      #consensus method to do ensemble binary
                     png_ensemble = TRUE,                                  #salving a png with the results
                     uncertainty = TRUE,                                   #calculating uncertainty (Attention: the uncertainty calculated by the package use all algorithms in the folder!!)
                     consensus_level = 0.7,                                #the consensus value (default = 0.5)
                     models_dir = model_folder,                            #the folder of the analysis
                     overwrite = TRUE)                                     #allows overwriting if you run the code again
    } else if (length(algorithms_vector) == 1) {
      #Create the ensemble 2 folder
      ensemble_dir <- paste0("./cap3/", study_sp[i], "/present/ensemble_2")
      dir_create(ensemble_dir)
      
      #Algorithm file name
      algorithm_name <- algorithms_vector[1]
      source_file <- paste0("./cap3/", study_sp[i], "/present/final_models/", study_sp[i], "_", algorithm_name, "_raw_mean_th.tif")
      target_file <- paste0(ensemble_dir, "/", study_sp[i], "_", algorithm_name, "_raw_mean_th.tif")
      
      #Check if the source file exists before copying
      if (file.exists(source_file)) {
        #Copy the file
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
