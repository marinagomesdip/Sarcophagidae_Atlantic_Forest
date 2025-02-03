#Chapter two - thesis 
#SDM of Atlantic Forest - Preparing data
#Marina Morim Gomes 2024
#gomes.mari.95@gmail.com

#loading packages -------------------------------------
library(tidyverse)

#loading data -------------------------------------
dadoslimpos <- read_csv("./Data/Processados/4_dadoslimpos.csv")
occsbiome <- read_csv("./Data/Processados/5_SM2.csv")

#Selecting species that occur in Atlantic Forest following Gomes et al. 2024 -----------------------
occsatl <- occsbiome %>%
  filter(mata_atlantica == 1) %>%
  dplyr::select(nome_cientifico)

#Filtering the original dataset only with species from Atlantic Forest -----------------------------
dadosatl <- left_join(occsatl, dadoslimpos, by = "nome_cientifico")

#Filtering data without coordinates ----------------------------------------------------------------
dadosatl <- dadosatl %>%
  filter(!is.na(longitude))

#Removing duplicates --------------------------------------------------------------------------------
dadosatlun <- dadosatl %>%
  dplyr::select(nome_cientifico, latitude, longitude) %>%
  distinct()

#Removing species without at least 10 occs ----------------------------------------------------------
tenrecords <- dadosatlun %>%
  count(nome_cientifico) %>%
  filter(n>=10)

dadosmodel <- left_join(tenrecords,dadosatlun, by = "nome_cientifico") %>%
  dplyr::select(-n)

#Exponting raw data to next steps ----------------------------------------
write_csv(dadosmodel,"./Data/Processados/7_para_rodar_modelagem.csv")