#Chapter two - thesis 
#SDM of Atlantic Forest - Preparing data
#Marina Morim Gomes 2024
#gomes.mari.95@gmail.com

#loading packages -------------------------------------
library(tidyverse)

#loading data -------------------------------------
dadoslimpos <- read_csv("./Data/Processados/4_dadoslimpos.csv")
occsbiome <- read_csv("./Data/Processados/5_SM2.csv")

#Filtering species that will be used from all data

#Removing occurence data before 1970 to mach with WorlClim enviromental
#dadoslimpos <- dadoslimpos %>%
  #filter(ano_inicio >= 1970)

#Selecting species that occur in AF -----------------------
occsatl <- occsbiome %>%
  filter(mata_atlantica == 1) %>%
  dplyr::select(nome_cientifico)

#Filtrar o banco de dados originais apenas com espécies da AF --------------
dadosatl <- left_join(occsatl, dadoslimpos, by = "nome_cientifico")

#Filtrar dados sem GPS -----------------------------------------
dadosatl <- dadosatl %>%
  filter(!is.na(longitude))

#Gerar dados únicos -----------------------------------------------------------
dadosatlun <- dadosatl %>%
  dplyr::select(nome_cientifico, latitude, longitude) %>%
  distinct()

#Remover espécies que não tem pelo menos 10 occs ------------------------------
tenrecords <- dadosatlun %>%
  count(nome_cientifico) %>%
  filter(n>=10)

dadosmodel <- left_join(tenrecords,dadosatlun, by = "nome_cientifico") %>%
  dplyr::select(-n)

#Exportando dados para a próxima etapa ----------------------------------------
write_csv(dadosmodel,"./Data/Processados/7_para_rodar_modelagem.csv")