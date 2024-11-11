#Chapter three - thesis 
#Atlantic Forest - Modelling
#Marina Morim Gomes 2024
#gomes.mari.95@gmail.com

#loading packages -------------------------------------
library(tidyverse)

#loading data ----------------------------------------
occ <- read.csv("./Data/Processados/7_para_rodar_modelagem.csv")

#Gerar uma lista com o nome dos gêneros -----------------
generos <- occ %>%
  separate(nome_cientifico, into = c("genero", "subgenero", "especie"), sep = " ") %>%
  dplyr::select(genero) %>%
  distinct()

#Exportar lista de gêneros ------------------------------
write_csv(generos,"./Data/Processados/7_generos.csv")

#Gerar uma lista com o nome das espécies -----------------
species <- occ %>%
  dplyr::select(nome_cientifico) %>%
  distinct()

#Exportar lista de gêneros ------------------------------
write_csv(species,"./Data/Processados/7_species.csv")
