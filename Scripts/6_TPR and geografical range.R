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


#para calcular TPR --------------------------------------------------------------
# Criar um data frame vazio para armazenar os resultados
df_tpr <- data.frame(nome_cientifico = character(), TPR = numeric(), stringsAsFactors = FALSE)

for(i in 1:length(study_sp)){
  
  # IMPORTANDO RASTER DA ESPECIE ALVO
  # path to file
  caminho_arquivo <- paste0("C:/Users/Dell/OneDrive/Área de Trabalho/cap3/", study_sp[i], 
                            "/present/ensemble_2/", study_sp[i], "_ensemble_0.7_consensus.tif")
  
  # read the raster file
  ras <- raster(caminho_arquivo)
  
  # FILTRAR DADOS DE OCC DA ESPECIE ALVO
  occal <- occ %>%
    filter(nome_cientifico == study_sp[i])
  
  # TRANSFORMAR AS COORDS EM OBJETO ESPACIAL
  occal_sf <- occal %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = crs(ras), remove = FALSE)
  
  # EXTRAIR OS VALORES DO RASTER PARA AS OCORRENCIAS
  valores_raster <- raster::extract(ras, st_coordinates(occal_sf))
  
  # ADICIONAR OS VALORES EXTRAÍDOS AO DATA FRAME
  occal$valor_raster <- valores_raster
  
  # CALCULAR O TPR
  # Contar o número de ocorrências com valor de raster = 1
  ocorrencias_valor_1 <- sum(occal$valor_raster == 1, na.rm = TRUE)
  
  # Total de ocorrências
  total_ocorrencias <- nrow(occal)
  
  # Calcular o TPR (ocorrências com valor 1 / total de ocorrências)
  tpr <- ocorrencias_valor_1 / total_ocorrencias
  
  # ADICIONAR O RESULTADO AO DATA FRAME FINAL
  df_tpr <- rbind(df_tpr, data.frame(nome_cientifico = study_sp[i], TPR = tpr))
}


#para calcular % de área prevista -----------------------------------------------------

# Criar um data frame vazio para armazenar os resultados
df_georan <- data.frame(nome_cientifico = character(), proporcao_area = numeric(), stringsAsFactors = FALSE)

# Para obter o shapefile da Mata Atlântica
mata_atlantica <- geobr::read_biomes(year = 2019, showProgress = FALSE) %>%
  filter(code_biome == 4)

# Iterar sobre as espécies
for(i in 1:length(study_sp)){
  
  #IMPORTANDO RASTER DA ESPECIE ALVO
  caminho_arquivo <- paste0("C:/Users/Dell/OneDrive/Área de Trabalho/cap3/", study_sp[i], 
                            "/present/ensemble_2/", study_sp[i], "_ensemble_0.7_consensus.tif")
  
  ras <- raster(caminho_arquivo)
  
  # Reprojetar a máscara da Mata Atlântica para combinar com o CRS do raster
  mata_atlantica_proj <- st_transform(mata_atlantica, crs = crs(ras))
  
  # Converter para objeto Spatial para aplicar a máscara
  mata_atlantica_sp <- as(mata_atlantica_proj, "Spatial")
  
  # Aplicar a máscara no raster
  ras_masked <- mask(ras, mata_atlantica_sp)
  
  # Contar o total de células dentro da área mascarada (excluindo as células NA)
  total_cells_within_mask <- sum(!is.na(values(ras_masked)))
  
  # Contar o número de células com valor 1 dentro da área mascarada
  cells_with_1 <- sum(values(ras_masked) == 1, na.rm = TRUE)
  
  # Calcular a proporção de células com valor 1 em relação ao total dentro da máscara
  proporcao <- (cells_with_1 / total_cells_within_mask) * 100
  
  # ADICIONAR O RESULTADO AO DATA FRAME FINAL
  df_georan <- rbind(df_georan, data.frame(nome_cientifico = study_sp[i], proporcao_area = proporcao))
}

#Extraindo valores para o Material Suplementar de todas as espécies -------------------------

# Criar um data frame vazio para armazenar os resultados
df_metrics <- data.frame(nome_cientifico = character(), N = numeric(), meanAUC = numeric(), meanTSS = numeric(), stringsAsFactors = FALSE)

for(i in 1:length(study_sp)){
  #IMPORTANDO METADATA PRA EXTRAIR O N DA ESPECIE ALVO
  caminho_arquivo <- paste0("C:/Users/Dell/OneDrive/Área de Trabalho/cap3/", study_sp[i], 
                            "/present/data_setup/metadata.csv")
  
  metada <- read.csv(caminho_arquivo)
  
  #Selecionar apenas a coluna do N
  metada <- metada %>%
    dplyr::select(original.n)
  
  #IMPORTANDO METRICAS PRA EXTRAIR O MEAN AUC E MEAN TSS
  caminho_arquivo <- paste0("C:/Users/Dell/OneDrive/Área de Trabalho/cap3/", study_sp[i], 
                            "/present/final_models/", study_sp[i], "_mean_statistics_original.csv")
  
  metric <- read.csv(caminho_arquivo)
  
  #calculando o mean AUC
  auc <- mean(metric$AUC, na.rm = TRUE)
  
  #calculando o mean TSS
  tss <- mean(metric$TSSmax, na.rm = TRUE)
  
  # ADICIONAR O RESULTADO AO DATA FRAME FINAL
  df_metrics <- rbind(df_metrics, data.frame(nome_cientifico = study_sp[i], N = metada, meanAUC = auc, meanTSS = tss))
}


#Unindo tudo em um único DF -------------------------------

united <- left_join(df_tpr, df_georan, by = "nome_cientifico")

united <- left_join(united, df_metrics, by = "nome_cientifico")

#exportando dados--------------------------------------------------
#Saving metadata
write_csv(united,"./Data/Processados/7_Suplementar_Material5.csv")

#histogramas ------------------------------------------------------

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
