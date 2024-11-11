#Chapter two - thesis 
#SDM of Atlantic Forest - Generating Pseudoabsences
#Marina Morim Gomes 2024
#gomes.mari.95@gmail.com

#loading packages -------------------------------------
library(tidyverse)

#loading data -------------------------------------
dadoslimpos <- read_csv("./Data/Processados/4_dadoslimpos.csv")
dadospararodar <- read_csv("./Data/Processados/7_para_rodar_modelagem.csv")

# Definir a lista de espécies que serão utilizadas ----------------------------
study_sp <- c(
"Argoravinia aurea",
"Argoravinia rufiventris",
"Blaesoxipha (Gigantotheca) plinthopyga",
"Dexosarcophaga (Bezzisca) ampullula",
"Dexosarcophaga (Dexosarcophaga) transita",
"Dexosarcophaga (Farrimyia) carvalhoi",
"Dexosarcophaga (Farrimyia) globulosa",
"Engelimyia inops",
"Helicobia aurescens",
"Helicobia morionella",
"Helicobia pilifera",
"Helicobia rapax",
  "Lipoptilocnema crispina",
  "Lipoptilocnema crispula",
  "Oxysarcodexia admixta",
  "Oxysarcodexia amorosa",
  "Oxysarcodexia angrensis",
  "Oxysarcodexia avuncula",
  "Oxysarcodexia bakeri",
  "Oxysarcodexia carvalhoi",
  "Oxysarcodexia culmiforceps",
  "Oxysarcodexia culminata",
  "Oxysarcodexia diana",
  "Oxysarcodexia fluminensis",
  "Oxysarcodexia fringidea",
  "Oxysarcodexia injuncta",
  "Oxysarcodexia intona",
  "Oxysarcodexia modesta",
  "Oxysarcodexia parva",
  "Oxysarcodexia paulistanensis",
  "Oxysarcodexia terminalis",
  "Oxysarcodexia thornax",
  "Oxysarcodexia timida",
  "Oxysarcodexia varia",
  "Oxysarcodexia xanthosoma",
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
  "Sarcofahrtiopsis cuneata",
  "Titanogrypa (Cucullomyia) larvicida",
  "Tricharaea (Sarcophagula) canuta",
  "Tricharaea (Sarcophagula) occidua",
  "Tricharaea (Sarothromyia) femoralis")

# Inicialize um dataframe vazio para armazenar os resultados
resultados <- data.frame()

# Loop através de cada espécie no dataframe
for (i in 1:length(study_sp)) {
  #dados municipios que a espécie alvo ocorre
  municipios_especie_alvo <- dadoslimpos %>%
    filter(nome_cientifico == study_sp[i]) %>%
    filter(!is.na(municipio)) %>%
    select(municipio) %>%
    unique()
  
  #dados municipios que outras espécies ocorrem
  outros_municipios <- dadoslimpos %>%
    filter(nome_cientifico != study_sp[i]) %>%
    filter(!is.na(municipio)) %>%
    select(municipio) %>%
    unique()
  
  #quais municipios tem outras espécies mas não a alvo
  municipios <- anti_join(outros_municipios, municipios_especie_alvo, by = "municipio")

  #selecionar dados que ocorram apenas nos municipios selecionados
  dados <- left_join(municipios, dadoslimpos, by="municipio")
  
  #lista de espécies selecionadas para o estudo
  especies <- dadospararodar %>%
    select(nome_cientifico) %>%
    distinct()
  
  #filtrar o banco de dados só com as espécies alvo
  dados2 <- left_join(especies, dados, by="nome_cientifico")
  
  #tirar colunas que não são de interesse
  dados2 <- dados2 %>%
    select(nome_cientifico, latitude, longitude) %>%
    distinct
  
  #filtrar o banco de dados pra excluir a espécie alvo
  dados2 <- dados2 %>%
    filter(nome_cientifico != study_sp[i])
  
  #criando uma coluna para colocar todos os nomes iguais 
  dados2 <- dados2 %>%
    mutate(nome_cient = paste0( study_sp[i])) %>%
    select(-nome_cientifico) %>%
    rename(nome_cientifico = nome_cient)
  
  #adicionando dados de cada espécie no df resultados 
  resultados <- bind_rows(resultados, dados2)
}
  
  
  #ajustando o df
resultados <- resultados %>%
  rename(species_name = nome_cientifico) %>% 
  rename(lat = latitude) %>%
  rename(lon = longitude) %>% 
  select(species_name, lat, lon)
  
  #exportando csv 
  write_csv(resultados, "./Data/Processados/7_pseudoausencias.csv")
  