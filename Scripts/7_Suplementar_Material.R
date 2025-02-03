#Chapter two - thesis 
#SDM of Atlantic Forest - Suplementar Material
#Marina Morim Gomes 2024
#gomes.mari.95@gmail.com

#loading packages ----------------------------------------------------------
library(tidyverse)

#loading data --------------------------------------------------------------
original <- read_csv("./Data/Processados/4_dadoslimpos.csv")
model <- read_csv("./Data/Processados/7_para_rodar_modelagem.csv")

#Appendix 1 - occurrence data  ------------------
# Usamos a função 'semi_join' para garantir que só as linhas com coordenadas correspondentes sejam usadas
sm <- dplyr::semi_join(original, model, by = c("latitude", "longitude", "nome_cientifico"))

# Agora, selecionar as colunas necessárias para o resultado final ('sm')
sm <- sm %>%
  dplyr::select(
    pais, estado_ou_provincia, municipio, localidade, data, metodo_coleta, genero_original,
    especie_original, determinador, coletor, referencia, observacoes, ID, qualificador,
    latitude, longitude, incerteza, nome_cientifico, nome_especie_modificado,
    coordenadas_validas, latitude_longitudes_repetidas, coordenadas_zeradas
  ) %>%
  dplyr::distinct(latitude, longitude, nome_cientifico, .keep_all = TRUE)

# O resultado 'sm' deve ter o mesmo número de linhas que 'model'
print(nrow(sm) == nrow(model))

# Modificar a coluna 'nome_especie_modificado'
sm$nome_especie_modificado <- sm$nome_especie_modificado %>%
  str_replace_all(c("nao" = "no", "sim" = "yes"))

# Modificar a coluna 'qualificador'
sm$qualificador <- sm$qualificador %>%
  str_replace_all(c("otimo" = "Excellent", "bom" = "Fine", "ruim" = "Poor"))

# Modificar a coluna 'ID' para remover os números e manter apenas o nome da fonte
sm$ID <- sm$ID %>%
  str_replace("_\\d+", "")  # Remove qualquer sequência de números após o underline

#Exportando dados 
write_csv(sm,"./Data/Processados/7_Suplementar_Material1.csv")

#Appendix 2 - Metadata -----------------

#uniting all species metadata in one file
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

#Create a list to store the results of each iteration
rows <- list()

for(i in 1:length(study_sp)){
  
  #path to file
  caminho_arquivo <- paste0("C:/Users/Dell/OneDrive/Área de Trabalho/cap3/", study_sp[i], "/present/data_setup/metadata.csv")
  
  #read the file
  metadados <- read_csv(caminho_arquivo)
  
  # Extract the second row from the dataframe
  segunda_linha <- metadados[1,]
  
  # Append the second row to the list
  rows[[i]] <- segunda_linha
}

# Convert the list into a dataframe
statistics <- do.call(rbind, rows)

#Saving metadata
write_csv(statistics,"./Data/Processados/7_Suplementar_Material2.csv")