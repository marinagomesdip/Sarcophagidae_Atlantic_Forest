#Chapter three - thesis 
#Atlantic Forest - Modelling
#Marina Morim Gomes 2024
#gomes.mari.95@gmail.com

#loading packages -------------------------------------
library(tidyverse)

#loading data ----------------------------------------
occ <- read.csv("./Data/Processados/7_para_rodar_modelagem.csv")

#Juntar as métricas de todos modelos para analisar --------------------------

#uniting all species AUC and TSS data in one file
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
  caminho_arquivo <- paste0("C:/Users/Dell/OneDrive/Área de Trabalho/cap3/", study_sp[i], 
                            "/present/final_models/", study_sp[i], "_mean_statistics_original.csv")
  
  #read the file
  statistics <- read_csv(caminho_arquivo)
  
  # Filtrar as 8 primeiras linhas e selecionar as colunas "species_name", "algorithm", "AUC" e "TSSmax"
  selected_data <- statistics[1:8, c("species_name", "algorithm", "AUC", "TSSmax")]
  
  # Append the second row to the list
  rows[[i]] <- selected_data
}

# Convert the list into a dataframe
statist <- do.call(rbind, rows)

#Saving metadata
write_csv(statist,"./Data/Processados/7_General_Statistics.csv")

#Gerando gráficos com as estatísticas dos modelos

AUCgraph <- ggplot(statist, aes(algorithm, AUC)) +
  geom_boxplot() +
  theme_classic() +
  labs(x = "Algorithm", y = "AUC values") +
  ylim(0, 1) +
  theme (axis.title.x = element_text(size = 14), 
         axis.title.y = element_text(size = 14),
         axis.text.x = element_text(size = 14),   
         axis.text.y = element_text(size = 14))

ggsave(filename = "./cap3/graphs/AUCgraph.png",
       plot = AUCgraph, device = "png", width = 8, height = 5, dpi = 300)


TSSgraph <- ggplot(statist, aes(algorithm, TSSmax)) +
  geom_boxplot() +
  theme_classic() +
  labs(x = "Algorithm", y = "TSS values") +
  ylim(-1, 1) +
  theme (axis.title.x = element_text(size = 14), 
         axis.title.y = element_text(size = 14),
         axis.text.x = element_text(size = 14),   
         axis.text.y = element_text(size = 14))

ggsave(filename = "./cap3/graphs/TSSgraph.png",
       plot = TSSgraph, device = "png", width = 8, height = 5, dpi = 300)