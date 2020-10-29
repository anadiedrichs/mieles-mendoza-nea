library(tidyverse)
library(DataExplorer)

library(readxl)
mieles_mza_nea <- read_excel("data/mieles-mza-nea.xls", 
                             n_max = 205)
#View(mieles_mza_nea)

dataset <- 
  mieles_mza_nea %>%  
  select(-starts_with("Columna")) %>% 
  rename( "Pb" = "208 Pb")

# colnames(dataset)[17] <- "Pb"

unique(dataset$Origin)


