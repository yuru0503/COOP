library(tidyverse)

Phenotypic_data_tbl <- read.csv("00_Data/COOP_CyBox/COOP - Analysis and Writing/Data_Analysis/COOP_Phenotypic.csv")

Pheno_tbl <- Phenotypic_data_tbl %>% 
  mutate_at(vars(Experiment:Range), as.factor) %>% 
  mutate_at(vars(PDT:ASH), as.numeric) %>% 
  glimpse() 

Training_set_info_tbl <- read.csv("00_Data/COOP_CyBox/COOP - Analysis and Writing/Data_Analysis/Modeling_Set.csv")
Prediction_set_info_tbl <- read.csv("00_Data/COOP_CyBox/COOP - Analysis and Writing/Data_Analysis/Validation_Hybrids.csv")

Training_set_info_tbl %>% glimpse()
Prediction_set_info_tbl %>% glimpse()

Training_HYB_list <- Training_set_info_tbl %>% 
  pull(HYB)

Training_Line_list <- Training_set_info_tbl %>% 
  pull(LineID)

Prediction_Line_list <- Prediction_set_info_tbl %>% 
  pull(LineID)

## Phenotypic training data----
## filter the primary traits for GP
Pheno_training_tbl <- Pheno_tbl %>% 
  filter(HYB %in% Training_HYB_list) %>% 
  select(Experiment:Range, YLD, PRO, MST, PHT, EHT) %>% 
  glimpse()

str(Pheno_training_tbl)

Pheno_training_nest_tbl <- Pheno_training_tbl %>% 
  pivot_longer(cols = -(1:17), names_to = "Trait", values_to = "Value") %>% 
  group_by(Trait) %>%
  nest() 
Pheno_training_nest_tbl$data[[1]] %>% glimpse()

## Genotypic training and prediction data----

Genotypic_matrix <- read.csv("00_Data_processed/COOP_SNP_matrix.csv")
Genotypic_matrix[1:5, 1:7]

SNP_training_tbl <- Genotypic_matrix %>% 
  rename(LineID = X) %>% 
  filter(LineID %in% Training_Line_list)
dim(SNP_training_tbl)

SNP_prediction_tbl <- Genotypic_matrix %>% 
  rename(LineID = X) %>% 
  filter(LineID %in% Prediction_Line_list)
dim(SNP_prediction_tbl)

save(Pheno_training_nest_tbl, SNP_training_tbl, SNP_prediction_tbl, file = "00_Data_processed/COOP_GP_Pheno_Geno_Data.RData")
