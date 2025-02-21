library(tidyverse)
source("01_Scripts/Functions.R")
today <- format(Sys.Date(), "%m%d")

# Load data
load("00_Data_processed/COOP_GP_Pheno_Geno_Data.RData")
load("00_Data_processed/GP_Training_Pheno_estimation_tbl.RData")

GP_data_tbl <- Pheno_training_estimation_tbl %>% 
  select(Trait, matches("Ran|Fix")) %>% 
  # Data preparation
  mutate(Training_SNP_tbl = list(SNP_training_tbl)) %>% 
  mutate(Training_data = map2(Ranef_tbl, Training_SNP_tbl, ~left_join(.x, .y, by = "LineID"))) %>% 
  mutate(Prediction_SNP_tbl = list(SNP_prediction_tbl)) %>% 
  # Prediction
  mutate(Prediction_rrBLUP_result = map2(Training_data, Prediction_SNP_tbl, ~GP_rrBLUP(.x, .y, Target_Y_col = "RandomEF", ID_col = "LineID"))) %>%
  mutate(Prediction_BayesB_result = map2(Training_data, Prediction_SNP_tbl, ~GP_BGLR(.x, .y, model = "BayesB", Target_Y_col = "RandomEF", ID_col = "LineID")))

# Save data
save(GP_data_tbl, file = paste0("00_Data_processed/GP_Predection_tbl_", today, ".RData"))

rrBLUP <- GP_data_tbl$Prediction_rrBLUP_result[[1]] %>% pluck(1)
BayesB <- GP_data_tbl$Prediction_BayesB_result[[1]] %>% pluck(1)
cor(rrBLUP, BayesB)

GP_data_tbl$Prediction_rrBLUP_result[[1]] %>% GP_results_tbl_generator() %>% head()

GP_selection_tbl <- GP_data_tbl %>% 
  select(Trait, Prediction_rrBLUP_result, Prediction_BayesB_result) %>% 
  mutate(rrBLUP = map(Prediction_rrBLUP_result, GP_results_tbl_generator)) %>% 
  mutate(BayesB = map(Prediction_BayesB_result, GP_results_tbl_generator)) %>% 
  select(Trait, rrBLUP, BayesB) %>% 
  pivot_longer(cols = c(rrBLUP, BayesB), names_to = "Method", values_to = "Predictions") %>% 
  unnest(Predictions) %>% 
  ungroup() %>%
  group_by(Trait, Method) %>%
  slice_max(Predicted_Value, prop = 0.1) %>% 
  nest() 

GP_selection_tbl
