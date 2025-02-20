library(tidyverse)
library(rrBLUP)

# Load data
load("00_Data_processed/SB_Pheno_Geno_Data.RData")
load("00_Data_processed/SB_Pheno_estimation_tbl.RData")
load("00_Data_processed/SB_Pheno_estimation_GxY_tbl.RData")

SB_prediction_data_tbl <- SB_Pheno_estimation_GxY_tbl %>% #SB_Pheno_estimation_tbl
  select(Trait, BLUE, BLUP) %>% 
  mutate(Y_tbl = map2(BLUE, BLUP, ~left_join(.x, .y, by = "Ident")), .keep = "unused") %>% 
  mutate(SNP_tbl = list(SB_Geno_tbl)) %>% 
  mutate(Training_data = map2(Y_tbl, SNP_tbl, ~left_join(.x, .y, by = "Ident")))

SB_prediction_data_tbl$Y_tbl[[1]] %>% head()
SB_prediction_data_tbl$Training_data[[1]] %>% view()

# Detect missing values
No_genotyping_lines <- SB_prediction_data_tbl$Training_data[[1]] %>% 
  ## dectect Ident with NA columns
  filter(rowSums(is.na(.)) > 0) %>% 
  pull(Ident)

SB_prediction_data_final_tbl <- SB_prediction_data_tbl%>% 
  select(Trait, Training_data) %>%
  mutate(Training_data = map(Training_data, ~filter(.x, !Ident %in% No_genotyping_lines) %>% 
                               select(!contains("EF")))
                             )

SB_prediction_data_final_tbl$Training_data[[1]] %>% view()  

# 5 repeat 5 fold cross-validation
SB_prediction_results_tbl <- SB_prediction_data_final_tbl %>% 
  mutate(BLUE_BayesB_CV = map(Training_data, ~GP_BGLR_CV(., model = "BayesB", Target_Y_col = "BLUE", ID_col = "Ident"))) %>% 
  mutate(BLUP_BayesB_CV = map(Training_data, ~GP_BGLR_CV(., model = "BayesB", Target_Y_col = "BLUP", ID_col = "Ident"))) %>% 
  mutate(BLUE_rrBLUP_CV = map(Training_data, ~GP_rrBLUP_CV(., Target_Y_col = "BLUE", ID_col = "Ident"))) %>% 
  mutate(BLUP_rrBLUP_CV = map(Training_data, ~GP_rrBLUP_CV(., Target_Y_col = "BLUP", ID_col = "Ident"))) 
  
SB_prediction_results_tbl$BLUE_rrBLUP_CV[[1]] 
SB_prediction_results_tbl$BLUP_rrBLUP_CV[[1]]

SB_prediction_results_tbl$BLUE_rrBLUP_CV[[2]] 
SB_prediction_results_tbl$BLUP_rrBLUP_CV[[2]]

SB_prediction_results_tbl$BLUE_BayesB_CV[[1]] 
SB_prediction_results_tbl$BLUP_BayesB_CV[[1]]

SB_prediction_results_tbl$BLUE_BayesB_CV[[2]] 
SB_prediction_results_tbl$BLUP_BayesB_CV[[2]]

# Save data
save(SB_prediction_results_tbl, file = "00_Data_processed/SB_GP_5repeat_5Fold_CV_results.RData")  

