library(tidyverse)
source("01_Scripts/Functions.R")
today <- format(Sys.Date(), "%m%d")

# Load data
load("00_Data_processed/COOP_GP_Pheno_Geno_Data.RData")
load("00_Data_processed/GP_Training_Pheno_estimation_tbl.RData")

SNP_training_tbl[1:5, 1:7]
Pheno_training_estimation_tbl$Ranef_tbl[[1]] %>% head()

GP_training_data_tbl <- Pheno_training_estimation_tbl %>% 
  select(Trait, matches("Ran|Fix")) %>% 
  mutate(Training_SNP_tbl = list(SNP_training_tbl)) %>% 
  mutate(Training_data = map2(Ranef_tbl, Training_SNP_tbl, ~left_join(.x, .y, by = "LineID")))

GP_training_data_tbl$Training_data[[3]][, 1:2]

# 5 repeat 5 fold cross-validation
GP_CV_results_tbl <- GP_training_data_tbl %>% 
  filter(!Trait == "MST") %>% 
  mutate(rrBLUP_CV = map(Training_data, ~GP_rrBLUP_CV(., Target_Y_col = "RandomEF", ID_col = "LineID", Repeat = 5))) %>% 
  mutate(BayesB_CV = map(Training_data, ~GP_BGLR_CV(., Target_Y_col = "RandomEF", ID_col = "LineID", model = "BayesB", Repeat = 5))) 
  
GP_CV_results_tbl$rrBLUP_CV[[1]]
GP_CV_results_tbl$BayesB_CV[[1]]

test <- GP_training_data_tbl %>% 
  filter(Trait == "YLD") %>% 
  mutate(rrBLUP_stra_CV = map(Training_data, ~GP_rrBLUP_stratified_CV(., Target_Y_col = "RandomEF", ID_col = "LineID", Cluster = 1, Repeat = 5)))

test$rrBLUP_stra_CV[[1]] %>% pluck(1) %>% acc_summaryer()

# Save CV results
today <- format(Sys.Date(), "%m%d")
save(GP_CV_results_tbl, file = paste0("00_Data_processed/COOP_GP_5repeat_5Fold_CV_results_", today, ".RData"))

GP_CV_results_tbl$rrBLUP_CV[[1]] %>% pluck(1) %>% acc_summaryer()

GP_summary_tbl <- GP_CV_results_tbl %>% 
  mutate(PA_rrBLUP = rrBLUP_CV %>% map(pluck(1)) %>% map(acc_summaryer)) %>% 
  mutate(PA_BayesB = BayesB_CV %>% map(pluck(1)) %>% map(acc_summaryer)) %>%
  select(Trait, PA_rrBLUP, PA_BayesB) %>%
  unnest_wider(PA_rrBLUP, names_sep = "_") %>%
  unnest_wider(PA_BayesB, names_sep = "_") %>%
  mutate(across(where(is.numeric), round, 2)) %>% 
  pivot_longer(-Trait, names_to = "Variable", values_to = "Value") %>% 
  separate(Variable, c("Models", "Stats"), , sep = "_(?=[^_]+$)") %>% 
  mutate(Models = str_remove(Models, "PA_")) %>% 
  pivot_wider(names_from = "Stats", values_from = "Value") 

p <- GP_summary_tbl %>% 
  ggplot(aes(Trait, mean, fill = Models))+
  geom_errorbar(#data = GP_mean_sd_tbl,
    aes(x =Trait,
        ymax = mean + sd/sqrt(5),
        ymin = mean - sd/sqrt(5)), 
    position = position_dodge(width=0.6), 
    width = 0.1,
    linewidth = 0.5)+
  geom_bar(aes(fill = Models),
           stat = "identity",
           color = "black",
           position = position_dodge(width=0.6),
           width = 0.5,
           linewidth = 0.5 )+
  labs(y="Predictive ability")+
  # add title
  ggtitle("5 repeat 5 fold cross-validation")
  
p

ggsave(paste0("02_Figures/GP_PA_summary_plot_", today, ".png"),
       p, width = 8, height = 6, units = "in", dpi = 300)
