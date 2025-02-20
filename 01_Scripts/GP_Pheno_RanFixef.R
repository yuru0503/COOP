library(tidyverse)
library(emmeans)
library(lme4)

# Load data
load("00_Data_processed/COOP_GP_Pheno_Geno_Data.RData")

Pheno_training_nest_tbl$data[[1]] %>% glimpse()

Pheno_training_nest_tbl %>%
  summarise(
    Tester_n = map_int(data, ~n_distinct(.$Tester)),
    Location_n = map_int(data, ~n_distinct(.$Location)),
    Parent1_n = map_int(data, ~n_distinct(.$Parent1))
  )

Pheno_training_estimation_tbl <- Pheno_training_nest_tbl %>%
  mutate(
    # Fit models
    Random_model = map(data, ~lme4::lmer(Value ~ 1 + (1|Year) + (1|Location) + Tester + (1|Parent1), data = .)),
    Results = map(Random_model, ~Estimation_results_tbl_generator(., Ran_var = "Parent1", fix_var = c("MOB712", "PS017", "MOB709")))
  ) %>% 
  unnest_wider(Results)

Pheno_training_estimation_tbl$Ranef_tbl[[1]] %>% summary()

## functions
Estimation_results_tbl_generator <- function(Object, Ran_var = "Parent1", fix_var = c("MOB712", "PS017", "MOB709")){
  
  Ranef_tbl <- lme4::ranef(Object) %>% 
    as.data.frame() %>%
    filter(grpvar == Ran_var) %>% 
    select(grp, condval) %>% 
    rename(RandomEF = condval, LineID = grp)
  
  Fixef_tbl <- lme4::fixef(Object) %>%
    unlist() %>%
    enframe(name = "Name", value = "Value") %>% 
    mutate(SumWithIntercept = Value + Value[Name == "(Intercept)"]) %>% 
    filter(str_detect(Name, paste(fix_var, collapse = "|")))
  
  out <- list(Ranef_tbl = Ranef_tbl, Fixef_tbl = Fixef_tbl)
    
  
  return(out)
}

save(Pheno_training_estimation_tbl, file = "00_Data_processed/GP_Training_Pheno_estimation_tbl.RData")  






