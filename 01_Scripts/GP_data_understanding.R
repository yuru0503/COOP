library(tidyverse)
library(DataExplorer)

Training_set_info_tbl <- read.csv("00_Data/COOP_CyBox/COOP - Analysis and Writing/Data_Analysis/Modeling_Set.csv")
Prediction_set_info_tbl <- read.csv("00_Data/COOP_CyBox/COOP - Analysis and Writing/Data_Analysis/Validation_Hybrids.csv")
Phenotypic_data_tbl <- read.csv("00_Data/COOP_CyBox/COOP - Analysis and Writing/Data_Analysis/COOP_Phenotypic.csv")

Pheno_tbl <- Phenotypic_data_tbl %>% 
  mutate_at(vars(Experiment:Range), as.factor) %>% 
  mutate_at(vars(PDT:ASH), as.numeric) %>% 
  glimpse()                                    

## Phenotypic data ----

str(Pheno_tbl)
plot_missing(Pheno_tbl)
plot_histogram(Pheno_tbl)
#plot_correlation(Pheno_tbl)
plot_bar(Pheno_tbl, by = "Treatments")
plot_bar(Pheno_tbl, by = "Location")
plot_boxplot(Pheno_tbl, by = "Treatments")
plot_boxplot(Pheno_tbl, by = "Location")
plot_boxplot(Pheno_tbl, by = "Tester")
plot_boxplot(Pheno_tbl %>% select(YLD, Tester), by = "Tester")

## Training set info----
glimpse(Training_set_info_tbl)
plot_missing(Training_set_info_tbl )
plot_bar(Training_set_info_tbl)
plot_bar(Training_set_info_tbl$TesterID)
Training_set_info_tbl$LineID %>% unique() %>% length()
Training_set_info_tbl$TesterID %>% unique() %>% length()

## Prediction set info----
glimpse(Prediction_set_info_tbl)
plot_bar(Prediction_set_info_tbl)
