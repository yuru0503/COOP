library(tidyverse)
library(broom)

SNP_binary_tbl <- read.csv("00_Data_out/33lines_SNP_matrix.csv")

SNP_tbl <- SNP_binary_tbl[-c(1:4), ] %>%
  drop_na() %>% 
  rename("ID" = "X") %>% 
  select(where(is.numeric)) %>% 
  select_if(colSums(.) != 0) %>% 
  mutate_all(as.numeric)
dim(SNP_tbl)
str(SNP_tbl)

SNP_tbl[,apply(SNP_tbl, 2, var, na.rm=TRUE) != 0]
dim(SNP_tbl)

