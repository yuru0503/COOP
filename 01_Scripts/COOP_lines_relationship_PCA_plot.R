library(tidyverse)
library(broom)

SNP_matrix <- read.csv("00_Data_processed/COOP_SNP_matrix.csv")
SNP_matrix[1:5, 1:7]
dim(SNP_matrix)

SNP_tbl <- SNP_matrix %>%
  rename("ID" = "X") %>% 
  select(where(is.numeric)) %>% 
  select_if(colSums(.) != 0) %>% # remove columns with all zeros
  mutate_all(as.numeric)
dim(SNP_tbl)
str(SNP_tbl)

SNP_tbl[,apply(SNP_tbl, 2, var, na.rm=TRUE) != 0] # remove columns with zero variance
dim(SNP_tbl)

SNP_tbl[1:5, 1:5]

pca_fit <- SNP_tbl %>%
  #select(where(is.numeric)) %>%
  #scale() %>%
  prcomp()

summary(pca_fit)

COOP_lines_info_tbl <- read.csv("00_Data/COOP_CyBox/COOP - Analysis and Writing/Genotypic_Data/Processed/COOP_SGS_IDs.csv")
COOP_lines_info_tbl[1:3, 1:5]

COOP_lines_info_SNP_tbl <- SNP_matrix %>% 
  left_join(COOP_lines_info_tbl, by = c("X" = "SGS_Code")) %>% 
  rename("SGS_Code" = "X") %>%
  relocate(Identifier, Pedigree, SGS_Code, Program, Group) %>% 
  distinct(SGS_Code, .keep_all = TRUE) # remove duplicates?

COOP_lines_info_SNP_tbl[1:5, 1:7]

dim(COOP_lines_info_SNP_tbl)

pca_augmented <- augment(pca_fit, data = COOP_lines_info_SNP_tbl)
pca_augmented %>% glimpse() %>% head(5)

g <- pca_augmented %>%
  #filter(str_detect(Group, "BS39")) %>%
  rename_at(vars(starts_with(".fitted")),
            list(~str_replace(.,".fitted",""))) %>%
  ggplot(aes(x=PC1, 
             y=PC2,
             color = Group))+
  theme_bw()+
  geom_point()#+
  ggrepel::geom_text_repel(
    aes(label = ID), size = 3.5, 
    min.segment.length = 0, seed = 13, box.padding = 0.5, max.overlaps = Inf
  )
g

png("02_Figures/COOP_PCA_plot.png", width = 1000, height = 600, res = 150)
g
dev.off()
