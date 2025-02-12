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

pca_fit <- SNP_tbl %>%
  select(where(is.numeric)) %>%
  #scale() %>%
  prcomp()

summary(pca_fit)

pca_augmented <- augment(pca_fit, data = SNP_binary_tbl[-c(1:4), ] %>% rename("ID" = "X"))
pca_augmented %>% glimpse()

g <- pca_augmented %>%
  rename_at(vars(starts_with(".fitted")),
            list(~str_replace(.,".fitted",""))) %>%
  ggplot(aes(x=PC1, 
             y=PC2
             ))+
  theme_bw()+
  geom_point()+
  ggrepel::geom_text_repel(
    aes(label = ID), size = 3.5, 
    min.segment.length = 0, seed = 13, box.padding = 0.5, max.overlaps = Inf
  )
g

png("02_Figures/29lines_PCA_plot.png", width = 1000, height = 600, res = 150)
g
dev.off()
