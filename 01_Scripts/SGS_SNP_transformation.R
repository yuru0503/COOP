library(tidyverse)
library(gt)

SGS_genotypic_data <- read.csv("00_Data/COOP_CyBox/COOP - Analysis and Writing/Genotypic_Data/Processed/Combined_COOP_SGS_Markers.csv")
SGS_genotypic_data[1:3, 1:5]

SGS_SNP_info_tbl <- read.csv("00_Data/COOP_CyBox/COOP - Analysis and Writing/Genotypic_Data/SGS_SNP_information.csv")
SGS_SNP_info_tbl[1:3, ]
dim(SGS_SNP_info_tbl)

COOP_lines_info_tbl <- read.csv("00_Data/COOP_CyBox/COOP - Analysis and Writing/Genotypic_Data/Processed/COOP_SGS_IDs.csv")
COOP_lines_info_tbl[1:3, 1:5]

COOP_lines_info_tbl %>% 
  filter(str_detect(Program, "Thomas")) %>% 
  filter(!str_detect(Pedigree, "haploid")) %>% 
  gt()

SNP_Geno_tbl <- SGS_SNP_info_tbl %>%
  left_join(SGS_genotypic_data, by = c("Name"="Identifier")) %>% 
  rename("rs" = "Name", "chrom" = "CROM_V5", "pos" = "POSphysV5") %>% # using version 5
  select(!any_of(c("CHROM_V4", "POSphysV4","IBMCHR","IBMPOS", "Marker.used.2024"))) %>% 
  #drop NA rows
  filter(!is.na(chrom)) 

SNP_Geno_tbl[1:5, 1:7]
dim(SNP_Geno_tbl)

# Gather the allele columns into key-value pairs
long_data <- SNP_Geno_tbl %>%
  pivot_longer(cols = -(rs:pos), names_to = 'Sample', values_to = 'Allele')

# Calculate the major allele for each row
major_allele_df <- long_data %>%
  group_by(rs, chrom, pos) %>%
  summarise(Major_Allele = Allele[which.max(table(Allele))])

# Merge the major allele information back to the original data
result_df <- major_allele_df %>%
  left_join(SNP_Geno_tbl, by = c('rs', 'chrom', 'pos')) %>% 
  ungroup() 

result_df %>% 
  str()

# convert the allele columns to 0/1
SNP_Binary_tbl <- result_df %>%
  mutate(across(!contains(c("rs", "chrom", "pos", "Major_Allele")), ~as.integer(.x == Major_Allele))) %>% 
  select(-Major_Allele) %>% 
  mutate_at(vars(-c('rs', 'chrom', 'pos')), as.numeric)

SNP_Binary_tbl[1:5, 1:7]
SNP_Binary_tbl[is.na(SNP_Binary_tbl)] <- 0.5

write.csv(SNP_Binary_tbl, "00_Data_processed/COOP_SNP_binary_genotype.csv", row.names = FALSE)

SNP_matrix <- SNP_Binary_tbl %>% 
  arrange(chrom, pos) %>% 
  mutate(SNP = str_glue("S{chrom}_{pos}"), .keep = "unused") %>% 
  column_to_rownames("SNP") %>%
  select(-rs) %>% 
  mutate_all(as.double) %>%
  t()
SNP_matrix[1:5, 1:5]

SNP_matrix[SNP_matrix == 0.5] <- 1
SNP_matrix[SNP_matrix == 1] <- 2
SNP_matrix[SNP_matrix == 0] <- 0
dim(SNP_matrix)
write.csv(SNP_matrix, "00_Data_processed/COOP_SNP_matrix.csv", row.names = FALSE)
