library(tidyverse)
library(fs)

fs::dir_create("00_Data", "01_Scripts", "02_Figures", "03_Tables")

subdirs <- c("Iowa", "Illinois","SHGD_project", "REGENPGC")
dir_create("00_Data", subdirs)
fs::dir_tree()
fs::dir_ls()
dir_exists("00_Data/Illinois")
data_files <- dir_ls("00_Data", glob = "*is|*wa")
dir_delete(data_files)
dir_tree()
dir_create("00_Data", c("COOP_CyBox"))
dir_tree()
dir_create("04_Others")
## step 1----
library(usethis)
use_git()
create_github_token() 

## step 2----
library(gitcreds)
gitcreds_set()

## step 3----
usethis::use_github()
