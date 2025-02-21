library(tidyverse)

# 1. GP_data_wrangling----
# 2. GP_Pheno_RanFixef----
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


# 3. GP_Kfold_CV----
GP_rrBLUP_CV <- function(Training_data, Target_Y_col = "BLUE", ID_col = "Ident", Repeat = 5, Fold = 5){
  
  PA<- matrix(NA, nrow= Fold, ncol= Repeat)
  u <- matrix(NA, nrow= ncol(Training_data)-2, ncol= Fold) # two columns are ID and Target_Y_col
  ME <- matrix(NA, nrow= ncol(Training_data)-2, ncol= Repeat)
  
  ## Repeat K-fold cross-validation
  cat("Cross-validation started... \n")
  start_time <- Sys.time()
  
  for(j in 1:Repeat){
    
    set.seed(0727+j)
    CV <- rep(1:Fold, round(nrow(Training_data)/Fold)+1)[order(runif(round(nrow(Training_data))))]
    for(i in 1:Fold){
      
      Test <- which(CV == i) 
      
      Train_Pheno <- Training_data %>% 
        column_to_rownames(var = ID_col) %>%
        select(Target_Y_col) %>%
        # filter out the rows that are in the test set
        slice(-Test) %>% 
        pull()
      
      Test_Pheno <- Training_data %>% 
        column_to_rownames(var = ID_col) %>%
        select(Target_Y_col) %>%
        slice(Test)
      
      Train_Geno <- Training_data %>% 
        column_to_rownames(var = ID_col) %>%
        select(!contains("EF")) %>% 
        slice(-Test) %>% 
        as.matrix()
      
      Test_Geno <- Training_data %>% 
        column_to_rownames(var = ID_col) %>%
        select(!contains("EF")) %>% 
        slice(Test) %>% 
        as.matrix()
      
      #Test_Pheno <- Pheno[Test, Target_Y_col]
      #Train_Pheno <- Pheno[-Test, Target_Y_col]
      #Test_Geno <- as.matrix(Geno[Test, ])
      #Train_Geno <- as.matrix(Geno[-Test, ])
      cat(paste0(j,"-repeat ", i, "-fold running... \n"))
      fold_start_time <- Sys.time()
      
      fit <- rrBLUP::mixed.solve(y = Train_Pheno, Z = Train_Geno) ##rr_BLUP
      
      PredGV <- Test_Geno %*% fit$u
      
      PA[i,j]<- cor(PredGV, Test_Pheno) # rmp, prediction ability 
      
      cat(paste0(j,"-repeat ", i, "-fold done \n"))
      fold_end_time <- Sys.time()
      cat("Running time:", as.numeric(fold_end_time - fold_start_time, units = "mins"), "mins", "\n")
    }
  }
  cat("Cross-validation finished \n")
  A <- apply(PA, 2, mean)
  
  end_time <- Sys.time()
  cat("CV time in minutes:", as.numeric(end_time - start_time, units = "mins"), "\n")
  Running_time <- end_time-start_time
  
  out <- list(Accuracy = A, Time = Running_time)
  
  return(out)
}

GP_rrBLUP_stratified_CV <- function(Training_data, Target_Y_col = "BLUE", ID_col = "Ident", Cluster = 2, Repeat = 5, Fold = 5){
  
  library(cluster)
  
  Geno <- Training_data %>% 
    column_to_rownames(var = ID_col) %>%
    select(!contains(Target_Y_col)) %>%
    as.matrix()
  
  X <- as.matrix(Geno)
  clarax <- clara(X, k = Cluster, metric = c("manhattan") )
  Training_data <- Training_data %>% 
    mutate(Cluster=as.factor(clarax$clustering))
  
  PA<- matrix(NA, nrow= Fold, ncol= Repeat)
  u <- matrix(NA, nrow= ncol(Training_data)-2, ncol= Fold) # two columns are ID and Target_Y_col
  ME <- matrix(NA, nrow= ncol(Training_data)-2, ncol= Repeat)
  
  library(splitTools)
  
  ## Repeat K-fold cross-validation
  cat("Cross-validation started... \n")
  start_time <- Sys.time()
  
  for(j in 1:Repeat){
    
    
    set.seed(0727+j)
    folds <- create_folds(Training_data$Cluster, k = Fold)
    
    for(i in 1:Fold){
      
      Train <- folds[[i]] 
      #Test_Pheno <- Pheno[-Train, 2]
      #Train_Pheno <- Pheno[Train, 2]
      
      #Test_Geno <- as.matrix(Geno[-Train, ])
      #Train_Geno <- as.matrix(Geno[Train, ])
      
      Train_Pheno <- Training_data %>% 
        column_to_rownames(var = ID_col) %>%
        select(Target_Y_col) %>%
        # filter out the rows that are in the test set
        slice(Train) %>% 
        pull()
      
      Test_Pheno <- Training_data %>% 
        column_to_rownames(var = ID_col) %>%
        select(Target_Y_col) %>%
        slice(-Train)
      
      Train_Geno <- Training_data %>% 
        column_to_rownames(var = ID_col) %>%
        select(!contains(Target_Y_col)) %>% 
        select(!contains("Cluster")) %>%
        slice(Train) %>% 
        as.matrix()
      
      Test_Geno <- Training_data %>% 
        column_to_rownames(var = ID_col) %>%
        select(!contains(Target_Y_col)) %>% 
        select(!contains("Cluster")) %>%
        slice(-Train) %>% 
        as.matrix()
      
      cat(paste0(j,"-repeat ", i, "-fold running... \n"))
      fold_start_time <- Sys.time()
      
      fit <- rrBLUP::mixed.solve(y = Train_Pheno, Z = Train_Geno) ##rr_BLUP
      
      PredGV <- Test_Geno %*% fit$u
      
      PA[i,j]<- cor(PredGV, Test_Pheno) # rmp, prediction ability 
      
      cat(paste0(j,"-repeat ", i, "-fold done \n"))
      fold_end_time <- Sys.time()
      cat("Running time:", as.numeric(fold_end_time - fold_start_time, units = "mins"), "mins", "\n")
      
    } 
  }
  
  cat("Cross-validation finished \n")
  A <- apply(PA, 2, mean)
  
  end_time <- Sys.time()
  cat("CV time in minutes:", as.numeric(end_time - start_time, units = "mins"), "\n")
  Running_time <- end_time-start_time
  
  out <- list(Accuracy = A, Time = Running_time)
  return(out)
  
}


GP_BGLR_CV <- function(Training_data, model = "BayesB", Target_Y_col = "BLUE", ID_col = "Ident", Repeat = 5, Fold = 5){
  
  library(BGLR)
  PA<- matrix(NA, nrow= Fold, ncol= Repeat)
  u <- matrix(NA, nrow= ncol(Training_data)-2, ncol= Fold)
  ME <- matrix(NA, nrow= ncol(Training_data)-2, ncol= Repeat)
  
  ## Repeat K-fold cross-validation
  cat("Cross-validation started... \n")
  start_time <- Sys.time()
  
  for(j in 1:Repeat){
    
    set.seed(0727+j)
    CV <- rep(1:Fold, round(nrow(Training_data)/Fold)+1)[order(runif(round(nrow(Training_data))))]
    for(i in 1:Fold){
      
      Test <- which(CV == i) 
      
      Train_Pheno <- Training_data %>% 
        column_to_rownames(var = ID_col) %>%
        select(Target_Y_col) %>%
        # filter out the rows that are in the test set
        slice(-Test) %>% 
        pull()
      
      Test_Pheno <- Training_data %>% 
        column_to_rownames(var = ID_col) %>%
        select(Target_Y_col) %>%
        slice(Test)
      
      Train_Geno <- Training_data %>% 
        column_to_rownames(var = ID_col) %>%
        select(!contains("EF")) %>% 
        slice(-Test) %>% 
        as.matrix()
      
      Test_Geno <- Training_data %>% 
        column_to_rownames(var = ID_col) %>%
        select(!contains("EF")) %>% 
        slice(Test) %>% 
        as.matrix()
      
      cat(paste0(j,"-repeat ", i, "-fold running... \n"))
      fold_start_time <- Sys.time()
      
      fm=BGLR(y=Train_Pheno, ETA=list(list(X=Train_Geno, model=model)),
              nIter=6000, burnIn=1000, verbose = FALSE) ##BGLR
      
      PredGV <- Test_Geno %*% fm$ETA[[1]]$b
      
      PA[i,j]<- cor(PredGV, Test_Pheno) # rmp, prediction ability 
      
      cat(paste0(j,"-repeat ", i, "-fold done \n"))
      fold_end_time <- Sys.time()
      cat("Running time:", as.numeric(fold_end_time - fold_start_time, units = "mins"), "mins", "\n")
    }
  }
  cat("Cross-validation finished \n")
  A <- apply(PA, 2, mean)
  
  end_time <- Sys.time()
  cat("CV time in minutes:", as.numeric(end_time - start_time, units = "mins"), "\n")
  Running_time <- end_time-start_time
  
  out <- list(Accuracy = A, Time = Running_time)
  
  return(out)
}


acc_summaryer <- function(vector){
  
  out <- data.frame(mean = mean(vector), sd = sd(vector))
  return(out)
  
}

# 4. Genomic prediction----

GP_rrBLUP <- function(Training_data, Prediction_SNP_tbl, Target_Y_col = "BLUE", ID_col = "Ident"){
  
  Train_Pheno <- Training_data %>% 
    column_to_rownames(var = ID_col) %>%
    select(Target_Y_col) %>%
    pull()
  
  Train_Geno <- Training_data %>% 
    column_to_rownames(var = ID_col) %>%
    select(!contains(Target_Y_col)) %>% 
    as.matrix()
  
  Prediction_Geno <- Prediction_SNP_tbl %>%
    column_to_rownames(var = ID_col) %>%
    as.matrix()
  
  fit <- rrBLUP::mixed.solve(y = Train_Pheno, Z = Train_Geno) ##rr_BLUP
  
  PredGV <- Prediction_Geno %*% fit$u 
  
  Mkref <- fit$u
  
  out <- list(Prediction = PredGV, Marker_effect = Mkref)
  
  return(out)
  
}

GP_BGLR <- function(Training_data, Prediction_SNP_tbl, model = "BayesB", Target_Y_col = "BLUE", ID_col = "Ident"){
  
  library(BGLR)
  
  Train_Pheno <- Training_data %>% 
    column_to_rownames(var = ID_col) %>%
    select(Target_Y_col) %>%
    pull()
  
  Train_Geno <- Training_data %>% 
    column_to_rownames(var = ID_col) %>%
    select(!contains(Target_Y_col)) %>% 
    as.matrix()
  
  Prediction_Geno <- Prediction_SNP_tbl %>%
    column_to_rownames(var = ID_col) %>%
    as.matrix()
  
  fm=BGLR(y=Train_Pheno, ETA=list(list(X=Train_Geno, model=model)),
          nIter=6000, burnIn=1000, verbose = FALSE) ##BGLR
  
  PredGV <- Prediction_Geno %*% fm$ETA[[1]]$b
  
  Mkref <- fm$ETA[[1]]$b
  
  out <- list(Prediction = PredGV, Marker_effect = Mkref)
  
  return(out)
  
}

# 5. GP_line_selection----

GP_results_tbl_generator <- function(Result_list, Name_col = "LineID", Value_col = "Predicted_value"){
  
  out <- Result_list %>% 
    pluck(1) %>%
    as.data.frame() %>% 
    rownames_to_column(var = Name_col) %>%
    rename(Predicted_Value = V1)
  
  return(out)
}
