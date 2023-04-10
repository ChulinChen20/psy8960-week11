# Script Settings and Resources
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(haven)
library(caret)
library(mlbench)
library(tictoc)
library(parallel)
library(doParallel)

# Data Import and Cleaning
gss <- read_sav("../data/GSS2016.sav")

gss_tbl<- gss  %>%
  select(-HRS2, -MOSTHRS)   %>%
  filter(is.na(HRS1) != T) %>%
  rename(workhours = HRS1) %>%
  select(where(~mean(is.na(.)) < 0.75)) %>%
  mutate(workhours = as.numeric(workhours)) 

# Analysis
split <- round(nrow(gss_tbl) * 0.75)

gss_train <- gss_tbl[1:split, ]

gss_test <- gss_tbl[(split + 1):nrow(gss_tbl), ]

algo <- c("lm", "glmnet", "ranger", "xgbTree") 

grids <- function(x) {
  if(x == "lm") {
    tuneGrid = NULL
  } else if(x=="glmnet") {
    tuneGrid = expand.grid(
      alpha=0:1,
      lambda=seq(0.0001,1,length=20)
    )
  } else if(x=="ranger"){
    
    tuneGrid = data.frame(
      .mtry = c(2,3,7),
      .splitrule = "variance",
      .min.node.size = 5
    )
  } else  {
    tuneGrid = expand.grid(
      nrounds = 100,
      eta = c(0.01, 0.001, 0.0001),
      max_depth = c(2, 4, 6, 8),
      gamma = 0, 
      subsample = 1,
      min_child_weight = c(1, 2, 3), 
      colsample_bytree = 1
    )
  }
}

Table3 <- c()
Table4 <- c()


for(i in algo) {
  set.seed(12)
  

  tic()
  model1 <- train(
    workhours~.,
    gss_train,
    method = i,
    na.action = na.pass,
    tuneGrid = grids(i),
    preProcess = c("center","scale","nzv","medianImpute"),
    trControl=trainControl(method="cv",number=10, verboseIter=T)
  )
  t1<- toc()
  

  local_cluster <- makeCluster(7)
  registerDoParallel(local_cluster)
  

  tic()
  model2 <- train(
    workhours~.,
    gss_train,
    method = i,
    na.action = na.pass,
    tuneGrid = grids(i),
    preProcess = c("center","scale","nzv","medianImpute"),
    trControl=trainControl(method="cv",number=10, verboseIter=T)
  )
  t2<- toc()
  

  stopCluster(local_cluster)
  registerDoSEQ()
  
  # Publication
  cv_rsq <- max(model1$results$Rsquared)
  ho_rsq <- cor(predict(model1, gss_test,na.action = na.pass),gss_test$workhours)^2
  table <- cbind(i,str_remove(format(round(cv_rsq,2),nsmall=2),"^0"),str_remove(format(round(ho_rsq,2),nsmall=2),"^0"))
  Table3  <- rbind(Table3,table)
  colnames(Table3) <- c("algo","cv_rsq","ho_rsq")
  
  original <- t1$toc - t1$tic
  parallelized <- t2$toc - t2$tic
  table2 <- cbind(i,original,parallelized)
  Table4 <- rbind(Table4,table2)
  colnames(Table4) <- c("algo","supercomputer","supercomputer_7")
}

tibble(Table3)
tibble(Table4)

write_csv(data.frame(Table3),"../out/table3.csv")
write_csv(data.frame(Table4),"../out/table4.csv")

