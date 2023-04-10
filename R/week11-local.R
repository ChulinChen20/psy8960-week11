# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
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
  # Remove the other two “work hours” variables
  select(-HRS2, -MOSTHRS)   %>%
  filter(is.na(HRS1) != T) %>%
  rename(workhours = HRS1) %>%
  select(where(~mean(is.na(.)) < 0.75)) %>%
  mutate(workhours = as.numeric(workhours)) 

gss_tbl %>% 
  ggplot(aes(workhours)) +
  geom_freqpoly()

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

table1_tbl <- c()
table2_tbl <- c()

# fit models
for(i in algo) {
  set.seed(12)
  
  #original model
  tic()
  model1 <- train(
    workhours~.,
    gss_train,
    method = i,
    na.action = na.pass,
    tuneGrid = grids(i),
    preProcess = c("center","scale","nzv","medianImpute"),
    trControl=trainControl(method="cv",number=2, verboseIter=T)
  )
  t1<- toc()
  
  # turn on parallel processing
  local_cluster <- makeCluster(7)
  registerDoParallel(local_cluster)
  
  # parallelized models
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

  # turn off parallel processing
  stopCluster(local_cluster)
  registerDoSEQ()
  
  # Publication
  cv_rsq <- max(model1$results$Rsquared)
  ho_rsq <- cor(predict(model1, gss_test,na.action = na.pass),gss_test$workhours)^2
  table <- cbind(i,str_remove(format(round(cv_rsq,2),nsmall=2),"^0"),str_remove(format(round(ho_rsq,2),nsmall=2),"^0"))
  table1_tbl  <- rbind(table1_tbl ,table)
  colnames(table1_tbl) <- c("algo","cv_rsq","ho_rsq")
  
  # combine algorithm names, time taken for original, time taken for parallelized into a table
  original <- t1$toc - t1$tic
  parallelized <- t2$toc - t2$tic
  print(original)
  print(parallelized)
  table2 <- cbind(i,original,parallelized)
  table2_tbl <- rbind(table2_tbl,table2)
  colnames(table2_tbl) <- c("algo","original","parallelized")
}

tibble(table1_tbl)
tibble(table2_tbl)

# Q1: R-squared values vary across different models. Elastic net performs the best in both cross validation and holdout data, whilst linear regression performs the worst. This is because different models differ mathematically and also use different regulization methods.
# Q2: In general, the models perform better on the holdout sample, which implies that the models do not overfit and predict unseen data well.
# Q3: I will choose Elastic net as they perform the best on both training and testing data. XGBoost's performance is very close to elastic net's, it might perform better if more refined hyperparameter tuning is applied, so it is a good option too. No tradeoffs. 