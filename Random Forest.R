
library(randomForest)
set.seed(15)
rm(list=ls())
k12 <- read.table("K-12 Homicide Feb 23.csv", header=T, sep=",", row.names=1)
nrow <- dim(k12)[[1]]
data_rows <- sample(1:nrow, replace=F)
data <- k12[data_rows,]
n_test <- ceiling(nrow/5)
data_test <- data[1:n_test,]
data_test <- data.frame(data_test)
new=n_test+1
data_train=data[c(new: nrow),]
data_train= data.frame(data_train)
# Check dimensions
dim(data_test)
dim(data_train)

# Define grid search parameters
grids <- expand.grid(mtry=seq(1, ncol(data_train)-1, by=1), nodesize=seq(1, 10, by=1))

# Split data into K=4 parts
MSPE <- matrix(, nrow=dim(grids)[[1]], ncol=4)
folds <- cut(seq(1,nrow(data_train)),breaks=4,labels=FALSE)

for(i in 1:4){
  picked = which(folds==i, arr.ind=TRUE)
  testing_sample <- data_train[picked, ]
  training_sample <- data_train[-picked, ]
  sample_size_train = nrow(training_sample)
  sample_size_test = nrow(testing_sample)
  
  for(j in 1:dim(grids)[[1]]) {
    mtry_value = grids[j, 1]
    nodesize_value = grids[j, 2]
    
    # Fit random forest model to the training dataset
    rf_mod <- randomForest(Victims_Killed ~ Victims_Injured + Day_of_Week + School_Level + Students_Injured + Staff_Injured + Other_Weapons + Mental_Health_Concerns + Psychiatric_Medication + Prior_Hospitalization + School_Violence + Studied_Other_Shooters, 
                           data=training_sample, mtry=mtry_value, nodesize=nodesize_value, ntree=500)
    
    # Obtain predictions for testing data from trained model
    predict_test <- predict(rf_mod, newdata=testing_sample)
    
    # Compute testing error (MSPE) for kth fold
    MSPE[j, i] = sum((predict_test - testing_sample$Victims_Killed)^2) / sample_size_test
  }
}

rowMeans(MSPE)
select = which.min(rowMeans(MSPE))
optimal = grids[select,]
optimal

# Fit best model to full data
rf_mod2 <- randomForest(Victims_Killed ~ Victims_Injured + Day_of_Week + School_Level + Students_Injured + Staff_Injured + Other_Weapons + Mental_Health_Concerns + Psychiatric_Medication + Prior_Hospitalization + School_Violence + Studied_Other_Shooters, 
                        data=data_train, mtry=optimal[[1]], nodesize=optimal[[2]], ntree=500)

# Obtain predictions for testing data from trained model
predict_test <- predict(rf_mod2, newdata=data_test)

# Compute testing error (MSPE) for test data
MSPE_best = sum((predict_test - data_test$Victims_Killed)^2) / dim(data_test)[[1]]
MSPE_best
