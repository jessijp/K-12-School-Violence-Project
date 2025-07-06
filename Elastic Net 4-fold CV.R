library(glmnet)
set.seed(15) 
rm(list=ls()) 
k12 <- read.table("K-12 Homicide Feb 23.csv", header=T, sep=",", row.names=1) 
attach(k12)
nrow <- dim(k12)[[1]]  
data_rows <- sample(1:nrow, replace=F) 
data <- k12[data_rows,]  
n_test <- ceiling(nrow/5)  
data_test <- data[1:n_test,] 
data_test <- data.frame(data_test)  
new=n_test+1 
data_train=data[c( new: nrow),]  
data_train= data.frame(data_train) 
#Check dimensions  
dim(data_test)  
dim(data_train) 
grids <- expand.grid(alpha=seq(0,1,0.1),lambda=seq(0,100,1)) 
#Split data into K=4 parts 
#Create vector to store K=4 generalization errors 
MSPE <- matrix(, nrow=dim(grids)[[1]], ncol=4)  
folds <- cut(seq(1,nrow(data_train)),breaks=4,labels=FALSE) 
for(i in 1:4){ 
  picked =which(folds==i,arr.ind=TRUE)  
  testing_sample <- data_train[picked, ]  
  training_sample <- data_train[-picked, ]  
  sample_size_train = nrow(training_sample) 
  sample_size_test=nrow(testing_sample)
  #Define the model equation  
  X <- model.matrix(Victims_Killed~Victims_Injured +Day_of_Week+ School_Level+ Students_Injured+ Staff_Injured+ Other_Weapons +Mental_Health_Concerns+ Psychiatric_Medication+ Prior_Hospitalization+ School_Violence+ Studied_Other_Shooters, data=training_sample)[,-1] 
  #Define response variable
  Y <- training_sample[,"Victims_Killed"] 
  x <- model.matrix(Victims_Killed~Victims_Injured +Day_of_Week+ School_Level+ Students_Injured+ Staff_Injured+ Other_Weapons +Mental_Health_Concerns+ Psychiatric_Medication+ Prior_Hospitalization+ School_Violence+ Studied_Other_Shooters, data=testing_sample)[,-1] 
  for(j in 1: dim(grids)[[1]]) { 
    alpha_value=grids[j,1]  
    lambda_value= grids[j,2] 
    # Lasso regression is fit to the training dataset 
    elasticnet_mod <- glmnet(x=X, y=Y,alpha = alpha_value,lambda =lambda_value, standardize=TRUE, data=training_sample) 
    #Obtain predictions for testing data from trained model 
    predict_test<-predict(elasticnet_mod , newx=x ) 
    # Compute testing error (MSPE) for kth fold 
    MSPE[j,i]=sum((predict_test-testing_sample$Victims_Killed)^2)/(sample_size_test) 
  } 
} 
rowMeans(MSPE) 
select=which.min(rowMeans(MSPE))  
optimal=grids[select,]  
optimal
#Fit best model to full data 
X <- model.matrix(Victims_Killed~Victims_Injured +Day_of_Week+ School_Level+ Students_Injured+ Staff_Injured+ Other_Weapons +Mental_Health_Concerns+ Psychiatric_Medication+ Prior_Hospitalization+ School_Violence+ Studied_Other_Shooters, data=data_train)[,-1] 
#define response variable 
Y <- data_train[,"Victims_Killed"] 
#Obtain MSPE for test  
x <- model.matrix(Victims_Killed~Victims_Injured +Day_of_Week+ School_Level+ Students_Injured+ Staff_Injured+ Other_Weapons +Mental_Health_Concerns+ Psychiatric_Medication+ Prior_Hospitalization+ School_Violence+ Studied_Other_Shooters, data=data_test)[,-1]  
elasticnet_mod2 <- glmnet(x=X, y=Y,alpha = optimal[[1]],lambda =optimal[[2]], standardize=TRUE, data=data_train) 
#Obtain predictions for testing data from trained model  
predict_test<-predict(elasticnet_mod2 , newx=x) 
#Compute testing error (MSPE) for kth fold 
MSPE_best=sum((predict_test-data_test$Victims_Killed)^2)/(dim(data_test)[[1]]) 
MSPE_best 