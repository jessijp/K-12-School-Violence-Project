library(ggplot2)
library(sandwich)
library(regclass)
library(vip)
library(MASS)

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
m2 <- glm(Victims_Killed~Victims_Injured+ Day_of_Week+ School_Level+ Students_Injured+ Staff_Injured+ Other_Weapons +Mental_Health_Concerns+ Psychiatric_Medication+ Prior_Hospitalization+ School_Violence+ Studied_Other_Shooters,  family="poisson", data=data_train)

#Sort importances in decreasing order and plot them   
imp <- vi_firm(m2, train=data_train)

#Sort importances in decreasing order and plot them   
imp <- imp[order(imp$Importance, decreasing = T),]
vip::vip(imp)
imp

# Create wrapper for VIP
pfun <- function(object, newdata) {
  # Return averaged prediction for class of interest
  as.vector(predict(object, newdata = newdata, type="response"))
}

feature_names=c("Victims_Injured", "Day_of_Week","School_Level","Students_Injured","Staff_Injured","Other_Weapons","Mental_Health_Concerns","Psychiatric_Medication","Prior_Hospitalization","School_Violence","Studied_Other_Shooters")

imp2 <- vip::vi_permute(m2, feature_names=feature_names,
                        train=data_train,target=data_train$Victims_Killed,metric="rsq",pred_wrapper =  pfun)
#Sort importances in decreasing order and plot them   
imp2 <- imp2[order(imp2$Importance, decreasing = T),]
vip::vip(imp2)
imp2

#####For residuals
plot(m2)

#Test MSPE
pred=predict(m2, newdata=data_test, type="response")
res=data_test$Victims_Killed-pred
mean(res^2)