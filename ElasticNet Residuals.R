
library(glmnet)
library(ggplot2)

# Define the model matrix for the test data
x <- model.matrix(Victims_Killed ~ Victims_Injured + Day_of_Week + School_Level + Students_Injured + Staff_Injured + Other_Weapons + Mental_Health_Concerns + Psychiatric_Medication + Prior_Hospitalization + School_Violence + Studied_Other_Shooters, data=data_test)[,-1]

# Obtain predictions for testing data from trained model
predict_test <- predict(elasticnet_mod2, newx=x)

# Ensure predict_test is a vector
predict_test <- as.vector(predict_test)

# Calculate residuals
residuals <- data_test$Victims_Killed - predict_test

# Create a data frame for plotting
residuals_df <- data.frame(Actual = data_test$Victims_Killed, Predicted = predict_test, Residuals = residuals)

# Plot residuals with custom dimensions
ggplot(residuals_df, aes(x = Predicted, y = Residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals of Elastic Net Model",
       x = "Predicted Values",
       y = "Residuals") +
  theme_minimal() +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))
