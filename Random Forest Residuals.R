
library(randomForest)
library(ggplot2)

# Obtain predictions for testing data from trained model
predict_test <- predict(rf_mod2, newdata=data_test)

# Calculate residuals
residuals <- data_test$Victims_Killed - predict_test

# Create a data frame for plotting
residuals_df <- data.frame(Actual = data_test$Victims_Killed, Predicted = predict_test, Residuals = residuals)

# Plot residuals
ggplot(residuals_df, aes(x = Predicted, y = Residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals of Random Forest Model",
       x = "Predicted Values",
       y = "Residuals") +
  theme_minimal()
