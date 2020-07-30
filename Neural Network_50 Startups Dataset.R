
startups <- read.csv('D:\\Data Science\\Excelr\\Assignments\\Assignment\\Neural Networks\\50_Startups_r.csv')

# custom normalization function
normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}
# apply normalization to entire data frame
startups_norm <- as.data.frame(lapply(startups[2:5], normalize))
# create training and test data
startups_train <- startups_norm[1:35, ]
startups_test <- startups_norm[36:50, ]
## Training a model on the data ----
# train the neuralnet model
library(neuralnet)
# simple ANN with only a single hidden neuron
startups_model <- neuralnet(formula = Profit ~ R.D.Spend + Administration +
                              Marketing.Spend ,
                            data = startups_train)
# visualize the network topology
windows()
plot(startups_model)
## Evaluating model performance ----
# obtain model results
model_results <- compute(startups_model, startups_test)
# obtain predicted strength values
predicted_profit <- model_results$net.result
# examine the correlation between predicted and actual values
cor(predicted_profit, startups_test$strength)
## Improving model performance ----
# a more complex neural network topology with 5 hidden neurons
startups_model2 <- neuralnet(Profit ~ R.D.Spend + Administration +
                               Marketing.Spend ,
                             data = startups_train, hidden =c(5,2))
# plot the network
windows()
plot(startups_model2)
# evaluate the results as we did before
model_results2 <- compute(startups_model2, startups_test)
predicted_profit2 <- model_results2$net.result
cor(predicted_profit2, startups_test$Profit)
