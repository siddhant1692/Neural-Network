
forestfires <- read.csv('D:\\Data Science\\Excelr\\Assignments\\Assignment\\Neural Networks\\forestfires_r1.csv')

# custom normalization function
normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}
# apply normalization to entire data frame
forestfires_norm1 <- as.data.frame(lapply(forestfires[3:31], normalize))
# create training and test data
forestfires_train <- forestfires_norm1[1:361, ]
forestfires_test <- forestfires_norm1[362:517, ]
## Training a model on the data ----
# train the neuralnet model
library(neuralnet)
# simple ANN with only a single hidden neuron   

forestfires_model <- neuralnet(size_category ~ FFMC + DMC + DC + ISI + temp + RH + wind + rain + area,
                               data = forestfires_train,
                               linear.output = FALSE, 
                               err.fct = 'ce',
                               likelihood = TRUE)
# visualize the network topology
plot(forestfires_model)
## Evaluating model performance ----
# obtain model results
model_results <- compute(forestfires_model, forestfires_test)
# obtain predicted strength values
predicted_size <- model_results$net.result
# examine the correlation between predicted and actual values
cor(predicted_size, forestfires_test$size_category)
## Improving model performance ----
# a more complex neural network topology with 5 hidden neurons
forestfires_model2 <- neuralnet(size_category ~ FFMC + DMC + DC + ISI + temp + RH + wind + rain + area,
                                data = forestfires_train,
                                linear.output = FALSE, 
                                err.fct = 'ce',
                                likelihood = TRUE, hidden =c(5,2))
# plot the network
plot(forestfires_model2)
# evaluate the results as we did before
model_results2 <- compute(forestfires_model2, forestfires_test)
predicted_size2 <- model_results2$net.result
cor(predicted_size2, forestfires_test$size_category)
