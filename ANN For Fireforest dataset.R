##### Neural Networks -------------------
View(forestfires)
# custom normalization function
normalise <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}

# apply normalization to required columns in  data frame
forestfires$temp <- normalise(forestfires$temp)
forestfires$rain <- normalise(forestfires$rain)
forestfires$RH <- normalise(forestfires$RH)
forestfires$wind <- normalise(forestfires$wind)

sum(forestfires$area<5) ## 366 
sum(forestfires$area>5) ## 151 

forestfires$size_category <- NULL

forestfires$size_category <- factor(ifelse(forestfires$area < 5, 0, 1),
                      labels = c("small", "large"))

# create training and test data
forestfires_train <- forestfires[1:413, ]
forestfires_test <- forestfires[414:517, ]

View(forestfires_test)


## Training a model on the data ----
# train the neuralnet model
install.packages("neuralnet")
library(NeuralNetTools)
library(neuralnet)
library(caret)
# simple ANN with only a single hidden neuron
forestfires_model <- neuralnet(formula = size_category ~rain+RH+wind+temp,
                            data = forestfires_train)


# visualize the network topology
plot(forestfires_model)

## Evaluating model performance 

----
  # obtain model results
  
  #results_model <- NULL
forestfires_test[7:10]
results_model <- compute(forestfires_model, forestfires_test[7:10])
# obtain predicted strength values
str(results_model)
predicted_size_category <- results_model$net.result
predicted_size_category
# examine the correlation between predicted and actual values
cor(predicted_size_category, forestfires_test$size_category)



### For single hidden layer only my model gave to me 91.18% Accuracy 


## Tried different Hidden layers and observed the  model performance ----
# a more complex neural network topology with 5 hidden neurons
forestfires_model2 <- neuralnet(size_category.small ~.,
                             data = forestfires_train, hidden = 5)


# plot the network
plot(forestfires_model2)

# evaluate the results as we did before
model_results2 <- compute(forestfires_model2, forestfires_test[1:45])
predicted_size_category2 <- model_results2$net.result
cor(predicted_size_category2, forestfires_test$size_category.small)
 
###  NOTE: if i will take Hidden Layers =5 , my model accuracy is 62.60% 

############## HIDDEN LAYERS = 3 ###########
forestfires_model3 <- neuralnet(size_category.small ~.,
                                data = forestfires_train, hidden = 3)

###  If i Will take Hidden Layers =3,  my Model Accuracy is 81.55%  



############## HIDDEN LAYERS = 10 ###########
forestfires_model3 <- neuralnet(size_category.small ~.,
                                data = forestfires_train, hidden = 10)

###  If i Will take Hidden Layers =1,  my Model Accuracy is 64.59% 