########### Neural Networks #############

View(startup)
install.packages("NeuralNetTools")
library(NeuralNetTools)
library(plyr)

 ### in my data set  state column convert into numeric ###
startup$State <- as.numeric(revalue(startup$State,
                                     c("New York"="0", "California"="1",
                                       "Florida"="2")))
str(startup)

startup <- as.data.frame(startup)
attach(startup)

 ### Exploratory data analysis ### 
summary(startup)
plot(State,Profit)
plot(R.D.Spend,Profit)
plot(Administration,Profit)
plot(Marketing.Spend,Profit)

# Find the correlation between Output (Profit) & inputs (R.D Spend, Administration, Marketing, State) - SCATTER DIAGRAM
pairs(startup)

# Correlation coefficient - Strength & Direction of correlation
cor(startup)

# Apply Normalization technique to the whole dataset :

normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
startup_norm<-as.data.frame(lapply(startup,normalize))
summary(startup_norm$Profit) # Normalized form of profit

# create training and test data
startup_train <- startup_norm[1:40, ]
startup_test <- startup_norm[41:50, ]

# train the neuralnet model
install.packages("neuralnet")
library(neuralnet)
library(caret)
# simple ANN with only a single hidden neuron
startup_model <- neuralnet(Profit ~ R.D.Spend+Administration+Marketing.Spend+State,
                               data = startup_train,hidden = 20)  
str(startup_model)

# visualize the network topology
plot(startup_model)

## Evaluating model performance 

----
  # obtain model results
  
  #results_model <- NULL
startup_test[1:4]
results_model <- compute(startup_model, startup_test[1:4])
predicted_Profit<-results_model$net.result
cor(predicted_Profit,startup_test$Profit)


#### CONCLUSION : Hidden layers 20 my model gave 90.22% Accuracy 


