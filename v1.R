rm(list=ls())

console <- function(){
cat(" select the data set containg records:\n")
data_set= read.csv(file.choose(), stringsAsFactors = TRUE)
cat("data set selected \n\n\n---------------------\n")
cat("summary of data set\n")
cat(" 'B' in diagnosis column mean  benign tumors, non-cancerous tumors\n")
cat(" 'M' in diagnosis column mean  malignant tumors, cancerous tumors\n\n")
View(data_set)
str(data_set)

# patitioning data set 
library(caret)
set.seed(3) 
cat("\n\n------------------------\ncreating test and training data set\n")
train_variables  <-  createDataPartition(data_set$diagnosis, p = 0.67, list = FALSE)
training_data_set <- data_set[train_variables,]
test_data_set <- data_set[-train_variables,]

 cat(" number of rows in test data set ",nrow(test_data_set),"\n")
 cat(" number of rows in training  data set ",nrow(training_data_set),"\n------------------------\n")

#prop.table(table(training_data_set$diagnosis))
#prop.table(table(test_data_set$diagnosis))

# creating model 

library(e1071)
library(rminer)

NB_model <-  naiveBayes( diagnosis~., training_data_set)


# prediction
prediction_of_cancer <- predict( NB_model, test_data_set)

#head(prediction_of_cancer, n= 5)
cat("\n\n")
print(confusionMatrix(prediction_of_cancer, test_data_set$diagnosis, positive = "M",
                   dnn = c("Prediction", "True")   ))

sumerize = confusionMatrix(prediction_of_cancer, test_data_set$diagnosis, positive = "M",
                      dnn = c("Prediction", "True")   )

cat("\n--------------------------\n")
cat( " classified with " ,sumerize$overall[1]*100,"% accuracy\n") 

cat( " correctly classified ",sumerize$table[1,1], "benign tumors out of",sumerize$table[1,1] +sumerize$table[1,2],"\n" )

cat( " correctly classified ",sumerize$table[2,2], "malignant tumors out of", sumerize$table[2,2]+sumerize$table[2,1],"\n")

}

##########################################################

console()
