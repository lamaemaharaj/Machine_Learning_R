# Attatch the data set to the environment
data("iris")
dataset <- iris

# Loading proper packages
library(caret)
library(ggplot2)
library(lattice)

#Splitting the data set into two parts. 80% Train model and 20% as validation
index_validation <- createDataPartition(dataset$Species,p=0.80,list=FALSE)

#Select 20% of the data for validation 
validation<-dataset[-index_validation, ]

#Select 80% of the remaining data to train and test the models
dataset<- dataset[index_validation,]

#Dataset dimensions
dim(dataset)
[1] 120   5

#List types for each attribute 
sapply(dataset,class)
Sepal.Length  Sepal.Width Petal.Length  Petal.Width      Species 
"numeric"    "numeric"    "numeric"    "numeric"     "factor" 

# Looking at the Data
head(dataset)

levels(dataset$Species)

#Summarize the class distribution 
percentage<-prop.table(table(dataset$Species))*100
cbind(freq=table(dataset$Species),percentage=percentage)

freq percentage
setosa       40   33.33333
versicolor   40   33.33333
virginica    40   33.33333

summary(dataset)

#Splitting input values 
y <- dataset[ ,5]
x <- dataset[,1:4]

# Boxplot for each attribute one one image 
par(mfrow=c(1,4))
for(i in 1:4) {
  boxplot(x[,i], main=names(iris)[i])
}

# Barplot package openning
library(dplyr)
library(ggplot2)

dataset %>% ggplot(aes(x= y)) + geom_bar() +labs(x = "Iris Flower Species")

# scatterplot matrix 
featurePlot(x=x, y=y, plot="ellipse")

# scatterploit matrix 
featurePlot(x=x, y=y, plot="ellipse")

# Box and whisker plots for each attribute 
featurePlot(x = x, y = y, plot = "box")

# Density plots for each attribute by species class value
scales<-list(x = list(relation = "free"), y = list(relation = "free"))
featurePlot(x = x, y = y, plot = "density", scales = scales)

# Run algorithms using 10-fold cross validation
control<-trainControl(method = "cv", number = 10)
metric<-"Accuracy"

# Linear Discriminant Analysis (LDA)
set.seed(7)
fit.lda <- train(Species~., data=dataset, method="lda", metric=metric, trControl=control)

# Classification and Regression Trees (CART)
set.seed(7)
fit.cart <- train(Species~., data=dataset, method="rpart", metric=metric, trControl=control)

# k-Nearest Neighbors (kNN)
set.seed(7)
fit.knn <- train(Species~., data=dataset, method="knn", metric=metric, trControl=control)

# Support Vector Machines (SVM)
set.seed(7)
fit.svm <- train(Species~., data=dataset, method="svmRadial", metric=metric, trControl=control)

# Random Forest (RF)
set.seed(7)
fit.rf <- train(Species~., data=dataset, method="rf", metric=metric, trControl=control)

# Summarize model accuracy for each model
results <- resamples(list(lda=fit.lda,cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf)

summary(results)

# Compare accuracy of models
dotplot(results)

# Summarize Best Model 
print(fit.lda)

# Estimating the skill of LDA on the validation dataset 
predictions<-predict(fit.lda, validation)
confusionMatrix(predictions, validation$Species)














