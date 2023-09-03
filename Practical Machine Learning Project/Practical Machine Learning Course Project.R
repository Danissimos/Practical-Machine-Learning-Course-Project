#install.packages('psych')
library(ggplot2)
library(dplyr)
library(caret)
library(tibble)
library(psych)
library(corrplot)


trainSource <- "C:/Users/skako/Documents/rstudio projects/Practical Machine Learning Project/pml-training.csv"
validSource <- "C:/Users/skako/Documents/rstudio projects/Practical Machine Learning Project/pml-testing.csv"
train <- read.csv(trainSource)
valid <- read.csv(validSource)

ncol(train)
ncol(valid)

nrow(train)
nrow(valid)

table(train$classe)


na_count <-sapply(train, function(train) sum(length(which(is.na(train)))))
na_count <- data.frame(na_count)
na_count <- tibble::rownames_to_column(na_count, "Columns")

blank_count <-sapply(train, function(train) sum(length(which(train == ''))))
blank_count <- data.frame(blank_count)
blank_count <- tibble::rownames_to_column(blank_count, "Columns")

na_and_blank <- full_join(na_count, blank_count, by='Columns')
na_and_blank <- transform(na_and_blank, All = na_count + blank_count)

na_and_blank_summary <- na_and_blank %>% group_by(All)
na_and_blank_summary <- na_and_blank_summary %>% summarise(n = n())

for_selection <- filter(na_and_blank, All == 0)
for_selection_columns <- for_selection$Columns
train <- select(train,one_of(for_selection_columns))

ncol(train)
colnames(train)

train = train[-c(1, 3, 4, 5)]
train

#IVs <- as.matrix(train[,3:55])
#cor(IVs)
#corrplot(cor(IVs))


forTrain <- createDataPartition(y=train$classe, p=0.8, list=F)
train <- train[forTrain,]
test <- train[-forTrain,]

cv <- trainControl(method="cv", number=5, verboseIter=F)


random_forest <- train(classe~., data=train, method="rf", trControl = cv, ntree = 200)
pred_random_forest <- predict(random_forest, test)
random_forest_conf_matrix <- confusionMatrix(pred_random_forest, factor(test$classe))
random_forest_conf_matrix


gradient_boosting<- train(classe~., data=train, method="gbm", trControl = cv, verbose = F)
pred_gradient_boosting <- predict(gradient_boosting, test)
gradient_boosting_conf_matrix <- confusionMatrix(pred_gradient_boosting, factor(test$classe))
gradient_boosting_conf_matrix

print(predict(random_forest, valid))

