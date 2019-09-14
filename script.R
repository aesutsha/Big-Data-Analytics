library(tidyverse)
library(dplyr)
library(caret)
library(rpart)
library(rpart.plot)
library(caTools)
library(tree)

###pre-processing
#loading dataset
dataset <- read.csv("test.csv")
#dataset

#exploring dataset
nrow(dataset)
summary(dataset)
str(dataset)
names(dataset)
sum(is.na(dataset))



#removing records of cancelled flights
dataset <- dataset %>% filter(CANCELLED == 0) %>% filter(DIVERTED == 0)

#selecting column
dataset <- dataset %>% select("MONTH", "DAY_OF_WEEK", "AIRLINE", "SCHEDULED_DEPARTURE", "DEPARTURE_DELAY", "ARRIVAL_DELAY")
#exploring dataset
head(dataset)

#adding delay_result column
dataset <- mutate(dataset, DELAY_RESULT = DEPARTURE_DELAY + ARRIVAL_DELAY)

#exploring dataset
head(dataset)


#removing DEPARTURE_DELAY and ARRIVAL_DELAY column
dataset <- dataset %>% select(-c("DEPARTURE_DELAY", "ARRIVAL_DELAY"))

#exploring dataset
head(dataset)
tail(dataset)
summary(dataset)
str(dataset)
names(dataset)

#categorizing day_of_week to weekday and weekend
dataset$DAY_OF_WEEK <- cut(dataset$DAY_OF_WEEK, 
                   breaks=c(0,1,6,7), 
                   labels=c("Weekend","Weekday","Weekend"))

#converting factor to character
#dataset$DAY_OF_WEEK <- as.character(dataset$DAY_OF_WEEK)



#check datatype of day_of_week
class(dataset$DAY_OF_WEEK)

#categorizing month to quarter
dataset$MONTH <-cut(dataset$MONTH, 
                     breaks=4, 
                      labels=c(1,2,3,4))

#converting factor to character
#dataset$MONTH <- as.numeric(dataset$MONTH)

#changing name of comlumn month to yearly_quarter
dataset <- dataset %>% rename(YEARLY_QUARTER = MONTH)

#categorizing delay_result to Yes and No
dataset$DELAY_RESULT <-ifelse(dataset$DELAY_RESULT<0, "No", "Yes")

#converting factor to character
#dataset$DELAY_RESULT <- as.character(dataset$DELAY_RESULT)

#categorizing SCHEDULED_DEPARTURE to day and night
dataset$SCHEDULED_DEPARTURE <-cut(dataset$SCHEDULED_DEPARTURE, 
                           breaks=c(-1,0659,1859,2359), 
                           labels=c("Night","Day", "Night"))
#converting factor to character
#dataset$SCHEDULED_DEPARTURE <- as.character(dataset$SCHEDULED_DEPARTURE)

#exploring dataset
sum(is.na(dataset))
nrow(dataset)
head(dataset)
tail(dataset)
summary(dataset)
str(dataset)
names(dataset)



#saving csv
#write.csv(dataset, file = "result.csv")
#dataset <- read.csv("result.csv")
#dataset <- dataset %>% select(-c(1))

#dividing the training and testing data
set.seed(1)
#test_index <- createDataPartition(dataset, times = 1, p = 0.8, list = FALSE)
#train_set <- dataset %>% slice(-test_index)
#test_set <- dataset %>% slice(test_index)

#########
sample = sample.split(dataset, SplitRatio = .80)
train = subset(dataset, sample==TRUE)
test = subset(dataset, sample==FALSE)

test$DELAY_RESULT <- as.factor(test$DELAY_RESULT)

nrow(train)
nrow(test)
nrow(dataset)

# building the classification tree with rpart
tree <- rpart(DELAY_RESULT~.,
              data=train)
# Visualize the decision tree with rpart.plot
rpart.plot(tree, nn=TRUE)

summary(tree)
#pred <- predict(tree,test_set,type="class")
pred <- predict(tree,test,type="class")
confusionMatrix(pred, test$DELAY_RESULT)


prp(tree)



#######using tree package

# building the classification tree with rpart
tree_model <- tree(as.factor(DELAY_RESULT)~., train)
# Visualize the decision tree with rpart.plot
plot(tree)
text(tree_model, pretty = 0)
#pred <- predict(tree,test_set,type="class")
pred <- predict(tree_model,test,type="class")
confusionMatrix(pred, test$DELAY_RESULT)

#prune
cv_tree = cv.tree(tree_model, FUN = prune.misclass)
names(cv_tree)
plot(cv_tree$size, cv_tree$dev, type="b")
prune_model = prune.misclass(tree_model, best = 3)
plot(prune_model)
prune_pred <- predict(prune_model,test,type="class")
confusionMatrix(prune_pred, test$DELAY_RESULT)
