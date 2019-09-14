library(tidyverse)
library(dplyr)
library(caret)
library(rpart)
library(rpart.plot)
library(caTools)
library(tree)
library(hms)
library(party)
library(gbm)
library(ggplot2)
library(plyr)

#reading the flights dataset
dataset <- read.csv("flights.csv")
airline <- read.csv("airlines.csv")
airport <- read.csv("airports.csv")

#deducting all rows which have the records of cancellation and diversion
dataset <- dataset %>% filter(CANCELLED == 0) %>% filter(DIVERTED == 0)

#adding new column of delay result
dataset <- mutate(dataset, DELAY_RESULT = ARRIVAL_DELAY)

#categorizing delay result from numeric to yes or no
dataset$DELAY_RESULT <-ifelse(dataset$DELAY_RESULT<15, "No", "Yes")

#converting delay result to factor
dataset$DELAY_RESULT <- as.factor(dataset$DELAY_RESULT)

#explore dataset
head(dataset$DELAY_RESULT)
names(dataset)
class(dataset$DELAY_RESULT)
typeof(dataset$DELAY_RESULT)
prop.table(table(dataset$DELAY_RESULT))
nrow(dataset)


#selecting necessary features only
dataset <- dataset %>% select("MONTH", "AIRLINE", "DAY_OF_WEEK", "FLIGHT_NUMBER", 
            "ORIGIN_AIRPORT", "DESTINATION_AIRPORT", "SCHEDULED_DEPARTURE", "DELAY_RESULT")

#checking dataset
head(dataset)
head(airline)
head(airport)

#changing column airline's value
colnames(dataset)[colnames(dataset) == "AIRLINE"] <- "IATA_CODE"
dataset <- merge(dataset, airline, by.x="IATA_CODE", by.y="IATA_CODE") 
dataset <- dataset %>% select(- "IATA_CODE")


#changing column origin_airport's value
colnames(dataset)[colnames(dataset) == "ORIGIN_AIRPORT"] <- "IATA_CODE"
dataset <- merge(dataset, airport, by.x="IATA_CODE", by.y="IATA_CODE") 
dataset <- dataset %>% select(- c("IATA_CODE", "CITY", "STATE", "COUNTRY", "LATITUDE", "LONGITUDE"))
colnames(dataset)[colnames(dataset) == "AIRPORT"] <- "ORIGIN_AIRPORT"


#changing column DESTINATION_AIRPORT's value
colnames(dataset)[colnames(dataset) == "DESTINATION_AIRPORT"] <- "IATA_CODE"
dataset <- merge(dataset, airport, by.x="IATA_CODE", by.y="IATA_CODE") 
dataset <- dataset %>% select(- c("IATA_CODE", "CITY", "STATE", "COUNTRY", "LATITUDE", "LONGITUDE"))
colnames(dataset)[colnames(dataset) == "AIRPORT"] <- "DESTINATION_AIRPORT"



#EDA
counts <- table(dataset$AIRLINE)/1000
barplot(counts, main="Airline Distribution", 
        ylab="Number of Filghts(in thousands)", las=2, cex.names=.5)


pal <- colorRampPalette(colors = c("lightblue", "blue"))(3)
counts <- table(dataset$MONTH)/1000
barplot(counts, main="No. of Flights(Monthly)", col = pal,
        ylab="Number of Filghts(in thousands)", las=1, cex.names=.5)


pal <- colorRampPalette(colors = c("lightgreen", "green"))(3)
counts <- table(dataset$DAY_OF_WEEK)/1000
barplot(counts, main="No. of Flights(Daily)", col = pal,ylim=c(0,800),
        ylab="Number of Filghts(in thousands)", las=1, cex.names=.5)


w <- table(dataset$DAY_OF_WEEK)
t = as.data.frame(w)
t$Freq <- t$Freq/5231130

pie = ggplot(t, aes(x="", y=Freq, fill=Var1)) + geom_bar(stat="identity", width=1)
pie = pie + coord_polar("y", start=0) + geom_text(aes(label = paste0(round(t$Freq*100), "%")), position = position_stack(vjust = 0.5))
pie = pie + labs(x = NULL, y = NULL, fill = NULL, title = "Percentage of Flights (Daily)")
pie = pie + theme_classic() + theme(axis.line = element_blank(),
                                    axis.text = element_blank(),
                                    axis.ticks = element_blank(),
                                    plot.title = element_text(hjust = 0.5, color = "#666666"))

pie


#find index to suffle all rows
shuffle_index <- sample(1:nrow(dataset))
head(shuffle_index)

#suffle all rows
dataset <- dataset[shuffle_index, ]
head(dataset)

#creating traing an testing dataset
training_index <- createDataPartition(dataset$DELAY_RESULT, p=0.80, list=FALSE)
training_data <- dataset[training_index,] 
testing_data <- dataset[-training_index,] 

head(training_data)
head(testing_data)

#probability of YES and NO
prop.table(table(dataset$DELAY_RESULT))


#tree
tree <- rpart(DELAY_RESULT~., data = training_data, method = 'class')
rpart.plot(tree, extra = 106)

predict_unseen <-predict(tree, testing_data, type = 'class')

table_mat <- table(testing_data$DELAY_RESULT, predict_unseen)
table_mat

accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
accuracy_Test


#boosting
# train GBM model
gbm.fit <- gbm(
  formula = DELAY_RESULT~.,
  distribution = "bernoulli",
  data = training_data,
)  

print(gbm.fit)

sqrt(min(gbm.fit$cv.error))

gbm.perf(gbm.fit, method = "cv")

pred <- predict(gbm.fit, n.trees = gbm.fit$n.trees, testing_data)

training_data$DELAY_RESULT <- as.factor(training_data$DELAY_RESULT)

table_mat <- table(testing_data$DELAY_RESULT, pred)
table_mat

accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
accuracy_Test



