library(tidyverse)
library(dplyr)
library(caret)
library(rpart)
library(rpart.plot)
library(caTools)
library(tree)

dataset <- read.csv("flights.csv")

dataset <- dataset %>% filter(CANCELLED == 0) %>% filter(DIVERTED == 0)

dataset <- mutate(dataset, DELAY_RESULT = DEPARTURE_DELAY)

dataset$DELAY_RESULT <-ifelse(dataset$DELAY_RESULT<15, "No", "Yes")

dataset$DELAY_RESULT <- as.factor(dataset$DELAY_RESULT)

write.csv(dataset, file = "flight_pre_arr.csv")
