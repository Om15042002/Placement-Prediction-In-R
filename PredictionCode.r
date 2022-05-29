library(ggplot2)
library(lattice)
library(caret)

# Read data from csv file 
placement_data <- read.csv("D:\\College Data\\Sem 5 (All Labs)\\Advanced Programming Laboratory\\R Programming\\Enginnering Placement Prediction\\collegePlace.csv")
print(head(placement_data))

cat("\n")

# Print mean of PlacedOrNot data based on Age data 
print(aggregate(PlacedOrNot~Age, data=placement_data, FUN=mean))
  
# Draw the graph on the relation between PlacedOrNot and Age data
ggplot(placement_data ,aes(Age, fill=factor(PlacedOrNot))) + geom_bar() + 
  scale_x_continuous(breaks=seq(18,30, by=1)) + labs(fill="PlacedOrNot")

cat("\n")

# Print mean of PlacedOrNot data based on Gender data
print(aggregate(PlacedOrNot~Gender, data=placement_data, FUN=mean))

# Draw the graph on the relation between Stream and Gender data
ggplot(placement_data,aes(Gender, fill=Stream)) + geom_bar()

cat("\n")

# Print mean of PlacedOrNot data based on Stream data
print(aggregate(PlacedOrNot~Stream, data=placement_data, FUN=mean))

# Draw the graph on the relation between PlacedOrNot and Stream data
ggplot(placement_data,aes(Stream)) + geom_bar(aes(fill=factor(PlacedOrNot))) +
  theme(axis.text.x=element_text(angle=60)) + scale_fill_manual(values=c("red", "pink"))

# Draw the graph on the relation between PlacedOrNot and CGPA and Internships data 
ggplot(placement_data,aes(CGPA, Internships, fill=factor(PlacedOrNot))) + geom_tile() +
        scale_fill_manual(values=c("yellow", "purple"))

cat("\n")

# Print mean of PlacedOrNot data based on Hostel and HistoryofBacklogs data
print(aggregate(PlacedOrNot~Hostel+HistoryOfBacklogs, data=placement_data, FUN=mean))

# Draw the graph on the relation between PlacedOrNot and Hostel and HistoryofBacklogs data
ggplot(placement_data,aes(factor(Hostel), factor(HistoryOfBacklogs), color=factor(PlacedOrNot))) + 
        geom_jitter()

# Draw the graph of overall PlacedOrNot data
ggplot(placement_data,aes(factor(PlacedOrNot))) + geom_bar(fill="pink")

# Percentage of Placement of overall data
prop.table(table(placement_data$PlacedOrNot))

# Chooses 90% of the data randomly from given data
set.seed(53)

sample_data <- createDataPartition(placement_data$PlacedOrNot,p=0.9,list=FALSE)
print(head(sample_data))  

# 90% data given to train the model
train_data <- placement_data[sample_data,] 
print(head(train_data))
cat("\n")

# 10% data given to test the model
test_data <- placement_data[-sample_data,]
print(head(test_data))
cat("\n")

# Creating the model
model <- lm(PlacedOrNot~.,data=train_data)

# Predicting the PlacedOrNot of the test data
predictions <- predict(model,test_data)
cat("\n")
print(head(predictions))

# Comparing the actual value and predicted value
compare <- data.frame(actual=test_data$PlacedOrNot,predicted=predictions)
cat("\n")
print(compare[1:10,])

# Calculating teh error
error <- RMSE(predictions,test_data$PlacedOrNot)
cat("\n")
print(error)







