install.packages("mice")
install.packages("VIM")
install.packages("pvclust")

#Loading all the libraries
library(readxl)
library(mice)
library(VIM)
library(Amelia)
library(arules)
library(arulesViz)
library(stats)
library(Hmisc)
require(pvclust)
library(pvclust)
library(cluster)

#Reading the dataset
Simulated_Coles_Data <- read_excel("E:\\praneeth\\DataScience\\DataMining\\assignment1\\Simulated Coles Data.xlsx", sheet = "Coles Transactions")
summary(Simulated_Coles_Data)
table(is.na(Simulated_Coles_Data))

#preprocessing
table(duplicated(Simulated_Coles_Data$ReceiptID))
#Preprocessing income, age ==> replacing NA with mean
Simulated_Coles_Data$income[is.na(Simulated_Coles_Data$income)] <- mean(Simulated_Coles_Data$income, na.rm = TRUE)
Simulated_Coles_Data$age[is.na(Simulated_Coles_Data$age)] <- mean(Simulated_Coles_Data$age, na.rm = TRUE)

#preprocessing Postcode, nchildren ==> replacing with mode
mode_nchildren <- as.numeric(tail(names(sort(table(Simulated_Coles_Data$nchildren))), 1))
Simulated_Coles_Data$nchildren[is.na(Simulated_Coles_Data$nchildren)] <- mode_nchildren
mode_postcode <- as.numeric(tail(names(sort(table(Simulated_Coles_Data$PostCode))), 1))
Simulated_Coles_Data$PostCode[is.na(Simulated_Coles_Data$PostCode)] <- mode_postcode

#preprocessing all the products ==> replacing NA with 0
Simulated_Coles_Data[, 10:53][is.na(Simulated_Coles_Data[, 10:53])] <- 0

#handling outliers and error values
boxplot(Simulated_Coles_Data$nchildren, xlab = "nchildren", ylab = "count", col = "Blue", notch = TRUE, main = "Box Plot Before preproceesing")
mode_nchildren <- as.numeric(tail(names(sort(table(Simulated_Coles_Data$nchildren))), 1))
Simulated_Coles_Data$nchildren[Simulated_Coles_Data$nchildren > 5] <- mode_nchildren
boxplot(Simulated_Coles_Data$nchildren, xlab = "nchildren", ylab = "count", col = "Blue", notch = TRUE, main = "Box Plot after preproceesing")

boxplot(Simulated_Coles_Data$Value, xlab = "Value of transaction", ylab = "value", col = "Blue", notch = TRUE, main = "Box Plot Before preproceesing")
Simulated_Coles_Data$Value[Simulated_Coles_Data$Value > 500] <- mean(Simulated_Coles_Data$Value)
boxplot(Simulated_Coles_Data$Value, xlab = "Value of transaction", ylab = "value", col = "Blue", notch = TRUE, main = "Box Plot after preproceesing")
boxplot(as.numeric(Simulated_Coles_Data$PostCode), ylim=c(0,50000), xlab = "Postcode", ylab = "value", col = "Blue", notch = TRUE, main = "Post code before preprocessing")
mode_postcode <- as.numeric(tail(names(sort(table(Simulated_Coles_Data$PostCode))), 1))
Simulated_Coles_Data$PostCode[Simulated_Coles_Data$PostCode < 100] <- mode_postcode
Simulated_Coles_Data$PostCode[Simulated_Coles_Data$PostCode > 9999] <- mode_postcode
boxplot(as.numeric(Simulated_Coles_Data$PostCode), ylim=c(0,10000), xlab = "Postcode", ylab = "value", col = "Blue", notch = TRUE, main = "Post code after preprocessing")


plot((Simulated_Coles_Data$pmethod), ylim = c(0,5), xlim = c(0,103), col = "Blue", xlab = "Payment Method", ylab = "Count", main ="Payment method before preprocessing", sub = "1=Cash, 2=Credit Card, 3=Eftpos, 4=Others")
mode_pmethod <- as.numeric(tail(names(sort(table(Simulated_Coles_Data$pmethod))), 1))
mode_pmethod
Simulated_Coles_Data$pmethod[Simulated_Coles_Data$pmethod > 4] <- mode_pmethod
Simulated_Coles_Data$pmethod <- as.factor(Simulated_Coles_Data$pmethod)
plot(Simulated_Coles_Data$pmethod, xlim = c(0,5), col = "Blue", xlab = "Payment Method", ylab = "Count", main ="Payment method after preprocessing", sub = "1=Cash, 2=Credit Card, 3=Eftpos, 4=Others")
summary(Simulated_Coles_Data$pmethod)

plot(Simulated_Coles_Data$homeown, ylim=c(0,2),xlim = c(0,104), col = "Blue", xlab = "Homeown", ylab = "Count", main ="Homeown before preprocessing", sub = "1=Homeown, 2=Not Homeown, 3=Others")
mode_homeown <- as.numeric(tail(names(sort(table(Simulated_Coles_Data$homeown))), 1))
mode_homeown
Simulated_Coles_Data$homeown[Simulated_Coles_Data$homeown > 3] <- mode_homeown
Simulated_Coles_Data$homeown <- as.factor(Simulated_Coles_Data$homeown)
plot(Simulated_Coles_Data$homeown, xlim = c(0,4), col = "Blue", xlab = "Homeown", ylab = "Count", main ="Homeown after preprocessing", sub = "1=Homeown, 2=Not Homeown, 3=Others")

mode_fruit <- as.numeric(tail(names(sort(table(Simulated_Coles_Data$fruit))), 1))
mode_fruit
Simulated_Coles_Data$fruit[Simulated_Coles_Data$fruit > 1] <- mode_fruit
Simulated_Coles_Data$fruit <- as.factor(Simulated_Coles_Data$fruit)
plot(Simulated_Coles_Data$fruit, xlim = c(0,2), col = "Blue", xlab = "fruit purchases", ylab = "Count", main ="fruit purchases", sub = "1=Bought, 0=Not Bought")


mode_fruitjuice <- as.numeric(tail(names(sort(table(Simulated_Coles_Data$fruitjuice))), 1))
mode_fruitjuice
Simulated_Coles_Data$fruitjuice[Simulated_Coles_Data$fruitjuice > 1] <- mode_fruitjuice
Simulated_Coles_Data$fruitjuice <- as.factor(Simulated_Coles_Data$fruitjuice)
plot(Simulated_Coles_Data$fruitjuice, xlim = c(0,2), col = "Blue", xlab = "fruit juice purchases", ylab = "Count", main ="fruit juice purchases", sub = "1=Bought, 0=Not Bought")



#COnverting data to Category

Simulated_Coles_Data$sex <- as.factor(Simulated_Coles_Data$sex)


cols <- c(10:53)
Simulated_Coles_Data[cols] <- lapply(Simulated_Coles_Data[cols], factor)

summary(Simulated_Coles_Data)

#Data Analysis for different variables
Simulated_Coles_Data$nchildren <- as.factor(Simulated_Coles_Data$nchildren)
plot(Simulated_Coles_Data$nchildren,ylim= c(0, 30000), xlim = c(0,8), col = "Blue", xlab = "Number of children", ylab = "Count", main ="Number of children")
tail(names(sort(table(Simulated_Coles_Data$PostCode))), 1)
head(names(sort(table(Simulated_Coles_Data$PostCode))), 1)
hist(Simulated_Coles_Data$age, breaks=10, ylim= c(0,40000), xlim= c(0,110), col = "Blue", xlab = "Age in years", ylab = "Count", main ="Age of customers")
hist(Simulated_Coles_Data$income, breaks=10, ylim= c(0,60000), xlim= c(6000,200000), col = "Blue", xlab = "Income in Dollars", ylab = "Count", main ="Annual Income of customers")
plot(Simulated_Coles_Data$sex, xlim = c(0,5), col = "Blue", xlab = "Sex", ylab = "Count", main ="Different genders count", sub = "1=Male, 2=Female")
plot(Simulated_Coles_Data$pmethod,xlim = c(0,5), xlab = "Payment Method", ylab = "Count", main ="Payment method", sub = "1=Cash, 2=Credit Card, 3=Eftpos, 4=Others")



#Market Basket Analysis

summary(Simulated_Coles_Data[10:53])
Simulated_Coles_Data[, 10:53][is.na(Simulated_Coles_Data[, 10:53])] <- 0
trans2 <- Simulated_Coles_Data[,c(10:53)]
trans2 <- as(trans2, "matrix")
trans2 <- as(trans2, "itemMatrix")
summary(trans2)
class(trans2) 
rules1 <- apriori(trans2)
rules1
data(rules1)
itemFrequencyPlot(trans2, topN =45, ylim=c(0,1))
rules1 <- sort(rules1, decreasing=TRUE, by="support")
inspect(rules1[1:100])
rules2 <- sort(rules1, decreasing=TRUE, by="confidence")
inspect(rules2[1:100])
rules3 <- sort(rules1, decreasing=TRUE, by="lift")
inspect(rules3[1:100])



#Kmeans Clustering
cluster3 <- kmeans(Simulated_Coles_Data, 3)
cluster4 <- kmeans(Simulated_Coles_Data, 2)

wss=(nrow(Simulated_Coles_Data)-1)*sum(apply(Simulated_Coles_Data,2,var))
for (i in 2:4) wss[i] = sum(kmeans(Simulated_Coles_Data,centers = i)$withinss)
par(bg="white")
plot(1:4,wss,type="b", xlab="Number of clusters",ylab="sum of squares", col = "blue")

cluster2 <- kmeans(Simulated_Coles_Data, 2, algorithm = "Lloyd")

Simulated_Coles_Data$cluster2 <- as.factor(cluster2$cluster)
Simulated_Coles_Data$cluster3 <- as.factor(cluster3$cluster)
Simulated_Coles_Data$cluster4 <- as.factor(cluster4$cluster)
summary(Simulated_Coles_Data)

clusplot(Simulated_Coles_Data, Simulated_Coles_Data$cluster2, color = TRUE, shade = FALSE, labels = 4, lines = 0, main = "cluster 2")
clusplot(Simulated_Coles_Data, Simulated_Coles_Data$cluster3, color = TRUE, shade = FALSE, labels = 4, lines = 0, main = "cluster 3")
clusplot(Simulated_Coles_Data, Simulated_Coles_Data$cluster4, color = TRUE, shade = FALSE, labels = 4, lines = 0, main = "cluster 4")

h1 <- hclust(dist(Simulated_Coles_Data[, c(2:53)], method = "euclidean"), method = "complete")

cluster1_age=Simulated_Coles_Data$age[Simulated_Coles_Data$cluster2==1]
avg_age1= sum(cluster1_age)/length(cluster1_age)
avg_age1

cluster2_age=Simulated_Coles_Data$age[Simulated_Coles_Data$cluster2==2]
avg_age2= sum(cluster2_age)/length(cluster2_age)
avg_age2

cluster1_income=Simulated_Coles_Data$income[Simulated_Coles_Data$cluster2==1]
avg_income1= sum(cluster1_income)/length(cluster1_income)
avg_income1

cluster2_income=Simulated_Coles_Data$income[Simulated_Coles_Data$cluster2==2]
avg_income2= sum(cluster2_income)/length(cluster2_income)
avg_income2

cluster1_value=Simulated_Coles_Data$Value[Simulated_Coles_Data$cluster2==1]
avg_value1= sum(cluster1_value)/length(cluster1_value)
avg_value1

cluster2_value=Simulated_Coles_Data$Value[Simulated_Coles_Data$cluster2==2]
avg_value2= sum(cluster2_value)/length(cluster2_value)
avg_value2

