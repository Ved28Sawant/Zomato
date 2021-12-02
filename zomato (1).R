#loading packages
library(dplyr)
library(ggplot2)
library(caret)
library(superml)
library(psych)

#reading data
data=read.csv('C:/Users/VEDANG SAWANT/Desktop/Projects/zomato/zomato.csv')
head(data)
View(data)


#deleting unnecessary columns
data=data[,-c(1,2,8,14,15)]


#removing /5 from rate column
data$rate=sub("/5","",data$rate)


#renaming columns
library(plyr)
data=rename(data,c(city=listed_in.city.,cost=approx_cost.for.two.people.,type=listed_in.type.))
summary(data)


#coverting character to numeric type
data$rate=as.numeric(data$rate)
data$cost=as.numeric(data$cost)
data$votes=as.numeric(data$votes)
summary(data)


#replacing missing values with mean
data$rate[is.na(data$rate)]=mean(data$rate,na.rm = TRUE)
data$cost[is.na(data$cost)]=mean(data$cost,na.rm= TRUE)
colSums(is.na(data))
unique(data$location)
count(data,city)

#Binary encoding
data$online_order=ifelse(data$online_order=='Yes',1,0)
unique(data$online_order)
data$book_table=ifelse(data$book_table=='Yes',1,0)


#categorical label encoding
label=LabelEncoder$new()
label$fit(data$city)
data$city=label$fit_transform(data$city)
unique(data$city)

label$fit(data$type)
data$type=label$fit_transform(data$type)
unique(data$type)

label$fit(data$location)
data$location=label$fit_transform(data$location)
unique(data$location)

label$fit(data$rest_type)
data$rest_type=label$fit_transform(data$rest_type)
unique(data$rest_type)

label$fit(data$cuisines)
data$cuisines=label$fit_transform(data$cuisines)
unique(data$cuisines)

label$fit(data$cuisines)
data$cuisines=label$fit_transform(data$cuisines)
unique(data$cuisines)

label$fit(data$name)
data$name=label$fit_transform(data$name)
unique(data$name)

#correlation plot
corPlot(data)

#barplot of online_order
ggplot(data = data,aes(online_order))+geom_bar(fill='blue')

#barplot of book table
ggplot(data = data,aes(book_table))+geom_bar(fill='red')

ggplot(data = data,aes(city))+geom_bar(fill='red')


#####Machine learning
#removing outliers
Q=quantile(data$votes, probs = c(.25,.75),na.rm = FALSE)
IQR=IQR(data$votes)
up=Q[2]+1.5*IQR
low=Q[1]-1.5*IQR
data=subset(data,(data$votes>low & data$votes<up))


xtrain=data[1:41371,c("online_order","book_table","rate","votes","cuisines")]
ytrain=data[1:41371,c("cost")]

xtest=data[41372:51717,c("online_order","book_table","rate","votes","cuisines")]
ytest=data[41372:51717,c("cost")]

data_target=data[,"cost"]
data_attribute=data.matrix(data[,c("online_order","book_table","rate","votes","cuisines")])

data1=scale(data)
head(data1)
View(data1)
model=lm(cost~online_order+book_table+rate+votes+location+rest_type+cuisines+cost+type+city,data = data1)

