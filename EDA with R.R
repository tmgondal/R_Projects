#importing data from local device
library("readxl")
library(corrplot)

load_data <- read_excel("D:\\stirling\\mateen\\3084028BodyFatData.xlsx")
print(load_data)

#Size of dataset
dim(load_data)

#upper dataset
head(load_data)

#lower of dataset
tail(load_data)

#summary of overall dataset
summary(load_data)

#summary of one variable BodyFat
summary(load_data$BodyFat)

#summary of second variable Abdomen
summary(load_data$Abdomen)


#table chart for dataset
table(load_data$BodyFat)
plot(table(load_data$BodyFat))

table(load_data$Abdomen)
plot(table(load_data$Abdomen))


plot(load_data$BodyFat~load_data$Abdomen,col='blue')



t.test(load_data$BodyFat)
t.test(load_data$Abdomen)

#statistical Summary
#1- Mean
mean_bodyfat=mean(load_data$BodyFat)
print(mean_bodyfat)
mean_Abdomen=mean(load_data$Abdomen)
print(mean_Abdomen)

#2-Standard Daviation
sd_bodyfat=sd(load_data$BodyFat)
print(sd_bodyfat)
sd_Abdomen=sd(load_data$Abdomen)
print(sd_Abdomen)

#Normal Distribution
#1-dnorm
x <- load_data
y <- dnorm(load_data$BodyFat, mean = 19.60, sd = 8.08) #chosng the actual mean and sd
plot(x,y)

#2-histogram
#plot the histogram of dataset
hist(load_data$BodyFat,col='yellow')
#checking second histogram
hist(load_data$Abdomen,col='orange')


#Correlation Values
corrplot(cor(load_data),method = "number",type = "upper")


#predictive analysis
library(GGally)

ggpairs(data=load_data, columns=1:2, title="Body Fat Dataset") #dataset
linear_regression_model <- lm(BodyFat ~ Abdomen, data = load_data) #variable selection
ggplot(data=load_data, aes(linear_regression_model$residuals)) +
  geom_histogram(binwidth = 2, color = "red", fill = "black") +
  theme(panel.background = element_rect(fill = "grey"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("Histogram for Body Fat")
ggplot(data = load_data, aes(x = BodyFat, y = Abdomen)) + #drawfit
  geom_point() +
  stat_smooth(method = "lm", col = "blue") +
  theme(panel.background = element_rect(fill = "grey"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("Linear Model fitted to above data")
predict(linear_regression_model, data.frame(weight = 70.2)) #make prediction
confint(linear_regression_model) #check confint
summary(linear_regression_model) #print out summary
