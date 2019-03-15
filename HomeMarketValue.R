#Refine the data set remove $ & ,
hmv=read.csv("Home Market value.csv")
hmv$Market.Value=as.numeric(gsub('[$,]','',hmv$Market.Value))
hmv$Square.Feet=as.numeric(gsub('[,]','',hmv$Square.Feet))
hmv

#statistical properties of data
summary(hmv)

#Correlation within the data set
cor(hmv)

#Visualizaton
hist(hmv$House.Age, xlab="House.Age", ylab="Frequency", main="Histogram of House.Age", col="blue", border = "red")
hist(hmv$Square.Feet, xlab="Square.Feet", ylab="Frequency", main="Histogram of Square.Feet", col="red", border = "blue")
hist(hmv$Market.Value, xlab="Market.Value", ylab="Frequency", main="Histogram of Market.Value", col="green", border = "blue")

par(mfrow=c(1,2))
boxplot(hmv$Market.Value~hmv$House.Age, data = hmv)
boxplot(hmv$Market.Value~hmv$Square.Feet, data = hmv)

#Build a multiple linear regression model
lm.hmv = lm(hmv$Market.Value~hmv$Square.Feet+hmv$House.Age)
summary(lm.hmv)

#Predict market value
newAge = c(26,28,29,30,31)
newSquareFeet = c(1650,1500,1800,2200,2400)
newData = data.frame(House.Age = newAge,Square.Feet = newSquareFeet)
hmv.pre = predict(lm.hmv,newData, level = 0.95, interval = "confidence")

z = hmv$Market.Value
x = hmv$Square.Feet
y = hmv$House.Age
lm.hmv = lm(z~x + y)
newData = data.frame(x = newSquareFeet, y = newAge)
hmv.pre = predict(lm.hmv, newData, level = 0.95, interval = "confidence")
hmv.pre
