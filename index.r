install.packages("moments")
data(mtcars)
names(mtcars)
library(moments)
skewness(mtcars$carb)
kurtosis(mtcars$carb)
plot(density(mtcars$carb))
summary(mtcars$carb)
table(mtcars$carb , mtcars$mpg)
cov(mtcars$carb , mtcars$mpg)
cor(mtcars$carb , mtcars$mpg)
tapply(mtcars$carb , mtcars$mpg)
summary(mtcars)
plot(mtcars$carb)
install.packages("coronavirus")
library(coronavirus)
names(coronavirus)
plot(coronavirus$population)
pie(table(coronavirus$population))
plot(x = coronavirus$population , 
y = rep(0 , 
nrow(coronavirus)),
ylab = "",yaxt="n")
boxplot(x = coronavirus$population , xlab = "Population" , horizontal = TRUE)
hist(coronavirus$population)
hist(x = coronavirus$population , breaks = 100)
plot(density(coronavirus$population))
points(x = coronavirus$population , y = rep(-0.0005 , nrow(coronavirus)))
plot(density(mtcars$mpg))
points(x = mtcars$mpg , y = rep(-0.0005 , nrow(mtcars)))
spineplot(x = mtcars$mpg , y = mtcars$cyl)
mosaicplot(x = table(mtcars$mpg , mtcars$cyl) , las = 3)
mosaicplot(x = table(coronavirus$population , coronavirus$cases) , las = 3)
plot(x = coronavirus$date , y = coronavirus$cases)
plot(x = table(coronavirus$cases) , type = "l")
barplot(tapply(coronavirus$cases , coronavirus$date , mean),las = 3)
plot(coronavirus)
plot(x = coronavirus$cases , main = "Cases" , xlab = "Category" , ylab = "case" , col = "red")
unique(iris$Species)
unique(coronavirus)
View(unique(coronavirus))
plot(coronavirus[1:8])
plot(x = coronavirus$population , y = coronavirus$cases)
x = coronavirus$population
y = coronavirus$cases
model = lm(y ~ x)
lines(x = coronavirus$cases , y = model$fitted , col = "red" , lwd = 3)
cor(x = coronavirus$population , y = coronavirus$cases)
summary(model)
predict(object = model , newdata = data.frame(x = c(2,5,7)))
plot(x = iris[1:4] , col = as.integer(iris$Species))
plot(x = coronavirus[1:4] , col = as.integer(coronavirus$population))
plot(x = coronavirus$cases , y = coronavirus$population , col = as.numeric(coronavirus$population))
clusters = kmeans(x = iris[, 1:4], centers = 3 , nstart = 10)
plot(x = iris$Petal.Length , y = iris$Petal.Width , col = as.numeric(iris$Species) , pch = clusters$cluster)
points(x = clusters$centers[, "Petal.Length"],y = clusters$centers[, "Petal.Width"], pch = 4 , lwd = 4 , col = "blue")
table(x = clusters$cluster , y = iris$Species)