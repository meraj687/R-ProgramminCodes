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
assign("product" , c("apple" , "cookie" , "lemon" , "pizza"))
product
typeof(product)
length(product)
nchar(product)
price = c(1.3,2,5.6,7)
price
str(price)
price2 = c('apple' = 1.3 , 'cookie' = 5.6 , 'lemon' = 0.5 , 'pizza' = 9)
price2
names(price) = product
price
str(price)
quantity = scan()
sort(price , decreasing = TRUE)
sort(price , decreasing = FALSE)
sort(price)
order(price)
max(price)
which.max(price)
rank(price)
product[2]
product[-2]
quantity = c(1 , 'two' , 2 , 'second' , 3 , 'three')
print(quantity)
typeof(quantity)
as.character(quantity)
product = c('apple',1,'cookie')
as.numeric(product)
a = 1:5
b = 4:7
union(a,b)
intersect(a,b)
setdiff(a,b)
setdiff(b,a)
products = c('apple','ornage','banana','guavava')
factor1 = factor(products)
print(factor1)
row1 = c(1:5)
row2 = c(6:10)
cbind(row1,row2)
rbind(row1,row2)
library(RColorBrewer)
words = c("Aryaan","Aryaan","Aryaan","Aryaan","Aryaan","Aryaan","Aryaan","Aryaan","Aryaan","Aryaan","Aryaan","Aryaan")
freq = c(1:12)
freq
wordcloud(words = words , freq = freq , max.words = 100 , color = brewer.pal(8,'Dark2'))
matrix_quant = matrix(1:18 , nrow = 3 , byrow = TRUE)
matrix_quant
nrow(matrix_quant)
ncol(matrix_quant)
typeof(matrix_quant)
dim(matrix_quant)
col_name  = c("apple","orange","mango","banana","pizza","lemon")
colnames(matrix_quant) = col_name
matrix_quant
rowSums(matrix_quant)
colSums(matrix_quant)
matrix_quant[1,2]
matrix_quant[2,4]
matrix_quant[,-2]
matrix_quant + 2
t(matrix_quant)
matrix_quant %*% t(matrix_quant)
matrix_quant == matrix_quant
diag(4)
row3 = c(10:15)
store_quant = rbind(row1 , row2 , row3)
store_quant
store_quant1 = store_quant * 2
store_quant1
show_data = array(c(store_quant , store_quant1) , dim = c(3,6,2))
show_data
length(show_data)
typeof(show_data)
dim(show_data)
show_data[,,1]
show_data[-2,1,2]
quantity = list(store_quant , store_quant1)
quantity
listQuant = list(c("aryaan","anzalna") , c(1:5))
listQuant
df = data.frame(Name=c("Aryaan","Anzalna","Saheen","Neha") , Number=c(1,2,3,4,5,6,4,8))
is.vector(df[,1])
is.data.frame(df[2])
price = c(38,100,110,200,30)
ifelse(price < 100 , "This price is less than 100" , "The price is greater than 100")
quantity = c(1,3,4,6)
avg_quant = function(quantity,type){
switch(type,arithemetic = mean(quantity),geometric = prod(quantity)^(1/length(quantity)))
}
avg_quant(quantity , "arithemetic")
avg_quant(quantity , "geometric")

cart = c("apple" , "mango" , "orange" , "lemon")
for(product in cart)
{
print(product)
}
index = 1
while(index < 3)
{
print(index)
index = index + 1
}

x = 1
repeat{
	print(x)
	x = x + 1
	if(x == 3)
	{
		break
	}
}
 add = function(x , y)
	{
		x + y
	}
add( x = 1 , y = 2)
add(2,3)

add  = function(x , y = 0)
	{
		z = x + y
		return(z)
	}

add(1)
add(4,6)

price = c(1:10)
get_max = function(x)
	{
		max = x[1]
		for(i in 2:length(x))
			{
				if(x[i] > max)
					{
						max = x[i]
					}
			}
		return(max)
	}
get_max(price)