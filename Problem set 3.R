problemset3 <- read.table("/media/removable/USB Drive/S670/s670-ps3-data.txt", head=TRUE)

library(ggplot2)

summary(problemset3)

head(problemset3)

plot1 <- ggplot(problemset3, aes(x = x, y = (y1)^-1)) + geom_point()
plot1

plot1 + geom_smooth()

plot1 + geom_smooth(method = "lm", formula = y ~ x + I(x^2))

plot1 + geom_smooth(method.args=list(degree=1))
#Bingo


plot2 <- ggplot(problemset3, aes(x = x, y = log10(y2))) + geom_point()
plot2

plot2 +geom_smooth(method = "lm")

plot2 + geom_smooth(method.args=list(degree = 1, family = "symmetric"))
#Bingo


plot3 <- ggplot(problemset3, aes(x = x, y = y3)) + geom_point()
plot3

plot3 + geom_smooth()

plot3 + geom_smooth(span=.1)
#Bingo