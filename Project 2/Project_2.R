unemployment <- read.csv("/media/removable/USB Drive/S670/Project 2/UNRATENSA.csv", head = TRUE)

library(ggplot2)
library(broom)
library(tidyr)
library(GGally)
library(ggfortify)

ggplot(unemployment, aes(x = NewDate, y = UNRATENSA)) + geom_point() + geom_line() + geom_smooth()

unemployment$NewDate <- seq(1948.000, 2017.000, length = 829)

unemployment.lo <- loess(UNRATENSA ~ NewDate, degree = 2, data = unemployment)
unemployment.lo

unemployment.lo.df <- augment(unemployment.lo)

ggplot(unemployment.lo.df, aes(x= NewDate, y = .resid)) +geom_point() + geom_line()

unemployment.lo2 <- loess(.resid~NewDate, span = 0.25, data = unemployment.lo.df)
unemployment.lo2.df <- augment(unemployment.lo2)
names(unemployment.lo2.df)
names(unemployment.lo2.df) = c(".resid", "NewDate", ".fitted", ".se.fit", ".resid2")

ggplot(unemployment.lo2.df, aes(x = NewDate, y = .resid2)) + geom_point() + geom_line()

ggplot(unemployment.lo2.df, aes(sample = .resid2)) + stat_qq()  

Year <- unemployment$NewDate
Trend <- unemployment.lo.df$.fitted - mean(unemployment.lo.df$.fitted)
Oscillatory <- unemployment.lo2.df$.fitted
Residuals <- unemployment.lo2.df$.resid2
unemployment.ts <- data.frame(Year, Trend, Oscillatory, Residuals)

unemployment.ts.long <- unemployment.ts%>% gather(type, UNDATANSA, Trend: Residuals)
unemployment.ts.long$type <- factor (unemployment.ts.long$type, levels = c("Trend", "Oscillatory", "Residuals"))
ggplot(unemployment.ts.long, aes(x=Year, y = UNDATANSA))+ geom_point()+geom_line()+facet_grid(~type)

var(Trend)
var(Oscillatory)
var(Residuals)
# Residuals explain the most here, possibly seasonal as that hasn't been pulled out yet.

seasonal <- ts(unemployment$UNRATENSA, frequency = 12)

unemployment.stl <- stl(seasonal, s.window = 15, s.degree = 0)

year.cut <- cut_number(time(unemployment$NewDate), n=5)
unemployment2 <- data.frame(year=time(unemployment$UNRATENSA), unemployment.stl$time.series, year.cut)
ggplot(unemployment2, aes(x=year, y=seasonal)) + geom_line() + facet_wrap(~year.cut, ncol=1, scales="free_x")

monthplot(unemployment.stl, choice="seasonal")
# So, unemployment is highest after the holidays end (Jan-March), seems to be a large seasonal component.

autoplot(seasonal)
