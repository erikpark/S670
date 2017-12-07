library(ggplot2)
library(broom)
library(MASS)
library(tidyr)

movie_budgets <- read.table("./movie_budgets.txt", head = TRUE)
movie_budgets$log.budget <- log10(movie_budgets$budget)

plot.year <- ggplot(movie_budgets, aes(x = year, y = log.budget)) + geom_point()

plot.length <- ggplot(movie_budgets, aes(x = length, y = log.budget)) + geom_point()

plot.year + geom_smooth(method = "lm")

plot.year + geom_smooth(method = "rlm")

plot.year + geom_smooth(method = "rlm", method.args=list(psi=psi.bisquare))

plot.year + geom_smooth(method = "loess")

plot.year + geom_smooth(method = "loess", method.args = list(degree = 1))

plot.year + geom_smooth(method = "loess", span = .5)

plot.year + geom_smooth(method = "loess", method.args = list(degree = 1), span = .5)

plot.year + geom_smooth(method = "loess", span = .5, method.args = list(family = "symmetric"))
# I like this last plot a lot. lower span keeps the fit closer to the far left point right above 3 on the y, and also allows 
# the far right tail to decrease more which seems like an accurate representation of what is going on in the data.
# Using the "symmetric" bisquare fit keeps the model a bit more resistant to outliers in the middle of the x axis, and allows the 
# right side of the distribution to represent the really high budget values better. A curved fit in general seems much better here, 
# as degree 1 fits are a little too simplistic and don't seem to capture enough of what's going on.



plot.length + geom_smooth(method = "loess")

plot.length + geom_smooth(method = "loess", span = .5)

plot.length + geom_smooth(method = "loess",method.args = list(degree = 1))

plot.length + geom_smooth(method = "loess",method.args = list(degree = 1, family = "symmetric"))

plot.length + geom_smooth(method = "loess", span = .5, method.args = list(degree = 1, family = "symmetric"))

# Based on these plots, and the residual plots below, the best fit for budget~length seems to be a loess with degree 2, guassian family, and span of 0.5.



# plots of budget ~ year -- not good?

ggplot(movie_budgets, aes(x = year, y = log.budget)) + geom_point() + geom_smooth(method = "loess") + 
  facet_grid(~cut_number(length, n = 4)) + labs(title = "Loess of movie budget by year, split by length")
# The loess curves for the faceted plots keep increasing in budget as length gets longer, but their slopes are pretty different too. Not great I think.

ggplot(movie_budgets, aes(x = year, y = log.budget)) + geom_point() + geom_smooth(method = "loess", method.args = list(family = "symmetric")) + 
  facet_grid(~cut_number(length, n = 4)) + labs(title = "Loess of movie budget by year, split by length")

ggplot(movie_budgets, aes(x = year, y = log.budget)) + geom_point() + geom_smooth(method = "loess", span = 1, method.args = list(family = "symmetric", degree = 1)) + 
  facet_grid(~cut_number(length, n = 3)) + labs(title = "Loess of movie budget by year, split by length")



# Plots of budget ~ length

ggplot(movie_budgets, aes(x = length, y = log.budget)) + geom_point() + geom_smooth(method = "loess", method.args = list(degree = 1)) + 
  facet_grid(~cut_number(year, n = 4)) + labs(title = "Loess of movie budget by length, split by year")
# Seems like the best so far, no left curving on any plots, curves all seem similar and representitive of the data.

ggplot(movie_budgets, aes(x = length, y = log.budget)) + geom_point() + geom_smooth(method = "loess", method.args = list(family = "symmetric")) + 
  facet_grid(~cut_number(year, n = 4)) + labs(title = "Loess of movie budget by length, split by year")


ggplot(movie_budgets, aes(x = length, y = log.budget)) + geom_point() + geom_smooth(method = "loess", span = .5) + 
  facet_grid(~cut_number(year, n = 4)) + labs(title = "Loess of movie budget by length, split by year")

ggplot(movie_budgets, aes(x = length, y = log.budget)) + geom_point() + geom_smooth(method = "loess") + 
  facet_grid(~cut_number(year, n = 4)) + labs(title = "Loess of movie budget by length, split by year")

# In general, seems like it's best to plot budget ~ length and facet on year. Need to check the residuals though.

# How about this shit? fit model of budget ~ length, facet on year, play around with arguments
length.lo1.fac <- loess(log.budget ~ length, data = movie_budgets)
length.lo1.fac.df <- augment(length.lo1.fac)
# Make new variable for year cut to facet on
year.cut <- cut_number(movie_budgets$year, n = 4)
length.lo1.fac.df <- data.frame(length.lo1.fac.df, year.cut)
# Plot residuals
ggplot(length.lo1.fac.df, aes(x = length, y = .resid)) + geom_point() + geom_smooth(method = "loess", method.args = list(family = "symmetric", degree = 1)) + geom_abline(slope = 0, intercept = 0) +
  facet_grid(~year.cut)
# Above never really gives a good result, going to work in an interaction between length and year

# fit model of budget ~ length * year, facet on year, play around with arguments
length.loy <- loess(log.budget ~ length * year, data = movie_budgets, family = "symmetric", degree = 1)
length.loy.df <- augment(length.loy)
# Make new variable for year cut to facet on
year.cut <- cut_number(movie_budgets$year, n = 3)
length.loy.df <- data.frame(length.loy.df, year.cut)
# Plot residuals
ggplot(length.loy.df, aes(x = length, y = .resid)) + geom_point() + geom_smooth(method = "loess", span = 1, method.args = list(degree = 1, family = "symmetric")) + geom_abline(slope = 0, intercept = 0) +
  facet_grid(~year.cut)




# fit model of budget ~ year, facet on length, play around with arguments
year.lof <- loess(log.budget ~ year, data = movie_budgets)
year.lof.df <- augment(year.lof)
# Make new variable for length cut to facet on
length.cut <- cut_number(movie_budgets$length, n = 3)
year.lof.df <- data.frame(year.lof.df, length.cut)
# Plot residuals
ggplot(year.lof.df, aes(x = year, y = .resid)) + geom_point() + geom_smooth(method = "loess") + geom_abline(slope = 0, intercept = 0) +
  facet_grid(~length.cut)



###
# fit model of budget ~ year*length, facet on length, play around with arguments
year.lol <- loess(log.budget ~ year*length, data = movie_budgets, degree = 2, family = "symmetric", drop.square = "year", parametric = "length", span = .3)
year.lol.df <- augment(year.lol)
# Make new variable for length cut to facet on
length.cut <- cut_number(movie_budgets$length, n = 3)
year.cut <- cut_number(movie_budgets$year, n = 3)
year.lol.df <- data.frame(year.lol.df, length.cut, year.cut)
# Plot residuals
ggplot(year.lol.df, aes(x = length, y = .resid)) + geom_point() + geom_smooth(method = "loess", span = 1, method.args = list(degree = 1, family = "symmetric")) + geom_abline(slope = 0, intercept = 0) +
  facet_grid(~year.cut)
ggplot(year.lol.df, aes(x = year, y = .resid)) + geom_point() + geom_smooth(method = "loess", span = 1, method.args = list(degree = 1, family = "symmetric")) + geom_abline(slope = 0, intercept = 0) +
  facet_grid(~length.cut)
# THE ONE!!!!!
# "Parametric" argument here used to specify that length should be fitted globally rather than locally. I think this helps because legth has a lot of weird outliers, so fitting it globally avoids 
# giving them too large an influence? 
# "drop.square" argument used for year to specify that that variable should be fit with a linear polynomial, not using a quadratic term.
###

ggplot(year.lol.df, aes(x = .fitted, y = sqrt(abs(.resid)))) + geom_point() + geom_smooth(method = loess)
ggplot(year.lol.df, aes(sample = .resid)) +stat_qq()

year.fit = year.lol.df$.fitted - mean(year.lol.df$.fitted)
year.resid = year.lol.df$.resid
year.lol.long = data.frame(year.fit, year.resid) %>% gather(component, log.budget)
ggplot(year.lol.long, aes(sample = log.budget)) + stat_qq(distribution = "qunif") + facet_grid(~component)
# So, residuals and model are explaining just about the same amount of variation. Model is not perfect, but Brad says that it is pretty good for this dataset.



# Plotting the interaction fit by using the predict command on a new grid of values.
movies.grid <- expand.grid(year = seq(1910, 2000, length = 6), length = seq(10, 400, length = 20))
movies.predict <- predict(year.lol, newdata = movies.grid)

ggplot(data.frame(movies.grid, fit = as.vector(movies.predict)), aes(x = length, y = fit)) + geom_line() + facet_grid(~year)

ggplot(data.frame(movies.grid, fit = as.vector(movies.predict)), aes(x = length, y = fit, group = year, color = factor(year))) + geom_line() 


ggplot(data.frame(movies.grid, fit = as.vector(movies.predict)), aes(x = year, y = fit)) + geom_line() + facet_grid(~length)

ggplot(data.frame(movies.grid, fit = as.vector(movies.predict)), aes(x = year, y = fit, group = length, color = factor(length))) + geom_line() 





######
# Countour plot

movies.grid.contour <- expand.grid(year = seq(1910, 2000, 0.5), length = seq(10, 400, 1))
movies.predict.contour <- predict(year.lol, newdata = movies.grid.contour)
movies.plot.df <- data.frame(movies.grid.contour, fit = as.vector(movies.predict.contour))

ggplot(movies.plot.df, aes(x = year, y = length, z = fit)) + geom_raster(aes(fill = fit)) + 
  coord_fixed() + scale_fill_distiller(palette = "RdYlBu") + geom_contour()

ggplot(movies.plot.df, aes(x = year, y = length, fill = fit)) + geom_raster() + 
  scale_fill_distiller(palette = "RdYlBu") + facet_wrap(~cut_number(fit, n = 16), ncol = 4)



-------------------------------------------------------------------------------
year.lm <- lm(log.budget ~ year * length, data = movie_budgets)
year.lm.df <- augment(year.lm)
# Make new variable for length cut to facet on
length.cut <- cut_number(movie_budgets$length, n = 3)
year.cut <- cut_number(movie_budgets$year, n = 3)
year.lm.df <- data.frame(year.lm.df, length.cut, year.cut)
# Plot residuals
ggplot(year.lm.df, aes(x = length, y = .resid)) + geom_point() + geom_smooth(method = "loess", span = 1, method.args = list(degree = 1, family = "symmetric")) + geom_abline(slope = 0, intercept = 0) +
  facet_grid(~year.cut)


year.rlm <- rlm(log.budget ~ year * length, data = movie_budgets, psi = psi.bisquare)
year.rlm.df <- augment(year.rlm)
# Make new variable for length cut to facet on
length.cut <- cut_number(movie_budgets$length, n = 3)
year.cut <- cut_number(movie_budgets$year, n = 3)
year.rlm.df <- data.frame(year.rlm.df, length.cut, year.cut)
# Plot residuals
ggplot(year.rlm.df, aes(x = year, y = .resid)) + geom_point() + geom_smooth(method = "loess", span = 1, method.args = list(degree = 1, family = "symmetric")) + geom_abline(slope = 0, intercept = 0) +
  facet_grid(~length.cut)


year.fit2 = year.rlm.df$.fitted - mean(year.rlm.df$.fitted)
year.resid2 = year.rlm.df$.resid
year.lol.long2 = data.frame(year.fit2, year.resid2) %>% gather(component, log.budget)
ggplot(year.lol.long2, aes(sample = log.budget)) + stat_qq(distribution = "qunif") + facet_grid(~component)






# Below all useless? Need to redo it with trivariate loess model fitting?

# Pull out residuals of lm for budget on year
year.lm <- lm(log.budget ~ year, data = movie_budgets)
year.lm.df <- augment(year.lm)
# Plot residuals
ggplot(year.lm.df, aes(x = year, y = .resid)) + geom_point() + geom_smooth() + geom_abline(slope = 0, intercept = 0)
# Residuals are super curvy and confidence band doesn't really contain like of y = 0, so linear model doesn't fit well.

# Fit quadratic function for year and pull out residuals
year.lmq <- lm(log.budget ~ year + I(year^2), data = movie_budgets)
year.lmq.df <- augment(year.lmq)
# Plot residuals
ggplot(year.lmq.df, aes(x = year, y = .resid)) + geom_point() + geom_smooth() + geom_abline(slope = 0, intercept = 0)
# Quadratic is no better for year and budget

# Let's try a robust fit using rlm and Tukey's bisquare to deal with all the outlying points.
year.rlm <- rlm(log.budget ~ year, data = movie_budgets)
year.rlm.df <- augment(year.rlm)
# Plot residuals
ggplot(year.rlm.df, aes(x = year, y = .resid)) + geom_point() + geom_smooth() + geom_abline(slope = 0, intercept = 0)
ggplot(year.rlm.df, aes(sample=.resid)) + stat_qq()


# Model year and budget with loess, pull out residuals
year.lo <- loess(log.budget ~ year, data = movie_budgets)
year.lo.df <- augment(year.lo)
# Plot residuals
ggplot(year.lo.df, aes(x = year, y = .resid)) + geom_point() + geom_smooth(method = "loess") + geom_abline(slope = 0, intercept = 0)
# Best result yet! Let's try playing with the span and degree some.

# year and budget loess degree 1
year.lo1 <- loess(log.budget ~ year, data = movie_budgets, degree = 1, span = .5)
year.lo1.df <- augment(year.lo)
# Plot residuals
ggplot(year.lo1.df, aes(x = year, y = .resid)) + geom_point() + geom_smooth(method = "loess") + geom_abline(slope = 0, intercept = 0)
# No difference?


# Model length and budget with loess, pull out residuals
length.lo <- loess(log.budget ~ length, data = movie_budgets)
length.lo.df <- augment(length.lo)
# Plot residuals
ggplot(length.lo.df, aes(x = length, y = .resid)) + geom_point() + geom_smooth(method = "loess") + geom_abline(slope = 0, intercept = 0)
# Pretty good except at the far right edge, let's try again with a symmetric fit

# length budget symmetric
length.los <- loess(log.budget ~ length, data = movie_budgets, family = "symmetric")
length.los.df <- augment(length.los)
# Plot residuals
ggplot(length.los.df, aes(x = length, y = .resid)) + geom_point() + geom_smooth(method = "loess") + geom_abline(slope = 0, intercept = 0)
# Worse than before

# length budget degree 1
length.lo1 <- loess(log.budget ~ length, data = movie_budgets, degree = 1)
length.lo1.df <- augment(length.lo1)
# Plot residuals
ggplot(length.lo1.df, aes(x = length, y = .resid)) + geom_point() + geom_smooth(method = "loess") + geom_abline(slope = 0, intercept = 0)
# Nah, first still best

# length budget degree 1 symmetric
length.los1 <- loess(log.budget ~ length, data = movie_budgets, degree = 1, family = "symmetric")
length.los1.df <- augment(length.los1)
# Plot residuals
ggplot(length.los1.df, aes(x = length, y = .resid)) + geom_point() + geom_smooth(method = "loess") + geom_abline(slope = 0, intercept = 0)
# Worst of all! Normal loess does the best job (degree 2, gaussian)

# length budget span .5
length.lo5 <- loess(log.budget ~ length, data = movie_budgets, span = .5)
length.lo5.df <- augment(length.lo5)
# Plot residuals
ggplot(length.lo5.df, aes(x = length, y = .resid)) + geom_point() + geom_smooth(method = "loess") + geom_abline(slope = 0, intercept = 0)
# That's the one!
