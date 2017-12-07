library(ggplot2)
library(tidyr)
library(MASS)
library(broom)

barley <- read.table("minnesota.barley.yield.txt", head = TRUE)

barley.factor <- barley
barley.factor$year <- factor(barley.factor$year)

barley.factor <- barley.factor[!barley.factor$year == "1934",]

barley.test <- barley.factor

barley.test <- barley.test[!barley.test$year == "1934",]
barley.test <- barley.test[!barley.test$year == "1928",]
barley.test <- barley.test[!barley.test$year == "1933",]

barley.test$year <- factor(barley.test$year)

barley.test <- barley.test[!barley.test$site == "Waseca",]

barley.test <- barley.test[!barley.test$gen == "Jeans",]
barley.test <- barley.test[!barley.test$gen == "CompCross",]
barley.test <- barley.test[!barley.test$gen == "MechMixture",]
barley.test <- barley.test[!barley.test$gen == "Dryland",]
barley.test <- barley.test[!barley.test$gen == "Colsess",]
barley.test <- barley.test[!barley.test$gen == "Spartan",]
barley.test <- barley.test[!barley.test$gen == "Minsturdi",]
barley.test <- barley.test[!barley.test$gen == "No475",]
barley.test <- barley.test[!barley.test$gen == "Heinrichs",]
barley.test <- barley.test[!barley.test$gen == "Oderbrucker",]
barley.test <- barley.test[!barley.test$gen == "Odessa",]
barley.test <- barley.test[!barley.test$gen == "SD1340",]
barley.test <- barley.test[!barley.test$gen == "WisNo38",]
barley.test <- barley.test[!barley.test$gen == "No474",]
barley.test <- barley.test[!barley.test$gen == "Svansota",]
barley.test <- barley.test[!barley.test$gen == "ManxSA",]
barley.test <- barley.test[!barley.test$gen == "SAxMan",]











# 1 plotting yield by year for each location


ggplot(barley.test, aes(x = yield, y = gen, color = year)) + geom_point() + facet_wrap(~site)

ggplot(barley, aes(x = yield, y = gen)) + geom_point() + facet_wrap(~site + year)



ggplot(barley.factor, aes(x = yield, y = year, color = (site == "Waseca"))) + geom_point() + facet_wrap(~gen)


ggplot(barley, aes(x = year, y = yield)) + geom_col() + facet_wrap(~site)


ggplot(barley.test, aes(x = year, y = yield, col = gen)) + geom_col() + facet_wrap(~site)



ggplot(barley.test, aes(x = yield, y = year, color = gen)) + geom_point() + facet_wrap(~site)


# 2 fitting a model yield ~ gen, year, site. Fit robust

model.rlm <- rlm(yield ~ site*year + gen, data = barley.test, psi = psi.bisquare)
  
model.rlm <- rlm(yield ~ site + year + gen, data = barley.factor, psi = psi.bisquare)
model.rlm.df <- augment(model.rlm)
model.rlm.df$.fitted = model.rlm.df$.fitted - mean(model.rlm.df$.fitted)
model.rlm.long = model.rlm.df %>% gather(component, value, c(.fitted, .resid))
ggplot(model.rlm.long, aes(sample = value)) + stat_qq(distribution = "qunif") + 
  facet_grid(~component)
ggplot(model.rlm.df, aes(x = site, y = .resid)) + geom_point() + geom_smooth(method = "loess", span = 1, method.args = list(degree = 1, family = "symmetric")) + geom_abline(slope = 0, intercept = 0)



# 3 Drawing plots using the model to explore 1931-32 weirdness

model.effects <- dummy.coef(model.rlm)
year.site.effect <- outer(model.effects$year, model.effects$site, "+")
years <- rep(row.names(year.site.effect), 6)
sites <- rep(colnames(year.site.effect), each = 9)
sites <- factor(sites, levels = names(model.effects$site))
year.site.df <- data.frame(year = years, site = sites, effect = as.vector(year.site.effect))
ggplot(year.site.df, aes(x = effect, y = site, col = year)) + geom_point(size = 3)


gen.effects <- sort(model.effects$gen)
varieties <- factor(names(model.effects$gen), levels = names(model.effects$gen))
variety.df <- data.frame(effect = gen.effects, variety = varieties)
ggplot(variety.df, aes(x = effect, y = variety)) + geom_point()


ggplot(model.rlm.df, aes(x = .resid, y = site)) + geom_point() + facet_wrap(~gen + year)
# Really small hard to look at plot, but there doesnt seem to be any real pattern left to the residuals here.

ggplot(model.rlm.df, aes(x = .resid, y = gen)) + geom_point() + facet_wrap(~site + 
year) + theme(axis.text.y = element_text(size = 7))
# Again, hard to look at, but it does seem like there is an interaction between site and year.  Same site shows similar residual patterns over the years in most cases.  Would really like to get that interaction working...