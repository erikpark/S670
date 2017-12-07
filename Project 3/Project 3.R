library(ggplot2)
library(broom)
library(tidyr)
library(gridExtra)

load("CCES2016_Common_FirstRelease.RData")
election <- x
election <- election[election$tookpost == "Yes",]
election <- election[election$CC16_326 == "Barack Obama",]
election$switch <- ifelse(election$CC16_410a == "Donald Trump (Republican)", 1, 0)

election2 <- election %>% drop_na(switch)


election.model <- glm(switch ~ gender + educ + race, family = "quasibinomial", weights = commonweight_post, data = election2)

summary(election.model)

election.model.df <- election2
election.model.df$.fitted <- fitted.values(election.model)
election.model.df$.resid <- residuals(election.model, type = "response")

ggplot(election.model.df, aes(x = .fitted, y = .resid)) +geom_point() + geom_smooth(method = "loess", method.args = list(degree = 1))

ggplot(election.model.df, aes(x = educ, y = .resid)) +geom_point() + geom_smooth(method = "loess", method.args = list(degree = 1))

switch.pred <- predict(election.model, type = "response", newdata = election2)

switch.pred.df <- data.frame(election.model.df, switch.prob = as.vector(switch.pred))

ggplot(switch.pred.df, aes(x = race, y = switch.prob, color = gender)) + facet_wrap("educ") + geom_point()


#2 Improve model

election3 <- election2 %>% drop_na(marstat)

election3$married <- ifelse(election3$marstat == "Married", 1, 0)

election4 <- election2 %>% drop_na(sexuality)

election4$straight <- ifelse(election4$sexuality == "Heterosexual / straight", "yes", "no")

election.model.2 <- glm(switch ~ gender + educ + race + straight, family = "quasibinomial", weights = commonweight_post, data = election4)

summary(election.model.2)

election.model.df.2 <- election4
election.model.df.2$.fitted <- fitted.values(election.model.2)
election.model.df.2$.resid <- residuals(election.model.2, type = "response")


ggplot(election.model.df.2, aes(x = .fitted, y = .resid)) +geom_point() + geom_smooth(method = "loess", method.args = list(degree = 1))

switch.pred.2 <- predict(election.model.2, type = "response", newdata = election4)

switch.pred.df.2 <- data.frame(election.model.df.2, switch.prob = as.vector(switch.pred.2))

ggplot(switch.pred.df.2, aes(x = race, y = switch.prob, color = gender)) + facet_wrap(~ educ + straight) + geom_point()
