library(ggplot2)
library(tidyr)
library(png)
library(grid)
library(gridExtra)

curry <- read.table("curry2015-16.txt", head = TRUE)


# 1. Plot shots made/missed on loc_x vs. loc_y grid

background <- readPNG("basketball")

g <- rasterGrob(background, interpolate=TRUE) 

ggplot(data = curry, aes(x = LOC_X, y = LOC_Y, col = EVENT_TYPE)) + coord_fixed() + annotation_custom(g, xmin=-250, xmax=250, ymin=-45, ymax=430) + geom_point()


# 2. Fit a logistic regression to predict whether the shot is made, using the single predictor SHOT_DISTANCE. 
# Draw an appropriate ggplot of the fitted curve and write an equation for the fit.

curry.logit <- glm(SHOT_MADE_FLAG ~ SHOT_DISTANCE, family = binomial, data = curry)
# Logistic regression fit through glm() command after specifying the binomial family of fits.

ggplot(data= curry, aes(x = SHOT_DISTANCE, y = SHOT_MADE_FLAG)) + geom_smooth(method = "glm", method.args = list(family = "binomial")) + geom_jitter(height = 0.1, width = 0)

summary(curry.logit)

# Equation for fit: logit(P(shot made)) = 0.54508 - 0.03045 * Shot distance

# 3. Plot the residuals in a way that shows where the logistic regression doesn't fit the data well. 
# Describe in some detail how the model is inaccurate.

curry.model.df <- curry
curry.model.df$.fitted <- fitted.values(curry.logit)
curry.model.df$.resid <- residuals(curry.logit, type = "response")

ggplot(curry.model.df, aes(x = SHOT_DISTANCE, y = .resid)) +geom_point() + geom_smooth(method = "loess", method.args = list(degree = 1))

# From viewing the plot of shot distance vs. the residuals, at the extreme left side (short distances from the basket) the average residuals are positive meaning that the model is underestimating Steph's 
# ability to score in the paint. After that, as we move into the midrange, the average residuals fall into the negative range, meaning that the model is overestimating his shooting ability from about 5-23 feet.
# After 23 feet or so, as we continue to move away from the basket, we can see that the average residuals become quite positive and remain there, meaning that the model once again underestimates Chef Curry's accuracy - this time at long range.

# 4. Fit a better model. You could try a different functional form or a model with more predictors (as long as you use the predictors sensibly.) Your model doesn't have to be perfect, just better. 
# Draw a graph that shows how your model differs from the simple logistic regression, and convince us that your model is better.

better.model <- glm(SHOT_MADE_FLAG ~ SHOT_DISTANCE + SHOT_ZONE_BASIC * SHOT_ZONE_AREA, family = binomial, data = curry)


better.model.df <- curry
better.model.df$.fitted <- fitted.values(better.model)
better.model.df$.resid <- residuals(better.model, type = "response")

first <- ggplot(curry.model.df, aes(x = SHOT_DISTANCE, y = .resid)) +geom_point() + geom_smooth(method = "loess", method.args = list(degree = 1)) + labs(title = "Original model")
better <- ggplot(better.model.df, aes(x = SHOT_DISTANCE, y = .resid)) + geom_point() + geom_smooth(method = "loess", method.args = list(degree = 1)) + labs(title = "Better model")
better

grid.arrange(first, better)

# The residuals in this model are much better behaved than the previous model with the single shot distance predictor.  Adding in the interaction between the two shot zone variables (as there is most certainly an interaction between them in reality)
# really improved the fit and leads to the model accurately predicting Curry's shot percentage at almost all ranges.  The only real remaining problem is in the 10-18ft range where the model still underestimates Steph's probability of making the shot.

