library(ggplot2)
library(GGally)
library(broom)
library(tidyr)
library(fiftystater)
library(colorplaner)
library(mapproj)
library(viridis)

data <- read.csv("data_Food.csv", head = TRUE)
data <- data[-3144,]

data$above <- ifelse(data$PCT_OBESE_ADULTS10<=26.7,0,1)


ggpairs(data, columns = c("GROCPTH07", "SUPERCPTH07", "CONVSPTH07", "SPECSPTH07", "PCT_DIABETES_ADULTS09"), cardinality_threshold = NULL)

ggpairs(data, columns = c("GROCPTH07", "SUPERCPTH07", "CONVSPTH07", "SPECSPTH07", "PCT_DIABETES_ADULTS10"), cardinality_threshold = NULL)

ggpairs(data, columns = c("GROCPTH07", "SUPERCPTH07", "CONVSPTH07", "SPECSPTH07", "PCT_OBESE_ADULTS09"), cardinality_threshold = NULL)

ggpairs(data, columns = c("GROCPTH07", "SUPERCPTH07", "CONVSPTH07", "SPECSPTH07", "PCT_OBESE_ADULTS10"), cardinality_threshold = NULL)

ggpairs(data, columns = c("PC_FFRSALES07", "FFRPTH07", "CONVSPTH07", "SPECSPTH07", "above"), cardinality_threshold = NULL)


# In genearl, see no positive correlation between either adult obesity or diabetes rate and any kind of store type.  Interstingly though,
# do see a rather large at times, negative correlation between the two responses and the number of convenience and speciality stores per 1000 people.  
# A priori I would expect convenience stores to be positively correlated with these two response variables if anything, so this is intriguing and worthy of including in the model.


ggpairs(data, columns = c(as.character("NATAMEN"), "CONVSPTH07", "FSRPTH07", "above", "FFRPTH07"), cardinality_threshold = NULL)

ggpairs(data, columns = c("PCT_DIABETES_ADULTS09", "CONVSPTH07", "SPECSPTH07", "PCT_OBESE_ADULTS10"), cardinality_threshold = NULL)

ggpairs(data, columns = c("PCT_DIABETES_ADULTS09", "CONVSPTH07", "SPECSPTH07", "OBESE10_NUM"), cardinality_threshold = NULL)


# These three, plus state which I can't get a correlation for, seem like good predictors of adult obesity.

ggpairs(data, columns = c("GROCPTH07", "SUPERCPTH07", "CONVSPTH07", "SPECSPTH07", "PCT_OBESE_CHILD08"), cardinality_threshold = NULL)

ggpairs(data, columns = c("GROCPTH07", "SUPERCPTH07", "CONVSPTH07", "SPECSPTH07", "PCT_OBESE_CHILD11"), cardinality_threshold = NULL)


ggplot(data = data, aes(x = log(CONVSPTH07), y = PCT_OBESE_ADULTS10)) + geom_point() + geom_smooth(method = "lm")

ggplot(data = data, aes(x = log(SPECSPTH07), y = PCT_OBESE_ADULTS10)) + geom_point() + geom_smooth(method = "lm")

ggplot(data = data, aes(x = NATAMEN, y = PCT_OBESE_ADULTS10)) + geom_point() + geom_smooth(method = "lm")

ggplot(data = data, aes(x = as.factor(NATAMEN), y = PCT_OBESE_ADULTS10)) + geom_boxplot()

ggplot(data = data, aes(x = as.factor(State), y = PCT_OBESE_ADULTS10)) + geom_boxplot() + theme(axis.text.x = element_text(size=rel(.8)))





# State NATAMEN and obesity maps
data$states <- state.name[match(data$State,state.abb)]

data$states <- tolower(data$states)

ggplot(data, aes(map_id=states)) +  geom_map(aes(fill = NATAMEN), map = fifty_states) + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  theme(legend.position = "bottom", 
        panel.background = element_blank()) + aes(fill2 = PCT_OBESE_ADULTS10) + scale_fill_colorplane() +
  theme(legend.position = "right")
# map of NATAMEN and obesity


ggplot(data, aes(map_id=states)) +  geom_map(aes(fill = NATAMEN), map = fifty_states) + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  theme(legend.position = "bottom", 
        panel.background = element_blank())  +  scale_fill_viridis()


ggplot(data, aes(map_id=states)) +  geom_map(aes(fill = PCT_OBESE_ADULTS10), map = fifty_states) + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  theme(legend.position = "bottom", 
        panel.background = element_blank()) +  scale_fill_viridis()





model.data <- na.omit(data)

model.data$regions <- state.region[match(model.data$State,state.abb)]


#model.conv.ob <- glm(PCT_OBESE_ADULTS10 ~ NATAMEN + State + CONVSPTH07, data = data, na.action = na.exclude)


model.ob <- glm(above ~ CONVSPTH07 + FSRPTH07 + NATAMEN, family = "binomial", data = model.data)
# NATAMEN is: The natural amenities scale is a measure of the physical characteristics of a county area that enhance the location as a place to live. The scale was 
# constructed by combining six measures of climate, topography, and water area that reflect environmental qualities most people prefer. These measures are warm winter, winter sun, temperate summer, low summer humidity, topographic variation, and water area


model.ob.df = model.data
model.ob.df$.fitted = fitted.values(model.ob)
model.ob.df$.resid = residuals(model.ob, type = "response")

ggplot(model.ob.df, aes(x = .fitted, y = .resid)) + geom_point() + geom_smooth(method = "loess", method.args = list(degree = 1))

#ggplot(model.ob.df, aes(sample=.resid)) + stat_qq()

#model.fit <- model.ob.df$.fitted - mean(model.ob.df$.fitted)
#model.resid <- model.ob.df$.resid
#model.lo.long <- data.frame(model.fit, model.resid) %>% gather(component, above)
#ggplot(model.lo.long, aes(sample=above)) + stat_qq(distribution="qunif") + facet_grid(~component)


ggplot(model.data, aes(x = NATAMEN, y = above)) + geom_point() + geom_jitter(width = 0.2, height = 0.05) + geom_smooth(method = "loess", method.args = list(degree=1))


above.pred <- predict(model.ob, type = "response", newdata = model.data)

above.pred.df <- data.frame(model.ob.df, above.prob = as.vector(above.pred))

ggplot(above.pred.df, aes(x = NATAMEN, y = above.prob, color = regions)) + geom_point() + 
  geom_jitter(width = 0.3, height = 0) + labs(colour = "Region", y = "Probability")
# Probability vs NATAMEN colored by region.  Shows that the west generally has higher NATAMEN and lower prob, NC lowest NATAMEN and high prob, and South high prob moderate NATAMEN.

ggplot(above.pred.df, aes(x = regions, y = above.prob, color = as.factor(NATAMEN))) + geom_point() + 
  geom_jitter(width = 0.3, height = 0) +
  labs(colour = "NATAMEN", y = "Probability", x = "Region") 

ggplot(above.pred.df, aes(x = NATAMEN, y = above.prob, color = as.numeric(as.character(POVRATE10)))) + geom_point() + 
  facet_wrap("regions") + geom_jitter(width = 0.3, height = 0) + 
  scale_color_viridis(option = "inferno") + labs(colour = "Poverty Rate", y = "Probability") 

ggplot(above.pred.df, aes(x = as.numeric(as.character(POVRATE10)), y = above.prob, color = as.factor(NATAMEN))) + 
  geom_point() + facet_wrap("regions") + 
  geom_jitter(width = 0.3, height = 0) + labs(colour = "NATAMEN", y = "Probability", x = "Poverty Rate") 
# Plot showing probability of above, vs poverty rate, colored by NATAMEN and faceted by region.  Interesting because the West is much different than other regions, and south and north central both show really dramatic relationships of increased probability with increased poverty rate.
