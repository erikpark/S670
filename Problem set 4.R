pew.data <- read.csv("./Pew.data.final.HW4.csv", head = TRUE)

library(ggplot2)
library(GGally)

birthplace <- pew.data[1:14,]
birthplace.score <- birthplace$Standardized.score
language <- pew.data[15:28,]
language.score <- language$Standardized.score
religion <- pew.data[29:42,]
religion.score <- religion$Standardized.score
customs <- pew.data[43:56,]
customs.score <- customs$Standardized.score
country <- customs$Country

score.by.country <- cbind(birthplace, birthplace.score, language.score, religion.score, customs.score)
score.by.country <- score.by.country[,-(2:3)]

# 1. Univariate analysis. Find the total score for each country (adding up the standardized scores for all four questions) and 
#display the results in a table or graph.

total.scores <- rowSums(score.by.country[,-1])
score.by.country <- cbind(score.by.country, total.scores)
final.score.by.country <- score.by.country[,c(1,6)]
final.score.by.country

ggplot(score.by.country, aes(x = country, y = total.scores)) + theme(axis.text.x = element_text(angle=45, vjust = 0.5)) + geom_boxplot() + scale_size(range = c(3,10)) + labs(title = "Total standardized score by country", y = "Total scores", x = "Country")


# 2. Bivariate analysis. Use ggpairs() in the GGally library to create a scatterplot matrix. There should be 4-choose-2 = 6 pairs of 
# variables plotted. Identify which of these six pairs are strongly related, and which are weakly related.

ggpairs(score.by.country, columns = c("birthplace.score", "language.score", "religion.score", "customs.score"))

# From the pairwise correlation values shown in the plot above, it is clear that customs and birthplace are the most strongly related national identity variables across all countries, 
# with religion and customs falling into a rather distant second place.. Conversely, language and customs, language and birthplace, and language and religion are quite weakly related.
# Interstingly (given American religious right political talking points), language doesn't seem to be strongly related to any of the other metrics of national identity.



# 3. Trivariate analysis. You should find that three of the variables are quite strongly related, while the other variable is more 
# weakly related. Draw a scatterplot of the two most strongly correlated variables. Then color the points according to the weakly related variable 
# (e.g. make the points where the weakly related variable is above average one color, and the weakly related variable is below average another color.) 
# What does this tell you that was not obvious from the bivariate analysis?

# Customs, birthplace and religion.


score.by.country$religion.score <- c(0.49968, -0.106866, -0.311285, -0.01743255, 1.3820730, 0.90439, 0.939202, -0.4083846, 1.0561559, -0.53230049, -0.5670624, -0.0570308, -0.233095, -2.6621072395)

avg.religion <- mean(score.by.country$religion.score)
# For some reason calculating the sum (and so, the average) of these values gave super low numbers (e-12 low). I think it was some weird formatting from excel or something.
# So I entered the religion scores manually so I could compute the average, and it worked!


ggplot(score.by.country, aes(x = customs.score, y = birthplace.score, group = religion.score, color = factor(religion.score>avg.religion))) + geom_point() + labs(title="Importance of customs, birthplace, and religion to national identity", x="Standardized customs score", y="Standardized birthplace score", colour = "Higher than average religion score?")


# This plot seems to be saying that in general, respondants in countries with higher than average religion scores (ones who think religion is more important to national identity) also seem to show a positive correlation between 
# seeing local customs and birthplace as important to national identity. So, people in more religious countries are likely to also think that adoption of local customs and birthplace are important to a national identity.
