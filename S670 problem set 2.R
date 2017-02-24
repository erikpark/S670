library(ggplot2)
library(tidyr)

votes <- read.table("./pennsylvania.txt", header = TRUE)

#1. use gather to convert votes data to long form.

votes.long <- votes %>% gather(Candidate, Votes, Clinton:Obama)

# 2. reproduce normal QQ plot of log_10 transformed votes data, with clinton and obama as the facet.

ggplot(votes.long, aes(sample = Votes)) + stat_qq() + facet_wrap(~Candidate)
# QQ plot of untransformed data looks right shit mate!

votes.long.log <- votes.long
votes.long.log[,3] <- log10(votes.long[,3])
# used to log transform just one comumn (the votes). On left, specify which column is changing. Then on right do the operation to just that column.

ggplot(votes.long.log, aes(sample = Votes)) + stat_qq() + facet_wrap(~Candidate) + geom_abline(intercept = 4, slope = .7)

# From this plot, we can conclude that the log_10 transformed data (while certainly better than the untransformed data!) still isn't really normal. 
# Despite the transformation there is a hump in the left side of the plot for both candidates, indicating that the distribution seems to be left skewed slightly for both. 

# 3. Is the relationship between Clinton and Obama's votes additive, multiplicative, or more complicated than that?

ggplot(votes.long.log, aes(sample = Votes)) + stat_qq(distribution = qunif) + facet_wrap(~Candidate)
ggplot(votes.long, aes(sample = Votes)) + stat_qq(distribution = qunif) + facet_wrap(~Candidate)
# Should look at doing inverse transformation on the data, see if that looks better. If the data seem really similar after that, could just be noise and not really any difference there.

ggplot(votes.long.log, aes(x = County, y = Votes)) + geom_point() + facet_wrap(~Candidate)
#omg no

#Set up for two sample qq plot below, will plot against y=x line to see what kind of shift we have here.
Clinton <- votes.long$Votes[votes.long$Candidate=="Clinton"]
#Isolate clinton votes
Obama <- votes.long$Votes[votes.long$Candidate=="Obama"]
#Isolate Obama votes
qq.df <- as.data.frame(qqplot(Clinton, Obama, plot.it = FALSE))
#Combine two into dataframe, with nothing else.

ggplot(qq.df, aes(x=x,y=y)) + geom_point() + geom_abline()
#Two sample QQ plot of obama vs clinton votes against y=x line. doesn't seem to be any shift present.

ggplot(qq.df, aes(x=(x+y)/2, y=y-x)) + geom_point() + geom_abline(slope=0)
# This Tukey mean-difference plot does seem to show a real difference between the two here.


# Let's try with log transformed data now.


Clinton.log <- votes.long.log$Votes[votes.long.log$Candidate=="Clinton"]
Obama.log <- votes.long.log$Votes[votes.long.log$Candidate=="Obama"]
qq.df.log <- as.data.frame(qqplot(Clinton.log, Obama.log, plot.it = FALSE))

ggplot(qq.df.log, aes(x=x, y=y)) + geom_point() + geom_abline()
#! Probably wrong -- The slope of the two-sample QQ plot seems to largely be the same as the y=x line, this seems to suggest that the difference in votes between Obama and CLinton is additive.
# The log transformed data seems to be well described as a straight line above, but intersecting with the line y=x. This is indicative of a multiplicative shift as we are working with log_10 transformed values. 
# The slope of the two-sample QQ plot seems to largely be the same as the y=x line, just shifted  this seems to suggest that the difference in votes between Obama and CLinton is multiplicative.


ggplot(qq.df.log, aes(x=(x+y)/2, y=y-x)) + geom_point() + geom_abline(slope=0)
# Here the story is a bit more complicated. It seems to be that if the average vote total is high (right side of x), the difference between y (Clinton votes) and x (Obama votes) is negative, meaning Clinton earned more votes in high turnout places than did Obama.
# On the other hand, if the average number of votes (turnout) was low or middling, the difference between y and x is positive, meaning that Obama earned more votes. This second case held true for the majority of datapoints, so for the majority of counties in Pennsylvania Obama got more votes than Clinton did.
# These graphs together seem to be saying that the relationship between the votes for Obama and Clinton is more complicated than just additive vs multiplicative.