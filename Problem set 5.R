movie_budgets <- read.table("/media/Datas/S670/movie_budgets.txt", head = TRUE)
movie_budgets$log.budget <- log10(movie_budgets$budget)

ggplot(movie_budgets, aes(x = year, y = log.budget)) + geom_point()

ggplot(movie_budgets, aes(x = length, y = log.budget)) + geom_point()

ggplot(movie_budgets, aes(x = year, y = log.budget)) + geom_point() + geom_smooth(method = "lm")

ggplot(movie_budgets, aes(x = length, y = log.budget)) + geom_point() + geom_smooth(method = "loess")

