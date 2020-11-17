arabica <-
  read.csv(
    url(
      "https://raw.githubusercontent.com/jldbc/coffee-quality-database/master/data/arabica_data_cleaned.csv"
    )
  )
robusta <-
  read.csv(
    url(
      "https://raw.githubusercontent.com/jldbc/coffee-quality-database/master/data/robusta_data_cleaned.csv"
    )
  )


pointsArabica <-
  data.frame(points = arabica$Total.Cup.Points, species = "arabica")
pointsRobusta <-
  data.frame(points = robusta$Total.Cup.Points, species = "robusta")

points <- rbind(pointsArabica, pointsRobusta)

library(ggplot2)

ggplot(data = points, aes(x = points)) +
  geom_density(aes(fill = species, alpha = species)) +
  scale_alpha_manual(values = c(1, 0.25)) +
  scale_fill_manual(values = c("light blue", "red")) +
  scale_x_continuous(breaks = round(seq(
    min(points$points), max(points$points), by = 3
  ), 1)) +
  labs(x = "Total Cup Points", y = "Density", title = "Total Cup Points distribution by species")

library("corrplot")

corrplot(cor(arabica[c(13, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 36, 42, 43, 44)], use =
               "na.or.complete", method = "spearman"),
         method = "number")

corrplot(cor(robusta[c(13, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 36, 42, 43, 44)], use =
               "na.or.complete", method = "spearman"),
         method = "number")