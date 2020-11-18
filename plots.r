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
  scale_fill_manual(values = c("#00ff00", "#0000ff")) +
  scale_x_continuous(breaks = round(seq(
    min(points$points), max(points$points), by = 3
  ), 1)) +
  labs(x = "Total Cup Points", y = "Density", title = "Total Cup Points distribution by species")

library("corrplot")

corrplot(
  cor(arabica[c(13, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 36, 42, 43, 44)], use =
        "na.or.complete", method = "spearman"),
  method = "number",
  order = "hclust"
)

corrplot(
  cor(robusta[c(13, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 36, 42, 43, 44)], use =
        "na.or.complete", method = "spearman"),
  method = "number",
  order = "hclust"
)

ggplot(
  arabica,
  aes(
    x = Acidity,
    y = altitude_mean_meters,
    size = Total.Cup.Points,
    color = Country.of.Origin
  )
) + scale_colour_manual(
  name = "Origin",
  values = c(
    "#808080",
    "#556b2f",
    "#006400",
    "#8b0000",
    "#808000",
    "#483d8b",
    "#3cb371",
    "#008080",
    "#4682b4",
    "#000080",
    "#9acd32",
    "#32cd32",
    "#daa520",
    "#7f007f",
    "#b03060",
    "#d2b48c",
    "#ff4500",
    "#00ced1",
    "#ff8c00",
    "#ffff00",
    "#7fff00",
    "#00fa9a",
    "#8a2be2",
    "#dc143c",
    "#9370db",
    "#0000ff",
    "#f08080",
    "#ff7f50",
    "#ff00ff",
    "#1e90ff",
    "#f0e68c",
    "#dda0dd",
    "#add8e6",
    "#ee82ee",
    "#7fffd4",
    "#ff69b4",
    "#ffc0cb"
  )
) + scale_size(
  name = "Total Cup Points",
  breaks = c(85, 87.5, 90),
  range = c(4, 20),
  trans = "exp"
) + geom_point(alpha = .75) + xlim(5.25, 8.75) + ylim(0, 2750) + labs(x = "Acidity", y = "Altitude [m]", title = "Acidity by altitude (Total Cup Points as the bubble size)")