# MODULE 2 - EXERCISES

library(ggplot2)
library(ggthemes)

dplyr::glimpse(mpg)

# a fixed color goes outside aes(): inside, "blue" is treated as data
# and mapped to the default color scale
ggplot(mpg, aes(x = displ, y = cty, color = "blue")) +
  geom_point()

ggplot(mpg, aes(x = displ, y = cty)) +
  geom_point(color = "blue")

# three equivalent ways of writing the same bar chart:
# aesthetics can be mapped in the geom or in ggplot()
ggplot(mpg, aes(x = drv)) +
  geom_bar(aes(fill = drv), color = "black")

ggplot(mpg, aes(x = drv, fill = drv)) +
  geom_bar(color = "black")

ggplot(mpg) +
  geom_bar(aes(x = drv, fill = drv), color = "black")

# mapping color to a variable goes inside aes()
ggplot(mpg, aes(x = displ, y = cty)) +
  geom_point(aes(color = drv))

# color in ggplot() applies to all geoms: one regression line per group
ggplot(mpg, aes(x = displ, y = hwy, color = drv)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_colorblind() +
  theme_economist()

# color only in geom_point(): a single line for all points,
# and theme() adjusts individual elements of the plot
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = drv)) +
  geom_smooth(method = "lm", se = FALSE) +
  theme( panel.grid.major.x = element_blank())
