library(ggplot2)
library(ggthemes)

glimpse(mpg)

ggplot(mpg, aes(x = displ, y = cty)) +
  geom_point(color = "blue")

ggplot(mpg, aes(x = drv)) +
  geom_bar(aes(fill = drv), color = "black")

ggplot(mpg, aes(x = drv, fill = drv)) +
  geom_bar(color = "black")

ggplot(mpg) +
  geom_bar(aes(x = drv, fill = drv), color = "black")

ggplot(mpg, aes(x = displ, y = cty)) +
  geom_point(aes(color = drv))

ggplot(mpg, aes(x = displ, y = hwy, color = drv)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_colorblind() +
  theme_economist()

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = drv)) +
  geom_smooth(method = "lm", se = FALSE) +
  theme( panel.grid.major.x = element_blank())


