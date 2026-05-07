library(dplyr)
library(ggplot2)

glimpse(diamonds)

# base R vs dplyr ----

# filter
diamonds[diamonds$price > 15000, ]
subset(diamonds, price > 15000)
diamonds |> filter(price > 15000)

diamonds[diamonds$cut == "Ideal" & diamonds$carat >= 1, ]
diamonds |> filter(cut == "Ideal", carat >= 1)

# select
diamonds[, c("carat", "cut", "price")]
diamonds[c("carat", "cut", "price")]
diamonds |> select(carat, cut, price)

diamonds[, !(names(diamonds) %in% c("x", "y", "z"))]
diamonds |> select(-x, -y, -z)

# filter ----

diamonds |>
  filter(price > 15000)

diamonds |>
  filter(cut != "Fair")

diamonds |>
  filter(color %in% c("D", "E", "F"))

diamonds |>
  filter(carat >= 1 & price < 5000)

diamonds |>
  filter(clarity %in% c("IF", "VVS1"), cut == "Ideal", price < 2000)

diamonds |>
  filter(x == 0 | y == 0 | z == 0)

# select ----

diamonds |>
  select(carat, cut, price)

diamonds |>
  select(-x, -y, -z)

diamonds |>
  select(carat:clarity)

diamonds |>
  select(starts_with("c"))

diamonds |>
  select(where(is.numeric))

# arrange ----

diamonds |>
  arrange(price)

diamonds |>
  arrange(desc(carat))

diamonds |>
  arrange(cut, desc(price))

# relocate ----

diamonds |>
  relocate(price)

diamonds |>
  relocate(price, .after = carat)

diamonds |>
  relocate(where(is.numeric), .after = last_col())

# rename ----

diamonds |>
  rename(weight = carat, usd = price)

# mutate ----

diamonds |>
  mutate(price_per_carat = price / carat)

diamonds |>
  mutate(
    volume = x * y * z,
    density = carat / volume
  )

diamonds |>
  mutate(expensive = price > 10000) |>
  arrange(desc(price))

diamonds |>
  mutate(size = case_when(
    carat < 0.5 ~ "small",
    carat < 1 & carat >= 0.5   ~ "medium",
    carat < 2   ~ "large",
    .default    ~ "huge"
  ))

# transmute ----

diamonds |>
  transmute(cut, carat, price_per_carat = price / carat)

diamonds |>
  transmute(
    cut,
    log_price = log(price),
    z_carat = (carat - mean(carat)) / sd(carat)
  )

# summarize ----

diamonds |>
  summarize(
    n = n(),
    avg_price = mean(price),
    sd_price = sd(price),
    max_carat = max(carat)
  )

diamonds |>
  filter(cut == "Ideal") |>
  summarize(median_price = median(price))

# group_by ----

diamonds |>
  group_by(cut) |>
  summarize(avg_price = mean(price), n = n())

diamonds |>
  group_by(cut, color) |>
  summarize(avg_price = mean(price), .groups = "drop")

diamonds |>
  group_by(clarity) |>
  summarize(price_range = max(price) - min(price)) |>
  arrange(desc(price_range))

# count ----

diamonds |>
  count(cut)

diamonds |>
  count(cut, color, sort = TRUE)

diamonds |>
  count(cut, wt = price, sort = TRUE)

# putting it together ----

diamonds_ppc <- diamonds |>
  filter(carat >= 1) |>
  mutate(price_per_carat = price / carat,
         color   = forcats::fct_rev(color), # reverse order of color
         cut     = factor(cut,     ordered = FALSE),
         color   = factor(color,   ordered = FALSE),
         clarity = factor(clarity, ordered = FALSE))

diamonds_ppc_summary <- diamonds_ppc |>
  group_by(cut) |>
  summarize(avg_ppc = mean(price_per_carat), n = n()) |>
  arrange(desc(avg_ppc))

# further analysis ----
# Average price per carat is higher for "Very good" diamond cut than for
# "Premium": why?

# Two visualizations

diamonds_ppc |>
  count(cut, clarity) |>
  group_by(cut) |>
  mutate(prop = n / sum(n)) |>
  ggplot(aes(cut, prop, fill = clarity)) +
  geom_col()

diamonds_ppc |>
  count(cut, color) |>
  group_by(cut) |>
  mutate(prop = n / sum(n)) |>
  ggplot(aes(cut, prop, fill = color)) +
  geom_col()

# There are more "Very good" than "Premium" diamonds with better color and
# clarity, let's check it with a linear model

fit <- lm(log(price) ~ log(carat) + cut + color + clarity, data = diamonds_ppc)

broom::tidy(fit, conf.int = TRUE)

# In general, the coefficients of  color and clarity are larger than those of
# cut meaning they have a greater effect on the price.
