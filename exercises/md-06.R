library(tibble)
library(dplyr)

# data.frame() vs tibble(): the same call, different behaviours ----

df <- data.frame(
  name   = c("Luke", "Leia", "R2-D2"),
  height = c(172, 150, 96),
  droid  = c(FALSE, FALSE, TRUE)
)

tb <- tibble(
  name   = c("Luke", "Leia", "R2-D2"),
  height = c(172, 150, 96),
  droid  = c(FALSE, FALSE, TRUE)
)

# Printing ----

data.frame(x = 1:100, y = letters[rep(1:4, 25)])
tibble(x = 1:100, y = letters[rep(1:4, 25)])

# Recycling ----

data.frame(
  name  = c("Luke", "Leia", "R2-D2", "C-3PO"),
  score = c(1, 2)
)

tibble(
  name = c("Luke", "Leia", "R2-D2"),
  film = "A New Hope"
)

tibble(
  name  = c("Luke", "Leia", "R2-D2", "C-3PO"),
  score = c(1, 2)
)

# Column names ----
data.frame(
  name          = c("Luke", "Leia", "R2-D2"),
  `height (cm)` = c(172, 150, 96)
)

tibble(
  name          = c("Luke", "Leia", "R2-D2"),
  `height (cm)` = c(172, 150, 96)
)

# Use a column just defined ----

tibble(
  height_cm = c(172, 150, 96),
  height_m  = height_cm / 100
)

data.frame(
  height_cm = c(172, 150, 96),
  height_m  = height_cm / 100
)



# Vectorization: many loops are one line in R ----

heights <- c(172, 150, 96, 202)

# Converting every height to meters
heights_m <- vector("double", length(heights))

for (i in seq_along(heights)) {
  heights_m[i] <- heights[i] / 100
}
heights_m

heights / 100

# Counting how many heights are above 150
n_tall <- 0

for (i in seq_along(heights)) {
  if (heights[i] > 150) {
    n_tall <- n_tall + 1
  }
}
n_tall

sum(heights > 150)


# Exercise: the mean of a "numeric" column ----

characters <- tibble(
  name   = c("Luke", "Leia", "R2-D2", "C-3PO", "Chewbacca", "Han"),
  height = c("172", "150", "96", "unknown", "228", "?")
)

mean(characters$height)

typeof(characters$height)

unique(characters$height)

characters <- characters |>
  mutate(
    height = case_when(
      height %in% c("unknown", "?") ~ NA,
      .default = height
    ),
    height = as.numeric(height)
  )
characters

mean(characters$height, na.rm = TRUE)

# Exercise: collecting loop results in a vector vs a list ----
# Each iteration produces a single value of the same type

set.seed(42)
sizes <- c(5, 50, 500, 50000)

sample_means <- vector()

for (i in seq_along(sizes)) {
  sample_means[i] <- mean(rnorm(sizes[i], mean = 170, sd = 10))
}
sample_means

# Each iteration produces a whole vector, and of a different length
samples <- list()

for (i in seq_along(sizes)) {
  samples[[i]] <- rnorm(sizes[i], mean = 170, sd = 10)
}
str(samples)

for (i in seq_along(samples)) {
  print(mean(samples[[i]]))
}
