# MODULE 7 - EXERCISES

library(tidyr)
library(dplyr)

# tidyr example tables ----
# The same data organized in four different ways; only table1 is tidy

table1
table2
table3
table4a

# Case 1: column names are values ----

table4a |>
  pivot_longer(
    cols      = c(`1999`, `2000`),
    names_to  = "year",
    values_to = "cases"
  )

# Case 2: one observation spread over several rows ----

table2 |>
  pivot_wider(
    names_from  = type,
    values_from = count
  )

# pivot_longer() and pivot_wider() are inverses

table4a |>
  pivot_longer(cols = c(`1999`, `2000`), names_to = "year", values_to = "cases") |>
  pivot_wider(names_from = year, values_from = cases)

# split one column into many ----

table3 |>
  separate_wider_delim(
    rate,
    delim = "/",
    names = c("cases", "population")
  )

# combine several columns into one ----

table1 |>
  unite("country_year", country, year, sep = "_")

# nest() and unnest(): a column of tibbles ----

nested <- table1 |>
  nest(data = c(year, cases, population))

nested

# unnest() expands those cells back into ordinary rows.
nested |>
  unnest(data)

# Treating missing values ----

sales <- tibble(
  quarter = c("Q1", NA, NA, "Q2"),
  revenue = c(100, 150, NA, 200)
)
sales

# fill() carries the last value down; replace_na() swaps NA for a value.
sales |>
  fill(quarter) |>
  replace_na(list(revenue = 0))

# drop_na() removes any row that still contains an NA.
sales |>
  drop_na()

# join functions ----
# dplyr ships two tiny tables sharing a name column.

band_members
band_instruments

# left_join() keeps every row of x, adding columns from y where they match.
band_members |>
  left_join(band_instruments, by = "name")

# Exercise ----
# tidyr ships table4a (cases) and table4b (population), both with years as columns:

table4a
table4b

# Combine them into a single tidy table of rates:
#   1. pivot each table longer so year becomes a single column
#   2. join the two results by country and year
#   3. add a rate column (cases per 10,000 people)
# Try it before looking at one possible answer below.

cases <- table4a |>
  pivot_longer(c(`1999`, `2000`), names_to = "year", values_to = "cases")

population <- table4b |>
  pivot_longer(c(`1999`, `2000`), names_to = "year", values_to = "population")

cases |>
  left_join(population, by = c("country", "year")) |>
  mutate(rate = cases / population * 10000)
