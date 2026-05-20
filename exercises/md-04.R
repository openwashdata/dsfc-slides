library(readr)
library(readxl)
library(dplyr)

# A simple read_csv ----

read_csv("data/penguins.csv")

# Specifying column types with the short spec
read_csv("data/penguins.csv", col_types = "ccddiici")

# Specifying column types with cols()
read_csv(
  "data/penguins.csv",
  col_types = cols(
    species = col_factor(),
    island  = col_factor(),
    sex     = col_factor()
  )
)

# Reading only some columns
read_csv(
  "data/penguins.csv",
  col_select = c(species, island, starts_with("bill_"))
)

# A tricky European-style CSV ----

# data/sales-tricky.csv combines several pitfalls:
#   - 3 metadata lines on top
#   - semicolon as field separator
#   - comma as decimal mark, period as thousands separator
#   - dates in DD/MM/YYYY format
#   - "N/A" used for missing values

# Attempt 1: read_csv treats the whole line as a single field
read_csv("data/sales-tricky.csv")

# Attempt 2: read_csv2 handles the semicolons but reads the metadata as data
read_csv2("data/sales-tricky.csv")

# Attempt 3: skip the metadata so the column header is on the first row
read_csv2("data/sales-tricky.csv", skip = 4)

# Attempt 4: declare which strings count as missing values
read_csv2("data/sales-tricky.csv", skip = 4, na = c("", "NA", "N/A"))

# Attempt 5: tell readr how decimals, thousands, and dates are written
sales <- read_csv2(
  "data/sales-tricky.csv",
  skip = 4,
  na = c("", "NA", "N/A"),
  locale = locale(
    decimal_mark  = ",",
    grouping_mark = ".",
    date_format   = "%d/%m/%Y"
  ),
  col_types = cols(
    product    = col_character(),
    region     = col_character(),
    quantity   = col_integer(),
    unit_price = col_number(),   # col_number() strips the thousands "."
    sale_date  = col_date()
  )
)
sales

# write_csv ----

write_csv(sales, "data/sales-clean.csv")

# read_excel ----

read_excel("data/datasets.xlsx")
read_excel("data/datasets.xlsx", sheet = "mtcars")
read_excel("data/datasets.xlsx", sheet = 2)

# A tricky Excel file: real data is surrounded by metadata
read_excel("data/deaths.xlsx")
read_excel("data/deaths.xlsx", sheet = "other", range = "A5:F15")

# Forcing column types
read_excel(
  "data/deaths.xlsx",
  sheet = "other",
  range = "A5:F15",
  col_types = c("text", "text", "numeric", "logical", "date", "date")
)

# factor() ----

# Default: levels are taken in alphabetical order
risk_default <- factor(c("low", "high", "medium", "low", "high"))
risk_default
levels(risk_default)

# Explicit levels: control the order
risk <- factor(
  c("low", "high", "medium", "low", "high"),
  levels = c("low", "medium", "high")
)
risk
levels(risk)

# Values not in levels become NA: a built-in typo guard
factor(
  c("low", "med", "high"),
  levels = c("low", "medium", "high")
)

# labels: show pretty names instead of raw codes
satisfaction <- factor(
  c(1, 2, 3, 2, 1, 3),
  levels = c(1, 2, 3),
  labels = c("Unhappy", "Neutral", "Happy")
)
satisfaction

# ordered() ----

# An ordered factor adds a ranking to the levels
risk_ord <- ordered(
  c("low", "high", "medium", "low", "high"),
  levels = c("low", "medium", "high")
)
risk_ord

# Equivalent
factor(
  c("low", "high", "medium", "low", "high"),
  levels  = c("low", "medium", "high"),
  ordered = TRUE
)

# Comparisons: factor vs ordered ----

# Ordered factors support <, >, <=, >=
risk_ord[1] < risk_ord[2]
risk_ord >= "medium"

# Unordered factors do not: this throws an error
risk[1] < risk[2]
