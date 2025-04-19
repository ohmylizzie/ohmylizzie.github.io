# Load packages
library(tidyverse)
library(janitor)

# Load and clean data
data <- read.csv("./Butterfly+moths_folder/33a66d6a-dd9b-4a19-9026-cf1ffb969cdb/data/ecological_traits_2022.csv")
colnames(data) <- as.character(data[1, ])
data <- data[-1, ]
colnames(data) <- make.names(colnames(data), unique = TRUE)

# Fix rarity column
data$rarirty_gb <- ifelse(data$rarirty_gb == "", "Not Assessed", data$rarirty_gb)
data$rarirty_gb <- case_when(
  data$rarirty_gb == "recent (year 2000 onwards) colonist not assessed" ~ "Too Recent to Assess",
  TRUE ~ data$rarirty_gb
)

# Make is_scarce column
data <- data %>%
  mutate(is_scarce = ifelse(rarirty_gb == "Nationally Scarce", 1, 0))

# Keep only numeric or binary-like columns for modeling
data_clean <- data %>%
  select(where(~n_distinct(.) > 1)) %>%  # remove constant columns
  select(where(is.numeric))              # keep only numeric columns

# Add back the response variable
data_clean$is_scarce <- data$is_scarce

# Remove NA rows to avoid glm errors
data_model <- data_clean %>% drop_na()

# Optionally limit to first 500 rows for speed
data_model <- head(data_model, 500)

# Fit logistic regression
model <- glm(is_scarce ~ ., data = data_model, family = "binomial")

# Save the model to disk
saveRDS(model, file = "scarcity_model.rds")