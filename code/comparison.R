library(dplyr)
library(lubridate)
library(excessmort)
library(ggplot2)

str(puerto_rico_counts)
dim(puerto_rico_counts)

setwd("~/Downloads/bst260-final/code")
excess_mort_pdf <- read.csv("./../data/excess_mort.csv") # 2015, 2016, 2017

month_lookup <- c("JAN" = 1, "FEB" = 2, "MAR" = 3, "APR" = 4, "MAY" = 5, "JUN" = 6,
                  "JUL" = 7, "AGO" = 8, "SEP" = 9, "OCT*" = 10, "NOV" = 11, "DEC" = 12)

excess_mort_pdf <- excess_mort_pdf |>
  group_by(year, month) |>
  summarise(count = sum(count, na.rm = TRUE), .groups = "drop") |>
  mutate(month = as.numeric(month_lookup[month])) |>
  arrange(year, month)

str(excess_mort_pdf)
colnames(excess_mort_pdf) 
View(excess_mort_pdf)

excess_mort_15_17 <- puerto_rico_counts |> mutate(date = as.Date(date)) |> 
  filter(date > as.Date("2014-12-31") & date < as.Date("2017-12-01")) |> 
  mutate(year = year(date), month = month(date)) |> 
  group_by(year, month) |>
  summarise(outcome = sum(outcome, na.rm = TRUE), .groups = "drop")

excess_mort_pdf$count == excess_mort_15_17$outcome
excess_mort_pdf$count - excess_mort_15_17$outcome

comparison_table <- excess_mort_15_17
colnames(comparison_table)[colnames(excess_mort_15_17) == "outcome"] <- "excessmort_count"
comparison_table$NYT_count <- c(excess_mort_pdf$count)
comparison_table$count_diff <- comparison_table$excessmort_count - comparison_table$NYT_count
totals <- colSums(comparison_table)
comparison_table <- rbind(comparison_table, totals)
comparison_table[nrow(comparison_table), 1:2] <- NA
View(comparison_table)


str(excess_mort_15_17)
colnames(excess_mort_pdf) 
View(excess_mort_15_17)

weekly_counts <- puerto_rico_counts |> 
  mutate(age_group_category = case_when(
    agegroup %in% c("0-4", "5-9", "10-14","15-19") ~ "Child",
    agegroup %in% c("20-24", "25-29","30-34","35-39","40-44") ~ "Young Adult",
    agegroup %in% c("45-49","50-54","55-59","60-64") ~ "Adult",
    TRUE ~ "Senior Citizens")) |>
  mutate(date = as.Date(date)) |> 
  mutate(date = floor_date(date, unit = "week", week_start = 3)) |>
  group_by(date, sex, age_group_category) |> 
  summarize(outcome = sum(outcome, na.rm = TRUE), 
            population = mean(population, na.rm = TRUE), 
            n = n(), .groups = "drop") |>
  select(-n) |>
  mutate(sex = as.factor(sex), age_group_category = as.factor(age_group_category), 
         year = year(date),
         month = month(date))

weekly_counts_16 <- weekly_counts |> filter(date < as.Date("2017-01-01"))
weekly_counts_17 <- weekly_counts |> filter(date > as.Date("2016-12-31") & date < as.Date("2018-01-01"))

weekly_counts_16

fit <- weekly_counts_16 |> glm(outcome ~ age_group_category + sex + year + month, 
                               offset = log(population), 
                               data = _, family = poisson())
summary(fit)
prediction <- predict(fit, newdata = weekly_counts_17, se.fit=TRUE)
prediction$fit

weekly_counts_17 |> mutate(outcome_hat = prediction$fit,
              se = prediction$se.fit, 
              sigma = sd(fit$resid)) |>
  filter(date > as.Date("2016-12-31")) |> 
  mutate(excess = outcome - outcome_hat) |> 
  group_by(date) |> 
  summarize(excess = sum(excess), se = sqrt(sum(sigma^2 + se^2)), 
            .groups = "drop") |>
  mutate(lower_ci = excess - 1.96 * se,
         upper_ci = excess + 1.96 * se) |>
  ggplot(aes(x = date, y = excess)) +
  geom_line() +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci)) +
  labs(title = "Excess Mortality Estimate with Confidence Intervals",
       x = "Week",
       y = "Excess Mortality Estimate"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 12))
