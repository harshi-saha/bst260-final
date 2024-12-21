library(dplyr)
library(lubridate)
library(tidyverse)
library(ggplot2)
library(devtools)
install_github("rafalab/excessmort")
library(excessmort)
library(ggplot2)
library(MASS)
library(car)
library(lmtest)
library(devtools)


# Q1
counts <- puerto_rico_counts |> mutate(date = ymd(date))
counts |> 
  mutate(year = year(date)) |>
  group_by(year,sex,agegroup) |>
  summarize(population = mean(population), .groups="drop") |>
  ggplot() + geom_line(aes(x = year, y = population, color = agegroup)) +
  labs(title = "Population Trends by Age Group and Sex",
       x = "Year",
       y = "Population") +
  facet_wrap(~sex) + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 28),
        axis.text.x = element_text(size = 24), 
        axis.text.y = element_text(size = 24),
        axis.title.x = element_text(size = 22), 
        axis.title.y = element_text(size = 22),
        strip.text = element_text(size = 22),
        legend.position = "right", legend.box = "vertical",
        legend.text = element_text(size = 24),
        legend.title = element_text(size = 24))
counts |>
  mutate(year=year(date)) |>
  mutate(age_group_category = case_when(
    agegroup %in% c("0-4", "5-9", "10-14","15-19") ~ "Child",
    agegroup %in% c("20-24", "25-29","30-34","35-39","40-44") ~ "Adult",
    agegroup %in% c("45-49","50-54","55-59","60-64") ~ "Older Adult",
    TRUE ~ "Senior Citizens")) |>
  group_by(year, age_group_category, sex) |>
  summarize(total_population = sum(population, na.rm = TRUE), 
            .groups = "drop") |>
  group_by(year,sex) |>
  mutate(
    year_total_population = sum(total_population),
    percentage = (total_population / year_total_population) * 100) |>
  ggplot() + geom_line(aes(x = year, y = percentage, 
                           color = age_group_category)) + 
  labs(title = "Population Trends by Age Group Category and Sex",
       x = "Year",
       y = "Percentage of the Population") +
  facet_wrap(~sex) + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 28),
        axis.text.x = element_text(size = 24), 
        axis.text.y = element_text(size = 24),
        axis.title.x = element_text(size = 22), 
        axis.title.y = element_text(size = 22),
        strip.text = element_text(size = 22),
        legend.position = "right", legend.box = "vertical",
        legend.text = element_text(size = 24),
        legend.title = element_text(size = 24))

# Q2
# data processing
weekly_counts <- puerto_rico_counts |> 
  mutate(date = as.Date(date)) |> 
  mutate(date = floor_date(date, unit = "week", week_start = 3)) |> # start defined as Wed due to the date when Maria hurricane hit
  group_by(date, sex, agegroup) |> 
  summarize(outcome = sum(outcome, na.rm = TRUE), 
            population = mean(population, na.rm = TRUE), 
            n = n(), .groups = "drop") |>
  filter(n==7) |>
  mutate(sex = as.factor(sex), agegroup = as.factor(agegroup), 
         year = year(date),
         month = as.factor(month(date)),
         week = as.factor(week(date)),
         day = difftime(date, min(date), units = "day"),
         rate = 1000*outcome/population)

# filtering to obtain data before 2017
weekly_counts_16 <- weekly_counts |> filter(date < as.Date("2017-01-01"))

# model fitting
fit1 <- weekly_counts_16 |> glm(outcome ~ agegroup + sex + day + week, 
                                offset = log(population), 
                                data = _, family = poisson())
summary(fit1)
vif(fit1) # no VIFs are above 2, indicating no multicollinearity

# some of coefficients corresponding to weeks are not statistically significant,
# training the model without weeks
fit2 <- weekly_counts_16 |> glm(outcome ~ agegroup + sex + day, 
                                offset = log(population), 
                                data = _, family = poisson())
summary(fit2)
# performing LRT to understand whether inclusion of the weeks improves model fit
anova(fit2,fit1, test = "Chisq") # according to p value, weeks should be included

# assessing effect modification, an interaction term between agegroup and sex
fit3 <- weekly_counts_16 |> glm(outcome ~ agegroup + sex + week + day 
                                + agegroup*sex, 
                                offset = log(population), 
                                data = _, family = poisson())
summary(fit3)
anova(fit3,fit1, test = "Chisq") # according to p value, interaction term should be included

# assessing effect modification, an interaction term between sex and week
fit4 <- weekly_counts_16 |> glm(outcome ~ agegroup+ sex + week + day 
                                + agegroup*sex + sex*week, 
                                offset = log(population), 
                                data = _, family = poisson())
summary(fit4) # most of the coefficient corresponding to interaction are not statistically significant,
# this interaction term will not be included in the model

# assessing effect modification, an interaction term between day and agegroup
fit5 <- weekly_counts_16 |> glm(outcome ~ agegroup + sex + week + day  + agegroup*sex + day*agegroup, 
                                offset = log(population), 
                                data = _, family = poisson())
summary(fit5) # # most of the coefficient corresponding to interaction are statistically significant
anova(fit5,fit3, test = "Chisq")  # according to p value, interaction term should be included

# model 5 was chosen as final
summary(fit5)
AIC(fit5)
# check dispersion parameter
phi <- sum(residuals(fit5, type = "pearson")^2)/fit5$df.residual
cat("Dispersion parameter (phi):", phi, "\n") # approximately one, indicating slight over-dispersion

# negative binomial regression model
fit_nb <- glm.nb(outcome ~ agegroup + sex + week + day 
                 + agegroup*sex + day*agegroup,+ offset(log(population)), 
                 data = weekly_counts_16)
summary(fit_nb) # much higher AIC

# prediction
prediction <- predict(fit5, newdata = weekly_counts_16, se.fit = TRUE, 
                      type = 'response')
prediction$fit

new_counts <- weekly_counts_16 |> mutate(outcome_hat = prediction$fit,
                                         se = prediction$se.fit, 
                                         sigma = sd(fit5$resid)) |>
  mutate(excess = outcome-outcome_hat) |>
  group_by(date) 

# plot for expected mortality by age group and sex from 1986 to 2016
new_counts |> group_by(agegroup,sex) |>
  ggplot() + geom_line(aes(x = date, y = outcome_hat, color = sex)) + 
  facet_wrap(~agegroup, scales = "free_y") +
  ggtitle("Expected mortality by age group and sex between 1986 and 2016")

new_counts |>
  group_by(agegroup,sex) |>
  ggplot(aes(x = date, y = 1000*outcome/population, color = sex)) + 
  geom_smooth(method = 'loess') + facet_wrap(~agegroup, scales = "free_y") +
  ggtitle("Death rates (LOESS) by age group and sex between 1986 and 2016") + 
  labs(y = 'Rate')

# Q3
weekly_counts <- puerto_rico_counts |> 
  mutate(date = as.Date(date)) |> 
  filter(year(date) < 2018) |>
  mutate(date = floor_date(date, unit = "week", week_start = 3)) |>
  group_by(date, sex, agegroup) |> 
  summarize(outcome = sum(outcome, na.rm = TRUE), 
            population = mean(population, na.rm = TRUE), 
            n = n(), .groups = "drop") |>
  filter(n == 7) |>
  mutate(age_group_category = case_when(
    agegroup %in% c("5-9", "10-14") ~ "5-14",
    agegroup %in% c("20-24", "25-29") ~ "20-29",
    agegroup %in% c("30-34","35-39","40-44") ~ "30-44",
    TRUE ~ agegroup)) |>
  group_by(date,sex,age_group_category) |>
  summarize(outcome = sum(outcome, na.rm = TRUE), 
            population = sum(population, na.rm = TRUE), 
            .groups = 'drop') |>
  mutate(sex = as.factor(sex), 
         age_group_category = as.factor(age_group_category), 
         year = year(date),
         month = as.factor(month(date)),
         week = as.factor(week(date)),
         day = difftime(date, min(date), units = "day"),
         rate = 1000*outcome/population)

# fitting model with age group as a category, as well as expanding timeline to
# include 2017
new_model <- weekly_counts |> 
  glm(outcome ~ age_group_category + sex + week + day  + age_group_category*sex 
      + day*age_group_category, offset = log(population), data = _, 
      family = poisson())
summary(new_model)

prediction <- predict(new_model, se.fit = TRUE, type = 'response')
prediction$fit

# identifying periods with excess mortality
counts_q3 <- weekly_counts |> mutate(outcome_hat = prediction$fit,
                                     se = prediction$se.fit, 
                                     sigma = sd(new_model$resid)) |>
  mutate(excess = outcome-outcome_hat) |>
  group_by(date) |>
  summarize(excess = sum(excess), outcome = sum(outcome), 
            outcome_hat = sum(outcome_hat), 
            se = sqrt(sum(sigma^2 + se^2))) |>
  filter(outcome > outcome_hat + 3*se) |>
  filter(excess > 100)

# remove dates based on the findings above
dates_to_remove <- c("1998-09-23","1998-09-30","1998-10-07","1998-10-14",
                     "1998-10-21","1998-10-28","2017-09-20", "2017-09-27", 
                     "2017-10-04")

# Q4 
weekly_counts <- puerto_rico_counts |> 
  mutate(date = as.Date(date)) |> 
  mutate(date = floor_date(date, unit = "week", week_start = 3)) |>
  group_by(date, sex, agegroup) |> 
  summarize(outcome = sum(outcome, na.rm = TRUE), 
            population = mean(population, na.rm = TRUE), 
            n = n(), .groups = "drop") |>
  filter(n==7) |>
  mutate(age_group_category = case_when(
    agegroup %in% c("5-9", "10-14") ~ "5-14",
    agegroup %in% c("20-24", "25-29") ~ "20-29",
    agegroup %in% c("30-34","35-39","40-44") ~ "30-44",
    TRUE ~ agegroup)) |>
  group_by(date,sex,age_group_category) |>
  summarize(outcome = sum(outcome, na.rm = TRUE), 
            population = sum(population, na.rm = TRUE), 
            .groups = 'drop') |>
  mutate(sex = as.factor(sex), 
         age_group_category = as.factor(age_group_category), 
         year = year(date),
         month = as.factor(month(date)),
         week = as.factor(week(date)),
         day = difftime(date, min(date), units = "day"),
         rate = 1000*outcome/population) |>
  filter(!(date %in% dates_to_remove))

# final model
final_model <- weekly_counts |> glm(outcome ~ age_group_category + sex + week
                                    + day + age_group_category*sex 
                                    + day*age_group_category, 
                                    offset = log(population), 
                                    data = _, family = poisson())
summary(final_model)

# predicting for 2017 and 2018
weekly_counts_17_18 <- puerto_rico_counts |> 
  mutate(date = as.Date(date)) |> 
  mutate(date = floor_date(date, unit = "week", week_start = 3)) |>
  group_by(date, sex, agegroup) |> 
  summarize(outcome = sum(outcome, na.rm = TRUE), 
            population = mean(population, na.rm = TRUE), 
            n = n(), .groups = "drop") |>
  filter(n==7) |>
  mutate(age_group_category = case_when(
    agegroup %in% c("5-9", "10-14") ~ "5-14",
    agegroup %in% c("20-24", "25-29") ~ "20-29",
    agegroup %in% c("30-34","35-39","40-44") ~ "30-44",
    TRUE ~ agegroup)) |>
  group_by(date,sex,age_group_category) |>
  summarize(outcome = sum(outcome, na.rm = TRUE), 
            population = sum(population, na.rm = TRUE), 
            .groups = 'drop') |>
  mutate(sex = as.factor(sex), age_group_category = as.factor(age_group_category), 
         year = year(date),
         month = as.factor(month(date)),
         week = as.factor(week(date)),
         day = difftime(date, min(date), units = "day"),
         rate = 1000*outcome/population) |>
  filter(year(date) == 2017 | year(date) == 2018)

prediction <- predict(final_model, newdata = weekly_counts_17_18, 
                      se.fit=TRUE, type = 'response')

counts_q4 <- weekly_counts_17_18 |> mutate(outcome_hat = prediction$fit)

# excess mortality with confidence intervals
weekly_counts_17_18 |> mutate(outcome_hat = prediction$fit,
                              se = prediction$se.fit, 
                              sigma = sd(final_model$resid)) |>
  mutate(excess = outcome-outcome_hat) |>
  group_by(date) |> 
  summarize(excess = sum(excess), se = sqrt(sum(sigma^2 + se^2)), 
            .groups = "drop") |>
  mutate(lower_ci = excess - 1.96 * se,
         upper_ci = excess + 1.96 * se) |>
  ggplot(aes(x = date, y = excess)) +
  geom_line() +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci, color = '95% CI')) +
  scale_color_manual(values = c("95% CI" = "red")) +
  labs(title = "Excess Mortality Estimate with Confidence Intervals",
       x = "Week",
       y = "Excess Mortality Estimate",
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 12))

weekly_counts_17_18 |> mutate(outcome_hat = prediction$fit,
                              se = prediction$se.fit, 
                              sigma = sd(final_model$resid)) |>
  mutate(excess = outcome-outcome_hat) |>
  mutate(age_group_category = factor(age_group_category, 
                                     levels = c("0-4", "5-14", "15-19", "20-29", 
                                                "30-44", "45-49", "50-54", 
                                                "55-59", "60-64", "65-69", 
                                                "70-74", "75-79", "80-84", 
                                                "85-Inf"))) |>
  group_by(date, sex,age_group_category) |> 
  mutate(lower_ci = excess - 1.96 * se,
         upper_ci = excess + 1.96 * se) |>
  ggplot(aes(x = date, y = excess, group = sex, color = sex)) +
  geom_line() +
  geom_vline(xintercept = as.Date("2017-09-20"), linetype = "dashed", 
             color = "navyblue", size = 0.5) +
  facet_wrap(~age_group_category, scale = 'free_y', ncol = 3) +
  labs(title = "Excess Mortality Estimate with Confidence Intervals",
       x = "Week",
       y = "Excess Mortality Estimate",
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 12))

# Q5
# reading csv
excess_mort_pdf <- read.csv("../data/excess_mort.csv") # 2015, 2016, 2017

month_lookup <- c("JAN" = 1, "FEB" = 2, "MAR" = 3, "APR" = 4, "MAY" = 5, 
                  "JUN" = 6, "JUL" = 7, "AGO" = 8, "SEP" = 9, "OCT*" = 10, 
                  "NOV" = 11, "DEC" = 12)

# data extracted from the pdf
excess_mort_pdf <- excess_mort_pdf |>
  group_by(year, month) |>
  summarise(count = sum(count, na.rm = TRUE), .groups = "drop") |>
  mutate(month = as.numeric(month_lookup[month])) |>
  arrange(year, month)

str(excess_mort_pdf)
colnames(excess_mort_pdf) 
View(excess_mort_pdf)

# data from the package
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

