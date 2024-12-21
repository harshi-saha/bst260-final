library(dplyr)
library(lubridate)
library(excessmort)
library(ggplot2)
library(MASS)
library(car)
library(lmtest)
library(sandwich)

str(puerto_rico_counts)
dim(puerto_rico_counts)

# Q5

#setwd("~/Desktop/Harvard/Courses/BST260/Project/bst260-final")
excess_mort_pdf <- read.csv("./data/excess_mort.csv") # 2015, 2016, 2017

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

# Q2

# data processing

weekly_counts <- puerto_rico_counts |> 
  mutate(date = as.Date(date)) |> 
  mutate(date = floor_date(date, unit = "week", week_start = 3)) |> # defined as Wed due to the date when Maria hit
  filter(year(date) < 2017) |> # filter for data before 2017
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
  
weekly_counts_16 <- weekly_counts |> filter(date < as.Date("2017-01-01"))
# model fitting
fit1 <- weekly_counts_16 |> glm(outcome ~ agegroup + sex + day + week, 
                                offset = log(population), 
                                data = _, family = poisson())
summary(fit1)
vif(fit1)

fit2 <- weekly_counts_16 |> glm(outcome ~ agegroup + sex + day, 
                                offset = log(population), 
                                data = _, family = poisson())
summary(fit2)
anova(fit2,fit1, test = "Chisq")

fit3 <- weekly_counts_16 |> glm(outcome ~ agegroup + sex + week, 
                                offset = log(population), 
                                data = _, family = poisson())
summary(fit3)
anova(fit3,fit1, test = "Chisq")

fit4 <- weekly_counts_16 |> glm(outcome ~ agegroup + sex + week + day 
                                + agegroup*sex, 
                                offset = log(population), 
                                data = _, family = poisson())
summary(fit4)
anova(fit4,fit1, test = "Chisq")

fit5 <- weekly_counts_16 |> glm(outcome ~ agegroup+ sex + week + day 
                                + agegroup*sex + sex*week, 
                                offset = log(population), 
                                data = _, family = poisson())
summary(fit5)
anova(fit5,fit1, test = "Chisq")



# final model
fit7 <- weekly_counts_16 |> glm(outcome ~ agegroup + sex + week + day  + agegroup*sex + day*agegroup, 
                                offset = log(population), 
                                data = _, family = poisson())
summary(fit7)
anova(fit7,fit4, test = "Chisq")
# check dispersion parameter
phi <- sum(residuals(fit7, type = "pearson")^2) / fit7$df.residual
cat("Dispersion parameter (phi):", phi, "\n")

fit_nb <- glm.nb(outcome ~ agegroup + sex + week + day 
                 + agegroup*sex + day*agegroup,+ offset(log(population)), 
                 data = weekly_counts_16)
summary(fit_nb)

x <- summary(fit7)
x <- as.data.frame(x$coefficients)
x$exp <- exp(x[,'Estimate'])
x[order(x$exp),]

prediction
prediction <- predict(fit7, newdata = weekly_counts_16, se.fit=TRUE, type = 'response')
prediction$fit

new_counts <- weekly_counts_16 |> mutate(outcome_hat = prediction$fit,
                           se = prediction$se.fit, 
                           sigma = sd(fit7$resid)) |>
  mutate(excess = outcome-outcome_hat) |>
  group_by(date) 

new_counts |>
  group_by(agegroup,sex) |>
  ggplot() + geom_line(aes(x = date, y = outcome_hat, color = sex)) + facet_wrap(~agegroup, scales = "free_y") +
  ggtitle("Expected mortality by age group and sex from 1986 - 2016")

new_counts |>
  group_by(agegroup,sex) |>
  ggplot(aes(x = date, y = 1000*outcome/population, color = sex)) + geom_smooth(method = 'loess') + facet_wrap(~agegroup, scales = "free_y") +
  ggtitle("Death rates (LOESS) by age group and sex from 1986 - 2016") + labs(y = 'Rate')


######### Q3:

weekly_counts <- puerto_rico_counts |> 
  mutate(date = as.Date(date)) |> 
  filter(year(date)<2018) |>
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
  summarize(outcome = sum(outcome, na.rm = TRUE), population = sum(population, na.rm = TRUE), .groups = 'drop') |>
  mutate(sex = as.factor(sex), age_group_category = as.factor(age_group_category), 
         year = year(date),
         month = as.factor(month(date)),
         week = as.factor(week(date)),
         day = difftime(date, min(date), units = "day"),
         rate = 1000*outcome/population)

new_model <- weekly_counts |> glm(outcome ~ age_group_category + sex + week + day  + age_group_category*sex 
                                  + day*age_group_category, 
                                offset = log(population), 
                                data = _, family = poisson())
summary(new_model)

prediction <- predict(new_model, se.fit=TRUE, type = 'response')
prediction$fit

counts_q3 <- weekly_counts |> mutate(outcome_hat = prediction$fit,
                                         se = prediction$se.fit, 
                                         sigma = sd(new_model$resid)) |>
  mutate(excess = outcome-outcome_hat) |>
  group_by(date)|>
  summarize(excess = sum(excess), outcome = sum(outcome), outcome_hat = sum(outcome_hat), se = sqrt(sum(sigma^2 + se^2)))  |>
  filter(outcome > outcome_hat + 3*se) |>
  filter(excess > 100)
  
dates_to_remove <- c("1998-09-23","1998-09-30","1998-10-07","1998-10-14","1998-10-21","1998-10-28","2017-09-20", "2017-09-27", "2017-10-04")


## Q4: 
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
  summarize(outcome = sum(outcome, na.rm = TRUE), population = sum(population, na.rm = TRUE), .groups = 'drop') |>
  mutate(sex = as.factor(sex), age_group_category = as.factor(age_group_category), 
         year = year(date),
         month = as.factor(month(date)),
         week = as.factor(week(date)),
         day = difftime(date, min(date), units = "day"),
         rate = 1000*outcome/population) |>
  filter(!(date %in% dates_to_remove))

final_model <- weekly_counts |> glm(outcome ~ age_group_category + sex + week + day  + age_group_category*sex 
                                  + day*age_group_category, 
                                  offset = log(population), 
                                  data = _, family = poisson())
summary(final_model)

##Predict on 17-18

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
  summarize(outcome = sum(outcome, na.rm = TRUE), population = sum(population, na.rm = TRUE), .groups = 'drop') |>
  mutate(sex = as.factor(sex), age_group_category = as.factor(age_group_category), 
         year = year(date),
         month = as.factor(month(date)),
         week = as.factor(week(date)),
         day = difftime(date, min(date), units = "day"),
         rate = 1000*outcome/population) |>
  filter(year(date)==2017 | year(date)==2018)

prediction <- predict(final_model, newdata = weekly_counts_17_18, se.fit=TRUE, type = 'response')
prediction$fit
counts_q4 <- weekly_counts_17_18 |>
  mutate(outcome_hat = prediction$fit)

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
                                     levels = c("0-4", "5-14", "15-19", "20-29", "30-44", 
                                     "45-49", "50-54", "55-59", "60-64", "65-69", 
                                     "70-74", "75-79", "80-84", "85-Inf"))) |>
  group_by(date, sex,age_group_category) |> 
  mutate(lower_ci = excess - 1.96 * se,
         upper_ci = excess + 1.96 * se) |>
  ggplot(aes(x = date, y = excess, group = sex, color = sex)) +
  geom_line() +
  geom_vline(xintercept = as.Date("2017-09-20"), linetype = "dashed", color = "navyblue", size = 0.5) +
  facet_wrap(~age_group_category, scale= 'free_y', ncol = 3) +
  labs(title = "Excess Mortality Estimate with Confidence Intervals",
       x = "Week",
       y = "Excess Mortality Estimate",
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 12))


#################
