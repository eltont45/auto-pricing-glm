library(tidyverse)
policy <- readRDS("policy_features.rds")

# Train/test split (saved so later steps use the same one)
set.seed(42)
policy <- policy %>% mutate(set = if_else(runif(n()) < 0.7, "train", "test"))
train <- filter(policy, set == "train")
test  <- filter(policy, set == "test")
nrow(train); nrow(test)
saveRDS(policy, "policy_split.rds")

# Null model (average frequency only) and main model
f0 <- glm(n_claims ~ 1 + offset(log(exposure)), family = poisson, data = train)
f1 <- glm(n_claims ~ bm_band + drivage_band + Area_g + vehage_band +
            VehGas + vehpower_g + VehBrand + offset(log(exposure)),
          family = poisson, data = train)

# Overdispersion check (about 1 means Poisson fits the variance well)
disp <- sum(residuals(f1, type = "pearson")^2) / f1$df.residual
disp

# Relativities with 95% confidence intervals (scaled if overdispersed)
se <- sqrt(diag(vcov(f1)) * max(1, disp))
rel <- tibble(term = names(coef(f1)),
              relativity = exp(coef(f1)),
              lower = exp(coef(f1) - 1.96 * se),
              upper = exp(coef(f1) + 1.96 * se)) %>%
  filter(term != "(Intercept)")
print(rel, n = Inf)

# Does Region add anything beyond the main model?
f2 <- update(f1, . ~ . + Region)
AIC(f1, f2)
anova(f1, f2, test = "Chisq")

# Holdout check: Poisson deviance per policy (lower is better)
pois_dev <- function(y, mu) 2 * sum(ifelse(y > 0, y * log(y / mu), 0) - (y - mu))

test <- test %>%
  mutate(mu0 = predict(f0, test, type = "response"),
         mu1 = predict(f1, test, type = "response"),
         mu2 = predict(f2, test, type = "response"))

c(null        = pois_dev(test$n_claims, test$mu0),
  main        = pois_dev(test$n_claims, test$mu1),
  with_region = pois_dev(test$n_claims, test$mu2)) / nrow(test)

# Calibration by predicted-frequency decile, and overall actual/expected
test %>%
  mutate(decile = ntile(mu1 / exposure, 10)) %>%
  group_by(decile) %>%
  summarise(exposure  = sum(exposure),
            actual    = sum(n_claims) / sum(exposure),
            predicted = sum(mu1) / sum(exposure))

sum(test$n_claims) / sum(test$mu1)

# Scaled region test (accounts for overdispersion)
dev_region <- deviance(f1) - deviance(f2)
pchisq(dev_region / disp, df = f1$df.residual - f2$df.residual, lower.tail = FALSE)

# Age x BM cells: exposure first, then frequency
cells <- policy %>%
  group_by(drivage_band, bm_band) %>%
  summarise(exposure = sum(exposure), claims = sum(n_claims_raw), .groups = "drop") %>%
  mutate(freq = claims / exposure)

cells %>% select(drivage_band, bm_band, exposure) %>%
  pivot_wider(names_from = bm_band, values_from = exposure) %>%
  mutate(across(where(is.numeric), round))

cells %>% select(drivage_band, bm_band, freq) %>%
  pivot_wider(names_from = bm_band, values_from = freq) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

write_csv(rel, "frequency_relativities.csv")

# 1. Print the full cell tables (the last three BM columns were cut off)
cells %>% select(drivage_band, bm_band, exposure) %>%
  pivot_wider(names_from = bm_band, values_from = exposure) %>%
  mutate(across(where(is.numeric), round)) %>% print(width = Inf)
cells %>% select(drivage_band, bm_band, freq) %>%
  pivot_wider(names_from = bm_band, values_from = freq) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>% print(width = Inf)

# 2. Coarse age groups, then rebuild train/test with the new column
policy <- policy %>%
  mutate(age_grp = fct_relevel(cut(DrivAge, c(17, 30, 50, Inf),
                                   labels = c("18-30", "31-50", "51+")), "31-50"))
train <- filter(policy, set == "train")
test  <- filter(policy, set == "test")

# 3. Main effects with coarse age, versus age x BM interaction
f1c <- glm(n_claims ~ bm_band + age_grp + Area_g + vehage_band + VehGas +
             vehpower_g + VehBrand + offset(log(exposure)),
           family = poisson, data = train)
f3  <- glm(n_claims ~ bm_band * age_grp + Area_g + vehage_band + VehGas +
             vehpower_g + VehBrand + offset(log(exposure)),
           family = poisson, data = train)

# Interaction test, scaled for overdispersion
dev_diff <- deviance(f1c) - deviance(f3)
df_diff  <- f1c$df.residual - f3$df.residual
pchisq(dev_diff / disp, df_diff, lower.tail = FALSE)

# 4. Holdout deviance per policy for all three
pois_dev <- function(y, mu) 2 * sum(ifelse(y > 0, y * log(y / mu), 0) - (y - mu))
test <- test %>%
  mutate(mu1  = predict(f1,  test, type = "response"),
         mu1c = predict(f1c, test, type = "response"),
         mu3  = predict(f3,  test, type = "response"))

c(main_8_age_bands = pois_dev(test$n_claims, test$mu1),
  main_3_age_bands = pois_dev(test$n_claims, test$mu1c),
  interaction      = pois_dev(test$n_claims, test$mu3)) / nrow(test)

# 5. Where does the main-effects model miss? Actual / expected by cell
test %>%
  group_by(age_grp, bm_band) %>%
  summarise(exposure = sum(exposure), actual = sum(n_claims),
            exp_main = sum(mu1), exp_inter = sum(mu3), .groups = "drop") %>%
  filter(exposure > 500) %>%
  mutate(AE_main = round(actual / exp_main, 2),
         AE_inter = round(actual / exp_inter, 2)) %>%
  print(n = Inf)

# 1. Print the A/E table with every column
test %>%
  group_by(age_grp, bm_band) %>%
  summarise(exposure = sum(exposure), actual = sum(n_claims),
            exp_main = sum(mu1), exp_inter = sum(mu3), .groups = "drop") %>%
  filter(exposure > 500) %>%
  mutate(AE_main = round(actual / exp_main, 2),
         AE_inter = round(actual / exp_inter, 2)) %>%
  print(n = Inf, width = Inf)

# 2. Candidate models: finer age bands crossed with BM
policy <- policy %>%
  mutate(age5 = fct_relevel(cut(DrivAge, c(17, 25, 30, 40, 50, Inf),
                                labels = c("18-25","26-30","31-40","41-50","51+")), "41-50"))
train <- filter(policy, set == "train")
test  <- filter(policy, set == "test")

rest <- "Area_g + vehage_band + VehGas + vehpower_g + VehBrand + offset(log(exposure))"
f4 <- glm(as.formula(paste("n_claims ~ bm_band * age5 +", rest)),
          family = poisson, data = train)
f5 <- glm(as.formula(paste("n_claims ~ bm_band * drivage_band +", rest)),
          family = poisson, data = train)

models <- list(main_8_bands = f1, inter_3_bands = f3,
               inter_5_bands = f4, inter_8_bands = f5)

dev_tbl <- map_dfr(names(models), function(nm) {
  mu <- predict(models[[nm]], test, type = "response")
  tibble(model = nm,
         n_params = length(coef(models[[nm]])),
         holdout_dev = pois_dev(test$n_claims, mu) / nrow(test),
         actual_to_expected = sum(test$n_claims) / sum(mu))
})
dev_tbl

dev_tbl <- dev_tbl %>%
  mutate(gain_vs_main8_pct = 100 * (holdout_dev[model == "main_8_bands"] - holdout_dev) /
           holdout_dev[model == "main_8_bands"])

as.data.frame(dev_tbl)                               # shows full digits and full model names
sapply(models, function(m) sum(is.na(coef(m))))      # coefficients R couldn't estimate

dev_i <- function(y, mu) 2 * (ifelse(y > 0, y * log(y / mu), 0) - (y - mu))

mu_main <- predict(f1, test, type = "response")
mu_int5 <- predict(f4, test, type = "response")
d <- dev_i(test$n_claims, mu_main) - dev_i(test$n_claims, mu_int5)   # > 0 means interaction is better

c(mean_gain = mean(d), se = sd(d) / sqrt(length(d)), z = mean(d) / (sd(d) / sqrt(length(d))))

saveRDS(policy, "policy_split.rds")
