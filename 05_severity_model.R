library(tidyverse)
policy <- readRDS("policy_split.rds")

claims <- readRDS("claims.rds") %>%
  select(IDpol, ClaimAmount, amt_capped) %>%
  inner_join(select(policy, IDpol, set, bm_band, age5, Area_g, vehage_band,
                    VehGas, vehpower_g, VehBrand), by = "IDpol")

nrow(claims)                                  # about 26,444 priced claims
sev_train <- filter(claims, set == "train")
sev_test  <- filter(claims, set == "test")
nrow(sev_train); nrow(sev_test)

# The spike: does it vary by risk group?
claims <- claims %>% mutate(spike = round(ClaimAmount) %in% c(1128, 1172, 1204))
mean(claims$spike)
claims %>% group_by(age5) %>% summarise(n = n(), spike_share = mean(spike), mean_amt = mean(amt_capped))
claims %>% group_by(bm_band) %>% summarise(n = n(), spike_share = mean(spike), mean_amt = mean(amt_capped))
claims %>% group_by(Area_g) %>% summarise(n = n(), spike_share = mean(spike), mean_amt = mean(amt_capped))

# Severity models: null vs. same factors as the frequency model
s0 <- glm(amt_capped ~ 1, family = Gamma(link = "log"), data = sev_train)
s1 <- glm(amt_capped ~ bm_band + age5 + Area_g + vehage_band + VehGas +
            vehpower_g + VehBrand,
          family = Gamma(link = "log"), data = sev_train)

summary(s1)$dispersion

# Which factors matter? (F-tests, using the estimated dispersion)
# NOTE: these F-tests use a deviance-based dispersion (~1.4); the Pearson dispersion is ~8.2,
# so they overstate significance. The out-of-sample checks below are what I relied on.
drop1(s1, test = "F")

# Holdout: gamma deviance per claim, plus average severity actual vs. predicted
gamma_dev <- function(y, mu) 2 * sum(-log(y / mu) + (y - mu) / mu)
sev_test <- sev_test %>%
  mutate(mu0 = predict(s0, sev_test, type = "response"),
         mu1 = predict(s1, sev_test, type = "response"))

c(null = gamma_dev(sev_test$amt_capped, sev_test$mu0),
  full = gamma_dev(sev_test$amt_capped, sev_test$mu1)) / nrow(sev_test)
mean(sev_test$amt_capped); mean(sev_test$mu1)

# Calibration by predicted-severity decile
sev_test %>%
  mutate(decile = ntile(mu1, 10)) %>%
  group_by(decile) %>%
  summarise(n = n(), actual = mean(amt_capped), predicted = mean(mu1))

# Paired comparison on the existing holdout (positive = full model better)
gamma_dev_i <- function(y, mu) 2 * (-log(y / mu) + (y - mu) / mu)
d <- gamma_dev_i(sev_test$amt_capped, sev_test$mu0) - gamma_dev_i(sev_test$amt_capped, sev_test$mu1)
c(mean_gain = mean(d), se = sd(d) / sqrt(length(d)))

# 5-fold cross-validation: null vs. full severity model
set.seed(7)
fold <- sample(rep(1:5, length.out = nrow(claims)))
cv_sev <- map_dfr(1:5, function(k) {
  tr <- claims[fold != k, ]; te <- claims[fold == k, ]
  m0 <- glm(amt_capped ~ 1, family = Gamma(link = "log"), data = tr)
  m1 <- glm(amt_capped ~ bm_band + age5 + Area_g + vehage_band + VehGas +
              vehpower_g + VehBrand, family = Gamma(link = "log"), data = tr)
  tibble(fold = k,
         null = gamma_dev(te$amt_capped, rep(exp(coef(m0)[1]), nrow(te))) / nrow(te),
         full = gamma_dev(te$amt_capped, predict(m1, te, type = "response")) / nrow(te))
})
cv_sev
colMeans(select(cv_sev, null, full))