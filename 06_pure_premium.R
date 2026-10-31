library(tidyverse)
policy <- readRDS("policy_split.rds")
train <- filter(policy, set == "train")
test  <- filter(policy, set == "test")

# Final frequency model
rest <- "Area_g + vehage_band + VehGas + vehpower_g + VehBrand + offset(log(exposure))"
f_final <- glm(as.formula(paste("n_claims ~ bm_band * age5 +", rest)),
               family = poisson, data = train)

# Severity: flat average vs. the factor model
claims <- readRDS("claims.rds") %>%
  select(IDpol, amt_capped) %>%
  inner_join(select(policy, IDpol, set, bm_band, age5, Area_g, vehage_band,
                    VehGas, vehpower_g, VehBrand), by = "IDpol")
claims_tr <- filter(claims, set == "train")
avg_sev <- mean(claims_tr$amt_capped)
avg_sev

s1 <- glm(amt_capped ~ bm_band + age5 + Area_g + vehage_band + VehGas +
            vehpower_g + VehBrand, family = Gamma(link = "log"), data = claims_tr)

test <- test %>%
  mutate(mu_freq   = predict(f_final, test, type = "response"),  # expected claims
         loss_flat = mu_freq * avg_sev,
         loss_sevm = mu_freq * predict(s1, test, type = "response"))

# Overall actual vs. expected (capped losses)
sum(test$loss_capped) / sum(test$loss_flat)
sum(test$loss_capped) / sum(test$loss_sevm)

# Lift table: predicted vs. actual capped loss cost per policy-year
lift <- test %>%
  mutate(decile = ntile(loss_flat / exposure, 10)) %>%
  group_by(decile) %>%
  summarise(exposure  = sum(exposure),
            actual    = sum(loss_capped) / sum(exposure),
            predicted = sum(loss_flat) / sum(exposure))
lift

ggplot(pivot_longer(lift, c(actual, predicted)), aes(factor(decile), value, fill = name)) +
  geom_col(position = "dodge") +
  labs(title = "Pure premium: actual vs. predicted by decile (holdout)",
       x = "Decile of predicted loss cost (1 = lowest)",
       y = "Capped loss per policy-year", fill = NULL)

# Gini (ordered Lorenz curve): 0 = no separation
gini <- function(loss, expo, pred_rate) {
  o <- order(pred_rate)
  x <- cumsum(expo[o]) / sum(expo)
  y <- cumsum(loss[o]) / sum(loss)
  auc <- sum(diff(c(0, x)) * (y + c(0, head(y, -1))) / 2)
  1 - 2 * auc
}
c(flat_severity  = gini(test$loss_capped, test$exposure, test$loss_flat / test$exposure),
  severity_model = gini(test$loss_capped, test$exposure, test$loss_sevm / test$exposure))

# Is the severity model's Gini difference real? Bootstrap
set.seed(1)
boot <- replicate(200, {
  i <- sample(nrow(test), replace = TRUE)
  g1 <- gini(test$loss_capped[i], test$exposure[i], (test$loss_flat / test$exposure)[i])
  g2 <- gini(test$loss_capped[i], test$exposure[i], (test$loss_sevm / test$exposure)[i])
  g2 - g1
})
quantile(boot, c(0.025, 0.5, 0.975))

p_lift <- ggplot(pivot_longer(lift, c(actual, predicted)), aes(factor(decile), value, fill = name)) +
  geom_col(position = "dodge") +
  labs(title = "Pure premium: actual vs. predicted by decile (holdout)",
       subtitle = "Losses capped at 100,000 per claim",
       x = "Decile of predicted loss cost (1 = lowest)",
       y = "Capped loss per policy-year", fill = NULL)
p_lift
ggsave("lift_chart.png", p_lift, width = 7, height = 5, dpi = 300)