library(tidyverse)
policy <- readRDS("policy_split.rds")
claims <- readRDS("claims.rds")

cap_amt <- 100000
full_cred <- 1082
# --- Final models on all data ---
rest <- "Area_g + vehage_band + VehGas + vehpower_g + VehBrand + offset(log(exposure))"
f_int  <- glm(as.formula(paste("n_claims ~ bm_band * age5 +", rest)),
              family = poisson, data = policy)
f_main <- glm(as.formula(paste("n_claims ~ bm_band + age5 +", rest)),
              family = poisson, data = policy)

avg_sev     <- mean(claims$amt_capped)                       # flat severity, capped at 100k
excess_load <- sum(claims$ClaimAmount) / sum(claims$amt_capped)
avg_sev; excess_load

# --- Base risk and grid of age x BM cells (other factors at base levels) ---
base <- tibble(
  Area_g      = factor("C",       levels = levels(policy$Area_g)),
  vehage_band = factor("6-10",    levels = levels(policy$vehage_band)),
  VehGas      = factor("Regular", levels = levels(policy$VehGas)),
  vehpower_g  = factor("6",       levels = levels(policy$vehpower_g)),
  VehBrand    = factor("B1",      levels = levels(policy$VehBrand)))

grid <- expand_grid(bm_band = levels(policy$bm_band), age5 = levels(policy$age5)) %>%
  mutate(bm_band = factor(bm_band, levels = levels(policy$bm_band)),
         age5    = factor(age5,    levels = levels(policy$age5)),
         Area_g = base$Area_g, vehage_band = base$vehage_band, VehGas = base$VehGas,
         vehpower_g = base$vehpower_g, VehBrand = base$VehBrand, exposure = 1)

base_freq <- unname(exp(coef(f_int)["(Intercept)"]))
base_freq

grid <- grid %>%
  mutate(rel_int  = as.numeric(exp(predict(f_int,  grid, type = "link"))) / base_freq,
         rel_main = as.numeric(exp(predict(f_main, grid, type = "link"))) /
           unname(exp(coef(f_main)["(Intercept)"])))

# --- Credibility blend (square-root rule, full credibility at 1,082 claims) ---
cell <- policy %>%
  group_by(bm_band, age5) %>%
  summarise(claims = sum(n_claims_raw), cell_exposure = sum(exposure), .groups = "drop")

grid <- grid %>%
  left_join(cell, by = c("bm_band", "age5")) %>%
  mutate(Z = pmin(1, sqrt(claims / full_cred)),
         rel_blend = Z * rel_int + (1 - Z) * rel_main)

# Rebalance so the blended grid has the same exposure-weighted average as the interaction model
# (approximate: it ignores the other rating factors)
off_bal <- sum(grid$cell_exposure * grid$rel_int) / sum(grid$cell_exposure * grid$rel_blend)
off_bal
grid <- grid %>% mutate(rel_final = rel_blend * off_bal)

# The grid, by age (rows) and bonus-malus (columns)
grid %>% select(age5, bm_band, rel_final) %>%
  pivot_wider(names_from = bm_band, values_from = rel_final) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2))) %>% print(width = Inf)
grid %>% select(age5, bm_band, claims) %>%
  pivot_wider(names_from = bm_band, values_from = claims) %>% print(width = Inf)
grid %>% select(age5, bm_band, Z) %>%
  pivot_wider(names_from = bm_band, values_from = Z) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2))) %>% print(width = Inf)

# --- Other factors' relativities ---
rel_other <- exp(coef(f_int))
tibble(term = names(rel_other), relativity = round(rel_other, 3)) %>%
  filter(!str_detect(term, "bm_band|age5|Intercept")) %>% print(n = Inf)

# --- Base premium and quotes ---
base_loss_cost <- base_freq * avg_sev * excess_load   # per policy-year, base risk
plr <- 0.65    # permissible loss ratio: 1 - 30% expenses - 5% profit (ILLUSTRATIVE)
base_premium <- base_loss_cost / plr
c(base_loss_cost = base_loss_cost, base_premium = base_premium)

get_rel <- function(prefix, level) {
  nm <- paste0(prefix, level)
  if (nm %in% names(rel_other)) rel_other[[nm]] else 1
}

quote_premium <- function(age_lvl, bm_lvl, area, vehage, gas, power, brand) {
  r_cell <- grid$rel_smooth[grid$age5 == age_lvl & grid$bm_band == bm_lvl]
  r_oth  <- get_rel("Area_g", area) * get_rel("vehage_band", vehage) *
    get_rel("VehGas", gas) * get_rel("vehpower_g", power) * get_rel("VehBrand", brand)
  round(base_premium * r_cell * r_oth)
}
quote_premium("51+",   "50",  "A",  "6-10", "Regular", "6", "B1")   # experienced, claim-free, rural
quote_premium("18-25", "100", "EF", "3-5",  "Diesel",  "9", "B12")  # new driver, urban, powerful car

# --- Sanity check: portfolio average loss cost (should land near 167) ---
sum(fitted(f_int)) / sum(policy$exposure) * avg_sev * excess_load

write_csv(grid %>% select(age5, bm_band, claims, Z, rel_int, rel_main, rel_final),
          "age_bm_relativities.csv")

# --- 1. Enforce non-decreasing relativities across BM within each age band ---
pav <- function(y, w) {
  vals <- y; wts <- w; sizes <- rep(1L, length(y)); i <- 1
  while (i < length(vals)) {
    if (vals[i] > vals[i + 1]) {
      newv  <- (vals[i] * wts[i] + vals[i + 1] * wts[i + 1]) / (wts[i] + wts[i + 1])
      vals  <- c(vals[seq_len(i - 1)],  newv,                 vals[-seq_len(i + 1)])
      wts   <- c(wts[seq_len(i - 1)],   wts[i] + wts[i + 1],  wts[-seq_len(i + 1)])
      sizes <- c(sizes[seq_len(i - 1)], sizes[i] + sizes[i + 1], sizes[-seq_len(i + 1)])
      i <- max(i - 1, 1)
    } else i <- i + 1
  }
  rep(vals, sizes)
}

grid <- grid %>%
  arrange(age5, bm_band) %>%
  group_by(age5) %>%
  mutate(rel_smooth = pav(rel_final, cell_exposure)) %>%
  ungroup()

grid %>%
  mutate(age5 = fct_relevel(age5, "18-25", "26-30", "31-40", "41-50", "51+")) %>%
  select(age5, bm_band, rel_smooth) %>%
  pivot_wider(names_from = bm_band, values_from = rel_smooth) %>%
  arrange(age5) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2))) %>% print(width = Inf)

# --- 2. Rate balance: apply the final plan to every policy ---
policy_base <- policy %>%
  mutate(age5 = factor("41-50", levels = levels(age5)),
         bm_band = factor("50", levels = levels(bm_band)),
         exposure = 1)
policy$other_rel <- as.numeric(exp(predict(f_int, policy_base, type = "link"))) / base_freq

plan <- policy %>%
  left_join(select(grid, age5, bm_band, rel_smooth), by = c("age5", "bm_band")) %>%
  mutate(plan_loss = base_freq * rel_smooth * other_rel * avg_sev * excess_load * exposure)

balance <- sum(plan$plan_loss) / sum(plan$loss_uncapped)
balance            # plan-expected / actual total losses (in-sample); want close to 1

# --- 3. Spread of premiums across the book ---
quantile(plan$plan_loss / plan$exposure / plr, c(0.01, 0.10, 0.50, 0.90, 0.99))

# --- 4. In-sample lift of the final plan (a reasonableness check, not validation) ---
plan %>%
  mutate(decile = ntile(plan_loss / exposure, 10)) %>%
  group_by(decile) %>%
  summarise(exposure = sum(exposure),
            actual   = sum(loss_uncapped) / sum(exposure),
            plan     = sum(plan_loss) / sum(exposure))

# Where does the excess layer land, and does average severity rise with predicted risk?
plan <- plan %>% mutate(decile = ntile(plan_loss / exposure, 10))

plan %>%
  group_by(decile) %>%
  summarise(exposure       = sum(exposure),
            claims         = sum(n_claims_raw),
            avg_sev_capped = sum(loss_capped) / sum(n_claims_raw),
            actual_capped  = sum(loss_capped) / sum(exposure),
            actual_excess  = (sum(loss_uncapped) - sum(loss_capped)) / sum(exposure),
            plan_capped    = sum(plan_loss) / excess_load / sum(exposure),
            plan_excess    = sum(plan_loss) * (1 - 1 / excess_load) / sum(exposure))

dec_map <- plan %>% select(IDpol, decile)
claims %>%
  left_join(dec_map, by = "IDpol") %>%
  group_by(decile) %>%
  summarise(n_claims     = n(),
            n_over_100k  = sum(ClaimAmount > cap_amt),
            n_over_1m    = sum(ClaimAmount > 1000000),
            excess_total = sum(pmax(ClaimAmount - cap_amt, 0)))

dec_tbl <- plan %>%
  group_by(decile) %>%
  summarise(exposure      = sum(exposure),
            actual_capped = sum(loss_capped) / sum(exposure),
            actual_excess = (sum(loss_uncapped) - sum(loss_capped)) / sum(exposure),
            plan_capped   = sum(plan_loss) / excess_load / sum(exposure),
            plan_excess   = sum(plan_loss) * (1 - 1 / excess_load) / sum(exposure))

dec_tbl %>%
  mutate(AE_capped = actual_capped / plan_capped,
         AE_excess = actual_excess / plan_excess) %>%
  print(width = Inf)

claims %>% filter(ClaimAmount > 1e6)    # the three largest claims

# How concentrated is the excess layer?
exc <- claims %>%
  mutate(excess = pmax(ClaimAmount - cap_amt, 0)) %>%
  arrange(desc(excess))
c(total_excess = sum(exc$excess),
  n_claims_with_excess = sum(exc$excess > 0),
  top1_share  = sum(head(exc$excess, 1))  / sum(exc$excess),
  top3_share  = sum(head(exc$excess, 3))  / sum(exc$excess),
  top10_share = sum(head(exc$excess, 10)) / sum(exc$excess))

# Sensitivity of the base premium to the excess load
sorted <- sort(claims$ClaimAmount)
sens <- tibble(
  scenario = c("All claims (selected)", "Without largest claim", "Without 3 claims over 1M"),
  mean_claim_cost = c(mean(sorted),
                      mean(sorted[-length(sorted)]),
                      mean(sorted[sorted <= 1e6]))) %>%
  mutate(implied_load  = mean_claim_cost / avg_sev,   # approximate: avg_sev is for the full set
         base_premium  = base_freq * mean_claim_cost / plr,
         pct_vs_selected = round(100 * (base_premium / first(base_premium) - 1), 1))
sens

write_csv(grid %>% select(age5, bm_band, claims, Z, rel_int, rel_main, rel_final, rel_smooth),
          "age_bm_relativities.csv")
write_csv(tibble(term = names(rel_other), relativity = round(rel_other, 3)) %>%
            filter(!str_detect(term, "bm_band|age5|Intercept")),
          "other_relativities.csv")
write_csv(sens, "excess_load_sensitivity.csv")
write_csv(dec_tbl, "excess_layer_by_decile.csv")