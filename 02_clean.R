library(tidyverse)

freq <- read_csv("data/freMTPL2freq.csv", show_col_types = FALSE)
sev  <- read_csv("data/freMTPL2sev.csv",  show_col_types = FALSE)

cap_amt <- 100000   # per-claim cap for the severity model

# 1. Drop orphan claims
sev_ok <- sev %>% filter(IDpol %in% freq$IDpol)

# Claim-level data (used for severity), with policy attributes attached
claims <- sev_ok %>%
  mutate(amt_capped = pmin(ClaimAmount, cap_amt)) %>%
  left_join(select(freq, -ClaimNb, -Exposure), by = "IDpol")

# Policy-level summary of priced claims
by_pol <- sev_ok %>%
  group_by(IDpol) %>%
  summarise(n_claims_raw  = n(),
            loss_uncapped = sum(ClaimAmount),
            loss_capped   = sum(pmin(ClaimAmount, cap_amt)),
            .groups = "drop")

policy <- freq %>%
  rename(ClaimNb_orig = ClaimNb) %>%
  left_join(by_pol, by = "IDpol") %>%
  mutate(across(c(n_claims_raw, loss_uncapped, loss_capped), ~ replace_na(.x, 0)),
         exposure = pmin(Exposure, 1),
         n_claims = pmin(n_claims_raw, 4))

# Checks
nrow(policy)
sum(policy$exposure)
sum(policy$n_claims_raw)                      # should be 26,444
count(policy, n_claims_raw)                   # how many policies have several claims
sum(policy$n_claims_raw) / sum(policy$exposure)   # frequency on priced claims
sum(policy$loss_uncapped) / sum(policy$exposure)  # pure premium per policy-year
excess_load <- sum(policy$loss_uncapped) / sum(policy$loss_capped)
excess_load                                   # expect about 1.2

saveRDS(policy, "policy.rds")
saveRDS(claims, "claims.rds")