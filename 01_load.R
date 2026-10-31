library(tidyverse)

# Option A: CSVs
freq <- read_csv("data/freMTPL2freq.csv", show_col_types = FALSE)
sev  <- read_csv("data/freMTPL2sev.csv",  show_col_types = FALSE)

dim(freq); dim(sev)
glimpse(freq)
glimpse(sev)

n_distinct(freq$IDpol); sum(duplicated(freq$IDpol))
count(freq, ClaimNb)
summary(freq$Exposure)
mean(freq$Exposure > 1)                 # share with exposure over one year
sum(freq$ClaimNb) / sum(freq$Exposure)  # overall claim frequency per policy-year
sum(!sev$IDpol %in% freq$IDpol)         # claims with no matching policy
summary(sev$ClaimAmount)

# 1. Claim count vs severity records, per policy
sev_by_pol <- sev %>%
  group_by(IDpol) %>%
  summarise(n_sev = n(), total_amt = sum(ClaimAmount), .groups = "drop")

chk <- freq %>%
  left_join(sev_by_pol, by = "IDpol") %>%
  mutate(n_sev = replace_na(n_sev, 0L))

chk %>%
  count(status = case_when(ClaimNb == n_sev ~ "match",
                           ClaimNb >  n_sev ~ "freq count higher than severity records",
                           TRUE             ~ "severity records higher than freq count"))
sum(chk$ClaimNb); sum(chk$n_sev)

# 2. Orphan claims (no matching policy)
sev %>%
  filter(!IDpol %in% freq$IDpol) %>%
  summarise(n = n(), total = sum(ClaimAmount))

# 3. Large losses: how much cost sits above each threshold
sev_ok <- sev %>% filter(IDpol %in% freq$IDpol)

map_dfr(c(10000, 25000, 50000, 100000, 250000, 1000000), ~ tibble(
  threshold = .x,
  n_over    = sum(sev_ok$ClaimAmount > .x),
  share_of_cost_above = sum(pmax(sev_ok$ClaimAmount - .x, 0)) / sum(sev_ok$ClaimAmount)
))

# 4. The spike: the most common exact claim amounts
sev_ok %>% count(ClaimAmount, sort = TRUE) %>% head(10)

ggplot(sev_ok, aes(ClaimAmount)) +
  geom_histogram(bins = 80) +
  scale_x_log10()