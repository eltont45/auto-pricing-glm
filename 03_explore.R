library(tidyverse)
policy <- readRDS("policy.rds")

# 1. Check ranges before choosing bins
summary(select(policy, VehPower, VehAge, DrivAge, BonusMalus, Density))
count(policy, VehGas)
count(policy, Area)
count(policy, VehBrand)
count(policy, Region, sort = TRUE)

# 2. Group the variables
policy <- policy %>%
  mutate(
    drivage_band = cut(DrivAge, c(17, 20, 25, 30, 40, 50, 60, 70, Inf),
                       labels = c("18-20","21-25","26-30","31-40","41-50","51-60","61-70","71+")),
    vehage_band  = cut(VehAge, c(-1, 0, 2, 5, 10, 15, Inf),
                       labels = c("0","1-2","3-5","6-10","11-15","16+")),
    bm_band      = cut(BonusMalus, c(0, 50, 60, 80, 99, 100, 120, Inf),
                       labels = c("50","51-60","61-80","81-99","100","101-120","121+")),
    vehpower_g   = factor(pmin(VehPower, 9)),
    log_density  = log(Density),
    dens_decile  = factor(ntile(Density, 10))
  )

policy <- policy %>%
  mutate(
    Area_g       = fct_collapse(factor(Area), EF = c("E", "F")),
    drivage_band = fct_relevel(drivage_band, "41-50"),
    vehage_band  = fct_relevel(vehage_band, "6-10"),
    bm_band      = fct_relevel(bm_band, "50"),
    vehpower_g   = fct_relevel(vehpower_g, "6"),
    Area_g       = fct_relevel(Area_g, "C"),
    VehBrand     = fct_relevel(factor(VehBrand), "B1"),
    VehGas       = fct_relevel(factor(VehGas), "Regular"),
    Region       = fct_relevel(factor(Region), "Centre")
  )

saveRDS(policy, "policy_features.rds")

# 3. One-way frequency table for any variable
freq_by <- function(var) {
  policy %>%
    group_by(level = {{ var }}) %>%
    summarise(policies = n(), exposure = sum(exposure),
              claims = sum(n_claims_raw), .groups = "drop") %>%
    mutate(frequency  = claims / exposure,
           relativity = frequency / (sum(claims) / sum(exposure)))
}

freq_by(drivage_band)
freq_by(vehage_band)
freq_by(bm_band)
freq_by(vehpower_g)
freq_by(VehGas)
freq_by(Area)
freq_by(dens_decile)
freq_by(VehBrand) %>% print(n = Inf)
freq_by(Region) %>% print(n = Inf)

# 4. Charts for two key variables
plot_rel <- function(tbl, title) {
  ggplot(tbl, aes(level, relativity, group = 1)) +
    geom_hline(yintercept = 1, linetype = "dashed") +
    geom_line() +
    geom_point(aes(size = exposure)) +
    labs(title = title, x = NULL, size = "Exposure",
         y = "Frequency relativity (1.0 = portfolio average)")
}

age_levels <- c("18-20","21-25","26-30","31-40","41-50","51-60","61-70","71+")
p_age <- plot_rel(freq_by(drivage_band) %>% mutate(level = factor(level, levels = age_levels)),
                  "Claim frequency by driver age")
p_bm  <- plot_rel(freq_by(bm_band), "Claim frequency by bonus-malus")
p_age
p_bm
ggsave("age_frequency.png", p_age, width = 7, height = 5, dpi = 300)
ggsave("bm_frequency.png",  p_bm,  width = 7, height = 5, dpi = 300)

# 5. Why one-way tables can mislead
policy %>%
  group_by(drivage_band) %>%
  summarise(avg_bonus_malus = weighted.mean(BonusMalus, exposure))

policy %>%
  group_by(Area) %>%
  summarise(min_density = min(Density), median_density = median(Density),
            max_density = max(Density))