
# Directories
datadir <- "output"
datadir_shiny <- "shiny_app/data"

# Read data
data_orig <- readRDS(file.path(datadir, "2011_subnational_nutrient_intake_inadequacy_estimates_full.Rds"))
data <- data_orig %>%
  # Simplify
  select(continent, iso3, country, nutrient, units_short, sex, age_range,
         supply_agesex_med, ar, ar_source, ar_cv,
         npeople, sev, ndeficient,
         best_dist, g_shape_shift, g_rate_shift, ln_meanlog_shift, ln_sdlog_shift) %>%
  # Rename
  rename(supply50=supply_agesex_med, shape=g_shape_shift, rate=g_rate_shift, meanlog=ln_meanlog_shift, sdlog=ln_sdlog_shift) %>%
  # Calculate 95% for intake
  rowwise() %>%
  mutate(supply025=ifelse(best_dist=="gamma", qgamma(p=0.025, shape=shape, rate=rate), qlnorm(p=0.025, meanlog=meanlog, sdlog=sdlog)),
         supply25=ifelse(best_dist=="gamma", qgamma(p=0.25, shape=shape, rate=rate), qlnorm(p=0.25, meanlog=meanlog, sdlog=sdlog)),
         supply75=ifelse(best_dist=="gamma", qgamma(p=0.75, shape=shape, rate=rate), qlnorm(p=0.75, meanlog=meanlog, sdlog=sdlog)),
         supply975=ifelse(best_dist=="gamma", qgamma(p=0.975, shape=shape, rate=rate), qlnorm(p=0.975, meanlog=meanlog, sdlog=sdlog))) %>%
  ungroup()

# Export data
saveRDS(data, file.path(datadir_shiny, "2011_subnational_nutrient_intake_inadequacy_estimates.Rds"))
