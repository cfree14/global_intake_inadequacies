
# Directories
datadir <- "output"
datadir_shiny <- "shiny_app/data"
gisdir <- "data/world/processed"

# Read world data
world_lg <- readRDS(file=file.path(gisdir, "world_large.Rds"))
world_sm <- readRDS(file=file.path(gisdir, "world_small.Rds")) %>% sf::st_as_sf()
world_centers <- readRDS(file=file.path(gisdir, "world_centroids.Rds"))

# Save world data
saveRDS(world_lg, file.path(datadir_shiny, "world_large.Rds"))
saveRDS(world_sm, file.path(datadir_shiny, "world_small.Rds"))
saveRDS(world_centers, file.path(datadir_shiny, "world_centroids.Rds"))


# Read data
data_orig <- readRDS(file.path(datadir, "2018_subnational_nutrient_intake_inadequacy_estimates_full.Rds"))
data <- data_orig %>%
  # Simplify
  select(continent, iso3, country, nutrient, units, sex, age_range,
         supply_med, ar, ar_source, ar_cv,
         npeople, sev, ndeficient,
         best_dist, g_shape_shift, g_rate_shift, ln_meanlog_shift, ln_sdlog_shift) %>%
  # Rename
  rename(supply50=supply_med, shape=g_shape_shift, rate=g_rate_shift, meanlog=ln_meanlog_shift, sdlog=ln_sdlog_shift) %>%
  # Calculate 95% for intake
  rowwise() %>%
  mutate(supply025=ifelse(best_dist=="gamma", qgamma(p=0.025, shape=shape, rate=rate), qlnorm(p=0.025, meanlog=meanlog, sdlog=sdlog)),
         supply25=ifelse(best_dist=="gamma", qgamma(p=0.25, shape=shape, rate=rate), qlnorm(p=0.25, meanlog=meanlog, sdlog=sdlog)),
         supply75=ifelse(best_dist=="gamma", qgamma(p=0.75, shape=shape, rate=rate), qlnorm(p=0.75, meanlog=meanlog, sdlog=sdlog)),
         supply975=ifelse(best_dist=="gamma", qgamma(p=0.975, shape=shape, rate=rate), qlnorm(p=0.975, meanlog=meanlog, sdlog=sdlog))) %>%
  ungroup() %>%
  # Remove Vitamin D
  filter(nutrient!="Vitamin D")

# Export data
saveRDS(data, file.path(datadir_shiny, "2018_subnational_nutrient_intake_inadequacy_estimates.Rds"))
