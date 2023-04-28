

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)

# Directories
outdir <- "output"
popdir <- "data/population"
plotdir <- "figures/gdd_approach"

# Get ARs
ars_orig <- nutriR::nrvs

# Read population data (for sex/age template)
pop_orig <- readRDS(file.path(popdir, "WB_1960_2020_population_size_by_country_agesex.Rds"))

# Read country key for HDI range
country_key <- read.csv(file.path(outdir, "country_key_gdd.csv"), as.is=T)

# Read
wessels <- readRDS(file.path("data/wessells_brown/wessells_brown_2012_zinc_absorption.Rds"))


# Build data
################################################################################

# Age/sex template
agesex_key <- pop_orig %>%
  select(sex, age) %>%
  unique() %>%
  arrange(sex, age)

# Zinc data
zinc_ars <- ars_orig %>%
  # Zinc ARs for non-stages
  filter(grepl("Zinc", nutrient) & nrv_type=="Average requirement" & !stage %in% c("Pregnancy", "Lactation")) %>%
  # Spread
  select(sex, age_group, nutrient, nrv) %>%
  spread(key=nutrient, value=nrv)

# Build data
data <- agesex_key %>%
  # Add columns for joining zinc ARs
  mutate(sex_zinc=ifelse(age %in% c("0-4", "5-9"), "Children", sex),
         age_zinc=recode(age,
                         "0-4"="1-3 y",
                         "5-9"="7-10 y",
                         "10-14"="11-14 y",
                         "15-19"="15-17 y",
                         "20-24"="18-24 y",
                         "25-29"="25-50 y",
                         "30-34"="25-50 y",
                         "35-39"="25-50 y",
                         "40-44"="25-50 y",
                         "45-49"="25-50 y",
                         "50-54"="51-70 y",
                         "55-59"="51-70 y",
                         "60-64"="51-70 y",
                         "65-69"="51-70 y",
                         "70-74"=">70 y",
                         "75-79"=">70 y",
                         "80+"=">70 y")) %>%
  # Add NRV
  left_join(zinc_ars, by=c("sex_zinc"="sex", "age_zinc"="age_group")) %>%
  # Gather
  gather(key="diet_type", value="ar_mg", 5:ncol(.)) %>%
  # Format diet type
  mutate(diet_type=recode_factor(diet_type,
                                 "Zinc (refined diet)"="Refined (300 mg phytate/d)",
                                 "Zinc (semi-refined diet)"="Semi-refined (600 mg phytate/d)",
                                 "Zinc (semi-unrefined diet)"="Semi-unrefined (900 mg phytate/d)",
                                 "Zinc (unrefined diet)"="Unrefined (1200 mg phytate/d)")) %>%
  # Add AR type
  mutate(ar_type=ifelse(!is.na(ar_mg), "Reported", "Derived")) %>%
  # Derive AR type
  # First, find refence value
  group_by(sex) %>%
  mutate(ar_mg_ref=ar_mg[diet_type=="Semi-unrefined (900 mg phytate/d)" & age=="30-34"]) %>%
  ungroup() %>%
  # Second, derive adjustment factor
  group_by(sex, diet_type) %>%
  mutate(ar_mg_adj=ar_mg[age=="30-34"]-ar_mg_ref) %>%
  ungroup() %>%
  # Third adjust with factor
  group_by(sex) %>%
  mutate(ar_mg=ifelse(ar_type=="Derived", ar_mg[diet_type=="Semi-unrefined (900 mg phytate/d)"]+ar_mg_adj, ar_mg)) %>%
  ungroup() %>%
  # Add phytate mg
  mutate(phytate_mg=recode(diet_type %>% as.character(),
                           "Refined (300 mg phytate/d)"="300",
                           "Semi-refined (600 mg phytate/d)"="600",
                           "Semi-unrefined (900 mg phytate/d)"="900",
                           "Unrefined (1200 mg phytate/d)"="1200") %>% as.numeric())





# For if you decide to expand up to 4500 mg/day
################################################################################

# Confirm that ARs scale linearly with
ggplot(data, aes(x=phytate_mg, y=ar_mg, linetype=sex, color=age_zinc)) +
  geom_line() +
  geom_point() +
  # Labels
  labs(x="Phytate intake (mg)", y="Average requirement (mg)") +
  scale_x_continuous(lim=c(0, NA), breaks=seq(0, 1500, 300)) +
  # Theme
  theme_bw()

# Loop through sex-ages and project to 4500 mg/day
key <- data %>%
  select(sex, age) %>% unique
x <- 1
ars4500mg <- purrr::map_df(1:nrow(key), function(x){

  # Get data
  sex_do <- key$sex[x]
  age_do <- key$age[x]
  sdata <- data %>%
    filter(sex==sex_do & age==age_do)

  # Fit model
  lmfit <- lm(ar_mg ~ phytate_mg, sdata)

  # Predict
  ar_pred <- predict(lmfit, newdata = tibble(phytate_mg=4500))

  # Record results
  df <- tibble(sex=sex_do,
               age=age_do,
               phytate_mg=4500,
               ar_mg=ar_pred)

})

# Expand
# This is for if you decide to expand up to 4500
ars4500mg_exp <- ars4500mg %>%
    mutate(diet_type="Ultra-unrefined (4500 mg phytate/d)",
           ar_type="Derived")

# Build data
data_exp <- bind_rows(data %>% select(diet_type, phytate_mg, sex, age, ar_type, ar_mg),
                  ars4500mg_exp) %>%
  mutate(diet_type=factor(diet_type,
                          levels=c("Refined (300 mg phytate/d)",
                                   "Semi-refined (600 mg phytate/d)",
                                   "Semi-unrefined (900 mg phytate/d)",
                                   "Unrefined (1200 mg phytate/d)",
                                   "Ultra-unrefined (4500 mg phytate/d)")))



# Derive AR based on estimated absorption fraction
################################################################################

# Function to derive AR based on HDI
phytate <- 3000; sex <- "Females"; age <- "15-19"
derive_ar_zinc <- function(sex, age, phytate){

  # Phytate range
  phytate_range <- range(wessels$phytate_mg)
  phytate_min <- phytate_range[1]
  phytate_max <- phytate_range[2]

  # Age/sex
  sex_do <- sex
  age_do <- age

  # Loweest AR (refined diet, high HDI)
  ar_lo <- data %>%
    filter(sex==sex_do & age==age_do & diet_type=="Refined (300 mg phytate/d)") %>% pull(ar_mg)

  # Highest AR (unrefined diet, low HDI)
  ar_hi <- data %>%
    filter(sex==sex_do & age==age_do & diet_type=="Unrefined (1200 mg phytate/d)") %>% pull(ar_mg)

  x <- c(phytate_min, phytate_max)
  y <- c(ar_lo, ar_hi)

  # Interpolate AR
  ar <- approx(x, y, xout = phytate)$y

  # Return
  return(ar)

}

# Build phytate dataframe
range(wessels$phytate_mg)
phytate_ars <- expand.grid(phytate=seq(1000, 4000, 500),
                       sex=c("Males", "Females"),
                       age=unique(agesex_key$age)) %>%
  arrange(phytate, sex, age) %>%
  rowwise() %>%
  mutate(ar_mg=derive_ar_zinc(sex=sex, age=age, phytate=phytate)) %>%
  ungroup()


# Build data
################################################################################

# Theme
my_theme <- theme(axis.text=element_text(size=6),
                  axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                  axis.title=element_text(size=8),
                  legend.text=element_text(size=6),
                  legend.title=element_text(size=8),
                  strip.text=element_text(size=8),
                  plot.title=element_text(size=10),
                  # Gridlines
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(),
                  axis.line = element_line(colour = "black"),
                  # Legend
                  legend.key.size = unit(0.3, "cm"),
                  legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(data, aes(x=age, y=ar_mg, size=diet_type, group=diet_type)) +
  facet_wrap(~sex) +
  geom_line() +
  # Plot phytate
  geom_line(data=phytate_ars, mapping=aes(x=age, y=ar_mg, color=phytate, group=phytate), inherit.aes = F) +
  # Range
  lims(y=c(0,NA)) +
  # Labels
  labs(x="Age range", y="Average requirement (mg)") +
  # Legend
  scale_size_manual(name="Diet type", values=seq(0.2, 1.5, length.out=5)) +
  scale_linetype_manual(name="AR type", values=c("dotted", "solid")) +
  scale_color_gradientn(name="Phytate intake (mg/d)\n(Wessells & Brown 2012)", colors = RColorBrewer::brewer.pal(9, "Spectral")) +
  guides(color = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + my_theme
g

# Export figure
ggsave(g, filename=file.path(plotdir, "FigS7_average_requirements_zinc_phytate.png"),
       width=6.5, height=3, units="in", dpi=600)


# Build ARS for paper
################################################################################

# Build phytate dataframe
phytate_all <- unique(wessels$phytate_mg) %>% sort()
phytate_ars <- expand.grid(phytate=phytate_all,
                        sex=c("Males", "Females"),
                        age=unique(agesex_key$age)) %>%
  arrange(phytate, sex, age) %>%
  rowwise() %>%
  mutate(ar_mg=derive_ar_zinc(sex=sex, age=age, phytate=phytate)) %>%
  ungroup()

# Export
saveRDS(phytate_ars, file="data/wessells_brown/zinc_ars_phytate.Rds")

# Theme
my_theme <- theme(axis.text=element_text(size=6),
                  axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                  axis.title=element_text(size=8),
                  legend.text=element_text(size=6),
                  legend.title=element_text(size=8),
                  strip.text=element_text(size=8),
                  plot.title=element_text(size=10),
                  # Gridlines
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(),
                  axis.line = element_line(colour = "black"),
                  # Legend
                  legend.key.size = unit(0.3, "cm"),
                  legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(data, aes(x=age, y=ar_mg, size=diet_type, group=diet_type)) +
  facet_wrap(~sex) +
  # Plot phytate
  geom_line(data=phytate_ars, mapping=aes(x=age, y=ar_mg, color=phytate, group=phytate), inherit.aes = F) +
  # Plot AR lines
  geom_line() +
  # Range
  lims(y=c(0,NA)) +
  # Labels
  labs(x="Age range", y="Average requirement (mg)") +
  # Legend
  scale_size_manual(name="Diet type\n(Allen et al. 2020)", values=seq(0.2, 1.5, length.out=5)) +
  scale_color_gradientn(name="Phytate intake (mg)\n(Wessells & Brown 2012)", colors = RColorBrewer::brewer.pal(9, "Spectral")) +
  guides(color = guide_colorbar(ticks.colour = "black", frame.colour = "black", order=2), size=guide_legend(order=1)) +
  # Theme
  theme_bw() + my_theme
g

# Export figure
ggsave(g, filename=file.path(plotdir, "FigS7_average_requirements_zinc_phytate_all.png"),
       width=6.5, height=3, units="in", dpi=600)

