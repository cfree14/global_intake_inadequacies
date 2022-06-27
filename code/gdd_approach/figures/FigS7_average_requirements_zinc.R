

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
  # Add AR group
  mutate(ar_group=paste(diet_type, ar_type, sep="-"))



# Derive AR based on HDI
################################################################################

# Function to derive AR based on HDI
hdi <- 0.54
derive_ar_zinc <- function(sex, age, hdi){

  # HDI range
  hdi_range <- c(0.394, 0.957)
  hdi_min <- hdi_range[1]
  hdi_max <- hdi_range[2]

  # Age/sex
  sex_do <- sex
  age_do <- age

  # Loweest AR (refined diet, high HDI)
  ar_lo <- data %>%
    filter(sex==sex_do & age==age_do & diet_type=="Refined (300 mg phytate/d)") %>% pull(ar_mg)

  # Highest AR (unrefined diet, low HDI)
  ar_hi <- data %>%
    filter(sex==sex_do & age==age_do & diet_type=="Unrefined (1200 mg phytate/d)") %>% pull(ar_mg)

  x <- c(hdi_max, hdi_min)
  y <- c(ar_lo, ar_hi)

  # Interpolate AR
  ar <- approx(x, y, xout = hdi)$y

  # Return
  return(ar)

}

# Build HDI dataframe
hdi_ars <- expand.grid(hdi=seq(0.4, 0.9, 0.1),
                       sex=c("Males", "Females"),
                       age=unique(agesex_key$age)) %>%
  arrange(hdi, sex, age) %>%
  rowwise() %>%
  mutate(ar_mg=derive_ar_zinc(sex=sex, age=age, hdi=hdi)) %>%
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
  # Plot HDI
  geom_line(data=hdi_ars, mapping=aes(x=age, y=ar_mg, color=hdi, group=hdi), inherit.aes = F) +
  # Range
  lims(y=c(0,NA)) +
  # Labels
  labs(x="Age range", y="Average requirement (mg)") +
  # Legend
  scale_size_manual(name="Diet type", values=seq(0.2, 1.5, length.out=4)) +
  scale_linetype_manual(name="AR type", values=c("dotted", "solid")) +
  scale_color_gradientn(name="HDI", colors = RColorBrewer::brewer.pal(9, "Spectral")) +
  guides(color = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + my_theme
g

# Export figure
ggsave(g, filename=file.path(plotdir, "FigS7_average_requirements_zinc.png"),
       width=6.5, height=3, units="in", dpi=600)
