

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

# Read iron bioavailability
iron_key <- readRDS(file="data/gdd/processed/iron_bioavailability.Rds")



# Build data
################################################################################

# Age/sex template
agesex_key <- pop_orig %>%
  select(sex, age) %>%
  unique() %>%
  arrange(sex, age)

# Iron data
iron_ars <- ars_orig %>%
  # Zinc ARs for non-stages
  filter(grepl("Iron", nutrient) & nrv_type=="Average requirement" & !stage %in% c("Pregnancy", "Lactation")) %>%
  # Spread
  select(sex, age_group, nutrient, nrv) %>%
  spread(key=nutrient, value=nrv)

# Build data
data <- agesex_key %>%
  # Add columns for joining zinc ARs
  mutate(sex_iron=ifelse(age %in% c("0-4", "5-9"), "Children", sex),
         age_iron=recode(age,
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
  left_join(iron_ars, by=c("sex_iron"="sex", "age_iron"="age_group")) %>%
  # Gather
  gather(key="abs_level", value="ar_mg", 5:ncol(.)) %>%
  # Format diet type
  mutate(abs_level=recode_factor(abs_level,
                                 "Iron (low absorption)"="Low (5%)",
                                 "Iron (moderate absorption)"="Moderate (10%)",
                                 "Iron (high absorption)"="High (16%)")) %>%
  # Add AR type
  mutate(ar_type=ifelse(!is.na(ar_mg), "Reported", "Derived")) %>%
  # Derive AR type
  # First, find refence value
  group_by(sex) %>%
  mutate(ar_mg_ref=ar_mg[abs_level=="Moderate (10%)" & age=="25-29"]) %>%
  ungroup() %>%
  # Second, derive adjustment factor
  group_by(sex, abs_level) %>%
  mutate(ar_mg_adj=ar_mg[age=="30-34"]-ar_mg_ref) %>%
  ungroup() %>%
  # Third adjust with factor
  group_by(sex) %>%
  mutate(ar_mg=ifelse(ar_type=="Derived", ar_mg[abs_level=="Moderate (10%)"]+ar_mg_adj, ar_mg)) %>%
  ungroup() %>%
  # Add AR group
  mutate(ar_group=paste(abs_level, ar_type, sep="-"))



# Derive AR based on ASF
################################################################################

# Function to derive AR based on bioavailability
iron_abs <- 8; sex <- "Males"; age <- "5-9"
derive_ar_iron <- function(sex, age, iron_abs){

  # iron_abs range
  iron_abs_range <- range(iron_key$bioavailability)
  iron_abs_min <- iron_abs_range[1]
  iron_abs_max <- iron_abs_range[2]

  # Age/sex
  sex_do <- sex
  age_do <- age

  # Lowest AR (high absorption, high iron_abs)
  ar_lo <- data %>%
    filter(sex==sex_do & age==age_do & abs_level=="High (16%)") %>% pull(ar_mg)

  # Highest AR (low absorption, low iron_abs)
  ar_hi <- data %>%
    filter(sex==sex_do & age==age_do & abs_level=="Low (5%)") %>% pull(ar_mg)

  x <- c(iron_abs_max, iron_abs_min)
  y <- c(ar_lo, ar_hi)

  # Interpolate AR
  ar <- approx(x, y, xout = iron_abs)$y

  # Return
  return(ar)

}


# Simple visualization
################################################################################

# Build iron_abs dataframe
iron_abs_ars <- expand.grid(iron_abs=seq(5,15,2.5),
                          sex=c("Males", "Females"),
                          age=unique(agesex_key$age)) %>%
  arrange(iron_abs, sex, age) %>%
  rowwise() %>%
  mutate(ar_mg=derive_ar_iron(sex=sex, age=age, iron_abs=iron_abs)) %>%
  ungroup()



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
g <- ggplot(data, aes(x=age, y=ar_mg, size=abs_level, group=abs_level)) +
  facet_wrap(~sex) +
  # Plot protein
  geom_line(data=iron_abs_ars, mapping=aes(x=age, y=ar_mg, color=iron_abs, group=iron_abs), inherit.aes = F, size=0.4) +
  # Plot ARs
  geom_line() +
  # Range
  lims(y=c(0,NA)) +
  # Labels
  labs(x="Age range", y="Average requirement (mg)") +
  # Legend
  scale_size_manual(name="Absorption level", values=seq(0.4, 1.5, length.out=3)) +
  scale_linetype_manual(name="AR type", values=c("dotted", "solid")) +
  scale_color_gradientn(name="Iron absorption", colors = RColorBrewer::brewer.pal(9, "Reds")[4:9]) +
  guides(color = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + my_theme
g

# # Export figure
# ggsave(g, filename=file.path(plotdir, "FigS8_average_requirements_iron_protein.png"),
#        width=6.5, height=3, units="in", dpi=600)


# Bigger visualization
################################################################################

# Build iron_abs dataframe
iron_abs_avgs <- sort(unique(iron_key$bioavailability))
iron_abs_ars <- expand.grid(iron_abs=iron_abs_avgs,
                           sex=c("Males", "Females"),
                           age=unique(agesex_key$age)) %>%
  arrange(iron_abs, sex, age) %>%
  rowwise() %>%
  mutate(ar_mg=derive_ar_iron(sex=sex, age=age, iron_abs=iron_abs)) %>%
  ungroup()

# Export
saveRDS(protein_ars, file="data/gdd/processed/iron_ars_bioavailability.Rds")


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
g <- ggplot(data, aes(x=age, y=ar_mg, size=abs_level, group=abs_level)) +
  facet_wrap(~sex) +
  # Plot iron_abs
  geom_line(data=iron_abs_ars,
            mapping=aes(x=age, y=ar_mg, color=iron_abs/100, group=iron_abs/100),
            inherit.aes = F, size=0.4) +
  # Plot Allen ARs
  geom_line() +
  # Range
  lims(y=c(0,NA)) +
  # Labels
  labs(x="Age range", y="Average requirement (mg)") +
  # Legend
  scale_size_manual(name="Absorption level\n(Allen et al. 2020)", values=seq(0.4, 1.5, length.out=3)) +
  scale_color_gradientn(name="Absorption (%)\n(based on ASF\nand phytate intake)",
                        colors = RColorBrewer::brewer.pal(9, "Spectral"),
                        breaks=seq(0.06, 0.16, 0.02),
                        labels=scales::percent) +
  guides(color = guide_colorbar(ticks.colour = "black", frame.colour = "black", order=2), size=guide_legend(order=1)) +
  # Theme
  theme_bw() + my_theme
g

# Export figure
ggsave(g, filename=file.path(plotdir, "FigS8_average_requirements_iron_bioavailability.png"),
       width=6.5, height=3, units="in", dpi=600)
