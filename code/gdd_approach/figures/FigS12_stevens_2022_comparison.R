

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)

# Directories
plotdir <- "figures/gdd_approach"
outdir <- "output"

# Read data
data_orig <- readRDS(file.path(outdir, "2018_subnational_nutrient_intake_inadequacy_estimates_simple.Rds"))

# Read Stevens data
stevens_orig <- readRDS("data/stevens_etal_2022/stevens_etal_2022_data.Rds")
stevens <- stevens_orig %>%
  # Simplify
  select(group, country, iso3, year, nutrient, pdeficient) %>%
  # Recode nutrient
  mutate(nutrient=recode(nutrient, "Vitamin A"="Vitamin A (RAE)")) %>%
  # Rename/recalculate percent
  rename(pdeficient_stevens=pdeficient) %>%
  mutate(pdeficient_stevens=pdeficient_stevens/100) %>%
  # Select most recent
  filter(!is.na(pdeficient_stevens)) %>%
  mutate(year1=substr(year, 1, 4) %>% as.numeric()) %>%
  arrange(group, country, nutrient, desc(year1)) %>%
  group_by(group, country, nutrient) %>%
  slice(1) %>%
  ungroup()

check <- count(stevens, group, iso3, nutrient)


# Build data
################################################################################

# Build data
unique(data_orig$age_range)
data <- data_orig %>%
  # Mark group
  mutate(group=case_when(sex=="Females" & age_range %in% c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49") ~ "Non-pregnant women aged 15–49 years",
                         age_range %in% c("0-4") ~ "Preschool-aged children aged 6–59 months",
                         T ~ "Other")) %>%
  # Summarize
  group_by(group, nutrient, country, iso3) %>%
  summarize(npeople=sum(npeople, na.rm=T),
            ndeficient=sum(ndeficient, na.rm=T),
            pdeficient=ndeficient/npeople) %>%
  ungroup() %>%
  # Simplify
  filter(group!="Other" & country %in% stevens_orig$country & nutrient %in% stevens$nutrient) %>%
  # Add Stevens
  left_join(stevens %>% select(group, iso3, nutrient, pdeficient_stevens), by=c("group", "iso3", "nutrient")) %>%
  # Simplify
  filter(!is.na(pdeficient_stevens)) %>%
  # Remove vitamin D
  filter(nutrient!="Vitamin D") %>%
  # Fromat group
  mutate(group=recode(group,
                      "Non-pregnant women aged 15–49 years"="Non-pregnant women\naged 15–49 years",
                      "Preschool-aged children aged 6–59 months"="Preschool-aged children\naged 6–59 months"))



# Plot data
################################################################################

# Setup theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=7),
                   strip.text=element_text(size=6),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(data, aes(x=pdeficient, y=pdeficient_stevens, label=iso3)) +
  facet_grid(group ~ nutrient) +
  geom_abline(slope=1, intercept = 0, color="grey70", lwd=0.5) +
  # Points
  geom_text(size=1.5) +
  # Labels
  labs(x="% inadequate\n(present study)", y="% deficient\n(Stevens et al. 2022)") +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "FigS12_stevens_2022_comparison.png"),
       width=6.5, height=3.5, units="in", dpi=600)


