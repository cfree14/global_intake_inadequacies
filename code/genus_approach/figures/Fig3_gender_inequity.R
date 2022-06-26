

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)

# Directories
outdir <- "output"
gisdir <- "data/world/processed"
plotdir <- "figures"

# Read data
data_orig <- readRDS(file.path(outdir, "2011_subnational_nutrient_intake_inadequacy_estimates_simple.Rds"))


# Build data
################################################################################

# Build data
data <- data_orig %>%
  # Add region
  mutate(region=countrycode::countrycode(iso3, "iso3c", "un.regionsub.name")) %>%
  mutate(region=case_when(iso3=="CHI" ~ "Western Europe",
                          iso3=="XKX" ~ "Southern Europe",
                          T ~ region)) %>%
  # Summarize
  group_by(nutrient, region, iso3, country, sex, age_range) %>%
  summarize(npeople=sum(npeople, na.rm=T),
           ndeficient=sum(ndeficient, na.rm=T)) %>%
  mutate(pdeficient=ndeficient/npeople) %>%
  ungroup() %>%
  # Simplify and spread
  select(nutrient, region, iso3, age_range, sex, pdeficient) %>%
  spread(key="sex", value="pdeficient") %>%
  # Add difference
  mutate(pdiff=Females-Males) %>%
  # Add difference type
  group_by(nutrient, region) %>%
  mutate(pdiff_type=ifelse(median(pdiff, na.rm=T)>=0, "Females", "Males")) %>%
  ungroup() #%>%
  # Order nutrients
  # mutate(nutrient=factor(nutrient, levels=nutrients))

# Nutrient order
nutrient_order <- data %>%
  group_by(nutrient) %>%
  summarize(pdiff_med=median(pdiff, na.rm=T)) %>%
  arrange(desc(pdiff_med))

# Region order
region_order <- data %>%
  filter(nutrient!="Magnesium") %>%
  group_by(region) %>%
  summarize(pdiff_med=median(pdiff, na.rm=T)) %>%
  arrange(pdiff_med)

# Order data
data_ordered <- data %>%
  # Order nutrient
  mutate(nutrient=factor(nutrient, levels=nutrient_order$nutrient)) %>%
  # Order region
  mutate(region=factor(region, levels=region_order$region))



# Plot
################################################################################

# Theme
my_theme <- theme(axis.text=element_text(size=7),
                  axis.title=element_text(size=8),
                  axis.title.y=element_blank(),
                  legend.text=element_text(size=7),
                  legend.title=element_text(size=8),
                  strip.text=element_text(size=8),
                  # Gridlines
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(),
                  axis.line = element_line(colour = "black"),
                  # Legend
                  legend.position = "top",
                  legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(data_ordered, aes(x=pdiff, y=region, fill=pdiff_type)) +
  facet_wrap(~nutrient, ncol=3) +
  geom_boxplot(outlier.size = 0.5, lwd=0.3) +
  # Reference line
  geom_vline(xintercept = 0) +
  # Labels
  labs(x="Δ in % intake inadequacies\nbetween females and males", y="") +
  # Axes
  scale_x_continuous(labels=scales::percent) +
  # Legend
  scale_fill_manual(name="Intake inadequacies higher for:",
                    values=c(RColorBrewer::brewer.pal(9, "Spectral")[2],
                             RColorBrewer::brewer.pal(9, "Spectral")[8])) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(0,-10,-10,-10))
g

# Export data
ggsave(g, filename=file.path(plotdir, "Fig3_gender_inequity.png"),
       width=6.5, height=7.25, units="in", dpi=600)


