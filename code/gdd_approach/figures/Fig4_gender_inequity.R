

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
plotdir <- "figures/gdd_approach"
tabledir <- "tables/gdd_approach"

# Read data
data_orig <- readRDS(file.path(outdir, "2018_subnational_nutrient_intake_inadequacy_estimates_simple.Rds"))


# Build data
################################################################################

# Nutrient order (when by SEVs)
# nutrient_key <- read.csv(file=file.path(tabledir, "TableS1_global_inadequacies.csv"), as.is=T)  %>%
#   arrange(desc(pdeficient))

# Build data
data <- data_orig %>%
  # Remove Vitamin D
  filter(nutrient!="Vitamin D") %>%
  # Remove GDD borrowed countris
  filter(gdd_type=="Reported") %>%
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
  ungroup() %>%
  # Add short region
  mutate(region_short=recode(region,
                             "East Asia & Pacific"="E Asia & Pacific",
                             "Europe & Central Asia"="Europe & C Asia",
                             "Latin America & Caribbean"="L America & Carib",
                             "Middle East & North Africa"="Mid East & N Africa",
                             "North America"="N America",
                             "South Asia"="S Asia",
                             "Sub-Saharan Africa"="SS Africa"))

# Nutrient order
nutrient_key <- data %>%
  filter(nutrient!="Vitamin D") %>%
  group_by(nutrient, region) %>%
  summarize(pdiff=median(pdiff, na.rm=T)) %>%
  ungroup() %>%
  group_by(nutrient) %>%
  summarize(pdiff=median(pdiff, na.rm=T)) %>%
  ungroup()  %>%
  arrange(desc(pdiff))

# Region order
region_order <- data %>%
  filter(nutrient!="Magnesium") %>%
  group_by(region) %>%
  summarize(pdiff_med=median(pdiff, na.rm=T)) %>%
  arrange(pdiff_med)

# Order data
data_ordered <- data %>%
  # Order nutrient
  mutate(nutrient=factor(nutrient, levels=nutrient_key$nutrient)) %>%
  # Order region
  mutate(region=factor(region, levels=region_order$region))


# Plot
################################################################################

# Theme
my_theme <- theme(axis.text.y=element_text(size=5),
                  axis.text.x=element_text(size=6),
                  axis.title=element_text(size=7),
                  axis.title.y=element_blank(),
                  legend.text=element_text(size=6),
                  legend.title=element_text(size=7),
                  strip.text=element_text(size=7),
                  # Gridlines
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(),
                  axis.line = element_line(colour = "black"),
                  # Legend
                  legend.position = c(0.88, 0.11),
                  legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(data_ordered, aes(x=pdiff, y=region, fill=pdiff_type)) +
  facet_wrap(~nutrient, ncol=4, scales="free_x") +
  geom_boxplot(outlier.size = 0.5, lwd=0.3) +
  # Reference line
  geom_vline(xintercept = 0) +
  # Labels
  labs(x="Difference in % intake inadequacies\nbetween females and males", y="") +
  # Axes
  scale_x_continuous(labels=scales::percent) +
  # Legend
  scale_fill_manual(name="Intake inadequacies\nhigher for:",
                    values=c(RColorBrewer::brewer.pal(9, "Spectral")[2],
                             RColorBrewer::brewer.pal(9, "Spectral")[8])) +
  # Theme
  theme_bw() + my_theme
  # Use this if you put the legend on top again
  # theme(legend.margin=margin(0,0,0,0),
  #       legend.box.margin=margin(0,-10,-10,-10))
g

# Export data
ggsave(g, filename=file.path(plotdir, "Fig4_gender_inequity.png"),
       width=6.5, height=5.5, units="in", dpi=600)
ggsave(g, filename=file.path(plotdir, "Fig4_gender_inequity.pdf"),
       width=6.5, height=5.5, units="in", dpi=600)

