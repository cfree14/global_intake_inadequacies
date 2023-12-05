

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
# Make sure plyr is unchecked
library(countrycode)
library(ggbiplot) # devtools::install_github("vqv/ggbiplot")
library(vegan)
library(factoextra)
library(tidyverse)

# Directories
datadir <- "data"
plotdir <- "figures/gdd_approach"
outdir <- "output"
gdddir <- "data/gdd/processed"

# Get distributions
dists_orig <- nutriR::dists_full

# Read GDD data
gdd_orig <- readRDS(file=file.path(gdddir, "GDD_2018_intakes_national.Rds"))

# Read map
world <- rnaturalearth::ne_countries(scale="small", returnclass="sf")

# Countries with distribution data
dists_cntry_key <- dists_orig %>%
  select(iso3, country) %>%
  unique() %>%
  arrange(iso3)


# Build data for multivariate analysis
################################################################################

# Build data
gdd <- gdd_orig %>%
  # Filter (vitamins and minerals)
  filter(factor_type %in% c("Vitamins", "Minerals")) %>%
  # Filter to totals
  filter(sex=="Both sexes" & residence=="All residences" & education=="All education levels") %>%
  # Average national intake
  group_by(iso3, country, factor) %>%
  summarize(supply_avg=mean(supply_med)) %>%
  ungroup() %>%
  # Spread
  spread(key="factor", value="supply_avg") %>%
  # Add
  mutate(continent=countrycode(iso3, "iso3c", "continent")) %>%
  select(continent, country, iso3, everything())

# Convert to matrix
gdd_mat <- gdd %>%
  column_to_rownames(var="iso3") %>%
  select(-c(continent, country)) %>%
  as.matrix()

# Country key
cntry_key <- gdd %>%
  select(continent, country, iso3) %>%
  unique() %>%
  arrange(continent, iso3)

# Identify continent separators
cntry_key_seps <- cntry_key %>%
  group_by(continent) %>%
  slice(1)


# Compute and visualize dissimilarity matrix
################################################################################

# Compute dissimilarity
# diss_vec <- get_dist(data_mat, method = "euclidean")
diss_vec <- vegan::vegdist(gdd_mat, method = "euclidean", upper=T)
diss_mat <- as.matrix(diss_vec)
diff_df <- diss_mat %>%
  as.data.frame() %>%
  rownames_to_column(var="iso1") %>%
  select(iso1, everything()) %>%
  gather(key="iso2", value="diss", 2:ncol(.)) %>%
  mutate(iso1=factor(iso1, levels=cntry_key$iso3),
         iso2=factor(iso2, levels=cntry_key$iso3))

# Export dissimilarity matrix
saveRDS(diff_df, file=file.path(outdir, "GDD_country_dissimilarity.Rds"))

# Plot dissimilarity
g <- ggplot(diff_df, aes(x=iso1, y=iso2, fill=diss)) +
  geom_raster(alpha=0.7) +
  # Lines
  geom_hline(yintercept = cntry_key_seps$iso3) +
  geom_vline(xintercept = cntry_key_seps$iso3) +
  geom_text(data=cntry_key_seps, mapping=aes(x=iso3, y=iso3, label=continent), inherit.aes = F,
            hjust=-0.1, vjust=-0.5) +
  # Labels
  labs(x="", y="") +
  scale_fill_gradientn(name="Euclidean\ndistance",
                       colors=RColorBrewer::brewer.pal(n=9, name="Spectral") %>% rev()) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.text=element_text(size=3),
        axis.title=element_text(size=6),
        legend.text=element_text(size=5),
        legend.title=element_text(size=6),
        # Gridlines
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        # Legend
        legend.position="bottom")
g

# Export figure
ggsave(g, filename=file.path(plotdir, "FigS3_gdd_diss_matrix.png"),
       width=6.5, height=6.5, units="in", dpi=600)



