

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)
library(ggbiplot)
library(vegan)
library(factoextra)

# Directories
datadir <- "data"
plotdir <- "figures"

# Get distributions
dists_orig <- nutriR::dists_full

# Get GENUS data
genus_orig <- GENuS::genus_food_cntry

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
genus <- genus_orig %>%
  # Filter (remove NAs and columns with zeros)
  filter(year==2011 & !is.na(g_person_day) & g_person_day>0) %>%
  # Reduce
  select(iso3, country, food, g_person_day) %>%
  # Rename
  rename(food=food, supply_g=g_person_day) %>%
  spread(key="food", value="supply_g") %>%
  # Fill missing values
  replace(is.na(.), 0) %>%
  # Add
  mutate(continent=countrycode(iso3, "iso3c", "continent")) %>%
  select(continent, country, iso3, everything())

# Convert to matrix
genus_mat <- genus %>%
  column_to_rownames(var="iso3") %>%
  select(-c(continent, country)) %>%
  as.matrix()

# Country key
cntry_key <- genus %>%
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
diss_vec <- vegan::vegdist(genus_mat, method = "euclidean", upper=T)
diss_mat <- as.matrix(diss_vec)
diff_df <- diss_mat %>%
  as.data.frame() %>%
  rownames_to_column(var="iso1") %>%
  select(iso1, everything()) %>%
  gather(key="iso2", value="diss", 2:ncol(.)) %>%
  mutate(iso1=factor(iso1, levels=cntry_key$iso3),
         iso2=factor(iso2, levels=cntry_key$iso3))

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
  scale_fill_gradientn(name="Euclidean\ndistance", colors=RColorBrewer::brewer.pal(n=9, name="YlOrRd")) +
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
ggsave(g, filename=file.path(plotdir, "FigSX_genus_diss_matrix.png"),
       width=6.5, height=6.5, units="in", dpi=600)


# Search for best match
################################################################################

# Build intake group key
intake_key <- diff_df %>%
  # Mark countries with data
  mutate(reference=ifelse(iso2 %in% dists_cntry_key$iso3, "yes", "no")) %>%
  filter(reference=="yes") %>%
  # Identify best match
  group_by(iso1) %>%
  arrange(iso1, diss) %>%
  slice(1) %>%
  ungroup() %>%
  # Format
  rename(iso3=iso1, group_iso3=iso2, dissimilarity=diss) %>%
  mutate(country=countrycode(iso3, "iso3c", "country.name"),
         group_country=countrycode(group_iso3, "iso3c", "country.name")) %>%
  # Arrange
  select(group_country, group_iso3, country, iso3, dissimilarity) %>%
  arrange(group_country, dissimilarity)


# Plot distribution groups
################################################################################

# Map assignments
world1 <- world %>%
  select(gu_a3) %>%
  rename(iso3=gu_a3) %>%
  left_join(intake_key, by=c("iso3"))

# Countries with data
centroids <- world %>%
  select(gu_a3) %>%
  rename(iso3=gu_a3) %>%
  filter(iso3 %in% dists_cntry_key$iso3) %>%
  mutate(country=countrycode(iso3, "iso3c", "country.name")) %>%
  sf::st_centroid() %>%
  mutate(long_dd=sf::st_coordinates(.)[,1],
         lat_dd=sf::st_coordinates(.)[,2])

# Plot
g1 <- ggplot() +
  geom_sf(world1, mapping=aes(fill=group_country), color="grey30", lwd=0.2) +
  # Crop out Antarctica
  coord_sf(y=c(-55, NA)) +
  # Plot/label centroids
  geom_sf(data=centroids, size=1.2) +
  ggrepel::geom_text_repel(data=centroids, mapping=aes(x=long_dd, y=lat_dd, label=country),
                           min.segment.length = 0, size=2, segment.size=0.2) +
  # Legend
  labs(title="Group assignment based on most similar country with data") +
  scale_fill_discrete(name="Group", na.value="grey80") +
  # Theme
  theme_bw() +
  theme(axis.text=element_blank(),
        axis.title=element_blank(),
        legend.text=element_text(size=5),
        legend.title=element_text(size=6),
        plot.title = element_text(size=6),
        # Gridlines
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.key.size = unit(0.25, "cm"))
g1

# Export figure
ggsave(g1, filename=file.path(plotdir, "FigSX_dist_groups_map_possible.png"),
       width=6.5, height=2.5, units="in", dpi=600)

