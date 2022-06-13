
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(GENuS)
library(nutriR)

# Directories
tabledir <- "tables"

# Get data
nutr_dists <- nutriR::dists_full
genus_nutr_agesex_2011 <- GENuS::genus_nutr_agesex_2011
ars <- nutriR::nrvs %>%
  mutate(nutrient=recode())


# Build data
################################################################################

# Nutrient availability
nutrients_genus <- sort(unique(genus_nutr_agesex_2011$nutrient))
nutrients_dists <- c(sort(unique(nutr_dists$nutrient)), "Vitamin A", 'Thiamin')
nutrients_ars <-  c(sort(unique(ars$nutrient)), "Iron", "Zinc", "Vitamin B6")

# Nutrients with ARd that aren't in GENuS: zinc, iron, vitamin B6 are
nutrients_ars[!nutrients_ars %in% nutrients_genus]

# Nutrients in GENuS and with ARS
nutrients_use <- nutrients_ars[nutrients_ars %in% nutrients_genus] %>% sort()

# Nutrients in GENuS with ARs but without distributions: just phosphorus
nutrients_use[!nutrients_use %in% nutrients_dists ]

# Final nutrients to use
nutrient_final <- nutrients_use[nutrients_use %in% nutrients_dists ] %>% sort() %>% unique()

# AR key
key <- ars %>%
  filter(nrv_type=="Average requirement") %>%
  group_by(nutrient, source, units) %>%
  summarize(ar=mean(nrv))

# Build table
data <- tibble(nutrient=nutrient_final) %>%
  # Remove protein
  filter(nutrient!="Protein") %>%
  # Add AR info
  left_join(key)








