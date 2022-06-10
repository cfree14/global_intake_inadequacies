

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)

# Directories
outdir <- "output"
plotdir <- "figures"

# Read data
data_orig <- readRDS(file=file.path(outdir, "2011_subnational_nutrient_intake_estimates.Rds"))

# Get distributions
dists_orig <- nutriR::dists_full

# Read GENuS dissimilarity matrix
diss_matrix <- readRDS(file=file.path(outdir, "GENUS_country_dissimilarity.Rds"))


# To-do list
# 1) Function to shift distribution to match medians (I think current is for means)


# Merge
################################################################################

# Nutrient coverage
nutrients_genus <- sort(unique(data_orig$nutrient))
nutrients_dist <- sort(unique(dists_orig$nutrient))
nutrients_genus[!nutrients_genus %in% nutrients_dist]

# Nutrients w/out ARs
nutrients_without_ar <- c("Protein", "Carbohydrates", "Fat", "Monounsaturated fatty acids", "Polyunsaturated fatty acids", "Potassium", "Sodium")

# Format dists
dists <- dists_orig %>%
  # Rename
  rename(age_range=age_group) %>%
  # Add distribution code
  mutate(dist_id=paste(nutrient, iso3, sex, age_range, sep="-")) %>%
  # Simplify
  select(nutrient, iso3, sex, age_range, dist_id, best_dist, g_shape, g_rate, g_mu, ln_meanlog, ln_sdlog, ln_mu)

# Format data
data <- data_orig %>%
  # Rename nutrients to match nutriR
  mutate(nutrient=recode(nutrient, "Thiamin"="Thiamine", "Vitamin A"="Vitamin A (RAE)"),
         nutrient_ar=recode(nutrient_ar, "Thiamin"="Thiamine", "Vitamin A"="Vitamin A (RAE)")) %>%
  # Reduce to nutrients in nutriR
  filter(nutrient %in% nutrients_dist) %>%
  # Add distribution code
  mutate(dist_id=paste(nutrient, iso3, sex, age_range, sep="-"),
         dist_id_genus=paste(nutrient, genus_iso3, sex, age_range, sep="-")) %>%
  # Mark whether shape is known or must be imputed
  mutate(shape_status=ifelse(dist_id %in% dists$dist_id, "Known", "Imputed"),
         shape_source=shape_status) %>%
  # Remove protein and other nutrients w/out AR data
  filter(!nutrient %in% nutrients_without_ar)

# Inspect nutrients
nutrient_key <- data %>%
  select(nutrient, units_short, ar_source) %>%
  unique() %>% arrange(desc(ar_source))


# Build country match key
################################################################################

# Dist key
dist_key <- dists %>%
  count(nutrient, iso3)

#
nutrient <- "Copper"; iso3 <- "KEN"
find_most_sim_cntry_w_data <- function(nutrient, iso3){

  # Identify countries with data for nutrients
  nutrient_do <- nutrient
  isos_w_data <- dist_key %>%
    filter(nutrient==nutrient_do) %>%
    pull(iso3)

  # Which one is most similar to this country?
  most_sim_iso <- diss_matrix %>%
    filter(iso1==iso3 & iso2 %in%   isos_w_data) %>%
    arrange(diss) %>% slice(1) %>% pull(iso2) %>% as.character()

  # Return
  return(most_sim_iso)

}

# Build country match key: 30 seconds to run
sim_cntry_w_nutriR_key <- expand.grid(nutrient=nutrient_key$nutrient,
                                      iso3=unique(diss_matrix$iso1)) %>%
  arrange(nutrient, iso3) %>%
  # Mark whether it has dist data
  left_join(dist_key, by=c("nutrient", "iso3")) %>%
  rename(dist_yn=n) %>%
  mutate(dist_yn=ifelse(!is.na(dist_yn), "yes", "no")) %>%
  # Get most similar iso
  rowwise() %>%
  mutate(iso3_w_data=ifelse(dist_yn=="yes", iso3, find_most_sim_cntry_w_data(nutrient, iso3))) %>%
  ungroup()




# Begin to impute
################################################################################

# Impute order
# (1) Known
# (2) Nearest age within sex
# (3) From other sex within country
# (4) From nearest country

# Mark imputation tpye
data1 <- data %>%
  # Mark whether dist info is available within sex
  group_by(nutrient, iso3, sex) %>%
  mutate(within_sex_yn="Known" %in% shape_status) %>%
  ungroup() %>%
  mutate(shape_source=ifelse(shape_status=="Imputed" & within_sex_yn==T, "From closest age group", shape_status)) %>%
  # Mark whether dist info is available from other sex
  group_by(nutrient, iso3) %>%
  mutate(within_iso_yn="Known" %in% shape_source) %>%
  ungroup() %>%
  mutate(shape_source=ifelse(shape_source=="Imputed" & within_iso_yn==T, "From opposite sex", shape_source)) %>%
  # Mark remaining as from most similar country
  mutate(shape_source=recode(shape_source, "Imputed"="From most similar country")) %>%
  # Order sources
  mutate(shape_source=factor(shape_source, levels=c("Known", "From closest age group", "From opposite sex", "From most similar country"))) %>%
  # Create column indicating the distribution id used to describe shape
  mutate(dist_id_shape=ifelse(shape_source=="Known", dist_id, NA)) %>%
  # Add most similar country with data available
  left_join(sim_cntry_w_nutriR_key, by=c("genus_iso3"="iso3", "nutrient"))

# Examine imputation types
table(data1$shape_source)

# Examine: only npeople, HDI, and dist_id_shape should have NAs
freeR::complete(data1)


# 1. Nearest age within sex
#######################################

# Function to find closest age group with data
dist_id <- "Calcium-ROU-Females-80+" #"Calcium-MOZ-Females-15-19" # "Calcium-MOZ-Females-0-4"
df <- data1
find_closest_age_group_w_data <- function(df, dist_id){

  # Find data for nutrient-iso-sex
  nutrient_do <- strsplit(dist_id, split="-")[[1]][1]
  iso_do <- strsplit(dist_id, split="-")[[1]][2]
  sex_do <- strsplit(dist_id, split="-")[[1]][3]
  sdata <- df %>%
    # Reduce to dists with age/sex
    filter(nutrient==nutrient_do & iso3==iso_do & sex==sex_do) %>%
    # Split age
    mutate(age_range=recode(age_range, "80+"="80-100")) %>%
    separate(age_range, sep="-", into=c("age1", "age2"), convert = T) %>%
    # Reduce to distributions with data
    filter(shape_source=="Known")

  # Identity dist id that's closest
  if(grepl("80+", dist_id)){
    age1 <- 80
    age2 <- 100
  }else{
    age1 <- strsplit(dist_id, split="-")[[1]][4] %>% as.numeric()
    age2 <- strsplit(dist_id, split="-")[[1]][5] %>% as.numeric()
  }
  dist_id_candidates <- sdata$dist_id
  closest_back <- dist_id_candidates[which.min(abs(age1-sdata$age2))]
  closest_back_dist <- min(abs(age1-sdata$age2))
  closest_forward <- dist_id_candidates[which.min(abs(sdata$age1-age2))]
  closest_forward_dist <- min(abs(sdata$age1-age2))
  if(closest_back==closest_forward){
    dist_use <- closest_forward
  }else{
      dist_use <- ifelse(closest_forward_dist<=closest_back_dist, closest_forward, closest_back)
  }

  # Return
  return(dist_use)

}

# First imputation: closest age group
data2 <- data1 %>%
  # filter(nutrient %in% c("Calcium", "Zinc")) %>%
  rowwise() %>%
  mutate(dist_id_shape=ifelse(shape_source=="From closest age group", find_closest_age_group_w_data(., dist_id), dist_id_shape)) %>%
  ungroup()


# 2. From opposite sex
#######################################

# Function to find id of opposite sex
df <- data2
dist_id <- "Calcium-ETH-Males-15-19"
find_id_of_opposite_sex <- function(df, dist_id){

  # Derive the opposite
  sex_do <- strsplit(dist_id, split="-")[[1]][3]
  sex_opp <- ifelse(sex_do=="Males", "Females", "Males")

  # Opposite sex dist id
  dist_id_opp <- gsub(sex_do, sex_opp, dist_id)

  # Dist id used by the opposite sex
  dist_use <- df$dist_id_shape[df$dist_id==dist_id_opp]

  # Return
  return(dist_use)

}

# Second imputation: from opposite sex
data3 <- data2 %>%
  rowwise() %>%
  mutate(dist_id_shape=ifelse(shape_source=="From opposite sex", find_id_of_opposite_sex(., dist_id), dist_id_shape)) %>%
  ungroup()


# 3. From nearest country
#######################################

# df <- data3
# dist_id <- "Calcium-AGO-Females-80+"
# find_id_of_nearest_cntry <- function(df, dist_id){
#
#   # Extract info
#   nutrient_do <- strsplit(dist_id, split="-")[[1]][1]
#   iso_do <- strsplit(dist_id, split="-")[[1]][2]
#   sex_do <- strsplit(dist_id, split="-")[[1]][3]
#   age_do <- paste(strsplit(dist_id, split="-")[[1]][4], strsplit(dist_id, split="-")[[1]][5], sep="-")
#   if(age_do=="80+-NA"){age_do <- "80+"}
#
#   # Identify countries with info for this nutrient
#   isos_w_data <- dists_orig %>%
#     filter(nutrient==nutrient_do) %>% pull(iso3) %>% unique()
#
#   # Identify most similar country among countries with info
#   most_similar_iso <- diss_matrix %>%
#     filter(iso1==iso_do & iso2 %in% isos_w_data) %>%
#     arrange(diss) %>% slice(1) %>% pull(iso2) %>% as.character()
#
#   # Extract distribution id used for sex-age from most similar country
#   dist_use <- try(df %>%
#     filter(nutrient==nutrient_do & iso3==most_similar_iso & sex==sex_do & age_range==age_do) %>%
#     pull(dist_id_shape))
#
#   # If none
#   if(length(dist_use)==0 | inherits(dist_use, "try-error")){
#     dist_use <- NA
#   }
#
#   # Return
#   return(dist_use)
#
# }
#
# # Third imputation: from nearest country
# data4 <- data3 %>%
#   rowwise() %>%
#   mutate(dist_id_shape=ifelse(shape_source=="From most similar country",
#                               find_id_of_nearest_cntry(., dist_id_genus), dist_id_shape)) %>%
#   ungroup()


# Create key for ones with data
dist_key2 <- data3 %>%
  select(dist_id_genus, dist_id_shape) %>%
  unique() %>%
  rename(dist_id_shape2=dist_id_shape) %>%
  filter(!is.na(dist_id_shape2))

# Third imputation: from nearest country
data4 <- data3 %>%
  # Add dist id for nearest country with data
  mutate(dist_id_nutriR=paste(nutrient, iso3_w_data, sex, age_range, sep="-")) %>%
  # Add dist id source based on this data
  left_join(dist_key2, by=c("dist_id_nutriR"="dist_id_genus")) %>%
  mutate(dist_id_shape=ifelse(!is.na(dist_id_shape), dist_id_shape, dist_id_shape2)) %>%
  select(-dist_id_shape2)


# Inspect
freeR::complete(data4)

# Add distribution info
################################################################################

# Add dist info
data5 <- data4 %>%
  # Add distribution info
  left_join(dists %>% select(-c(nutrient, iso3, sex, age_range)), by=c("dist_id_shape"="dist_id"))

freeR::complete(data5)

# Do gammas
data5_gamma <- data5 %>%
  # Reduce to gammas
  filter(best_dist=="gamma") %>%
  # Shift parameters
  rowwise() %>%
  mutate(g_shape_shift=nutriR::shift_dist(shape=g_shape, rate=g_rate, to=supply_agesex_med, plot=F)$shape,
         g_rate_shift=nutriR::shift_dist(shape=g_shape, rate=g_rate, to=supply_agesex_med, plot=F)$rate) %>%
  ungroup() %>%
  # Calculate intake inadequacy
  rowwise() %>%
  mutate(sev=nutriR::sev(ear = ar, cv = ar_cv, shape=g_shape_shift, rate=g_rate_shift, plot=F)) %>%
  ungroup() %>%
  # Calculate number of people with inadeuate intakes
  mutate(ndeficient=npeople*sev/100)

# Do gammas
data5_lognormal <- data5 %>%
  # Reduce to lognormals
  filter(best_dist=="log-normal") %>%
  # Shift parameters
  rowwise() %>%
  mutate(ln_meanlog_shift=nutriR::shift_dist(meanlog=ln_meanlog, sdlog=ln_sdlog, to=supply_agesex_med, plot=F)$meanlog,
         ln_sdlog_shift=nutriR::shift_dist(meanlog=ln_meanlog, sdlog=ln_sdlog, to=supply_agesex_med, plot=F)$sdlog) %>%
  ungroup() %>%
  # Calculate intake inadequacy
  rowwise() %>%
  mutate(sev=nutriR::sev(ear = ar, cv = ar_cv, meanlog=ln_meanlog_shift, sdlog=ln_sdlog_shift, plot=F)) %>%
  ungroup() %>%
  # Calculate number of people with inadeuate intakes
  mutate(ndeficient=npeople*sev/100)

# Merge
data6 <- bind_rows(data5_gamma, data5_lognormal) %>%
  # Arrange
  arrange(nutrient, continent, country, sex, age_range)


# Export data
saveRDS(data6, file.path(outdir, "2011_subnational_nutrient_intake_inadequacy_estimates_full.Rds"))


# Simplify
################################################################################

# Simplify data
data6_simple <- data6 %>%
  # Select
  select(continent, iso3, country, nutrient, units_short, sex, age_range, year,
         supply_agesex_med, ar, ar_source, sev, npeople, ndeficient) %>%
  # Rename
  rename(units=units_short, intake=supply_agesex_med)

# Export simplified data
saveRDS(data6_simple, file.path(outdir, "2011_subnational_nutrient_intake_inadequacy_estimates_simple.Rds"))



# Inspect impute rate
################################################################################

# Plot imputation rate
data_do=data1; nutrient="Zinc"
plot_impute_rate <- function(data_do=data1, nutrient="Calcium"){

  # Subset data
  nutrient_do <- nutrient
  sdata <- data_do %>%
    filter(nutrient==nutrient_do) %>%
    select(continent, iso3, age_range, sex, shape_status, shape_source) %>%
    unique()

  # Theme
  theme1 <- theme(axis.text=element_text(size=6),
                  axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                  axis.title=element_text(size=7),
                  legend.text=element_text(size=6),
                  legend.title=element_text(size=7),
                  strip.text=element_text(size=8),
                  plot.title=element_text(size=9),
                  # Gridlines
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(),
                  axis.line = element_line(colour = "black"),
                  # Legend
                  legend.background = element_rect(fill=alpha('blue', 0)))

  # Plot data
  g <- ggplot(sdata, aes(x=age_range, y=iso3, fill=shape_source)) +
    facet_grid(continent~sex, scales="free_y", space="free_y") +
    geom_raster() +
    # Labels
    labs(x="Age range (yr)", y="Country ISO3 code", title=nutrient_do) +
    # Legend
    # scale_fill_manual(name="Shape info", values=c("grey80", "grey30")) + # for known/imputed
    scale_fill_manual(name="Shape info", values=c("grey30", "lightblue", "lightgreen", "grey95")) +
    # Theme
    theme_bw() + theme1
  g

  # Return
  return(g)

}





