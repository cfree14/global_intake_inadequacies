

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
data_orig <- readRDS(file=file.path(outdir, "1961_2011_subnational_nutrient_intake_estimates.Rds"))

# Get distributions
dists_orig <- nutriR::dists_full

# To-do list
# 1) Function to shift distribution to match medians (I think current is for means)
# 2) Figure out procedure for adding reference dist id
# 3) Have figure show where the shape parameters were imputed from

# Merge
################################################################################

# Nutrient coverage
nutrients_genus <- sort(unique(data_orig$nutrient))
nutrients_dist <- sort(unique(dists_orig$nutrient))
nutrients_genus[!nutrients_genus %in% nutrients_dist]

# Format dists
dists <- dists_orig %>%
  # Rename
  rename(age_range=age_group) %>%
  # Add distribution code
  mutate(dist_id=paste(nutrient, iso3, sex, age_range, sep="-")) %>%
  # Simplify
  select(dist_id, best_dist, g_shape, g_rate, g_mu, ln_meanlog, ln_sdlog, ln_mu)

# Format data
data <- data_orig %>%
  # Filter to most recent year
  filter(year==2011) %>%
  # Rename nutrients to match nutriR
  mutate(nutrient=recode(nutrient, "Thiamin"="Thiamine", "Vitamin A"="Vitamin A (RAE)"),
         nutrient_ar=recode(nutrient_ar, "Thiamin"="Thiamine", "Vitamin A"="Vitamin A (RAE)")) %>%
  # Reduce to nutrients in nutriR
  filter(nutrient %in% nutrients_dist) %>%
  # Add distribution code
  mutate(dist_id=paste(nutrient, iso3, sex, age_range, sep="-")) %>%
  # Mark whether shape is known or must be imputed
  mutate(shape_status=ifelse(dist_id %in% dists$dist_id, "Known", "Imputed"),
         shape_source=shape_status)


# Begin to impute
################################################################################

# Impute order
# Known
# Nearest age within sex
# From other sex within country
# From nearest country

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
  mutate(dist_id_shape=ifelse(shape_source=="Known", dist_id, NA))

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
  filter(nutrient=="Calcium") %>%
  rowwise() %>%
  mutate(dist_id_shape=ifelse(shape_source=="From closest age group", find_closest_age_group_w_data(., dist_id), dist_id_shape)) %>%
  ungroup()

# Function to find id of oppositie sex
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



# Second imputation
data3 <- data2 %>%
  rowwise() %>%
  mutate(dist_id_shape=ifelse(shape_source=="From opposite sex", find_id_of_opposite_sex(., dist_id), dist_id_shape)) %>%
  ungroup()


# Examine imputation types
table(data1$shape_source)



# Inspect impute rate
################################################################################

# Plot imputation rate
data_do=data1; nutrient="Calcium"
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




