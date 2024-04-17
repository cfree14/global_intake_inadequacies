
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(ggplot2)
library(tidyverse)
library(countrycode)

# Directories
outdir <- "output"
gisdir <- "data/world/processed"
plotdir <- "figures/gdd_approach"

# Read data
data_orig <- readRDS(file.path(outdir, "2018_subnational_nutrient_intake_inadequacy_estimates_full.Rds"))


# Build data
################################################################################

# Which to do?
dist_do <- "Calcium-DZA-Females-15-19"
data <- data_orig %>%
  filter(dist_id==dist_do)

# X range
xmax1 <- qlnorm(p=0.99,
               meanlog = data$ln_meanlog,
               sdlog = data$ln_sdlog) %>% round()
xmax2 <- qlnorm(p=0.99,
                meanlog = data$ln_meanlog_shift,
                sdlog = data$ln_sdlog_shift) %>% round()
xmax <- pmax(xmax1, xmax2)
xvals <- 0:xmax

# Original distribution
y_orig <- dlnorm(x=xvals,
                 meanlog = data$ln_meanlog,
                 sdlog = data$ln_sdlog)
plot(y_orig ~ xvals)
df_orig <- tibble(dist="Reference",
                  x=xvals,
                  y=y_orig,
                  y_rel=y_orig/max(y_orig))

# Shifted distribution
y_shift <- dlnorm(x=xvals,
                  meanlog = data$ln_meanlog_shift,
                  sdlog = data$ln_sdlog_shift)
plot(y_orig ~ xvals)
df_shift <- tibble(dist="Shifted",
                   x=xvals,
                   y=y_shift,
                   y_rel=y_shift/max(y_shift))


# Build data
data1 <- bind_rows(df_orig, df_shift) %>%
  mutate(dist=factor(dist, levels=c("Reference", "Shifted")))

# Y maximum
ymax <- max(data1$y)

# AR
ar_mu <- data$ar
ar_sd <- ar_mu * data$ar_cv
y_ar <- dnorm(x=xvals, mean=ar_mu, sd=ar_sd)
risk <- 1 - pnorm(xvals, mean=ar_mu, sd=ar_sd)
ar_df <- tibble(ar=xvals,
                density=y_ar,
                risk=risk)

# Intersection
df3 <- data1 %>%
  filter(dist=="Shifted") %>%
  # Add risk
  left_join(ar_df %>% select(ar, risk), by=c("x"="ar"))



# Plot data
################################################################################

# Base theme
base_theme <- theme(axis.text=element_text(size=7),
                    axis.title=element_text(size=8),
                    legend.text=element_text(size=7),
                    legend.title=element_text(size=8),
                    strip.text=element_text(size=8),
                    plot.title=element_text(size=8),
                    # Gridlines
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(),
                    axis.line = element_line(colour = "black"),
                    # Legend
                    legend.background = element_rect(fill=alpha('blue', 0)))

# Plot
g1 <- ggplot(data1, aes(x=x, ymax=y, ymin=0, fill=dist)) +
  geom_ribbon(alpha=0.5, color="black") +
  # Labels
  labs(x="Intake", y="Density", title="Step 1. Gather inputs") +
  lims(y=c(0, ymax )) +
  # Legend
  scale_fill_discrete(drop=F) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = c(0.8, 0.8))

# Plot
g2 <- ggplot(data1 %>% filter(dist=="Shifted"), aes(x=x, ymax=y, ymin=0, fill=dist)) +
  geom_ribbon(alpha=0.5, color="black") +
  # Requirement
  geom_vline(xintercept=data$ar, linetype="dotted") +
  # Labels
  labs(x="Intake", y="Density", title="Step 2. Derive intake distribution") +
  lims(y=c(0, ymax)) +
  # Legend
  scale_fill_discrete(drop=F) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position="none")
g2

# Plot
g3 <- ggplot(ar_df, aes(x=ar, ymax=density, ymin=0)) +
  geom_ribbon(fill="grey90", color="black") +
  # Requirement
  geom_vline(xintercept=data$ar, linetype="dotted") +
  # Labels
  labs(x="Intake requirement", y="Density", title="Step 3. Derive requirement distribution") +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position="none")
g3

# Plot
g4 <- ggplot(ar_df, aes(x=ar, y=risk)) +
  geom_line(linetype="dashed") +
  geom_line(data=data1 %>% filter(dist=="Shifted"), mapping=aes(x=x, y=y_rel)) +
  # Requirement
  geom_vline(xintercept=data$ar, linetype="dotted") +
  # Labels
  labs(x="Intake", y="Risk of inadequacy", title="Step 4. Derive percent inadequate") +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position="none")
g4


# Merge
layout_matrix <- matrix(data=c(1,2,
                               NA,3,
                               NA,4), ncol=2, byrow=T)
g <- gridExtra::grid.arrange(g1, g2, g3, g4, layout_matrix=layout_matrix)
g




