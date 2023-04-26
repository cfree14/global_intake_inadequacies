

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(nutriR)
library(tidyverse)
library(countrycode)

# Directories
outdir <- "output"
plotdir <- "figures/gdd_approach"
plotdir1 <- "igures/gdd_approach/imputation"

# Read data
data_orig <- readRDS(file.path(outdir, "2018_subnational_nutrient_intake_inadequacy_estimates_full.Rds"))


# Build data
################################################################################


# Gamma
######################

# X values
x <- seq(0,600,1)

# Original
shape1 <- 11.680446
rate1 <- 4.476543e-02
mu1 <- nutriR::mean_dist(shape=shape1, rate=rate1)
mu_dens1 <- dgamma(mu1, shape = shape1, rate=rate1)
intakes1 <- dgamma(x, shape = shape1, rate=rate1)

# Upshift
gamma_hi <- nutriR::shift_dist(shape=shape1, rate=rate1, to=290)
shape_hi <- gamma_hi$shape
rate_hi <- gamma_hi$rate
mu_hi <- nutriR::mean_dist(shape=shape_hi, rate=rate_hi)
mu_dens_hi <- dgamma(mu_hi, shape = shape_hi, rate=rate_hi)
intakes_hi <- dgamma(x, shape = shape_hi, rate=rate_hi)

# Downshift
gamma_lo <- nutriR::shift_dist(shape=shape1, rate=rate1, to=230)
shape_lo <- gamma_lo$shape
rate_lo <- gamma_lo$rate
mu_lo <- nutriR::mean_dist(shape=shape_lo, rate=rate_lo)
mu_dens_lo <- dgamma(mu_lo, shape = shape_lo, rate=rate_lo)
intakes_lo <- dgamma(x, shape = shape_lo, rate=rate_lo)


# Build data
data_g <- tibble(dist="Gamma distribution",
                 scenario=c(rep("Original", length(x)),
                            rep("Upshift", length(x)),
                            rep("Downshift", length(x))),
                 intake=rep(x, 3),
                 density=c(intakes1, intakes_hi, intakes_lo ))

# Build labels
labels_g <- tibble(dist="Gamma distribution",
                   scenario=c("Original", "Upshift", "Downshift"),
                   mean=c(mu1, mu_hi, mu_lo),
                   density=c(mu_dens1, mu_dens_hi, mu_dens_lo),
                   shape=c(shape1, shape_hi, shape_lo),
                   rate=c(rate1, rate_hi, rate_lo)) %>%
  mutate(label=paste0(format(mean, digits=0), " mg, α=", format(shape, digits=1, nsmall=1), ", β=", format(rate, digits=2, nsmall=2)))


# Lognormal
######################

# X values
x <- seq(0,600,1)

# Original
meanlog1 <- 5.5208332
sdlog1 <- 0.2986003
log_mu1 <- nutriR::mean_dist(meanlog=meanlog1, sdlog=sdlog1)
log_mu_dens1 <- dlnorm(log_mu1, meanlog = meanlog_hi, sdlog=sdlog_hi)
log_intakes1 <- dlnorm(x, meanlog=meanlog1, sdlog=sdlog1)

# Upshift
lognorm_hi <- nutriR::shift_dist(meanlog=meanlog1, sdlog=sdlog1, to=290)
meanlog_hi <- lognorm_hi$meanlog
sdlog_hi <- lognorm_hi$sdlog
log_mu_hi <- nutriR::mean_dist(meanlog=meanlog_hi, sdlog=sdlog_hi)
log_mu_dens_hi <- dlnorm(log_mu_hi, meanlog = meanlog_hi, sdlog=sdlog_hi)
log_intakes_hi <- dlnorm(x, meanlog = meanlog_hi, sdlog=sdlog_hi)

# Downshift
lognorm_lo <- nutriR::shift_dist(meanlog=meanlog1, sdlog=sdlog1, to=230)
meanlog_lo <- lognorm_lo$meanlog
sdlog_lo <- lognorm_lo$sdlog
log_mu_lo <- nutriR::mean_dist(meanlog=meanlog_lo, sdlog=sdlog_lo)
log_mu_dens_lo <- dlnorm(log_mu_lo, meanlog = meanlog_lo, sdlog=sdlog_lo)
log_intakes_lo <- dlnorm(x, meanlog = meanlog_lo, sdlog=sdlog_lo)


# Build data
data_ln <- tibble(dist="Log-normal distribution",
                 scenario=c(rep("Original", length(x)),
                            rep("Upshift", length(x)),
                            rep("Downshift", length(x))),
                 intake=rep(x, 3),
                 density=c(log_intakes1, log_intakes_hi, log_intakes_lo))

# Build labels
labels_ln <- tibble(dist="Log-normal distribution",
                   scenario=c("Original", "Upshift", "Downshift"),
                   mean=c(log_mu1, log_mu_hi, log_mu_lo),
                   density=c(log_mu_dens1, log_mu_dens_hi, log_mu_dens_lo),
                   meanlog=c(meanlog1, meanlog_hi, meanlog_lo),
                   sdlog=c(sdlog1, sdlog_hi, sdlog_lo)) %>%
  mutate(label=paste0(format(mean, digits=0), " mg, μ=", format(meanlog, digits=1, nsmall=1), ", σ=", format(sdlog, digits=2, nsmall=2)))


# Lognormal
######################

# Merge data
data <- bind_rows(data_g, data_ln)
labels <- bind_rows(labels_g %>% select(dist, scenario, label, mean, density),
                    labels_ln %>% select(dist, scenario, label, mean, density))


# Plot data
################################################################################

# Setup theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=8),
                   legend.text=element_text(size=6),
                   legend.title=element_blank(),
                   strip.text=element_text(size=8),
                   plot.title=element_text(size=10),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.position = "top",
                   legend.margin = margin(-5,0,-8,0),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot
g <- ggplot(data, aes(x=intake, y=density, color=scenario)) +
  facet_wrap(~dist) +
  # Line
  geom_line() +
  # Point
  geom_point(data=labels, mapping=aes(x=mean, y=density, color=scenario)) +
  # Lables
  geom_text(data=labels, mapping=aes(x=mean, y=density, color=scenario, label=label), hjust=0, nudge_x = 15, size=2.4) +
  # Labels
  labs(x="Nutrient intake (mg)", y="Density") +
  # Theme
  theme_bw() + my_theme
g

# Export figure
ggsave(g, filename=file.path(plotdir, "FigS5_dist_shift_illustration.png"),
       width=6.5, height=3.5, units="in", dpi=600)








