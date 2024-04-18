
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

# Subset data
##################################

# Which to do?
# dist_do <- "Calcium-DZA-Females-15-19"
# dist_do <- "Vitamin C-KHM-Females-20-24"
dist_do <- "Iron-KAZ-Males-15-19"

# Parameters for example distribution
params <- data_orig %>%
  filter(dist_id==dist_do)

# Data for example nutrient / country
data <- data_orig %>%
  filter(nutrient==params$nutrient & country==params$country) %>%
  # Rename
  rename(supply50=supply_med, shape=g_shape_shift, rate=g_rate_shift, meanlog=ln_meanlog_shift, sdlog=ln_sdlog_shift) %>%
  # Calculate 95% for intake
  rowwise() %>%
  mutate(supply025=ifelse(best_dist=="gamma", qgamma(p=0.025, shape=shape, rate=rate), qlnorm(p=0.025, meanlog=meanlog, sdlog=sdlog)),
         supply25=ifelse(best_dist=="gamma", qgamma(p=0.25, shape=shape, rate=rate), qlnorm(p=0.25, meanlog=meanlog, sdlog=sdlog)),
         supply75=ifelse(best_dist=="gamma", qgamma(p=0.75, shape=shape, rate=rate), qlnorm(p=0.75, meanlog=meanlog, sdlog=sdlog)),
         supply975=ifelse(best_dist=="gamma", qgamma(p=0.975, shape=shape, rate=rate), qlnorm(p=0.975, meanlog=meanlog, sdlog=sdlog))) %>%
  ungroup()

# Units
units <- params$units


# Intake distributions
##################################

# X range
xmax1 <- qlnorm(p=0.999,
               meanlog = params$ln_meanlog,
               sdlog = params$ln_sdlog) %>% round()
xmax2 <- qlnorm(p=0.999,
                meanlog = params$ln_meanlog_shift,
                sdlog = params$ln_sdlog_shift) %>% round()
xmax <- pmax(xmax1, xmax2)
xvals <- seq(0, xmax, length.out=1000)

# Original distribution
y_orig <- dlnorm(x=xvals,
                 meanlog = params$ln_meanlog,
                 sdlog = params$ln_sdlog)
plot(y_orig ~ xvals)
dist_orig <- tibble(dist="Reference",
                  x=xvals,
                  y=y_orig,
                  y_rel=y_orig/max(y_orig))

# Shifted distribution
y_shift <- dlnorm(x=xvals,
                  meanlog = params$ln_meanlog_shift,
                  sdlog = params$ln_sdlog_shift)
plot(y_orig ~ xvals)
dist_shift <- tibble(dist="Shifted",
                   x=xvals,
                   y=y_shift,
                   y_rel=y_shift/max(y_shift))


# Merge data
dist_all <- bind_rows(dist_orig, dist_shift) %>%
  mutate(dist=factor(dist, levels=c("Reference", "Shifted")))

# Y maximum
ymax <- max(dist_shift$y)

# Mean
intake_mu <- exp(params$ln_meanlog_shift)
intake_mu_dens <- dlnorm(x=intake_mu,
                         meanlog = params$ln_meanlog_shift,
                         sdlog = params$ln_sdlog_shift)


# Requirement distributions
##################################

# AR
ar_mu <- params$ar
ar_sd <- ar_mu * data$ar_cv
y_ar <- dnorm(x=xvals, mean=ar_mu, sd=ar_sd)
risk <- 1 - pnorm(xvals, mean=ar_mu, sd=ar_sd)
ar_df <- tibble(ar=xvals,
                density=y_ar,
                risk=risk)


# Inadequate intersection
##################################

# Intersection
x_intersect <- dist_shift %>%
  # Add risk
  left_join(ar_df %>% select(ar, risk), by=c("x"="ar")) %>%
  # Calculate difference
  mutate(diff=abs(risk-y_rel)) %>%
  # Get really small difference
  arrange(diff) %>%
  slice(1) %>%
  pull(x)

# Construct inadequate curve
inad1 <- dist_shift %>%
  filter(x<=x_intersect) %>%
  select(x, y_rel) %>%
  rename(y=y_rel)
inad2 <- ar_df %>%
  filter(ar>x_intersect) %>%
  select(ar, risk) %>%
  rename(x=ar, y=risk)
inad_curve <- bind_rows(inad1, inad2) %>%
  mutate(type="Inadequate")

# Plot data
################################################################################

# Base theme
base_theme <- theme(axis.text=element_text(size=7),
                    axis.title=element_text(size=8),
                    legend.text=element_text(size=7),
                    legend.title=element_text(size=8),
                    strip.text=element_text(size=8),
                    plot.title=element_text(size=8),
                    plot.tag=element_text(size=8, face="bold"),
                    plot.tag.position = c(0.01, 1),
                    # Gridlines
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(),
                    axis.line = element_line(colour = "black"),
                    # Legend
                    legend.background = element_rect(fill=alpha('blue', 0)))

# Plot intake distribution
xtitle1 <- paste0("Intake (", units, ")")
ymax1 <- max(dist_shift$y)
g1 <- ggplot(dist_shift, aes(x=x, ymax=y, ymin=0)) +
  geom_ribbon(alpha=0.5, color="black", fill="skyblue", linewidth=0.2) +
  # Plot median
  geom_segment(x=intake_mu, xend=intake_mu, y=0, yend=intake_mu_dens, color="dodgerblue4") +
  # Requirement
  geom_vline(xintercept=params$ar, linetype="dotted") +
  # Plot param labels
  annotate(geom="text", x=xmax2, y=ymax1, label="Median: GDD", color="dodgerblue4", hjust=1, size=2) +
  annotate(geom="text",x=xmax2, y=ymax1*0.92, label="Shape: nutriR", color="skyblue2", hjust=1, size=2) +
  # Labels
  labs(x=xtitle1, y="Density", tag="A", title="1. Derive intake distribution") +
  scale_x_continuous(breaks=seq(0, 25, 5)) +
  # Legend
  scale_fill_discrete(drop=F) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position="none",
        axis.text.y = element_text(angle = 90, hjust = 0.5))
g1

# Plot requirement distribution
ymax2 <- max(ar_df$density)
ymax2_lim <- ymax2*1.15
xtitle2 <- paste0("Intake requirement (", units, ")")
g2 <- ggplot(ar_df, aes(x=ar, ymax=density, ymin=0)) +
  geom_ribbon(fill="grey90", color="black", linewidth=0.2) +
  # Requirement
  geom_segment(x=params$ar, xend=params$ar, y=0, yend=ymax2, linetype="dotted") +
  # Plot param labels
  annotate(geom="text", x=xmax2, y=ymax2_lim, label="Mean: Allen et al. 2022", color="black", hjust=1, size=2) +
  annotate(geom="text", x=xmax2, y=ymax2_lim*0.92, label="CV: Renwick et al. 2004", color="grey50", hjust=1, size=2) +
  # Labels
  labs(x=xtitle2, y="Density", tag="B", title="2. Derive requirement distribution") +
  scale_x_continuous(breaks=seq(0, 25, 5)) +
  lims(y=c(0, ymax2_lim)) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position="none",
        axis.text.y = element_text(angle = 90, hjust = 0.5))
g2

# Plot risk curve
ymax3 <- 1
sev_label1 <- paste0(round(100-params$sev, digits = 1), "% adequate")
sev_label2 <- paste0(round(params$sev, digits = 1), "% inadequate")
ppl_label <- paste(formatC(params$ndeficient, format="d", big.mark = ","), "people") # (WB 2020)
xtitle3 <- paste0("Intake (", units, ")")
g3 <- ggplot(ar_df, aes(x=ar, y=risk)) +
  # Plot intake distribution
  geom_ribbon(data=dist_shift, mapping=aes(x=x, ymax=y_rel, ymin=0), alpha=0.5, color="black", fill="skyblue", linewidth=0.2) +
  # Plot inadequate portion
  geom_ribbon(data=inad_curve, mapping=aes(x=x, ymax=y, ymin=0), fill="dodgerblue4", color=NA, alpha=0.7) +
  # Plot risk curve
  geom_line(color="red", linewidth=0.5) + # linetype="dashed",
  # Requirement
  geom_vline(xintercept=params$ar, linetype="dotted") +
  # Plot param labels
  annotate(geom="text", x=xmax2, y=ymax3, label=sev_label1, color="skyblue2", hjust=1, size=2) +
  annotate(geom="text", x=xmax2, y=ymax3*0.92, label=sev_label2, color="dodgerblue4", hjust=1, size=2) +
  # annotate(geom="text", x=xmax2, y=ymax3*0.84, label="(World Bank 2020)", color="black", hjust=1, size=2) +
  # Labels
  labs(x=xtitle3, y="Density", tag="C", title="3. Calculate percent inadequate") +
  scale_x_continuous(breaks=seq(0, 25, 5)) +
  scale_y_continuous(sec.axis = sec_axis(~., name = "Risk of inadequacy")) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position="none",
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        axis.text.y.right = element_text(angle = 270, hjust = 0.5, color = "red"),
        axis.title.y.right = element_text(color = "red"),
        axis.ticks.y.right = element_line(color = "red"),
        axis.line.y.right = element_line(color = "red"))
g3

# Plot data
ytitle4 <- paste0("Intake (", units, ")")
g4 <- ggplot() +
  facet_wrap(~sex) +
  # Plot CI
  geom_segment(data=data, mapping=aes(x=age_range, xend=age_range, y=supply025, yend=supply975, color=sev/100), lwd=1) +
  geom_segment(data=data, mapping=aes(x=age_range, xend=age_range, y=supply25, yend=supply75, color=sev/100), lwd=2.5) +
  # Plot point
  geom_point(data=data, mapping=aes(x=age_range, y=supply50, fill=sev/100), size=2.5, pch=21) +
  # Reference
  geom_line(data=data, mapping=aes(x=age_range, y=ar, group=sex), inherit.aes = F, linewidth=0.75) +
  # Mark example distribution
  geom_point(data=params, aes(x=age_range, y=0), shape=8) +
  geom_text(data=params, aes(x=age_range, y=0), hjust=-0.05, label="Age-sex group illustrated in panels A-C", fontface="italic", size=2) +
  # Labels
  labs(x="Age range (yr)", y=ytitle4, tag="D") +
  # Limits
  lims(y=c(0, NA)) +
  # Legend
  scale_color_gradientn(name="% inadequate", colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev(),
                        lim=c(0,1), labels=scales::percent, guide="none") +
  scale_fill_gradientn(name="% inadequate", colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev(),
                       lim=c(0,1), labels=scales::percent) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position="right",
        legend.key.size = unit(0.3, "cm"),
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
g4

# Merge
layout_matrix <- matrix(data=c(1,2,3,
                               4,4,4), ncol=3, byrow=T)
g <- gridExtra::grid.arrange(g1, g2, g3, g4, layout_matrix=layout_matrix,
                             heights=c(0.45, 0.55), widths=c(0.32, 0.32, 0.36))
g

# Export
ggsave(g, filename=file.path(plotdir, "Fig1_conceptual_figure.png"),
       width=6.5, height=5, units="in", dpi=600)



