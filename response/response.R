

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(nutriR)

# Seed
set.seed(123)

# Helper function
sim_data <- function(n, ear, cv, mean, sd){

  # Simulate intakes
  nsim <- n
  intakes <- rnorm(n=nsim, mean=mean, sd=sd)
  reqs <- rnorm(n=nsim, mean=ear, sd=cv*ear)
  data <- tibble(intake=intakes,
                 req=reqs,
                 adequate_yn=ifelse(intake>=req, "Adequate", "Inadequate"))

  # Calculate percent inadeuqate
  sev <- sum(data=="Inadequate") / nrow(data)
  sev

  # Plot data
  g <- ggplot(data, aes(x=intake, y=req, color=adequate_yn)) +
    geom_point( alpha=0.5) +
    # Reference lines
    geom_abline(slope=1) +
    geom_vline(xintercept=ear, linetype="dashed") +
    geom_hline(yintercept=ear, linetype="dashed") +
    # Print calculation
    annotate(geom="text", x=max(data$intake), y=max(data$req),
             label=paste0(round(sev*100, 1), "%"), color="black") +
    # Limit
    lims(x=c(0,NA), y=c(0, NA)) +
    # Labels
    labs(x="Intake", y="Requirement") +
    # Legend
    scale_color_discrete(name="") +
    # Theme
    theme_bw() +
    theme(legend.position = "top")
  print(g)

  # Return
  return(data)

}

# Helper function
sim_data_ln <- function(n, ear, cv, meanlog, sdlog){

  # Simulate intakes
  nsim <- n
  intakes <- rlnorm(n=nsim, meanlog=meanlog, sdlog=sdlog)
  reqs <- rnorm(n=nsim, mean=ear, sd=cv*ear)
  data <- tibble(intake=intakes,
                 req=reqs,
                 adequate_yn=ifelse(intake>=req, "Adequate", "Inadequate"))

  # Calculate percent inadeuqate
  sev <- sum(data=="Inadequate") / nrow(data)
  sev

  # Plot data
  g <- ggplot(data, aes(x=intake, y=req, color=adequate_yn)) +
    geom_point( alpha=0.5) +
    # Reference lines
    geom_abline(slope=1) +
    geom_vline(xintercept=ear, linetype="dashed") +
    geom_hline(yintercept=ear, linetype="dashed") +
    # Print calculation
    annotate(geom="text", x=max(data$intake), y=max(data$req),
             label=paste0(round(sev*100, 1), "%"), color="black") +
    # Limit
    lims(x=c(0,NA), y=c(0, NA)) +
    # Labels
    labs(x="Intake", y="Requirement") +
    # Legend
    scale_color_discrete(name="") +
    # Theme
    theme_bw() +
    theme(legend.position = "top")
  print(g)

  # Return
  return(data)

}

# Plot theme
my_theme <-  theme(axis.text=element_text(size=7),
                   axis.text.y = element_text(angle = 90, hjust = 0.5),
                   axis.title=element_text(size=8),
                   plot.title=element_text(size=8),
                   plot.tag=element_text(size=9),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.position = "none",
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))


# Example 1: normal distribution
################################################################################

# Example 1
ear_mu1 <- 1200
ear_sd1 <- 200
ear_cv1 <- ear_sd1 / ear_mu1
intake_mu1 <- 1500
intake_sd1 <- 400
intake_cv1 <- intake_sd1/intake_mu1

# Calculcate SEV empircally
sev1 <- nutriR::sev(ear=ear_mu1, cv=ear_cv1, mean=intake_mu1, sd=intake_sd1, plot=T)

# Build data
x <- seq(0,3000,1)
yintake <- dnorm(x=x, mean=intake_mu1, sd=intake_sd1)
yrisk <- 1 - pnorm(x, mean=ear_mu1, sd=ear_sd1)
data1 <- tibble(intake=x,
                density=yintake,
                risk=yrisk) %>%
  mutate(density_scaled=density/max(density),
         density_scaled2=pmin(density_scaled, risk))

# Plot dataa
g1 <- ggplot(data1) +
  # Plot intake distribution
  geom_ribbon(mapping=aes(x=intake, ymin=0, ymax=density_scaled), fill="lightblue") +
  geom_ribbon(mapping=aes(x=intake, ymin=0, ymax=density_scaled2), fill="coral") +
  # Plot EAR
  geom_vline(xintercept = ear_mu1, linetype="dashed", color="grey40") +
  # Plot risk curve
  geom_line(mapping=aes(x=intake, y=risk)) +
  # Print calculation
  annotate(geom="text", x=max(data1$intake), y=1, hjust=1, vjust=1, size= 2.0,
           label=paste0(round(sev1, 1), "%\ninadequate"), color="black") +
  # Labels
  labs(x="Intake",
       y="Density (risk of inaqeuate intake)", tag="A",
       title="Normal intake distribution") +
  # scale_y_continuous(sec.axis = sec_axis(~ ., name = "Risk")) +
  # Theme
  theme_bw() + my_theme
g1

# Simulations
################################

# Simulate data
sim1 <- sim_data(n=5000, ear=ear_mu1, cv=ear_cv1, mean=intake_mu1, sd=intake_sd1)
sim1_sev <- sum(sim1$adequate_yn=="Inadequate") / nrow(sim1)

# Plot simulations
g2 <- ggplot(sim1, aes(x=intake, y=req, color=adequate_yn)) +
  geom_point( alpha=0.8, size=0.8) +
  # Reference lines
  geom_abline(slope=1) +
  geom_vline(xintercept=ear_mu1, linetype="dashed", color="grey40") +
  geom_hline(yintercept=ear_mu1, linetype="dashed", color="grey40") +
  # Print calculation
  annotate(geom="text", x=0, y=max(sim1$req), hjust=0, vjust=1, size= 2.0,
           label=paste0(round(sim1_sev*100, 1), "%\ninadequate"), color="black") +
  # Limit
  lims(x=c(0,NA), y=c(0, NA)) +
  # Labels
  labs(x="Intake", y="Requirement", tag="B") +
  # Legend
  scale_color_manual(name="", values=c("lightblue", "coral")) +
  # Theme
  theme_bw() + my_theme
g2


# Example 2: gamma distributions
################################################################################

ln_median <- intake_mu1
ln_meanlog <- log(ln_median) # median = exp(meanlog)

# CV = sqrt(10)
ln_sdlog <- sqrt(log(1+intake_cv1^2))

# Calculate SEV
sev2 <- nutriR::sev(ear=ear_mu1, cv=ear_cv1, meanlog=ln_meanlog, sdlog=ln_sdlog, plot=T)

# Build data
x <- seq(0,4000,1)
yintake <- dlnorm(x=x, meanlog=ln_meanlog , sdlog=ln_sdlog)
yrisk <- 1 - pnorm(x, mean=ear_mu1, sd=ear_sd1)
data2 <- tibble(intake=x,
                density=yintake,
                risk=yrisk) %>%
  mutate(density_scaled=density/max(density),
         density_scaled2=pmin(density_scaled, risk))

# Plot dataa
g3 <- ggplot(data2) +
  # Plot intake distribution
  geom_ribbon(mapping=aes(x=intake, ymin=0, ymax=density_scaled), fill="lightblue") +
  geom_ribbon(mapping=aes(x=intake, ymin=0, ymax=density_scaled2), fill="coral") +
  # Plot EAR
  geom_vline(xintercept = ear_mu1, linetype="dashed", color="grey40") +
  # Plot risk curve
  geom_line(mapping=aes(x=intake, y=risk)) +
  # Print calculation
  annotate(geom="text", x=max(data2$intake), y=1, hjust=1, vjust=1, size= 2.0,
           label=paste0(round(sev2, 1), "%\ninadequate"), color="black") +
  # Labels
  labs(x="Intake",
       y="Density (risk of inaqeuate intake)", tag="C",
       title="Log-normal intake distribution") +
  # scale_y_continuous(sec.axis = sec_axis(~ ., name = "Risk")) +
  # Theme
  theme_bw() + my_theme
g3

# Simulations
################################

# Simulate data
sim2 <- sim_data_ln(n=5000, ear=ear_mu1, cv=ear_cv1, meanlog=ln_meanlog, sdlog=ln_sdlog)
sim2_sev <- sum(sim2$adequate_yn=="Inadequate") / nrow(sim2)

# Plot simulations
g4 <- ggplot(sim2, aes(x=intake, y=req, color=adequate_yn)) +
  geom_point( alpha=0.8, size=0.8) +
  # Reference lines
  geom_abline(slope=1) +
  geom_vline(xintercept=ear_mu1, linetype="dashed", color="grey40") +
  geom_hline(yintercept=ear_mu1, linetype="dashed", color="grey40") +
  # Print calculation
  annotate(geom="text", x=0, y=max(sim2$req), hjust=0, vjust=1, size= 2.0,
           label=paste0(round(sim2_sev*100, 1), "%\ninadequate"), color="black") +
  # Limit
  lims(x=c(0,NA), y=c(0, NA)) +
  # Labels
  labs(x="Intake", y="Requirement", tag="D") +
  # Legend
  scale_color_manual(name="", values=c("lightblue", "coral")) +
  # Theme
  theme_bw() + my_theme
g4


# Example 3: equivalent distributions
################################################################################

# Example 3
ear_mu3 <- 1200
ear_sd3 <- 200
ear_cv3 <- ear_sd3 / ear_mu3
intake_mu3 <- 1200
intake_sd3 <- 200

# Calculcate SEV empircally
sev3 <- nutriR::sev(ear=ear_mu3, cv=ear_cv3, mean=intake_mu3, sd=intake_sd3, plot=T)

# Build data
x <- seq(0,3000,1)
yintake <- dnorm(x=x, mean=intake_mu3, sd=intake_sd3)
yrisk <- 1 - pnorm(x, mean=ear_mu3, sd=ear_sd3)
data3 <- tibble(intake=x,
                density=yintake,
                risk=yrisk) %>%
  mutate(density_scaled=density/max(density),
         density_scaled2=pmin(density_scaled, risk))

# Plot dataa
g5 <- ggplot(data3) +
  # Plot intake distribution
  geom_ribbon(mapping=aes(x=intake, ymin=0, ymax=density_scaled), fill="lightblue") +
  geom_ribbon(mapping=aes(x=intake, ymin=0, ymax=density_scaled2), fill="coral") +
  # Plot EAR
  geom_vline(xintercept = ear_mu1, linetype="dashed", color="grey40") +
  # Plot risk curve
  geom_line(mapping=aes(x=intake, y=risk)) +
  # Print calculation
  annotate(geom="text", x=max(data3$intake), y=1, hjust=1, vjust=1, size= 2.0,
           label=paste0(round(sev3, 1), "%\ninadequate"), color="black") +
  # Labels
  labs(x="Intake",
       y="Density (risk of inaqeuate intake)", tag="E",
       title="Identical intake/requirement dist.") +
  # scale_y_continuous(sec.axis = sec_axis(~ ., name = "Risk")) +
  # Theme
  theme_bw() + my_theme
g5


# Simulations
################################

# Simulate data
sim3 <- sim_data(n=5000, ear=ear_mu3, cv=ear_cv3, mean=intake_mu3, sd=intake_sd3)
sim3_sev <- sum(sim3$adequate_yn=="Inadequate") / nrow(sim1)

# Plot simulations
g6 <- ggplot(sim3, aes(x=intake, y=req, color=adequate_yn)) +
  geom_point( alpha=0.8, size=0.8) +
  # Reference lines
  geom_abline(slope=1) +
  geom_vline(xintercept=ear_mu3, linetype="dashed", color="grey40") +
  geom_hline(yintercept=ear_mu3, linetype="dashed", color="grey40") +
  # Print calculation
  annotate(geom="text", x=0, y=max(sim3$req), hjust=0, vjust=1, size= 2.0,
           label=paste0(round(sim3_sev*100, 1), "%\ninadequate"), color="black") +
  # Limit
  lims(x=c(0,NA), y=c(0, NA)) +
  # Labels
  labs(x="Intake", y="Requirement", tag="F") +
  # Legend
  scale_color_manual(name="", values=c("lightblue", "coral")) +
  # Theme
  theme_bw() + my_theme
g6


# Merge
################################################################################

# Merge data
layout_matrix <- matrix(data=c(1,3,5,
                               2,4,6), nrow=2,byrow=T)
g <- gridExtra::grid.arrange(g1, g2, g3, g4, g5, g6, layout_matrix=layout_matrix)
g

# Export figutre
ggsave(g, filename="response/Fig1_calc_demo.png",
       width=6.5, height=4.5, units="in", dpi=600)


