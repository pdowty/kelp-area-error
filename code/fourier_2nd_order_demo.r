# Install packages if you haven't already
# install.packages(c("ggplot2", "dplyr"))

library(ggplot2)
library(dplyr)

# 1. Define the time domain and parameters
T_period <- 365                       # Total days in the annual cycle
t_days   <- 1:365                      # Time domain (X-axis)

# Fourier Coefficients
B0 <- 20                              # Baseline annual biomass
a1 <- -15; b1 <- 10                   # 1st Harmonic (Annual cycle)
a2 <- -5;  b2 <- -5                   # 2nd Harmonic (Semi-annual asymmetry)

# 2. Calculate the values of B (Y-axis) using the Fourier Series
kelp_data <- tibble(Day = t_days) %>%
  mutate(
    # First Harmonic (n = 1)
    harmonic_1 = a1 * cos(2 * pi * 1 * Day / T_period) + b1 * sin(2 * pi * 1 * Day / T_period),
    
    # Second Harmonic (n = 2)
    harmonic_2 = a2 * cos(2 * pi * 2 * Day / T_period) + b2 * sin(2 * pi * 2 * Day / T_period),
    
    # Combined Biomass Model
    Biomass = B0 + harmonic_1 + harmonic_2
  )

# 3. Create the time series graph using ggplot2
p <- ggplot(kelp_data, aes(x = Day, y = Biomass)) +
  geom_line(color = "darkgreen", size = 1.2) +
  # Optional: Shading underneath to represent biomass volume
  geom_area(fill = "darkgreen", alpha = 0.1) + 
  scale_x_continuous(
    breaks = seq(1, 365, by = 30), 
    labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan")
  ) +
  labs(
    title = "Simulated Annual Kelp Canopy Phenology",
    subtitle = "2nd-Order Multi-Harmonic Fourier Series Model",
    x = "Month of the Year (Day of Year)",
    y = "Relative Kelp Biomass / Canopy Area (B)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(color = "darkgray", hjust = 0.5),
    panel.grid.minor = element_blank()
  )
