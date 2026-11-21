###############################################################################
#
# Appendix fig showing error dependence on kelp area.
# Appears prior to demo 2.
#
###############################################################################


library(ggplot2)
library(dplyr)

N <- 500
k <- c(0.1,0.3)
Amax <- 20
Amin <- 0.5

df <- data.frame(
  k = rep(k, each=N), 
  xA = runif(2*N, min=Amin, max=Amax) 
)

k_labels = c("0.1" = "k = 0.1", "0.3" = "k = 0.3")

df <- df %>%
  mutate(
    k_fct = factor(k, levels=c(0.1, 0.3)),
    err_stdev = k*xA,
    z_val = rnorm(2*N),
    error = err_stdev*z_val,
    Ahat = xA + error
  )

p <- ggplot(data = df,
            mapping = aes(x=xA, y=error)) +
     geom_point(alpha=0.2) +
     facet_grid(rows=vars(k_fct),
                labeller=labeller(k_fct=k_labels)) +
     theme_bw() +
     theme(
      axis.title.x = element_text(margin=margin(t=15)),
      axis.title.y = element_text(margin=margin(r=15))
     ) +
     scale_x_continuous(name="True Kelp Area (ha)") +
     scale_y_continuous(name="Error (ha)")

p2 <- ggplot(data = df,
            mapping = aes(x=xA, y=Ahat)) +
     geom_point(alpha=0.2) +
     facet_grid(rows=vars(k_fct),
                labeller=labeller(k_fct=k_labels)) +
     theme_bw() +
     theme(
      axis.title.x = element_text(margin=margin(t=15)),
      axis.title.y = element_text(margin=margin(r=15))
     ) +
     scale_x_continuous(name="True Kelp Area (ha)") +
     scale_y_continuous(name="Kelp Area Estimate (ha)")





