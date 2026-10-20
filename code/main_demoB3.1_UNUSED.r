###############################################################################
#  Demo in Appendix B3 - as of 10/20/2026 this is unused within draft report
#
#  This function takes a value of k and a data frame with variable A.
#  A=kelp area values; k = constant to relate area to stdev of source errors:
#     error_std_dev = k * A
#
#  This function returns a table with correlation results.
#
#  October 2026
###############################################################################

library(dplyr)
library(ggplot2)
library(rstatix)


# proportional constant relating A to standard deviation of source errors 
k <- 0.10

#  generate A vector on scale meaningful for hectare units
df <- data.frame(A = seq(0, 20, by=1.0))
nrows <- dim(df)[1]

# generate 4 error vectors as function of A
df <- df %>%
  mutate(
    e1 = (k*A) * rnorm(nrows),
    e2 = (k*A) * rnorm(nrows),
    e3 = (k*A) * rnorm(nrows),
    e4 = (k*A) * rnorm(nrows),
  )

corr.out <- df %>% cor_test(e1, e2, e3, e4)
corr.out <- corr.out %>%
  filter(cor != 1,
         !(var1=="e2" & var2=="e1"),
         !(var1=="e3" & var2=="e1"),
         !(var1=="e3" & var2=="e2"),
         var1 != "e4")

# graph of specified error (y-axis) across range of A values (x-axis)
p <- ggplot(data = df,
            mapping = aes(x=A, y=e1)) +
     geom_point(size=0.5, stroke=0, fill="gray30", alpha=0.2) +
     theme_bw()

# custom pivot longer
df_1_2 <- data.frame(
  A = df$A,
  group = rep("e1-e2", times=nrows),
  var1 = df$e1,
  var2 = df$e2
)
df_1_3 <- data.frame(
  A = df$A,
  group = rep("e1-e3", times=nrows),
  var1 = df$e1,
  var2 = df$e3
)
df_1_4 <- data.frame(
  A = df$A,
  group = rep("e1-e4", times=nrows),
  var1 = df$e1,
  var2 = df$e4
)
df_2_3 <- data.frame(
  A = df$A,
  group = rep("e2-e3", times=nrows),
  var1 = df$e2,
  var2 = df$e3
)
df_2_4 <- data.frame(
  A = df$A,
  group = rep("e2-e4", times=nrows),
  var1 = df$e2,
  var2 = df$e4
)
df_3_4 <- data.frame(
  A = df$A,
  group = rep("e3-e4", times=nrows),
  var1 = df$e3,
  var2 = df$e4
)
df_long <- rbind(df_1_2, df_1_3, df_1_4, df_2_3, df_2_4, df_3_4)


# make error1-error2 scatter plots with error pairs in panels
pscatter <- ggplot(data=df_long,
                   mapping=aes(x=var1, y=var2)) +
     geom_point(size=0.5, stroke=0, fill="gray30", alpha=0.2) +
     facet_wrap(~ group) +
     theme_bw()





