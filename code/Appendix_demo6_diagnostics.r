# Diagnostics to help explain the reuslts from the Appendix 6 noncentral
# force error demo - i.e. the decreasing A1-A2hat correlation with declining
# A2.

set.seed(11)
N <- 1000


# function generates random variates from normal distr. truncated to neg. values
get_neg_z <- function(n) {
  x <- rnorm(2.1*n)
  neg_x <- x[x<=0][1:n]
  if (anyNA(neg_x)) {
    print("error with neg z")
  } 
  return(neg_x) 
}




### 1
z1 <- rnorm(N)
z2 <- rnorm(N)

cat(sprintf("1: z1-z2  %f\n",cor(z1,z2)))

### 2  z2 -> k * z2
k <- 10
z2 <- k * z2

cat(sprintf("2: k*z2   %f\n",cor(z1,z2)))

### 3
z1 <- rnorm(N)
z2 <- rnorm(N)

A1 <- z1 + z2
A2 <- z1 + z2

cat(sprintf("3: z1+z2   %f\n",cor(A1,A2)))

### 4
A2 <- k * A2

cat(sprintf("4: k*(z1+z2)   %f\n",cor(A1,A2)))

#############################################################

### 5
z1 <- rnorm(N)
z2 <- rnorm(N)

k1 <- 0.1
k2 <- 0.1
A1 <- 10
A2 <- 4

A1est <- A1 + k1*A1*z1 + k1*A1*z2
A2est <- A2 + k2*A2*z1 + k2*A2*z2

cat(sprintf("\n5:  kelp-z-z  %f\n",cor(A1est,A2est)))

### 6
z1 <- rnorm(N)
z2 <- rnorm(N)
tz <- get_neg_z(N)
k <- 0.1

A1est <- A1 + k*A1*tz + k*A1*tz + k*A1*z1
A2est <- A2 + k*A2*tz + k*A2*tz + k*A2*z1

cat(sprintf("6: kelp t-t-z %f\n",cor(A1est,A2est)))








