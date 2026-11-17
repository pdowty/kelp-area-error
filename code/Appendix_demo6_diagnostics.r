# Diagnostics to help explain the reuslts from the Appendix 6 noncentral
# force error demo.

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
A1 <- rnorm(N)
A2 <- rnorm(N)

cat(sprintf("1:  %f\n",cor(A1,A2)))

### 2
k <- 10
A2 <- k * A2

cat(sprintf("2:  %f\n",cor(A1,A2)))

### 3
z1 <- rnorm(N)
z2 <- rnorm(N)

A1 <- z1 + z2
A2 <- z1 + z2


A1 <- z1 + z2
A2 <- z1 + z2

cat(sprintf("3:  %f\n",cor(A1,A2)))

### 4
A2 <- k * A2

cat(sprintf("4:  %f\n",cor(A1,A2)))

#############################################################

### 5
z1 <- rnorm(N)
z2 <- rnorm(N)

k1 <- 0.8
k2 <- 0.2

A1 <- 10 + k1*z1 + k1*z2
A2 <-  4 + k2*z1 + k2*z2

cat(sprintf("\n5:  %f\n",cor(A1,A2)))

### 6
tz1 <- get_neg_z(N)
tz2 <- get_neg_z(N)
A1 <- 10 + k1*tz1 + k1*tz2 + k1*z1
A2 <- 4 + k2*tz1 + k2*tz2 + k2*z2

cat(sprintf("6:  %f\n",cor(A1,A2)))








