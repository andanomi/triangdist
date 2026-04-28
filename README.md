
# triangdist

The `triangdist` package provides the density, distribution function, quantile function, and random generation for the Triangular Distribution in R.

## Installation

You can install the development version of triangdist from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("andanomi/triangdist")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(triangdist)

# Define a scenario: Values between 0 and 10, with the peak (mode) at 5.
lower_limit <- 0
upper_limit <- 10
peak <- 5

# 1. Generate 5 random values following this distribution
rtriang(n = 5, min = lower_limit, max = upper_limit, mode = peak)

# 2. Compute the exact density at the peak
dtriang(x = peak, min = lower_limit, max = upper_limit, mode = peak)

# 3. Compute the cumulative probability up to the peak (should be 0.5 due to symmetry)
ptriang(q = peak, min = lower_limit, max = upper_limit, mode = peak)

```

