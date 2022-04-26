
<!-- README.md is generated from README.Rmd. Please edit that file -->

# hhsim

<!-- badges: start -->

<!-- badges: end -->

The goal of hhsim is to simulate household dynamics by allowing
individuals to appear, change households or disappear from the observed
population.

## Installation

You can install the development version of hhsim from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Jean-Rubin/hhsim")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(hhsim)
#> 
#> Attaching package: 'hhsim'
#> The following object is masked from 'package:base':
#> 
#>     remove
## basic example code

hh_gen <- hh_geometric(2L)
population_init <- generate_population(n = 10L, hh_gen = hh_gen)
population_init |>
  introduce(n = 5L, hh_gen = hh_gen) |>
  evolve(prop = 0.2) |>
  remove(n = 5L)
#> $pop
#> # A tibble: 10 × 2
#>    ind   hh   
#>    <fct> <fct>
#>  1 1     2    
#>  2 3     3    
#>  3 5     10   
#>  4 6     5    
#>  5 7     5    
#>  6 9     10   
#>  7 10    9    
#>  8 11    9    
#>  9 12    10   
#> 10 13    10   
#> 
#> $ind_max
#> [1] 15
#> 
#> $hh_max
#> [1] 18
#> 
#> attr(,"class")
#> [1] "population"
```
