## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
library(comprehenr)
to_vec(for(i in 1:10) if(i %% 2==0) i*i)

## ------------------------------------------------------------------------
to_list(for (x in 1:20) for (y in x:20) for (z in y:20) if (x^2 + y^2 == z^2) c(x, y, z))

## ------------------------------------------------------------------------
colours = c("red", "green", "yellow", "blue")
things = c("house", "car", "tree")
to_vec(for(x in colours) for(y in things) paste(x, y))

# prime numbers
noprimes = to_vec(for (i in 2:7) for (j in seq(i*2, 99, i)) j)
primes = to_vec(for (x in 2:99) if(!x %in% noprimes) x)
primes

## ------------------------------------------------------------------------
to_vec(for(`i, j` in numerate(letters)) if(i %% 2==0) paste(i, j))

set.seed(123)
rand_sequence = runif(20)
# gives only locally increasing values
to_vec(for(`i, j` in lag_list(rand_sequence)) if(j>i) j)


