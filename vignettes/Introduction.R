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


## ------------------------------------------------------------------------
data(iris)
# scale numeric variables
res = alter(for(i in iris) if(is.numeric(i)) scale(i))
str(res)

# convert factors to characters
res = alter(for(i in iris) if(is.factor(i)) as.character(i))
str(res)

# drop factors
res = alter(for(i in iris) if(is.factor(i)) exclude())
str(res)

# 'data' argument example
# specify which columns to map with a numeric vector of positions:
res = alter(
    for(`i, value` in numerate(mtcars)) if(i %in% c(1, 4, 5)) as.character(value),
    data = mtcars
)
str(res)

# or with a vector of names:
res = alter(
    for(`name, value` in mark(mtcars)) if(name %in% c("cyl", "am")) as.character(value),
    data = mtcars
)
str(res)

