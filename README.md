# List comprehensions in R

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/comprehenr)](https://cran.r-project.org/package=comprehenr)
[![](https://cranlogs.r-pkg.org/badges/comprehenr)](https://cran.rstudio.com/web/packages/comprehenr/index.html)
[![](https://cranlogs.r-pkg.org/badges/grand-total/comprehenr)](https://cran.rstudio.com/web/packages/comprehenr/index.html)
[![Coverage Status](https://img.shields.io/codecov/c/github/gdemin/comprehenr/master.svg)](https://codecov.io/github/gdemin/comprehenr?branch=master)

Package provides Python-style list comprehensions for R. List comprehension
expressions use usual loops (`for`, `while` and `repeat`) and usual `if` as
list producers. Syntax is very similar to Python. The difference is that
returned value should be at the end of the loop body.

There are three main functions:

- `to_list` converts usual R loops expressions to list producers. Expression should be started with `for`, `while` or `repeat`. You can iterate over multiple lists if you provide several loop variables in backticks. See examples.
- `to_vec` is the same as `to_list` but return vector. See examples.
- `alter` return the same type as its argument but with modified elements. It is useful for altering existing data.frames or lists. See examples.

Rather unpractical example - squares of even numbers:
```R
library(comprehenr)
to_vec(for(i in 1:10) if(i %% 2==0) i*i)
```
Pythagorean triples:
```R
to_list(for (x in 1:20) for (y in x:20) for (z in y:20) if (x^2 + y^2 == z^2) c(x, y, z))
```
More examples:
```R
colours = c("red", "green", "yellow", "blue")
things = c("house", "car", "tree")
to_vec(for(x in colours) for(y in things) paste(x, y))

# prime numbers
noprimes = to_vec(for (i in 2:7) for (j in seq(i*2, 99, i)) j)
primes = to_vec(for (x in 2:99) if(!x %in% noprimes) x)
primes
```
You can iterate over multiple lists if you provide several loop variables in backticks:
```R
to_vec(for(`i, j` in numerate(letters)) if(i %% 2==0) paste(i, j))

set.seed(123)
rand_sequence = runif(20)
# gives only locally increasing values
to_vec(for(`i, j` in lag_list(rand_sequence)) if(j>i) j)

```

`alter` examples:
```R
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
```
