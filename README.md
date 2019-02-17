# List comprehensions in R

Package provides 'Python'-style list comprehensions for R. It uses usual R loops (`for`, `while` and `repeat`) and usual `if` as list producers. Syntax is very similar to Python. The difference is that returned value should be at the end of the loop body. 

```R
# rather useless statement - squares of even numbers
to_list(for(i in 1:10) if(i %% 2==0) i*i)

# Pythagorean triples
to_list(for (x in 1:20) for (y in x:20) for (z in y:20) if (x^2 + y^2 == z^2) c(x, y, z))

colours = c("red", "green", "yellow", "blue")
things = c("house", "car", "tree")
to_vec(for(x in colours) for(y in things) paste(x, y))

# prime numbers
noprimes = to_vec(for (i in 2:7) for (j in seq(i*2, 99, i)) j)
primes = to_vec(for (x in 2:99) if(!x %in% noprimes) x)
primes
```
