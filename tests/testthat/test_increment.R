context("increment/decrement")

x = 5

+x = 2
expect_identical(x, 7)

-x = 3
expect_identical(x, 4)
