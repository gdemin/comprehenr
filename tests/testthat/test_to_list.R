context("to_list")

expect_identical(
    to_list(for(i in 1:10) if(i %% 2 !=0) i),
    list(1L,3L,5L,7L,9L)
)

expect_identical(
    to_list(for(i in 1:10) if(i %% 2 !=0) i else exclude()),
    list(1L,3L,5L,7L,9L)
)

expect_identical(
    to_list(for(i in 1:10) if(i>1000) i),
    list()
)

expect_equal(
    to_list(for(i in 1:3) for(j in 1:3) if(i==j) c(i,j)),
    list(c(1,1), c(2,2), c(3,3))
)

expect_equal(
    to_list(for(i in 1:3) if(i==2) for(k in 1:2) k*2 else for(l in 3:4) l*3),
    list(9, 12, 2, 4, 9, 12)
)

expect_equal(
    to_list(for (x in 1:29) for (y in x:29) for (z in y:29) if (x^2 + y^2 == z^2) c(x, y, z)),
    list(c(3, 4, 5),
         c(5, 12, 13),
         c(6, 8, 10),
         c(7, 24, 25),
         c(8, 15, 17),
         c(9, 12, 15),
         c(10, 24, 26),
         c(12, 16, 20),
         c(15, 20, 25),
         c(20, 21, 29)
    )
)
expect_false(exists(".___res"))
expect_false(exists(".___counter"))
expect_false(exists(".___curr"))

expect_error(to_list(for(i in 1:10) i*brrrr))
expect_false(exists(".___res"))
expect_false(exists(".___counter"))
expect_false(exists(".___curr"))


expect_error(to_list(1))
expect_false(exists(".___res"))
expect_false(exists(".___counter"))
expect_false(exists(".___curr"))
a = 11
expect_error(to_list(a + 2))



context("to_vec")
expect_identical(
    to_vec(for(i in 1:10) if(i %% 2 !=0) i),
    c(1L,3L,5L,7L,9L)
)

expect_identical(
    to_vec(for(i in 1:10) if(i>1000) i),
    c()
)


expect_equal(
    to_vec(for(i in 1:10) i*i),
    (1:10)^2
)

a = 11
i = 0
expect_equal(
    to_vec(while(i<a){
        i = i + 2
        i*i

    }), c(2, 4, 6, 8, 10, 12)^2)


i = 0
expect_equal(
    to_vec(repeat{
        i = i + 2
        if(i>12) break
        i*i
    }), c(2, 4, 6, 8, 10, 12)^2)


data(iris)

expect_equal(
    to_vec(for(i in colnames(iris)) if(is.numeric(iris[[i]])) setNames(mean(iris[[i]]), i) ),
    unname(colMeans(iris[,-5]))
)
expect_equal(
    to_vec(for(i in colnames(iris)) if(is.numeric(iris[[i]])) setNames(mean(iris[[i]]), i) , use.names = TRUE),
    colMeans(iris[,-5])
)


context("`i, j`")

res = to_vec(for(`i, j` in zip_lists(letters, LETTERS)) paste(i, j))
expect_identical(res, paste(letters, LETTERS))


res = to_vec(for(`i, j` in numerate(letters)) if(i %% 2==0) paste(i, j))
index = seq_along(letters)
expect_identical(res, paste(index[index %% 2==0], letters[index %% 2==0]))


res = to_vec(for(`i, j` in numerate(letters)) if(i %% 2==0) for(`k, z` in numerate(LETTERS)) paste(i, j, k, z))
index = seq_along(letters)
true_res = expand.grid(paste(seq_along(letters), LETTERS), paste(index[index %% 2==0], letters[index %% 2==0]))
true_res = paste(true_res[[2]], true_res[[1]])
expect_identical(res, true_res)


context("alter")
data(iris)
iris2 = alter(for(i in iris) if(is.numeric(i)) scale(i))
res_iris = iris
res_iris[,-5] = lapply(iris[,-5], scale)
expect_equal(iris2, res_iris)

data(iris)
iris2 = alter(for(i in iris) if(is.factor(i)) exclude())
res_iris = iris[,-5]
expect_equal(iris2, res_iris)

iris2 = alter(for(`name, value` in mark(iris)) if(endsWith(name, "Width")) scale(value), data = iris)

res_iris = iris
res_iris[,c("Sepal.Width", "Petal.Width")] = lapply(iris[,c("Sepal.Width", "Petal.Width")], scale)
expect_equal(iris2, res_iris)

expect_error(alter(1))
# library(data.table)
# dt_iris = as.data.table(iris)
# dt_iris2 = alter(for(i in dt_iris) if(is.numeric(i)) scale(i))
