context("to_list")

expect_identical(
    to_list(for(i in 1:10) if(i %% 2 !=0) i),
    list(1L,3L,5L,7L,9L)
)

expect_identical(
    to_list(for(i in 1:10) if(i>1000) i),
    list()
)

expect_false(exists(".___res"))
expect_false(exists(".___counter"))
expect_false(exists(".__curr"))

expect_error(to_list(for(i in 1:10) i*brrrr))
expect_false(exists(".___res"))
expect_false(exists(".___counter"))
expect_false(exists(".__curr"))


expect_error(to_list(1))
expect_false(exists(".___res"))
expect_false(exists(".___counter"))
expect_false(exists(".__curr"))
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
        +i = 2
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
