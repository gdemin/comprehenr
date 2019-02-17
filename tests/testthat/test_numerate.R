context("numerate")

a = list(4, 5, 6)

expect_equal(numerate(a),
             list(
                 list(1, 4),
                 list(2, 5),
                 list(3, 6)
             ))


expect_equal(unnumerate(numerate(a)),
            a
             )

expect_equal(unnumerate(list(
    list(1,2,3),
    list(4,5,6)
), item = 3),
list(3, 6)
)

context("mark")
a = list(a = 1, b = 2, c = 3)
expect_equal(mark(
    a
),
list(
    list("a", a = 1),
    list("b", b = 2),
    list("c", c = 3)
)
)


expect_equal(mark(
    unname(a)
),
list(
    list("", 1),
    list("", 2),
    list("", 3)
)
)

expect_equal(unmark(mark(
    a
)),
    a
)

context("unzip_list")
b = list(
    list(1, 4),
    list(2, 5),
    list(3, 6)
)

expect_equal(
    unzip_list(b),
    list(
        list(1,2,3),
        list(4,5,6)
    )

)


expect_equal(
    unzip_list(zip_lists(b[[1]], b[[2]], b[[3]])),
    b
)

a = list(
    a1 = list(1,2,3),
    a2 = list(4,5,6)
)
expect_equal(
    unzip_list(a),
    list(
        list(a1 = 1, a2 = 4),
        list(a1 = 2, a2 = 5),
        list(a1 = 3, a2 = 6)
    )
)


a = list(
    d1 = list(a1 = 1, a2 = 2, a3 = 3),
    d2 = list(b1 = 4, b2 = 5, b3 = 6)
)
expect_equal(
    unzip_list(a),
    list(
        list(a1 = 1, b1 = 4),
        list(a2 = 2, b2 = 5),
        list(a3 = 3, b3 = 6)
    )
)

expect_equal(
    unzip_list(unzip_list(a)),
    list(
        list(a1 = 1, a2 = 2, a3 = 3),
        list(b1 = 4, b2 = 5, b3 = 6)
    )
)




expect_equal(
    unzip_list(list()),
    list()
)

expect_error(
    unzip_list(
        list(
            list(a1 = 1, a2 = 2, a3 = 3),
            list(b1 = 4, b2 = 5)
        )
    )
)
