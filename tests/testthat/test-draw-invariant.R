## Rvecs may have zero rows, but never zero columns, even as prototypes.

test_that("all constructor boundaries reject zero draws", {
    types <- c("chr", "dbl", "int", "lgl")
    values <- list("a", 1, 1L, TRUE)
    for (i in seq_along(types)) {
        public <- get(paste0("rvec_", types[i]))
        internal <- get(paste0(".new_rvec_", types[i]))
        fill <- get(paste0("new_rvec_", types[i]))
        for (n in c(0L, 2L)) {
            m <- matrix(values[[i]], nrow = n, ncol = 0L)
            expect_error(rvec(m), "must have at least one column")
            expect_error(public(m), "must have at least one column")
            expect_error(internal(m), "must have at least one column")
            expect_error(fill(length = n, n_draw = 0), "n_draw.*equals 0")
            expect_error(.new_rvec(types[i], length = n, n_draw = 0),
                         "n_draw.*equals 0")
        }
        expect_error(fill(n_draw = 0.5), "non-integer")
        expect_error(ptype_rvec(0, values[[i]][FALSE]), "at least one column")
    }
    withr::local_options(lifecycle_verbosity = "quiet")
    expect_error(new_rvec(n_draw = 0), "n_draw.*equals 0")
})


test_that("restoration rejects zero-draw proxies for every rvec type", {
    for (value in list("a", 1, 1L, TRUE)) {
        for (n in c(0L, 2L)) {
            x <- rvec(matrix(value, nrow = n, ncol = 3L))
            proxy <- vctrs::vec_proxy(x)
            proxy$data <- matrix(value, nrow = n, ncol = 0L)
            expect_error(vctrs::vec_restore(proxy, x), "at least one column")
        }
    }
})


test_that("empty vectors and prototypes retain at least one draw", {
    for (value in list("a", 1, 1L, TRUE)) {
        for (nd in c(1L, 3L)) {
            m <- matrix(value, nrow = 2L, ncol = nd,
                        dimnames = list(c("a", "b"), NULL))
            x <- rvec(m)
            expect_identical(vctrs::vec_restore(vctrs::vec_proxy(x), x), x)
            empty <- x[integer()]
            results <- list(empty, rep(x, 0), vctrs::vec_slice(x, integer()),
                            vctrs::vec_ptype(x), vctrs::vec_ptype2(x, x),
                            vctrs::vec_init(x, 0), c(empty, empty),
                            vctrs::vec_restore(vctrs::vec_proxy(empty), x),
                            vctrs::vec_cast(empty, x), rvec_chr(empty),
                            ptype_rvec(nd, value[FALSE]))
            for (result in results) {
                expect_s3_class(result, "rvec")
                expect_identical(length(result), 0L)
                expect_identical(n_draw(result), nd)
            }
            expect_identical(n_draw(c(empty, x)), nd)
            if (is.numeric(value)) {
                expect_identical(n_draw(empty + 1), nd)
                expect_identical(length(empty + 1), 0L)
            }
        }
    }
})


test_that("internal coercion cannot create zero draws", {
    for (type in c("chr", "dbl", "int", "lgl")) {
        from_atomic <- get(paste0("atomic_to_rvec_", type))
        from_rvec <- get(paste0("rvec_to_rvec_", type))
        for (n in c(0L, 2L)) {
            values <- rep(TRUE, n)
            x <- rvec(matrix(values, nrow = n, ncol = 1L))
            ## Base matrix() may warn before the low-level constructor rejects it.
            expect_error(suppressWarnings(from_atomic(values, n_draw = 0)),
                         "at least one column")
            expect_error(suppressWarnings(from_rvec(x, n_draw = 0)),
                         "at least one column")
        }
    }
})
