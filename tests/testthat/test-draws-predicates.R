predicate_cases <- list(
    na = list(predicate = is.na,
              inputs = list(
                  rbind(c(1, NA, NaN), c(NA, NaN, NA), c(Inf, -Inf, 2), c(0, 1, 2)),
                  rbind(c(1L, NA_integer_, 2L), rep(NA_integer_, 3), 1:3),
                  rbind(c(TRUE, NA, FALSE), rep(NA, 3), c(FALSE, TRUE, FALSE)),
                  rbind(c("NA", NA, "NaN"), rep(NA_character_, 3), c("Inf", "a", "")))),
    infinite = list(predicate = is.infinite),
    finite = list(predicate = is.finite)
)
predicate_cases$infinite$inputs <- predicate_cases$na$inputs[1:3]
predicate_cases$finite$inputs <- predicate_cases$na$inputs[1:3]

for (kind in names(predicate_cases)) {
    for (mode in c("any", "all")) {
        local({
            kind <- kind
            mode <- mode
            fun_name <- paste("draws", mode, kind, sep = "_")
            fun <- get(fun_name)
            predicate <- predicate_cases[[kind]]$predicate
            summary <- get(mode, envir = baseenv())
            test_that(paste(fun_name, "summarises each element and preserves names"), {
                for (m in predicate_cases[[kind]]$inputs) {
                    rownames(m) <- paste0("element", seq_len(nrow(m)))
                    x <- rvec(m)
                    before <- serialize(x, NULL)
                    expected <- vapply(seq_len(nrow(m)), function(i)
                        summary(predicate(m[i, ])), logical(1))
                    names(expected) <- rownames(m)
                    expect_identical(fun(x), expected)
                    expect_false(anyNA(fun(x)))
                    expect_identical(fun(x), get(paste0("draws_", mode))(predicate(x)))
                    expect_identical(serialize(x, NULL), before)
                    # One element and one draw exercise both singleton dimensions.
                    expect_identical(fun(x[1]), expected[1])
                    single <- rvec(m[, 1, drop = FALSE])
                    expect_identical(fun(single), predicate(m[, 1]))
                    rownames(m) <- NULL
                    expect_identical(fun(rvec(m)), unname(expected))
                    empty <- rvec(m[integer(), , drop = FALSE])
                    expect_identical(fun(empty), logical())
                }
            })
            if (kind != "na") {
                test_that(paste(fun_name, "rejects character rvecs"), {
                    expect_error(fun(rvec(c("1", NA_character_))),
                                 "not defined for character rvecs")
                    expect_error(fun(rvec(matrix(character(), 0, 3))),
                                 "not defined for character rvecs")
                })
            }
        })
    }
}

test_that("draw predicates distinguish missing, infinite, and finite draws", {
    x <- rvec(rbind(c(NA, NaN), c(Inf, -Inf), c(1, 2), c(NA, 1), c(Inf, NA)))
    expect_identical(draws_any_na(x), c(TRUE, FALSE, FALSE, TRUE, TRUE))
    expect_identical(draws_all_na(x), c(TRUE, FALSE, FALSE, FALSE, FALSE))
    expect_identical(draws_any_infinite(x), c(FALSE, TRUE, FALSE, FALSE, TRUE))
    expect_identical(draws_all_infinite(x), c(FALSE, TRUE, FALSE, FALSE, FALSE))
    expect_identical(draws_any_finite(x), c(FALSE, FALSE, TRUE, TRUE, FALSE))
    expect_identical(draws_all_finite(x), c(FALSE, FALSE, TRUE, FALSE, FALSE))
    expect_false(identical(draws_all_finite(x), !draws_any_infinite(x)))
})

test_that("draw predicates support ordinary S3 subclass dispatch", {
    x <- rvec(1)
    class(x) <- c("predicate_test", class(x))
    method <- function(x) "custom summary"
    vctrs::s3_register("rvec::draws_any_na", "predicate_test", method)
    withr::defer(rm("draws_any_na.predicate_test",
                   envir = asNamespace("rvec")$.__S3MethodsTable__.))
    expect_identical(draws_any_na(x), "custom summary")
})
