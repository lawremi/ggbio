setClassUnion("list_OR_NULL", c("list", "NULL"))
setClass("TestDetails", representation(
    with_parameter_name = "character_OR_NULL",
    with_parameter_values = "list_OR_NULL",
    module_name = "character",
    generic_name = "character",
    test_func = "function",
    expected_func = "function",
    other_parameters = "list",
    seed_values = "list"
))

TestDetails <- function(with_name = NULL, with_values = NULL, module_name, generic_name,
                        test_func, expected_func, other_parameters, seed_values) {
    # checks
    stopifnot(is.character(with_name) || is.null(with_name))
    stopifnot(is.list(with_values) || is.null(with_name))
    stopifnot(is.character(module_name))
    stopifnot(is.character(generic_name))
    stopifnot(is.function(test_func))
    stopifnot(is.function(expected_func))
    stopifnot(is.list(other_parameters))
    stopifnot(is.list(seed_values))
    td <- new("TestDetails", with_parameter_name = with_name,
                             with_parameter_values = with_values,
                             module_name = module_name,
                             generic_name = generic_name,
                             test_func = test_func,
                             expected_func = expected_func,
                             other_parameters = other_parameters,
                             seed_values = seed_values)
    td
}

test_generator <- function(td) {
    # check
    stopifnot(is(td, "TestDetails"))

    # mapping
    with_name <- td@with_parameter_name
    with_values <- td@with_parameter_values
    module_name <- td@module_name
    generic_name <- td@generic_name
    test_func <- td@test_func
    expected_func <- td@expected_func
    others <- td@other_parameters
    seed_values <- td@seed_values

    # generator
    if (!is.null(with_name) && !is.null(with_values)) {
        # walk through with_values and append "with_name = value" to the seed_values
        for (value in with_values) {
            position <- 1L
            for (parameter in others) {
                test_msg <- paste0("Test ", parameter, " parameter with ", with_name, " = '",
                                    value, "' of ", module_name ," (", generic_name , ")")
                test_that(test_msg, {
                    args <- seed_values[[position]]
                    args[with_name] <- value
                    test <- do.call(test_func, args)
                    expected <- do.call(expected_func, args)
                    expect_equal(test, expected)
                })
                position <- position + 1L
            }
        }
    } else {
        position <- 1L
        for (parameter in others) {
            test_msg <- paste0("Test ", parameter, " parameter of ",
                               module_name ," (", generic_name , ")")
            test_that(test_msg, {
                args <- seed_values[[position]]
                test <- do.call(test_func, args)
                expected <- do.call(expected_func, args)
                expect_equal(test, expected)
            })
            position <- position + 1L
        }
    }
}

function_wrapper <- function(FUNC, data) {
    function(...) {
        FUNC(data, ...)
    }
}
