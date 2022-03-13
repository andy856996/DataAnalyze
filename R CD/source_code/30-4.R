# ---------
#  30-4
# ---------
# ---------------------------------------------------------- #

library(testthat)
library(ExamplePackage)

test_check("ExamplePackage")

# ---------------------------------------------------------- #

use_test('simpleEx')

# ---------------------------------------------------------- #

library(testthat)

test_that('Correct Answer', {
    expect_equal(simpleEx(2, 3), 6)
    expect_equal(simpleEx(5, 4), 20)
    expect_equal(simpleEx(c(1, 2, 3), 3), c(3, 6, 9))
    expect_equal(simpleEx(c(1, 2, 3), c(2, 4, 6)), c(2, 8, 18))
    })

test_that('Correct Type', {
    expect_is(simpleEx(2, 3), 'numeric')
    expect_is(simpleEx(2L, 3L), 'integer')
    expect_is(simpleEx(c(1, 2, 3), c(2, 4, 6)), 'numeric')
    })

test_that('Correct length', {
    expect_length(simpleEx(2, 3), 1)
    expect_length(simpleEx(c(1, 2, 3), 3), 3)
    expect_length(simpleEx(c(1, 2, 3), c(2, 4, 6)), 3)
    })


# ---------------------------------------------------------- #

test_that('Appropriate error or warning', {
  expect_error(simpleEx(3, 'A'))
  expect_equal(simpleEx(1:3, 1:2), c(1, 4, 3))
  expect_warning(simpleEx(1:3, 1:2))
  })


# ---------------------------------------------------------- #

apropos('expect_')

# ---------------------------------------------------------- #

devtools::test()

# ---------------------------------------------------------- #
