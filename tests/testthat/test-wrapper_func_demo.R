context("test for wrapper_func_demo.R")

test_that("wrapper_funcs works appropriately with fully specified parameters", {
  
  expect_equal(
    wrapper_funcs(some_arg = 1, x = 2, y = 3, a = 4, b = 5), 
    1 - ( 2 + 3 ) + ( 4 * 5 )
  )
  
})

test_that("wrapper_funcs works appropriately with partially specified parameters", {
  
  expect_equal(
    wrapper_funcs(some_arg = 1, x = 2, y = 3, a = 4), 
    1 - ( 2 + 3 ) + ( 4 * 2 )
  )
  
})

