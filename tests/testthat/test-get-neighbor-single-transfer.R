context("Test get.neighbor.single.transfer()")


test_that("get.neighbor.single.transfer validates input correctly", {

  expect_error(get.neighbor.single.transfer(), 'Missing required argument')
  expect_error(get.neighbor.single.transfer(1), 'Invalid argument type')
  expect_error(get.neighbor.single.transfer('a'), 'Invalid argument type')
  expect_error(get.neighbor.single.transfer(c(1,2)), 'Invalid argument type')
  expect_error(get.neighbor.single.transfer(list()), 'Invalid argument length')
  expect_error(get.neighbor.single.transfer(list('')), 'Non-numeric argument') 
  expect_error(get.neighbor.single.transfer(list(c())), 'Non-numeric argument') 
  expect_error(get.neighbor.single.transfer(list(0)), 'Invalid argument') 
  expect_error(get.neighbor.single.transfer(list(-1)), 'Invalid argument') 
  
})


test_that("get.neighbor.single.transfer handle single instance case", {
	
    assignment <- get.initial.assignment(1,10)
    new.assignment <- get.neighbor.single.transfer(assignment)
    expect_is(new.assignment, 'list')
    expect_equal(assignment, new.assignment)
	
	assignment <- get.initial.assignment(1, c(10, 20, 30))
    new.assignment <- get.neighbor.single.transfer(assignment)
    expect_is(new.assignment, 'list')
    expect_equal(assignment, new.assignment)
  	
})


test_that("get.neighbor.single.transfer handles single job case", {
	
	assignment <- get.initial.assignment(2,10)
    new.assignment <- get.neighbor.single.transfer(assignment)
    expect_is(new.assignment, 'list')
	expect_equal(assignment[[1]], new.assignment[[2]])
	expect_equal(assignment[[2]], new.assignment[[1]])
	
	assignment <- get.initial.assignment(3,10)
    new.assignment <- get.neighbor.single.transfer(assignment)
    expect_is(new.assignment, 'list')
	expect_equal(new.assignment[[1]], NULL)
 
})