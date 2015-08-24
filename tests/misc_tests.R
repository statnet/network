# tests for misc R functions

require(network)
require(testthat)
# tests for is.isolate

test<-network.initialize(5)
test[1,2]<-1
expect_equal(is.isolate(test), c(FALSE,FALSE,TRUE,TRUE,TRUE))
expect_equal(is.isolate(test,v=2:3),c(FALSE,TRUE))
expect_error(is.isolate(test,v=10),regexp = 'argument must be a valid vertex id')
expect_equal(length(is.isolate(network.initialize(0))),0)
