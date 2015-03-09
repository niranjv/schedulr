
# Functions for Simulated annealing


data.env <- new.env()

# If an instance has more than bootstrap.threshold tasks, use Normal approx.
# to get runtime dist., instead of generating bootstrap samples
bootstrap.threshold <- 50
num.bootstrap.reps <- 1000



# -----
# Internal functions for validating input
# -----


#' Validate that the input value is a positive integer (test single number, not array)
#'
#' @param val The value to validate
#' @examples
#' check.if.positive.integer()
#' check.if.positive.integer(c())
#' check.if.positive.integer('')
#' check.if.positive.integer(5)
#' check.if.positive.integer(0)
#' check.if.positive.integer(-10)
#' check.if.positive.integer(3.14)
#' check.if.positive.integer(1:2)
#' check.if.positive.integer('a')
.check.if.positive.integer <- function(value) {

  .check.if.nonnegative.integer(value)
  value > 0 || stop("Invalid argument: Value must be > 0")

} # end function - .check.if.positive.integer


#' Validate that the input value is a non-negative integer (test single number, not array)
#'
#' @param val The value to validate
#' @examples
#' check.if.nonnegative.integer()
#' check.if.nonnegative.integer(c())
#' check.if.nonnegative.integer('')
#' check.if.nonnegative.integer(5)
#' check.if.nonnegative.integer(0)
#' check.if.nonnegative.integer(-10)
#' check.if.nonnegative.integer(3.14)
#' check.if.nonnegative.integer(1:2)
#' check.if.nonnegative.integer('a')
.check.if.nonnegative.integer <- function(value) {

  !missing(value) || stop("Missing required argument: Must specify a value")
  length(value) == 1 || stop("Invalid argument length: Must specify a single number")
  (is.numeric(value) && value == floor(value)) || stop('Non-integer argument: value')
  value >= 0 || stop("Invalid argument: Value must be >= 0")

} # end function - .check.if.nonnegative.integer


#' Verify that the input value is a positive real (test arrays)
#'
#' @param value Array of values to validate
#' @examples
#' .check.if.positive.real()
#' .check.if.positive.real(c())
#' .check.if.positive.real('')
#' .check.if.positive.real(0)
#' .check.if.positive.real(1)
#' .check.if.positive.real(3.14)
#' .check.if.positive.real(-5)
#' .check.if.positive.real(c(1.2, 3.4))
#' .check.if.positive.real('a')
.check.if.positive.real <- function(value) {

  .check.if.nonnegative.real(value)
  all(value > 0) || stop('Invalid argument: Value must be > 0')

} # end function - .check.if.positive.real


#' Verify that the input value is a non-negative real (test arrays)
#'
#' @param value Array of values to validate
#' @examples
#' .check.if.nonnegative.real()
#' .check.if.nonnegative.real(c())
#' .check.if.nonnegative.real('')
#' .check.if.nonnegative.real(0)
#' .check.if.nonnegative.real(1)
#' .check.if.nonnegative.real(c(1.2, 3.4))
#' .check.if.nonnegative.real(3.14)
#' .check.if.nonnegative.real('a')
.check.if.nonnegative.real <- function(value) {

  !missing(value) || stop('Missing required argument: Must specify a value')
  length(value) > 0 || stop('Invalid argument length: Must specify a value')
  is.numeric(value) || stop('Non-numeric argument: Must specify a valid +ve real number')
  all(value >= 0) || stop('Invalid argument: Value must be >= 0')

} # end function - .check.if.nonnegative.real


#' Verify that assignment is valid
#'
#' @param assignment Array of task sizes
#' @examples
#' a <- get.initial.assignment(2, 3)
#' .validate.assignment(a)
#' .validate.assignment(b<-NULL)
.validate.assignment <- function(assignment) {

  !missing(assignment) || stop("Missing required argument: assignment")
  is.list(assignment) || stop("Invalid argument type: assignment must be a list")
  length(assignment) != 0 || stop("Invalid argument length: assignment must contain at least 1 instance")
  is.numeric(unlist(assignment)) || stop("Non-numeric argument: tasks sizes must be valid numbers")
  sum(unlist(assignment) <= 0) == 0 || stop("Invalid argument: tasks sizes must be > 0")

} # end function - .validate.assignment



#' Verify that assignment attributes are valid
#'
#' @param assignment Array of task sizes
#' @examples
#' a <- get.initial.assignment(2, c(10))
#' .validate.assignment.attributes(a)
#' attr(a, 'score') <- 0
#' attr(a, 'runtime95pct') <- 0
#' attr(a, 'runtime99pct') <- 0
#' .validate.assignment.attributes(a)
.validate.assignment.attributes <- function(assignment) {

  is.numeric(attr(assignment, 'score')) || stop("Invalid argument: assignment score must be a valid number")
  attr(assignment, 'score') >= 0 || stop("Invalid argument: assignment score must be >= 0")

  is.numeric(attr(assignment, 'deadline')) || stop("Invalid argument: assignment deadline must be a valid number")
  attr(assignment, 'deadline') > 0 || stop("Invalid argument: deadline must be > 0")

  is.numeric(attr(assignment, 'runtime95pct')) || stop("Invalid argument: assignment runtime95pct must be a valid number")
  attr(assignment, 'runtime95pct') >= 0 || stop("Invalid argument: assignment runtime95pct must be >= 0")

  is.numeric(attr(assignment, 'runtime99pct')) || stop("Invalid argument: assignment runtime99pct must be a valid number")
  attr(assignment, 'runtime99pct') >= 0 || stop("Invalid argument: assignment runtime99pct must be >= 0")

} # end function - .validate.assignment



#' Verify that the assignment has the minimum number of tasks required
#'
#' @param assignment List mapping tasks to instances
#' @param min.num.tasks Minimum number of tasks in assignment
#' @examples
#' a <- get.initial.assignment(2, c(10))
#' .validate.num.tasks.in.assignment(a, 2)
#' .validate.num.tasks.in.assignment(a, 5)
.validate.num.tasks.in.assignment <- function(assignment, num.tasks.required) {

  num.tasks.available <- length(unlist(assignment))
  if (num.tasks.available >= num.tasks.required) {
    return (TRUE)
  } else {
    return (FALSE)
  } # end if - move more tasks than available?

} # end function .validate.num.tasks.in.assignment


#' Verify that runtimes are valid values
#'
#' @param runtimes Matrix of runtime of past runs for the given instance type. Each row in the matrix represents a single training sample and has 2 columns. The size column is the size of task that was processed. The runtime_sec column is the time taken to process the task in seconds.
#' @examples
#' r <- matrix(c(1,1), nrow=1, ncol=2)
#' .validate.runtimes.summary(r)
.validate.runtimes <- function(runtimes) {

  !missing(runtimes) || stop("Missing required argument: Must specify a numeric matrix with 2 columns")
  is.matrix(runtimes) || stop("Invalid argument type: Must specify a numeric matrix with 2 columns")
  NCOL(runtimes) == 2 || stop("Invalid argument dimensions: Must specify a numeric matrix with 2 columns")
  NROW(runtimes) > 0 || stop("Invalid argument dimensions: Must specify a numeric matrix with 2 columns and at least 1 row")
  is.numeric(runtimes) || stop ("Invalid argument: Must specify a numeric matrix with 2 columns")
  all(runtimes[,1] > 0) || stop("Invalid argument: 1st column (size) must have positive values")
  all(runtimes[,2] >= 0) || stop("Invalid argument: 2nd column (runtime) must have positive values")

} # end function - .validate.runtimes



#' Verify that runtime summaries are valid values
#'
#' @param runtimes.summary Numeric matrix containing mean and variance of runtimes for each size
#' @examples
#' rs <- matrix(c(1,1,1), nrow=1, ncol=3)
#' .validate.runtimes.summary(rs)
.validate.runtimes.summary <- function(runtimes.summary) {

  !missing(runtimes.summary) || stop("Missing required argument: Must specify a numeric matrix with 2 columns")
  is.matrix(runtimes.summary) || stop("Invalid argument type: Must specify a numeric matrix with 2 columns")
  NCOL(runtimes.summary) == 3 || stop("Invalid argument dimensions: Must specify a numeric matrix with 3 columns")
  NROW(runtimes.summary) > 0 || stop("Invalid argument dimensions: Must specify a numeric matrix with 2 columns and at least 1 row")
  is.numeric(runtimes.summary) || stop ("Invalid argument: Must specify a numeric matrix with 2 columns")
  all(runtimes.summary[,1] > 0) || stop("Invalid argument: 1st column (size) must have positive values")
  all(runtimes.summary[,2] > 0) || stop("Invalid argument: 2nd column (runtime) must have positive values")
  all(runtimes.summary[,3] >= 0) || stop("Invalid argument: 3rd column (var(runtimes)) cannot have negative values")

} # end function - .validate.runtimes.summary



.validate.instance.type <- function(instance.type) {

  !missing(instance.type) || stop("Missing required argument: Must specify instance.type")
  length(instance.type) != 0 || stop("Invalid argument length: instance.type must be a string")
  nchar(instance.type) > 0 || stop("Invalid argument length: instance.type must be a string")
  is.character(instance.type) || stop ("Invalid argument type: instance.type must be a string")
  NROW(instance.type) == 1 || stop ("Invalid argument length: instance.type must be a string, not a vector of strings")

} # end function - .validate.runtimes


# -----
# Other internal functions
# -----


#' Get runtimes for instance type
#'
#' @inheritParams setup.trainingset.runtimes
#' @param summary Return only summary of runtimes.
#' @return
#' If summary=F, return value is a matrix of runtimes for the given instance type. Each row in the matrix represents a single trial and has 2 columns. The 1st column is the size of task that was processed and the 2nd column is the runtime for this size.
#' If summary=T, return value is a matrix of summary of runtimes for the given instance type. Each row in the matrix represents a single size and has 3 columns. The 1st column is the size of task that was processed, the 2nd column is the mean runtime for this size and the 3rd column is the variance of the runtimes for this size
#' @examples
#' .get.trainingset.runtimes('m3xlarge')
.get.trainingset.runtimes <- function(instance.type, summary=F) {

  if (summary) {
    varname <- paste(instance.type, '.runtimes.summary', sep='')
  } else {
    varname <- paste(instance.type, '.runtimes', sep='')
  } # end if - get summary?

  exists(varname, envir=data.env) || stop("Runtimes for ", instance.type, " not setup correctly")
  var <- get(varname, envir=data.env) # get var from internal env (data.env)
  return (var)

} # end function - .get.trainingset.runtimes



#' Get initial assignment of tasks to instances in a cluster
#'
#' Tasks are randomly assigned to instances
#'
#' @inheritParams get.initial.assignment
#' @return List containing a mapping of tasks to instances in cluster. The list index represents the id of an instance in the cluster while the associated list member represents the task assigned to that instance
#' @examples
#' assignment <- get.initial.assignment.random(4, 1:30)
.get.initial.assignment.random <- function(cluster.size, task.sizes) {

	assignment <- vector('list', cluster.size)
  num.tasks <- length(task.sizes)
  idx.shuffle <- sample(num.tasks, replace=F)
  shuffled.task.sizes <- task.sizes[idx.shuffle]

	for (i in 1:num.tasks) {

    # get random instance
    inst <- sample(length(assignment), 1)
		assignment[[inst]] <- c(assignment[[inst]], shuffled.task.sizes[i])

	} # end for - loop over all tasks in order

	return (assignment)

} # end function - get.initial.assignment.random



#' Get initial assignment of tasks to instances in a cluster
#'
#' Tasks are assigned to instances in decreasing order of expected processing time (i.e., Longest Expected Processing Time First rule)
#'
#' @inheritParams get.initial.assignment
#' @return List containing a mapping of tasks to instances in cluster. The list index represents the id of an instance in the cluster while the associated list member represents the task assigned to that instance
#' @examples
#' rs <- matrix(nrow=2, ncol=3)
#' rs[1,1] <- 10; rs[1,2] <- 23.5; rs[1,3] <- 2.5
#' rs[2,1] <- 20; rs[2,2] <- 33.5; rs[2,3] <- 3.5
#' assignment <- get.initial.assignment.leptf(2, rep(c(1,2), 3), rs)
.get.initial.assignment.leptf <- function(cluster.size, task.sizes, runtimes.summary) {

	assignment <- vector('list', cluster.size)
  assignment.runtimes <- vector('list', cluster.size) # to keep track of total runtimes in each instance
  num.tasks <- length(task.sizes)

  means <- sapply(task.sizes, function(x) { idx <- which(runtimes.summary[,1] == x); return(runtimes.summary[idx,2]) } )
  size.means <- cbind(task.sizes, means)
  size.means <- size.means[order(size.means[,2], decreasing=TRUE), ]
  if (class(size.means) == 'numeric') size.means <- as.matrix(t(size.means))
  colnames(size.means) <- NULL

	for (i in 1:num.tasks) {

		total.runtimes.per.instance <- lapply(assignment.runtimes, sum)
		instance.with.smallest.total.runtime <- which.min(total.runtimes.per.instance)
		# if multiple elements in list have the lowest value, which.min returns the first
		# for our purposes, it doesn't matter which of the instances with the lowest total is used next

		assignment[[instance.with.smallest.total.runtime]] <- c(assignment[[instance.with.smallest.total.runtime]], size.means[i,1])
    assignment.runtimes[[instance.with.smallest.total.runtime]] <- c(assignment.runtimes[[instance.with.smallest.total.runtime]], size.means[i,2])

	} # end for - loop over all tasks in order

	return (assignment)

} # end function - get.initial.assignment.leptf



#' Get list of instances that have the minimum number of tasks required
.get.admissable.instances <- function(assignment, num.tasks.per.instance, num.instances.to.use) {

  num.tasks.in.instances <- lapply(assignment, length)
  admissable.instances <- which(num.tasks.in.instances >= num.tasks.per.instance)
  return (admissable.instances)
} # end function - get.admissable.instances



#' Get number of instances depending on whether to exchange tasks or move tasks
.get.num.instances <- function(exchange) {
  num.instances <- 1
  if (exchange) num.instances <- 2

  return (num.instances)

} # end function - .get.num.instances



#' Get temperature for current iteration
#'
#' Temperature decreases linearly with each iteration
#'
#' @inheritParams get.temperature
#' @return Value of temperture for the current iteration (integer)
#' @examples
#' temp <- .get.temperature.linear.decrease(25, 100, 7)
.get.temperature.linear.decrease <- function(max.temp, max.iter, cur.iter) {

  # cur.iter is guaranteed to be at most 1 less than max.iter
	# so cur.temp will always be > 0
	cur.temp <- (max.iter-cur.iter)*(max.temp/max.iter)
	return (cur.temp)

} # end function - get.temperature.linear.decrease



#' Get bootstrap sample for a task in the input job
#'
#' @param input.size Task size for which samples are required (integer)
#' @param num.samples Number of samples required  (integer)
#' @param runtimes Matrix containing size & runtime info for training set samples
#' @return Matrix containing required number of samples for the given size
.bootstrap.get.task.sample <- function(input.size, num.samples, runtimes) {

	# get data.frame containing runtimes for all samples for input.size
	# runtime.table.varname <- paste('runtimes.', input.size, sep='')
	# runtime.table <- get(runtime.table.varname)

  runtimes.cur.size <- subset(runtimes, runtimes[,1] == input.size)
	num.rows <- NROW(runtimes.cur.size)

  num.rows > 0 || stop('Cannot find any samples for size=', input.size, ' in training set. Ensure that training set has samples for this task size')

	idx <- sample(1:num.rows, num.samples, replace=T)
	s <- runtimes.cur.size[idx,]

	# transpose data frames due to the way they are 'flattened' in unlist
	if (NROW(s) > 1) s <- t(s)

	return (s)
} # end function - .bootstrap.get.task.sample


#' Get bootstrapped samples for all sizes in the input job
.bootstrap.get.job.sample <- function(size.reps.table, runtimes) {

	# FORMAT of size.reps.table (generated via aggregate())
	# > size.reps.table
  	# Group.1 x
	# 1      10 1
	# 2      90 1
	# 3     200 1
	# 4     850 1
	# 5    2100 1

	samples.list <- apply(size.reps.table, 1, function(x) { .bootstrap.get.task.sample(x[1], x[2], runtimes) })
	samples.matrix <- matrix(unlist(samples.list), ncol=2, byrow=TRUE)

	return (samples.matrix)

} # end function - .bootstrap.get.job.sample


.bootstrap.get.job.runtime <- function(size.reps.table, runtimes) {

	samples.matrix <- .bootstrap.get.job.sample(size.reps.table, runtimes)
	s <- sum(samples.matrix[,2])
	return (s)

} # end function - .bootstrap.get.job.runtime


.bootstrap.get.job.runtime.dist <- function(size.reps.table, num.bootstrap.reps, runtimes) {

	job.runtime.dist <- array(dim=num.bootstrap.reps)
	for(i in 1:num.bootstrap.reps) {
		r <- .bootstrap.get.job.runtime(size.reps.table, runtimes)
    job.runtime.dist[i] <- r
	} # end for - perform required number of iterations

	return (job.runtime.dist)

} # end function - .bootstrap.get.job.runtime.dist



# -----
# Exported functions
# -----


#' Setup runtimes for given instance type
#'
#' All instances in a cluster are assumed to be of the same type
#'
#' @param instance.type Instance type of cluster (string). All instances in the cluster are assumed to be of the same type
#' @param runtimes Matrix of runtimes for the given instance type. Each row in the matrix represents a single training sample and has 2 columns. The size column is the size of task that was processed. The runtime_sec column is the time taken to process the task in seconds.
#' @export
#' @examples
#' runtimes <- cbind(rep(c(1,2), each=5), c(rpois(5,5), rpois(5,10)))
#' setup.trainingset.runtimes('m3xlarge', runtimes)
setup.trainingset.runtimes <- function(instance.type, runtimes) {

  # Validate args
  .validate.instance.type(instance.type)
  .validate.runtimes(runtimes)

  # Save runtimes of individual trials to use in bootstrap sampling
  varname <- paste(instance.type, '.runtimes', sep='')
  assign(varname, runtimes, envir=data.env) # create new var in internal env (data.env)

  # save runtime summary
  m <- aggregate(runtimes[, 2], by=list(runtimes[, 1]), mean)
  v <- aggregate(runtimes[, 2], by=list(runtimes[, 1]), var)
  mv <- cbind(m[, 1], m[, 2], v[, 2])
  colnames(mv) <- c('size', 'mean', 'var')

  varname <- paste(instance.type, '.runtimes.summary', sep='')
  assign(varname, mv, envir=data.env) # create new var in internal env (data.env)

} # end function - setup.trainingset.runtimes



#' Get initial assignment of jobs to instances in a cluster
#'
#' @param cluster.size Number of instances in the cluster (+ve integer)
#' @param task.sizes Array of task sizes (+ve reals)
#' @param runtimes.summary Numeric matrix containing mean and variance of runtimes for each size. Must be supplied when method='leptf'
#' @param method Method to use to assign tasks to instances. Must be one of ('random', 'leptf').
#' @return List containing a mapping of tasks to instances in cluster. The list index represents the id of an instance in the cluster while the associated list member represents the task assigned to that instance
#' @export
#' @examples
#' assignment <- get.initial.assignment(3, 1:30)
#' rs <- matrix(nrow=2, ncol=3)
#' rs[1,1] <- 10; rs[1,2] <- 23.5; rs[1,3] <- 2.5
#' rs[2,1] <- 20; rs[2,2] <- 33.5; rs[2,3] <- 3.5
#' assignment <- get.initial.assignment(3, c(rep(10, 3), rep(20, 3)), rs, method='leptf')
get.initial.assignment <- function(cluster.size, task.sizes, runtimes.summary, method='random') {

  # Validate args
  .check.if.positive.integer(cluster.size)
  .check.if.positive.real(task.sizes)

  if (method=='random') {
    assignment <- .get.initial.assignment.random(cluster.size, task.sizes)
  } else if (method=='leptf') {
    .validate.runtimes.summary(runtimes.summary)
    assignment <- .get.initial.assignment.leptf(cluster.size, task.sizes, runtimes.summary)
  } else {
    stop('Invalid argument: ', method, ' is not a valid value for method')
  } # end if - method=random?

  return (assignment)

} # end function - get.initial.assignment



#' Generate a neighbor to an assignment
#'
#' The input assignment is modified in one of several different ways, including
#' \itemize{
#'  \item Move a task from 1 instance to another
#'  \item Exchange a task with another instance
#'  \item Move 2 tasks from 1 instance to another
#'  \item Exchange 2 tasks with another instance
#'  \item Move 2 tasks from an instance to 2 other instance
#'  \item Exchange 2 tasks with 2 other instances
#'  \item and so on...
#' }
#' Only the first 2 methods are currently implemented with an equal probability of selecting either method.
#'
#' @param assignment A list representing a mapping of tasks to instances in a cluster
#' @return A list representing the modified assignment of tasks to instances in the cluster
#' @export
#' @examples
#' assignment <- get.initial.assignment(3, 1:30)
#' proposed.assignment <- get.neighbor(assignment)
get.neighbor <- function(assignment) {

  ex <- sample(c(TRUE, FALSE), 1)
  cat('ex=', ex, '\n', sep='')
  neighbor <- move.tasks(assignment, 1, exchange=ex)

  return (neighbor)

} # end function - get.neighbor



#' Generate neighbor by moving 1 task
#'
#' Randomly select 2 instances in the cluster. Randomly select a task from one of the instances and move it to the other instance. Simple random sampling without replacement is used in both sampling stages.
#'
#' @param assignment A list representing the assignment for which a neighbor is desired
#' @param num.tasks Integer representing the number of tasks to be moved from 1 instance to another
#' @param exchange Exchange tasks between instances instead of moving them
#' @return A list representing the neighboring assignment
#' @export
#' @examples
#' assignment <- get.initial.assignment(3, 1:30)
#' neighbor <- move.tasks(assignment, 1)
#' neighbor <- move.tasks(assignment, 1, exchange=TRUE)
move.tasks <- function(assignment, num.tasks, exchange=FALSE) {

  # Validate args
  .validate.assignment(assignment)
  .check.if.positive.integer(num.tasks)


  # Need at least 2 instances in assignment to move or exchange tasks
  num.instances.in.assignment <- length(assignment)
  if (num.instances.in.assignment < 2) { return (assignment) }


  # Check if we have sufficient # tasks in the assignment (across all instances)
  if (exchange) {
    # Check if we have enough tasks to exchange
    valid <- .validate.num.tasks.in.assignment(assignment, 2*num.tasks)

    if (! valid) {
      # If not, check if we have enough tasks to move
      exchange <- FALSE
      valid <- .validate.num.tasks.in.assignment(assignment, num.tasks)
      if (! valid) {
        # If not, fail
        stop("Invalid argument: Insufficient number of task to move")
      } # end if - insufficient # tasks to move
    } # end if - have enough tasks to exchange?

  } else {
    # Check if we have enough tasks to move
    valid <- .validate.num.tasks.in.assignment(assignment, num.tasks)
    if (! valid) {
      # If not, fail
      stop("Invalid argument: Insufficient number of task to move")
    } # end if - insufficient # tasks to move

  } # end if - exchange tasks?


  # number of instances to use depends on whether we are moving tasks or exchanging tasks
  # - exchange requires 2 instances; move requires 1 instance
  num.instances.to.use <- .get.num.instances(exchange)


  # Get all instances with at least num.tasks tasks
  all.admissable.instances <- .get.admissable.instances(assignment, num.tasks, num.instances.to.use)

  # Can fail to get sufficient # admissable instances when:
  # exchange & # instances < 2
  # !exchange and # instances < 1 (due to insufficient # tasks to move in all instances)

  if (  (exchange && (length(all.admissable.instances) < 2)) ||
        (length(all.admissable.instances) < 1) ) {
    # Insuffucient # admissable instances, so try moving 1 task between instances
    exchange <- F
    num.instances.to.use <- .get.num.instances(exchange) # use 1 instance
    num.tasks <- 1
    all.admissable.instances <- .get.admissable.instances(assignment, num.tasks, num.instances.to.use)

    if(length(all.admissable.instances) < 1) {
      stop("Error: Cannot find a single instance with at least 1 task!")
    } # end if - found at least 1 instance with 1 task?

  } # end if - sufficient # instances found?

  idx.admissable.instances.sample <- sample(1:length(all.admissable.instances), num.instances.to.use)
  admissable.instances.sample <- all.admissable.instances[idx.admissable.instances.sample]

  # Remove task(s) from donor instance(s)
  tasks.mat <- matrix(nrow=num.instances.to.use, ncol=num.tasks)
  for (i in 1:num.instances.to.use) {

    inst <- admissable.instances.sample[i]
    num.tasks.in.instance <- length(assignment[[inst]])
    idx.tasks <- sample(1:num.tasks.in.instance, num.tasks)
    tasks <- assignment[[inst]][idx.tasks]

    assignment[[inst]] = assignment[[inst]][-idx.tasks]
    num.remaining.tasks.in.instance <- length(assignment[[inst]])
    if (num.remaining.tasks.in.instance == 0) assignment[inst] <- list(NULL)

    tasks.mat[i,] <- tasks
  } # end for - loop over all instances


  # TODO: need a more general way to do this
  if (exchange) {
    instance1 <- admissable.instances.sample[1]
    assignment[[instance1]] <- c(assignment[[instance1]], tasks.mat[2,])

    instance2 <- admissable.instances.sample[2]
    assignment[[instance2]] <- c(assignment[[instance2]], tasks.mat[1,])

  } else {
    # Get acceptor instance
    idx.remaining.instances <- (1:length(assignment))[-admissable.instances.sample]
    num.remaining.instances <- length(idx.remaining.instances)
    if (num.remaining.instances == 1) { instance2 <- idx.remaining.instances }
    else { instance2 <- sample(c(idx.remaining.instances), 1) }

    # Move the task to this instance
    assignment[[instance2]] <- c(assignment[[instance2]], tasks.mat[1,])

  } # end if - move only?

  attr(assignment, 'score') <- NULL
  attr(assignment, 'runtime95pct') <- NULL
  attr(assignment, 'runtime99pct') <- NULL

  return (assignment)

} # end sub - move.tasks



#' Compare 2 assignments based on their score
#'
#' Scores are calculated for both assignments. If the score of the proposed assignment is lower than the score for the current assignment, the proposed assignment and score are returned. If the score of the proposed assignment is greater than or equal to the current assignment, the a function of the current temperature and the 2 scores is used to determine which assignment to return.
#'
#' @param cur.assignment Current assigment with score attribute (list)
#' @param proposed.assignment Proposed assignment with no score (list)
#' @param runtimes Matrix of runtimes for the given instance type. Each row in the matrix represents a single training sample and has 2 columns. The size column is the size of task that was processed. The runtime_sec column is the time taken to process the task in seconds.
#' @param runtimes.summary Numeric matrix containing mean and variance of runtimes for each size
#' @param deadline Time by which job must be complete (float). Same time units as runtimes
#' @param max.temp Max temperature to use in the simulated annealing process (integer)
#' @param max.iter Max # iterations to use to find the optimal assignment via simulated annealing (integer)
#' @param cur.iter Value of current iteration (integer)
#' @return A list containing the accepted assignment and score
#' @export
#' @examples
#' r <- matrix(c(1,48), nrow=1, ncol=2)
#' rs <- matrix(c(1,48,2555), nrow=1, ncol=3)
#' c.a <- get.initial.assignment(2, c(1,1,1,1))
#' c.a <- get.score(c.a, r, rs, 120)
#' p.a <- get.neighbor(c.a)
#' a <- compare.assignments(c.a, p.a, r, rs, 120, 25, 100, 7)
compare.assignments <- function(cur.assignment, proposed.assignment, runtimes, runtimes.summary, deadline, max.temp, max.iter, cur.iter) {

  # Validate args
  .validate.assignment(cur.assignment)
  .validate.assignment.attributes(cur.assignment)
  .check.if.nonnegative.real(attr(cur.assignment, 'score'))
  .validate.assignment(proposed.assignment)

  .validate.runtimes(runtimes)
  .validate.runtimes.summary(runtimes.summary)

  .check.if.positive.real(deadline)
  length(deadline) == 1 || stop("Invalid argument length: deadline must be a single +ve real number")

  .check.if.positive.real(max.temp)
  length(max.temp) == 1 || stop("Invalid argument length: max.temp must be a single +ve real number")

  .check.if.positive.integer(max.iter)

  .check.if.nonnegative.integer(cur.iter)
  if (cur.iter >= max.iter) { stop('Invalid argument: cur.iter ', cur.iter, ' is >= max.iter ', max.iter) }

	proposed.assignment <- get.score(proposed.assignment, runtimes, runtimes.summary, deadline)

  cat('cur.assignment: \n')
  print(cur.assignment)
  cat('\n')

  cat('proposed.assignment \n')
  print(proposed.assignment)
  cat('\n')

	if (attr(proposed.assignment, 'score') >= attr(cur.assignment, 'score')) {
    cat('PROPOSED.score >= current.score. Returning PROPOSED \n')
    # new assignment has greater or equal prob. of completing job by deadline than current assignment
    result <- proposed.assignment

  } else {
	  cat('proposed.score is lower \n')
		temp <- get.temperature(max.temp, max.iter, cur.iter)
		lhs <- round(exp((attr(proposed.assignment, 'score') - attr(cur.assignment, 'score'))/temp), 2)
		rhs <- round(runif (1, min=0, max=1), 2)
    cat('temp=',temp, ' lhs=',lhs, ' rhs=',rhs, '\n')

		if (lhs > rhs) {
      cat('lhs > rhs; returning PROPOSED \n')
      result <- proposed.assignment
		} else {
      cat('lhs <= rhs; returning CURRENT \n')
      result <- cur.assignment
		} # end if - lhs > rhs?

	} # end if - proposed.score >= cur.score?

  return (result)

} # end function - compare.assignments



#' Get score for input assignment
#'
#' @param assignment The assignment which needs to be scored (list)
#' @param runtimes Matrix of runtimes for the given instance type. Each row in the matrix represents a single training sample and has 2 columns. The size column is the size of task that was processed. The runtime_sec column is the time taken to process the task in seconds.
#' @param runtimes.summary Numeric matrix containing mean and variance of runtimes for each size
#' @param deadline Time by which job must complete (float; same units as runtimes)
#' @return The input assignment with a value for the score attribute. Score is the probability of the assignment completing the job by the deadline based on the training set runtimes of the tasks in the job (float).
#' @export
#' @examples
#' assignment <- get.initial.assignment(2, c(1,1,1,1))
#' runtimes <- matrix(c(1,48), nrow=1, ncol=2)
#' runtimes.summary <- matrix(c(1,48,2555), nrow=1, ncol=3)
#' assignment <- get.score(assignment, runtimes, runtimes.summary, 600)
get.score <- function(assignment, runtimes, runtimes.summary, deadline) {

  # Validate args
  .validate.assignment(assignment)

  .validate.runtimes(runtimes)
  .validate.runtimes.summary(runtimes.summary)

  .check.if.positive.real(deadline)
  length(deadline) == 1 || stop("Invalid argument length: deadline must be a single +ve real number")

  num.instances <- length(assignment)
  scores <- matrix(nrow=num.instances, ncol=3)

  for (i in 1:num.instances) {

    tasks <- assignment[[i]]
    num.tasks <- length(tasks)

    if (num.tasks == 0) {
      scores[i] <- 1
      next;
    } # end if - any tasks on instance?

    g <- aggregate(tasks, by=list(tasks), FUN=length)

    if (num.tasks > bootstrap.threshold) {
      # use Normal approx. by CLT
      means <- apply(g, 1,
  					function(x) { runtimes.summary[which(runtimes.summary[,1] == x[1]), 2] * x[2] }
  		)

  		vars <- apply(g, 1,
  					function(x) { runtimes.summary[which(runtimes.summary[,1] == x[1]), 3] * x[2] }
  		)

  		job.mean <- sum(means)
  		job.sd <- sqrt(sum(vars))

      # score for this instance = Prob(tasks on this instance completing by deadline)
      scores[i,1] <- round(pnorm(deadline, mean=job.mean, sd=job.sd), 2)
      scores[i,2] <- round(qnorm(0.95, mean=job.mean, sd=job.sd), 2)
      scores[i,3] <- round(qnorm(0.99, mean=job.mean, sd=job.sd), 2)

    } else {
      # get dist. via bootstrap-ish sampling
      bootstrap.dist <- .bootstrap.get.job.runtime.dist(g, num.bootstrap.reps, runtimes)

      # Prob. of this instance completing by deadline
      ecdf.fn <- ecdf(bootstrap.dist)

      scores[i,1] <- round(ecdf.fn(deadline), 2)
      scores[i,2] <- round(quantile(bootstrap.dist, 0.95), 2)
      scores[i,3] <- round(quantile(bootstrap.dist, 0.99), 2)

    } # end if - more than bootstrap.threshold tasks?

  } # end for - loop over all instances in assignment

  # Return score & times of instance with least prob of completing by deadline
  min.idx <- which.min(scores[,1])

  attr(assignment, 'score') <- scores[min.idx, 1]
  attr(assignment, 'deadline') <- deadline
  attr(assignment, 'runtime95pct') <- scores[min.idx, 2]
  attr(assignment, 'runtime99pct') <- scores[min.idx, 3]

  return (assignment)

} # end function - get.score



#' Get temperature for current iteration
#'
#' @param max.temp Max value of temperature to use (float)
#' @param max.iter Max number of iterations to search for optimal solution (integer)
#' @param cur.iter Value of current iteration (integer)
#' @param method Method used to decrease temperature. Currently only linear decrease of temperature with each iteration is supported
#' @return Value of temperture for the current iteration (integer)
#' @export
#' @examples
#' temp <- get.temperature(25, 100, 7)
get.temperature <- function(max.temp, max.iter, cur.iter, method='linear') {

  # Validate args
  .check.if.positive.real(max.temp)
  length(max.temp) == 1 || stop("Invalid argument length: max.temp must be a single +ve real number")
  .check.if.positive.integer(max.iter)
  .check.if.nonnegative.integer(cur.iter)
  if (cur.iter >= max.iter) { stop('Invalid argument: cur.iter ', cur.iter, ' is >= max.iter ', max.iter) }

  if (method=='linear') {
    temp <- .get.temperature.linear.decrease(max.temp, max.iter, cur.iter)
  } else {
    stop('Invalid argument: ', method, ' method of decreasing temperature is invalid!')
  }# end if - linear decrease in temp?

  return (temp)

} # end function - get.temperature



#' Find optimal schedule
#'
#' Want an assignment with >= .95 probability of completing job by the deadline with the lowest makespan (cost)
#'
#' @param job Array of integers representing sizes of tasks in job
#' @param deadline Time (in seconds) by which job must be completed (integer)
#' @param cluster.instance.type Instance type of cluster (string). All instances in the cluster are assumed to have the same instance type
#' @param cluster.size Integer representing the number of instances in the cluster
#' @param max.iter Max number of iterations to use to find the optimal assignment (integer)
#' @param max.temp Max temperature to use in the simulated annealing process (flaot)
#' @param debug Print debug info
#' @return A list representing the optimal assignment that could be found under the given constraints
#' @export
#' @examples
#' job <- c(1,60,100)
#' deadline <- 300
#' cluster.instance.type <- 'm3xlarge'
#' cluster.size <- 2
#' max.iter <- 2
#' max.temp <- 0.5
#' data(m3xlarge.runtimes.expdist)
#' setup.trainingset.runtimes('m3xlarge', m3xlarge.runtimes.expdist)
#' best.schedule <- schedule(job, deadline, cluster.instance.type, cluster.size, max.iter, max.temp)
schedule <- function(job, deadline, cluster.instance.type, cluster.size, max.iter, max.temp, debug=FALSE) {

  start.time <- proc.time()

  if (debug) {
    output.prefix <- paste(cluster.size, '-inst-', length(job), '-tasks-', max.iter, '-SAiter-', num.bootstrap.reps, '-BSreps', sep='')
    filename <- filename <- paste(output.prefix, '.output.txt', sep='')
    sink(paste(output.prefix, '-output.txt', sep=''))
  }

  runtimes <- .get.trainingset.runtimes(cluster.instance.type)
  runtimes.summary <- .get.trainingset.runtimes(cluster.instance.type, summary=T)

	cur.assignment <- get.initial.assignment(cluster.size, job)
	cur.assignment <- get.score(cur.assignment, runtimes, runtimes.summary, deadline)

	best.assignment <- cur.assignment

  if (debug) {
    scores.timeseries <- matrix(nrow=(max.iter+1), ncol=6)
    colnames(scores.timeseries) <- c(paste('Acpt_', deadline, 's', sep=''), 'Acpt_95%', 'Acpt_99%', paste('Best_', deadline, 's', sep=''), 'Best_95%', 'Best_99%')

    scores.timeseries[1,1] <- attr(cur.assignment, 'score')
    scores.timeseries[1,2] <- attr(cur.assignment, 'runtime95pct')
    scores.timeseries[1,3] <- attr(cur.assignment, 'runtime99pct')

    scores.timeseries[1,4] <- attr(cur.assignment, 'score')
    scores.timeseries[1,5] <- attr(cur.assignment, 'runtime95pct')
    scores.timeseries[1,6] <- attr(cur.assignment, 'runtime99pct')
  } # end if - debug?


	# go from 0 to 1 less than max.iter
	# so we start at max temp and end just above 0 and avoid divide-by-zero errors
	for (i in 0:(max.iter-1)) {

    if (debug) cat('\n\n==========\nSA iter: ', i, '\n', '==========\n\n', sep='')

		proposed.assignment <- get.neighbor(cur.assignment)
		accepted.assignment <- compare.assignments(cur.assignment, proposed.assignment, runtimes, runtimes.summary, deadline, max.temp, max.iter, i)
		cur.assignment <- accepted.assignment

    best.score <- attr(best.assignment, 'score')
    accepted.score <- attr(accepted.assignment, 'score')
    # update best score, if necessary
    if (accepted.score > best.score) best.assignment <- accepted.assignment
    if (debug) cat('best score=', best.score, '\n')

    # restart from current best assignment if score of current assignment is too low
    d <- (best.score - accepted.score)
    d.pct <- d/best.score
    if (d.pct > 0.10) {
      cur.assignment <- best.assignment
      if (debug) cat('Resetting current assignment to best assignment since d.pct=', d.pct, '. Best score so far = ', best.score, '\n', sep='')
    } # end if - reset current assignment to best assignment


    if (debug) {
      scores.timeseries[(i+2),1] <- attr(accepted.assignment, 'score')
      scores.timeseries[(i+2),2] <- attr(accepted.assignment, 'runtime95pct')
      scores.timeseries[(i+2),3] <- attr(accepted.assignment, 'runtime99pct')

      scores.timeseries[(i+2),4] <- attr(best.assignment, 'score')
      scores.timeseries[(i+2),5] <- attr(best.assignment, 'runtime95pct')
      scores.timeseries[(i+2),6] <- attr(best.assignment, 'runtime99pct')
    } # end if - debug?

	} # end for - loop over all iterations

	cat('\nBest score: ', attr(best.assignment, 'score'), '\n')
	cat('Best assignment: \n')
	print(best.assignment)
  cat('\n\n')

  d <- proc.time()-start.time
  cat('Time taken: ', d[3], ' seconds')

  if (debug)  {
    sink()
    filename <- paste(output.prefix, '-scores-timeseries.csv', sep='')
    write.csv(scores.timeseries, filename, quote=F, row.names=F)
  } # end if - debug?

  return (best.assignment)

} # end function - schedule
