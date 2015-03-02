
# Functions for Simulated annealing


data.env <- new.env()


# -----
# Internal functions for validating input
# -----

#' Validate that the input value is a positive integer
#'
#' @param val The value to validate
#' @examples
#' check.if.positive.integer(5)
#' check.if.positive.integer(3.14)
#' check.if.positive.integer(-10)
#' check.if.positive.integer(1:2)
#' check.if.positive.integer('a')
.check.if.positive.integer <- function(value) {

  if (missing(value)) { stop("Missing required argument: Must specify a value") }
  if (length(value) != 1) { stop("Invalid argument length: Must specify a single number") }
  if (!is.numeric(value) || value != floor(value)) { stop('Non-integer argument: value') }
  if (value <= 0) { stop("Invalid argument: Value must be > 0") }

} # end function - .check.if.positive.integer


.check.if.nonnegative.integer <- function(value) {

  if (missing(value)) { stop("Missing required argument: Must specify a value") }
  if (length(value) != 1) { stop("Invalid argument length: Must specify a single number") }
  if (!is.numeric(value) || value != floor(value)) { stop('Non-integer argument: value') }
  if (value < 0) { stop("Invalid argument: Value must be >= 0") }

} # end function - .check.if.positive.integer



#' Verify that input values are valid
#'
#' @param task.sizes Array of values to validate
#' @examples
#' '.check.if.positive.real(c(1.2,3.4))
#' .check.if.positive.real(3.14)
#' .check.if.positive.real('a')
.check.if.positive.real <- function(value) {

  if (missing(value)) { stop('Missing required argument: Must specify a value') }
  if (length(value) == 0) { stop('Invalid argument length: Must specify a value') }
  if (!is.numeric(value)) { stop('Non-numeric argument: Must specify a valid +ve real number') }
  if (any(value <= 0)) { stop('Invalid argument: Value must be > 0') }

} # end function - .check.if.positive.real



#' Verify that assignment is valid
#'
#' @param assignment Array of task sizes
#' @examples
#' a <- get.initial.assignment(2, 3)
#' .validate.assignment(a)
#' .validate.assignment(b<-NULL)
.validate.assignment <- function(assignment) {

  if (missing(assignment)) { stop("Missing required argument: assignment") }
  if (!is.list(assignment)) { stop("Invalid argument type: assignment must be a list") }
  if (length(assignment) == 0) { stop("Invalid argument length: assignment must contain at least 1 instance") }
  if (!is.numeric(unlist(assignment))) { stop("Non-numeric argument: tasks sizes must be valid numbers") }
  if (sum((unlist(assignment) <= 0) > 0)) { stop("Invalid argument: tasks sizes must be > 0") }

} # end function - .validate.assignment



#' Verify that the assignment has the minimum number of tasks required
#'
#' @param assignment List mapping tasks to instances
#' @param min.num.tasks Minimum number of tasks in assignment
#' @examples
#' a <- get.initial.assignment(2, 4)
#' .validate.num.tasks.in.assignment(a, 2)
#' .validate.num.tasks.in.assignment(a, 5)
.validate.num.tasks.in.assignment <- function(assignment, num.tasks.required) {

  num.tasks.available <- length(unlist(assignment))
  if (num.tasks.available < num.tasks.required) {
    msg <- paste('Invalid argument: Cannot move', num.tasks.required, 'tasks when only', num.tasks.available, 'tasks are available')
    stop(msg)
  } # end if - move more tasks than available?

} # end function .validate.num.tasks.in.assignment


# -----
# Other internal functions
# -----


#' Get runtimes for instance type
#'
#' @inheritParams setup.runtimes
#' @return Matrix of runtimes for the given instance type. Each row in the matrix represents a single training sample and has 2 columns. The size column is the size of task that was processed. The runtime_sec column is the time taken to process the task in seconds.
#' @examples
#' .get.runtimes('m3xlarge')
.get.runtimes <- function(instance.type) {

  # dataset.name <- paste(instance.type, '.runtimes.expdist', sep='')
  # data(list=dataset.name)

  varname <- paste(instance.type, '.runtimes', sep='')
  if(!exists(varname, envir=data.env)) { stop("Runtimes for ", instance.type, " not setup correctly") }
  var <- get(varname, envir=data.env) # get var from internal env (data.env)
  return (var)

} # end function - get.runtimes



#' Get initial assignment of jobs to instances in a cluster
#'
#' Tasks are assigned to instances in decreasing order of longest processing time first (i.e., Longest Expected Processing Time First rule). Since task runtime is approximately proportional to task size, ordering tasks by runtime is equivalent to ordering tasks by size. The largest task is assigned to the first available machine, the 2nd largest task is assigned to the next available machine and so on.
#'
#' @inheritParams get.initial.assignment
#' @return List containing a mapping of tasks to instances in cluster. The list index represents the id of an instance in the cluster while the associated list member represents the task assigned to that instance
#' @examples
#' init <- get.initial.assignment.leptf(10, seq(1:30))
.get.initial.assignment.leptf <- function(cluster.size, task.sizes) {

	assignment <- vector('list', cluster.size)
	sorted.task.sizes <- sort(task.sizes)
  num.tasks <- length(sorted.task.sizes)

	for (i in 1:num.tasks) {

		total.size.per.instance <- lapply(assignment, sum)
		instance.with.smallest.total <- which.min(total.size.per.instance)
		# if multiple elements in list have the lowest value, which.min returns the first
		# for our purposes, it doesn't matter which of the instances with the lowest total is used next

		assignment[[instance.with.smallest.total]] <- c(assignment[[instance.with.smallest.total]], task.sizes[i])

	} # end for - loop over all tasks in order

	return (assignment)

} # end function - get.initial.assignment.leptf



#' Get list of instances that have the minimum number of tasks required
.get.admissable.instances <- function(assignment, num.tasks.per.instance, num.instances.to.use) {

  num.tasks.in.instances <- lapply(assignment, length)
  admissable.instances <- which(num.tasks.in.instances >= num.tasks.per.instance)
  num.admissable.instances <- length(admissable.instances)
  if (num.admissable.instances < num.instances.to.use) stop("Invalid argument: Cannot find ", num.instances.to.use, " instances with at least ", num.tasks.per.instance, " tasks each")

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



# -----
# Exported functions
# -----


#' Setup runtimes for given instance type
#'
#' All instances in a cluster are assumed ot be of the same type
#'
#' @param instance.type Instance type of cluster (string). All instances in the cluster are assumed to be of the same type
#' @param runtimes Matrix of runtimes for the given instance type. Each row in the matrix represents a single training sample and has 2 columns. The size column is the size of task that was processed. The runtime_sec column is the time taken to process the task in seconds.
#' @export
#' @examples
#' runtimes <- cbind(rep(c(1,2), each=5), c(rpois(5,5), rpois(5,10)))
#' setup.runtimes('m3xlarge', runtimes)
setup.runtimes <- function(instance.type, runtimes) {

  varname <- paste(instance.type, '.runtimes', sep='')
  assign(varname, runtimes, envir=data.env) # create new var in internal env (data.env)

} # end function - setup.runtimes



#' Get initial assignment of jobs to instances in a cluster
#'
#' @param cluster.size Number of instances in the cluster (+ve integer)
#' @param task.sizes Array of task sizes (positive reals)
#' @return List containing a mapping of tasks to instances in cluster. The list index represents the id of an instance in the cluster while the associated list member represents the task assigned to that instance
#' @export
#' @examples
#' init <- get.initial.assignment(10, seq(1:30))
get.initial.assignment <- function(cluster.size, task.sizes) {

  # Validate args
  .check.if.positive.integer(cluster.size)
  .check.if.positive.real(task.sizes)

  assignment <- .get.initial.assignment.leptf(cluster.size, task.sizes)
  return(assignment)

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
#' assignment <- get.initial.assignment(10, seq(1:30))
#' candidate.assignment <- get.neighbor(assignment)
get.neighbor <- function(assignment) {

	neighbor.generation.method <- sample(c('single.transfer', 'single.exchange'), 1)
	if (neighbor.generation.method == 'single.transfer') {
		neighbor <- move.tasks(assignment, 1, exchange=F)
	} else if (neighbor.generation.method == 'single.exchange') {
		neighbor <- move.tasks(assignment, 1, exchange=T)
	} # end if - which neigbor generation method to use?

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
#' assignment <- get.initial.assignment(10, seq(1:30))
#' neighbor <- move.tasks(assignment, 1)
move.tasks <- function(assignment, num.tasks, exchange=F) {

  # Validate args
  .validate.assignment(assignment)
  .check.if.positive.integer(num.tasks)

  # Cannot shift more tasks than available
  .validate.num.tasks.in.assignment(assignment, num.tasks)


  # number of instances to use depends on whether we are moving tasks or exchanging tasks
  num.instances.to.use <- .get.num.instances(exchange)

  # Need at least 2 instances in assignment
  num.instances.in.assignment <- length(assignment)
  if (num.instances.in.assignment < 2) { return (assignment) }


  # - Get all instances with at least num.tasks.to.use
  # - Sample required number of instances from this list
  all.admissable.instances <- .get.admissable.instances(assignment, num.tasks, num.instances.to.use)
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

  return (assignment)

} # end sub - move.tasks



#' Compare 2 assignments based on their score
#'
#' Scores are calculated for both assignments. If the score of the proposed assignment is lower than the score for the current assignment, the proposed assignment and score are returned. If the score of the proposed assignment is greater than or equal to the current assignment, the a function of the current temperature and the 2 scores is used to determine which assignment to return.
#'
#' @param cur.assignment The current best assigment (list)
#' @param proposed.assignment The proposed assignment (list)
#' @param runtimes Training set runtimes for current instance type of cluster (matrix)
#' @param deadline Time by which job must be complete (float). Same time units as runtimes
#' @param max.temp Max temperature to use in the simulated annealing process (integer)
#' @param max.iter Max # iterations to use to find the optimal assignment via simulated annealing (integer)
#' @param cur.iter Value of current iteration (integer)
#' @return A list containing the accepted assignment and score
#' @export
# @examples
# accepted <- compare.assignments(cur.assignment, proposed.assignment, runtimes, 3600, 25, 100, 7)
compare.assignments <- function(cur.assignment, proposed.assignment, runtimes, deadline, max.temp, max.iter, cur.iter) {

	cur.score <- get.score(cur.assignment, runtimes, deadline)
	proposed.score <- get.score(proposed.assignment, runtimes, deadline)

	if (proposed.score < cur.score) {
		return (list("assignment"=proposed.assignment, "score"=proposed.score))
	} else {
		temp <- get.temperature(max.temp, max.iter, cur.iter)
		lhs <- exp((cur.score-proposed.score)/temp)
		rhs <- runif (1, min=0, max=1)
		if (lhs > rhs) {
			return (list("assignment"=proposed.assignment, "score"=proposed.score))
		} else {
			return (list("assignment"=cur.assignment, "score"=cur.score))
		}
	} # end if - proposed.score < cur.score?

} # end function - compare.assignments



#' Get score for input assignment
#'
#' @param assignment The assignment which needs to be scored (list)
#' @param runtimes Runtimes used to determine score (matrix)
#' @param deadline Time by which job must complete (float; same units as runtimes)
#' @return The score of the assignment, i.e, the probability of the assignment completing the job by the deadline based on the training set runtimes of the tasks in the job (float).
#' @export
#' @examples
#' prob <- get.score(assignment, runtimes, 3600)
get.score <- function(assignment, runtimes, deadline) {

	return (0)

} # end function - get.score



#' Get temperature for current iteration
#'
#' @param max.temp Max value of temperature to use (float)
#' @param max.iter Max number of iterations to search for optimal solution
#'   (integer)
#' @param cur.iter Value of current iteration (integer)
#' @param method Method used to decrease temperature. Currently only linear decrease of temperature with each iteration is supported
#' @return Value of temperture for the current iteration (integer)
#' @export
#' @examples
#' temp <- get.temperature(25, 100, 7)
get.temperature <- function(max.temp, max.iter, cur.iter, method='linear') {

  # Validate args
  .check.if.positive.integer(max.temp)
  .check.if.positive.integer(max.iter)
  .check.if.nonnegative.integer(cur.iter)
  if (cur.iter >= max.iter) { stop('Invalid argument: cur.iter ', cur.iter, ' is >= max.iter ', max.iter) }
  if (method != 'linear') { stop('Invalid argument: Only method=linear is currently supported') }

  if (method=='linear') {
    temp <- .get.temperature.linear.decrease(max.temp, max.iter, cur.iter)
  } else {
    stop('Invalid argument: ', method, ' method of decreasing temperature is invalid!')
  }# end if - linear decrease in temp?

  return(temp)

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
#' @return A list representing the optimal assignment that could be found under the given constraints
#' @export
#' @examples
#' job <- seq(1:30)
#' deadline <- 3600
#' cluster.instance.type <- 'm3xlarge'
#' cluster.size <- 10
#' max.iter <- 10
#' max.temp <- 25
#' data(m3xlarge.runtimes.expdist)
#' setup.runtimes('m3xlarge', m3xlarge.runtimes.expdist)
#' best.schedule <- schedule(job, deadline, cluster.instance.type, cluster.size, max.iter, max.temp)
schedule <- function(job, deadline, cluster.instance.type, cluster.size, max.iter, max.temp) {

	runtimes <- .get.runtimes(cluster.instance.type)

	cur.assignment <- get.initial.assignment(cluster.size, job)
	cur.score <- get.score(cur.assignment, runtimes, deadline)
	best.score <- cur.score

	# go from 0 to 1 less than max.iter
	# so we start at max temp and end just above 0 and avoid divide-by-zero errors
	for (i in 0:(max.iter-1)) {

		proposed.assignment <- get.neighbor(cur.assignment)
		accepted <- compare.assignments(cur.assignment, proposed.assignment, runtimes, deadline, max.temp, max.iter, i)
		cur.assignment <- accepted$assignment

		if (accepted$score < best.score) {
			best.score = accepted$score
		} # end if - update best score

	} # end for - loop over all iterations

	cat('Best score: ', best.score, '\n')
	cat('Best assignment: \n')
	print(cur.assignment)

} # end function - schedule
