
# Functions for Simulated annealing



#' Get training set runtimes for instance type
#'
#' This function is called for its side-effect
#'
#' @param instance.type Instance type of cluster (string). All instances in the cluster are assumed to be of the same type
#' @return Matrix of runtimes for the given instance type. Each row in the matrix represents a single training sample and has 2 columns. The size column is the size of task that was processed. The runtime_sec column is the time taken to process the task in seconds.
#' @export
#' @examples
#' get.runtimes('m3xlarge')
get.runtimes <- function(instance.type) {

  dataset.name <- paste(instance.type, '.runtimes.expdist', sep='')
  data(list=dataset.name)

} # end function - get.runtimes



#' Get initial assignment of jobs to instances in a cluster
#'
#' Tasks are assigned to instances in decreasing order of longest processing time first (i.e., Longest Expected Processing Time First rule). Since task runtime is approximately proportional to task size, ordering tasks by runtime is equivalent to ordering tasks by size. The largest task is assigned to the first available machine, the 2nd largest task is assigned to the next available machine and so on.
#'
#' @param cluster.size Integer representing the number of instances in the cluster
#' @param tasks Array of integers representing the sizes of tasks in the job
#' @return A list containing a mapping of tasks to instances in cluster. The list index represents the id of an instance in the cluster while the associated list member represents the task assigned to that instance
#' @export
#' @examples
#' init <- get.initial.assignment(10, seq(1:30))
get.initial.assignment <- function(cluster.size, tasks) {

  # Validate args
  if(missing(cluster.size)) { stop('Missing required argument: cluster.size') }
  if(length(cluster.size) != 1) { stop('Invalid argument length: cluster.size must be an integer') }
  if(!is.numeric(cluster.size) || cluster.size != floor(cluster.size)) { stop('Non-integer argument: cluster.size') }
  if(cluster.size <= 0) { stop('Invalid argument: cluster.size must be > 0') }

  if(missing(tasks)) { stop('Missing required argument: tasks') }
  if(length(tasks) == 0) { stop('Invalid argument length: Must specify at least 1 task to schedule') }
  if(!is.numeric(tasks)) { stop('Non-numeric argument: Tasks sizes must be valid numbers') }
  if(any(tasks <= 0)) { stop('Invalid argument: Tasks sizes must be > 0') }

	assignment <- vector('list', cluster.size)
	sorted.tasks <- sort(tasks)

	for(i in 1:length(sorted.tasks)) {

		total.size.per.instance <- lapply(assignment, sum)
		idx.instance.with.smallest.total <- which.min(total.size.per.instance)
		# if multiple elements in list have the lowest value, which.min returns the first
		# for our purposes, it doesn't matter which of the instances with the lowest total is used next

		assignment[[idx.instance.with.smallest.total]] <- c(assignment[[idx.instance.with.smallest.total]], sorted.tasks[i])

	} # end for - loop over all tasks in order

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
	neighbor <- list()
	if (neighbor.generation.method == 'single.transfer') {
		# cat('single.transfer \n')
		neighbor <- get.neighbor.by.moving.tasks(assignment, 1)
	} else if (neighbor.generation.method == 'single.exchange') {
		# cat('single.exchange \n')
		neighbor <- get.neighbor.single.exchange(assignment)
	} # end if - which neigbor generation method to use?

	return(neighbor)

} # end function - get.neighbor



#' Generate neighbor by exchanging 1 task
#'
#' Randomly select 2 instances in the cluster. Randomly select 1 task from each of these instances. Exchange these tasks between the 2 instances. Simple random sampling without replacement is used in both sampling stages.
#' @param assignment A list representing the assignment for which a neighbor is desired
#' @return A list representing the neighboring assignment
#' @export
#' @examples
#' assignment <- get.initial.assignment(10, seq(1:30))
#' neighbor <- get.neighbor.single.exchange(assignment)
get.neighbor.single.exchange <- function(assignment) {

	# instance1 will donate a task to instance2
	# instance2 will donate a task to instance1 (different from the above task)
	instances.idx <- sample(length(assignment), 2, replace=F)
	instance1.idx <- instances.idx[1]
	instance2.idx <- instances.idx[2]

	# get index of tasks to exchange
	task1.idx <- sample(1:length(assignment[[instance1.idx]]), 1)
	task2.idx <- sample(1:length(assignment[[instance2.idx]]), 1)

	# save tasks
	task1 <- assignment[[instance1.idx]][task1.idx]
	task2 <- assignment[[instance2.idx]][task2.idx]

	# delete tasks from instances
	assignment[[instance1.idx]] <- assignment[[instance1.idx]][-task1.idx]
	assignment[[instance2.idx]] <- assignment[[instance2.idx]][-task2.idx]

	# add tasks to the other instance
	assignment[[instance1.idx]] <- c(assignment[[instance1.idx]], task2)
	assignment[[instance2.idx]] <- c(assignment[[instance2.idx]], task1)

	return(assignment)

} # end function - get.neighbor.single.exchange



#' Generate neighbor by moving 1 task
#'
#' Randomly select 2 instances in the cluster. Randomly select a task from one of the instances and move it to the other instance. Simple random sampling without replacement is used in both sampling stages.
#'
#' @param assignment A list representing the assignment for which a neighbor is desired
#' @param num.tasks Integer representing the number of tasks to be moved from 1 instance to another
#' @return A list representing the neighboring assignment
#' @export
#' @examples
#' assignment <- get.initial.assignment(10, seq(1:30))
#' neighbor <- get.neighbor.by.moving.tasks(assignment, 1)
get.neighbor.by.moving.tasks <- function(assignment, num.tasks) {

  # Validate args
  if(missing(assignment)) { stop("Missing required argument: assignment") }
  if(!is.list(assignment)) { stop("Invalid argument type: assignment must be a list") }
  if(length(assignment) == 0) { stop("Invalid argument length: assignment must contain at least 1 instance") }
  if(!is.numeric(unlist(assignment))) { stop("Non-numeric argument: tasks sizes must be valid numbers") }
  if(sum((unlist(assignment) <= 0) > 0)) { stop("Invalid argument: tasks sizes must be > 0") }

  if(missing(num.tasks)) { stop("Missing required argument: num.tasks") }
  if(length(num.tasks) > 1) { stop("Invalid argument type: num.tasks must be an single number") }
  if(!is.numeric(num.tasks) || num.tasks != floor(num.tasks)) { stop('Non-integer argument: num.tasks') }
  if(num.tasks <= 0) { stop("Invalid argument: num.tasks must be > 0") }


  # Cannot move tasks between instances when there is only 1 instance
  num.instances <- length(assignment)
  if(num.instances == 1) { return(assignment) }

  # Cannot move more tasks than available
  num.tasks.available <- length(unlist(assignment))
  if (num.tasks.available < num.tasks) {
    msg <- paste('Invalid argument: Cannot move', num.tasks, 'tasks when only', num.tasks.available, 'tasks are available')
    stop(msg)
  } # end if - move more tasks than available?


  # Get all instances with at least num.tasks
  num.tasks.in.instances <- lapply(assignment, length)
  idx.all.instances.with.tasks <- which(num.tasks.in.instances >= num.tasks)
  if (length(idx.all.instances.with.tasks) == 0) { stop("Invalid argument: No instance has ", num.tasks, ' tasks to move') }

  # Sample an instance from this list
  idx.idx.instance.with.tasks <- sample(1:length(idx.all.instances.with.tasks), 1)
  idx.instance.with.tasks <- idx.all.instances.with.tasks[idx.idx.instance.with.tasks]

  # Remove a task from this instance
  num.tasks.in.instance <- length(assignment[[idx.instance.with.tasks]])
  idx.tasks <- sample(1:num.tasks.in.instance, num.tasks)
  tasks <- assignment[[idx.instance.with.tasks]][idx.tasks]
  assignment[[idx.instance.with.tasks]] = assignment[[idx.instance.with.tasks]][-idx.tasks]
  num.remaining.tasks.in.instance <- length(assignment[[idx.instance.with.tasks]])
  if (num.remaining.tasks.in.instance == 0) assignment[idx.instance.with.tasks] <- list(NULL)


  # Get another instance
  idx.remaining.instances <- (1:length(assignment))[-idx.instance.with.tasks]
  if (length(idx.remaining.instances) == 1) { idx.instance2 <- idx.remaining.instances }
  else { idx.instance2 <- sample(c(idx.remaining.instances), 1) }

  # Move the task to this instance
  assignment[[idx.instance2]] <- c(assignment[[idx.instance2]], tasks)

  return(assignment)

} # end sub - get.neighbor.by.moving.tasks



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
		return(list("assignment"=proposed.assignment, "score"=proposed.score))
	} else {
		temp <- get.temperature(max.temp, max.iter, cur.iter)
		lhs <- exp((cur.score-proposed.score)/temp)
		rhs <- runif(1, min=0, max=1)
		if (lhs > rhs) {
			return(list("assignment"=proposed.assignment, "score"=proposed.score))
		} else {
			return(list("assignment"=cur.assignment, "score"=cur.score))
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

	return(0)

} # end function - get.score



#' Get temperature for current iteration
#'
#' Temperature decreases linearly with each iteration
#'
#' @param max.temp Max value of temperature to use (float)
#' @param max.iter Max number of iterations to search for optimal solution
#'   (integer)
#' @param cur.iter Value of current iteration (integer)
#' @return Value of temperture for the current iteration (integer)
#' @export
#' @examples
#' temp <- get.temperature(25, 100, 7)
get.temperature <- function(max.temp, max.iter, cur.iter) {

	# cur.iter is guaranteed to be at most 1 less than max.iter
	# so cur.temp will always be > 0
	cur.temp <- (max.iter-cur.iter)*(max.temp/max.iter)
	return(cur.temp)

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
#'best.schedule <- schedule(job, deadline, cluster.instance.type, cluster.size, max.iter, max.temp)
schedule <- function(job, deadline, cluster.instance.type, cluster.size, max.iter, max.temp) {

	runtimes <- get.runtimes(cluster.instance.type)

	cur.assignment <- get.initial.assignment(cluster.size, job)
	cur.score <- get.score(cur.assignment, runtimes, deadline)
	best.score <- cur.score

	# go from 0 to 1 less than max.iter
	# so we start at max temp and end just above 0 and avoid divide-by-zero errors
	for(i in 0:(max.iter-1)) {

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
