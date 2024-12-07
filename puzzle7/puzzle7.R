# Puzzle 7A

# Reading and formatting the input
input <- readLines("puzzle7.txt")
input <- strsplit(input, ":")

result <- sapply(input, function(x) as.numeric(x[1]))
numbers <- lapply(input, function(x) trimws(x[2]))
numbers <- lapply(numbers, function(x) as.numeric(strsplit(x, " ")[[1]]))

# A function that determines whether a set of numbers have a solution for operators + and *
is_solution <- function(res, nums) {
	# We create a data.frame of possible solutions
	solutions <- expand.grid(as.list(as.data.frame(matrix(rep(c(`+`, `*`), times = length(nums) - 1), nrow = 2))))

	# We loop over the possible solutions until we find one that may fit
	for(i in 1:nrow(solutions)) {
		if(test_solution(nums, solutions[i, , drop = FALSE]) == res) return(TRUE)
	}

	# If we have reached this point it means there is no solution
	return(FALSE)
}

# A function that tests a single solution and returns the result of the equation
test_solution <- function(nums, solution) {
	result <- nums[1]

	for(i in 1:(length(nums) - 1)) {
		result <- solution[[i]][[1]](result, nums[i + 1])
	}

	result
}

# Finding which lines have solutions
solutions <- logical(length(result))

for(i in 1:length(result)) {
	message("Testing line ", i, " out of ", length(result))
	solutions[i] <- is_solution(result[i], numbers[[i]])
}

# Solution to puzzle 7A
options(scipen = 10)
sum(result[solutions])

# For this problem we need to include a third possible function, one that concatenates its inputs
is_solution <- function(res, nums) {
	# We create a data.frame of possible solutions
	solutions <- expand.grid(as.list(as.data.frame(matrix(rep(c(`+`, `*`, function(x, y) as.numeric(paste0(x, y))),
								  times = length(nums) - 1),
							      nrow = 3))))

	# We loop over the possible solutions until we find one that may fit
	for(i in 1:nrow(solutions)) {
		if(test_solution(nums, solutions[i, , drop = FALSE]) == res) return(TRUE)
	}

	# If we have reached this point it means there is no solution
	return(FALSE)
}

# Then we do the same thing as before
solutions <- logical(length(result))

for(i in 1:length(result)) {
	message("Testing line ", i, " out of ", length(result))
	solutions[i] <- is_solution(result[i], numbers[[i]])
}

# Solution to puzzle 7A
# The solution is very inefficient (> 10 minutes) but it works
options(scipen = 10)
sum(result[solutions])
