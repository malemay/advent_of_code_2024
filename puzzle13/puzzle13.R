# Puzzle 13A

# Reading the input and formatting it as a list with one machine per element
input <- readLines("puzzle13.txt")
input <- strsplit(paste0(input, collapse = "\n"), "\n\n")[[1]]
input <- strsplit(input, "\n")

# A function that parses a machine into usable numeric vectors
parse_machine <- function(x) {
	# Parsing the x and y components of each line
	output <- regmatches(x, gregexpr("[0-9]+", x))

	# Formatting as numeric and giving names
	output <- lapply(output, as.numeric)
	names(output) <- c("a", "b", "p")

	output
}

# A function that computes the possible solutions for a given machine
get_solutions <- function(x) {
	# Creating a matrix of the solutions to test
	solutions <- as.matrix(expand.grid(1:100, 1:100))

	# Formatting the button coefficients as a matrix for matrix product
	x <- parse_machine(x)
	ab <- rbind(x$a, x$b)

	sums <- solutions %*% ab

	# Returning the possible solutions
	solutions[sums[, 1] == x$p[1] & sums[, 2] == x$p[2], , drop = FALSE]
}

# A function that gets the minimum number of tokens for a given machine
get_tokens <- function(x) {
	solutions <- get_solutions(x)

	if(!nrow(solutions)) return(0)

	min(apply(solutions, 1, function(x) x[1] * 3 + x[2]))
}

# SOLUTION TO PUZZLE 13A
sum(sapply(input, get_tokens))

# This actually amounts to solving a system of two linear equations
# We recode the get_solutions function to reflect this
# We keep only solutions that involve integers
# The function returns  0 if there is no integer solution
# because in this case we do not add tokens
# We need to work with very large integers and use the gmp package for this
library(gmp)
get_tokens <- function(x) {
	x <- parse_machine(x)
	solution <- solve(as.bigz(cbind(x$a, x$b)), c(x$p[1] + as.bigz(10000000000000), x$p[2] + as.bigz(10000000000000)))

	# We need to check if the solution involves only integers
	# If not there will be a fraction ("/") in the solution
	is_solution <- !any(grepl("/", as.character(solution)))

	if(!is_solution) return(as.bigz(0))

	return(add.bigz(mul.bigz(solution[1], 3), solution[2]))
}

solution <- as.bigz(0)

for(i in 1:length(input)) solution <- add.bigz(solution, get_tokens(input[[i]]))

# SOLUTION TO PUZZLE 13B
options(scipen = 15)
solution
