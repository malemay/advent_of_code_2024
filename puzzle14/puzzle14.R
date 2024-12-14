# Puzzle 14A

# Reading the input
input <- readLines("puzzle14.txt")

# Extracting a list of positions and movements
pos <- regmatches(input, gregexpr("-?[0-9]+", input))
pos <- lapply(pos, as.numeric)

# A function that gets the end position of a robot after n seconds
move_robot <- function(robot, n = 100, width = 101, height = 103) {
	# We compute the position and use the modulo operator to bring it back on the grid
	end_x <- robot[1] + robot[3] * n
	end_x <- end_x %% width

	# We do the same with the y positions
	end_y <- robot[2] + robot[4] * n
	end_y <- end_y %% height

	c(end_x, end_y)
}

# Computing the end positions of all robots
end_positions <- lapply(pos, move_robot)
end_positions <- do.call(rbind, end_positions)

# We compute the safety factor by multiplying the number of robots in each quadrant
get_sf <- function(x) {
	# Quadrant 1
	q1 <- sum(x[, 1] < 50 & x[, 2] < 51)
	q2 <- sum(x[, 1] > 50 & x[, 2] < 51)
	q3 <- sum(x[, 1] < 50 & x[, 2] > 51)
	q4 <- sum(x[, 1] > 50 & x[, 2] > 51)
	
	q1 * q2 * q3 * q4
}

# SOLUTION TO PUZZLE 14A
get_sf(end_positions)

# Let us define a function that completes exactly one second and returns a matrix of robot positions
# The function takes a four-column matrix with robot positions (updated) and movements (unchanged)
one_sec <- function(robots) {
	# Using apply to loop over the rows
	robots <- apply(robots, 1, function(x) {new_pos <- move_robot(x, n = 1); x[1:2] <- new_pos; x})
	t(robots)
}

pos <- do.call(rbind, pos)
n_seconds <- 0

# We will use the disequilibrium between quadrants as a criterion for finding the Christmas tree image
# This should show up as a low safety factor
# Let us assume this can happen within the first 10000 seconds
safety_factors <- numeric(10000)

while(n_seconds < 10000) {
	if(n_seconds %% 100 == 0) message(n_seconds, " seconds elapsed")
	pos <- one_sec(pos)
	n_seconds <- n_seconds + 1
	safety_factors[n_seconds] <- get_sf(pos)
}

# SOLUTION TO PUZZLE 14B
which.min(safety_factors)
