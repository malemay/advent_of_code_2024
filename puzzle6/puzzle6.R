# Puzzle 6

# Reading the input and formatting as a matrix
input <- readLines("puzzle6.txt")
input <- do.call(rbind, strsplit(input, ""))

# Creating a matrix that will hold the positions that are already visited
# Initially all the positions are FALSE because they have not been visited
visited <- matrix(FALSE, nrow = nrow(input), ncol = ncol(input))

# Let us find what is our guard's starting position and update the visited matrix accordingly
guard_pos <- c(row(input)[input == "^"], col(input)[input == "^"])
visited[guard_pos[1], guard_pos[2]] <- TRUE

# The guards initial direction is to go one row up
guard_dir <- c(-1, 0)

# A function that moves the guard one cycle and returns
# its current position as well as its direction
move_guard <- function(map, pos, dir) {
	# Let us find what is at the next position that the guard should visit
	next_pos <- pos + dir

	# We need to check if we are out of bounds
	if(next_pos[1] < 1 || next_pos[1] > nrow(map) || next_pos[2] < 1 || next_pos[2] > ncol(map)) {
		return(list(next_pos, dir))
	}

	# If it is empty then this is the output position and the direction remains the same
	if(map[next_pos[1], next_pos[2]] %in% c(".", "^")) return(list(next_pos, dir))

	# Otherwise we need to switch the direction but the position remains the same
	# If he goes up then he turns right
	if(dir[1] == -1) {
		dir <- c(0, 1)
	} else if(dir[2] == 1) { # if he goes right then he turns down
		dir <- c(1, 0)
	} else if(dir[1] == 1) { # if he goes down then he turns towards the left
		dir <- c(0, -1)
	} else { # otherwise that means he's going left and he turns up
		dir <- c(-1, 0)
	}

	# In this case when return the same position but a different direction
	return(list(pos, dir))
}

# Let us move our guard and update the matrix of visited positions until we go out of the map
while(guard_pos[1] > 0 && guard_pos[1] <= nrow(input) && guard_pos[2] > 0 && guard_pos[2] <= ncol(input)) {
	visited[guard_pos[1], guard_pos[2]] <- TRUE
	new_pos <- move_guard(input, guard_pos, guard_dir)
	guard_pos <- new_pos[[1]]
	guard_dir <- new_pos[[2]]
}

# Solution to puzzle 6A
sum(visited)

# Puzzle 6B

# We need a fancier function to solve this problem
# This one returns TRUE if the guard enters an infinite loop
# and FALSE if the guard exits the map
is_loop <- function(map, start_pos = c(66, 86), start_dir = c(-1, 0)) {
	# Initializing four matrices, one for each of the directions that the guard could be going
	up_matrix <- matrix(FALSE, nrow = nrow(map), ncol = ncol(map))
	right_matrix <- matrix(FALSE, nrow = nrow(map), ncol = ncol(map))
	down_matrix <- matrix(FALSE, nrow = nrow(map), ncol = ncol(map))
	left_matrix <- matrix(FALSE, nrow = nrow(map), ncol = ncol(map))

	# Initializing the position and direction
	pos <- start_pos
	dir <- start_dir

	# Our main exit condition is that the guard has gone off the map
	while(pos[1] > 0 && pos[1] <= nrow(map) && pos[2] > 0 && pos[2] <= ncol(map)) {

		# The matrix that gets updated depends on the direction
		# If we were to put a TRUE where there already is, it means the guard is an a loop
		if(dir[1] == -1) {
			if(up_matrix[pos[1], pos[2]]) return(TRUE) else up_matrix[[pos[1], pos[2]]] <- TRUE
		} else if(dir[2] == 1) {
			if(right_matrix[pos[1], pos[2]]) return(TRUE) else right_matrix[[pos[1], pos[2]]] <- TRUE
		} else if(dir[1] == 1) {
			if(down_matrix[pos[1], pos[2]]) return(TRUE) else down_matrix[[pos[1], pos[2]]] <- TRUE
		} else {
			if(left_matrix[pos[1], pos[2]]) return(TRUE) else left_matrix[[pos[1], pos[2]]] <- TRUE
		}

		# Then we trigger the movement
		new_pos <- move_guard(map, pos, dir)
		pos <- new_pos[[1]]
		dir <- new_pos[[2]]
	}
	
	# If we exited the while loop that means the guard was not caught in an infinite loop
	return(FALSE)
}

# Let us test all the possible positions to put an obstacle at, except those positions that already have one
solutions <- 0

# This loop is very inefficient but it works
for(i in 1:nrow(input)) {
	for(j in 1:ncol(input)) {
		if(input[i, j] %in% c("^", "#")) next
		message("Testing i = ", i, " j = ", j)
		test_matrix <- input
		test_matrix[i, j] <- "#"
		solutions <- solutions + is_loop(test_matrix, c(66, 86), c(-1, 0))
	}
}

# SOLUTION TO PUZZLE 6B
solutions
