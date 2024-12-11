# Puzzle 10A

# Reading the input and formatting it as a matrix
input <- readLines("puzzle10.txt")
input <- do.call(rbind, strsplit(input, ""))
input <- apply(input, 1, as.numeric)

# A function that returns the positions of all neighbouring
# spots that are 1 level higher than current position
next_coord <- function(map, current_pos) {
	# Finding the value of the current position
	current_val <- map[current_pos]

	# Creating a matrix of the coordinates of the neighbouring positions
	nb <- rbind(current_pos + c(1, 0),
		    current_pos + c(-1, 0),
		    current_pos + c(0, 1), 
		    current_pos + c(0, -1))

	# Removing the neighbours that are off-bounds
	nb <- nb[nb[, 1] > 0 & nb[, 1] <= nrow(map) & nb[, 2] > 0 & nb[, 2] <= ncol(map), , drop = FALSE]

	# Keeping only the neighbors that have the right number
	nb <- nb[map[nb] == current_val + 1, , drop = FALSE]

	# If we just reached 9s, we can stop here
	if(current_val == 8) {
		return(nb)
	} else if(nrow(nb) == 0) {
		# Then we return an empty matrix
		return(matrix(nrow = 0, ncol = 2))
	} else {
		# We need to run the function recursively on all positions and join them together
		candidates <- list()

		for(i in 1:nrow(nb)) {
			candidates[[i]] <- next_coord(map, nb[i, , drop = FALSE])
		}

		return(do.call(rbind, candidates))
	}
}

# A function that takes a given index in the matrix and computes the score of its position
get_score <- function(map, index) {
	# Converting the index into a row/column vector and then a matrix
	pos <- c(row(map)[index], col(map)[index])
	pos <- matrix(pos, nrow = 1, ncol = 2)

	# Getting the end of trailhead coordinates
	trail_ends <- next_coord(map, pos)

	# We count the number of unique endpoints
	nrow(unique(trail_ends))
}

# Computing this number for all the positions that are 0s in the matrix
# SOLUTION TO PUZZLE 10A
sum(sapply(which(input == 0), function(x, map) get_score(map, x), map = input))

# We simply need to remove the requirement for unique end positions in the get_score function
get_rating <- function(map, index) {
	# Converting the index into a row/column vector and then a matrix
	pos <- c(row(map)[index], col(map)[index])
	pos <- matrix(pos, nrow = 1, ncol = 2)

	# Getting the end of trailhead coordinates
	trail_ends <- next_coord(map, pos)

	# We count the total number of paths to the endpoints
	nrow(trail_ends)
}

# SOLUTION TO PUZZLE 10B
sum(sapply(which(input == 0), function(x, map) get_rating(map, x), map = input))
