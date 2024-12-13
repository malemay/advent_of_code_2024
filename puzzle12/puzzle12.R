# Puzzle 12A

# Reading the input and formatting it as a matrix
input <- readLines("puzzle12.txt")
input <- do.call(rbind, strsplit(input, ""))

# A function that returns the indices in a region for a given index in the matrix
# We can refactor some of the elements from a function in puzzle 10
# The index is a row,col pair (formatted as a single row) into the matrix
define_region <- function(map, index, visited = NULL) {

	# If this is the top-level function call, we created a map of visited spots
	if(is.null(visited)) visited <- matrix(FALSE, nrow = nrow(map), ncol = ncol(map))

	# And update it with the current index
	visited[index] <- TRUE

	# Finding the value of the index
	val <- map[index]

	# Creating a matrix of the coordinates of the neighbouring positions
	nb <- rbind(index + c(1, 0),
		    index + c(-1, 0),
		    index + c(0, 1), 
		    index + c(0, -1))

	# Removing the neighbours that are off-bounds
	nb <- nb[nb[, 1] > 0 & nb[, 1] <= nrow(map) & nb[, 2] > 0 & nb[, 2] <= ncol(map), , drop = FALSE]

	# Keeping only the neighbors that have the right character
	nb <- nb[map[nb] == val, , drop = FALSE]

	# We also remove the indices that have already been visited
	if(nrow(nb)) nb <- nb[!visited[nb], , drop = FALSE]

	# If there are no indices to explore further, we return here
	if(!nrow(nb)) {
		return(visited)
	} else {

		# We update the matrix of candidates
		visited[nb] <- TRUE

		# We need to run the function recursively on all positions and update the visited matrix
		for(i in 1:nrow(nb)) {
			visited <- define_region(map, nb[i, , drop = FALSE], visited = visited)
		}

		return(visited)
	}
}

# A function that determines the area of a region given pre-processing with find_region
get_area <- function(x) {
	sum(x)
}

# A function that determines the perimeter of a region given pre-processing with find_region
get_perimeter <- function(x) {
	# Get a matrix of the indices in the region
	indices <- cbind(row(x)[x], col(x)[x])

	# Looping over all the indices and summing the sides that
	# do not border another tile of the same region
	perimeter <- 0

	for(i in 1:nrow(indices)) {
		index <- indices[i, , drop = FALSE]

		nb <- rbind(index + c(1, 0),
			    index + c(-1, 0),
			    index + c(0, 1), 
			    index + c(0, -1))

		# First computing the number of in-bounds neighbours
		inbounds <- nb[, 1] > 0 & nb[, 1] <= nrow(x) & nb[, 2] > 0 & nb[, 2] <= ncol(x)
		perimeter <- perimeter + sum(!inbounds)
		nb <- nb[inbounds, , drop = FALSE]

		perimeter <- perimeter + sum(!x[nb])
	}

	perimeter
}

# A function that computes the price to fence a region
# The index is a single integer into the matrix
get_price <- function(map, index) {
	# Formating the single index as a matrix
	m_index <- matrix(c(row(map)[index], col(map)[index]), nrow = 1, ncol = 2)

	# Computing the region that corresponds to this index
	region <- define_region(map, m_index)

	# Computing the price and returning it along with the region
	list(region, get_area(region) * get_perimeter(region))
}

# Looping over the matrix until we have visited all regions
visited_regions <- matrix(FALSE, nrow = nrow(input), ncol = ncol(input))
price <- 0

while(!all(visited_regions)) {
	lowest_index <- min(which(!visited_regions))
	i_price <- get_price(input, lowest_index)
	visited_regions <- visited_regions | i_price[[1]]
	price <- price + i_price[[2]]
}

# SOLUTION TO PUZZLE 12A
price

# We need to define a new function that finds the number of sides
# The solution will be based on scanning along vertical and horizontal lines
# For each scan, we will determine the number of positions int the region that
# are not bordered on the left/right or up/down. Every contiguous segment of
# such positions defines a side
get_sides <- function(x) {
	# Initializing the output value
	nsides <- 0

	# Doing the vertical scan
	# We only need to scan the columns in which we do have our region
	vrange <- range(col(x)[x])

	for(i in vrange[1]:vrange[2]) {
		left_col <- if(i == 1) rep(FALSE, nrow(x)) else x[, i - 1]
		left_sides <- x[, i] & !left_col
		nsides <- nsides + sum(rle(left_sides)$values)

		right_col <- if(i == ncol(x)) rep(FALSE, nrow(x)) else x[, i + 1]
		right_sides <- x[, i] & !right_col
		nsides <- nsides + sum(rle(right_sides)$values)
	}

	# Doing the horizontal scan similarly
	hrange <- range(row(x)[x])

	for(i in hrange[1]:hrange[2]) {
		up_row <- if(i == 1) rep(FALSE, ncol(x)) else x[i - 1, ]
		up_sides <- x[i, ] & !up_row
		nsides <- nsides + sum(rle(up_sides)$values)

		down_row <- if(i == nrow(x)) rep(FALSE, ncol(x)) else x[i + 1, ]
		down_sides <- x[i, ] & !down_row
		nsides <- nsides + sum(rle(down_sides)$values)
	}

	nsides
}

# Updating the get_price function to use the sides instead
get_price <- function(map, index) {
	# Formating the single index as a matrix
	m_index <- matrix(c(row(map)[index], col(map)[index]), nrow = 1, ncol = 2)

	# Computing the region that corresponds to this index
	region <- define_region(map, m_index)

	# Computing the price and returning it along with the region
	list(region, get_area(region) * get_sides(region))
}

# Doing the same thing as in puzzle 12A
visited_regions <- matrix(FALSE, nrow = nrow(input), ncol = ncol(input))
price <- 0

while(!all(visited_regions)) {
	lowest_index <- min(which(!visited_regions))
	i_price <- get_price(input, lowest_index)
	visited_regions <- visited_regions | i_price[[1]]
	price <- price + i_price[[2]]
}

# SOLUTION TO PUZZLE 12B
price
