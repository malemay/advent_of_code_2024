# Puzzle 8A

# Reading the input and formatting it as a matrix
input <- readLines("puzzle8.txt")
input <- do.call(rbind, strsplit(input, ""))

# Getting a vector of the unique chracters that represent antennas
antennas <- unique(as.character(input[input != "."]))

# A function that finds returns a matrix with the positions
# of the antennas matching a given character
find_pos <- function(map, char) {
	pos_map <- map == char
	return(cbind(row(map)[pos_map], col(map)[pos_map]))
}

# A function that returns a matrix of antinode coordinates for a pair of antennae
# The input is a two-row matrix with row coordinates in the first column and column coordinates in the second
antinode_pair <- function(coords) {
	# The subtraction of pos1 from pos2 gives a vector going from pos1 to pos2
	coord_vect <- coords[2, ] - coords[1, ]

	# The first antinode is pos1 minus the vector
	n1 <- coords[1, ] - coord_vect

	# The second antinode is pos2 plus the vector
	n2 <- coords[2, ] + coord_vect

	# We return both antinodes in a matrix
	# We leave the bounds checking to another step of the script
	rbind(n1, n2)
}

# A function that finds all antinode positions for a given map and antenna character
find_antinodes <- function(map, char) {
	# Getting a matrix of that character's position in the map
	coords <- find_pos(map, char)

	# We initialize a list of antinode positions
	antinodes <- list()

	# We need to test all combinations, therefore we need two indices
	for(i in 1:(nrow(coords) - 1)) {
		for(j in (i + 1):(nrow(coords))) {
			antinodes[[paste0(i, j)]] <- antinode_pair(coords[c(i, j), ])
		}
	}

	# Joining all the results together
	do.call(rbind, antinodes)
}

# Computing the antinode positions for all characters
antinodes <- lapply(antennas, function(char, map) find_antinodes(map, char), map = input)
antinodes <- do.call(rbind, antinodes)

# Removing antinode positions thatr are off-bounds
antinodes <- antinodes[antinodes[, 1] > 0 & antinodes[, 1] <= nrow(input) & antinodes[, 2] > 0 & antinodes[, 2] <= ncol(input), ]

# Keeping only unique occurrences
antinodes <- unique(antinodes)

# SOLUTION TO PUZZLE 8A
nrow(antinodes)

# We need an updated antinode_pair function that finds a larger number of antinodes
# We will uglily simplify the problem by just adding 50 antinodes on either side
# of each antenna. This will make sure that we get all antennas in the frame.
# Off-bounds removal will then take care of removing superfluous antinodes
antinode_pair <- function(coords) {
	# The subtraction of pos1 from pos2 gives a vector going from pos1 to pos2
	coord_vect <- coords[2, ] - coords[1, ]

	# The first set of antinodes is pos1 minus the vector
	n1 <- matrix(NA, nrow = 50, ncol = 2)
	for(i in 1:50) n1[i, ] <- coords[1, ] - coord_vect * i

	# The second set of antinodes is pos2 plus the vector
	n2 <- matrix(NA, nrow = 50, ncol = 2)
	for(i in 1:50) n2[i, ] <- coords[2, ] + coord_vect * i

	# We return both sets of antinodes in a matrix
	# We also need to add the antenna coordinates themselves
	# We leave the bounds checking to another step of the script
	rbind(n1, n2, coords)
}

# The rest of the problem is exactly as before
antinodes <- lapply(antennas, function(char, map) find_antinodes(map, char), map = input)
antinodes <- do.call(rbind, antinodes)
antinodes <- antinodes[antinodes[, 1] > 0 & antinodes[, 1] <= nrow(input) & antinodes[, 2] > 0 & antinodes[, 2] <= ncol(input), ]
antinodes <- unique(antinodes)

# SOLUTION TO PUZZLE 8B
nrow(antinodes)
