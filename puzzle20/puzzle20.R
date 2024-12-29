# Puzzle 20A

# Reading the input and formatting it as a character matrix
input <- readLines("puzzle20.txt")
input <- do.call(rbind, strsplit(input, ""))

# We can adapt some functions from puzzle 18 to this puzzle
# A function that creates a graph from a matrix of "." (free spots) and "#"
create_graph <- function(map) {
	# "S" and "E" positions are converted to "." for the purposes of this puzzle
	map[map %in% c("S", "E")] <- "."

	# We create the graph as a list with names corresponding to indices in the matrix
	# Each list element contains a link to the nodes to which the node is adjacent
	graph <- list()

	for(i in 1:length(map)) {
		# No need to create nodes for "#" nodes
		if(map[i] == "#") next

		# Extracting the neighbouring indices
		index <- c(row(map)[i], col(map)[i])

		nb <- rbind(index + c(0, 1),
			    index + c(0, -1),
			    index + c(1, 0),
			    index + c(-1, 0))

		# Removing the indices that are off-bounds or are "#"
		nb <- nb[nb[, 1] > 0 & nb[, 1] <= nrow(map) & nb[, 2] > 0 & nb[, 2] <= ncol(map), , drop = FALSE]
		nb <- nb[map[nb] != "#", , drop = FALSE]
		if(!nrow(nb)) next

		# Computing the single index into the matrix
		nb_indices <- (nb[, 2] - 1) * nrow(map) + nb[, 1]
		graph[[as.character(i)]] <- as.character(nb_indices)
	}

	return(graph)
}

# A function that uses Dijkstra's algorithm to calculate the shortest distance to the destination
# Largely copied from problem 18 (itself copied from problem 16)
dijkstra <- function(maze, start, destination) {
	# We create a vector of the unvisited nodes; initially they are all unvisited
	unvisited <- names(maze)

	# And a vector of their corresponding distances from the start
	distances <- rep(Inf, length(maze))
	names(distances) <- names(maze)

	# We set the distance from the starting node to 0
	distances[start] <- 0
	current_node <- start

	# We visit the node with the smallest distance from start until we have filled the distance vector
	while(length(unvisited)) {
		# We compute the distances to each node from the current one
		to_visit <- maze[[current_node]]
		to_visit <- to_visit[to_visit %in% unvisited]

		# We loop over the unvisited nodes (if any)
		for(i in to_visit) {

			i_dist <- distances[current_node] + 1

			# We update the distances and origin vector if the distance is shorter
			if(i_dist < distances[i]) distances[i] <- i_dist
		}

		# We remove the current node from the vector of unvisited nodes
		unvisited <- unvisited[unvisited != current_node]

		# We set the next node to visit
		current_node <- unvisited[which.min(distances[unvisited])]
	}

	distances
}

# A function that takes a new index in the graph and computes its neighbors
get_neighbors <- function(map, index) {

	# Converting the single-integer index into a row-column index
	index <- c(row(map)[index], col(map)[index])

	# Computing the neighbors
	nb <- rbind(index + c(0, 1),
		    index + c(0, -1),
		    index + c(1, 0),
		    index + c(-1, 0))

	# Removing the indices that are off-bounds or are "#"
	nb <- nb[nb[, 1] > 0 & nb[, 1] <= nrow(map) & nb[, 2] > 0 & nb[, 2] <= ncol(map), , drop = FALSE]
	nb <- nb[map[nb] != "#", , drop = FALSE]
	if(!nrow(nb)) return(character())

	# Computing the single index into the matrix
	nb_indices <- (nb[, 2] - 1) * nrow(map) + nb[, 1]

	return(nb_indices)
}

# Computing the length of the path in the original graph
graph <- create_graph(input)

start <- as.character(which(input == "S"))
destination <- as.character(which(input == "E"))

# Computing the distance to any point from the starting position
from_start <- dijkstra(graph, start, destination)
# And from any point to the end position
from_destination <- dijkstra(graph, destination, start)

# For any wall node, if removing this point creates new adjacencies,
# we can compute the distance as the distance from the start,
# plus two (going into and out of the node), plus the distance from
# the cheat's end position to the destination
# We will store these distances as a list with node1_node2 as element names
cheat_distances <- list()
wall_indices <- which(input == "#")

for(i in wall_indices) {
	message("Testing index ", i)
	i_nb <- get_neighbors(input, i)

	# A wall needs to have at least two neighbors to create a new edge
	# that may make the cheat effective
	if(length(i_nb) < 2) next

	# Otherwise we need to go over all pairs of new nodes
	for(j in 1:(length(i_nb) - 1)) {
		for(k in (j + 1):length(i_nb)) {
			# The distance including the cheat is the minimum distance from start
			# plus the minimum distance to destination, plus two
			index_pair <- as.character(i_nb[c(j, k)])
			cheat_distances[[paste0(i_nb[j], "_", i_nb[k])]] <- min(from_start[index_pair]) + min(from_destination[index_pair]) + 2
		}
	}
}

# SOLUTION TO PUZZLE 20A
sum(from_start[destination] - unlist(cheat_distances) >= 100)

# Puzzle 20B
# Here, we can test every "." node and compute the distance with the nodes that are
# less than 20 nodes away from them

# A matrix of coordinates of "." nodes
coords <- cbind(row(input)[input != "#"], col(input)[input != "#"])

# The number of cheats that save at least 100 picoseconds
n <- 0

# We can loop over all the rows
# I should find a way to make this loop faster but at least it works
for(i in 1:(nrow(coords) - 1)) {
	message("Processing index ", i)
	# Computing the single index for that row
	i_index <- (coords[i, 2] - 1) * nrow(input) + coords[i, 1]

	# The subset of other nodes to explore
	coord_subset <- coords[(i + 1):nrow(coords), , drop = FALSE]
	
	# Computing the number of picoseconds to each node
	n_ps <- abs(coords[i, 1] - coord_subset[, 1]) + abs(coords[i, 2] - coord_subset[, 2])
	coord_subset <- coord_subset[n_ps <= 20, , drop = FALSE]
	n_ps <- n_ps[n_ps <= 20]

	# Computing the single-integer indices from the coordinates
	indices <- (coord_subset[, 2] - 1) * nrow(input) + coord_subset[, 1]

	if(!length(indices)) next

	for(j in 1:length(indices)) {
		index_pair <- as.character(c(i_index, indices[j]))
		total_time <-  min(from_start[index_pair]) + min(from_destination[index_pair]) + n_ps[j]
		if(from_start[destination] - total_time >= 100) n <- n + 1
	}
}

# SOLUTION TO PUZZLE 20B
n
