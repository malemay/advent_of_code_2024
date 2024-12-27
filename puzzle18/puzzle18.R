# Puzzle 18A

# Reading the input and formatting them as coordinates
input <- readLines("puzzle18.txt")
coords <- lapply(regmatches(input, gregexpr("[0-9]+", input)), as.numeric)
coords <- do.call(rbind, coords)
# We invert the coordinates so that x-indices (columns) are in the second column
coords <- coords[, c(2, 1)]

# Creating the map with dots (.) representing free spots and hashes (#) representing corrupted memory
# We need to take into account that R indexes at 1 while the problem indexes at 0
# At first we only use the first 1024 bytes, as specified in the problem
map <- matrix(".", nrow = 71, ncol = 71)
map[coords[1:1024, ] + 1] <- "#"

# A function that creates a graph from a matrix of "." (free spots) and "#"
create_graph <- function(map) {
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
# Largely copied from problem 16
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
	while(current_node != destination) {
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

graph <- create_graph(map)
path_lengths <- dijkstra(graph, start = "1", destination = as.character(length(map)))

# Solution to problem 18A
path_lengths[as.character(length(map))]

# Puzzle 18B

# A function that updates the graph by removing edges
remove_edge <- function(graph, edge) {
	graph <- graph[names(graph) != edge]
	lapply(graph, function(x) x[x != edge])
}

# We simply need to update the graph until the distance to the destination is infinite
# This solution is not fast (a binary search would be faster) but it could be coded quickly
map_copy <- map
current_byte <- 1024

while(!is.infinite(dijkstra(graph, start = "1", destination = as.character(length(map)))[as.character(length(map))])) {
	message("Testing byte ", current_byte)
	current_byte <- current_byte + 1
	next_byte <- coords[current_byte, , drop = FALSE]
	next_byte <- (next_byte[, 2] - 1 + 1) * nrow(map) + next_byte[, 1] + 1
	graph <- remove_edge(graph, as.character(next_byte))
}

# SOLUTION TO PUZZLE 18B
coords[current_byte, c(2,1)]
