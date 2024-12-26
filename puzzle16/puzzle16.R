# Puzzle 16A

# Reading the input and formatting it as a matrix
input <- readLines("puzzle16.txt")
input <- do.call(rbind, strsplit(input, ""))

# We create a representation of the labyrinth as a graph
# It is formatted as a list with element names being the (single-number) index in the matrix
# and the list elements being vectors indicating which paths are available
# to the right ("R"), left ("L"), up ("U") and down ("D")
graph <- list()

for(i in 1:length(input)) {

	# No need to fill the graph for indices that are not "."
	if(!input[i] %in% c(".", "S", "E")) next

	# Extracting the indices
	index <- c(row(input)[i], col(input)[i])
	right_index <- index + c(0, 1)
	left_index  <- index + c(0, -1)
	up_index    <- index + c(-1, 0)
	down_index  <- index + c(1, 0)

	# Filling the index to the up value
	if(up_index[1] > 0 && input[up_index[1], up_index[2]] %in% c(".", "E", "S")) {
		graph[[as.character(i)]] <- c(graph[[as.character(i)]], c("U" = as.character(i - 1)))
	}

	# Filling the index to the right value
	if(right_index[2] <= ncol(input) && input[right_index[1], right_index[2]] %in% c(".", "E", "S")) {
		graph[[as.character(i)]] <- c(graph[[as.character(i)]], c("R" = as.character(i + nrow(input))))
	}

	# Filling the index to the left value
	if(left_index[2] > 0 && input[left_index[1], left_index[2]] %in% c(".", "E", "S")) {
		graph[[as.character(i)]] <- c(graph[[as.character(i)]], c("L" = as.character(i - nrow(input))))
	}

	# Filling the index to the down value
	if(down_index[1] <= nrow(input) && input[down_index[1], down_index[2]] %in% c(".", "E", "S")) {
		graph[[as.character(i)]] <- c(graph[[as.character(i)]], c("D" = as.character(i + 1)))
	}
}

# A function that finds a solution through Dijkstra's algorithm
dijkstra <- function(maze, start, destination, starting_direction = "R") {
	# We create a vector of the unvisited nodes; initially they are all unvisited
	unvisited <- names(maze)

	# And a vector of their corresponding distances from the start
	distances <- rep(Inf, length(maze))
	names(distances) <- names(maze)

	# And also a vector that holds the node that provided the shortest path to each node
	origin <- rep("", length(maze))
	names(origin) <- names(maze)

	# We set the distance from the starting node to 0
	distances[start] <- 0
	current_node <- start

	# We visit the node with the smallest distance from start until we have filled the distance vector
	while(length(unvisited)) {
		# We compute the distances to each node from the current one
		to_visit <- maze[[current_node]]
		to_visit <- to_visit[to_visit %in% unvisited]

		# We loop over the unvisited nodes
		for(i in names(to_visit)) {
			# The added distance depends on whether the path needs to turn or not
			i_node <- to_visit[i]

			# The case if we are at the starting node
			if(current_node == start) {
				to_add <- if(i == starting_direction) 1 else 1001
			} else {
				previous_node <- maze[[origin[current_node]]]
				to_add <- if(i == names(previous_node)[previous_node == current_node]) 1 else 1001
			}

			i_dist <- distances[current_node] + to_add

			# We update the distances and origin vector if the distance is shorter
			if(i_dist < distances[i_node]) {
				distances[i_node] <- i_dist
				origin[i_node] <- current_node
			}
		}

		# We remove the current node from the vector of unvisited nodes
		unvisited <- unvisited[unvisited != current_node]

		# We set the next node to visit
		current_node <- unvisited[which.min(distances[unvisited])]
	}

	distances
}

start <- c("R" = as.character(which(input == "S")))
destination <- as.character(which(input == "E"))
solution <- dijkstra(graph, start, destination)

# SOLUTION TO PUZZLE 16A
solution[destination]

# PUZZLE 16B
# If we sum the distance from the start to each node and
# from the destination to each node, the sum of these distances
# should be minimal for nodes that are part of a best path
# We should compute starting left and down from destination because
# the orientation at destination does not matter
from_dest_l <- dijkstra(graph, destination, start, "L")
from_dest_d <- dijkstra(graph, destination, start, "D")
from_dest <- pmin(from_dest_l, from_dest_d)

# We get all the sums that are shorter or equal to the best score
# It is <= because in some cases the path should turn at the node
# but this is not accounted for by our approach
# SOLUTION TO PUZZLE 16B
sum(solution + from_dest <= solution[destination])
