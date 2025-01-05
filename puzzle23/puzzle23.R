# Puzzle 23A

# Reading the input
input <- strsplit(readLines("puzzle23.txt"), "-")

# Formatting the network as a graph
graph <- list()

for(i in 1:length(input)) {
	graph[[ input[[i]][1] ]] <- c(graph[[ input[[i]][1] ]], input[[i]][2])
	graph[[ input[[i]][2] ]] <- c(graph[[ input[[i]][2] ]], input[[i]][1])
}

# Ordering the node names such that the ones starting with t appear first
nodes <- names(graph)
nodes <- nodes[order(!grepl("^t", nodes))]

# This object will hold the number of interconnected computers
n <- 0

# We loop over the nodes, only testing the combinations that we haven't tested yet at each loop level
for(i in which(grepl("^t", nodes))) {
	ii <- nodes[i]
	for(j in (i + 1):(length(nodes) - 1)) {
		jj <- nodes[j]
		for(k in (j + 1):length(nodes)) {
			kk <- nodes[k]
			if((jj %in% graph[[ii]]) && (kk %in% graph[[jj]]) && (ii %in% graph[[kk]])) {
				n <- n + 1
			}
		}
	}
}

# SOLUTION TO PUZZLE 23A
n

# Puzzle 23B

# A function that checks whether a graph is complete
# That is, it checks whether it contains nodes to every other node in the graph
is_complete <- function(graph, nodes) {
	for(i in 1:(length(nodes) - 1)) {
		for(j in (i + 1):length(nodes)) {
			if(! nodes[i] %in% graph[[nodes[j]]]) return(FALSE)
		}
	}

	return(TRUE)
}

# A function that checks all combinations of nodes to see whether they form a complete graph
check_completeness <- function(graph, nodes, k) {
	combinations <- combn(nodes, k)
	complete <- logical(ncol(combinations))

	for(i in 1:ncol(combinations)) complete[i] <- is_complete(graph, combinations[, i])

	combinations[, complete, drop = FALSE]
}

# A list containing the 14-connected complete graphs
graph14 <- list()

for(i in names(graph)) {
	i_nodes <- c(i, graph[[i]])
	graph14[[i]] <- check_completeness(graph, i_nodes, 14)
}

# There does not seem to be any 14-connected complete graph

# A list containing the 13-connected complete graphs
graph13 <- list()

for(i in names(graph)) {
	i_nodes <- c(i, graph[[i]])
	graph13[[i]] <- check_completeness(graph, i_nodes, 13)
}

# It looks like we have got a solution
graph13 <- graph13[sapply(graph13, function(x) ncol(x) > 0)]
graph13 <- do.call(cbind, graph13)
graph13 <- apply(graph13, 2, sort)

# SOLUTION TO PUZZLE 23B
paste0(graph13[, 1], collapse = ",")
