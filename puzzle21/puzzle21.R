# Puzzle 21 A

# Reading the codes from the input
input <- readLines("puzzle21.txt")

# Creating a transition matrix that describes the keys to go from one digit to another
symbols <- as.character(c(9:0, "A"))
symbol_positions <- matrix(c(symbols, ""), nrow = 4, ncol = 3, byrow = TRUE)
symbol_positions <- symbol_positions[, 3:1]
symbol_positions[4, ] <- c("", "0", "A")

# Creating and filling the transition matrix
symbol_transitions <- matrix(NA_character_, nrow = 11, ncol = 11, dimnames = list(symbols, symbols))

for(i in symbols) {
	for(j in symbols) {
		i_pos <- c(row(symbol_positions)[symbol_positions == i], col(symbol_positions)[symbol_positions == i])
		j_pos <- c(row(symbol_positions)[symbol_positions == j], col(symbol_positions)[symbol_positions == j])
		pos_diff <- j_pos - i_pos

		# We want to move right first and move left last to avoid going over the empty spot
		if(pos_diff[2] > 0) {
			arrow_sequence <- c(rep(">", abs(pos_diff[2])), rep(ifelse(pos_diff[1] > 0, "v", "^"), abs(pos_diff[1])))
		} else {
			arrow_sequence <- c(rep(ifelse(pos_diff[1] > 0, "v", "^"), abs(pos_diff[1])), rep("<", abs(pos_diff[2])))
		}

		symbol_transitions[i, j] <- paste0(arrow_sequence, collapse = "")
	}
}

# Filling the arrow transition matrix manually
arrow_symbols <- c("^", "<", "v", ">", "A")
arrow_transitions <- matrix(  #^    #<     #v    #>    #A
			    c("",   "v<",  "v",  "v>", ">",   #^
			      ">^", "",    ">",  ">>", ">>^", #<
			      "^",  "<",   "",   ">",  ">^",  #v
			      "<^", "<<",  "<",  "",   "^",   #>
			      "<",  "v<<", "<v", "v",  ""),   #A
			    nrow = 5, ncol = 5, byrow = TRUE,
			    dimnames = list(arrow_symbols, arrow_symbols))

# A vector of arrow transitions that can be done in any order
# Transitions that use only one symbol OR would go over the empty key are excluded
variable_transitions <- c(4, 15, 16, 23)

 # Both transition matrices need to have an "A" added at the end to enable the activation
symbol_transitions[] <- paste0(symbol_transitions, "A")
arrow_transitions[] <- paste0(arrow_transitions, "A")

# A function that produces a new symbol transition matrix given
# indices for which to reverse the symbols
reverse_matrix <- function(base_matrix, indices) {
	# Looping over the indices to transform
	for(i in indices) base_matrix[i] <- invert(base_matrix[i])

	base_matrix
}

# A function that inverts a single sequence of moves
invert <- function(x) {
	# Splitting the string and removing the A at the end
	x <- strsplit(x, "")[[1]]
	x <- x[-length(x)]

	# Reversing and putting the "A" back at the end
	x <- paste0(rev(x), collapse = "")
	x <- paste0(x, "A", collapse = "")
	x
}

# A function that produces all variants of a matrix based on the transitions found in a code
vary_matrix <- function(base_matrix, code) {
	# Extracting a vector of characters in the sequence (with the A in front)
	code <- strsplit(code, "")[[1]]
	code <- c("A", code)

	indices <- numeric(length(code) - 1)

	# This loop determines which indices correspond to the transitions found in the code
	for(i in 1:(length(code) - 1)) {
		i_row <- which(rownames(base_matrix) == code[i])
		i_col <- which(colnames(base_matrix) == code[i + 1])

		# Transitions to/from 0-A and 1-4-7 only have one possibility
		if(i_row %in% c(10, 11) && i_col %in% c(3, 6, 9)) {
			indices[i] <- NA
		} else if(i_row %in% c(3, 6, 9) && i_col %in% c(10, 11)) {
			indices[i] <- NA
		} else {
			indices[i] <- (i_col - 1) * nrow(base_matrix) + i_row
		}
	}

	# Removing the indices that cannot be inverted
	indices <- indices[!is.na(indices)]

	# We need to identify the indices that will be different when inverted
	# This is the case for sequences that consist of only one (A) or two (A and an arrow) characters
	for(i in 1:length(indices)) if(length(unique(strsplit(base_matrix[indices[i]], "")[[1]])) <= 2) indices[i] <- NA
	
	indices <- indices[!is.na(indices)]

	if(!length(indices)) return(list(base_matrix))

	# Testing all index combinations
	output <- list_combinations(base_matrix, indices)

	output
}

# A function that returns a list of matrices to test given all combinations
# of a vector of indices
list_combinations <- function(base_matrix, indices) {
	combinations <- expand.grid(as.list(as.data.frame(matrix(rep(c(TRUE, FALSE), times = length(indices)), nrow = 2))))

	output <- list()

	for(i in 1:nrow(combinations)) {
		to_vary <- indices[as.logical(combinations[i, ])]
		output[[i]] <- reverse_matrix(base_matrix, to_vary)
	}

	output
}

# A function that takes a code and generates a vector of minimum-length keys to generate it
get_keys <- function(code, symbol_matrix = symbol_transitions, arrows = arrow_matrices) {

	# A list of the symbol transition matrices to test
	matrices <- vary_matrix(symbol_matrix, code)

	# Reformatting the code as a character vector
	code <- strsplit(code, "")[[1]]
	code <- c("A", code)

	# Creating a vector with empty strings for the resulting strings
	# We test all possible combinations of the transition matrices
	results <- character(length(matrices) * length(arrows)^2)
	result_index <- 1

	# Also creating a matrix that holds the combinations that were tested
	cmatrix <- matrix(NA, nrow = length(results), ncol = 3)

	# The i-level loop tests all transitions on the first keypad
	for(i in 1:length(matrices)) {

		i_sequence <- list()

		for(char in 1:(length(code) - 1)) {
			i_sequence[[char]] <- matrices[[i]][code[char], code[char + 1]]
		}

		i_sequence <- paste0(unlist(i_sequence), collapse = "")

		# The j-level loop tests all transitions on the first robotic keypad
		for(j in 1:length(arrows)) {
			j_sequence <- key2key(i_sequence, arrows[[j]])
			j_sequence <- paste0(unlist(j_sequence), collapse = "")

			# The k-level loop tests all transitions on the second robotic keypad
			for(k in 1:length(arrows)) {
				k_sequence <- key2key(j_sequence, arrows[[k]])
				results[result_index] <- paste0(unlist(k_sequence), collapse = "")
				cmatrix[result_index, ] <- c(i, j, k)
				result_index <- result_index + 1
			}
		}
	}

	list(sequence = results, cmatrix = cmatrix)
}

# A function that generates a keypad sequence from an initial keypad sequence
key2key <- function(keys, arrow_matrix) {
	output <- list()

	# We loop over the elements of the list and replace it by another list with another level
	keys <- strsplit(paste0("A", keys), "")[[1]]

	for(i in 1:(length(keys) - 1)) {
		output[[i]] <- arrow_matrix[keys[i], keys[i + 1]]
	}

	output
}

# Creating a list of the possible transition matrix between arrows
arrow_matrices <- list_combinations(arrow_transitions, variable_transitions)

result <- 0

for(i in input) {
	minlength <- min(nchar(get_keys(i)$sequence))
	result <- result + minlength * as.numeric(substr(i, 1, 3))
}

# SOLUTION TO PUZZLE 21A
result

# A function that computes the sequence on the first keypad from a numerical code
code_to_keys <- function(code, symbol_matrix = symbol_transitions) {

	# Reformatting the code as a character vector
	code <- strsplit(code, "")[[1]]
	code <- c("A", code)

	output <- list()

	for(char in 1:(length(code) - 1)) {
		output[[char]] <- symbol_matrix[code[char], code[char + 1]]
	}

	paste0(unlist(output), collapse = "")
}

# A recursive function that computes the number of key presses required for a given sequence
# at a given depth
nkeys <- function(sequence, depth, arrows = arrow_transitions) {
	# We start by decomposing the input sequence into a character vector of individual symbols
	sequence <- strsplit(sequence, "")[[1]]
	
	# We always start from a "A" because we either start there or were here after the last symbol
	sequence <- c("A", sequence)

	# Initializing the number of characters to 0
	n <- 0

	# Looping over the transitions in the sequence
	for(i in 1:(length(sequence) - 1)) {
		# If the depth is 1 then it means we can simply add the number of characters for each transition
		if(depth == 1) {
			n <- n + nchar(arrows[sequence[i], sequence[i + 1]])
		} else {
			# Otherwise we call the function recursively at the next depth
			# We get the result from the cache if available
			if(!is.na(cache[sequence[i], sequence[i + 1], depth - 1])) {
				i_result <- cache[sequence[i], sequence[i + 1], depth - 1]
			} else {
				# Computing the result and filling the cache
				i_result <- nkeys(arrows[sequence[i], sequence[i + 1]], depth = depth - 1, arrows = arrows)
				cache[sequence[i], sequence[i + 1], depth - 1] <<- i_result
			}

			n <- n + i_result
		}
	}

	return(n)
}

# Doing the same as for puzzle 21A, but this time with the recursive function and the cache
result <- 0

for(i in input) {
	message("Processing code ", i)
	# Getting the minimum possible number of characters based on various matrices
	# We test all combinations of symbol matrices and arrow matrices for each code
	matrices <- vary_matrix(symbol_transitions, i)
	shortest <- Inf

	for(mat in matrices) {
		for(j in arrow_matrices) {
			# Creating a cache that will hold the number of keys required for each transition for a given depth
			cache <- array(NA, dim = c(5, 5, 25), dimnames = list(arrow_symbols, arrow_symbols))
			mres <- nkeys(code_to_keys(i, symbol_matrix = mat), 25, arrows = j)
			if(mres < shortest) shortest <- mres
		}
	}

	result <- result + shortest * as.numeric(substr(i, 1, 3))
}

options(scipen = 16)
result
