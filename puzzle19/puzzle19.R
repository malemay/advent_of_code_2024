# Puzzle 19A

# Reading and formatting the input as patterns and designs
input <- readLines("puzzle19.txt")
patterns <- regmatches(input[1], gregexpr("[a-z]+", input[1]))[[1]]
designs <- input[3:length(input)]

# A recursive function that determines whether a design
# can be produced from a set of patterns
is_possible <- function(design, patterns) {
	# Looping over the patterns that match the beginning of the design
	for(i in paste0("^", patterns)) {
		if(grepl(i, design)) {
			# If there are no characters left then that means this pattern is possible
			subdesign <- substring(design, nchar(i))
			if(!nchar(subdesign)) return(TRUE)

			# We isolate the part of the design that isn't matched yet and match it recursively
			if(is_possible(subdesign, patterns)) return(TRUE)
		}
	}

	# If it has not returned TRUE yet that means it is FALSE
	return(FALSE)
}

# SOLUTION TO PUZZLE 19A
sum(sapply(designs, is_possible, patterns = patterns))

# Puzzle 19B

# First we extract a list of only the designs that are possible
designs <- designs[sapply(designs, is_possible, patterns = patterns)]

# We will cache the number of patterns that are possible starting from each
# position so that we do not compute them over and over within each function call
# We need as many rows as the number of patterns to test and as many columns as the
# longest design
cache <- matrix(NA, nrow = length(designs), ncol = max(nchar(designs)))

# This function computes the number of possible patterns by looping over the
# design with various matching patterns and taking advantage of the cache
# position: the current position within the string
# index: the index of this pattern into the cache matrix
n_possible <- function(design, patterns, position, index) {
	# We initialize the number of possible arrangements to 0
	n <- 0

	# Looping over the patterns that match the beginning of the design
	for(i in paste0("^", patterns)) {
		if(grepl(i, design)) {
			# If there are no characters left then that means this pattern is possible
			subdesign <- substring(design, nchar(i))

			if(!nchar(subdesign)) {
				n <- n + 1
			# We isolate the part of the design that isn't matched yet and match it recursively
			# We get it from the cache if it has already been computed and compute it otherwise
			} else if(!is.na(cache[index, position + nchar(i) - 1])) {
				n <- n + cache[index, position + nchar(i) - 1]
			} else {
				n <- n + n_possible(subdesign, patterns, position = position + nchar(i) - 1, index = index)
			}
		}
	}

	# If it has not returned TRUE yet that means it is FALSE
	cache[index, position] <<- n
	return(n)
}

# Looping over all the patterns and computing the number if combinations
n <- numeric(length(designs))

for(i in 1:length(designs)) {
	message("Computing for design ", i, " out of ", length(designs))
	n[i] <- n_possible(designs[i], patterns, position = 1, index = i)
}

options(scipen = 16)
sum(n)

