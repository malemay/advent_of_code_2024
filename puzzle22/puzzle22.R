# Puzzle 22A

# Reading the input
input <- as.numeric(readLines("puzzle22.txt"))

# A function that compute the next secret number
next_secret <- function(x) {
	# First step
	x1 <- 64 * x
	x <- prune(mix(x, x1))

	# Second step
	x2 <- floor(x / 32)
	x <- prune(mix(x, x2))

	# Third step
	x3 <- 2048 * x
	x <- prune(mix(x, x3))

	x
}

# A function that converts a number to a vector of bits
to_bits <- function(n) {
	if(n > 2^37) stop("Number to big for conversion to bits")
	output <- numeric(38)

	for(i in 37:0) {
		if(n >= 2^i) {
			output[i + 1] <- 1
			n <- n - 2^i
		}
	}

	output
}

# A function that converts a vector of bits to 10-base numerical value
# The vector of bits must be 38 bits
to_decimal <- function(n) {
	sum(2^(0:37) * n)
}

# A function that mixes two numbers
mix <- function(x, y) {
	to_decimal(xor(to_bits(x), to_bits(y)))
}

# A function that prunes a number
prune <- function(x, mod = 16777216) {
	x %% mod
}

# A function that compute the next n secret numbers from an initial secret number
compute_secrets <- function(x, n) {
	output <- numeric(n)
	output[1] <- x

	for(i in 2:n) {
		output[i] <- next_secret(output[i - 1])
	}

	output
}

# Getting the 2000th new secret numbers and computing the sum of all of them
result <- sum(sapply(input, function(x) {message("Processing ", x); compute_secrets(x, 2001)[2001]}))

# SOLUTION TO PUZZLE 22A
result

# Puzzle 22B

# We need to retrieve the full sequences for this part of the puzzle
sequences <- sapply(input, function(x) {message("Processing ", x); compute_secrets(x, 2001)})

# We compute the consecutive differences from each of these sequences
last_digits <- substring(as.character(sequences), nchar(sequences))
last_digits <- matrix(as.numeric(last_digits), nrow = 2001)
diffs <- apply(last_digits, 2, diff)

# For each sequence we extract a named vector with the names being the 4-sequence
# of differences and the value being the number in the sequence
prices <- list()

for(i in 1:ncol(diffs)) {
	price_diffs <- diffs[, i]
	price_sequence <- last_digits[, i]
	name_vector <- character(length(price_sequence))

	for(j in 5:length(name_vector)) {
		name_vector[j] <- paste0(price_diffs[(j - 4):(j - 1)], collapse = ",")
	}

	names(price_sequence) <- name_vector
	prices[[i]] <- price_sequence
}

# For each price sequence we first remove the instances that have no name (the start of the sequence)
prices <- lapply(prices, function(x) x[names(x) != ""])

# Then we also remove duplicated names because they will never be considered
prices <- lapply(prices, function(x) x[!duplicated(names(x))])

# Getting a vector of sequences to test
to_test <- unique(names(unlist(prices)))

best_sum <- 0

# This loop should be improved to make it faster
for(i in 1:length(to_test)) {
	message("Testing value ", i, " out of ", length(to_test))
	current_sum <- sum(sapply(prices, function(x) x[to_test[i]]), na.rm = TRUE)

	# Checking whether this sum is larger than the previous ones
	if(current_sum > best_sum) {
		best_sum <- current_sum
		message("Found best sum ", best_sum, " with sequence ", i)
	}
}

# SOLUTION TO PUZZLE 22B
best_sum
