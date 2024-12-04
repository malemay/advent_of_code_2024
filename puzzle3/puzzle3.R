# Puzzle 3A

# Reading the input
input <- readLines("puzzle3.txt")

# Extracting the list of multiplication expressions
muls <- regmatches(input, gregexpr("mul\\([0-9]{1,3},[0-9]{1,3})", input))
muls <- unlist(muls)

# Extracting the first number from each expression
first <- unlist(regmatches(muls, gregexpr("\\([0-9]{1,3}", muls)))
first <- as.numeric(substring(first, 2))

# Extracting the second number from each expression
second <- unlist(regmatches(muls, gregexpr(",[0-9]{1,3}", muls)))
second <- as.numeric(substring(second, 2))

# SOLUTION TO PUZZLE 3A
sum(first * second)

# PUZZLE 3B

# We need to filter out the mul(,) match instances that are not enabled
# To do this we will build a function that checks each match against
# the do/don't positions and determines whether a given position should be included in the output
# The function takes one string at a time
extract_matches <- function(string) {
	stopifnot(length(string) == 1)

	# Extracting the positions of the do() and don't()
	do <- gregexpr("do\\(\\)", string)[[1]]
	dont <- gregexpr("don't\\(\\)", string)[[1]]

	# Extracting the positions of the "mul(,)" patterns
	muls <- gregexpr("mul\\([0-9]{1,3},[0-9]{1,3})", string)[[1]]

	# For each match we check whether it is closer to a do or a don't (going backwards)

	# First we compute the backwards distance to any do or don't
	do_dist <- outer(muls, do, "-")
	dont_dist <- outer(muls, dont, "-")

	# We set the negative values to Inf because they are forwards
	do_dist[do_dist < 0] <- Inf
	dont_dist[dont_dist < 0] <- Inf

	# Then we get a logical vector indicating whether a value is closer to a don't
	kept_matches <- apply(do_dist, 1, min) < apply(dont_dist, 1, min)

	# The only thing left to do is to adjust the contents of muls before returning from the function
	match_pos <- muls[kept_matches]
	match_lengths <- attr(muls, "match.length")[kept_matches]
	
	substring(string, first = match_pos, last = match_pos + match_lengths - 1)
}

# We now apply the function to each string
# We need to add do() in front because is enabled at first
# We also need to make sure to collapse the six input lines together
muls <- extract_matches(paste("do()", paste0(input, collapse = "")))

# The rest is just as for part A
first <- unlist(regmatches(muls, gregexpr("\\([0-9]{1,3}", muls)))
first <- as.numeric(substring(first, 2))
second <- unlist(regmatches(muls, gregexpr(",[0-9]{1,3}", muls)))
second <- as.numeric(substring(second, 2))

# SOLUTION TO PUZZLE 3B
sum(first * second)
