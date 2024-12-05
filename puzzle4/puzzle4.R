# Puzzle 4A

# Read the input
input <- readLines("puzzle4.txt")

### First we define a few functions

# A function that counts the number of XMAS/SAMX in a string
xc <- function(x) {
	# Getting the occurrences of each pattern
	xmas <- gregexpr("XMAS", x)[[1]]
	samx <- gregexpr("SAMX", x)[[1]]

	# Counting them accounting for the fact that no match gives a value of -1
	n_xmas <- if(length(xmas) == 1 && xmas == -1) 0 else length(xmas)
	n_samx <- if(length(samx) == 1 && samx == -1) 0 else length(samx)

	n_xmas + n_samx
}

# A function that collapses a vector of letters into a single string
coll <- function(x) paste0(x, collapse = "")

# We sum the number of matches in horizontal lines
# This is the easiest case because the characters are already collapsed
n_horiz <- sum(sapply(input, xc))

# For the other axes we need to first format the input as a matrix
i_matrix <- do.call("rbind", strsplit(input, ""))

# We sum the number of matches in vertical lines
n_vert <- sum(sapply(apply(i_matrix, 2, coll), xc))

# For the diagonals we will extract subsets of the matrix
# and leverage the diag function to extract the diagonal
n_diag <- 0

# Adding the main diagnonal
n_diag <- n_diag + xc(coll(diag(i_matrix)))


# We get all diagonals that go downwards
for(i in 1:(nrow(i_matrix) - 2)) {
	# Diagonals down from the main diagonal (starting top-left)
	matrix_subset <- i_matrix[(i + 1):nrow(i_matrix), 1:(ncol(i_matrix) - i)]
	n_diag <- n_diag + xc(coll(diag(matrix_subset)))

	# Diagonals up from the main diagonal (starting top-left)
	matrix_subset <- i_matrix[1:(nrow(i_matrix) - i), (i + 1):ncol(i_matrix)]
	n_diag <- n_diag + xc(coll(diag(matrix_subset)))
}

# Doing the same thing on the mirrored matrix so we can diagonals going the other side
m_matrix <- i_matrix[, ncol(i_matrix):1]

# Adding the other main diagonal
n_diag <- n_diag + xc(coll(diag(m_matrix)))

# We get all diagonals that go downwards
for(i in 1:(nrow(m_matrix) - 2)) {
	# Diagonals down from the main diagonal (starting top-left)
	matrix_subset <- m_matrix[(i + 1):nrow(m_matrix), 1:(ncol(m_matrix) - i)]
	n_diag <- n_diag + xc(coll(diag(matrix_subset)))

	# Diagonals up from the main diagonal (starting top-left)
	matrix_subset <- m_matrix[1:(nrow(m_matrix) - i), (i + 1):ncol(m_matrix)]
	n_diag <- n_diag + xc(coll(diag(matrix_subset)))
}

# SOLUTION TO PUZZLE 4A
n_horiz + n_vert + n_diag

# PUZZLE 4B

# This time we will not be using regular expressions
# Instead I will just compare sub-matrices from the big matrix
# to templates that match the X-MAS pattern
patterns <- list(matrix(c("M", NA, "M",
			  NA, "A", NA,
			  "S", NA, "S"),
			nrow = 3, ncol = 3, byrow = TRUE),

		 matrix(c("M", NA, "S",
			  NA, "A", NA,
			  "M", NA, "S"),
			nrow = 3, ncol = 3, byrow = TRUE),

		 matrix(c("S", NA, "S",
			  NA, "A", NA,
			  "M", NA, "M"),
			nrow = 3, ncol = 3, byrow = TRUE),

		 matrix(c("S", NA, "M",
			  NA, "A", NA,
			  "S", NA, "M"),
			nrow = 3, ncol = 3, byrow = TRUE))

# A function that compares a sub-matrix against a pattern
compare <- function(x, pattern) {
	all(x == pattern, na.rm = TRUE)
}

compare_patterns <- function(x, patterns) {
	n <- 0

	for(i in patterns) if(compare(x, i)) return(1)

	return(0)
}

# Now we simply need to check the patterns against all sub-matrices
n_matches <- 0

for(i in 1:(nrow(i_matrix) - 2)) {
	for(j in 1:(ncol(i_matrix) - 2)) {
		n_matches <- n_matches + compare_patterns(i_matrix[i:(i + 2), j:(j + 2)], patterns)
	}
}

# SOLUTION TO PUZZLE 4B
n_matches
