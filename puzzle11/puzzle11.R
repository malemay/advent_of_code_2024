# Puzzle 11A

# Reading the input
input <- readLines("puzzle11.txt")
input <- as.numeric(strsplit(input, " ")[[1]])

# A function that uses nested lists to implement the blinking routine
blink <- function(input) {
	# We loop over the elements of the input
	# It is a recursive function that splits elements into lists and jumps into new lists
	for(i in 1:length(input)) {

		if(is.list(input[[i]])) {
			input[[i]] <- blink(input[[i]])
		} else {
			if(input[[i]] == 0) {
				input[[i]] <- 1
			} else if((i_nchar <- nchar(input[[i]])) %% 2 == 0) {
				input[[i]] <- as.list(as.numeric(substring(as.character(input[[i]]), c(1, i_nchar / 2 + 1), c(i_nchar / 2, i_nchar))))
			} else {
				input[[i]] <- input[[i]] * 2024
			}
		}
	}

	input
}

# Blinking 25 times on the puzzle input and keeping track of the length
input_copy <- as.list(input)

for(i in 1:25) {
	input_copy <- blink(input_copy)
	message("Iteration ", i, ": list contains ", length(unlist(input_copy)), " numbers")
}

# SOLUTION TO PUZZLE 11A
length(unlist(input_copy))

# Puzzle 11B

# We can actually just keep track of the frequency of each number
# since there are acutally only a few unique numbers
blink2 <- function(input) {
	# We loop over the elements of the list
	# The names of the elements are the numbers and their values are their frequency

	# Initializing a list that will contain the output values
	output <- list()

	for(i in names(input)) {
		if(i == "0") {
			output[["1"]] <- input[[i]] + if(is.null(output[["1"]])) 0 else output[["1"]]
		} else if(nchar(i) %% 2 == 0) {
			new_nums <- substring(i, c(1, nchar(i) / 2 + 1), c(nchar(i) / 2, nchar(i)))
			new_nums <- as.character(as.numeric(new_nums))
			output[[new_nums[1]]] <- input[[i]] + if(is.null(output[[new_nums[1]]])) 0 else output[[new_nums[1]]]
			output[[new_nums[2]]] <- input[[i]] + if(is.null(output[[new_nums[2]]])) 0 else output[[new_nums[2]]]
		} else {
			new_val <- as.character(as.numeric(i) * 2024)
			output[[new_val]] <- input[[i]] + if(is.null(output[[new_val]])) 0 else output[[new_val]]
		}
	}

	output
}

input <- list(`0` = 1, `27` = 1, `5409930` = 1, `828979` = 1, `4471` = 1, `3` = 1, `68524` = 1, `170` = 1)

for(i in 1:75) {
	input <- blink2(input)
	message("Iteration ", i, ": list contains ", sum(unlist(input)), " numbers")
}

# SOLUTION TO PUZZLE 11B
options(scipen = 15)
sum(unlist(input))
