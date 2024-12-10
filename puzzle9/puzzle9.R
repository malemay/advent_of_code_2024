# Puzzle 9A

# Reading and formatting the input
input <- readLines("puzzle9.txt")
input <- strsplit(input, "")[[1]]

# Decompressing the disk layout
layout <- lapply(0:(length(input) - 1), function(x, disk) {
			 # If the index is divisible by 2 then it is a file
			 # and its ID is 2 times the index
			 if(x %% 2 == 0) return(rep(as.character(x / 2), disk[x + 1]))

			 # Otherwise we return as many dots as the number
			 rep(".", disk[x + 1])
}, disk = input)

layout <- unlist(layout)

# Let's see how many blocks are empty
table(layout == ".")

# To simplfy the task, I will fill all empty blocks with the
# appropriate digit and then convert the spaces at the end back to empty 
n_empty <- sum(layout == ".")

# Making a copy of the layout which we will modify
layout_copy <- layout

# Getting the indices of the empty blocks and filling them with the intended digit
empty <- which(layout == ".")
layout_copy[empty] <- rev(layout[layout != "."])[1:length(empty)]

# Replacing the spots at the end with empty spaces
layout_copy[(length(layout_copy) - n_empty + 1):length(layout_copy)] <- "."

# SOLUTION TO PUZZLE 9A
options(scipen = 10)
sum(as.numeric(layout_copy) * 0:(length(layout_copy) - 1), na.rm = TRUE)

# PUZZLE 9B

# It will help to use 4 vectors to keep track of the state of the disk
# The first vector is the layout of the disk in compressed space
compressed <- as.numeric(input)

# The second vector is the expanded layout of the disk
expanded <- layout

# The third vector is the index of the first location of a file/empty spot on disk
first <- cumsum(c(1, compressed[-length(compressed)]))

# The fourth vector contains the ID of the files starting at a given spot in the compressed space
ids <- as.character(rep(0:9999, each = 2))

# Now we will apply the disk compaction algorithm by keeping
# track of the state of the system in all those spaces

# We loop over files, starting from the last one
for(file_index in seq(length(compressed), 1, by = -2)) {
	# We loop over the empty spots (even indices) and exit when we find a spot
	for(free_index in seq(2, max(file_index, 2), by = 2)) {
		# The case when we have enough space to fit the file
		if(compressed[file_index] <= compressed[free_index]) {
			# Moving this file's numbers to the free spot
			expanded[first[free_index]:(first[free_index] + compressed[file_index] - 1)] <- ids[file_index]
			expanded[first[file_index]:(first[file_index] + compressed[file_index] - 1)] <- "."

			# We need to update the compressed and first vectors
			compressed[free_index] <- compressed[free_index] - compressed[file_index]
			first[free_index] <- first[free_index] + compressed[file_index]

			# We can move to the next file
			break
		}
	}
}

# SOLUTION TO PUZZLE 9B
sum(as.numeric(expanded) * 0:(length(expanded) - 1), na.rm = TRUE)
