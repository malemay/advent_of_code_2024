# Puzzle 5A

# Reading the input
input <- readLines("puzzle5.txt")

# Extracting the rules and updates
rules <- grep("\\|", input, value = TRUE)
updates <- grep(",", input, value = TRUE)
updates <- strsplit(updates, ",")

# Formatting the rules as a lookup list that gives the number that come after each number
rules <- strsplit(rules, "\\|")
first_num <- sapply(rules, `[`, 1)
second_num <- sapply(rules, `[`, 2)

rule_list <- list()

for(i in unique(first_num)) {
	rule_list[[i]] <- second_num[first_num == i]
}

# A function that checks whether a given update matches the rules
check_rule <- function(update, rlist) {
	for(i in 1:(length(update) - 1)) {
		if(!all(update[(i + 1):length(update)] %in% rlist[[update[i]]])) return(FALSE)
	}

	return(TRUE)
}

# Getting the lists that do match all the rules
matching_updates <- sapply(updates, check_rule, rlist = rule_list)

# SOLUTION TO PUZZLE 5A
sum(sapply(updates[matching_updates], function(x) as.numeric(x[ceiling(length(x) / 2)])))


# First we subset the updates to get only those that need to be reordered
updates <- updates[!matching_updates]

# A function that reorders an update so that it matches the rules
reorder_update <- function(update, rlist) {

	n <- length(update)

	# We will store the reordered vector in this object
	output <- character(n)

	remaining_nums <- update

	# We find the next character until all are found
	for(i in 1:n) {
		i_solution <- find_first(remaining_nums, rlist = rlist)
		output[i] <- i_solution[[1]]
		remaining_nums <- i_solution[[2]]
	}

	output
}

# A function that finds the number that comes last among a vector
find_first <- function(nums, rlist) {
	
	for(i in 1:length(nums)) {
		if(all(nums[-i] %in% rlist[[ nums[i] ]])) break
	}

	return(list(nums[i], nums[-i]))
}

# We can use those functions to reorder all the updates that were not already properly ordered
updates <- lapply(updates, reorder_update, rlist = rule_list)

# SOLUTION TO PUZZLE 5B
sum(sapply(updates, function(x) as.numeric(x[ceiling(length(x) / 2)])))
