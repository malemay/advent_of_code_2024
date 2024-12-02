# Puzzle 2A

# Reading the input and formatting it
input <- readLines("puzzle2.txt")
input <- lapply(strsplit(input, " "), as.numeric)

# Computing the differences between successive numbers
diffs <- lapply(input, diff)

# We need to check that :
# - all values have the same sign and all
# - differences are at least one and at most three
is_safe <- function(x) length(unique(sign(x))) == 1 && all(abs(x) >= 1) & all(abs(x) <= 3)
safe <- sapply(diffs, is_safe)

# SOLUTION TO PUZZLE 2A
sum(safe)

# Puzzle 2B

# We can filter out inputs for which we already know they are fine
input <- input[!safe]

# Now we can test them by removing one number at a time and checking if it is safe
safe2 <- sapply(input, function(x) {
			for(i in 1:length(x)) {
				if(is_safe(diff(x[-i]))) return(TRUE)
			}

			return(FALSE)
})

# SOLUTION TO PUZZLE 2B
sum(safe) + sum(safe2)

