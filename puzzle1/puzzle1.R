# Puzzle 1A
# Reading the input
input <- read.table("puzzle1.txt")

# SOLUTION TO PUZZLE 1A
sum(abs(sort(input[[1]]) - sort(input[[2]])))

# Puzzle 1B
# First we remove all numbers in the first list that are not in the second one
list_one <- input[[1]][input[[1]] %in% input[[2]]]

# Then we compute a table of occurrences in list 2
# and extract inputs that contain any of the numbers in list 1
table_two <- table(input[[2]])
table_two <- table_two[as.numeric(names(table_two)) %in% list_one]

# SOLUTION TO PUZZLE 1B
sum(as.numeric(names(table_two)) * as.numeric(table_two))
