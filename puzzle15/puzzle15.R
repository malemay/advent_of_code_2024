# PUZZLE 15A

# Reading the input and splitting it into two parts
input <- readLines("puzzle15.txt")

map <- grep("#", input, value = TRUE)
map <- do.call(rbind, strsplit(map, ""))

moves <-grep("#", input, value = TRUE, invert = TRUE) 
moves <- paste0(moves, collapse = "")

# A function that processes one move of the robot and returns the state of the map
move_robot <- function(map, move) {
	# Finding the position of the robot
	robot_index <- map == "@"
	robot <- c(row(map)[robot_index], col(map)[robot_index])

	# We set the direction of the next move
	move_list <- list("^" = c(-1, 0),
			  ">" = c(0, 1),
			  "v" = c(1, 0),
			  "<" = c(0, -1))

	move <- move_list[[move]]
	next_pos <- robot + move

	# We find the item at the position where the robot should move
	# We do not need to check for off-bounds moves because the map is lined with walls (#)
	item <- map[next_pos[1], next_pos[2]]

	# Simplest case: running into a wall, nothing changes
	if(item == "#") return(map)

	# Second simplest case: moving into an empty spot
	if(item == ".") {
		map[robot[1], robot[2]] <- "."
		map[next_pos[1], next_pos[2]] <- "@"
	}

	# Last case: moving boxes
	# We need to isolate the sequence of characters in front of the robot
	# to determine if and how much the robot is going to move
	if(item == "O") {
		# The case of vertical movement
		if(move[1] != 0) {
			# We can reflect the map along the horizontal axis if going up
			# This way the manipulations are the same whether
			# the robot is going up or down
			if(move[1] == -1) {
				map <- map[nrow(map):1, ]
				next_pos[1] <- nrow(map) - next_pos[1] + 1
				robot[1] <- nrow(map) - robot[1] + 1
			}

			chars <- map[next_pos[1]:nrow(map), next_pos[2]]
			nb <- nboxes(chars)

			if(nb > 0) {
				# Boxes are pushed
				map[next_pos[1]:(next_pos[1] + nb), next_pos[2]] <- "O"
				# The robot's initial position is freed
				map[robot[1], robot[2]] <- "."
				# The robot moves to its next position
				map[next_pos[1], next_pos[2]] <- "@"
			}

			# Reflecting the map back along the horizontal axis (if necessary)
			if(move[1] == -1) map <- map[nrow(map):1, ]
		}
		# The case of horizontal movement
		else if(move[2] != 0) {
			# We reflect the map along the vertical axis if going left
			if(move[2] == -1) {
				map <- map[, ncol(map):1]
				next_pos[2] <- ncol(map) - next_pos[2] + 1
				robot[2] <- ncol(map) - robot[2] + 1
			}

			chars <- map[next_pos[1], next_pos[2]:ncol(map)]
			nb <- nboxes(chars)

			if(nb > 0) {
				map[next_pos[1], next_pos[2]:(next_pos[2] + nb)] <- "O"
				map[robot[1], robot[2]] <- "."
				map[next_pos[1], next_pos[2]] <- "@"
			}

			# Reflecting the map back along the vertical axis (if necessary)
			if(move[2] == -1) map <- map[, ncol(map):1]
		} 
	}

	return(map)
}

# A function that determines how many boxes should be moved given a sequence of characters
nboxes <- function(chars) {
	# We use rle to compress the sequence of characters
	rle_chars <- rle(chars)
	
	# If there is a wall right after the boxes then we move no boxes at all
	if(rle_chars$values[2] == "#") return(0)

	# Otherwise we move as many boxes are there are before the next free spot
	stopifnot(rle_chars$values[1] == "O")
	return(rle_chars$lengths[1])
}

# Moving the robot according to the sequence of moves
map_copy <- map

for(i in 1:nchar(moves)) {
	i_move <- substring(moves, i, i)
	map_copy <- move_robot(map_copy, i_move)
}

# Getting the indices of the boxes
box_coords <- cbind(row(map_copy)[map_copy == "O"], col(map_copy)[map_copy == "O"])

# SOLUTION TO PUZZLE 15A
sum((box_coords[, 1] - 1) * 100 + (box_coords[, 2] - 1))

