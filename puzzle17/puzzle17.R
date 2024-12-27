# Puzzle 17A
# Reading the input
input <- readLines("puzzle17.txt")

# Formatting the input
regA <- as.numeric(regmatches(input[1], gregexpr("[0-9]+", input[1])))
regB <- as.numeric(regmatches(input[2], gregexpr("[0-9]+", input[2])))
regC <- as.numeric(regmatches(input[3], gregexpr("[0-9]+", input[3])))
program <- as.numeric(regmatches(input[5], gregexpr("[0-9]+", input[5]))[[1]])

# A function that processes a program and returns its outputs and status
prog <- function(program, regA, regB, regC) {

	# The status of the program is initialized from its inputs
	pstat <- list(prog = program, rega = regA, regb = regB, regc = regC, inst = 0, output = numeric())

	# Looping over the instructions as long as the program does not halt
	# The program runs until the instruction pointer goes past the limit
	while(pstat$inst < length(program)) {
		# The modification to the program's status depends on the instruction
		instr <- program[pstat$inst + 1]

		if(instr %in% c(0, 6, 7)) {
			pstat <- div(pstat, instr)
		} else if(instr == 1) {
			pstat <- bxl(pstat)
		} else if(instr == 2) {
			pstat <- bst(pstat)
		} else if(instr == 3) {
			pstat <- jnz(pstat)
		} else if(instr == 4) {
			pstat <- bxc(pstat)
		} else if(instr == 5) {
			pstat <- out(pstat)
		} else {
			stop("Unrecognized input")
		}
	}

	return(pstat)
}

# A function that gets the combo operand of a digit
combo <- function(status) {
	digit <- status$prog[status$inst + 2]
	if(digit %in% c(0, 1, 2, 3)) return(digit)

	if(digit == 4) return(status$rega)
	if(digit == 5) return(status$regb)
	if(digit == 6) return(status$regc)

	stop("Invalid operand")
}

# A function that performs division (adv, bdv and cdv operations)
div <- function(pstat, operand) {
	numerator <- pstat$rega
	denominator <- 2^combo(pstat)
	result <- floor(numerator / denominator)

	# Where the result is stored depends on the operand
	if(operand == 0) {
		pstat$rega <- result
	} else if(operand == 6) {
		pstat$regb <- result
	} else if(operand == 7) {
		pstat$regc <- result
	}

	# Increasing the instruction pointer before returning
	pstat$inst <- pstat$inst + 2
	pstat
}

# A function that converts a number to a vector of bits
to_bits <- function(n) {
	if(n > 2^63) stop("Number to big for conversion to bits")
	output <- numeric(64)

	for(i in 63:0) {
		if(n >= 2^i) {
			output[i + 1] <- 1
			n <- n - 2^i
		}
	}

	output
}

# A function that converts a vector of bits to 10-base numerical value
# The vector of bits must be 64 bits
to_decimal <- function(n) {
	sum(2^(0:63) * n)
}

# A function that performs the bxl instruction (bitwise XOR of B and literal operand)
bxl <- function(pstat) {
	operand <- pstat$prog[pstat$inst + 2]

	# Computing the bitwise xor
	result <- to_decimal(xor(to_bits(pstat$regb), to_bits(operand)))
	pstat$regb <- result

	# Increasing the instruction pointer before returning
	pstat$inst <- pstat$inst + 2
	pstat
}

# A function that performs the bst operation (combo operand modulo 8)
bst <- function(pstat) {
	pstat$regb <- combo(pstat) %% 8

	# Increasing the instruction pointer before returning
	pstat$inst <- pstat$inst + 2
	pstat
}

# A function that performs the jnz (jump) operation
jnz <- function(pstat) {
	# Does nothing if register A is 0
	if(pstat$rega == 0) {
		# Increasing the instruction pointer before returning
		pstat$inst <- pstat$inst + 2
		return(pstat)
	}

	# Jumping to the value of the literal operand
	pstat$inst <- pstat$prog[pstat$inst + 2]
	pstat
}

# A function that performs the bxc instruction (bitwise XOR of registers B and C)
bxc <- function(pstat) {
	pstat$regb <- to_decimal(xor(to_bits(pstat$regb), to_bits(pstat$regc)))

	# Moving the instruction pointer before returning
	pstat$inst <- pstat$inst + 2
	pstat
}

# A function that performs the out operation
out <- function(pstat) {
	output <- combo(pstat) %% 8
	pstat$output <- c(pstat$output, output)

	# Moving the instruction pointer before returning
	pstat$inst <- pstat$inst + 2
	pstat
}

output <- prog(program, regA, regB, regC)

# SOLUTION TO PUZZLE 17A
paste0(output$output, collapse = ",")

# Puzzle 17B

# Testing incremental values of register A until we meet the condition
# There seems to be a periodicity by which the values output change with each power of 8
# We start at 8^15 because it is the lowest value at which the program outputs 16 digits
# We use a recursive function that progressively increases register A by some product of a power of 8 until a solution is found
search_value <- function(start_val, start_exponent, program = program) {
	# We start from the biggest exponent and explore all the possibilities recursively
	values <- rep(Inf, 8)

	for(i in 0:7) {
		output <- prog(program, regA = start_val + 8^start_exponent * i, regB = 0, regC = 0)$output

		if(identical(program, output)) {
			values[i + 1] <- start_val + 8^start_exponent * i
			message("Solution found for register A: ", start_val + 8^start_exponent * i)
		} else if(all(output[(start_exponent + 1):length(program)] == program[(start_exponent + 1):length(program)])) {
			values[i + 1] <- search_value(start_val = start_val + 8^start_exponent * i,
						      start_exponent = start_exponent - 1,
						      program = program)
		}
	}

	return(min(values))
}

options(scipen = 16)

# SOLUTION TO PUZZLE 17B
search_value(start_val = 8^15, start_exponent = 15, program = program)
