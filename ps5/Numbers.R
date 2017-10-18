# Numbers
library(pryr)
bits('a')

bits(0)
bits(1)
bits(2)
bits(3)
bits(4)
bits(5)
bits(6)
bits(7)
bits(8)
bits(9)
bits(10)
bits(11)

bytes(0)
bytes(1)
bytes(2)

bits(0L)
bits(1L)
bits(2L)
bits(3L)
bits(4L)
bits(5L)
bits(6L)
bits(7L)
bits(8L)

bits(-1L)
bytes(-1L)


# Machine info
.Machine$double.max.exp
.Machine$sizeof.longdouble
.Machine$double.eps
.Machine$integer.max


# Numbers: 4 bytes, each byte 8 bits, 32 bits in total, then 2^32 (max number) 
# - the sign of the number (one bit), thus, 2^32 - 1 is the max number 

# Precision
0.3 + 0.1 == 0.4  # True
0.3 - 0.2 == 0.1  # False 

# mistery!!!
0.2 + 0.3 == 0.5    # True
0.01 + 0.49 == 0.5  # True
0.3 == 0.2 + 0.1    # False

i <- 0.1 + 0.05
if(isTRUE(all.equal(i,0.15))) cat("i equals 0.15") else cat("i does not equal 0.15")

j <- 0.2 + 0.1
if(isTRUE(all.equal(j,0.3))) cat("j equals 0.3") else cat("j does not equal 0.3")

options(digits = 22)  # Up to 16 in windows
0.3
0.2
