#=====================================================================================
# Sample program to compare trap data for similarities. These presently include common 
# sequences of observations or high numbers of common elements.
# Be careful of NA values or traps whose number of observations are not equal.
#=====================================================================================
# High degrees of similarity are indicative of either correlations or low variability 
# between observations. 
# They may indicate data copying, falsification or data rounding.
#=====================================================================================

source("repvec.R")
source("concatenate.R")
source("dbarplot.R")
source("number.common.elements.R")
source("longest.common.sequence.R")
source("compare.traps.R")
source("rplot.R")
source("chela.plot.R")

# Load example data:
x <- read.csv("Example Data.csv", header = TRUE, stringsAsFactors = FALSE)

# Sort data:
index <- order(x$trip.number, x$trap.number, x$crab.number)
x <- x[index, ]

# Define variables which define individual traps:
trap.id <- x[c("trip.number", "trap.number")]

# Compare the observations from two individual traps:
i <- which((x$trip.number == 1) & (x$trap.number == 1))
j <- which((x$trip.number == 1) & (x$trap.number == 2))

# Number of common data observations and common sequences for a single variable:
n <- number.common.elements(x[i, "carapace.width"], x[j, "carapace.width"])
s <- longest.common.sequence(x[i, "carapace.width"], x[j, "carapace.width"])

# Number of common data observations and common sequences for multiple variables:
vars <-  c("carapace.width", "chela.height.right")
n <- number.common.elements(x[i,vars], x[j, vars])
s <- longest.common.sequence(x[i, vars ], x[j, vars ])

# Perform full analysis:
# 'threshold' sets the minimum number of observations so that a pair of traps is retained. 
# 'fun' defines the function which is used to compare the data from each pair of traps 
# (i.e. 'number.common.elements' or 'longest.common.sequence')
r <- compare.traps(x[vars], trap.id = trap.id, threshold = 20)

# Display simularity matrix:
image(r$similarity.matrix)

# Show trap pairs with similarity exceeding limit set by 'threshold':
print(r$top.similarities)

# Check carapace width for rounding:
rplot(x$carapace.width, reference = 95)

# Check durometer for rounding:
rplot(x$durometer, reference = 68)

windows()
chela.plot(x$carapace.width, x$chela.height.right, scale = 3)


