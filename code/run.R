# ----------------------------------------------------------------------
# Information
# ----------------------------------------------------------------------
#
# (Title)  Simulation of Spread of Misinformation
# (File)   Run simulation
#
# (Course) 14D009 - Project
# (Author) Hans-Peter Höllwirth
# (Date)   06.2017


# ----------------------------------------------------------------------
# Setup
# ----------------------------------------------------------------------

# house cleaning
rm(list = ls())
par(mfrow=c(1,1), mar=c(4,4,1,1))
set.seed(1000)

# if interactive, during the development, set to TRUE
interactive <- FALSE
if (interactive) {
    setwd("/Users/Hans-Peter/Documents/Masters/14D009/project/code")
} 

# load libraries and files
library(igraph)

source('network.R')
source('simulation.R')

# ----------------------------------------------------------------------
# Simulate Spread of (mis)information 
# ----------------------------------------------------------------------
# - Network nodes represent agents and weighted links denote the possibility that the 
#   linked agents meet (the higher the weight, the more likely these two agents meet).
# - Initially, each agent observes a noisy representation of the (numerical) „truth“.
# - Optionally, a (small) subset of agents might be initialized with a misinformation.
# - Each node has a „forceful“ factor that determines, how strongly the node insists on its current belief. 

# (1) generate (or load) random network
d <- c(2,2,3,1,3,1,4,4,4,2,1,5)
#G <- gen.network(d, same.weights = TRUE)
#G <- load.network('G10')
G <- gen.network.exp(12)
n <- vcount(G)
plot(G, edge.width=E(G)$weight)

# (2) assign initial belief (and misinformation)
beliefs.init <- rnorm(n, mean=0, sd=1)

# (3) simulate exchange of information
beliefs <- simExchange(G, beliefs.init, T=100)$final

# (4) summarize process
result <- data.frame(matrix(nrow=n, ncol=3), row.names=(1:n))
colnames(result) <- c('beliefs.init', 'beliefs.T', 'beliefs.diff')
result[,1] <- beliefs.init
result[,2] <- beliefs
result[,3] <- abs(beliefs.init - mean(beliefs))

# (5) document convergence in presence of misinformation
T <- 500
conv.table <- misinfo.impact(G, beliefs.init, T)
plot.convergence(conv.table)

# (6) save network
save.network(G,'exp12_01')




