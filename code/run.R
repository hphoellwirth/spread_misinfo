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

# load libraries and files
library(igraph)

source('network.R')
source('simulation.R')

# if interactive, during the development, set to TRUE
interactive <- FALSE
if (interactive) {
    setwd("/Users/Hans-Peter/Documents/Masters/14D009/project/code")
} 

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
#d <- sample(1:100, 100, replace=TRUE, prob=exp(-0.5*(1:100))) # exponential degree distribution
#d <- sample(1:100, 100, replace=TRUE, prob=(1:100)^-2) # power-law degree distribution

G <- gen.network(d)
G <- load.network('G10')
n <- vcount(G)
plot(G, edge.width=E(G)$weight)

# (2) assign initial belief (and misinformation)
beliefs.init <- beliefs <- rnorm(n, mean=0, sd=1)

# (3) simulate exchange of information
beliefs <- simExchange(G, beliefs.init, T=100)$final

# (4) summarize process
result <- data.frame(matrix(nrow=n, ncol=3), row.names=(1:n))
colnames(result) <- c('beliefs.init', 'beliefs.T', 'beliefs.diff')
result[,1] <- beliefs.init
result[,2] <- beliefs
result[,3] <- abs(beliefs.init - mean(beliefs))

# (5) document convergence in presence of misinformation
conv.table <- misinfo.impact(G, beliefs.init, T=200)
col.scheme <- rainbow(n)
plot(conv.table[1,], type='l', xlab='time t', ylab='standard deviation', ylim=c(0,max(conv.table)), col=col.scheme[1])
for (i in 2:n) {
    lines(conv.table[i,], col=col.scheme[i])
}
legend(140, max(conv.table)-0.1, legend=(1:n), lty=rep(1,n), lwd=rep(2.5,n), col=col.scheme)

# (6) save network
save.network(G,'G10')




