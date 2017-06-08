# ----------------------------------------------------------------------
# Information
# ----------------------------------------------------------------------
#
# (Title)  Simulation of Spread of Misinformation
# (File)   Network functions
#
# (Course) 14D009 - Project
# (Author) Hans-Peter Höllwirth
# (Date)   06.2017


# ----------------------------------------------------------------------
# Setup
# ----------------------------------------------------------------------

# load libraries
library(igraph)

# ----------------------------------------------------------------------
# Network functions
# ----------------------------------------------------------------------

# generate a random, fully connected network
gen.network <- function(d, same.weights=FALSE) {
    # generate random (unweighted) graph with topology described by degree vector d
    fully.connected <- FALSE
    
    while (!fully.connected) {
        G <- sample_degseq(d, method='simple.no.multiple')
        
        # assign random weights to edges (TBD: use different distribution)
        if (!same.weights) {
            for (e in 1:length(E(G))) {
                E(G)$weight[e] <- ceiling(runif(1)*10)
            }
            E(G)$label <- E(G)$weight
        }
        
        # make sure network is fully connected
        if (components(G)$no == 1)
            fully.connected <- TRUE
    }
    return(G)
}

# generate a random network with exponential degree distribution
gen.network.exp <- function(n, same.weights=FALSE) {
    valid.distr <- FALSE
    
    while (!valid.distr) {
        d <- sample(1:n, n, replace=TRUE, prob=exp(-0.5*(1:n)))
        if (is_degseq(d)) 
            valid.distr <- TRUE
    }
    return(gen.network(d, same.weights))
}

# generate a random network with power law degree distribution
gen.network.power <- function(n, same.weights=FALSE) {
    # TBD: use sample_fitness_pl(12, 36, 2.2, 2.3)
    
    valid.distr <- FALSE
    
    while (!valid.distr) {
        d <- sample(1:n, n, replace=TRUE, prob=(1:n)^-2)
        if (is_degseq(d))
            valid.distr <- TRUE
    }
    return(gen.network(d, same.weights))
}

# save graph
save.network <- function(G, file) {
    write_graph(G, file=paste0('../graphs/',file), format='pajek')
}

# load saved network
load.network <- function(file) {
    G <- read_graph(file=paste0('../graphs/',file), format='pajek')
    E(G)$label <- E(G)$weight
    return(G)
}




