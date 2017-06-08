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
gen.network <- function(d) {
    # generate random (unweighted) graph with topology described by degree vector d
    fully.connected <- FALSE
    while(!fully.connected) {
        G <- sample_degseq(d, method='simple.no.multiple')
        
        # assign random weights to edges (TBD: use different distribution)
        for (e in 1:length(E(G))) {
            E(G)$weight[e] <- ceiling(runif(1)*10)
        }
        E(G)$label <- E(G)$weight
        
        # make sure network is fully connected
        if (components(G)$no == 1)
            fully.connected <- TRUE
    }
    return(G)
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




