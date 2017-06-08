# ----------------------------------------------------------------------
# Information
# ----------------------------------------------------------------------
#
# (Title)  Simulation of Spread of Misinformation
# (File)   Simulation functions
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
# Simulation functions
# ----------------------------------------------------------------------

# simulate information exchange
simExchange <- function(G, beliefs.init, T=1000) {
    sd.belief <- rep(0,T+1)
    beliefs <- beliefs.init
    
    meets <- sample(length(E(G)), T, replace=TRUE, prob=E(G)$weight)
    for (t in 1:T) {
        sd.belief[t] <- sd(beliefs)
        meeters <- ends(G, E(G)[meets[t]])
        beliefs[meeters] <- mean(beliefs[meeters])
    }
    
    sd.belief[T+1] <- sd(beliefs)
    return(list(final=beliefs, convergence=sd.belief))
}

# document the convergence to mean belief for different spreaders of misinformation
misinfo.impact <- function(G, beliefs.init, T=1000) {
    n <- vcount(G)
    conv.table <- matrix(nrow=n, ncol=T+1)
    
    # spread misinformation
    for (i in 1:n) {
        beliefs.start <- beliefs.init
        beliefs.start[i] <- 10
        conv.table[i,] <- simExchange(G, beliefs.start, T)$convergence
    }
    return(conv.table)
}




