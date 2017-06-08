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
    beliefs.hist <- matrix(nrow=vcount(G), ncol=T+1)
    beliefs.hist[,1] <- beliefs <- beliefs.init
    
    meets <- sample(length(E(G)), T, replace=TRUE, prob=E(G)$weight)
    for (t in 1:T) {
        sd.belief[t] <- sd(beliefs)
        meeters <- ends(G, E(G)[meets[t]])
        beliefs[meeters] <- mean(beliefs[meeters])
        beliefs.hist[,t+1] <- beliefs
    }
    
    sd.belief[T+1] <- sd(beliefs)
    return(list(beliefs.final=beliefs, beliefs.hist=beliefs.hist, sd.convergence=sd.belief))
}

# document the convergence of the group belief's standard deviation for different spreaders of misinformation
misinfo.impact <- function(G, beliefs.init, T=1000) {
    n <- vcount(G)
    conv.table <- matrix(nrow=n, ncol=T+1)
    
    # spread misinformation
    for (i in 1:n) {
        beliefs.start <- beliefs.init
        beliefs.start[i] <- 10
        conv.table[i,] <- simExchange(G, beliefs.start, T)$sd.convergence
    }
    return(conv.table)
}

# plot convergence of the group belief's standard deviation 
plot.sd.convergence <- function(conv.table) {
    n <- nrow(conv.table)
    T <- ncol(conv.table)
    
    col.scheme <- rainbow(n)
    plot(conv.table[1,], type='l', xlab='time t', ylab='standard deviation', ylim=c(0,max(conv.table)), col=col.scheme[1])
    for (i in 2:n) {
        lines(conv.table[i,], col=col.scheme[i])
    }
    text(x=(T+2), y=conv.table[,T], pos=4, labels=(1:n), cex=0.6)
    #legend(T-50, max(conv.table)-0.1, legend=(1:n), lty=rep(1,n), lwd=rep(2.5,n), col=col.scheme, cex=0.7)
}

# plot convergence of the group belief's standard deviation 
plot.convergence <- function(beliefs.hist) {
    n <- nrow(beliefs.hist)
    T <- ncol(beliefs.hist)
    
    col.scheme <- rainbow(n)
    plot(beliefs.hist[1,], type='l', xlab='time t', ylab='belief', ylim=c(min(beliefs.hist),max(beliefs.hist)), col=col.scheme[1])
    for (i in 2:n) {
        lines(beliefs.hist[i,], col=col.scheme[i])
    }
    text(x=-3, y=beliefs.hist[,1], pos=4, labels=(1:n), cex=0.6)
    #lines(rep(mean(beliefs.hist[1,]),T), col='black')
    #legend(T-50, max(conv.table)-0.1, legend=(1:n), lty=rep(1,n), lwd=rep(2.5,n), col=col.scheme, cex=0.7)
}



