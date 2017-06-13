# ----------------------------------------------------------------------
# Information
# ----------------------------------------------------------------------
#
# (Title)  Simulation of Spread of Misinformation
# (File)   Plotting functions
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
# Plotting functions
# ----------------------------------------------------------------------

# plot network
plot.network <- function(G, forceful.agents=c(), colors=rep('orange',12)) {
    V(G)$color <- colors
    for (i in 1:length(forceful.agents)) {
        V(G)$color[forceful.agents[i]] <- 'green'
    }
    par(mfrow=c(1,1), mar=c(0,0,0,0))
    plot(G, edge.width=E(G)$weight, vertex.color=V(G)$color, asp=0.7)
}

# plot convergence of the agents' beliefs
plot.convergence <- function(beliefs.hist, col.scheme=rainbow(12), title='') {
    n <- nrow(beliefs.hist)
    T <- ncol(beliefs.hist)
    
    par(mfrow=c(1,1), mar=c(4,4,3,1))
    plot(beliefs.hist[1,], type='l', xlab='time t', ylab='belief', main=title, ylim=c(min(beliefs.hist),max(beliefs.hist)), col=col.scheme[1], lwd=2.5)
    for (i in 2:n) {
        lines(beliefs.hist[i,], col=col.scheme[i], lwd=2.5)
    }
    text(x=-10*round(T/300), y=beliefs.hist[,1], pos=4, labels=(1:n), cex=0.8)
    lines(rep(mean(beliefs.hist[,1]),T), col='black', lty=2)
    #legend(T-50, max(conv.table)-0.1, legend=(1:n), lty=rep(1,n), lwd=rep(2.5,n), col=col.scheme, cex=0.7)
}

# plot convergence of the mean belief 
plot.mean.convergence <- function(conv.table, col.scheme=rainbow(12), title='') {
    n <- nrow(conv.table)
    T <- ncol(conv.table)
    
    par(mfrow=c(1,1), mar=c(4,4,3,1))
    plot(conv.table[1,], type='l', xlab='time t', ylab='mean belief', main=title, ylim=c(min(conv.table),max(conv.table)), col=col.scheme[1], lwd=2.5)
    for (i in 2:n) {
        lines(conv.table[i,], col=col.scheme[i], lwd=2.5)
    }
    text(x=(T+2), y=conv.table[,T], pos=4, labels=(1:n), cex=0.8)
}

# plot convergence of the group belief's standard deviation 
plot.sd.convergence <- function(conv.table, col.scheme=rainbow(12), title='') {
    n <- nrow(conv.table)
    T <- ncol(conv.table)
    
    par(mfrow=c(1,1), mar=c(4,4,3,1))
    plot(conv.table[1,], type='l', xlab='time t', ylab='standard deviation', main=title, ylim=c(0,max(conv.table)), col=col.scheme[1], lwd=2.5)
    for (i in 2:n) {
        lines(conv.table[i,], col=col.scheme[i], lwd=2.5)
    }
    text(x=(T+2), y=conv.table[,T], pos=4, labels=(1:n), cex=0.6)
   # legend(0.80 * T, max(conv.table)-0.1, legend=(1:n), lty=rep(1,n), lwd=rep(2.5,n), col=col.scheme, cex=0.7)
}





