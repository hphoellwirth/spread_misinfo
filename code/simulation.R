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
# Simulation function without forceful agents
# ----------------------------------------------------------------------

# simulate information exchange
sim.exchange <- function(G, beliefs.init, T=1000) {
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

# ----------------------------------------------------------------------
# Simulation function with potential forceful agents
# ----------------------------------------------------------------------

# generate forceful factor matrices
gen.forceful.factors <- function(G, forceful, probs) {
    A <- m.alpha <- m.beta <- m.gamma <- as_adjacency_matrix(G)
    
    for (i in 1:nrow(A)) {
        for (j in 1:ncol(A)) {
            if (A[i,j] == 1) {
                if (forceful[i] && forceful[j]) {
                    m.alpha[i,j] <- 0
                    m.beta[i,j]  <- 0
                    m.gamma[i,j] <- 1
                } else if (forceful[i] || forceful[j]) {
                    m.alpha[i,j] <- probs[1]
                    m.beta[i,j]  <- probs[2]
                    m.gamma[i,j] <- probs[3]                  
                } else {
                    m.alpha[i,j] <- 0
                    m.beta[i,j]  <- 1
                    m.gamma[i,j] <- 0                    
                }
            }
        }
    }
    return(list(alpha=m.alpha, beta=m.beta, gamma=m.gamma))
}

# simulate a meeting between 2 agents i and j
sim.meeting <- function(i.belief, j.belief, i.forceful, forceful.prob, eps) {
    draw <- runif(1, min=0, max=1)
    
    # one-sided influence (note: if alpha >0, either i or j must be forceful)
    if (draw < forceful.prob$alpha) {
        if (i.forceful) { 
            i.new.belief <- i.belief
            j.new.belief <- eps * (i.belief) + (1-eps) * (j.belief)
        } else {
            i.new.belief <- eps * (j.belief) + (1-eps) * (i.belief)
            j.new.belief <- j.belief
        }
    } 
    # pairwise concensus
    else if (draw < (forceful.prob$alpha + forceful.prob$beta)) {
        i.new.belief <- j.new.belief <- mean(c(i.belief, j.belief))
    }
    # both stick to their beliefs
    else {
        i.new.belief <- i.belief
        j.new.belief <- j.belief      
    }
    return(list(i.belief=i.new.belief, j.belief=j.new.belief))
}

# simulate information exchange including forceful agents
sim.exchange.forceful <- function(G, beliefs.init, forceful.agents, forceful.probs=c(0.6,0.3,0.1), eps=0.5, T=1000) {
    sd.belief <- rep(0,T+1)
    beliefs.hist <- matrix(nrow=vcount(G), ncol=T+1)
    beliefs.hist[,1] <- beliefs <- beliefs.init
    
    # generate forceful factor matrices
    forceful <- rep(FALSE, vcount(G))
    forceful[forceful.agents] <- TRUE
    ff <- gen.forceful.factors(G, forceful, forceful.probs)
    
    # simulate meetings between agents
    meets <- sample(length(E(G)), T, replace=TRUE, prob=E(G)$weight)
    for (t in 1:T) {
        sd.belief[t] <- sd(beliefs)
        meeters <- ends(G, E(G)[meets[t]])
        i <- meeters[1]
        j <- meeters[2]
        
        forceful.prob <- list(alpha=ff$alpha[i,j], beta=ff$beta[i,j], gamma=ff$gamma[i,j])
        new.beliefs <- sim.meeting(beliefs[i], beliefs[j], forceful[i], forceful.prob, eps=eps)
        
        beliefs[i] <- new.beliefs$i.belief
        beliefs[j] <- new.beliefs$j.belief
        #beliefs[i] <- beliefs[j] <- mean(c(beliefs[i], beliefs[j]))
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
        conv.table[i,] <- sim.exchange(G, beliefs.start, T)$sd.convergence
    }
    return(conv.table)
}






