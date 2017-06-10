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
save.plots <- FALSE

# if interactive, during the development, set to TRUE
interactive <- FALSE
if (interactive) {
    setwd("/Users/Hans-Peter/Documents/Masters/14D009/project/code")
} 

# load libraries and files
library(igraph)

source('network.R')
source('simulation.R')
source('plot.R')

# ----------------------------------------------------------------------
# Simulate Spread of (mis)information 
# ----------------------------------------------------------------------
# - Network nodes represent agents and weighted links denote the possibility that the 
#   linked agents meet (the higher the weight, the more likely these two agents meet).
# - Initially, each agent observes a noisy representation of the (numerical) „truth“.
# - Optionally, a (small) subset of agents might be initialized with a misinformation.
# - Each node has a „forceful“ factor that determines, how strongly the node insists on its current belief. 

# generate/load/save random network
#d <- c(2,2,3,1,3,1,4,4,4,2,1,5)
#G <- gen.network(d, same.weights = TRUE)
#G <- load.network('G10')
#G <- gen.network.exp(12)
#n <- vcount(G)
#plot(G, edge.width=E(G)$weight)
#save.network(G,'exp12_01')

# summarize process
#beliefs.init <- rnorm(n, mean=0, sd=1)
#beliefs <- simExchange(G, beliefs.init, T=100)$beliefs.final
#result <- data.frame(matrix(nrow=n, ncol=3), row.names=(1:n))
#colnames(result) <- c('beliefs.init', 'beliefs.T', 'beliefs.diff')
#result[,1] <- beliefs.init
#result[,2] <- beliefs
#result[,3] <- abs(beliefs.init - mean(beliefs))

# ----------------------------------------------------------------------
# Exponential degree distribution - exp12
# ----------------------------------------------------------------------
if(save.plots) png('../images/exp12_network_4.png', width=700, height=700, pointsize=15)
set.seed(2002)
G <- load.network('exp12_01')
plot.network(G, forceful.agents=c(4))
if(save.plots) dev.off()

# (1) plot belief convergence with misinformation at one node (without being forceful)
T <- 2000
if(save.plots) png(paste0('../images/exp12_conv_standard_34_',T,'.png'), width=1000, height=700, pointsize=15)
set.seed(2002)
beliefs.init <- rnorm(vcount(G), mean=0, sd=1)
beliefs.init[3] <- 5
beliefs.init[4] <- 5
beliefs.hist <- sim.exchange(G, beliefs.init, T=T)$beliefs.hist
plot.convergence(beliefs.hist)
if(save.plots) dev.off()

# (2) plot belief convergence with one forceful agent
T <- 2000
if(save.plots) png(paste0('../images/exp12_conv_forceful_34_',T,'.png'), width=1000, height=700, pointsize=15)
set.seed(2002)
beliefs.init <- rnorm(vcount(G), mean=0, sd=1)
beliefs.init[3] <- 5
beliefs.init[4] <- 5
beliefs.hist <- sim.exchange.forceful(G, beliefs.init, forceful.agents=c(3,4), forceful.probs=c(0.8,0.2,0.0), eps=0.4, T=T)$beliefs.hist
plot.convergence(beliefs.hist)
if(save.plots) dev.off()

# (3) plot mean convergence with one forceful agent
T <- 1500
if(save.plots) png(paste0('../images/exp12_conv_mean_',T,'.png'), width=700, height=500, pointsize=15)
set.seed(2002)
beliefs.init <- rnorm(vcount(G), mean=0, sd=0.01)
conv.table <- conv.mean.forceful(G, beliefs.init, T)
plot.mean.convergence(conv.table)
if(save.plots) dev.off()

# (4) plot standard deviation convergence in presence of misinformation (without being forceful)
T <- 2000
if(save.plots) png(paste0('../images/exp12_conv_sd_',T,'.png'), width=700, height=500, pointsize=15)
set.seed(2002)
beliefs.init <- rnorm(vcount(G), mean=0, sd=1)
conv.table <- conv.sd(G, beliefs.init, T)
plot.sd.convergence(conv.table)
if(save.plots) dev.off()

# (5) plot standard deviation convergence with one forceful agent
T <- 2000
if(save.plots) png(paste0('../images/exp12_conv_sd_forceful_',T,'.png'), width=700, height=500, pointsize=15)
set.seed(2002)
beliefs.init <- rnorm(vcount(G), mean=0, sd=1)
conv.table <- conv.sd.forceful(G, beliefs.init, T)
plot.sd.convergence(conv.table)
if(save.plots) dev.off()





