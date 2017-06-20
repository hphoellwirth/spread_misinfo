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
save.plots <- FALSE
col.scheme.12 <- c('dodgerblue', 'orange', 'sienna', 'green', 'yellow', 'magenta', 'blue', 'cyan', 'red', 'deeppink', 'forestgreen', 'purple')


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
# Load and plot network
# ----------------------------------------------------------------------
if(save.plots) png('../images/exp12_network_colored_small.png', width=700, height=700, pointsize=15)
set.seed(2013)
G <- load.network('exp12_01')
plot.network(G, forceful.agents=c(), colors=col.scheme.12)
if(save.plots) dev.off()

# ----------------------------------------------------------------------
# No misinformation
# ----------------------------------------------------------------------

# (1) plot belief convergence without any misinformation
T <- 300
if(save.plots) png(paste0('../images/exp12_conv_standard_',T,'.png'), width=1000, height=700, pointsize=16)
set.seed(2002)
beliefs.init <- rnorm(vcount(G), mean=0, sd=1)
beliefs.init[4] <- 5
beliefs.hist <- sim.exchange(G, beliefs.init, T=T)$beliefs.hist
plot.convergence(beliefs.hist, col.scheme.12, title='Belief convergence without misinformation')
if(save.plots) dev.off()

# (2) plot standard deviation convergence without misinformation
T <- 2000
if(save.plots) png(paste0('../images/exp12_conv_sd_standard_',T,'.png'), width=1000, height=700, pointsize=15)
set.seed(2002)
beliefs.init <- rnorm(vcount(G), mean=0, sd=1)
conv.table <- conv.sd(G, beliefs.init, misinfo=0, T)
plot.sd.convergence(conv.table, col.scheme.12, title='Belief deviation in presence of misinformation')
if(save.plots) dev.off()

# ----------------------------------------------------------------------
# Naive agent
# ----------------------------------------------------------------------

# (1) plot belief convergence with misinformation at one node (without being forceful)
T <- 300
if(save.plots) png(paste0('../images/exp12_conv_misinfo_',T,'.png'), width=1000, height=700, pointsize=16)
set.seed(2002)
beliefs.init <- rnorm(vcount(G), mean=0, sd=1)
beliefs.init[4] <- 5
beliefs.hist <- sim.exchange(G, beliefs.init, T=T)$beliefs.hist
plot.convergence(beliefs.hist, col.scheme.12, title='Belief convergence with misinformation (spread by node 4)')
if(save.plots) dev.off()

# (2) plot standard deviation convergence in presence of misinformation (without being forceful)
T <- 2000
if(save.plots) png(paste0('../images/exp12_conv_sd_misinfo_',T,'.png'), width=1000, height=700, pointsize=15)
set.seed(2002)
beliefs.init <- rnorm(vcount(G), mean=0, sd=1)
conv.table <- conv.sd(G, beliefs.init, misinfo=5, T)
plot.sd.convergence(conv.table, col.scheme.12, title='Belief deviation in presence of misinformation')
if(save.plots) dev.off()

# ----------------------------------------------------------------------
# Forceful agent
# ----------------------------------------------------------------------

# (1) plot belief convergence with one forceful agent
T <- 2000
if(save.plots) png(paste0('../images/exp12_conv_forceful_4_',T,'.png'), width=1000, height=700, pointsize=15)
set.seed(2002)
beliefs.init <- rnorm(vcount(G), mean=0, sd=1)
beliefs.init[4] <- 5
beliefs.hist <- sim.exchange.forceful(G, beliefs.init, forceful.agents=c(4), forceful.probs=c(0.8,0.2,0.0), eps=0.4, T=T)$beliefs.hist
plot.convergence(beliefs.hist, col.scheme.12, title='Belief convergence with forceful misinformation (spread by agent 4)')
if(save.plots) dev.off()

# (2) plot mean convergence with one forceful agent
T <- 1500
if(save.plots) png(paste0('../images/exp12_conv_mean_',T,'_2.png'), width=1000, height=700, pointsize=15)
set.seed(2003)
beliefs.init <- rnorm(vcount(G), mean=0, sd=0.01)
conv.table <- conv.mean.forceful(G, beliefs.init, T)
plot.mean.convergence(conv.table, col.scheme.12, title='Mean belief convergence with one forceful agent')
if(save.plots) dev.off()

# (3) plot standard deviation convergence with one forceful agent
T <- 2000
if(save.plots) png(paste0('../images/exp12_conv_sd_forceful_',T,'.png'), width=1000, height=700, pointsize=15)
set.seed(2002)
beliefs.init <- rnorm(vcount(G), mean=0, sd=1)
conv.table <- conv.sd.forceful(G, beliefs.init, T)
plot.sd.convergence(conv.table, col.scheme.12, title='Belief deviation in presence of forceful agent')
if(save.plots) dev.off()

# ----------------------------------------------------------------------
# Stubborn agents
# ----------------------------------------------------------------------

# (1) plot belief convergence with 2 stubborn agents
T <- 2000
if(save.plots) png(paste0('../images/exp12_conv_stubborn_46_',T,'.png'), width=1000, height=700, pointsize=15)
set.seed(2002)
beliefs.init <- rnorm(vcount(G), mean=0, sd=1)
beliefs.init[4] <- 5
beliefs.init[7] <- -5
beliefs.hist <- sim.exchange.forceful(G, beliefs.init, forceful.agents=c(4,7), forceful.probs=c(1,0,0.0), eps=0.4, T=T)$beliefs.hist
plot.convergence(beliefs.hist, col.scheme.12, title='Belief convergence with 2 stubborn agents (4 and 7)')
if(save.plots) dev.off()

# (2) plot standard deviation convergence with two contradicting stubborn agent
T <- 2000
if(save.plots) png(paste0('../images/exp12_conv_sd_stubborn_',T,'.png'), width=1000, height=700, pointsize=15)
set.seed(2002)
beliefs.init <- rnorm(vcount(G), mean=0, sd=1)
conv.table <- conv.sd.stubborn(G, beliefs.init, T)
plot.sd.convergence(conv.table, col.scheme.12, title='Belief deviation in presence of stubborn agents')
if(save.plots) dev.off()

# ----------------------------------------------------------------------
# Novel extension
# ----------------------------------------------------------------------

# (1) plot belief convergence with misinformation at one node (taking difference in beliefs into account)
T <- 5000
if(save.plots) png(paste0('../images/exp12_conv_misinfo_diff_',T,'.png'), width=1000, height=700, pointsize=16)
set.seed(2002)
beliefs.init <- rnorm(vcount(G), mean=0, sd=1)
beliefs.init[4] <- 5
beliefs.hist <- sim.exchange(G, beliefs.init, T=T, diff=TRUE)$beliefs.hist
plot.convergence(beliefs.hist, col.scheme.12, title='Belief convergence with misinformation (spread by node 4)')
if(save.plots) dev.off()

# (2) plot belief convergence with one forceful agent (taking difference in beliefs into account)
T <- 10000
if(save.plots) png(paste0('../images/exp12_conv_forceful_diff_4_',T,'.png'), width=1000, height=700, pointsize=15)
set.seed(2002)
beliefs.init <- rnorm(vcount(G), mean=0, sd=1)
beliefs.init[4] <- 5
beliefs.hist <- sim.exchange.forceful(G, beliefs.init, forceful.agents=c(4), forceful.probs=c(0.8,0.2,0.0), eps=0.4, T=T, diff=TRUE)$beliefs.hist
plot.convergence(beliefs.hist, col.scheme.12, title='Belief convergence with forceful misinformation (spread by agent 4)')
if(save.plots) dev.off()



