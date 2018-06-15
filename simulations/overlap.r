library(igraph)

# Reading the data in

leaders_ga <- readRDS("simulations/group_assignment.rds")
leaders_kp <- readRDS("simulations/keyplayer.rds")

dat_attributes <- readRDS("simulations/dat_attributes.rds")
dat_networks   <- readRDS("simulations/dat_networks.rds")
dat_nleaders   <- readRDS("simulations/dat_nleaders.rds")


