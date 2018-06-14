library(influenceR)
library(igraph)

source("utils.r") #options(error = utils::dump.frames())

# Reading the data in
dat_attributes <- readRDS("simulations/dat_attributes.rds")
dat_networks   <- readRDS("simulations/dat_networks.rds")
dat_nleaders   <- readRDS("simulations/dat_nleaders.rds")


# Computing assignments
keyplayer_results <- vector("list", length(dat_networks))
names(keyplayer_results) <- names(dat_networks)

for (n in names(keyplayer_results)) {
  
  # Making more space
  keyplayer_results[[n]] <- vector("list", length(dat_networks[[n]]))
  
  for (i in seq_along(keyplayer_results[[n]])) {
  
    # Computing geodesic distances
    G <- graph_from_adjacency_matrix(dat_networks[[n]][[i]])
    
    message("Starting keyplayer algorithm in class ", n, " network ", i, "...", appendLF = FALSE)
    keyplayer_results[[n]][[i]] <- keyplayer(
      G,
      k      = dat_nleaders[i],
      tol    = 1e-100,
      maxsec = 5*60
    )

    saveRDS(keyplayer_results, file="simulations/keyplayer.rds", compress=FALSE)  

    message(" done.")
  }
  
}
  
