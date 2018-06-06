library(GroupAssignment)

# Reading the data in
dat_attributes <- readRDS("simulations/dat_attributes.rds")
dat_networks   <- readRDS("simulations/dat_networks.rds")
dat_nleaders   <- readRDS("simulations/dat_nleaders.rds")

# Computing assignments
group_assignment_results <- vector("list", length(dat_networks))
names(group_assignment_results) <- names(dat_networks)

for (n in names(group_assignment_results)) {
  
  # Making more space
  group_assignment_results[[n]] <- vector("list", length(dat_networks[[n]]))
  
  for (i in seq_along(group_assignment_results[[n]])) {
  
    # Computing geodesic distances
    G <- distances(
      graph_from_adjacency_matrix(dat_networks[[n]][[i]])
      )
    
    # In the case of disconnected ones, we set the distance equal to max + 1
    G[is.infinite(G)] <- max(G[is.finite(G)]) + 1L
    
    # G <- G + 1L
    G <- max(G) - G + 1
    group_assignment_results[[n]][[i]] <- optimalAssignment(
      G,
      leaders      = dat_nleaders[i],
      minGroupSize = 1,
      maxGroupSize = nrow(G)
    )
    
    message("Network class ", n, " #", i, " done.")
  }
  
}
  
