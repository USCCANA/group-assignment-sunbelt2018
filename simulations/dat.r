library(igraph)
library(netdiffuseR)

# Parameters
nnet     <- 100
set.seed(100)
net_size     <- sample(20:50, nnet, TRUE)
dat_nleaders <- ceiling(runif(nnet, .05, .2)*net_size)

# Attributes
dat_attributes <- lapply(net_size, function(n) {
  data.frame(
    gender = sample.int(2L, n, TRUE),
    age    = sample(15L:17L, n, TRUE)
  )
})

# Networks
dat_networks <- list(
  `scale_free`    = lapply(net_size, function(n) as_adj(barabasi.game(n, m = 4), sparse = FALSE)),
  `small_world`   = lapply(net_size, function(n) as_adj(sample_smallworld(1, n, nei = 4, p = .1), sparse = FALSE)),
  `sf_homophilic` = lapply(seq_along(net_size), function(i) {
    netdiffuseR::rgraph_ba(t = net_size[i]-1, m=2L, eta = dat_attributes[[i]]$age)
  })
)

# Computing different scores
for (i in seq_along(dat_attributes))
  for (n in names(dat_networks)) {
    
    # Indegree
    dat_attributes[[i]][[paste0("indegree_", n)]] <- degree(
      graph_from_adjacency_matrix(dat_networks[[n]][[i]]), mode = "in"
    )
    
    # Betweenes
    dat_attributes[[i]][[paste0("betweenness_", n)]] <- betweenness(
      graph_from_adjacency_matrix(dat_networks[[n]][[i]]),
      normalized = TRUE
    )
    
  }
    
# Saving the data to be used later
saveRDS(dat_attributes, "simulations/dat_attributes.rds", compress = FALSE)
saveRDS(dat_networks, "simulations/dat_networks.rds", compress = FALSE)
saveRDS(dat_nleaders, "simulations/dat_nleaders.rds", compress = FALSE)