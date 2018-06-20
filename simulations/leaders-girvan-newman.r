library(igraph)
library(dplyr)
library(magrittr)

dat_attributes <- readRDS("simulations/dat_attributes.rds")
dat_networks   <- readRDS("simulations/dat_networks.rds")
dat_nleaders   <- readRDS("simulations/dat_nleaders.rds")

# Computing assignments --------------------------------------------------------

ans <- vector("list", length(dat_networks))
names(ans) <- names(dat_networks)

# Looping through networks
set.seed(1)
for (n in names(ans)) {
  
  ans[[n]] <- vector("list", length(dat_networks[[n]]))
  
  for (i in seq_along(ans[[n]])) {
    
    # Getting the membership
    ans[[n]][[i]] <- suppressWarnings({
      dat_networks[[n]][[i]] %>%
        graph_from_adjacency_matrix %>% 
        cluster_edge_betweenness %>% 
        unclass %>% "[["("membership")
    })
      
    ans[[n]][[i]] <- cbind(dat_attributes[[i]], mem = ans[[n]][[i]]) %>%
      mutate(id = 1:n()) 
    ans[[n]][[i]] <- ans[[n]][[i]][
      order(
        -ans[[n]][[i]][[paste0("indegree_", n)]],
        -ans[[n]][[i]][[paste0("betweenness_", n)]]
        ),]
    
    # How many leaders per group
    nlead <- max(1, dat_nleaders[i] %/% length(unique(ans[[n]][[i]]$mem)))
    
    ans[[n]][[i]] <- ans[[n]][[i]] %>% 
      group_by(mem) %>%
      filter(1:n() <= nlead) %>%
      ungroup %>%
      select(id)  %>% 
      unclass %>% unname %>% "[["(1)
    
  }
    
  
  message("Network ", n, " done.")
}

saveRDS(
  ans,
  file = "simulations/leaders-girvan-newman.rds"
)
